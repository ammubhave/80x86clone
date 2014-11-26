/*

Copyright (C) 2012

Arvind <arvind@csail.mit.edu>
Derek Chiou <derek@ece.utexas.edu>
Muralidaran Vijayaraghavan <vmurali@csail.mit.edu>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/


import Types::*;
import ProcTypes::*;
import Vector::*;
/*
(* noinline *)
function Data alu(Data a, Data b, AluFunc func);
  Data res = case(func)
     Add   : (a + b);
     Sub   : (a - b);
     And   : (a & b);
     Or    : (a | b);
     Xor   : (a ^ b);
     Nor   : ~(a | b);
     Slt   : zeroExtend( pack( signedLT(a, b) ) );
     Sltu  : zeroExtend( pack( a < b ) );
     LShift: (a << b[4:0]);
     RShift: (a >> b[4:0]);
     Sra   : signedShiftRight(a, b[4:0]);
  endcase;
  return res;
endfunction

(* noinline *)
function Bool aluBr(Data a, Data b, BrFunc brFunc);
  Bool brTaken = case(brFunc)
    Eq  : (a == b);
    Neq : (a != b);
    Le  : signedLE(a, 0);
    Lt  : signedLT(a, 0);
    Ge  : signedGE(a, 0);
    Gt  : signedGT(a, 0);
    AT  : True;
    NT  : False;
  endcase;
  return brTaken;
endfunction

(* noinline *)
function Addr brAddrCalc(Addr pc, Data val, IType iType, Data imm, Bool taken);
  Addr pcPlus4 = pc + 4; 
  Addr targetAddr = case (iType)
    J  : {pcPlus4[31:28], imm[27:0]};
    Jr : val;
    Br : (taken? pcPlus4 + imm : pcPlus4);
    Alu, Ld, St, Mfc0, Mtc0, Unsupported: pcPlus4;
  endcase;
  return targetAddr;
endfunction
*/

// function RegWord r16FromR8(RegByte r) = unpack({0, pack(r)[1:0]});

function ExecInst writeToRm(ExecInst eInst, Bit#(2) mod, Number data);
  if (mod == 'b11)
    eInst.dstVal2 = data;
  else
    eInst.dstValm = data;
  return eInst;
endfunction

function Number getRMVal(DecodedInst dInst);
  if (dInst.mod == 'b11)
    return dInst.srcVal2;
  else
    return dInst.srcValm;
endfunction

function Bit#(1) nb(Number n);
  let ret = 0;
  if (n matches tagged Byte .b)
    ret = b[3];
  else if (n matches tagged Word .w)
    ret = w[3];
  return ret;
endfunction

function RegFlags getSZPFlags(Number result, RegFlags flags, Bit#(16) flagsEn);
  Bit#(1) sF = flags.sF;
  Bit#(1) zF = flags.zF;
  Bit#(1) pF = flags.pF;
  if (result matches tagged Byte .b) begin
    sF = b[7];
    zF = b == 0 ? 1 : 0;
    pF = ~(b[7] ^ b[6] ^ b[5] ^ b[4] ^
                 b[3] ^ b[2] ^ b[1] ^ b[0]);
  end else if (result matches tagged Word .w) begin
    sF = w[15];
    zF = w == 0 ? 1 : 0;
    pF = ~(w[7] ^ w[6] ^ w[5] ^ w[4] ^
                 w[3] ^ w[2] ^ w[1] ^ w[0]);
  end
  if (flagsEn[7] == 1) flags.sF = sF;
  if (flagsEn[6] == 1) flags.zF = zF;
  if (flagsEn[2] == 1) flags.pF = pF;

  return flags;
endfunction

function RegFlags getOAFlagsAdd(Number result, Number op1, Number op2, RegFlags flags);
  flags.oF = (msb(op1) == msb(op2) && msb(op1) != msb(result)) ? 1 : 0;
  flags.aF = (nb(op1) == nb(op2) && nb(op1) != nb(result)) ? 1 : 0;
  return flags;
endfunction
function RegFlags getOAFlagsSub(Number result, Number op1, Number op2, RegFlags flags);
  flags.oF = (msb(op1) != msb(op2) && msb(op1) != msb(result)) ? 1 : 0;
  flags.aF = (nb(op1) != nb(op2) && nb(op1) != nb(result)) ? 1 : 0;
  return flags;
endfunction

function Number aluRes(AluFunc func, Number op1, Number op2, RegFlags flags);
  let ret = tagged Word (?);
  case (func)
    A:
    begin
      ret = op1;
    end

    ADD:
    begin
      ret = op1 + op2;
    end

    OR:
    begin
      ret = op1 | op2;
    end

    ADC:
    begin
      if (op1 matches tagged Byte .b)
        ret = op1 + op2 + tagged Byte (zeroExtend(flags.cF));
      else
        ret = op1 + op2 + tagged Word (zeroExtend(flags.cF));
    end

    SBB:
    begin
      if (op1 matches tagged Byte .b)
        ret = op1 - op2 - tagged Byte (zeroExtend(flags.cF));
      else
        ret = op1 - op2 - tagged Word (zeroExtend(flags.cF));
    end

    AND:
    begin
      ret = op1 & op2;
    end

    SUB,
    CMP:
    begin
      ret = op1 - op2;
    end

    XOR:
    begin
      ret = op1 ^ op2;
    end
  endcase
  return ret;
endfunction

function RegFlags getFlags(AluFunc func, Number op1, Number op2, Number result, RegFlags flags, Bit#(16) flagsEn);
  Bit#(1) oF = flags.oF;
  Bit#(1) aF = flags.aF;
  case (func)
    CMP,
    SUB:
    begin
      oF = (msb(op1) != msb(op2) && msb(op1) != msb(result)) ? 1 : 0;
      aF = (nb(op1) != nb(op2) && nb(op1) != nb(result)) ? 1 : 0;
      flags = getSZPFlags(result, flags, flagsEn);
    end
  endcase
  if (flagsEn[4] == 1) flags.aF = aF;
  if (flagsEn[11] == 1) flags.oF = oF;
  return flags;
endfunction

function AluFunc aluFuncFromAluArith(Bit#(3) r);
  AluFunc ret = ?;
  case (r)
    'b000: ret = ADD;
    'b001: ret = OR;
    'b010: ret = ADC;
    'b011: ret = SBB;
    'b100: ret = AND;
    'b101: ret = SUB;
    'b110: ret = XOR;
    'b111: ret = CMP;
  endcase
  return ret;
endfunction

(* noinline *)
function ExecInst exec(DecodedInst dInst, Word ip, Word pc);
  ExecInst eInst = ?;
  eInst.dst1 = dInst.dst1;
  eInst.dst2 = dInst.dst2;
  eInst.dstm = dInst.dstm;
  eInst.dstp = dInst.dstp;
  eInst.dstVal1 = 0;
  eInst.dstVal2 = 0;
  eInst.dstValm = 0;
  eInst.flags = dInst.flags;
 /* Data aluVal2 = isValid(dInst.imm) ? validValue(dInst.imm) : rVal2;
  
  let aluRes = alu(rVal1, aluVal2, dInst.aluFunc);
  */
  eInst.iType = dInst.iType;
  eInst.nextIp = pc + 1;

  Number op1 = tagged Word (?);
  Number op2 = tagged Word (?);
  AluFunc aluFunc = ?;
  Bit#(16) flagsEn = '0;

  case (dInst.iType)
    CALL_NEAR:
    begin
      aluFunc = ADD;
      op1 = dInst.srcVal1;
      op2 = tagged Word 2;
      //eInst.dstVal1 = dInst.srcVal1 + 2;
      eInst.dstValm = tagged Word eInst.nextIp;
      eInst.nextIp = eInst.nextIp + dInst.srcimm.Word;
    end

    ALU_RM_R:
    begin
      aluFunc = aluFuncFromAluArith(dInst.opcode[5:3]);
      op1 = getRMVal(dInst);
      op2 = dInst.srcVal1;
      flagsEn = 'b0000100011010101;
    end

    ALU_R_RM:
    begin
      aluFunc = aluFuncFromAluArith(dInst.opcode[5:3]);
      op1 = dInst.srcVal1;
      op2 = getRMVal(dInst);
      flagsEn = 'b0000100011010101;
    end

    ALU_R_IMM:
    begin
      aluFunc = aluFuncFromAluArith(dInst.opcode[5:3]);
      op1 = dInst.srcVal1;
      op2 = dInst.srcimm;
      flagsEn = 'b0000100011010101;
    end

    ALU_RM8_IMM8,
    ALU_RM16_IMM16:
    begin
      aluFunc = aluFuncFromAluArith(dInst.r);
      op1 = getRMVal(dInst);
      op2 = dInst.srcimm;
      flagsEn = 'b0000100011010101;
    end

    ALU_RM16_SIMM8:
    begin
      aluFunc = aluFuncFromAluArith(dInst.r);
      op1 = getRMVal(dInst);
      op2 = tagged Word signExtend(dInst.srcimm.Byte);
      flagsEn = 'b0000100011010101;
    end

    CMC:
    begin
      eInst.flags.cF = eInst.flags.cF == 0 ? 1 : 0;
    end

    CLC:
    begin
      eInst.flags.cF = 0;
    end

    CLI:
    begin
      eInst.flags.iF = 0;
    end

    CLD:
    begin
      eInst.flags.dF = 0;
    end

   /* DAA:
    begin
      let al = dInst.srcVal1;
      if (al[4:0] > 9 || eInst.flags.aF == 1) begin
        al = al + 6;
        eInst.flags.aF = 1;
      end
      if (al > 'h9F || eInst.flags.cF == 1) begin
        al = al + 'h60;
        eInst.flags.cF = 1;
      end
    end

    DAS:
    begin
      let al = dInst.srcVal1;
      if (al[4:0] > 9 || eInst.flags.aF == 1) begin
        al = al - 6;
        eInst.flags.aF = 1;
      end
      if (al > 'h9F || eInst.flags.cF == 1) begin
        al = al - 'h60;
        eInst.flags.cF = 1;
      end
    end*/

    INC: // 'h40, 'h41, 'h42, 'h43, 'h44, 'h45, 'h46, 'h47 // done
    begin
      aluFunc = ADD;
      op1 = dInst.srcVal1;
      op2 = tagged Word 1;
      flagsEn = 'b0000100011010100;
    end

    DEC: // 'h48, 'h49, 'h4A, 'h4B, 'h4C, 'h4D, 'h4E, 'h4F // done
    begin
      aluFunc = SUB;
      op1 = dInst.srcVal1;
      op2 = tagged Word 1;
      flagsEn = 'b0000100011010100;
    end

    LEA,
    MOV_R_IMM, // 'hB0, 'hB1, 'hB2, 'hB3, 'hB4, 'hB5, 'hB6, 'hB7
                // 'hB8, 'hB9, 'hBA, 'hBB, 'hBC, 'hBD, 'hBE, 'hBF, 'hC7  // done
    MOV_M_IMM16:
    begin
      aluFunc = A;
      op1 = dInst.srcimm;
      //eInst.dstVal1 = dInst.srcimm;
    end

   /* MOV_M_IMM16:
    begin
      aluFunc = A;
      op1 = dInst.srcimm;
      flagsEn = 'b0000000000000000;
      //eInst.dstValm = dInst.srcimm;
    end*/

    MOV_RM_R,
    OUT:
    begin
      aluFunc = A;
      op1 = dInst.srcVal1;
      //eInst.dstValm = dInst.srcVal1;
      //eInst.dstVal2 = dInst.srcVal1;
    end

    MOV_R_RM:
    begin
      aluFunc = A;
      op1 = getRMVal(dInst);//dInst.srcValm;
      //eInst.dstVal1 = dInst.srcValm;
    end

    JE: // JZ
    begin
      if (dInst.flags.zF == 1)
        eInst.nextIp = eInst.nextIp + signExtend(dInst.srcimm.Byte);
    end

    JMP_SHORT:
    begin
      eInst.nextIp = eInst.nextIp + signExtend(dInst.srcimm.Byte);
    end

    JMP_NEAR:
    begin
      eInst.nextIp = eInst.nextIp + dInst.srcimm.Word;
    end

    /*LEA:
    begin 
      eInst.dstVal1 = dInst.srcimm;
    end*/

   /* OUT:
    begin
      eInst.dstValp = dInst.srcVal1;
    end*/
    POP_R16:
    begin
      aluFunc = SUB;
      op1 = dInst.srcVal1;
      op2 = tagged Word 2;
      eInst.dstVal2 = dInst.srcValm;
    end

    PUSH_R16:
    begin
      aluFunc = ADD;
      op1 = dInst.srcVal1;
      op2 = tagged Word 2;
      eInst.dstValm = dInst.srcVal2;
    end

    RET:
    begin
      aluFunc = SUB;
      op1 = dInst.srcVal1;
      op2 = tagged Word 2;
      eInst.nextIp = dInst.srcValm.Word;
    end

    STC:
    begin
      eInst.flags.cF = 1;
    end

    STI:
    begin
      eInst.flags.iF = 1;
    end

    STD:
    begin
      eInst.flags.dF = 1;
    end

    XCHG_R_R:
    begin
      eInst.dstVal1 = dInst.srcVal2;
      eInst.dstVal2 = dInst.srcVal1;
    end
  endcase

  Number result = tagged Word (?);
 /* case (dInst.iType)
    ALU_RM8_IMM8, ALU_RM16_IMM16, ALU_RM16_SIMM8, INC, DEC:
    begin*/
      result = aluRes(aluFunc, op1, op2, eInst.flags);
      eInst.flags = getFlags(aluFunc, op1, op2, result, eInst.flags, flagsEn);
   /* end
  endcase*/

  case (dInst.iType)
    ALU_RM_R, ALU_RM8_IMM8, ALU_RM16_IMM16, ALU_RM16_SIMM8, MOV_RM_R:
    begin
      eInst = writeToRm(eInst, dInst.mod, result);
    end
    ALU_R_RM, ALU_R_IMM, CALL_NEAR, DEC, INC, LEA, MOV_R_IMM, MOV_R_RM, POP_R16, PUSH_R16, RET:
    begin
      eInst.dstVal1 = result;
    end
    MOV_M_IMM16:
    begin
      eInst.dstValm = result;
    end
    OUT:
    begin
      eInst.dstValp = result;
    end
  endcase
 /* 
  eInst.data = case(dInst.iType)
                    Mfc0:       copVal;
                    Mtc0:       rVal1;
                    Mfhi:       hi;
                    Mflo:       lo;
                    Mthi:       rVal1;
                    Mtlo:       rVal1;
                    St:         rVal2;
                    J:          (pc+4);
                    Jr:         (pc+4);
                    default:    aluRes;
                endcase;
  
  let brTaken = aluBr(rVal1, rVal2, dInst.brFunc);
  let brAddr = brAddrCalc(pc, rVal1, dInst.iType, validValue(dInst.imm), brTaken);
  eInst.mispredict = brAddr != ppc;

  eInst.brTaken = brTaken;
  eInst.addr = (dInst.iType == Ld || dInst.iType == St) ? aluRes : brAddr;
  
  eInst.dst = dInst.dst;*/
  return eInst;
endfunction
