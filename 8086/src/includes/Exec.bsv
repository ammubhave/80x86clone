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

function Word calcAddr(DecodedInst pdInst);
    let mod = pdInst.mod;
    let rm = pdInst.rm;
    case (mod)
        'b00:
        begin
            if (rm == 'b110)
                return zeroExtend(pdInst.highDispAddr) << 8 + pdInst.lowDispAddr;
            else if (rm[2] == 0)
                return pdInst.srcVal2.Word + pdInst.srcVal3.Word;
            else
                return pdInst.srcVal2.Word;
        end
        'b01:
        begin
            if (rm[2] == 0)
                return pdInst.srcVal2.Word + pdInst.srcVal3.Word + signExtend(pdInst.lowDispAddr);
            else
                return pdInst.srcVal2.Word + signExtend(pdInst.lowDispAddr);
        end
        'b10:
        begin
            if (rm[2] == 0)
                return pdInst.srcVal2.Word + pdInst.srcVal3.Word + zeroExtend(pdInst.highDispAddr) << 8 + pdInst.lowDispAddr;
            else
                return pdInst.srcVal2.Word + zeroExtend(pdInst.highDispAddr) << 8 + pdInst.lowDispAddr;
        end
        default: return ?;
    endcase
endfunction

(* noinline *)
function ExecInst exec(DecodedInst dInst, Word ip, Word pc);
  ExecInst eInst = ?;
  eInst.dst1 = tagged Invalid;
  eInst.dst2 = tagged Invalid;
  eInst.dstm = tagged Invalid;
  eInst.dstVal1 = 0;
  eInst.dstVal2 = 0;
  eInst.dstValm = 0;
 /* Data aluVal2 = isValid(dInst.imm) ? validValue(dInst.imm) : rVal2;
  
  let aluRes = alu(rVal1, aluVal2, dInst.aluFunc);
  */
  eInst.iType = dInst.iType;

  case (dInst.iType)
    INC: // 'h40, 'h41, 'h42, 'h43, 'h44, 'h45, 'h46, 'h47 // done
    begin
      eInst.dst1 = dInst.dst1;
      eInst.dstVal1 = dInst.srcVal1 + 1;
    end

    DEC: // 'h48, 'h49, 'h4A, 'h4B, 'h4C, 'h4D, 'h4E, 'h4F // done
    begin
      eInst.dst1 = dInst.dst1;
      eInst.dstVal1 = dInst.srcVal1 - 1;
    end

    MOV_R_IMM8: // 'hB0, 'hB1, 'hB2, 'hB3, 'hB4, 'hB5, 'hB6, 'hB7 // done
    begin
      eInst.dst1 = dInst.dst1;
      eInst.dstVal1 = dInst.srcimm;
    end

    MOV_R_IMM16: // 'hB8, 'hB9, 'hBA, 'hBB, 'hBC, 'hBD, 'hBE, 'hBF, 'hC7 // done
    begin
      eInst.dst1 = dInst.dst1;
      eInst.dstVal1 = dInst.srcimm;
    end

    MOV_M_IMM16:
    begin
      eInst.dstm = tagged Valid SegAddr{seg: DS, offset: calcAddr(dInst)};
      eInst.dstValm = dInst.srcimm;
    end

    MOV_R_RM16:
    begin
      eInst.dst1 = dInst.dst1;
      eInst.dstVal1 = dInst.srcValm;
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
  eInst.nextIp = pc + 1;
  return eInst;
endfunction
