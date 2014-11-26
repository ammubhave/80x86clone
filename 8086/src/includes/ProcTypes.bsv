/*

Copyright (C) 2012

Arvind <arvind@csail.mit.edu>
Muralidaran Vijayaraghavan <vmurali@csail.mit.edu>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/

import Types::*;
import FShow::*;
import MemTypes::*;

interface Proc;
    method ActionValue#(Tuple2#(RIndx, Data)) cpuToHost;
    method Action hostToCpu(Addr startpc);
    interface MemInitIfc iMemInit;
    interface MemInitIfc dMemInit;
endinterface

typedef Bit#(5) RIndx;

typedef 2 SegRegFifoSize;
typedef 2 RedirectFifoSize;
typedef 2 DMemFifoSize;
typedef 6 QBusSize;

typedef enum {Unsupported,
              ALU_RM_R,
              ALU_R_RM,
              ALU_R_IMM,
              ALU_RM8_IMM8,
              ALU_RM16_IMM16,
              ALU_RM16_SIMM8,
              CALL_NEAR,
              CLC,
              CLD,
              CLI,
              CMC,
              DAA,
              DAS,
              DEC,
              HLT,
              IN,
              INC,
              JE,
              JMP_FAR,
              JMP_NEAR,
              JMP_SHORT,
              LEA,
              MOV_M_IMM16,
              MOV_R_IMM,
              MOV_R_RM,
              MOV_RM_R,
              NOP,
              OUT,
              POP_R16,
              PUSH_R16,
              RET,
              STC,
              STD,
              STI,
              TEST,
              XCHG_R_R} IType deriving(Bits, Eq, FShow);
typedef enum {Eq, Neq, Le, Lt, Ge, Gt, AT, NT} BrFunc deriving(Bits, Eq, FShow);

typedef enum {
    A,
    ADD,
    OR,
    ADC,
    SBB,
    AND,
    SUB,
    XOR,
    CMP
} AluFunc deriving(Bits, Eq, FShow);

typedef enum {
    AX = 3'b000,
    CX = 3'b001,
    DX = 3'b010,
    BX = 3'b011,
    SP = 3'b100,
    BP = 3'b101,
    SI = 3'b110,
    DI = 3'b111
} RegWord deriving (Bits, Eq, FShow);

typedef enum {
    AL = 3'b000,
    CL = 3'b001,
    DL = 3'b010,
    BL = 3'b011,
    AH = 3'b100,
    CH = 3'b101,
    DH = 3'b110,
    BH = 3'b111
} RegByte deriving (Bits, Eq, FShow);

typedef struct {
    Bit#(4) reserved1;
    Bit#(1) oF;
    Bit#(1) dF;
    Bit#(1) iF;
    Bit#(1) tF;
    Bit#(1) sF;
    Bit#(1) zF;
    Bit#(1) reserved2;
    Bit#(1) aF;
    Bit#(1) reserved3;
    Bit#(1) pF;
    Bit#(1) reserved4;
    Bit#(1) cF;
} RegFlags deriving (Bits, Eq, FShow);

function RegWord r16FromR8(RegByte r) = unpack({0, pack(r)[1:0]});

typedef enum {
    CS, DS, ES, SS
} RegSeg deriving (Bits, Eq, FShow);

typedef union tagged {
    RegWord RegWord;
    RegByte RegByte;
} RegNumber deriving (Bits, Eq, FShow);

typedef struct {
    RegSeg seg;
    Word offset;
} SegAddr deriving (Bits, Eq, FShow);

function Bit#(3) regWordIdx(RegWord r) = pack(r);
function Bit#(3) regByteIdx(RegByte r) = {0, pack(r)[1:0]};

/*instance FShow#(RegOperand);
    function Fmt fshow (RegOperand value);
        let f = $format("RegOperand");
        return f;
    endfunction
endinstance
*/
typedef enum {Normal, CopReg} RegType deriving (Bits, Eq, FShow);

typedef void Exception;

typedef struct {
    Bool ie;
    Byte pInst;
    Word pc;    // The instruction's pointer
} QBusElement deriving (Bits);

typedef struct {
    Bool ie;
    DecodedInst pdInst;
    Word pc;    // The counter for partial instructions
    Word ip;
} Fetch2FetchExecute deriving (Bits);

typedef struct {
    Word ip;
    Word nextIp;
    IType brType;
    Bool taken;
    Bool mispredict;
} Redirect deriving (Bits, Eq, FShow);

typedef struct {
    RegType regType;
    RIndx idx;
} FullIndx deriving (Bits, Eq, FShow);

function Addr addrFromSegOff(Word seg, Word off) = (zeroExtend(seg) << 4) + zeroExtend(off);

function Maybe#(FullIndx) validReg(RIndx idx) = Valid (FullIndx{regType: Normal, idx: idx});

function Maybe#(FullIndx) validCop(RIndx idx) = Valid (FullIndx{regType: CopReg, idx: idx});

function RIndx validRegValue(Maybe#(FullIndx) idx) = validValue(idx).idx;

function Maybe#(SegAddr) getMemAddrFromModRm(Bit#(2) mod, Bit#(3) rm, DecodedInst pdInst);
    if (mod == 'b00 && rm == 'b110)
        return tagged Valid (SegAddr{seg: pdInst.mseg, offset: zeroExtend(pdInst.lowDispAddr) + (zeroExtend(pdInst.highDispAddr) << 8)});
    else if (mod == 'b00 && rm [2] == 0)
        return tagged Valid (SegAddr{seg: pdInst.mseg, offset: pdInst.srcVal2.Word + pdInst.srcVal3.Word});
    else if (mod == 'b00 && rm [2] == 1)
        return tagged Valid (SegAddr{seg: pdInst.mseg, offset: pdInst.srcVal2.Word});
    else if (mod == 'b01 && rm [2] == 0)
        return tagged Valid (SegAddr{seg: pdInst.mseg, offset: pdInst.srcVal2.Word + pdInst.srcVal3.Word + zeroExtend(pdInst.lowDispAddr)});
    else if (mod == 'b01 && rm [2] == 1)
        return tagged Valid (SegAddr{seg: pdInst.mseg, offset: pdInst.srcVal2.Word + zeroExtend(pdInst.lowDispAddr)});
    else if (mod == 'b10 && rm [2] == 0)
        return tagged Valid (SegAddr{seg: pdInst.mseg, offset: pdInst.srcVal2.Word + pdInst.srcVal3.Word + zeroExtend(pdInst.lowDispAddr) + (zeroExtend(pdInst.highDispAddr) << 8)});
    else if (mod == 'b10 && rm [2] == 1)
        return tagged Valid (SegAddr{seg: pdInst.mseg, offset: pdInst.srcVal2.Word + zeroExtend(pdInst.lowDispAddr) + (zeroExtend(pdInst.highDispAddr) << 8)});
    else
        return tagged Invalid;
endfunction

typedef struct {
    Byte             opcode;
    IType            iType;
/*    AluFunc          aluFunc;
    BrFunc           brFunc;
    Maybe#(FullIndx) dst;
    Maybe#(FullIndx) src1;
    Maybe#(FullIndx) src2;
    Maybe#(Data)     imm;
*/
    Bit#(2) mod; Bit#(3) r; Bit#(3) rm;

    RegSeg mseg;
    Byte lowDispAddr; Byte highDispAddr;

    Maybe#(RegNumber) src1;
    Maybe#(RegNumber) src2;
    Maybe#(RegNumber) src3;
    Number            srcimm;
    Maybe#(SegAddr)   srcm;
    Maybe#(Word)      srcp;
    Maybe#(RegNumber) dst1;
    Maybe#(RegNumber) dst2;
    Maybe#(SegAddr)   dstm;
    Maybe#(Word)      dstp;

    Number srcVal1;
    Number srcVal2;
    Number srcVal3;
    Number srcValm;
    Number srcValp;

    RegFlags flags;

    Bool reqAddressMode;
    Bool reqLowDispAddr;
    Bool reqHighDispAddr;
    Bool reqLowData;
    Bool reqHighData;
} DecodedInst deriving(Bits, Eq);

function Fmt show_epoch(Bool epoch);
    let ret = $format(" ");
    if (epoch)
        ret = $format("|") + ret;
    else
        ret = $format("-") + ret;
    return ret;
endfunction

function Fmt show_regfetch(DecodedInst pdInst);
    let ret = $format(" (");
    if (pdInst.src1 matches tagged Valid .d &&& d matches tagged RegWord .r)
        ret = ret + $format(fshow(r), " = 0x%4h, ", pdInst.srcVal1.Word);
    if (pdInst.src1 matches tagged Valid .d &&& d matches tagged RegByte .r)
        ret = ret + $format(fshow(r), " = 0x%2h, ", pdInst.srcVal1.Byte);
    if (pdInst.src2 matches tagged Valid .d &&& d matches tagged RegWord .r)
        ret = ret + $format(fshow(r), " = 0x%4h, ", pdInst.srcVal2.Word);
    if (pdInst.src2 matches tagged Valid .d &&& d matches tagged RegByte .r)
        ret = ret + $format(fshow(r), " = 0x%2h, ", pdInst.srcVal1.Byte);
    ret = ret + $format(")");
    return ret;
endfunction
/*
function Fmt show_execute(ExecInst eInst);
    let ret = $format(" (");
    if (pdInst.src1 matches tagged Valid .d &&& d matches tagged RegWord .r)
        ret = ret + $format(fshow(r), " = 0x%4h, ", pdInst.srcVal1.Word);
    if (pdInst.src1 matches tagged Valid .d &&& d matches tagged RegByte .r)
        ret = ret + $format(fshow(r), " = 0x%2h, ", pdInst.srcVal1.Byte);
    if (pdInst.src2 matches tagged Valid .d &&& d matches tagged RegWord .r)
        ret = ret + $format(fshow(r), " = 0x%4h, ", pdInst.srcVal2.Word);
    if (pdInst.src2 matches tagged Valid .d &&& d matches tagged RegByte .r)
        ret = ret + $format(fshow(r), " = 0x%2h, ", pdInst.srcVal1.Byte);
    ret = ret + $format(")");
    return ret;
endfunction*/

function Fmt show_memfetch(DecodedInst pdInst);
    let ret = $format(" (");
    if (pdInst.srcm matches tagged Valid .srcm)
        ret = ret + $format("[", fshow(srcm.seg) ,":0x%4h", srcm.offset,"] = ?");
    ret = ret + $format(")");
    return ret;
endfunction

instance FShow#(DecodedInst);
    function Fmt fshow (DecodedInst pdInst);
        let ret = $format("Unsupported: %h", pdInst.opcode);
        case (pdInst.opcode)
            'h3B:
            begin
                ret = $format("CMP ");
            end

            'h40, 'h41, 'h42, 'h43, 'h44, 'h45, 'h46, 'h47:
            begin
                ret = $format("INC ");
                if (pdInst.dst1 matches tagged Valid .d &&& d matches tagged RegWord .r) begin
                    ret = ret + $format(fshow(r), " (", fshow(r), " <= 0x%4h", pdInst.srcVal1.Word, " + 1)");
                end else
                    ret = ret + $format("?");
            end

            'h48, 'h49, 'h4A, 'h4B, 'h4C, 'h4D, 'h4E, 'h4F:
            begin
                ret = $format("DEC ");
                if (pdInst.dst1 matches tagged Valid .d &&& d matches tagged RegWord .r)
                    ret = ret + $format(fshow(r), " (", fshow(r), " <= 0x%4h", pdInst.srcVal1.Word, " - 1)");
                else
                    ret = ret + $format("?");
            end

            'h50, 'h51, 'h52, 'h53, 'h54, 'h55, 'h56, 'h57:
            begin
                ret = $format("PUSH ");
                if (pdInst.dst2 matches tagged Valid .d &&& d matches tagged RegWord .r)
                    ret = ret + $format(fshow(r));
                else
                    ret = ret + $format("?");
            end

            'h58, 'h59, 'h5A, 'h5B, 'h5C, 'h5D, 'h5E, 'h5F:
            begin
                ret = $format("POP ");
                if (pdInst.dst2 matches tagged Valid .d &&& d matches tagged RegWord .r)
                    ret = ret + $format(fshow(r));
                else
                    ret = ret + $format("?");
            end

            'h48, 'h49, 'h4A, 'h4B, 'h4C, 'h4D, 'h4E, 'h4F:
            begin
                ret = $format("DEC ");
                if (pdInst.dst1 matches tagged Valid .d &&& d matches tagged RegWord .r)
                    ret = ret + $format(fshow(r), " (", fshow(r), " <= 0x%4h", pdInst.srcVal1.Word, " - 1)");
                else
                    ret = ret + $format("?");
            end

            'h74:
            begin
                ret = $format("JE NEAR [pc+");
                ret = ret + $format("#0x%4h", pdInst.srcimm.Word);
                ret = ret + $format("]");
            end

            'h80, 'h81, 'h82, 'h83:
            begin
                case (pdInst.r)
                    'b000: ret = $format("ADD ");
                    'b001: ret = $format("OR ");
                    'b010: ret = $format("ADC ");
                    'b011: ret = $format("SBB ");
                    'b100: ret = $format("AND ");
                    'b101: ret = $format("SUB ");
                    'b110: ret = $format("XOR ");
                    'b111: ret = $format("CMP ");
                endcase
            end

            'h8B:
            begin
                if (pdInst.dst1 matches tagged Valid .d &&& d matches tagged RegWord .r)
                    ret = $format("MOV ", fshow(r) ,", [", fshow(pdInst.mseg), ":0x%2h%2h]", pdInst.highDispAddr, pdInst.lowDispAddr);
                else
                    ret = $format("MOV ?, [", fshow(pdInst.mseg), ":0x%2h%2h]", pdInst.highDispAddr, pdInst.lowDispAddr);
            end

            'h8D:
            begin
                ret = $format("LEA ");
                if (pdInst.dst1 matches tagged Valid .d &&& d matches tagged RegWord .r)
                    ret = ret + $format(fshow(r), ", ");
                else
                    ret = ret + $format("?, ");
                ret = ret + $format("[0x%4h]", pdInst.srcimm.Word);
            end

            'h90:
            begin
                ret = $format("NOP");
            end

            'h91, 'h92, 'h93, 'h94, 'h95, 'h96, 'h97:
            begin
                ret = $format("XCHG AX, ", fshow(pdInst.src2.Valid.RegWord));
            end

            'hA1:
            begin
                ret = $format("MOV AX, [", fshow(pdInst.mseg), ":0x%2h%2h]", pdInst.highDispAddr, pdInst.lowDispAddr);
            end

            'hB0, 'hB1, 'hB2, 'hB3, 'hB4, 'hB5, 'hB6, 'hB7:
            begin
                ret = $format("MOV ");
                if (pdInst.dst1 matches tagged Valid .d &&& d matches tagged RegByte .r)
                    ret = ret + $format(fshow(r));
                else
                    ret = ret + $format("?");
                if (pdInst.srcimm matches tagged Byte .i)
                    ret = ret + $format(", #0x%2h", i);
                else
                    ret = ret + $format(", ?");
            end

            'hB8, 'hB9, 'hBA, 'hBB, 'hBC, 'hBD, 'hBE, 'hBF:
            begin
                ret = $format("MOV ");
                if (pdInst.dst1 matches tagged Valid .d &&& d matches tagged RegWord .r)
                    ret = ret + $format(fshow(r));
                else
                    ret = ret + $format("?");
                if (pdInst.srcimm matches tagged Word .i)
                    ret = ret + $format(", #0x%4h", i);
                else
                    ret = ret + $format(", ?");
            end

            'hC7:
            begin
                ret = $format("MOV ");
                if (pdInst.dstm matches tagged Valid .d)
                    ret = ret + $format("[", fshow(d.seg), ":0x%4h", d.offset, "]");
                if (pdInst.srcimm matches tagged Word .i)
                    ret = ret + $format(", #0x%4h", i);
                else
                    ret = ret + $format(", ?");
            end

            'hE6:
            begin
                ret = $format("OUT ");
                if (pdInst.dstp matches tagged Valid .p)
                    ret = ret + $format("#0x%2h", p);
                else
                    ret = ret + $format("?");
                ret = ret + $format(", AL");
            end

            'hE8:
            begin
                ret = $format("CALL NEAR [pc+");
                ret = ret + $format("#0x%4h", pdInst.srcimm.Word);
                ret = ret + $format("]");
            end

            'hE9:
            begin
                ret = $format("JMP NEAR [pc+");
                ret = ret + $format("#0x%4h", pdInst.srcimm.Word);
                ret = ret + $format("]");
            end

            'hEB:
            begin
                ret = $format("JMP SHORT [pc+");
                ret = ret + $format("#0x%4h", pdInst.srcimm.Byte);
                ret = ret + $format("]");
            end
        endcase
        return ret;
    endfunction
endinstance

typedef struct {
    IType              iType;
    Maybe#(RegNumber)  dst1;
    Maybe#(RegNumber)  dst2;
    Maybe#(SegAddr)    dstm;
    Maybe#(Word)       dstp;
    Number             dstVal1;
    Number             dstVal2;
    Number             dstValm;
    Number             dstValp;
    RegFlags flags;
    Bool               mispredict;
    Bool               brTaken;
    Word               nextIp;
} ExecInst deriving(Bits, Eq, FShow);

/*
instance FShow#(ExecInst);
    function Fmt fshow (ExecInst eInst);
        let ret = $format("Unsupported: %h", pdInst.opcode);
        return ret;
    endfunction
endinstance
*/
// exception causes
Bit#(5) excHint = 5'b00000;  // external int
Bit#(5) excTing = 5'b00010;  // timer int
Bit#(5) excAdEL = 5'b00100;  // error on load
Bit#(5) excAdES = 5'b00101;  // error on store
Bit#(5) excAdEF = 5'b00110;  // error on fetch
Bit#(5) excTlbL = 5'b10000;  // tlb-miss on load, made up code
Bit#(5) excTlbS = 5'b10010;  // tlb-miss on store, made up code
Bit#(5) excTlbF = 5'b10110;  // tlb-miss on fetch, made up code
Bit#(5) excSys = 5'b01000;
Bit#(5) excBp = 5'b01001; // breakpoint  
Bit#(5) excRI = 5'b01010; // reserved instruction
Bit#(5) excOv = 5'b01100; // arithmetic overflow

Bit#(32) excHandlerPC = 32'h00008000;

Byte opNOP          = 8'h90;
Byte opMOV_AL_IMM8  = 8'hB0;

Bit#(6) opFUNC  = 6'b000000;
Bit#(6) opRT    = 6'b000001;
Bit#(6) opRS    = 6'b010000;
                            
Bit#(6) opLB    = 6'b100000;
Bit#(6) opLH    = 6'b100001;
Bit#(6) opLW    = 6'b100011;
Bit#(6) opLBU   = 6'b100100;
Bit#(6) opLHU   = 6'b100101;
Bit#(6) opSB    = 6'b101000;
Bit#(6) opSH    = 6'b101001;
Bit#(6) opSW    = 6'b101011;
                            
Bit#(6) opADDIU = 6'b001001;
Bit#(6) opSLTI  = 6'b001010;
Bit#(6) opSLTIU = 6'b001011;
Bit#(6) opANDI  = 6'b001100;
Bit#(6) opORI   = 6'b001101;
Bit#(6) opXORI  = 6'b001110;
Bit#(6) opLUI   = 6'b001111;
                            
Bit#(6) opJ     = 6'b000010;
Bit#(6) opJAL   = 6'b000011;
Bit#(6) fcJR    = 6'b001000;
Bit#(6) fcJALR  = 6'b001001;
Bit#(6) opBEQ   = 6'b000100;
Bit#(6) opBNE   = 6'b000101;
Bit#(6) opBLEZ  = 6'b000110;
Bit#(6) opBGTZ  = 6'b000111;
Bit#(5) rtBLTZ  = 5'b00000;
Bit#(5) rtBGEZ  = 5'b00001;

Bit#(6) fcSYSCALL = 6'b001100;

Bit#(5) rsMFC0  = 5'b00000;
Bit#(5) rsMTC0  = 5'b00100;
Bit#(5) rsERET  = 5'b10000;

Bit#(6) fcERET  = 6'b011000;

Bit#(6) fcSLL   = 6'b000000;
Bit#(6) fcSRL   = 6'b000010;
Bit#(6) fcSRA   = 6'b000011;
Bit#(6) fcSLLV  = 6'b000100;
Bit#(6) fcSRLV  = 6'b000110;
Bit#(6) fcSRAV  = 6'b000111;
Bit#(6) fcMFHI 	= 6'b010000;
Bit#(6) fcMTHI 	= 6'b010001;
Bit#(6) fcMFLO 	= 6'b010010;
Bit#(6) fcMTLO 	= 6'b010011;
Bit#(6) fcADDU  = 6'b100001;
Bit#(6) fcSUBU  = 6'b100011;
Bit#(6) fcAND   = 6'b100100;
Bit#(6) fcOR    = 6'b100101;
Bit#(6) fcXOR   = 6'b100110;
Bit#(6) fcNOR   = 6'b100111;
Bit#(6) fcSLT   = 6'b101010;
Bit#(6) fcSLTU  = 6'b101011;
Bit#(6) fcMULT  = 6'b011000;
Bit#(6) fcMULTU = 6'b011001;

function Bool dataHazard(Maybe#(RIndx) src1, Maybe#(RIndx) src2, Maybe#(RIndx) dst);
    return (isValid(dst) && ((isValid(src1) && validValue(dst)==validValue(src1)) ||
                             (isValid(src2) && validValue(dst)==validValue(src2))));
endfunction

function Fmt showInst(Data inst);
    Fmt ret = $format("");
    let opcode = inst[ 31 : 26 ];
    let rs     = inst[ 25 : 21 ];
    let rt     = inst[ 20 : 16 ];
    let rd     = inst[ 15 : 11 ];
    let shamt  = inst[ 10 :  6 ];
    let funct  = inst[  5 :  0 ];
    let imm    = inst[ 15 :  0 ];
    let target = inst[ 25 :  0 ];

    case (opcode)
        opADDIU, opSLTI, opSLTIU, opANDI, opORI, opXORI, opLUI:
        begin
            ret = case (opcode)
                        opADDIU: $format("addiu");
                        opLUI:   $format("lui");
                        opSLTI:  $format("slti");
                        opSLTIU: $format("sltiu");
                        opANDI:  $format("andi");
                        opORI:   $format("ori");
                        opXORI:  $format("xori");
                    endcase;
            ret = ret + $format(" r%0d = r%0d ", rt, rs);
            ret = ret + (case (opcode)
                            opADDIU, opSLTI, opSLTIU: $format("0x%0x", imm);
                            opLUI: $format("0x%0x", {imm, 16'b0});
                            default: $format("0x%0x", imm);
                        endcase);
        end

        opLB, opLH, opLW, opLBU, opLHU:
        begin
            ret = case (opcode)
                        opLB:  $format("lb");
                        opLH:  $format("lh");
                        opLW:  $format("lw");
                        opLBU: $format("lbu");
                        opLHU: $format("lhu");
                    endcase;
            ret = ret + $format(" r%0d = r%0d 0x%0x", rt, rs, imm);
        end

        opSB, opSH, opSW:
        begin
            ret = case (opcode)
                        opSB: $format("sb");
                        opSH: $format("sh");
                        opSW: $format("sw");
                    endcase;
            ret = ret + $format(" r%0d r%0d 0x%0x", rs, rt, imm);
        end

        opJ, opJAL: ret = (opcode == opJ? $format("j ") : $format("jal ")) + $format("0x%0x", {target, 2'b00});
    
        opBEQ, opBNE, opBLEZ, opBGTZ, opRT:
        begin
            ret = case(opcode)
                        opBEQ:  $format("beq");
                        opBNE:  $format("bne");
                        opBLEZ: $format("blez");
                        opBGTZ: $format("bgtz");
                        opRT: (rt==rtBLTZ ? $format("bltz") : $format("bgez"));
                    endcase;
            ret = ret + $format(" r%0d ", rs) + ((opcode == opBEQ || opcode == opBNE)? $format("r%0d", rt) : $format("0x%0x", imm));
        end
    
        opRS: 
        begin
            case (rs)
                rsMFC0: ret = $format("mfc0 r%0d = [r%0d]", rt, rd);
                rsMTC0: ret = $format("mtc0 [r%0d] = r%0d", rd, rt);
                rsERET: ret = $format("eret");
            endcase
        end
    
        opFUNC:
        case(funct)
            fcJR, fcJALR:   ret = (funct == fcJR ? $format("jr") : $format("jalr")) + $format(" r%0d = r%0d", rd, rs);
            fcSLL, fcSRL, fcSRA:
            begin
                ret = case (funct)
                            fcSLL: $format("sll");
                            fcSRL: $format("srl");
                            fcSRA: $format("sra");
                        endcase;
                ret = ret + $format(" r%0d = r%0d ", rd, rt) + ((funct == fcSRA) ? $format(">>") : $format("<<")) + $format(" %0d", shamt);
            end
            fcSLLV, fcSRLV, fcSRAV: 
            begin
                ret = case (funct)
                            fcSLLV: $format("sllv");
                            fcSRLV: $format("srlv");
                            fcSRAV: $format("srav");
                        endcase;
                ret = ret + $format(" r%0d = r%0d r%0d", rd, rt, rs);
            end
            fcSYSCALL: ret = $format("syscall");  
            fcMTLO, fcMTHI:
            begin
                ret = case (funct)
                    fcMTLO: $format("mtlo");
                    fcMTHI: $format("mthi");
                endcase;
                ret = ret + $format(" r%0d", rs);
            end
            fcMFLO, fcMFHI:
            begin
                ret = case (funct)
                    fcMFLO: $format("mflo");
                    fcMFHI: $format("mfhi");
                endcase;
                ret = ret + $format(" r%0d", rd);
            end
            fcMULT: ret = $format("mult r%0d r%0d", rs, rt);
            fcMULTU: ret = $format("multu r%0d r%0d", rs, rt);
            default: 
            begin
                ret = case (funct)
                            fcADDU: $format("addu");
                            fcSUBU: $format("subu");
                            fcAND : $format("and");
                            fcOR  : $format("or");
                            fcXOR : $format("xor");
                            fcNOR : $format("nor");
                            fcSLT : $format("slt");
                            fcSLTU: $format("sltu");
                        endcase;
                ret = ret + $format(" r%0d = r%0d r%0d", rd, rs, rt);
            end
        endcase

        default: ret = $format("nop 0x%0x", inst);
    endcase

    return ret;
  
endfunction
