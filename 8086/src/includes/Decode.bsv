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
import Cop::*;

(* noinline *)
function DecodedInst decodeOpcode(Byte pInst);
    DecodedInst pdInst = ?;
    pdInst.opcode = pInst;
    pdInst.reqAddressMode = False;
    pdInst.reqLowDispAddr = False;
    pdInst.reqHighDispAddr = False;
    pdInst.reqLowData = False;
    pdInst.reqHighData = False;
    pdInst.mseg = DS;
    pdInst.src1 = tagged Invalid;
    pdInst.src2 = tagged Invalid;
    pdInst.src3 = tagged Invalid;
    pdInst.dst1 = tagged Invalid;
    pdInst.dst2 = tagged Invalid;
    pdInst.dstm = tagged Invalid;
    pdInst.srcValm = tagged Word (?);

    // BYTE 1
    // Operation (Instruction) Code
    //let opcode = pInst[ 7 : 2 ];

    // Direction is to register / Direction is from register
    // Tells whether the register which is selected by the r field in the second
    // byte is the source or destination (D = 0 => source, D = 1 => destination)
    //let d      = pInst[ 1 : 1 ];

    // Data size word/byte for all registers (Byte = 0, Word = 1)
    //let w      = pInst[ 0 : 0 ]; // Word/Byte Operation

    case (pdInst.opcode)
        'h00,   // ADD RM8, R8
        'h01,   // ADD RM16, R16
        'h08,   // OR RM8, R8
        'h09,   // OR RM16, R16
        'h10,   // ADC RM8, R8
        'h11,   // ADC RM16, R16
        'h18,   // SBB RM8, R8
        'h19,   // SBB RM16, R16
        'h20,   // AND RM8, R8
        'h21,   // AND RM16, R16
        'h28,   // SUB RM8, R8
        'h29,   // SUB RM16, R16
        'h30,   // XOR RM8, R8
        'h31,   // XOR RM16, R16
        'h38,   // CMP RM8, R8
        'h39:   // CMP RM16, R16
        begin
            pdInst.iType = ALU_RM_R;
            pdInst.reqAddressMode = True;
        end

        'h02,   // ADD R8, RM8
        'h03,   // ADD R16, RM16
        'h0A,   // OR R8, RM8
        'h0B,   // OR R16, RM16
        'h12,   // ADC R8, RM8
        'h13,   // ADC R16, RM16
        'h1A,   // SBB R8, RM8
        'h1B,   // SBB R16, RM16
        'h22,   // AND R8, RM8
        'h23,   // AND R16, RM16
        'h2A,   // SUB R8, RM8
        'h2B,   // SUB R16, RM16
        'h32,   // XOR R8, RM8
        'h33,   // XOR R16, RM16
        'h3A,   // CMP R8, RM8
        'h3B:   // CMP R16, RM16
        begin
            pdInst.iType = ALU_R_RM;
            pdInst.reqAddressMode = True;
        end

        'h04,   // ADD AL, IMM8
        'h0C,   // OR AL, IMM8
        'h14,   // ADC AL, IMM8
        'h1C,   // SBB AL, IMM8
        'h24,   // AND AL, IMM8
        'h2C,   // SUB AL, IMM8
        'h34,   // XOR AL, IMM8
        'h3C:   // CMP AL, IMM8
        begin
            pdInst.iType = ALU_R_IMM;
            pdInst.src1 = tagged Valid tagged RegByte AL;
            pdInst.dst1 = tagged Valid tagged RegByte AL;
            pdInst.reqLowData = True;
        end

        'h05,   // ADD AX, IMM16
        'h0D,   // OR AX, IMM16
        'h15,   // ADC AX, IMM16
        'h1D,   // SBB AX, IMM16
        'h25,   // AND AX, IMM16
        'h2D,   // SUB AX, IMM16
        'h35,   // XOR AX, IMM16
        'h3D:   // CMP AX, IMM16
        begin
            pdInst.iType = ALU_R_IMM;
            pdInst.src1 = tagged Valid tagged RegWord AX;
            pdInst.dst1 = tagged Valid tagged RegWord AX;
            pdInst.reqLowData = True;
            pdInst.reqHighData = True;
        end

        /*'h27:   // DAA
        begin
            pdInst.iType = DAA;
            pdInst.src1 = tagged Valid tagged RegByte AL;
            pdInst.dst1 = tagged Valid tagged RegByte AL;
        end

        'h2F:   // DAS
        begin
            pdInst.iType = DAS;
            pdInst.src1 = tagged Valid tagged RegByte AL;
            pdInst.dst1 = tagged Valid tagged RegByte AL;
        end*/

        //  INC R16
        'h40, 'h41, 'h42, 'h43, 'h44, 'h45, 'h46, 'h47:
        begin
            pdInst.iType = INC;
            pdInst.src1 = tagged Valid tagged RegWord unpack(pInst[2:0]);
            pdInst.dst1 = tagged Valid tagged RegWord unpack(pInst[2:0]);
        end

        //  DEC R16
        'h48, 'h49, 'h4A, 'h4B, 'h4C, 'h4D, 'h4E, 'h4F:
        begin
            pdInst.iType = DEC;
            pdInst.src1 = tagged Valid tagged RegWord unpack(pInst[2:0]);
            pdInst.dst1 = tagged Valid tagged RegWord unpack(pInst[2:0]);
        end

        // PUSH R16
        'h50, 'h51, 'h52, 'h53, 'h54, 'h55, 'h56, 'h57:
        begin
            pdInst.iType = PUSH_R16;
            pdInst.src1 = tagged Valid tagged RegWord SP;
            pdInst.src2 = tagged Valid tagged RegWord unpack(pInst[2:0]);
            pdInst.dst1 = tagged Valid tagged RegWord SP;
        end

        // PUSH R16
        'h58, 'h59, 'h5A, 'h5B, 'h5C, 'h5D, 'h5E, 'h5F:
        begin
            pdInst.iType = POP_R16;
            pdInst.src1 = tagged Valid tagged RegWord SP;
            pdInst.dst2 = tagged Valid tagged RegWord unpack(pInst[2:0]);
            pdInst.dst1 = tagged Valid tagged RegWord SP;
        end

        // 'h60 - 'h6F -- not used

        'h74:   // JE SHORT-LABEL
        begin
            pdInst.iType = JE;
            pdInst.reqLowData = True;
        end

        'h80, 'h82:   // ALU RM8, IMM8
        begin            
            pdInst.iType = ALU_RM8_IMM8;
            pdInst.reqAddressMode = True;
            pdInst.reqLowData = True;
        end

        'h81:   // ALU RM16, IMM16
        begin            
            pdInst.iType = ALU_RM16_IMM16;
            pdInst.reqAddressMode = True;
            pdInst.reqLowData = True;
            pdInst.reqHighData = True;
        end

        'h83:   // ALU RM16, IMM8
        begin            
            pdInst.iType = ALU_RM16_SIMM8;
            pdInst.reqAddressMode = True;
            pdInst.reqLowData = True;
        end

        'h88,   // MOV RM8,  R8
        'h89:   // MOV RM16, R16
        begin
            pdInst.iType = MOV_RM_R;
            pdInst.reqAddressMode = True;
        end

        'h8A:   // MOV R8,  RM8
        begin
            pdInst.iType = MOV_R_RM;            
            pdInst.srcValm = tagged Byte (?);
            pdInst.reqAddressMode = True;
        end

        'h8B:   // MOV R16, RM16
        begin
            pdInst.iType = MOV_R_RM;
            pdInst.reqAddressMode = True;
        end

        'h8D:   // LEA R16, M16
        begin
            pdInst.iType = LEA;
            pdInst.reqAddressMode = True;
        end

        'h90:   // NOP (XCHG AX, AX)
        begin
            pdInst.iType = NOP;
        end

        //  XCHG AX, R16
        'h91, 'h92, 'h93, 'h94, 'h95, 'h96, 'h97:
        begin
            pdInst.iType = XCHG_R_R;
            pdInst.src1 = tagged Valid tagged RegWord AX;
            pdInst.dst1 = tagged Valid tagged RegWord AX;
            pdInst.src2 = tagged Valid tagged RegWord unpack(pInst[2:0]);
            pdInst.dst2 = tagged Valid tagged RegWord unpack(pInst[2:0]);
        end

        'hA0:   // MOV AL, M8
        begin
            pdInst.iType = MOV_R_RM;
            pdInst.mod = 'b00;
            pdInst.rm = 'b110;
            pdInst.srcValm = tagged Byte (?);
            pdInst.dst1 = tagged Valid tagged RegByte AL;
            pdInst.reqLowDispAddr = True;
            pdInst.reqHighDispAddr = True;
        end

        'hA1:   // MOV AX, M16
        begin
            pdInst.iType = MOV_R_RM;
            pdInst.mod = 'b00;
            pdInst.rm = 'b110;
            pdInst.dst1 = tagged Valid tagged RegWord AX;
            pdInst.reqLowDispAddr = True;
            pdInst.reqHighDispAddr = True;
        end

        'hA2:   // MOV M8, AL
        begin
            pdInst.iType = MOV_R_RM;
            pdInst.mod = 'b00;
            pdInst.rm = 'b110;
            pdInst.src1 = tagged Valid tagged RegByte AL;
            pdInst.reqLowDispAddr = True;
            pdInst.reqHighDispAddr = True;
        end

        'hA3:   // MOV M16, AX
        begin
            pdInst.iType = MOV_R_RM;
            pdInst.mod = 'b00;
            pdInst.rm = 'b110;
            pdInst.src1 = tagged Valid tagged RegWord AX;
            pdInst.reqLowDispAddr = True;
            pdInst.reqHighDispAddr = True;
        end

        //  MOV R8, IMM8
        'hB0, 'hB1, 'hB2, 'hB3, 'hB4, 'hB5, 'hB6, 'hB7:
        begin
            pdInst.iType = MOV_R_IMM;
            pdInst.dst1 = tagged Valid tagged RegByte unpack(pInst[2:0]);
            pdInst.reqLowData = True;
        end

        //  MOV R16, IMM16
        'hB8, 'hB9, 'hBA, 'hBB, 'hBC, 'hBD, 'hBE, 'hBF:
        begin
            pdInst.iType = MOV_R_IMM;
            pdInst.dst1 = tagged Valid tagged RegWord unpack(pInst[2:0]);
            pdInst.reqLowData = True;
            pdInst.reqHighData = True;
        end

        'hC3:   // RET (intresegment)
        begin
            pdInst.iType = RET;
            pdInst.src1 = tagged Valid tagged RegWord SP;
            pdInst.dst1 = tagged Valid tagged RegWord SP;
        end

        'hC7:   // MOV (R)M16, IMM16
        begin
            pdInst.iType = MOV_M_IMM16;
            pdInst.reqAddressMode = True;
            pdInst.reqLowData = True;
            pdInst.reqHighData = True;
        end

        'hE6:   // OUT AL, IMM8
        begin
            pdInst.iType = OUT;
            pdInst.src1 = tagged Valid tagged RegByte AL;
            pdInst.reqLowData = True;
        end

        'hE8:   // CALL NEAR-PROC
        begin
            pdInst.iType = CALL_NEAR;
            pdInst.reqLowData = True;
            pdInst.reqHighData = True;
            pdInst.src1 = tagged Valid tagged RegWord SP;
            pdInst.dst1 = tagged Valid tagged RegWord SP;
        end

        'hE9:   // JMP NEAR <offset16>
        begin
            pdInst.iType = JMP_NEAR;
            pdInst.reqLowData = True;
            pdInst.reqHighData = True;
        end

        'hEB:   // JMP SHORT <offset8>
        begin
            pdInst.iType = JMP_SHORT;
            pdInst.reqLowData = True;
        end

        'hF5:   // CMC
        begin
            pdInst.iType = CMC;
        end

        'hF8:   // CLC
        begin
            pdInst.iType = CLC;
        end

        'hF9:   // STC
        begin
            pdInst.iType = STC;
        end

        'hFA:   // CLI
        begin
            pdInst.iType = CLI;
        end

        'hFB:   // STI
        begin
            pdInst.iType = STI;
        end

        'hFC:   // CLD
        begin
            pdInst.iType = CLD;
        end

        'hFD:   // STD
        begin
            pdInst.iType = STD;
        end

        default:
        begin
            pdInst.iType = Unsupported;
        end
    endcase

    return pdInst;
endfunction

function Bool getReqLowDispAddr(Bit#(2) mod, Bit#(3) rm) = (mod == 'b00 && rm == 'b110) || mod == 'b01 || mod == 'b10;
function Bool getReqHighDispAddr(Bit#(2) mod, Bit#(3) rm) = (mod == 'b00 && rm == 'b110) || mod == 'b10;
function Maybe#(RegNumber) getFirstRegForMem(Bit#(2) mod, Bit#(3) rm);
    if (mod == 'b00 && rm == 'b110 || mod == 'b11)
        return tagged Invalid;
    else
        case (rm)
            'b000, 'b001, 'b111: return tagged Valid tagged RegWord BX;
            'b010, 'b011, 'b110: return tagged Valid tagged RegWord BP;
            'b100: return tagged Valid tagged RegWord SI;
            'b101: return tagged Valid tagged RegWord DI;
        endcase
endfunction
function Maybe#(RegNumber) getSecondRegForMem(Bit#(2) mod, Bit#(3) rm);
    if (mod == 'b00 && rm == 'b110 || mod == 'b11)
        return tagged Invalid;
    else
        case (rm)
            'b000, 'b010: return tagged Valid tagged RegWord SI;
            'b001, 'b011: return tagged Valid tagged RegWord DI;
            default: return tagged Invalid;
        endcase
endfunction
function Maybe#(RegNumber) getFirstRegForReg(Bit#(2) mod, Bit#(3) rm, Bit#(1) w);
    if (mod != 'b11)
        return tagged Invalid;
    else if (w == 0) // 8-bit register
        return tagged Valid tagged RegByte (unpack(rm));
    else // 16-bit register
        return tagged Valid tagged RegWord (unpack(rm));
endfunction

(* noinline *)
function DecodedInst decodeAddressingMode(DecodedInst pdInst, Byte addressMode);
    let mod = addressMode[7:6];
    let r = addressMode[5:3];
    let rm = addressMode[2:0];
    Bit#(1) w = pdInst.opcode[0];

    pdInst.mod = mod;
    pdInst.r = r;
    pdInst.rm = rm;

    pdInst.reqLowDispAddr = getReqLowDispAddr(mod, rm);
    pdInst.reqHighDispAddr = getReqHighDispAddr(mod, rm);

    case (pdInst.opcode)
        'h00, 'h01, 'h08, 'h09, 'h10, 'h11, 'h18, 'h19,
        'h20, 'h21, 'h28, 'h29, 'h30, 'h31, 'h38, 'h39,
        'h02, 'h03, 'h0A, 'h0B, 'h12, 'h13, 'h1A, 'h1B,
        'h22, 'h23, 'h2A, 'h2B, 'h32, 'h33, 'h3A, 'h3B,
        'h88, 'h89: // Instructions which need reading Reg
        begin
            if (w == 0) // 8 bit
                pdInst.src1 = tagged Valid tagged RegByte unpack(r);
            else
                pdInst.src1 = tagged Valid tagged RegWord unpack(r);
        end
    endcase
    
    case (pdInst.opcode)
        'h02, 'h03, 'h0A, 'h0B, 'h12, 'h13, 'h1A, 'h1B,
        'h22, 'h23, 'h2A,
        'h8A, 'h8B, 'h8D: // Instructions which need writing Reg
        begin
            if (w == 0 && pdInst.opcode != 'h8D) // 8 bit
                pdInst.dst1 = tagged Valid tagged RegByte unpack(r);
            else
                pdInst.dst1 = tagged Valid tagged RegWord unpack(r);
        end
    endcase

    case (pdInst.opcode)
        'h00, 'h01, 'h08, 'h09, 'h10, 'h11, 'h18, 'h19,
        'h20, 'h21, 'h28, 'h29, 'h30, 'h31, 'h38, 'h39,
        'h02, 'h03, 'h0A, 'h0B, 'h12, 'h13, 'h1A, 'h1B,
        'h22, 'h23, 'h2A, 'h2B, 'h32, 'h33, 'h3A, 'h3B,
        'h80, 'h81, 'h82, 'h83, 'h8A, 'h8B, 'hC5, 'h8D: // Instructions which need reading R/M
        begin
            let firstRegForMem = getFirstRegForMem(mod, rm);
            let firstRegForReg = getFirstRegForReg(mod, rm, w);
            let secondRegForMem = getSecondRegForMem(mod, rm);
            if (isValid(firstRegForMem))
                pdInst.src2 = firstRegForMem;
            else if (isValid(firstRegForReg))
                pdInst.src2 = firstRegForReg;
            if (isValid(secondRegForMem))
                pdInst.src3 = secondRegForMem;
        end
    endcase

    case (pdInst.opcode)
        'h00, 'h01, 'h08, 'h09, 'h10, 'h11, 'h18, 'h19,
        'h20, 'h21, 'h28, 'h29,
        'h80, 'h81, 'h82, 'h83, 'h88, 'h89: // Instructions which need writing R/M
        begin
            if (!((pdInst.opcode == 'h80 || pdInst.opcode == 'h81 || pdInst.opcode == 'h82 || pdInst.opcode == 'h83) && pdInst.r == 'b111)) begin
                let firstRegForMem = getFirstRegForMem(mod, rm);
                let firstRegForReg = getFirstRegForReg(mod, rm, w);
                let secondRegForMem = getSecondRegForMem(mod, rm);
                if (isValid(firstRegForMem))
                    pdInst.src2 = firstRegForMem;
                else if (isValid(firstRegForReg))
                    pdInst.dst2 = firstRegForReg;
                if (isValid(secondRegForMem))
                    pdInst.src3 = secondRegForMem;
            end
        end
    endcase

    return pdInst;
endfunction

(* noinline *)
function DecodedInst decodeLowData(DecodedInst pdInst, Byte lowData);
    case (pdInst.opcode)
        'hE6:   // OUT AL, IMM8
        begin
            pdInst.dstp = tagged Valid zeroExtend(lowData);
        end

        default:
        begin
            pdInst.srcimm = tagged Byte lowData;
        end
    endcase
    return pdInst;
endfunction

(* noinline *)
function DecodedInst decodeHighData(DecodedInst pdInst, Byte highData);
    pdInst.srcimm = tagged Word ({highData, pdInst.srcimm.Byte});
    return pdInst;
endfunction

(* noinline *)
function DecodedInst decodeRFToMemoryFetch(DecodedInst pdInst);
    let mod = pdInst.mod;
    let r = pdInst.r;
    let rm = pdInst.rm;

    case (pdInst.opcode)
        'hA0, 'hA1:
        begin
            pdInst.srcm = getMemAddrFromModRm('b00, 'b110, pdInst);
        end

        'h00, 'h01, 'h08, 'h09, 'h10, 'h11, 'h18, 'h19,
        'h20, 'h21, 'h28, 'h29, 'h30, 'h31, 'h38, 'h39,
        'h02, 'h03, 'h0A, 'h0B, 'h12, 'h13, 'h1A, 'h1B,
        'h22, 'h23, 'h2A, 'h2B, 'h32, 'h33, 'h3A, 'h3B,
        'h80, 'h81, 'h82, 'h83, 'h8A, 'h8B:
        begin
            pdInst.srcm = getMemAddrFromModRm(mod, rm, pdInst);
        end
        'h58, 'h59, 'h5A, 'h5B, 'h5C, 'h5D, 'h5E, 'h5F, 'hC3:
        begin
            pdInst.srcm = tagged Valid SegAddr { seg: SS, offset: pdInst.srcVal1.Word - 2 }; 
        end
        'h8D:
        begin
            pdInst.srcimm = tagged Word getMemAddrFromModRm(mod, rm, pdInst).Valid.offset;
        end
    endcase

    case (pdInst.opcode)
        'h00, 'h01, 'h08, 'h09, 'h10, 'h11, 'h18, 'h19,
        'h20, 'h21, 'h28, 'h29,
        'h80, 'h81, 'h82, 'h83, 'h88, 'h89, 'hA2, 'hA3, 'hC7:
        begin
            pdInst.dstm = getMemAddrFromModRm(mod, rm, pdInst);
        end
        'hE8:
        begin
            pdInst.dstm = tagged Valid SegAddr { seg: SS, offset: pdInst.srcVal1.Word };
        end
    endcase

    return pdInst;
endfunction

/*
function DecodedInst decode(Byte pInst);
    DecodedInst dInst = ?;

    // BYTE 1
    // Operation (Instruction) Code
    let opcode = inst[ 15 : 10 ];

    // Direction is to register / Direction is from register
    // Tells whether the register which is selected by the r field in the second
    // byte is the source or destination (D = 0 => source, D = 1 => destination)
    let d      = inst[ 9 : 9 ];

    // Data size word/byte for all registers (Byte = 0, Word = 1)
    let w      = inst[ 8 : 8 ]; // Word/Byte Operation

    // BYTE 2
    // Register mode/Memory mode with displacement length
    //   Memory mode = register to memory move
    //   Register mode = register to register move
    // 00 = Memory mode, no displacement follows (except when rm = 110, then 16-bit displacement follows)
    // 01 = Memory Mode, 8-bit displacement follows
    // 10 = Memory Mode, 16-bit displacement follows
    // 11 = Register Mode, no displacement
    let mod    = inst[ 7 : 6 ];

    // Register operand/Extension of opcode - selects register for first operand
    // REG | W = 0 | W = 1 |
    // 000 |   AL  |   AX  |
    // 001 |   CL  |   CX  |
    // 010 |   DL  |   DX  |
    // 011 |   BL  |   BX  |
    // 100 |   AH  |   SP  |
    // 101 |   CH  |   BP  |
    // 110 |   DH  |   SI  |
    // 111 |   BH  |   DI  |
    let r      = inst[ 5 : 3 ];

    // Register operand/Registers to user in EA calculation - specfies the second operand as a register or a memory location
    // If MOD = 11, same as before
    // 

    let rm     = inst[ 2 : 0 ];
   /*let lodisp = inst[ 31 : 24 ]; // Low Displacement/Data
    let hidisp = inst[ 23 : 16 ]; // High Displacement/Data
    let lodata = inst[ 15 :  8 ]; // Low Data
    let hidata = inst[  7 :  0 ]; // High Data*/
/*
    case (opcode)
        opADDIU, opSLTI, opSLTIU, opANDI, opORI, opXORI, opLUI:
        begin
            dInst.iType = Alu;
            dInst.aluFunc = case (opcode)
                opADDIU, opLUI: Add;
                opSLTI: Slt;
                opSLTIU: Sltu;
                opANDI: And;
                opORI: Or;
                opXORI: Xor;
            endcase;
            dInst.dst = validReg(rt);
            dInst.src1 = validReg(rs);
            dInst.src2 = Invalid;
            dInst.imm = Valid(case (opcode)
                opADDIU, opSLTI, opSLTIU: signExtend(imm);
                opLUI: {imm, 16'b0};
                default: zeroExtend(imm);
            endcase);
            dInst.brFunc = NT;
        end

        opLB, opLH, opLW, opLBU, opLHU:
        begin
            dInst.iType = Ld;
            dInst.aluFunc = Add;
            dInst.dst = validReg(rt);
            dInst.src1 = validReg(rs);
            dInst.src2 = Invalid;
            dInst.imm = Valid(signExtend(imm));
            dInst.brFunc = NT;
        end

        opSB, opSH, opSW:
        begin
            dInst.iType = St;
            dInst.aluFunc = Add;
            dInst.dst = Invalid;
            dInst.src1 = validReg(rs);
            dInst.src2 = validReg(rt);
            dInst.imm = Valid(signExtend(imm));
            dInst.brFunc = NT;
        end

        opJ, opJAL:
        begin
            dInst.iType = J;
            dInst.dst = opcode == opJ? Invalid: validReg(31);
            dInst.src1 = Invalid;
            dInst.src2 = Invalid;
            dInst.imm = Valid(zeroExtend({target,2'b00}));
            dInst.brFunc = AT;
        end

        opBEQ, opBNE, opBLEZ, opBGTZ, opRT:
        begin
            dInst.iType = Br;
            dInst.brFunc = case(opcode)
                opBEQ: Eq;
                opBNE: Neq;
                opBLEZ: Le;
                opBGTZ: Gt;
                opRT: (rt==rtBLTZ ? Lt : Ge);
            endcase;
            dInst.dst = Invalid;
            dInst.src1 = validReg(rs);
            dInst.src2 = (opcode==opBEQ || opcode==opBNE)? validReg(rt) : Invalid;
            dInst.imm = Valid(signExtend(imm) << 2);
        end

        opRS: 
        begin
            if (isUserMode) begin
                dInst.iType = Illegal;
                dInst.dst = Invalid;
                dInst.src1 = Invalid;
                dInst.src2 = Invalid;
                dInst.imm = Invalid;
                dInst.brFunc = NT;
            end else begin
                case (rs)
                    rsMFC0:
                    begin
                        dInst.iType = Mfc0;
                        dInst.dst = validReg(rt);
                        dInst.src1 = validCop(rd);
                        dInst.src2 = Invalid;
                        dInst.imm = Invalid;
                        dInst.brFunc = NT;
                    end
                    rsMTC0:
                    begin
                        dInst.iType = Mtc0;
                        dInst.dst = validCop(rd);
                        dInst.src1 = validReg(rt);
                        dInst.src2 = Invalid;
                        dInst.imm = Invalid;
                        dInst.brFunc = NT;
                    end
                    rsERET:
                    begin                        
                        dInst.iType = ERet;
                        dInst.dst = Invalid;
                        dInst.src1 = Invalid;
                        dInst.src2 = Invalid;
                        dInst.imm = Invalid;
                        dInst.brFunc = AT;
                    end
                endcase
            end
        end

        opFUNC:
        case(funct)
            fcJR, fcJALR:
            begin
                dInst.iType = Jr;
                dInst.dst = funct == fcJR? Invalid: validReg(rd);
                dInst.src1 = validReg(rs);
                dInst.src2 = Invalid;
                dInst.imm = Invalid;
                dInst.brFunc = AT;
            end
            
            fcSLL, fcSRL, fcSRA:
            begin
                dInst.iType = Alu;
                dInst.aluFunc = case (funct)
                    fcSLL: LShift;
                    fcSRL: RShift;
                    fcSRA: Sra;
                endcase;
                dInst.dst = validReg(rd);
                dInst.src1 = validReg(rt);
                dInst.src2 = Invalid;
                dInst.imm = Valid(zeroExtend(shamt));
                dInst.brFunc = NT;
            end

            fcSLLV, fcSRLV, fcSRAV: 
            begin
                dInst.iType = Alu;
                dInst.aluFunc = case (funct)
                    fcSLLV: LShift;
                    fcSRLV: RShift;
                    fcSRAV: Sra;
                endcase;
                dInst.dst = validReg(rd);
                dInst.src1 = validReg(rt);
                dInst.src2 = validReg(rs);
                dInst.imm = Invalid;
                dInst.brFunc = NT;
            end

            fcMFLO, fcMFHI:
            begin
                dInst.iType = (funct==fcMFLO)?Mflo:Mfhi;
                dInst.dst  = validReg(rd);
                dInst.src1 = Invalid;
                dInst.src2 = Invalid;
                dInst.imm  = Invalid;
                dInst.brFunc = NT;
            end

            fcMTLO, fcMTHI:
            begin
                dInst.iType = (funct==fcMTLO)?Mtlo:Mthi;
                dInst.dst  = Invalid;
                dInst.src1 = validReg(rs);
                dInst.src2 = Invalid;
                dInst.imm  = Invalid;
                dInst.brFunc = NT;
            end

            fcADDU, fcSUBU, fcAND, fcOR, fcXOR, fcNOR, fcSLT, fcSLTU:
            begin
                dInst.iType = Alu;
                dInst.aluFunc = case (funct)
                    fcADDU: Add;
                    fcSUBU: Sub;
                    fcAND : And;
                    fcOR  : Or;
                    fcXOR : Xor;
                    fcNOR : Nor;
                    fcSLT : Slt;
                    fcSLTU: Sltu;
                endcase;
                dInst.dst = validReg(rd);
                dInst.src1 = validReg(rs);
                dInst.src2 = validReg(rt);
                dInst.imm = Invalid;
                dInst.brFunc = NT;
            end

            fcMULT, fcMULTU:
            begin
                dInst.iType = Unsupported;
                dInst.dst = Invalid;
                dInst.src1 = Invalid;
                dInst.src2 = Invalid;
                dInst.imm = Invalid;
                dInst.brFunc = NT;
            end

            fcSYSCALL:
            begin
                dInst.iType = Syscall;
                dInst.dst = Invalid;
                dInst.src1 = Invalid;
                dInst.src2 = Invalid;
                dInst.imm = Invalid;
                dInst.brFunc = NT;
            end

            default: 
                begin
                    dInst.iType = Unsupported;
                    dInst.dst = Invalid;
                    dInst.src1 = Invalid;
                    dInst.src2 = Invalid;
                    dInst.imm = Invalid;
                    dInst.brFunc = NT;
                end
        endcase

        default: 
        begin
            dInst.iType = Unsupported;
            dInst.dst = Invalid;
            dInst.src1 = Invalid;
            dInst.src2 = Invalid;
            dInst.imm = Invalid;
            dInst.brFunc = NT;
        end
    endcase

    if(dInst.dst matches tagged Valid .dst &&& dst.regType == Normal &&& dst.idx == 0) begin
        dInst.dst = tagged Invalid;
    end

    return dInst;
endfunction

*/