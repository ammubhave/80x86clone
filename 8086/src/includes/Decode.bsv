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
        'h40, 'h41, 'h42, 'h43, 'h44, 'h45, 'h46, 'h47: // done
        begin
            pdInst.iType = INC;
            pdInst.src1 = tagged Valid tagged RegWord unpack(pInst[2:0]);
            pdInst.dst1 = tagged Valid tagged RegWord unpack(pInst[2:0]);
        end

        'h48, 'h49, 'h4A, 'h4B, 'h4C, 'h4D, 'h4E, 'h4F: // done
        begin
            pdInst.iType = DEC;
            pdInst.src1 = tagged Valid tagged RegWord unpack(pInst[2:0]);
            pdInst.dst1 = tagged Valid tagged RegWord unpack(pInst[2:0]);
        end

        'h50, 'h51, 'h52, 'h53, 'h54, 'h55, 'h56, 'h57:
        begin
            pdInst.src1 = tagged Valid tagged RegWord unpack(pInst[2:0]);
        end

        'h58, 'h59, 'h5A, 'h5B, 'h5C, 'h5D, 'h5E, 'h5F:
        begin
            pdInst.dst1 = tagged Valid tagged RegWord unpack(pInst[2:0]);
        end

        // 'h60 - 'h6F -- not used

        'h90:
        begin
            pdInst.iType = NOP;
        end

        'hB0, 'hB1, 'hB2, 'hB3, 'hB4, 'hB5, 'hB6, 'hB7: // done
        begin
            pdInst.iType = MOV_R_IMM8;
            pdInst.src2 = tagged Valid tagged RegWord unpack({0, pInst[1:0]});
            pdInst.dst1 = tagged Valid tagged RegByte unpack(pInst[2:0]);
            pdInst.reqLowData = True;
        end

        'hB8, 'hB9, 'hBA, 'hBB, 'hBC, 'hBD, 'hBE, 'hBF: // done
        begin
            pdInst.iType = MOV_R_IMM16;
            pdInst.dst1 = tagged Valid tagged RegWord unpack(pInst[2:0]);
            pdInst.reqLowData = True;
            pdInst.reqHighData = True;
        end

        'hC7:
        begin
            pdInst.iType = MOV_M_IMM16;
            pdInst.reqAddressMode = True;
            pdInst.reqLowData = True;
            pdInst.reqHighData = True;
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
function Maybe#(RegWord) getFirstRegForMem(Bit#(2) mod, Bit#(3) rm);
    if (mod == 'b00 && rm == 'b110 || mod == 'b11)
        return tagged Invalid;
    else
        case (rm)
            'b000, 'b001, 'b111: return tagged Valid BX;
            'b010, 'b011, 'b110: return tagged Valid BP;
            'b100: return tagged Valid SI;
            'b101: return tagged Valid DI;
        endcase
endfunction
function Maybe#(RegWord) getSecondRegForMem(Bit#(2) mod, Bit#(3) rm);
    if (mod == 'b00 && rm == 'b110 || mod == 'b11)
        return tagged Invalid;
    else
        case (rm)
            'b000, 'b010: return tagged Valid SI;
            'b001, 'b011: return tagged Valid DI;
            default: return tagged Invalid;
        endcase
endfunction
function Maybe#(RegWord) getFirstRegForReg(Bit#(2) mod, Bit#(3) rm, Bit#(1) w);
    if (mod != 'b11)
        return tagged Invalid;
    else if (w == 0) // 8-bit register
        return tagged Valid (unpack({0, rm[1:0]}));
    else // 16-bit register
        return tagged Valid (unpack(rm));
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
        'h28: // Instructions which need reading Reg
        begin
            if (w == 0) // 8 bit
                pdInst.src1 = tagged Valid tagged RegWord unpack({0, r[1:0]});
            else
                pdInst.src1 = tagged Valid tagged RegWord unpack(r);
        end
    endcase
    

    case (pdInst.opcode)
        'hC5: // Instructions which need reading R/M
        begin
            let firstRegForMem = getFirstRegForMem(mod, rm);
            let firstRegForReg = getFirstRegForReg(mod, rm, w);
            let secondRegForMem = getSecondRegForMem(mod, rm);
            if (isValid(firstRegForMem))
                pdInst.src2 = tagged Valid tagged RegWord validValue(firstRegForMem);
            else if (isValid(firstRegForReg))
                pdInst.src2 = tagged Valid tagged RegWord validValue(firstRegForReg);
            if (isValid(secondRegForMem))
                pdInst.src3 = tagged Valid tagged RegWord validValue(secondRegForMem);
        end
    endcase

    return pdInst;
endfunction

(* noinline *)
function DecodedInst decodeLowData(DecodedInst pdInst, Byte lowData);
    case (pdInst.opcode)
        'hB0, 'hB1, 'hB2, 'hB3, 'hB4, 'hB5, 'hB6, 'hB7:
        begin
            pdInst.src1 = tagged Valid tagged Imm8 lowData;
        end
        'hB8, 'hB9, 'hBA, 'hBB, 'hBC, 'hBD, 'hBE, 'hBF, 'hC7:
        begin
            pdInst.src1 = tagged Valid tagged Imm16 ({8'h00, lowData});
        end
        default:
        begin
            //$display("Unexpected decodeLowData pdInst: ", fshow(pdInst));
            //$finish;
        end
    endcase

    return pdInst;
endfunction

(* noinline *)
function DecodedInst decodeHighData(DecodedInst pdInst, Byte highData);
    case (pdInst.opcode)
        'hB8, 'hB9, 'hBA, 'hBB, 'hBC, 'hBD, 'hBE, 'hBF, 'hC7:
        begin
            pdInst.src1 = tagged Valid tagged Imm16 ({highData, truncate(pdInst.src1.Valid.Imm16)});
        end
        default:
        begin
            //$fwrite(stderr, "Unexpected decodeHighData pdInst: %x. Exiting\n", fshow(pdInst));
            //$finish;
        end
    endcase

    return pdInst;
endfunction

function Maybe#(SegAddr) getMemAddrFromModRm(Bit#(2) mod, Bit#(3) rm, DecodedInst pdInst);
    if (mod == 'b00 && rm == 'b110)
        return tagged Valid (SegAddr{seg: pdInst.mseg, offset: zeroExtend(pdInst.lowDispAddr) + (zeroExtend(pdInst.highDispAddr) << 8)});
    else if (mod == 'b00 && rm [2] == 0)
        return tagged Valid (SegAddr{seg: pdInst.mseg, offset: pdInst.srcVal2 + pdInst.srcVal3});
    else if (mod == 'b00 && rm [2] == 1)
        return tagged Valid (SegAddr{seg: pdInst.mseg, offset: pdInst.srcVal2});
    else if (mod == 'b01 && rm [2] == 0)
        return tagged Valid (SegAddr{seg: pdInst.mseg, offset: pdInst.srcVal2 + pdInst.srcVal3 + zeroExtend(pdInst.lowDispAddr)});
    else if (mod == 'b01 && rm [2] == 1)
        return tagged Valid (SegAddr{seg: pdInst.mseg, offset: pdInst.srcVal2 + zeroExtend(pdInst.lowDispAddr)});
    else if (mod == 'b10 && rm [2] == 0)
        return tagged Valid (SegAddr{seg: pdInst.mseg, offset: pdInst.srcVal2 + pdInst.srcVal3 + zeroExtend(pdInst.lowDispAddr) + (zeroExtend(pdInst.highDispAddr) << 8)});
    else if (mod == 'b10 && rm [2] == 1)
        return tagged Valid (SegAddr{seg: pdInst.mseg, offset: pdInst.srcVal2 + zeroExtend(pdInst.lowDispAddr) + (zeroExtend(pdInst.highDispAddr) << 8)});
    else
        return tagged Invalid;
endfunction

(* noinline *)
function DecodedInst decodeRFToMemoryFetch(DecodedInst pdInst);
    let mod = pdInst.mod;
    let r = pdInst.r;
    let rm = pdInst.rm;

    case (pdInst.opcode)
        'hC7:
        begin
            let srcm = getMemAddrFromModRm(mod, rm, pdInst);
            if (isValid(srcm))
                pdInst.srcm = srcm;
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