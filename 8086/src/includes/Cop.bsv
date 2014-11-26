/*

Copyright (C) 2012 Muralidaran Vijayaraghavan <vmurali@csail.mit.edu>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/


import Types::*;
import ProcTypes::*;
import Ehr::*;
import ConfigReg::*;
import Fifo::*;

interface Cop;
    method Action start;
    method Bool started;
    method Data rd(RIndx idx);
    //method Action wr(Maybe#(FullIndx) idx, Data val);
    method Action wr(Maybe#(Word) idx, Number val);
    method ActionValue#(Tuple2#(RIndx, Data)) cpuToHost;

    // external interface to status, cause, and epc registers
    method Action causeException(Addr current_pc, Bit#(5) cause);
    method Action returnFromException;
    method Addr getEPC;
    method Bool isUserMode;
endinterface

(* synthesize *)
module mkCop(Cop);
    Reg#(Bool) startReg <- mkReg(False);

    // FIFO for writing to co-processor registers that send messages to the host
    Fifo#(2, Tuple2#(RIndx, Data)) copFifo <- mkCFFifo;

    // Co-processor registers:
    // 10 - Number of clock cycles elapsed
    Reg#(Data) cycles   <- mkReg(0);
    // 11 - Number of instructions executed
    Reg#(Data) numInsts <- mkReg(0);
    // 12 - Status Register
    // Stores a 3 element stack of kernel/user mode and interrupt enable bits. The
    // current kernel/user mode is stored in bit 0, and the current interrupt enable bit is stored in bit 1.
    // Bits 2 and 3 are the previous kernel/user mode and interrupt enable, and bits 4 and 5 are the old
    // kernel/user mode and interrupt enable. A kernel/user mode value of 0 corresponds to kernel mode,
    // and a value of 1 corresponds to user mode
    Ehr#(2, Data) statusReg   <- mkEhr('b10);
    // 13 - Cause Register
    Reg#(Data) causeReg    <- mkReg(0);
    // 14 - EPC Register
    Ehr#(2, Addr) epcReg      <- mkEhr(0);
    // 18 - Write an integer to stderr
    //      implemented by enqueuing to copFifo
    // 19 - Write a char to stderr
    //      implemented by enqueuing to copFifo
    // 21 - Finish code
    //      implemented by enqueuing to copFifo

    rule count (startReg);
        cycles <= cycles + 1;
        $display("\nCycle %d ----------------------------------------------------", cycles);
    endrule

    method Action causeException(Addr current_pc, Bit#(5) cause);
        epcReg[1] <= current_pc;
        Data newCause = zeroExtend(cause);
        newCause = newCause << 2;
        causeReg <= newCause;
        statusReg[1] <= ((statusReg[1] << 2)) & 'h3F;

        // $display("causeReg: ", (cause << 2));
    endmethod

    method Action returnFromException;
        statusReg[1] <= (statusReg[1] >> 2);
    endmethod

    method Addr getEPC;
        return epcReg[0];
    endmethod

    method isUserMode;
        return (statusReg[0] & 'h1) != 0;
    endmethod

    method Action start;
        startReg <= True;
        cycles <= 0;
    endmethod

    method Bool started;
        return startReg;
    endmethod

    method Action wr(Maybe#(Word) idx_maybe, Number val);
        if (idx_maybe matches tagged Valid .idx) begin
            if (idx == 'hE9) begin
                copFifo.enq(tuple2(19, zeroExtend(val.Byte))); // Write char
            end
            if (idx == 'hEA) begin
                copFifo.enq(tuple2(18, zeroExtend(val.Byte))); // Write number
            end
            if (idx == 'hEB) begin
                copFifo.enq(tuple2(21, zeroExtend(val.Byte))); // Finish code
            end
        end
    endmethod

    // method for reading co-processor registers
    method Data rd(RIndx idx);
        return (case(idx)
                    10: cycles;
                    11: numInsts;
                 //   12: statusReg[0];
                //    13: causeReg;
                //    14: epcReg[0];
                endcase);
    endmethod
/*
    // method for writing co-processor registers
    method Action wr(Maybe#(FullIndx) idx, Data val);
        if(isValid(idx) && validValue(idx).regType == CopReg) begin
            case (validRegValue(idx))
                //12: statusReg[0] <= val;
                //14: epcReg[0] <= val;
                18: copFifo.enq(tuple2(18, val));
                19: copFifo.enq(tuple2(19, val));
                21: copFifo.enq(tuple2(21, val));
            endcase
        end
        numInsts <= numInsts + 1;
    endmethod*/

    method ActionValue#(Tuple2#(RIndx, Data)) cpuToHost;
        copFifo.deq;
        return copFifo.first;
    endmethod
endmodule
