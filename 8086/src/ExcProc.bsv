// ExcProc.bsv
//
// This is a one cycle implementation of the SMIPS processor.

import Types::*;
import ProcTypes::*;
import MemTypes::*;
import RFile::*;
import IMemory::*;
import DMemory::*;
import Decode::*;
import Scoreboard::*;
import Exec::*;
import Cop::*;
import Vector::*;
import Fifo::*;
import Ehr::*;

import BIU::*;
import EU::*;

(* synthesize *)
module mkProc(Proc);
    Cop       cop <- mkCop;

    // Communication channel between the BIU and the EU
    Fifo#(QBusSize, QBusElement) qBus <- mkCFFifo;
    Fifo#(DMemFifoSize, DMemReq) dMemReqFifo <- mkCFFifo;
    Fifo#(DMemFifoSize, DMemReq) dMemWriteReqFifo <- mkCFFifo;
    Fifo#(DMemFifoSize, DMemResp) dMemRespFifo <- mkCFFifo;
    Fifo#(RedirectFifoSize, Redirect) redirectFifo <- mkCFFifo;

	// Bus Interface Unit (BIU)

    Biu biu <- mkBiu(qBus, dMemReqFifo, dMemWriteReqFifo, dMemRespFifo, redirectFifo, cop);

    // Execution Unit (EU)

    Eu eu <- mkEu(qBus, dMemReqFifo, dMemWriteReqFifo, dMemRespFifo, redirectFifo, cop);

        /* decode
        //DecodedInst dInst = decode(inst);

        // trace - print the instruction
        $display("pc: %h inst: (%h) expanded: ", pc, inst, showInst(inst));

        // read register values 
        Data rVal1 = rf.rd1(validRegValue(dInst.src1));
        Data rVal2 = rf.rd2(validRegValue(dInst.src2));

        // Co-processor read for debugging
        Data copVal = cop.rd(validRegValue(dInst.src1));

        // execute
        ExecInst eInst = exec(dInst, rVal1, rVal2, pc, ?, copVal, hi, lo);

        if (eInst.iType == Unsupported) begin
            cop.causeException(pc, excRI);
        end else if (eInst.iType == Syscall) begin
            cop.causeException(pc, excSys);
        end else if (eInst.iType == ERet) begin
            cop.returnFromException;
        end

        // Executing illegal instruction. Exiting
        if(eInst.iType == Illegal) begin
            $fwrite(stderr, "Executing illegal instruction at pc: %x. Exiting\n", pc);
            $finish;
        end

        // memory
        if(eInst.iType == Ld) begin
            eInst.data <- dMem.req(MemReq{op: Ld, addr: eInst.addr, data: ?});
        end else if(eInst.iType == St) begin
            let d <- dMem.req(MemReq{op: St, addr: eInst.addr, data: eInst.data});
        end

        // write back
        if(eInst.iType == Unsupported) begin
            $fwrite(stderr, "Executing unsupported instruction at pc: %x. Exiting\n", pc);
            $finish;
        end else begin
            if(isValid(eInst.dst) && validValue(eInst.dst).regType == Normal) begin
                rf.wr(validRegValue(eInst.dst), eInst.data);
            end else if(eInst.iType == Mthi) begin
                hi <= eInst.data;
            end else if(eInst.iType == Mtlo) begin
                lo <= eInst.data;
            end
        end

        if (eInst.iType == Syscall || eInst.iType == Unsupported) begin
            pc <= excHandlerPC;
        end else if (eInst.iType == ERet) begin
            pc <= cop.getEPC;
        end else begin
            // update the pc depending on whether the branch is taken or not
            pc <= eInst.brTaken ? eInst.addr : pc + 4;
        end

        // Co-processor write for debugging and stats
        cop.wr(eInst.dst, eInst.data);
        end
    endrule*/

    method ActionValue#(Tuple2#(RIndx, Data)) cpuToHost;
        let ret <- cop.cpuToHost;
        return ret;
    endmethod

    method Action hostToCpu(Bit#(32) startpc) if ( !cop.started && biu.memReady );
        cop.start;
        biu.resetPc(truncate(startpc));
    endmethod

    interface MemInit iMemInit = biu.iMemInit;
    interface MemInit dMemInit = biu.dMemInit;
endmodule
