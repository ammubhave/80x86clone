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
import EUFetch::*;
import EURegisterFetch::*;
import GetPut::*;
import Connectable::*;

(* synthesize *)
module mkProc(Proc);
    Biu biu <- mkBiu;

    Reg#(RegFlags) flags <- mkReg(unpack('b1111000000000000));
    Reg#(Bool) euEpoch <- mkReg(False);

    Fifo#(2, Fetch2FetchExecute) f2rfFifo <- mkCFFifo;
    Fifo#(2, Fetch2FetchExecute) rf2mfFifo <- mkCFFifo;
    Fifo#(2, Fetch2FetchExecute) mf2eFifo <- mkCFFifo;


    EuFetch euFetch <- mkEuFetch;
    EuRegisterFetch euRegisterFetch <- mkEuRegisterFetch;

    mkConnection(biu.get, euFetch.put);
    mkConnection(euFetch.get, euRegisterFetch.put);
    rule registerFetch2ExecuteLink;
        let x <- euRegisterFetch.get.get; rf2mfFifo.enq(x);
    endrule

    rule doEU_MemoryFetch;
        let qi = rf2mfFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let dInst = decodeRFToMemoryFetch(qi.pdInst);

        if (dInst.srcm matches tagged Valid .d)
            biu.dMemReqFifoEnq(DMemReq{addr: d, data: dInst.srcValm});

        mf2eFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: dInst, pc: pc, ip: ip});

        rf2mfFifo.deq;
        $display("MemoryFetch:         ip: %h", ip, ", dinst: ", show_epoch(qi.ie), fshow(dInst), show_memfetch(dInst));
    endrule

    rule doEU_Execute;
        let qi = mf2eFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let dInst = qi.pdInst;
        dInst.flags = flags;

        if (dInst.srcm matches tagged Valid .d) begin
            let dMemRespFifoFirst = biu.dMemRespFifoFirst;
            dInst.srcValm = dMemRespFifoFirst.data;
            biu.dMemRespFifoDeq;
        end
        if (qi.ie == euEpoch) begin
            // Fetch
            //if (dInst.src1 matches tagged Valid .d &&& d matches tagged RegWord .r)
            //    dInst.srcVal1 = rf[pack(r)];
            //if (dInst.src2 matches tagged Valid .d &&& d matches tagged RegWord .r)
            //    dInst.srcVal2 = rf[pack(r)];
            //if (dInst.srcm matches tagged Valid .d) begin
            //    dInst.srcValm = dMemRespFifo.first.data;
            //    dMemRespFifo.deq;
            //end

            // Execute
            ExecInst eInst = exec(dInst, ip, pc);

            if(eInst.iType == Unsupported) begin
                $fwrite(stderr, "Executing unsupported instruction at ip: %x. Exiting\n", ip);
                $finish;
            end

            // WriteBack
            if (eInst.dst1 matches tagged Valid .r)
                euRegisterFetch.rf_wr1(r, eInst.dstVal1);
            if (eInst.dst2 matches tagged Valid .r)
                euRegisterFetch.rf_wr2(r, eInst.dstVal2);
            if (eInst.dstm matches tagged Valid .d)
                biu.dMemWriteReqFifoEnq(DMemReq{addr: d, data: eInst.dstValm});
            flags <= eInst.flags;

            biu.copIfc.wr(eInst.dstp, eInst.dstValp);


            //ipReg <= eInst.nextIp;
            if (eInst.nextIp == pc + 1) begin
                $display("Execute:             ip: %h", ip, ", dinst: ", show_epoch(qi.ie), fshow(dInst), ", einst: ", fshow(eInst));
            end else begin
                euEpoch <= !euEpoch;
                biu.redirectFifoEnq(Redirect { nextIp: eInst.nextIp });
                $display("Execute (Redirect):  ip: %h", ip, ", dinst: ", show_epoch(qi.ie), fshow(dInst), ", einst: ", fshow(eInst));
            end
        end else begin
            $display("Execute (Killing):  ip: %h", ip, ", dinst: ", show_epoch(qi.ie), fshow(dInst));
        end

        euRegisterFetch.sb_remove2(getRegWordFromRegNumber(dInst.dst1), 
                   getRegWordFromRegNumber(dInst.dst2));
        mf2eFifo.deq;
    endrule

    // Communication channel between the BIU and the EU
   /* Fifo#(QBusSize, QBusElement) qBus <- mkCFFifo;
    Fifo#(DMemFifoSize, DMemReq) dMemReqFifo <- mkCFFifo;
    Fifo#(DMemFifoSize, DMemReq) dMemWriteReqFifo <- mkCFFifo;
    Fifo#(DMemFifoSize, DMemResp) dMemRespFifo <- mkCFFifo;
    Fifo#(RedirectFifoSize, Redirect) redirectFifo <- mkCFFifo;*/

	// Bus Interface Unit (BIU)

   // Biu biu <- mkBiu(qBus, dMemReqFifo, dMemWriteReqFifo, dMemRespFifo, redirectFifo, cop);
	//	Eu eu <- mkEu;
    // Execution Unit (EU)

   // Eu eu <- mkEu(qBus, dMemReqFifo, dMemWriteReqFifo, dMemRespFifo, redirectFifo, cop);

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
        let ret <- biu.copIfc.cpuToHost;
        return ret;
    endmethod

    method Action hostToCpu(Bit#(32) startpc) if ( !biu.copIfc.started && biu.memReady );
        biu.copIfc.start;
        biu.resetPc(truncate(startpc));
    endmethod

    interface MemInit iMemInit = biu.iMemInit;
    interface MemInit dMemInit = biu.dMemInit;
endmodule
