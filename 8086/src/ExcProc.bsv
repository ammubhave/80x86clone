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

function Maybe#(RegWord) getRegWordFromRegNumber(Maybe#(RegNumber) o);
    Maybe#(RegWord) pdst1 = tagged Invalid;
    if (o matches tagged Valid .d) begin
        if (d matches tagged RegWord .r)
            pdst1 = tagged Valid r;
        else if (d matches tagged RegByte .r)
            pdst1 = tagged Valid r16FromR8(r);
    end
    return pdst1;
endfunction

(* synthesize *)
module mkProc(Proc);
    Biu biu <- mkBiu;

    RFile rf <- mkRFile;
    Reg#(RegFlags) flags <- mkReg(unpack('b1111000000000000));
    Reg#(Bool) euEpoch <- mkReg(False);
    Scoreboard sb <- mkBypassScoreboard;

    Fifo#(2, Fetch2FetchExecute) f2rfFifo <- mkCFFifo;
    Fifo#(2, Fetch2FetchExecute) rf2mfFifo <- mkCFFifo;
    Fifo#(2, Fetch2FetchExecute) mf2eFifo <- mkCFFifo;

// EXECUTION UNIT - FETCH/DECODE PIPELINE STAGE
    EuFetch euFetch <- mkEuFetch(biu, f2rfFifo);

// EXECUTION UNIT - EXECUTE PIPELINE STAGE
    rule doEU_RegisterFetch;
        let qi = f2rfFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let dInst = qi.pdInst;

       /* Bool s1 = True;
        Bool s2 = True;
        Bool s3 = True;
        if (dInst.src1 matches tagged Valid .d &&& d matches tagged RegWord .r)
            s1 = sb.search1(tagged Valid r);
        if (dInst.src2 matches tagged Valid .d &&& d matches tagged RegWord .r)
            s2 = sb.search2(tagged Valid r);
        if (dInst.src3 matches tagged Valid .d &&& d matches tagged RegWord .r)
            s3 = sb.search3(tagged Valid r);

        $display("     Searching: s1: ", fshow(s1), ", s2: ", fshow(s2), ", s3: ", fshow(s3));
*/
        if (sb.search1(getRegWordFromRegNumber(dInst.src1)) &&
            sb.search2(getRegWordFromRegNumber(dInst.src2)) && 
            sb.search3(getRegWordFromRegNumber(dInst.src3))) begin
            if (dInst.src1 matches tagged Valid .r)
                dInst.srcVal1 = rf.rd1(r);
            if (dInst.src2 matches tagged Valid .r)
                dInst.srcVal2 = rf.rd2(r);
            if (dInst.src3 matches tagged Valid .r)
                dInst.srcVal3 = rf.rd3(r);

            let pdst1 = getRegWordFromRegNumber(dInst.dst1);
            let pdst2 = getRegWordFromRegNumber(dInst.dst2);
            sb.insert2(pdst1, pdst2);
            $display("     Enqueing: x: ", fshow(pdst1), ", y: ", fshow(pdst2));

            rf2mfFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: dInst, pc: pc, ip: ip});

            f2rfFifo.deq;
            $display("RegisterFetch:       ip: %h", ip, ", dinst: ", show_epoch(qi.ie), fshow(dInst), show_regfetch(dInst));
        end else begin
            $display("RegisterFetch (Stalled): ip: %h", ip, ", dinst: ", fshow(dInst), ", epoch: ", qi.ie);
        end
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
                rf.wr1(r, eInst.dstVal1);
            if (eInst.dst2 matches tagged Valid .r)
                rf.wr2(r, eInst.dstVal2);
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

        sb.remove2(getRegWordFromRegNumber(dInst.dst1), 
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
