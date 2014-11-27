/*

Register Fetch
Execution Unit (EU)

*/
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
import GetPut::*;

interface EuRegisterFetch;
    interface Put#(Fetch2FetchExecute) put;
    interface Get#(Fetch2FetchExecute) get;

    method Action rf_wr1(RegNumber r, Number data);
    method Action rf_wr1(RegNumber r, Number data);
    method Action sb_remove2(Maybe#(RegWord) x, Maybe#(RegWord) y);
endinterface

(* synthesize *)
module mkEuRegisterFetch(EuRegisterFetch ifc);
    Fifo#(2, Fetch2FetchExecute) f2rfFifo <- mkCFFifo;
    Fifo#(2, Fetch2FetchExecute) rf2mfFifo <- mkCFFifo;
    Scoreboard sb <- mkBypassScoreboard;
    RFile rf <- mkRFile;

    rule doEU_RegisterFetch;
        let qi = f2rfFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let dInst = qi.pdInst;

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


    method Action rf_wr1(RegNumber r, Number data);
        rf.wr1(r, data);
    endmethod
    method Action rf_wr2(RegNumber r, Number data);
        rf.wr2(r, data);
    endmethod
    method Action sb_remove2(Maybe#(RegWord) x, Maybe#(RegWord) y);
        sb.remove2(x, y);
    endmethod

    interface Get get;
        method ActionValue#(Fetch2FetchExecute) get;
            rf2mfFifo.deq;
            return rf2mfFifo.first;
        endmethod
    endinterface

    interface Put put;
        method Action put(Fetch2FetchExecute x);
            f2rfFifo.enq(x);
        endmethod
    endinterface
endmodule
