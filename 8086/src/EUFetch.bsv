/*

Fetch Stages
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

interface EuFetch;
    interface Put#(QBusElement) put;
    interface Get#(Fetch2FetchExecute) get;
endinterface

// FSM states for Fetch Stage
typedef enum {FetchOpcode, FetchAddressingMode, FetchLowDispAddr, FetchHighDispAddr, FetchLowData, FetchHighData} FetchStage deriving(Eq, Bits, FShow);

(* synthesize *)
module mkEuFetch(EuFetch ifc);
    Fifo#(QBusSize, QBusElement) qBus <- mkCFFifo;
    Fifo#(2, Fetch2FetchExecute) f2rfFifo <- mkCFFifo;

    Fifo#(2, Fetch2FetchExecute) f2fFifo <- mkCFFifo;
    Reg#(FetchStage) fetchStage <- mkReg(FetchOpcode);
    Reg#(Bool) euEpoch <- mkReg(False);

    rule doEU_FetchOpcode(fetchStage == FetchOpcode);
        let qi = qBus.first;
        let pc = qi.pc;
        if (qi.ie != euEpoch) begin
            euEpoch <= !euEpoch;
        end

        DecodedInst pdInst = decodeOpcode(qi.pInst);

        if (pdInst.reqAddressMode || pdInst.reqLowDispAddr || pdInst.reqHighDispAddr || pdInst.reqLowData || pdInst.reqHighData)            
            f2fFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: pc});
        else
            f2rfFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: pc});

        if (pdInst.reqAddressMode)
            fetchStage <= FetchAddressingMode;
        else if (pdInst.reqLowDispAddr)
            fetchStage <= FetchLowDispAddr;
        else if (pdInst.reqHighDispAddr)
            fetchStage <= FetchHighDispAddr;
        else if (pdInst.reqLowData)
            fetchStage <= FetchLowData;
        else if (pdInst.reqHighData)
            fetchStage <= FetchHighData;
        else
            fetchStage <= FetchOpcode;
        qBus.deq;

        $display("FetchOpcode:         ip: %h", pc, ", dinst: ", show_epoch(qi.ie), fshow(pdInst), ", inst: %h", qi.pInst);
    endrule

    rule doEU_RestOfFetch(fetchStage != FetchOpcode);
        let qi = f2fFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let pdInst = qi.pdInst;
        if (qi.ie == euEpoch && qBus.first.ie == euEpoch) begin
            case(fetchStage)
                FetchAddressingMode:
                begin
                    pdInst = decodeAddressingMode(pdInst, qBus.first.pInst);
                    pdInst.reqAddressMode = False;
                end
                FetchLowDispAddr:
                begin
                    pdInst.lowDispAddr = qBus.first.pInst;
                    pdInst.reqLowDispAddr = False;
                end
                FetchHighDispAddr:
                begin
                    pdInst.highDispAddr = qBus.first.pInst;
                    pdInst.reqHighDispAddr = False;
                end
                FetchLowData:
                begin
                    pdInst = decodeLowData(pdInst, qBus.first.pInst);
                    pdInst.reqLowData = False;
                end
                FetchHighData:
                begin
                    pdInst = decodeHighData(pdInst, qBus.first.pInst);
                    pdInst.reqHighData = False;
                end
            endcase
            pc = pc + 1;
            qBus.deq;

            if (pdInst.reqLowDispAddr || pdInst.reqHighDispAddr || pdInst.reqLowData || pdInst.reqHighData)            
                f2fFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: ip});
            else
                f2rfFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: ip});

            if (pdInst.reqAddressMode)
                fetchStage <= FetchAddressingMode;
            else if (pdInst.reqLowDispAddr)
                fetchStage <= FetchLowDispAddr;
            else if (pdInst.reqHighDispAddr)
                fetchStage <= FetchHighDispAddr;
            else if (pdInst.reqLowData)
                fetchStage <= FetchLowData;
            else if (pdInst.reqHighData)
                fetchStage <= FetchHighData;
            else
                fetchStage <= FetchOpcode;

            $display(fshow(fetchStage), ": ip: %h", ip, ", dinst: ", show_epoch(qi.ie), fshow(qi.pdInst));
        end else begin
            fetchStage <= FetchOpcode;
        end
        f2fFifo.deq;
    endrule
/*
    rule doEU_FetchAddressingMode(fetchStage == FetchAddressingMode);
        let qi = f2fFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let pdInst = qi.pdInst;
        if (qi.ie == euEpoch && qBus.first.ie == euEpoch) begin
            if (qi.pdInst.reqAddressMode) begin
                pc = pc + 1;
                pdInst = decodeAddressingMode(pdInst, qBus.first.pInst);
                qBus.deq;
            end

            if (pdInst.reqLowDispAddr || pdInst.reqHighDispAddr || pdInst.reqLowData || pdInst.reqHighData)            
                f2fFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: ip});
            else
                f2rfFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: ip});

            if (pdInst.reqLowDispAddr)
                fetchStage <= FetchLowDispAddr;
            else if (pdInst.reqHighDispAddr)
                fetchStage <= FetchHighDispAddr;
            else if (pdInst.reqLowData)
                fetchStage <= FetchLowData;
            else if (pdInst.reqHighData)
                fetchStage <= FetchHighData;
            else
                fetchStage <= FetchOpcode;

            $display("FetchAddressingMode: ip: %h", ip, ", dinst: ", show_epoch(qi.ie), fshow(qi.pdInst));
        end else begin
            fetchStage <= FetchOpcode;
        end
        f2fFifo.deq;
    endrule

    rule doEU_FetchLowDispAddr(fetchStage == FetchLowDispAddr);
        let qi = f2fFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let pdInst = qi.pdInst;
        if (qi.ie == euEpoch && qBus.first.ie == euEpoch) begin
            if (qi.pdInst.reqLowDispAddr) begin
                pc = pc + 1;
                pdInst.lowDispAddr = qBus.first.pInst;
                qBus.deq;
            end
            
            if (pdInst.reqHighDispAddr || pdInst.reqLowData || pdInst.reqHighData)            
                f2fFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: ip});
            else
                f2rfFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: ip});

            if (pdInst.reqHighDispAddr)
                fetchStage <= FetchHighDispAddr;
            else if (pdInst.reqLowData)
                fetchStage <= FetchLowData;
            else if (pdInst.reqHighData)
                fetchStage <= FetchHighData;
            else
                fetchStage <= FetchOpcode;

            $display("FetchLowDispAddr:    ip: %h", ip, ", dinst: ", show_epoch(qi.ie), fshow(qi.pdInst));
        end else begin
            fetchStage <= FetchOpcode;
        end
        f2fFifo.deq;
    endrule

    rule doEU_FetchHighDispAddr(fetchStage == FetchHighDispAddr);
        let qi = f2fFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let pdInst = qi.pdInst;
        if (qi.ie == euEpoch && qBus.first.ie == euEpoch) begin
            if (qi.pdInst.reqHighDispAddr) begin
                pc = pc + 1;
                pdInst.highDispAddr = qBus.first.pInst;
                qBus.deq;
            end

            if (pdInst.reqLowData || pdInst.reqHighData)            
                f2fFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: ip});
            else
                f2rfFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: ip});

            if (pdInst.reqLowData)
                fetchStage <= FetchLowData;
            else if (pdInst.reqHighData)
                fetchStage <= FetchHighData;
            else
                fetchStage <= FetchOpcode;

            $display("FetchHighDispAddr:   ip: %h", ip, ", dinst: ", show_epoch(qi.ie), fshow(qi.pdInst));
        end else begin
            fetchStage <= FetchOpcode;
        end
        f2fFifo.deq;
    endrule

    rule doEU_FetchLowData(fetchStage == FetchLowData);
        let qi = f2fFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let pdInst = qi.pdInst;
        if (qi.ie == euEpoch && qBus.first.ie == euEpoch) begin
            if (pdInst.reqLowData) begin
                pc = pc + 1;
                pdInst = decodeLowData(pdInst, qBus.first.pInst);
                qBus.deq;
            end

            if (pdInst.reqHighData)            
                f2fFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: ip});
            else
                f2rfFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: ip});

            if (pdInst.reqHighData)
                fetchStage <= FetchHighData;
            else
                fetchStage <= FetchOpcode;

            $display("FetchLowData:        ip: %h", ip, ", dinst: ", show_epoch(qi.ie), fshow(pdInst));
        end else begin
            fetchStage <= FetchOpcode;
        end
        f2fFifo.deq;
    endrule

    rule doEU_FetchHighData(fetchStage == FetchHighData);
        let qi = f2fFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let pdInst = qi.pdInst;
        if (qi.ie == euEpoch && qBus.first.ie == euEpoch) begin
            if (qi.pdInst.reqHighData) begin
                pc = pc + 1;
                pdInst = decodeHighData(pdInst, qBus.first.pInst);
                qBus.deq;
            end

            f2rfFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: ip});
            fetchStage <= FetchOpcode;

            $display("FetchHighData:       ip: %h", ip, ", dinst: ", show_epoch(qi.ie), fshow(pdInst));
        end else begin
            fetchStage <= FetchOpcode;
        end
        f2fFifo.deq;
    endrule*/

    interface Get get;
        method ActionValue#(Fetch2FetchExecute) get;
            f2rfFifo.deq;
            return f2rfFifo.first;
        endmethod
    endinterface

    interface Put put;
        method Action put(QBusElement x);
            qBus.enq(x);
        endmethod
    endinterface
endmodule
