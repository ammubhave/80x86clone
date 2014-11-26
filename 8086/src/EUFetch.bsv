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

interface EuFetch;
endinterface

// FSM states for Fetch Stage
typedef enum {FetchOpcode, FetchAddressingMode, FetchLowDispAddr, FetchHighDispAddr, FetchLowData, FetchHighData} FetchStage deriving(Eq, Bits, FShow);

module mkEuFetch(Biu biu,
                 Fifo#(2, Fetch2FetchExecute) f2rfFifo,
                 EuFetch ifc);

    Fifo#(2, Fetch2FetchExecute) f2fFifo <- mkCFFifo;
    Reg#(FetchStage) fetchStage <- mkReg(FetchOpcode);
    Reg#(Bool) euEpoch <- mkReg(False);

    rule doEU_FetchOpcode(fetchStage == FetchOpcode);
        let qi = biu.qBusFirst;
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
        biu.qBusDeq;

        $display("FetchOpcode:         ip: %h", pc, ", dinst: ", show_epoch(qi.ie), fshow(pdInst), ", inst: %h", qi.pInst);
    endrule

    rule doEU_FetchAddressingMode(fetchStage == FetchAddressingMode);
        let qBusFirst = biu.qBusFirst;
        let qi = f2fFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let pdInst = qi.pdInst;
        if (qi.ie == euEpoch && qBusFirst.ie == euEpoch) begin
            if (qi.pdInst.reqAddressMode) begin
                pc = pc + 1;
                pdInst = decodeAddressingMode(pdInst, qBusFirst.pInst);
                biu.qBusDeq;
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
        let qBusFirst = biu.qBusFirst;
        let qi = f2fFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let pdInst = qi.pdInst;
        if (qi.ie == euEpoch && qBusFirst.ie == euEpoch) begin
            if (qi.pdInst.reqLowDispAddr) begin
                pc = pc + 1;
                pdInst.lowDispAddr = qBusFirst.pInst;
                biu.qBusDeq;
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
        let qBusFirst = biu.qBusFirst;
        let qi = f2fFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let pdInst = qi.pdInst;
        if (qi.ie == euEpoch && qBusFirst.ie == euEpoch) begin
            if (qi.pdInst.reqHighDispAddr) begin
                pc = pc + 1;
                pdInst.highDispAddr = qBusFirst.pInst;
                biu.qBusDeq;
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
        let qBusFirst = biu.qBusFirst;
        let qi = f2fFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let pdInst = qi.pdInst;
        if (qi.ie == euEpoch && qBusFirst.ie == euEpoch) begin
            if (pdInst.reqLowData) begin
                pc = pc + 1;
                pdInst = decodeLowData(pdInst, qBusFirst.pInst);
                biu.qBusDeq;
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
        let qBusFirst = biu.qBusFirst;
        let qi = f2fFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let pdInst = qi.pdInst;
        if (qi.ie == euEpoch && qBusFirst.ie == euEpoch) begin
            if (qi.pdInst.reqHighData) begin
                pc = pc + 1;
                pdInst = decodeHighData(pdInst, qBusFirst.pInst);
                biu.qBusDeq;
            end

            f2rfFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: ip});
            fetchStage <= FetchOpcode;

            $display("FetchHighData:       ip: %h", ip, ", dinst: ", show_epoch(qi.ie), fshow(pdInst));
        end else begin
            fetchStage <= FetchOpcode;
        end
        f2fFifo.deq;
    endrule
endmodule
