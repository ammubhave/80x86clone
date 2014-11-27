/*

Bus Interface Unit (BIU)

*/

import Types::*;
import ProcTypes::*;
import MemTypes::*;
import IMemory::*;
import DMemory::*;
import Decode::*;
import Exec::*;
import Cop::*;
import Vector::*;
import Fifo::*;
import Ehr::*;
import SegRFile::*;
import GetPut::*;

interface Biu;
    method Bool memReady;
    method Action resetPc(Word startpc);

    method DMemResp dMemRespFifoFirst;
    method Action dMemRespFifoDeq;
    method Action dMemReqFifoEnq(DMemReq req);
    method Action dMemWriteReqFifoEnq(DMemReq req);
    method Action redirectFifoEnq(Redirect req);

    interface MemInitIfc iMemInit;
    interface MemInitIfc dMemInit;
    interface Cop copIfc;
    interface Get#(QBusElement) get;
endinterface

(* synthesize *)
module mkBiu (Biu ifc);
    Fifo#(QBusSize, QBusElement) qBus <- mkCFFifo;
    Fifo#(DMemFifoSize, DMemReq) dMemReqFifo <- mkCFFifo;
    Fifo#(DMemFifoSize, DMemReq) dMemWriteReqFifo <- mkCFFifo;
    Fifo#(DMemFifoSize, DMemResp) dMemRespFifo <- mkCFFifo;
    Fifo#(RedirectFifoSize, Redirect) redirectFifo <- mkCFFifo;
    Cop cop <- mkCop;

    Reg#(Word) pcReg <- mkRegU;
    Reg#(Bool) biuEpoch <- mkReg(False);

    SegRFile segRf <- mkSegRFile;

    // Memory
    IMemory  iMem <- mkIMemory;
    DMemory  dMem <- mkDMemory;

    Reg#(Maybe#(Data)) dMemWriteStatusIsWritingData <- mkReg(tagged Invalid);

   rule doBIU_HandleMemRequest(cop.started && dMemReqFifo.notEmpty);
        let req = dMemReqFifo.first;  
        Data orig_data <- dMem.req(MemReq{op: Ld, addr: addrFromSegOff(segRf.rd1(req.addr.seg), req.addr.offset), data: ?});
        Number data = ?;
        if (req.data matches tagged Word .w)
            case (req.addr.offset & 'b10)
                'b10:
                begin
                    data = tagged Word orig_data[15:0];
                    $display("BIU (Reading data): addr: %h, data: %h", addrFromSegOff(segRf.rd2(req.addr.seg), req.addr.offset), data.Word);  
                end
                'b00:
                begin
                    data = tagged Word orig_data[31:16];
                    $display("BIU (Reading data): addr: %h, data: %h", addrFromSegOff(segRf.rd2(req.addr.seg), req.addr.offset), data.Word);
                end
            endcase
        else 
            case (req.addr.offset & 'b11)
                'b11:
                begin
                    data = tagged Byte orig_data[7:0];
                    $display("BIU (Reading data): addr: %h, data: %h", addrFromSegOff(segRf.rd2(req.addr.seg), req.addr.offset), data.Byte);  
                end
                'b10:
                begin
                    data = tagged Byte orig_data[15:8];
                    $display("BIU (Reading data): addr: %h, data: %h", addrFromSegOff(segRf.rd2(req.addr.seg), req.addr.offset), data.Byte);
                end
                'b01:
                begin
                    data = tagged Byte orig_data[23:16];
                    $display("BIU (Reading data): addr: %h, data: %h", addrFromSegOff(segRf.rd2(req.addr.seg), req.addr.offset), data.Byte);  
                end
                'b00:
                begin
                    data = tagged Byte orig_data[31:24];
                    $display("BIU (Reading data): addr: %h, data: %h", addrFromSegOff(segRf.rd2(req.addr.seg), req.addr.offset), data.Byte);
                end
            endcase
        dMemRespFifo.enq(DMemResp{data: data});
        dMemReqFifo.deq;
    endrule

    rule doBIU_HandleMemWriteRequest(cop.started && dMemWriteReqFifo.notEmpty);
        let req = dMemWriteReqFifo.first;
        if (isValid(dMemWriteStatusIsWritingData)) begin
            if (req.data matches tagged Word .d)
                case (req.addr.offset & 'b10)
                    'b00:
                    begin
                        let tmp <- dMem.req(MemReq{op: St, addr: addrFromSegOff(segRf.rd1(req.addr.seg), req.addr.offset), 
                                 data: ({d, validValue(dMemWriteStatusIsWritingData)[15:0]})});   
                        $display("BIU (Writing data): addr: %h", addrFromSegOff(segRf.rd2(req.addr.seg), req.addr.offset), 
                          ", data: %h", ({d, validValue(dMemWriteStatusIsWritingData)[15:0]}));                 
                    end
                    'b10:
                    begin
                        let tmp <- dMem.req(MemReq{op: St, addr: addrFromSegOff(segRf.rd1(req.addr.seg), req.addr.offset), 
                                 data: ({validValue(dMemWriteStatusIsWritingData)[31:16], d})});                    
                        $display("BIU (Writing data): addr: %h", addrFromSegOff(segRf.rd2(req.addr.seg), req.addr.offset), 
                                  ", data: %h", ({validValue(dMemWriteStatusIsWritingData)[31:16], d}));
                    end
                endcase
            else if (req.data matches tagged Byte .d)
                case (req.addr.offset & 'b11)
                    'b00:
                    begin
                        let tmp <- dMem.req(MemReq{op: St, addr: addrFromSegOff(segRf.rd1(req.addr.seg), req.addr.offset), 
                                 data: ({d, validValue(dMemWriteStatusIsWritingData)[23:0]})});   
                        $display("BIU (Writing data): addr: %h", addrFromSegOff(segRf.rd2(req.addr.seg), req.addr.offset), 
                          ", data: %h", ({d, validValue(dMemWriteStatusIsWritingData)[23:0]}));
                    end
                    'b01:
                    begin
                        let tmp <- dMem.req(MemReq{op: St, addr: addrFromSegOff(segRf.rd1(req.addr.seg), req.addr.offset), 
                                 data: ({validValue(dMemWriteStatusIsWritingData)[31:24], d, validValue(dMemWriteStatusIsWritingData)[15:0]})});                    
                        $display("BIU (Writing data): addr: %h", addrFromSegOff(segRf.rd2(req.addr.seg), req.addr.offset), 
                                  ", data: %h", ({validValue(dMemWriteStatusIsWritingData)[31:24], d, validValue(dMemWriteStatusIsWritingData)[15:0]}));
                    end
                    'b10:
                    begin
                        let tmp <- dMem.req(MemReq{op: St, addr: addrFromSegOff(segRf.rd1(req.addr.seg), req.addr.offset), 
                                 data: ({validValue(dMemWriteStatusIsWritingData)[31:16], d, validValue(dMemWriteStatusIsWritingData)[7:0]})});   
                        $display("BIU (Writing data): addr: %h", addrFromSegOff(segRf.rd2(req.addr.seg), req.addr.offset), 
                          ", data: %h", ({validValue(dMemWriteStatusIsWritingData)[31:16], d, validValue(dMemWriteStatusIsWritingData)[7:0]}));                 
                    end
                    'b11:
                    begin
                        let tmp <- dMem.req(MemReq{op: St, addr: addrFromSegOff(segRf.rd1(req.addr.seg), req.addr.offset), 
                                 data: ({validValue(dMemWriteStatusIsWritingData)[31:8], d})});                    
                        $display("BIU (Writing data): addr: %h", addrFromSegOff(segRf.rd2(req.addr.seg), req.addr.offset), 
                                  ", data: %h", ({validValue(dMemWriteStatusIsWritingData)[31:8], d}));
                    end
                endcase
            dMemWriteStatusIsWritingData <= tagged Invalid;
            dMemWriteReqFifo.deq;
        end else begin
            Data data <- dMem.req(MemReq{op: Ld, addr: addrFromSegOff(segRf.rd1(req.addr.seg), req.addr.offset), data: ?});      
            $display("BIU (Processing Write data): addr: %h", addrFromSegOff(segRf.rd2(req.addr.seg), req.addr.offset));
            dMemWriteStatusIsWritingData <= tagged Valid data;
        end
    endrule

    rule doBIU_InstructionFetch(cop.started && !dMemReqFifo.notEmpty && !dMemWriteReqFifo.notEmpty);
        if (redirectFifo.notEmpty) begin
            let redirect = redirectFifo.first;
            pcReg <= redirect.nextIp;
            biuEpoch <= !biuEpoch;
            redirectFifo.deq;
            $display("BIU (Redirect):      ip: %h", redirect.nextIp, " epoch: ", !biuEpoch);
        end else begin
            let fetch_addr = addrFromSegOff(segRf.rd1(CS), zeroExtend(pcReg));
            Data inst = iMem.req(fetch_addr);
            $display("BIU:                 pc: %h", fetch_addr, ",  inst: ", show_epoch(biuEpoch), "%h", inst);
            case (pcReg & 'b11)
                'b11:
                begin
                    qBus.enq(QBusElement{ie: biuEpoch, pInst: inst[7:0], pc: pcReg});
                end
                'b10:
                begin
                    qBus.enq(QBusElement{ie: biuEpoch, pInst: inst[15:8], pc: pcReg});
                end
                'b01:
                begin
                    qBus.enq(QBusElement{ie: biuEpoch, pInst: inst[23:16], pc: pcReg});
                end
                'h00:
                begin
                    qBus.enq(QBusElement{ie: biuEpoch, pInst: inst[31:24], pc: pcReg});
                end
            endcase
            pcReg <= pcReg + 1;
        end
    endrule

    method Action resetPc(Word startpc);
        pcReg <= startpc;
        segRf.wr(DS, 'h100);
    endmethod

    method Bool memReady;
        return iMem.init.done() && dMem.init.done();
    endmethod

    method DMemResp dMemRespFifoFirst;
        return dMemRespFifo.first;
    endmethod
    method Action dMemRespFifoDeq;
        dMemRespFifo.deq;
    endmethod
    method Action dMemReqFifoEnq(DMemReq req);
        dMemReqFifo.enq(req);
    endmethod
    method Action dMemWriteReqFifoEnq(DMemReq req);
        dMemWriteReqFifo.enq(req);
    endmethod
    method Action redirectFifoEnq(Redirect req);
        redirectFifo.enq(req);
    endmethod

    interface MemInit iMemInit = iMem.init;
    interface MemInit dMemInit = dMem.init;
    interface Cop copIfc = cop;

    interface Get get;
        method ActionValue#(QBusElement) get;
            qBus.deq;
            return qBus.first;
        endmethod
    endinterface
endmodule
