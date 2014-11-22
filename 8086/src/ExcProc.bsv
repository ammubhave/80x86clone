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

// FSM states for Fetch Stage
typedef enum {FetchOpcode, FetchAddressingMode, FetchLowDispAddr, FetchHighDispAddr, FetchLowData, FetchHighData} FetchStage deriving(Eq, Bits, FShow);

typedef struct {
    Bool ie;
    Byte pInst;
    Word pc;    // The instruction's pointer
} QBusElement deriving (Bits);

typedef struct {
    Bool ie;
    DecodedInst pdInst;
    Word pc;    // The counter for partial instructions
    Word ip;
} Fetch2FetchExecute deriving (Bits);

typedef struct {
    SegAddr addr;
    Word data;
} DMemReq deriving (Bits);

typedef struct {
    Word data;
} DMemResp deriving (Bits);

(* synthesize *)
module mkProc(Proc);
    //Reg#(Addr) ipReg <- mkRegU;
    Reg#(Word) pcReg <- mkRegU;
    Fifo#(6, QBusElement) qBus <- mkCFFifo;
    Fifo#(2, DMemReq) dMemReqFifo <- mkCFFifo;
    Fifo#(2, DMemReq) dMemWriteReqFifo <- mkCFFifo;
    Reg#(Maybe#(Data)) dMemWriteStatusIsWritingData <- mkReg(tagged Invalid);
    Fifo#(2, DMemResp) dMemRespFifo <- mkCFFifo;
    Reg#(Bool) biuEpoch <- mkReg(False);

    // RFile      rf <- mkRFile;
    Vector#(8, Reg#(Word)) rf <- replicateM(mkRegU);
    Vector#(4, Reg#(Word)) segRf <- replicateM(mkReg(0));
    IMemory  iMem <- mkIMemory;
    DMemory  dMem <- mkDMemory;
    Cop       cop <- mkCop;
    Reg#(FetchStage) fetchStage <- mkReg(FetchOpcode);
    Reg#(Bool) euEpoch <- mkReg(False);
    Scoreboard#(14) sb <- mkBypassScoreboard;

    Fifo#(2, Fetch2FetchExecute) f2fFifo <- mkCFFifo;
    /*Fifo#(2, Fetch2Execute) fam2fldaFifo <- mkCFFifo;
    Fifo#(2, Fetch2Execute) flda2fhdaFifo <- mkCFFifo;
    Fifo#(2, Fetch2Execute) fhda2fldFifo <- mkCFFifo;
    Fifo#(2, Fetch2Execute) fld2fhdFifo <- mkCFFifo; */
    Fifo#(2, Fetch2FetchExecute) f2rfFifo <- mkCFFifo;
    Fifo#(2, Fetch2FetchExecute) rf2mfFifo <- mkCFFifo;
    Fifo#(2, Fetch2FetchExecute) mf2eFifo <- mkCFFifo;

    Bool memReady = iMem.init.done() && dMem.init.done();

// BUS INTERFACE UNIT
    rule doBIU_HandleMemRequest(cop.started && dMemReqFifo.notEmpty);
        let req = dMemReqFifo.first;
        $display("BIU (Reading data): addr: %h", addrFromSegOff(segRf[pack(req.addr.seg)], req.addr.offset));    
        Data data <- dMem.req(MemReq{op: Ld, addr: addrFromSegOff(segRf[pack(req.addr.seg)], req.addr.offset), data: ?});
        case (req.addr.offset & 'b10)
            'b10:
            begin
                dMemRespFifo.enq(DMemResp{data: data[31:16]});
            end
            'b00:
            begin
                dMemRespFifo.enq(DMemResp{data: data[15:0]});
            end
        endcase
        dMemReqFifo.deq;
    endrule

    rule doBIU_HandleMemWriteRequest(cop.started && dMemWriteReqFifo.notEmpty);
        let req = dMemWriteReqFifo.first;
        if (isValid(dMemWriteStatusIsWritingData)) begin
            case (req.addr.offset & 'b10)
                'b10:
                begin
                    let tmp <- dMem.req(MemReq{op: St, addr: addrFromSegOff(segRf[pack(req.addr.seg)], req.addr.offset), 
                             data: ({req.data, validValue(dMemWriteStatusIsWritingData)[15:0]})});   
                    $display("BIU (Writing data): addr: %h", addrFromSegOff(segRf[pack(req.addr.seg)], req.addr.offset), 
                      ", data: %h", ({req.data, validValue(dMemWriteStatusIsWritingData)[15:0]}));                 
                end
                'b00:
                begin
                    let tmp <- dMem.req(MemReq{op: St, addr: addrFromSegOff(segRf[pack(req.addr.seg)], req.addr.offset), 
                             data: ({validValue(dMemWriteStatusIsWritingData)[31:16], req.data})});                    
                    $display("BIU (Writing data): addr: %h", addrFromSegOff(segRf[pack(req.addr.seg)], req.addr.offset), 
                              ", data: %h", ({validValue(dMemWriteStatusIsWritingData)[31:16], req.data}));
                end
            endcase
            dMemWriteStatusIsWritingData <= tagged Invalid;
            dMemWriteReqFifo.deq;
        end else begin
            Data data <- dMem.req(MemReq{op: Ld, addr: addrFromSegOff(segRf[pack(req.addr.seg)], req.addr.offset), data: ?});      
            $display("BIU (Processing Write data): addr: %h", addrFromSegOff(segRf[pack(req.addr.seg)], req.addr.offset));
            dMemWriteStatusIsWritingData <= tagged Valid data;
        end
    endrule

    rule doBIU_InstructionFetch(cop.started && !dMemReqFifo.notEmpty && !dMemWriteReqFifo.notEmpty);
        Data inst = iMem.req(zeroExtend(pcReg));
        $display("BIU: pc: %h", pcReg, ", inst: %h", inst, ", epoch: ", biuEpoch);
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
    endrule

// EXECUTION UNIT - FETCH/DECODE PIPELINE STAGE

    rule doEU_FetchOpcode(cop.started && fetchStage == FetchOpcode);
        let qi = qBus.first;
        let pc = qi.pc;
        if (qi.ie == euEpoch) begin
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

            $display("FetchOpcode: ip: %h", pc, ", inst: %h", qi.pInst,", dinst: ", fshow(pdInst), ", epoch: ", qi.ie);
        end else
            fetchStage <= FetchOpcode;
        qBus.deq;
    endrule

    rule doEU_FetchAddressingMode(cop.started && fetchStage == FetchAddressingMode);
        let qi = f2fFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let pdInst = qi.pdInst;
        if (qi.ie == euEpoch) begin
            if (qi.pdInst.reqAddressMode) begin
                pc = pc + 1;
                pdInst = decodeAddressingMode(pdInst, qBus.first.pInst);
                qBus.deq;
            end

            if (pdInst.reqLowDispAddr || pdInst.reqHighDispAddr || pdInst.reqLowData || pdInst.reqHighData)            
                f2fFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: pc});
            else
                f2rfFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: pc});

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

            $display("FetchAddressingMode: ip: %h", qi.ip, ", inst: ", fshow(qi.pdInst), ", epoch: ", qi.ie);
        end else
            fetchStage <= FetchOpcode;
        f2fFifo.deq;
    endrule

    rule doEU_FetchLowDispAddr(cop.started && fetchStage == FetchLowDispAddr);
        let qi = f2fFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let pdInst = qi.pdInst;
        if (qi.ie == euEpoch) begin
            if (qi.pdInst.reqLowDispAddr) begin
                pc = pc + 1;
                pdInst.lowDispAddr = qBus.first.pInst;
                qBus.deq;
            end
            
            if (pdInst.reqHighDispAddr || pdInst.reqLowData || pdInst.reqHighData)            
                f2fFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: pc});
            else
                f2rfFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: pc});

            if (pdInst.reqHighDispAddr)
                fetchStage <= FetchHighDispAddr;
            else if (pdInst.reqLowData)
                fetchStage <= FetchLowData;
            else if (pdInst.reqHighData)
                fetchStage <= FetchHighData;
            else
                fetchStage <= FetchOpcode;

            $display("FetchLowDispAddr: ip: %h", qi.ip, ", inst: ", fshow(qi.pdInst), ", epoch: ", qi.ie);
        end else
            fetchStage <= FetchOpcode;
        f2fFifo.deq;
    endrule

    rule doEU_FetchHighDispAddr(cop.started && fetchStage == FetchHighDispAddr);
        let qi = f2fFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let pdInst = qi.pdInst;
        if (qi.ie == euEpoch) begin
            if (qi.pdInst.reqHighDispAddr) begin
                pc = pc + 1;
                pdInst.highDispAddr = qBus.first.pInst;
                qBus.deq;
            end

            if (pdInst.reqLowData || pdInst.reqHighData)            
                f2fFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: pc});
            else
                f2rfFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: pc});

            if (pdInst.reqLowData)
                fetchStage <= FetchLowData;
            else if (pdInst.reqHighData)
                fetchStage <= FetchHighData;
            else
                fetchStage <= FetchOpcode;

            $display("FetchHighDispAddr: ip: %h", qi.ip, ", inst: ", fshow(qi.pdInst), ", epoch: ", qi.ie);
        end else
            fetchStage <= FetchOpcode;
        f2fFifo.deq;
    endrule

    rule doEU_FetchLowData(cop.started && fetchStage == FetchLowData);
        let qi = f2fFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let pdInst = qi.pdInst;
        if (qi.ie == euEpoch) begin
            if (pdInst.reqLowData) begin
                pc = pc + 1;
                pdInst = decodeLowData(pdInst, qBus.first.pInst);
                qBus.deq;
            end

            if (pdInst.reqHighData)            
                f2fFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: pc});
            else
                f2rfFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: pc});

            if (pdInst.reqHighData)
                fetchStage <= FetchHighData;
            else
                fetchStage <= FetchOpcode;

            $display("FetchLowData: ip: %h", ip, ", inst: ", fshow(pdInst), ", epoch: ", qi.ie);
        end else
            fetchStage <= FetchOpcode;
        f2fFifo.deq;
    endrule

    rule doEU_FetchHighData(cop.started && fetchStage == FetchHighData);
        let qi = f2fFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let pdInst = qi.pdInst;
        if (qi.ie == euEpoch) begin
            if (qi.pdInst.reqHighData) begin
                pc = pc + 1;
                pdInst = decodeHighData(pdInst, qBus.first.pInst);
                qBus.deq;
            end

            f2rfFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: pdInst, pc: pc, ip: pc});
            fetchStage <= FetchOpcode;

            $display("FetchHighData: ip: %h", ip, ", inst: ", fshow(pdInst), ", epoch: ", qi.ie);
        end else
            fetchStage <= FetchOpcode;
        f2fFifo.deq;
    endrule

// EXECUTION UNIT - EXECUTE PIPELINE STAGE
    rule doEU_RegisterFetch(cop.started);
        let qi = f2rfFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let dInst = qi.pdInst;

        Bool s1 = True;
        Bool s2 = True;
        Bool s3 = True;
   //     if (dInst.src1 matches tagged Valid .d &&& d matches tagged RegWord .r)
   //         s1 = sb.search1(tagged Valid r);
       /* if (dInst.src2 matches tagged Valid .d &&& d matches tagged RegWord .r)
            s2 = sb.search2(tagged Valid r);
        if (dInst.src3 matches tagged Valid .d &&& d matches tagged RegWord .r)
            s3 = sb.search3(tagged Valid r);
*/
        if (s1 && s2 && s3) begin

            if (dInst.src1 matches tagged Valid .d &&& d matches tagged RegWord .r) begin
                dInst.srcVal1 = rf[pack(r)];
            end
            if (dInst.src2 matches tagged Valid .d &&& d matches tagged RegWord .r) begin
                dInst.srcVal2 = rf[pack(r)];
            end
            if (dInst.src3 matches tagged Valid .d &&& d matches tagged RegWord .r) begin
                dInst.srcVal3 = rf[pack(r)];
            end


            Maybe#(RegWord) pdst1 = tagged Invalid;
            if (dInst.dst1 matches tagged Valid .d) begin
                if (d matches tagged RegWord .r)
                    pdst1 = tagged Valid r;
                else if (d matches tagged RegByte .r)
                    pdst1 = tagged Valid r16FromR8(r);
            end
            Maybe#(RegWord) pdst2 = tagged Invalid;
            if (dInst.dst2 matches tagged Valid .d) begin
                if (d matches tagged RegWord .r)
                    pdst2 = tagged Valid r;
                else if (d matches tagged RegByte .r)
                    pdst2 = tagged Valid r16FromR8(r);
            end

            sb.insert2(pdst1, pdst2);

            rf2mfFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: dInst, pc: pc, ip: ip});

            f2rfFifo.deq;
            $display("RegisterFetch: ip: %h", ip, ", dinst: ", fshow(dInst), ", epoch: ", qi.ie);
        end else begin
            $display("RegisterFetch (Stalled): ip: %h", ip, ", dinst: ", fshow(dInst), ", epoch: ", qi.ie);
        end
    endrule

    rule doEU_MemoryFetch(cop.started);
        let qi = rf2mfFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let dInst = decodeRFToMemoryFetch(qi.pdInst);

        if (dInst.srcm matches tagged Valid .d)
            dMemReqFifo.enq(DMemReq{addr: d});

        mf2eFifo.enq(Fetch2FetchExecute{ie: qi.ie, pdInst: dInst, pc: pc, ip: ip});

        rf2mfFifo.deq;
        $display("MemoryFetch: ip: %h", ip, ", dinst: ", fshow(dInst), ", epoch: ", qi.ie);
    endrule

    rule doEU_Execute(cop.started);
        let qi = mf2eFifo.first;
        let pc = qi.pc;
        let ip = qi.ip;
        let dInst = qi.pdInst;
        if (qi.ie == euEpoch) begin
            // Fetch
            //if (dInst.src1 matches tagged Valid .d &&& d matches tagged RegWord .r)
            //    dInst.srcVal1 = rf[pack(r)];
            //if (dInst.src2 matches tagged Valid .d &&& d matches tagged RegWord .r)
            //    dInst.srcVal2 = rf[pack(r)];
            if (dInst.srcm matches tagged Valid .d) begin
                dInst.srcValm = dMemRespFifo.first.data;
                dMemRespFifo.deq;
            end

            // Execute
            ExecInst eInst = exec(dInst, ip, pc);

            if(eInst.iType == Unsupported) begin
                $fwrite(stderr, "Executing unsupported instruction at ip: %x. Exiting\n", ip);
                $finish;
            end

            // WriteBack
            if (eInst.dst1 matches tagged Valid .r)
                rf[pack(r)] <= eInst.dstVal1;
            if (eInst.dst2 matches tagged Valid .r &&& eInst.dst1 != eInst.dst2)
                rf[pack(r)] <= eInst.dstVal2;
            if (eInst.dstm matches tagged Valid .d)
                dMemWriteReqFifo.enq(DMemReq{addr: d, data: eInst.dstValm});

            sb.remove2();
            
            //ipReg <= eInst.nextIp;

            $display("Execute: ip: %h", ip, ", dinst: ", fshow(dInst), ", einst: ", fshow(eInst), ", epoch: ", qi.ie);
        end
        mf2eFifo.deq;
    endrule

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

    method Action hostToCpu(Bit#(32) startpc) if ( !cop.started && memReady );
        cop.start;
        //ip <= startpc;
        pcReg <= truncate(startpc);
    endmethod

    interface MemInit iMemInit = iMem.init;
    interface MemInit dMemInit = dMem.init;
endmodule

