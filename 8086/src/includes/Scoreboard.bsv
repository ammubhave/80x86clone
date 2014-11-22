import SFifo::*;
import ProcTypes::*;

interface Scoreboard#(numeric type size);
    method Action insert(Maybe#(RegWord) r);
    method Action insert2(Maybe#(RegWord) x, Maybe#(RegWord) y);
    method Action remove;
    method Action remove2;
    method Bool search1(Maybe#(RegWord) r);
    method Bool search2(Maybe#(RegWord) r);
    method Bool search3(Maybe#(RegWord) r);
    method Action clear;
endinterface

function Bool isFound(Maybe#(RegWord) x, Maybe#(RegWord) k);
    if(x matches tagged Valid .xv &&& k matches tagged Valid .kv &&& kv == xv) begin
        return True;
    end else begin
        return False;
    end
endfunction

// search < insert < remove < clear
module mkBypassScoreboard(Scoreboard#(size));
    SFifo#(size, Maybe#(RegWord), Maybe#(RegWord))  f <- mkBypassSFifo(isFound);

    method insert = f.enq;
    method insert2 = f.enq2;

    method remove = f.deq;
    method remove2 = f.deq2;

    method search1 = f.search;
    method search2 = f.search;
    method search3 = f.search;

    method clear = f.clear;
endmodule
/*
// remove < search < insert < clear
module mkPipelineScoreboard(Scoreboard#(size));
    SFifo#(size, Maybe#(FullIndx), Maybe#(FullIndx)) f <- mkPipelineSFifo(isFound);

    method insert = f.enq;

    method remove = f.deq;

    method search1 = f.search;
    method search2 = f.search;
    method search3 = f.search;

    method clear = f.clear;
endmodule

*/