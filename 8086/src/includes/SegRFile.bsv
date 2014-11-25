import Types::*;
import ProcTypes::*;
import Vector::*;
import ConfigReg::*;

interface SegRFile;
    method Action wr(RegSeg r, Word data);
    method Word rd1 (RegSeg r);
    method Word rd2 (RegSeg r);
    method Word rd3 (RegSeg r);
endinterface

(* synthesize *)
module mkSegRFile( SegRFile );
    Vector#(4, Reg#(Word)) rfile <- replicateM(mkReg(0));

    function Word rd(RegSeg r);
        return rfile[pack(r)];
    endfunction

    method Action wr(RegSeg r, Word data);
        rfile[pack(r)] <= data;
    endmethod

    method Word rd1( RegSeg r ) = rd(r);
    method Word rd2( RegSeg r ) = rd(r);
    method Word rd3( RegSeg r ) = rd(r);
endmodule
