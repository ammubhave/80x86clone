import Ehr::*;
import ProcTypes::*;
import Vector::*;

interface Scoreboard;
    method Action insert2(Maybe#(RegWord) x, Maybe#(RegWord) y);
    method Action remove2(Maybe#(RegWord) x, Maybe#(RegWord) y);
    method Bool search1(Maybe#(RegWord) r);
    method Bool search2(Maybe#(RegWord) r);
    method Bool search3(Maybe#(RegWord) r);
    method Action clear;
endinterface

// search < insert < remove < clear
(* synthesize *)
module mkBypassScoreboard(Scoreboard);
    Vector#(8, Ehr#(5, Bit#(3))) uses <- replicateM(mkEhr(0));

    function Bool search(Maybe#(RegWord) r);
        let not_found = True;
        if (r matches tagged Valid .d &&& uses[pack(d)][0] != 0)
            not_found = False;
        return not_found;
    endfunction

    method Action insert2(Maybe#(RegWord) x, Maybe#(RegWord) y);
        if (x matches tagged Valid .r)
            uses[pack(r)][0] <= uses[pack(r)][0] + 1;
        if (y matches tagged Valid .r)
            uses[pack(r)][1] <= uses[pack(r)][1] + 1;
        /*$display("      0: %h", uses[0][2], 
                     ", 1: %h", uses[1][2],
                     ", 2: %h", uses[2][2],
                     ", 3: %h", uses[3][2],
                     ", 4: %h", uses[4][2],
                     ", 5: %h", uses[5][2],
                     ", 6: %h", uses[6][2],
                     ", 7: %h", uses[7][2]);*/
    endmethod

    method Action remove2(Maybe#(RegWord) x, Maybe#(RegWord) y);
        if (x matches tagged Valid .r)
            uses[pack(r)][2] <= uses[pack(r)][2] - 1;
        if (y matches tagged Valid .r)
            uses[pack(r)][3] <= uses[pack(r)][3] - 1;

        /*$display("      0: %h", uses[0][4], 
                     ", 1: %h", uses[1][4],
                     ", 2: %h", uses[2][4],
                     ", 3: %h", uses[3][4],
                     ", 4: %h", uses[4][4],
                     ", 5: %h", uses[5][4],
                     ", 6: %h", uses[6][4],
                     ", 7: %h", uses[7][4]);*/
    endmethod

    method search1 = search;
    method search2 = search;
    method search3 = search;

    method Action clear;
        for (Integer i = 0; i < 8; i = i + 1)
            uses[fromInteger(i)][4] <= 0;
    endmethod
endmodule
