;*****************************************************************************
; baseline.S
;-----------------------------------------------------------------------------
; micro benchmark to compute the execution time of 100 nop instructions
.TEXT

export _main
_main:
        nop
        nop

success:
        mov ax, #0x1233;
        inc ax;
        mov [0x1000], #0xDEAD;
        mov ah, #0xBE;
        mov bx, #0x5678;
        mov al, #0x0A;
        mov bl, #0xDE;
        mov ah, #0xBE;
        mov bh, #0xAD;
        nop;
        out 0xE9, al;
        mov al, #0;
        out 0xE8, al;
