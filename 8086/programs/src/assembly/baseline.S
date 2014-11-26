;*****************************************************************************
; baseline.S
;-----------------------------------------------------------------------------
; micro benchmark to compute the execution time of 100 nop instructions
.TEXT

export _main
_main:
        ; Initialize stack at 0x2000
        mov bp, #0x2000;
        mov sp, #0x2000;

        ; Print "Hello"
        ; mov al, #0x48;
        ; out 0xE9, al;
        ; mov al, #0x45;
        ; out 0xE9, al;
        ; mov al, #0x4C;
        ; out 0xE9, al;
        ; mov al, #0x4C;
        ; out 0xE9, al;
        ; mov al, #0x4F;
        ; out 0xE9, al;
        ; mov al, #0xA;
        ; out 0xE9, al;

        ; ; Print string
        ; mov cx, #16;
        ; lea si, string;
        ; call print_str;

        ; Do add
        mov ax, #253;
        mov bx, #36;
        sub ax, bx;
        out 0xEA, al;
        mov al, ah;
        out 0xEA, al;

        ; Exit Passed
        mov al, #0;
        out 0xEB, al;
me:     jmp me;

print_str:
        cmp cx, #0;
        je print_str_end;
        mov al, [si];
        out 0xE9, al;
        mov ax, [si];
        inc si;
        dec cx;
        jmp print_str;
print_str_end:
        ret

string: .ascii "My name is Amol\n"

db 0, 0, 0, 0, 0
