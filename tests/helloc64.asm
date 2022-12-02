; C64 Hello World
; assemble to .PRG file: asm6502 helloc64.asm helloc64.prg

.include "c64prg.inc"   ; .PRG header and BASIC upstart
.include "c64krnl.inc"  ; KERNAL function addresses

start:
        ldx #0
        sta $d020
@1      lda hello_msg,x
        jsr CHROUT
        inx
        cpx #hello_len
        bne @1
        rts

CR = $0C
LF = $0A

hello_msg .byte "HELLO, WORLD!", CR, LF
hello_len = @ - hello_msg
