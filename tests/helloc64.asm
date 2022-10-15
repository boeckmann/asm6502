; C64 Hello World
; assemble to .PRG file: asm6502 helloc64.asm helloc64.prg

LOAD_ADDR = $0801

        .word LOAD_ADDR         ; .PRG header: load address
        .org  LOAD_ADDR

CHROUT = $FFD2                  ; kernal function address
SYS    = $9E                    ; basic SYS token number
CR     = 13                     ; carrige return character
LF     = %1010                  ; line feed character

basic_upstart:                  ; BASIC code: 10 SYS 2062
        .word @end, 10          ; ptr to next basic line and line number 10
        .byte SYS, " 2062", 0   ; SYS token and address string of subroutine
@end    .word 0                 ; null ptr to indicate end of basic text

start:                          ; this is at address 2062 ($080E)
        ldx #0
@l      lda hello_msg,x
        jsr CHROUT
        inx
        cpx #hello_len
        bne @l
        rts

hello_msg .byte "HELLO, WORLD!", CR, LF
hello_len = @ - hello_msg
