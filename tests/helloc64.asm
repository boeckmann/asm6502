; C64 Hello World
; assemble to .PRG file: asm6502 helloc64.asm helloc64.prg helloc64.lst

.include "c64prg.inc"           ; .PRG header and BASIC upstart
.include "c64krnl.inc"          ; C64 kernal definitions

; start: label gets implicitly defined by c64prg.inc, see listing
        ldx #0                  ; load value 0 into X, beginning of string
 @1     lda hello@msg,x         ; read string byte indexed by X
                                ; hello@msg is a local label
        jsr CHROUT              ; output char via kernal routine CHROUT
        inx                     ; next string byte
        cpx #hello@len          ; compare X with the string length stored in
                                ; local variable hello@len
        bne @1                  ; and jump to @1 if X < hello@len
        rts

hello:                          ; hello string label with locals @msg and @len
  @msg .byte "HELLO, WORLD!", CR, LF
  @len = @ - @msg               ; calculate length of string

CR = $0C                        ; give carrige return a symbolic name
LF = $0A                        ; ... also line feed
