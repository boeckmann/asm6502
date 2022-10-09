CLRSCR = $E544
CHROUT = $FFD2

.org $801

; BASIC code: 1024 SYS 2062
.byte $0C,$08,$40,$00,$9E,$20,$32,$30,$36,$32,$00,$00,$00 

  jsr CLRSCR
  ldx #$00
 
write:
  lda hello,x
  jsr CHROUT
  inx
  cpx #$05
  bne write
  rts

hello:
  .byte 72,69,76,76,79
