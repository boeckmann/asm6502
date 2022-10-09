CLRSCR = $E544
CHROUT = $FFD2

.org $801

; BASIC code: 1024 SYS 2062
.byte $0C,$08,$40,$00,$9E,$20,$32,$30,$36,$32,$00,$00,$00 

  jsr CLRSCR    ; Call the Function that clears the screen
  ldx #$00      ; Put 0 in Register X (Index Register)
 
write:
  lda hello,x   ; Read next character from Address at Label .hello + Offeset X
  jsr CHROUT    ; CHROUT Subroutine, prints the Character loaded into Register A
  inx           ; Increments Register X by 1
  cpx #$0B      ; Compare if Value in Register X equals to 11
  bne write     ; If Value in Register X is not 11, go back to $033E
  rts           ; Return to Basic

hello:
