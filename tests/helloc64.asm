; C64 Hello World
; assemble to .PRG file: asm6502 helloc64.asm helloc64.prg

LOAD_ADDR = $0801

	.word LOAD_ADDR			; .PRG header: load address
	.org  LOAD_ADDR

CHROUT = $FFD2				; kernel function address
SYS    = $9E				; basic SYS token number
CR     = 13					; carrige return character
LF     = %1010				; line feed character

basic_upstart:				; BASIC code: 10 SYS 2062
	.word basic_end, %1010
	.byte SYS, " 2062", 0
  basic_end:
	.word 0 

start:
	ldx #0
write:
	lda hello_msg,x
	jsr CHROUT
	inx
	cpx #hello_len
	bne write
	rts

hello_msg :	.byte "HELLO, WORLD!", CR, LF
hello_len =	@ - hello_msg
