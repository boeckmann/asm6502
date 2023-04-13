; Testdatei für 6502 Assembler

	ror			; acc
	ror 17		; zp
	ror $4711	; abs
	clc			; imp
	lda	#1 		; imm
	lda ($ff,x)	; ind x
	lda ($fe),y ; ind y
	jmp ($1234)	; ind
	lda	$1000	; abs
	lda	$02 	; zp
TEST:
	BNE	TEST 	; rel
	lda $02,x 	; zpx
	ldx $02,y  	; zpy
	lda !$02,x  ; abx
	lda !$02,y  ; aby
END:
