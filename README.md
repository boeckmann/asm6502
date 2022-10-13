# ASM6502
Early version of my 6502 assembler implementation. Hopefully ANSI C conformant. Confirmed to compile with clang, gcc, Pelles C and Borland C++ 3.1. While in an early stage, it supports all official MOS6502 instructions and addressing modes and generates correct opcodes for all supported instructions.


## Example
The following example implements a hello world program for the Commodore C64. Compile with `asm6502 helloc64.asm helloc64.prg`.

	; C64 Hello World
	; assemble to .PRG file: asm6502 helloc64.asm helloc64.prg

	LOAD_ADDR = $0801

		.word LOAD_ADDR			; .PRG header: load address
		.org  LOAD_ADDR

	CHROUT = $FFD2				; kernel function address
	SYS    = $9E				; basic SYS token number
	CR     = 13				; carrige return character
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


## Description
* The documentation is currently only a stub *

Two datatypes are known to the assembler: 8-bit unsigned byte values and 16-but unsigned word values. In most cases, the type of an expression is automatically determined.

The assembler destinguishes two types of symbols: labels and variables. A label is defined at the beginning of a line by appending its name with a colon. Labels store the current address of the program counter. Variables are defined by assigning them an expression. In the following example, hello is a label and CHROUT is an expression. 

	CHROUT = $ffd2
	hello:	jmp CHROUT

Labels are allways of type word. Variables may be of type byte or word.

There are many places where expressions may occur, for example on the right side of a variable definition or as an argument to a machine instruction. The most primitive form of an expression is a numeric constant, which can be given in decimal, hexadecimal or binary. The value of the constant determines its type. A small value can be forced to be of type word by prepending zeros.

	5	; decimal byte constant
	$a 	; decimal byte constant
	$4711	; hex word constant
	%1011	; binary byte constant
	$00a	; hex word constant because more than 2 digits
	0123	; decimal word constant because more than 3 digits

Arithmetic operations may be used in expressions. Common operator precedence is respected, as in the following example:

	2+3*5	; yields value 17
	
The supported operations are the following:

  - addition (+), subtraction(-), bitwise or (|)
  - multiplication (\*), bitwise and(&)

Multiplicative operators have higher precedence...
