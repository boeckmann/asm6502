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
**The documentation is currently only a stub **

## Data types
Two data types are known to the assembler: 8-bit unsigned byte values and 16-but unsigned word values. In most cases, the type of an expression is automatically determined.

## Symbols
The assembler distinguishes two types of symbols: labels and variables. A label is defined at the beginning of a line by appending its name with a colon. Labels store the current address of the current instruction. Variables are defined by assigning an expression to them. In the following example, hello is a label and CHROUT is an expression. 

	CHROUT = $ffd2
	hello:	jmp CHROUT

Labels are always of type word. Variables may be of type byte or word, depending on the data type of the expression.

## Expressions
There are many places where expressions may occur, for example on the right side of a variable definition or as an argument to a machine instruction. The most primitive form of an expression is a numeric constant, which can be given in decimal, hexadecimal or binary. The value of the constant determines its type. A small value can be forced to be of type word by prepending zeros.

	5	; decimal byte constant
	$a 	; decimal byte constant
	$4711	; hex word constant
	%1011	; binary byte constant
	$00a	; hex word constant because more than 2 digits
	0123	; decimal word constant because more than 3 digits

Arithmetic operations may be used in expressions. Operator precedence is respected, as in the following example:

	2+3\*5	; yields value 17
	
The supported operations are the following:

  - lowest precedence: unary byte select: low byte (<) and high byte (>)
  - addition (+), subtraction(-), bitwise or (|)
  - multiplication (\*), bitwise and(&)
  - highest precedence: expressions enclosed by parentheses
  
Examples:

	<$4711	; selects low word $11
	>$4711		; selects high byte $47
	+(x+2)\*5

In the last example the unary + is only needed if used as an instruction argument to destinguish from 6502 indirect addressing mode.

## Addressing modes
The assembler supports all MOS6502 addressing modes.

### Implicit and accumulator addressing
Either no argument or accumulator is implicitly assumed by the instruction

	CLC	; clear carry
	ROR	; rotate accumulator right

### Immediate addressing
The byte sized argument is encoded in the byte following the opcode. The argument for the assembler instruction is prefixed by # to indicate an immediate value.

	LDA #42	; load value 42 into accumulator

### Relative addressing
Relative addressing is only used by branching instructions. The branch offset in the range of -128 to 127 is encoded by the byte following the opcode. The assembler interprets the argument, which may be any numeric expression, relative to the current program counter.

	loop:	bne loop

### Absolute addressing
A word sized address is encoded following the opcode byte. The assembler interpretes any word sized expression following an instruction mnemonic as an absolute address.

	lda	$4711	; load contents of address $4711 into accumulator

### Zeropage addressing
A byte sized address is encoded following the opcode byte. The assembler interpretes any byte sized expression following an instruction mnemonic as a zeropage address.

	LDA $47		; load contents of address $47 into accumulator
	LDA >$4711	; load contents of address $47 into accumulator

### Absolute X and absolute X addressing
The address is encoded in the word following the opcode and displaced by the contents for the X or Y register.

	LDA $4711,X	; load contents of address $4711 displaced by X
	LDA $4711,Y	; load contents of address $4711 displaced by Y

### Zeropage X and zeropage Y addressing
The address is encoded in the byte following the opcode and displaced by the contents for the X or Y register.

	LDA <$4711,X	; load contents of address $11 displaced by X
	LDX >$4711,Y	; load contents of address $47 displaced by Y into X

### Indirect addressing
The word sized address is stored in the memory location given by the word sized argument. In assembler syntax an indirect address is indicated by enclosing the argument in parentheses, like in the following.

	JMP ($4711)

This one is a syntax error, because the assembler assumes indirect addressing mode instead of a subexpression grouped by parentheses:

	JMP (2+3)\*1000

If one wants to start an expression with ( while not indicating indirect addressing to the assembler, one can for example write

	JMP +(2+3)\*1000

Examples are given below:

	CLC		; clear carry: implicit addressing
	ROR		; rotate right: accumulator addressing
	LDA #42		; load A with 42: immediate addressing mode
	LDA $4711	; load A with value at absolute address $4711
	LDA $47		; load A with value of zero page address $47
	