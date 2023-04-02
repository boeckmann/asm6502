# ASM6502
ASM6502 is a tiny assembler for the MOS Technology 6502 microprocessor used in
many home-computers of the 8-bit era. It consists of under 2k lines of C code
and can be built with compilers conformant to the C89 standard.

The assembler currently outputs plain binary files and is capable of producing
listing files. As of now it lacks a few features supported by more
sophisticated (and larger) assemblers, namely macros and conditional assembly.
Perhaps these limitations will be lifted in the future.

Apart from that it is a usefull assembler confirmed to produce correct code
for all supported instruction and addressing mode combinations.

## Example
The following example implements a hello world program for the Commodore C64.
Assemble with `asm6502 helloc64.asm helloc64.prg`. 

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

To produce a listing file during assembly run
`asm6502 helloc64.asm helloc64.prg hello64.lst`. The output looks like this:

    ASM6502 LISTING FOR helloc64.asm @ 2023-04-02 17:27
    
    FPos  PC    Code          Line# Assembler text
                                 1: ; C64 Hello World
                                 2: ; assemble to .PRG file: asm6502 helloc64.asm helloc64.prg
                                 3: 
                                 4: LOAD_ADDR = $0801
                                 5: 
    0000  0000  01 08            6:         .word LOAD_ADDR         ; .PRG header: load address
                                 7:         .org  LOAD_ADDR
                                 8: 
                                 9: CHROUT = $FFD2                  ; kernal function address
                                10: SYS    = $9E                    ; basic SYS token number
                                11: CR     = 13                     ; carrige return character
                                12: LF     = %1010                  ; line feed character
                                13: 
                                14: basic_upstart:                  ; BASIC code: 10 SYS 2062
    0002  0801  0C 08 0A        15:         .word @end, 10          ; ptr to next basic line and line number 10
    0006  0805  9E 20 32 ...    16:         .byte SYS, " 2062",0    ; SYS token and address string of subroutine
    000D  080C  00 00           17: @end    .word 0                 ; null ptr to indicate end of basic text
                                18: 
                                19: start:                          ; this is at address 2062 ($080E)
    000F  080E  A2 00           20:         ldx #0
    0011  0810  BD 1C 08        21: @l      lda hello_msg,x
    ...

The column *FPos* indicates the position in the output file while *PC*
indicates the address of the program counter. FPos and PC may not be in sync
if an *.ORG* directive is used in the assembler text. It may be used if the
binary image is not loaded to address zero, for example if it is loaded as
a .PRG file on commodore systems.

## Data types
Two data types are known to the assembler: 8-bit unsigned byte and 16-bit
unsigned word. In most cases, the type of an expression is automatically
determined.

## Symbols
The assembler distinguishes two types of case sensitive symbols: labels and
variables. A label is defined at the beginning of a line by appending its name
with a colon. The colon may be left out if the label name is not also an
instruction mnemonic. Labels store the address of the current instruction or
directive. Variables are defined by assigning an expression to them. In the
following example, hello is a label and CHROUT is a variable.

	CHROUT = $ffd2
	hello:  jmp CHROUT

Labels are always of type word. Labels may be defined as local labels by
prefixing them with `@`. Their scope reaches from the previously defined
non-local label to the next non-local label.

Variables may be of type byte or word, depending on the data type of the
expression.

## Expressions
There are many places where expressions may occur, for example on the right
side of a variable definition or as an argument to a machine instruction. The
most primitive form of an expression is a numeric constant, which can be given
in decimal, hexadecimal or binary. The value of the constant determines its
type. A small value can be forced to be of type word by prepending zeros.

	5     ; decimal byte constant
	$a    ; decimal byte constant
	$4711 ; hex word constant
	%1011 ; binary byte constant
	$00a  ; hex word constant because more than 2 digits
	0123  ; decimal word constant because more than 3 digits
	'x'   ; byte typed ascii charactar code of x
	-1    ; word constant $FFFF (2-complement)

Arithmetic operations may be used in expressions. Operator precedence is
respected, as in the following example:

	2+3*5 ; yields value 17
	@ - 2 ; current program counter - 2
	
The supported operations are the following:

	- lowest precedence: unary byte select: low byte (<) and high byte (>)
	- unary and binary addition (+) and subtraction(-), bitwise or (|), exclusive or (^)
	- multiplication (*), division (/), bitwise and(&)
	- highest precedence: expressions enclosed by parentheses
	
Examples:

	<$4711    ; selects low byte $11
	>$4711    ; selects high byte $47
	+(x+2)*5

In the last example the unary + is only needed if used as an instruction
argument to destinguish from 6502 indirect addressing mode.

### Current Program Counter
The special symbol `@` evaluates to the current program counter. It may not be
confused with a local label, like `@abc`.

## Line syntax
Each line may end with a comment. Comments start with a semicolon.

At the beginning of a line a label may be specified if the line does not
contain a variable definition.

	start:              ; line consisting of a label
	loop: BNE loop      ; label and instruction
	msg:  .byte "Hello" ; label followed by a directive

Variables are defined by giving the variable name followed by equal sign
followed by an expression yielding a numeric value:

	CHROUT = $FFD2

## Directives
Directive instruct the assembler to do certain things. They may or may not
produce output data. Names of directives start with a dot. The directives
currently known to the assembler are:

### .ORG directive
Sets the current program counter to the numeric value of the argument. Does
not modify the offset into the output file. This means that .ORG can not be
used to "jump around" in the output file.

	.ORG $0801

### .FILL directive
Starting from the current offset of the output file, emits as many bytes as
given by the first argument. If the second argument is given, the region is
filled with its byte-sized value. Otherwise it is filled with zero. The 
program counter is increased accordingly.

	.FILL 100       ; fill 100 bytes with zero
	.FILL 16, $EA	; insert 16 NOPs ($EA) into the code

### .INCLUDE directive
Substitutes the directive with the contents of a file given by the argument.

	.INCLUDE "c64krnl.inc"
	
### .BYTE directive
Produces one or more output bytes. The arguments are separated by comma.
Strings enclosed by " may also be used.

	.BYTE 1
	.BYTE 47, 11
	.BYTE "Hello, World", 13, 10

### .WORD directive
Produces one or more output words.

	.WORD $0801

## Instructions
In contrast to symbols, instruction mnemonics are case insensitive. Every
assembler instruction consists of a mnemonic followed by at most one numeric
argument including addressing mode specifiers.

## Addressing modes
The assembler supports all MOS6502 addressing modes.

### Implicit and accumulator addressing
Either no argument or accumulator is implicitly assumed by the instruction

	CLC ; clear carry
	ROR ; rotate accumulator right

### Immediate addressing
The byte sized argument is encoded in the byte following the opcode. The
argument for the assembler instruction is prefixed by # to indicate an
immediate value.

	LDA #42 ; load value 42 into accumulator

### Relative addressing
Relative addressing is only used by branching instructions. The branch offset
in the range of -128 to 127 is encoded by the byte following the opcode. The
assembler interprets the argument, which may be any numeric expression,
relative to the current program counter.

	loop: BNE loop

### Absolute addressing
A word sized address is encoded following the opcode byte. The assembler
interpretes any word sized expression following an instruction mnemonic as an
absolute address.

	LDA $4711 ; load contents of address $4711 into accumulator

### Zeropage addressing
A byte sized address is encoded following the opcode byte. The assembler
interpretes any byte sized expression following an instruction mnemonic as a
zeropage address.

	LDA $47   ; load contents of address $47 into accumulator
	LDA >$4711  ; load contents of address $47 into accumulator

### Absolute X and absolute X addressing
The address is encoded in the word following the opcode and displaced by the
contents for the X or Y register.

	LDA $4711,X ; load contents of address $4711 displaced by X
	LDA $4711,Y ; load contents of address $4711 displaced by Y

### Zeropage X and zeropage Y addressing
The address is encoded in the byte following the opcode and displaced by the
contents for the X or Y register.

	LDA $47,X ; load contents of address $47 displaced by X
	LDX >$4711,Y  ; load contents of address $47 displaced by Y into X

### Indirect addressing
The word sized address is stored in the memory location given by the word
sized argument. In assembler syntax an indirect address is indicated by
enclosing the argument in parentheses, like in the following.

	JMP ($4711)

The following one is a syntax error, because the assembler assumes indirect
addressing mode instead of a subexpression grouped by parentheses:

	JMP (2+3)*1000

If one wants to start an expression with ( while not indicating indirect
addressing to the assembler, one can for example write

	JMP +(2+3)*1000

This one is correct (indirect addressing):

	JMP ((2+3)*1000)

## Indirect X and indirect Y addressing

	ORA (15,x)
	ORA (15),y
