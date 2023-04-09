# ASM6502
ASM6502 is a tiny 2-pass assembler for the MOS Technology 6502 microprocessor
used in many home computers of the 8-bit era. It consists of under 2k lines
of C code and can be built with compilers conformant to the C89 standard.

The assembler currently outputs plain binary files. It implements a few
features worth mentioning like local labels, the ability to produce listing
files and the optimization of opcodes. In its current state it is a useful
assembler confirmed to produce correct code for all supported instruction and
addressing mode combinations.

But the small size also comes with some compromises: macros and conditional
assembly are not supported. This may be implemented in the future.

## Example
The following example implements a hello world program for the Commodore C64.
Assemble with `asm6502 helloc64.asm helloc64.prg helloc64.lst` to generate
`helloc64.prg` containing the C64 program and `helloc64.lst` containing 
the program listing. 

	; C64 Hello World
	; assemble to .PRG file: asm6502 helloc64.asm helloc64.prg helloc64.lst
	
	LOAD_ADDR = $0801
	
	        .word LOAD_ADDR         ; .PRG header: load address
	        .org  LOAD_ADDR
	
	CHROUT = $FFD2                  ; kernal function address
	SYS    = $9E                    ; basic SYS token number
	
	basic_upstart:                  ; BASIC code: 10 SYS 2062
	        .word @end, 10          ; ptr to next basic line and line number 10
	        .byte SYS, " 2062",0    ; SYS token and address string of subroutine
	@end    .word 0                 ; null ptr to indicate end of basic text
	
	start:                          ; this is at address 2062 ($080E)
	        ldx #0
	@l      lda hello@msg,x
	        jsr CHROUT
	        inx
	        cpx #hello@len
	        bne @l
	        rts
	
	hello:
	  @msg .byte "HELLO, WORLD!", CR, LF
	  @len = @ - @msg
	
	CR = 13                         ; carrige return character
	LF = %1010                      ; line feed character as binary number

## Data types
Two data types are known to the assembler: 8-bit unsigned byte and 16-bit
unsigned word. In most cases, the type of an expression is automatically
determined.

## Symbols
The assembler distinguishes two types of case sensitive symbols: labels and
variables. A label stores the address of the current instruction or
directive. It is defined at the beginning of a line by appending its name
with a colon. The colon may be left out if the label name is not also an
instruction mnemonic.

A variable is defined by assigning an expression to it. In the following
example, hello is a label and CHROUT is a variable.

	CHROUT = $ffd2
	hello:  jmp CHROUT

Labels and variables may be of type byte or word. A label is of type byte
if it is assigned an address within the first 256 bytes (zero page),
otherwise it is of type word. The data type of a variable is that of the
expression assigned to it, unless it is forward referenced.

Symbols may be forward referenced. That means that they can be used in
expressions before they are defined. Forward referenced symbols are *always*
of type word, regardless what is assigned to them.

Labels may not be redefined. If a variable is assigned a value multiple times,
it must be the same value. Otherwise it is an illegal redefinition.

Symbols may be defined locally by prepending them with `@`. They are
associated with the previous non-local label defined. They may be referenced
within expressions locally by `@name` or with their qualified name
`label@name` outside their local scope. Example:

```
        jmp hello@l		; fully qualified label reference
hello:
  @l    jmp @l			; local label reference
```

## Expressions
There are many places where expressions may occur, for example on the right
side of a variable definition or as an argument to a machine instruction. The
most primitive form of an expression is a numeric constant, which can be given
in decimal, hexadecimal or binary. The value of the constant determines its
type. A small value can be forced to be of type word by prepending zeros.

	5     ; decimal byte constant
	$a    ; hexadecimal byte constant
	$4711 ; hexadecimal word constant
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
The special symbol `@` evaluates to the current value of the program counter.
It may not be confused with a local label, like `@abc`.

## Line syntax
Each line may end with a comment started by a semicolon.

At the beginning of a line a label may be specified if the line does not
contain a variable definition.

	start:              ; line consisting of a label
	loop: BNE loop      ; label and instruction
	msg:  .byte "Hello" ; label followed by a directive

Variables are defined by giving the variable name followed by equal sign
followed by an expression yielding a numeric value of type byte or word:

	CHROUT = $FFD2

## Directives
Directives instruct the assembler to do certain things. They may or may not
produce output data. Names of directives start with a dot. The directives
currently known to the assembler are:

### .ORG directive
Sets the current program counter to the numeric value of the argument. Does
not modify the offset into the output file. This means that .ORG can not be
used to "jump around" in the output file.

	.ORG $0801

### .FILL directive
Starting from the current position of the output file, emits as many bytes as
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

## .LIST and .NOLIST
If a listing file is given via command line, listing generation is initially
enabled. If the user wants some parts of the code to be excluded from the
listing, the region can be surrounded by `.NOLIST` and `.LIST` statements.

If listing generation is disabled when an `.INCLUDE` statement is processed,
`.LIST` inside the include file has no effect.

The listing generation flag is restored when the processing of an include file
finished. If a `.NOLIST` statement is contained in an include file and the
listing is activated for the parent file, listing generation is resumed
after processing the include file from the line after the `.INCLUDE` line.

## Constraints
The following constraints apply for AMS6502:

```
Maximum identifier length: 32
Maximum line length      : no restriction

Maximum number of files :  64
Maximum file name length: 255
Maximum include depth   :  32
```

## Instructions
Every assembler instruction consists of a mnemonic identifying the machine
instruction followed by at most one numeric argument including addressing
mode specifiers. Instruction mnemonics are case insensitive. The assembler
supports all MOS6502 addressing modes:

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
Indirect X addresses the byte referenced by the contents of the word stored at
zero page address b + X. Indirect Y adds Y to the address word stored in zero
page address b to calculate the address to operate on.

	b = 15
	ORA (b,X)
	ORA (b),Y

## Listing Files
ASM6502 is capable of producing listing files containing the generated
code in hexedecimal representation along the lines of the input file.

The column *FPos* indicates the position in the output file while *PC*
indicates the address of the program counter. FPos and PC may not be in sync
if an *.ORG* directive is used in the assembler text.

The listing also contains a list of global labels and variables, once sorted
by address and once sorted by name. 2-digit hex values in symbol table
indicate values of type byte. 4-digit hex values are of type word.

Listing of the `helloc64.asm` file from the introduction:

	ASM6502 LISTING FOR helloc64.asm @ 2023-04-09 14:19

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
	                            11: 
	                            12: basic_upstart:                  ; BASIC code: 10 SYS 2062
	0002  0801  0C 08 0A        13:         .word @end, 10          ; ptr to next basic line and line 	number 10
	0006  0805  9E 20 32 ...    14:         .byte SYS, " 2062",0    ; SYS token and address string of 	subroutine
	000D  080C  00 00           15: @end    .word 0                 ; null ptr to indicate end of basic 	text
	                            16: 
	                            17: start:                          ; this is at address 2062 ($080E)
	000F  080E  A2 00           18:         ldx #0
	0011  0810  BD 1C 08        19: @l      lda hello@msg,x
	0014  0813  20 D2 FF        20:         jsr CHROUT
	0017  0816  E8              21:         inx
	0018  0817  E0 0F           22:         cpx #hello@len
	001A  0819  D0 F5           23:         bne @l
	001C  081B  60              24:         rts
	                            25: 
	                            26: hello:
	001D  081C  48 45 4C ...    27:   @msg .byte "HELLO, WORLD!", CR, LF
	                            28:   @len = @ - @msg
	                            29: 
	                            30: CR = 13                         ; carrige return character
	                            31: LF = %1010                      ; line feed character (decimal 10)
	
	
	<<< SYMBOLS BY NAME >>>
	
	   HEX    DEC   NAME
	V  FFD2  65490   CHROUT                          
	V  000D     13   CR                              
	V  000A     10   LF                              
	V  0801   2049   LOAD_ADDR                       
	V    9E    158   SYS                             
	L  0801   2049   basic_upstart                   
	L  081C   2076   hello                           
	L  080E   2062   start                           
	
	
	<<< SYMBOLS BY VALUE >>>
	
	   HEX    DEC   NAME
	V  000A     10   LF                              
	V  000D     13   CR                              
	V    9E    158   SYS                             
	L  0801   2049   basic_upstart                   
	V  0801   2049   LOAD_ADDR                       
	L  080E   2062   start                           
	L  081C   2076   hello                           
	V  FFD2  65490   CHROUT                          
