                         ASM6502 Assembler Manual
                         ========================

Copyright 2022-2023 by Bernd Boeckmann

1 Where to get
--------------

       Latest source and binaries may be found at:

        -  https://github.com/boeckmann/asm6502

        -  https://codeberg.org/boeckmann/asm6502

       Binaries are provided for Windows, DOS, and OS/2. A Unix make
       file is provided which should be easily adaptable to any ANSI C
       conformant compiler.

       There is a port of the Commodore 64 KERNAL v3 and BASIC v2 source to
       ASM6502. If it is of interest to you, get it at:

        -  https://github.com/boeckmann/c64krnl

2 Introduction
--------------

       ASM6502 is a small two-pass assembler for the MOS Technology 6502
       microprocessor used in many home computers of the 8-bit era. It
       consists of under 3K lines of C code and can be built with compilers
       conformant to the C89 standard.

       ASM6502 implements some advanced features, like local labels,
       conditional assembly, optimization of opcodes, and the ability to
       produce listing files. It is powerful enough to produce byte-exact
       replicas of the Commodore C64 KERNAL and BASIC ROMs.

       The assembler outputs plain binary files.

       The following listing contains a small sample program. It is
       the classic hello world for the Commodore C64. To assemble it,
       invoke the assembler with the source, output, and listing files as
       arguments:

       asm6502 helloc64.a65 -o helloc64.prg -l helloc64.lst

          1: ; C64 Hello World
          2:
          3: LOAD_ADDR = $0801
          4:
          5:       .word LOAD_ADDR
          6:       .org  LOAD_ADDR
          7:
          8: CHROUT = $FFD2
          9: SYS    = $9E
         10:
         11: basic_upstart:
         12:       .word @end, 10
         13:       .byte SYS, " 2062",0
         14: @end  .word 0
         15:
         16: start:
         17:       ldx #0
         18: @l    lda hello@msg,x
         19:       jsr CHROUT
         20:       inx
         21:       cpx #hello@len
         22:       bne @l
         23:       rts
         24:
         25: hello:
         26:   @msg .byte "HELLO, WORLD!", CR, LF
         27:   @len = @ - @msg
         28:
         29: CR = 13
         30: LF = %1010

3 Concepts and Terminology
--------------------------

       The task of an assembler is to translate an _assembler
       source_ containing human-readable _instructions_ to a _binary
       representation_ the processor understands. This binary
       representation is called _machine code_. There is a one-to-one
       mapping between the human-readable instructions contained in the
       assembler source and the generated machine code.

       Each instruction a processor understands is given a name, called
       _mnemonic_. This name is chosen so that it describes to the
       programmer what the instruction does. Every instruction is also
       assigned a numeric value, called the operation code or _opcode_.
       This is what gets written by the assembler as machine code to an
       output file. The opcode is interpreted by the processor to decide
       what to do.

       Beside the instruction itself, additional information may be
       required to process it. The additional information is provided in
       the source by one or more _arguments_ following the mnemonic. In
       machine code this additional information is encoded in binary form
       following the opcode.

       `ADC #42' is an example of an instruction, where ADC is the
       mnemonic identifying the instruction, and #42 is the argument. This
       particular instruction, understood by the MOS6502 processor, adds
       the value 42 to the value stored in processor register A. It then
       writes the result back to A.

       The set of instructions a CPU understands is called _instruction
       set_. There are many different kinds of CPUs and instruction sets.
       As a result, there is not something like `the assembler', but there
       are many of them, all adapted to one or more specific instruction
       sets. ASM6502 is an assembler that is adapted to the instruction set
       of the MOS6502 processor family.

       Beside generating an output file containing machine code, ASM6502
       may also generate a _listing file_ containing a _program listing_.
       This listing shows the lines of the assembler source side-by-side to
       the generated machine code in hexadecimal notation.

         Pos   Addr  Code          Line# Assembler text
         0000  0000  69 2A            1: ADC #42
         0002  0002  E8               2: INX

       Pos indicates the position of the instructions regarding the output
       file. Addr represents the _location_ or _address_ of the code while
       it is executed.

       Let's further elaborate what the arguments to instructions may be.
       In the example above, `#42' is a numeric value that is directly used
       to do the addition. It is therefore called an _immediate_ value, and
       the mode the processor operates in is called _immediate addressing_
       mode. The `INX' instruction above implicitly operates on a register
       called X. For this reason it is called _implicit addressing_. Often,
       the argument specifies a memory location. This memory location
       may be referenced to the start of the address space. In this case
       it is called _absolute addressing_ mode. If the memory location
       is specified relative to some other location we call it _relative
       addressing_ mode. Sometimes one does not want to encode a fixed
       memory location into the machine instruction, but instead use the
       content of some memory location as the address to operate on. This
       is called _indirect addressing_.

       The sequence of instructions executed by the processor may be
       altered by the programmer utilizing special machine instructions.
       Some of these instructions modify this sequence unconditionally,
       and some alter it if a special condition is met. The instructions
       are called _jump_ or _branching_ instructions. The information of
       the _jump target_ is encoded as address within the instruction. The
       assembler supports the programmer by letting him specify a jump
       target by giving it a name, called _label_. In the introductory
       example, `basic_upstart', `start', and `hello' are labels.

       One task of an assembler is to assign an address to every label the
       programmer defines. Sometimes this is not that easy. For example,
       a label may be used as a jump target before it is defined in the
       assembler source. This is called _forward-reference_. That is the
       reason ASM6502 is a so called two-pass assembler. In the first pass,
       the assembler processes the whole file to determine the addresses of
       all labels. In the second pass all required information is provided,
       and the machine code is generated.

       Using forward-references can sometimes lead to non-optimal machine
       code, because the assembler has to guess the size of the forward-
       referenced `thing'. In that case the programmer can support the
       assembler by giving a hint what type the referenced `thing' is. Some
       _multi-pass assemblers_ process the source more than two times and
       can optimize the machine code even in cases they encounter forward-
       referenced labels. But for the sake of simplicity ASM6502 does not
       do that.

       Beside labels the programmer may also define _variables_. Variables
       may be assigned any kind of mathematical expression. Variables and
       labels are also called _symbols_, and the name identifying them is
       referred to as _identifier_.

       Character sequences which by itself provide a value to the
       assembler, like the character sequence `42' that represents the
       numeric value 42, are considered to be _literals_.

4 Syntax and Semantics
----------------------

       The following chapter describes the data model and the syntax
       accepted by the assembler.

   4.1 Input Files

       Input files should have .a65 as extension to distinguish it from
       files written for other assemblers. The files should be encoded in
       the ASCII or UTF-8 character sets.

   4.2 Data Types

       Two data types are known to the assembler:

        -  _byte_: 8-bit, storing whole numbers between 0 and 255.

        -  _word_: 16-bit, storing positive whole numbers between 0 and
           65535. Negative numbers may also be stored in the range from -
           32768 to -1. Negative numbers are stored in two-complement
           representation.

       There is no distinct boolean type. In a boolean context, any value
       other than zero is considered to be a boolean true.

   4.3 Symbols

       The assembler distinguishes two types of case-sensitive symbols:
       _labels_ and _variables_. A label stores the address of the
       instruction or data that is currently assembling. It is defined
       at the beginning of a line by appending its name with a colon.
       The colon may be left out if the label name is not an instruction
       mnemonic.

       A variable is defined by assigning an expression to it. In the
       following example, hello is a label, and CHROUT is a variable.

         CHROUT = $ffd2
         hello:  jmp CHROUT

       Labels and variables may be of type byte or word. A label is of type
       byte if it is assigned an address within the first 256 bytes (zero-
       page). Otherwise, it is of type word. The data type of a variable
       is that of the expression assigned to it, unless it is forward-
       referenced.

       Forward-referenced means that a symbol is used in an expression
       before it is defined. Forward-referenced symbols are _always_ of
       type word, regardless of what is assigned to them.

       If a variable is assigned a value multiple times, it must be the
       same value. Otherwise, it is an illegal redefinition. Labels may not
       be defined more than once.

       Variables and labels may be defined locally by prepending their name
       with @. They are then associated with the previous non-local label
       defined. They may be referenced within expressions locally by @name
       or with their qualified name label@name outside their local scope.
       Example:

                 jmp hello@l   ; fully qualified label reference
         hello:
           @l    jmp @l        ; local label reference

   4.4 Expressions

       There are many places where expressions may occur, for example on
       the right side of a variable definition or as an argument to a
       machine instruction.

       Every expression is either of type byte, word or of unknown type.
       Every expression also has a defined numeric value or an undefined
       value.

 4.4.1 Primitive Expressions

       The most primitive form of an expression is a numeric constant,
       which can be given in decimal, hexadecimal, or binary. The value of
       the constant determines its type.

         5     ; decimal byte constant
         $a    ; hexadecimal byte constant
         $4711 ; hexadecimal word constant
         %1011 ; binary byte constant
         -1    ; word constant $FFFF (2-complement)

       A byte-sized value can be forced to be of type word by prepending
       zeros.

         $00a  ; hex word constant because more than 2 digits
         0123  ; decimal word constant because more than 3 digits

       A character enclosed by ' is evaluated to its ASCII value.

         'x'   ; byte typed ASCII character code of x

       The address counter symbol @ returns the address of the currently
       assembled instruction. The symbol .? returns an undefined value of
       unknown type.

         .?    ; undefined value
         @     ; current address

       Label and variable names evaluate to their respective value.

         LOAD_ADDR = 2048
         lda LOAD_ADDR     ; load memory cell 2048 into accumulator

 4.4.2 Operator Precedence

       Expressions may be composed of arithmetic sub-expressions. Operator
       precedence is respected.

       The supported operations from highest to lowest precedence are:

        -  Expressions enclosed by parentheses ()

        -  bit-wise complement ~, logical negation !, and is-defined ?
           operators, all unary and right-associative

        -  Multiplication *, division /, bit-wise and &, logical left <<
           and right >> shift

        -  Unary plus and binary addition +, unary minus and subtraction -,
           bit-wise or |, exclusive or ^

        -  Unary low < and high > byte select, lossless unary conversion
           operators [b] and [w]

        -  The comparison operators are ==, !=, <, >, <=, >=

        -  Logical and &&

        -  Logical or ||

        -  Defined-or-else ?:

 4.4.3 Arithmetic and bit-wise Operators

       The usual semantics for the arithmetic operators apply.

       If there is an undefined argument to one of the arithmetic
       operators, the result value is undefined. Type inference is
       performed as such that if any of the arguments is of type word, the
       result is of type word. The result is also of type word if it would
       otherwise overflow the range of type byte.

       The bit-wise complement operator ~ respects the data type when
       toggling bits. This means that ~1 becomes $FE, and not $FFFE.

       Examples:

         2+3*5        ; value 17
         $4700 | $11  ; is $4711
         255+255      ; of type word because >256

 4.4.4 Byte-select and Conversion Operators

       The low-byte select operator < returns the low byte of a word-sized
       expression, or the unmodified value of a byte-sized expression. The
       high-byte select operator > returns the high byte of a word-sized
       expression shifted eight bits to the right. It returns zero for
       byte-sized expressions. The resulting data type of both operators is
       byte. If applied to an undefined argument, the result is undefined.

       The convert to byte [b] and convert to word [w] operators change the
       data type of their expression. If the expression does not fit into a
       byte, [b] raises an error. The operators also change the type of the
       undefined value while retaining undefined as a value.

 4.4.5 Comparison and Logical Operators

       The comparison operators return 1 as true value, and 0 as false
       value.

       They return an undefined value if at least one of their arguments is
       undefined, with the logical or || operator being an exception. It
       returns true if at least one of its arguments is true.

       The logical negation operator ! is right-associative.

 4.4.6 Is-defined Operator

       The unary, right associative is-defined operator ? returns true, if
       its argument is defined. Otherwise, false is returned. The result is
       of type byte.

 4.4.7 Defined-or-else Operator

       The defined-or-else operator ?: returns its first argument if it is
       defined, otherwise its second. It is left-associative.

       Example:

         Y = .?
         Z = X ?: Y ?: $DEAD  ; assign $DEAD to Z
                              ; because X and Y are undefined

   4.5 Line Format

       A line may either contain a statement or a conditional statement
       or none of them. A statement it either a variable definition, an
       instruction or a directive. Instructions and directives may be
       preceded by a label definition. Also, a label definition may stand
       for its own. Conditional statements are .IF, .ELSE, and .ENDIF.
       These may not be preceded by a label. Each line may end with a
       comment. Comments are started by semicolon and ignored by the
       assembler.

         start:              ; line consisting only of a label
         loop: BNE loop      ; label and instruction
         msg   .byte "Hello" ; label followed by a directive
         X = 42              ; variable definition
         .if X == 42         ; conditional statement

   4.6 Directives

       Directives instruct the assembler to do certain things. They may or
       may not produce output data. Names of directives start with a dot.
       The directives currently known to the assembler are:

 4.6.1 .ALIGN

       Aligns the address counter to the next multiple of the first
       argument, and emits the number of bytes necessary to perform the
       alignment. If the second byte-sized argument is given, this value is
       used as fill-byte. Otherwise the fill-byte is zero.

         .ALIGN 4         ; align @ to a multiple of 4
         .ALIGN $100, $EA ; align to next page, fill with $EA

 4.6.2 .ASSERT and .ASSERT1

       Tests if the expression given first argument returns a true value,
       otherwise terminates with an error. .ASSERT runs on pass two, and
       ASSERT1 runs on pass one. The arguments following the first are
       handled like the .ECHO directive does it.

       Example:

         .ASSERT 2 > 1, "arithmetic implementation is flawed"

 4.6.3 .BINARY Directive

       Copies binary data from a file to the output file. Numeric
       expressions specifying a start offset and a length may be given
       as arguments. If a length is given, the start offset must also be
       specified.

       Example:

         .BINARY "SPRITE.DAT".         ; copies the whole file
         .BINARY "SPRITE.DAT", $10     ; skip the first 16 bytes
         .BINARY "SPRITE.DAT", $10, 64 ; copy 64 bytes from offset 16

 4.6.4 .BYTE Directive

       Produces one or more output bytes. The arguments are separated by a
       comma. Numeric expressions or strings may be used as arguments. The
       values of numeric expressions must fit into a byte. Strings must be
       enclosed by ".

       Example:

         .BYTE 1
         .BYTE 47, 11
         .BYTE "Hello, World", 13, 10

 4.6.5 .CPU Directive

       Selects the instruction set the assembler understands depending on
       the given CPU type.

         .CPU 6502   ; targets the NMOS 6502 CPU
         .CPU 65C02  ; targets the CMOS 65C02 CPU (experimental)

 4.6.6 .ECHO and .ECHO1 Directives

       Print the arguments to standard output. .ECHO does it on the
       second assembler pass, while .ECHO1 does it on the first pass. The
       arguments may either be strings or numeric expressions, separated by
       comma. Numeric expressions may be prefixed by the format specifier
       [$] to output the number in hexadecimal format. Otherwise, it is
       printed in decimal.

       Example:

         .ECHO "hexadecimal representation of ", 4711, " is ", [$]4711

 4.6.7 .ERROR directive

       Aborts the assembly along with file name and line number
       information. Accepts the same parameters as .ECHO for the error
       message.

 4.6.8 .FILL Directive

       Starting from the current position of the output file, emits as many
       bytes as given by the first argument. If the second argument is
       given, the region is filled with its byte-sized value. Otherwise, it
       is filled with zero. The address counter @ is increased accordingly.

       Example:

         .FILL 100       ; fill 100 bytes with zero
         .FILL 16, $EA   ; insert 16 NOPs ($EA) into the code

 4.6.9 .IF, .IFN, .ELSE and .ENDIF Directives

       Conditionally assembles code if the condition of the argument to .IF
       or .IFN is met. For .IF, the condition is met if the argument yields
       a defined value other than zero. For .IFN, the condition is met if
       the argument does not yield a defined value or the value is zero.

       If the condition is met, the code between .IF or .IFN and .ELSE is
       assembled, or between .IF or .IFN and .ENDIF, if .ELSE is not given.
       If the argument to .IF or .IFN is not met and .ELSE is specified,
       the code between .ELSE and .ENDIF is assembled.

       .IF and .IFN treat an undefined expression like a boolean false.

       The conditional directives may _not_ be preceded by a label.

       Example:

         C64 = 1
         .IF C64
           .ECHO "I am assembled for the C64"
         .ELSE
           .ECHO "I am assembled for the PET"
         .ENDIF

       In listing files, the unprocessed lines are indicated by a minus
       after the line number instead of a colon.

4.6.10 .IFDEF and .IFNDEF Directives

       An argument to .IFDEF is considered true, if its value is defined.
       An argument to .IFNDEF is considered true, if its value is
       undefined. Otherwise, the directives behave like their .IF and .IFN
       counterparts.

4.6.11 .INCLUDE Directive

       Substitutes the directive with the assembler source contained in the
       file given as argument.

       Example:

         .INCLUDE "c64prg.i65"

4.6.12 .LIST and .NOLIST Directives

       If a listing file is given via command line, listing generation is
       initially enabled. If the user wants some parts of the code to be
       excluded from the listing, the region can be surrounded by .NOLIST
       and .LIST statements.

       If listing generation is disabled when an .INCLUDE statement is
       processed, .LIST inside the included file has no effect.

       A .NOLIST inside an include file does not propagate to the parent
       file.

4.6.13 .ORG Directive

       Sets the address counter to the numeric value of the argument. Does
       not modify the offset into the output file. This means that .ORG
       can not be used to `jump around' in the output file. May be used
       multiple times.

       Example:

         .ORG $0801

4.6.14 .REPEAT and .ENDREP

       Repeats the block of code enclosed by .REPEAT and .ENDREP the number
       of times given by the argument to .REPEAT.

       Example:

         .REPEAT 30      ; ...for 30 overscan scanlines...
           sta WSYNC
         .ENDREP

4.6.15 .SYM and .NOSYM Directives

       Selectively enables or disables the inclusion of defined labels and
       variables in the symbol map for specific code regions. .SYM enables
       it (default), and .NOSYM disables it. The symbol map is part of the
       program listing.

4.6.16 .WARNING Directive

       Prints a warning along with file name and line number information.
       Accepts the same parameters as .ECHO for the warning message.

4.6.17 .WORD Directive

       Produces one or more output words.

       Example:

         .WORD $0801, 4711

   4.7 Addressing Modes

       Every assembler instruction consists of a mnemonic identifying
       the machine instruction followed by at most one numeric argument
       including addressing mode specifiers. Instruction mnemonics are
       case-insensitive. The assembler supports all MOS6502 addressing
       modes:

 4.7.1 Implicit and Accumulator Addressing

       Either no argument or accumulator is implicitly assumed by the
       instruction

         CLC ; clear carry
         ROR ; rotate accumulator right

 4.7.2 Immediate Addressing

       The byte-sized argument is encoded in the byte following the opcode.
       The argument for the assembler instruction is prefixed by # to
       indicate an immediate value. The argument may be any expression
       yielding a byte-sized numeric value.

         LDA #42 ; load value 42 into the accumulator

 4.7.3 Relative Addressing

       Relative addressing is only used by branch instructions. The branch
       offset in the range of -128 to 127 is encoded by the byte following
       the opcode. The assembler interprets the argument, which may be any
       numeric expression, relative to the current address counter.

         loop: BNE loop

 4.7.4 Absolute Addressing

       A word-sized address is encoded following the opcode byte. The
       assembler interprets any word-sized expression following an
       instruction mnemonic as an absolute address.

         LDA $4711 ; load contents of address $4711 into the accumulator

 4.7.5 Zero-page Addressing

       A byte-sized address is encoded following the opcode byte. The
       assembler interprets any byte-sized expression following an
       instruction mnemonic as a zero-page address.

         LDA $47   ; load contents of address $47 into the accumulator
         LDA >$4711  ; load contents of address $47 into the accumulator

 4.7.6 Absolute X and Absolute X Addressing

       The address is encoded in the word following the opcode and
       displaced by the contents for the X or Y register.

         LDA $4711,X ; load contents of address $4711 displaced by X
         LDA $4711,Y ; load contents of address $4711 displaced by Y

 4.7.7 Zero-page X and Zero-page Y Addressing

       The address is encoded in the byte following the opcode displaced by
       the contents for the X or Y register.

         LDA $47,X ; A = contents of address $47 displaced by X
         LDX $11,Y ; X = load contents of address $47 displaced by Y

 4.7.8 Indirect Addressing

       The word-sized address is stored in the memory location given by
       the word-sized argument. In assembler syntax, an indirect address
       is indicated by enclosing the argument in parentheses, like in the
       following.

         JMP ($4711)

       The following one is a syntax error, because the assembler assumes
       indirect addressing mode instead of a sub-expression grouped by
       parentheses:

         JMP (2+3)*1000

       To correct this, you may rewrite it as:

         JMP +(2+3)*1000

       This one is correct (indirect addressing):

         JMP ((2+3)*1000)

 4.7.9 Indexed Indirect by X and Indirect Indexed by Y Addressing

       Indexed indirect by X addresses the byte referenced by the contents
       of the word stored at zero-page address b + X. Indirect indexed
       by Y adds Y to the address word stored in zero-page address b to
       calculate the address to operate on.

         b = 15
         ORA (b,X)
         ORA (b),Y

A Command Line Syntax
---------------------

         Usage: asm6502 input -o output [options]... [VAR=number]...
         
         Options:
           -q             be quiet, unless an error occurred
           -o output      set output file name
           -l listing     set optional listing file name
           -w0            disable all warnings
           -w1            enable warnings (default)
           -w2            enable warnings and hints

       Variables defined from the command line are known to the assembler
       when assembling files. The numbers are parsed like number literals
       in the source code.

B Instruction Reference
-----------------------

       In the following instruction list, #$42 is a representative for a
       byte-sized immediate value. This value may be substituted by any
       other byte-sized value. $15 is a representative for a zero-page
       memory address, and $4711 is a representative for a word-sized
       memory address.

       The first hexadecimal value on a line is the instruction opcode
       followed by at most two bytes of additional data defined by the
       instruction argument. The syntax of the different addressing modes
       is described in one of the previous chapters.

   B.1 ADC - add with carry

       Flags: N Z C V

         69 42      adc #$42
         65 15      adc $15
         75 15      adc $15,x
         6D 11 47   adc $4711
         7D 11 47   adc $4711,x
         79 11 47   adc $4711,y
         61 15      adc ($15,x)
         71 15      adc ($15),y

   B.2 AND - bit-wise and with accumulator

       Flags: N Z

         29 42      and #$42
         25 15      and $15
         35 15      and $15,x
         2D 11 47   and $4711
         3D 11 47   and $4711,x
         39 11 47   and $4711,y
         21 15      and ($15,x)
         31 15      and ($15),y

   B.3 ASL - arithmetic shift left

       Flags: N Z C

         0A         asl
         06 15      asl $15
         16 15      asl $15,x
         0E 11 47   asl $4711
         1E 11 47   asl $4711,x

   B.4 BCC - branch if carry cleared

         90 FE      bcc @

   B.5 BCS - branch if carry set

         B0 FE      bcs @

   B.6 BEQ - branch if equal

         F0 FE      beq @

   B.7 BIT - test bits

       Negative flag becomes the bit 7 of the operand, overflow flag
       becomes bit 6 of the operand. Zero flag is set if the bit-wise and
       operation between the accumulator and the operand is zero, otherwise
       it is cleared.

       Flags: N Z V

         24 15      bit $15
         2C 11 47   bit $4711

   B.8 BMI - branch if negative

         30 FE      bmi @

   B.9 BNE - branch if not equal

         D0 FE      bne @

  B.10 BPL - branch if positive

         10 FE      bpl @

  B.11 BRK - force break

       BRK cannot be masked by setting interrupt disable flag. Forces the
       processor to continue at the address stored in the IRQ vector $FFFE.
       Pushes the flags with set break (B) flag to differentiate from a
       hardware interrupt. RTI and PLP ignore the break flag.

       Flags: I=1

         00         brk

  B.12 BVC - branch if overflow flag cleared

         50 FE      bvc @

  B.13 BVS - branch if overflow flag set

         70 FE      bvs @

  B.14 CLC - clear carry flag

       Flags: C=0

         18         clc

  B.15 CLD - clear decimal flag

       Flags: D=0

         D8         cld

  B.16 CLI - clear interrupt disable flag

       Flags: I=0

         58         cli

  B.17 CLV - clear overflow flag

       Flags: V=0

         B8         clv

  B.18 CMP - compare with accumulator

       Flags: N Z C

         C9 42      cmp #$42
         C5 15      cmp $15
         D5 15      cmp $15,x
         CD 11 47   cmp $4711
         DD 11 47   cmp $4711,x
         D9 11 47   cmp $4711,y
         C1 15      cmp ($15,x)
         D1 15      cmp ($15),y

  B.19 CPX - compare with X register

       Flags: N Z C

         E0 42      cpx #$42
         E4 15      cpx $15
         EC 11 47   cpx $4711

  B.20 CPY - compare with Y register

       Flags: N Z C

         C0 42      cpy #$42
         C4 15      cpy $15
         CC 11 47   cpy $4711

  B.21 DEC - decrement

       Flags: N Z

         C6 15      dec $15
         D6 15      dec $15,x
         CE 11 47   dec $4711
         DE 11 47   dec $4711,x

  B.22 DEX - decrement X register

       Flags: N Z

         CA         dex

  B.23 DEY - decrement Y register

       Flags: N Z

         88         dey

  B.24 EOR - exclusive or

       Flags: N Z

         49 42      eor #$42
         45 15      eor $15
         55 15      eor $15,x
         4D 11 47   eor $4711
         5D 11 47   eor $4711,x
         59 11 47   eor $4711,y
         41 15      eor ($15,x)
         51 15      eor ($15),y

  B.25 INC - increment

       Flags: N Z

         E6 15      inc $15
         F6 15      inc $15,x
         EE 11 47   inc $4711
         FE 11 47   inc $4711,x

  B.26 INX - increment X register

       Flags: N Z

         E8         inx

  B.27 INY - increment Y register

       Flags: N Z

         C8         iny

  B.28 JMP - jump

         4C 11 47   jmp $4711
         6C 11 47   jmp ($4711)

  B.29 JSR - call subroutine

         20 11 47   jsr $4711

  B.30 LDA - load accumulator

       Flags: N Z

         A9 42      lda #$42
         A5 15      lda $15
         B5 15      lda $15,x
         AD 11 47   lda $4711
         BD 11 47   lda $4711,x
         B9 11 47   lda $4711,y
         A1 15      lda ($15,x)
         B1 15      lda ($15),y

  B.31 LDX - load X register

       Flags: N Z

         A2 42      ldx #$42
         A6 15      ldx $15
         B6 15      ldx $15,y
         AE 11 47   ldx $4711
         BE 11 47   ldx $4711,y

  B.32 LDY - load Y register

       Flags: N Z

         A0 42      ldy #$42
         A4 15      ldy $15
         B4 15      ldy $15,x
         AC 11 47   ldy $4711
         BC 11 47   ldy $4711,x

  B.33 LSR - logical shift right

       Flags: N=0 Z C

         4A         lsr
         46 15      lsr $15
         56 15      lsr $15,x
         4E 11 47   lsr $4711
         5E 11 47   lsr $4711,x

  B.34 NOP - no-operation

         EA         nop

  B.35 ORA - bit-wise or with accumulator

       Flags: N Z

         09 42      ora #$42
         05 15      ora $15
         15 15      ora $15,x
         0D 11 47   ora $4711
         1D 11 47   ora $4711,x
         19 11 47   ora $4711,y
         01 15      ora ($15,x)
         11 15      ora ($15),y

  B.36 PHA - push accumulator on stack

         48         pha

  B.37 PHP - push flags on stack

         08         php

  B.38 PLA - pop accumulator from stack

       Flags: N Z

         68         pla

  B.39 PLP - pop flags from stack

       Flags: N Z C I D V

         28         plp

  B.40 ROL - rotate left through carry

       Flags: N Z C

         2A         rol
         26 15      rol $15
         36 15      rol $15,x
         2E 11 47   rol $4711
         3E 11 47   rol $4711,x

  B.41 ROR - rotate right through carry

       Flags: N Z C

         6A         ror
         66 15      ror $15
         76 15      ror $15,x
         6E 11 47   ror $4711
         7E 11 47   ror $4711,x

  B.42 RTI - return from interrupt

       Flags: N Z C I D V

         40         rti

  B.43 RTS - return from subroutine

         60         rts

  B.44 SBC - subtract from accumulator with carry

       Flags: N Z C V

         E9 42      sbc #$42
         E5 15      sbc $15
         F5 15      sbc $15,x
         ED 11 47   sbc $4711
         FD 11 47   sbc $4711,x
         F9 11 47   sbc $4711,y
         E1 15      sbc ($15,x)
         F1 15      sbc ($15),y

  B.45 SEC - set carry flag

       Flags: C=1

         38         sec

  B.46 SED - set decimal flag

       Flags: D=1

         F8         sed

  B.47 SEI - set interrupt disable flag

       Flags: I=1

         78         sei

  B.48 STA - store accumulator

         85 15      sta $15
         95 15      sta $15,x
         8D 11 47   sta $4711
         9D 11 47   sta $4711,x
         99 11 47   sta $4711,y
         81 15      sta ($15,x)
         91 15      sta ($15),y

  B.49 STX - store X register

         86 15      stx $15
         96 15      stx $15,y
         8E 11 47   stx $4711

  B.50 STY - store Y register

         84 15      sty $15
         94 15      sty $15,x
         8C 11 47   sty $4711

  B.51 TAX - transfer accumulator to X

         AA         tax

       Flags: N Z

  B.52 TAY - transfer accumulator to Y

         A8         tay

       Flags: N Z

  B.53 TSX - transfer stack pointer to X

       Flags: N Z

         BA         tsx

  B.54 TXA - transfer X to accumulator

       Flags: N Z

         8A         txa

  B.55 TXS - transfer X to stack pointer

       Flags: N Z

         9A         txs

  B.56 TYA - transfer Y to accumulator

       Flags: N Z

         98         tya

[Mo  8 Mai 10:30:47 2023]
