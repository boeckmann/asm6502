/* ASM6502 - a small but useful assembler for the MOS 6502 microprocessor

Copyright (c) 2022-2023 Bernd Boeckmann

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS”
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef ASM6502_H
#define ASM6502_H

typedef unsigned char u8;
typedef unsigned short u16;


enum {
   ERR_LVL_WARNING,
   ERR_LVL_FATAL
};

enum {
   DIAGNOSTIC_LVL_NONE,
   DIAGNOSTIC_LVL_WARN,
   DIAGNOSTIC_LVL_NOTICE
};

/* addressing modes */
enum {
   AM_ACC = 0,  /*         accumulator                       */
   AM_IMP = 1,  /*         implied                           */
   AM_IMM = 2,  /* #       immediate addressing              */
   AM_REL = 3,  /* R       program counter relative          */
   AM_ZP = 4,   /* ZP      zero-page                         */
   AM_ZPX = 5,  /* ZP,X    zero-page indexed with X          */
   AM_ZPY = 6,  /* ZP,Y    zero-page indexed with Y          */
   AM_ABS = 7,  /* A       absolute                          */
   AM_ABX = 8,  /* A,X     absolute indexed with X           */
   AM_ABY = 9,  /* A,Y     absolute indexed with Y           */
   AM_AIN = 10, /* (A)     absolute indirect                 */
   AM_ZIX = 11, /* (ZP,X)  zero-page indexed indirect        */
   AM_ZIY = 12, /* (ZP),Y  zero-page indirect indexed with Y */
   AM_ZIN = 13, /* (ZP)    zero-page indirect                */
   AM_AIX = 14, /* (ABS,X) absolute indexed indirect         */
   AM_ZPR = 15, /* ZP,R    zero-page, relative               */
   AM_INV = 16
};

enum {
   OP_RTS = 0x60,
   OP_JSR = 0x20,
   INV = 0xfc
};

typedef struct instruction_desc {
   char mn[5];
   u8 op[16];
} instruction_desc;


extern instruction_desc *instruction_tbl;
extern instruction_desc itbl_6502[56];
extern instruction_desc itbl_65c02[98];

extern int instruction_tbl_size;

extern u16 am_size[16];

#define AM_VALID( instr, am ) ((instr).op[am] != INV)
#define MAXINT( a, b ) (((b) >= (a)) ? (b) : (a))

void select_6502( void );

void select_65c02( void );

#endif
