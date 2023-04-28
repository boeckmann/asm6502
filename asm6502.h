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

/* addressing modes */
enum {
   AM_ACC = 0,
   AM_IMP = 1,
   AM_IMM = 2,
   AM_REL = 3,
   AM_ZP  = 4,
   AM_ZPX = 5,
   AM_ZPY = 6,
   AM_ABS = 7,
   AM_ABX = 8,
   AM_ABY = 9,
   AM_IND = 10,
   AM_INX = 11,
   AM_INY = 12,
   AM_INV = 13
};

enum {
   INV = 0xff
};

typedef struct instruction_desc {
   char mn[4];
   u8 op[13];
} instruction_desc;

extern instruction_desc instruction_tbl[56];
extern const unsigned itbl_size;

extern u16 am_size[13];

#define AM_VALID( instr, am ) ((instr).op[am] != INV)

#define MAXINT( a, b ) (((b) >= (a)) ? (b) : (a))

#endif
