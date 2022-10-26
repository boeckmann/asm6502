/* ASM6502
 *
 * Copyright (c) 2022 Bernd Böckmann
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#ifndef ASM6502_H
#define ASM6502_H

#define debugf printf

typedef short i16;
typedef long i32;

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned long u32;

#define AM_ACC 0
#define AM_IMP 1
#define AM_IMM 2
#define AM_REL 3
#define AM_ZP  4
#define AM_ZPX 5
#define AM_ZPY 6
#define AM_ABS 7
#define AM_ABX 8
#define AM_ABY 9
#define AM_IND 10
#define AM_INX 11
#define AM_INY 12
#define AM_INV 13

#define INV	0xff

typedef struct idesc {
	char mn[4];
	u8 op[13];
} idesc;

extern idesc itbl[56];
extern const int itbl_size;

extern u16 am_size[13];

#define AM_VALID(idesc, am) ((idesc).op[am] != INV)

#define MAXINT(a,b) (((b) >= (a)) ? (b) : (a))

#endif
