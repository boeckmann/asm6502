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
