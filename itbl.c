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

#include "asm6502.h"

u16 am_size[16] = { 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 2, 2, 2, 3, 3 };


instruction_desc *instruction_tbl;
int instruction_tbl_size;


void select_6502( void ) {
   instruction_tbl = itbl_6502;
   instruction_tbl_size = sizeof( itbl_6502 ) / sizeof( instruction_desc );
}


void select_65c02( void ) {
   instruction_tbl = itbl_65c02;
   instruction_tbl_size = sizeof( itbl_65c02 ) / sizeof( instruction_desc );
}


instruction_desc itbl_6502[56] = {
   { "ADC", { INV,  INV,  0x69, INV,  0x65, 0x75, INV,  0x6d, 0x7d, 0x79, INV,  0x61, 0x71, INV, INV, INV }},
   { "AND", { INV,  INV,  0x29, INV,  0x25, 0x35, INV,  0x2d, 0x3d, 0x39, INV,  0x21, 0x31, INV, INV, INV }},
   { "ASL", { 0x0a, INV,  INV,  INV,  0x06, 0x16, INV,  0x0e, 0x1e, INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "BCC", { INV,  INV,  INV,  0x90, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "BCS", { INV,  INV,  INV,  0xb0, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "BEQ", { INV,  INV,  INV,  0xf0, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "BIT", { INV,  INV,  INV,  INV,  0x24, INV,  INV,  0x2c, INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "BMI", { INV,  INV,  INV,  0x30, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "BNE", { INV,  INV,  INV,  0xd0, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "BPL", { INV,  INV,  INV,  0x10, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "BRK", { INV,  0x00, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "BVC", { INV,  INV,  INV,  0x50, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "BVS", { INV,  INV,  INV,  0x70, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "CLC", { INV,  0x18, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "CLD", { INV,  0xd8, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "CLI", { INV,  0x58, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "CLV", { INV,  0xb8, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "CMP", { INV,  INV,  0xc9, INV,  0xc5, 0xd5, INV,  0xcd, 0xdd, 0xd9, INV,  0xc1, 0xd1, INV, INV, INV }},
   { "CPX", { INV,  INV,  0xe0, INV,  0xe4, INV,  INV,  0xec, INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "CPY", { INV,  INV,  0xc0, INV,  0xc4, INV,  INV,  0xcc, INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "DEC", { INV,  INV,  INV,  INV,  0xc6, 0xd6, INV,  0xce, 0xde, INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "DEX", { INV,  0xca, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "DEY", { INV,  0x88, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "EOR", { INV,  INV,  0x49, INV,  0x45, 0x55, INV,  0x4d, 0x5d, 0x59, INV,  0x41, 0x51, INV, INV, INV }},
   { "INC", { INV,  INV,  INV,  INV,  0xe6, 0xf6, INV,  0xee, 0xfe, INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "INX", { INV,  0xe8, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "INY", { INV,  0xc8, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "JMP", { INV,  INV,  INV,  INV,  INV,  INV,  INV,  0x4c, INV,  INV,  0x6c, INV,  INV,  INV, INV, INV }},
   { "JSR", { INV,  INV,  INV,  INV,  INV,  INV,  INV,  0x20, INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "LDA", { INV,  INV,  0xa9, INV,  0xa5, 0xb5, INV,  0xad, 0xbd, 0xb9, INV,  0xa1, 0xb1, INV, INV, INV }},
   { "LDX", { INV,  INV,  0xa2, INV,  0xa6, INV,  0xb6, 0xae, INV,  0xbe, INV,  INV,  INV,  INV, INV, INV }},
   { "LDY", { INV,  INV,  0xa0, INV,  0xa4, 0xb4, INV,  0xac, 0xbc, INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "LSR", { 0x4a, INV,  INV,  INV,  0x46, 0x56, INV,  0x4e, 0x5e, INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "NOP", { INV,  0xea, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "ORA", { INV,  INV,  0x09, INV,  0x05, 0x15, INV,  0x0d, 0x1d, 0x19, INV,  0x01, 0x11, INV, INV, INV }},
   { "PHA", { INV,  0x48, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "PHP", { INV,  0x08, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "PLA", { INV,  0x68, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "PLP", { INV,  0x28, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "ROL", { 0x2a, INV,  INV,  INV,  0x26, 0x36, INV,  0x2e, 0x3e, INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "ROR", { 0x6a, INV,  INV,  INV,  0x66, 0x76, INV,  0x6e, 0x7e, INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "RTI", { INV,  0x40, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "RTS", { INV,  0x60, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "SBC", { INV,  INV,  0xe9, INV,  0xe5, 0xf5, INV,  0xed, 0xfd, 0xf9, INV,  0xe1, 0xf1, INV, INV, INV }},
   { "SEC", { INV,  0x38, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "SED", { INV,  0xf8, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "SEI", { INV,  0x78, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "STA", { INV,  INV,  INV,  INV,  0x85, 0x95, INV,  0x8d, 0x9d, 0x99, INV,  0x81, 0x91, INV, INV, INV }},
   { "STX", { INV,  INV,  INV,  INV,  0x86, INV,  0x96, 0x8e, INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "STY", { INV,  INV,  INV,  INV,  0x84, 0x94, INV,  0x8c, INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "TAX", { INV,  0xaa, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "TAY", { INV,  0xa8, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "TSX", { INV,  0xba, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "TXA", { INV,  0x8a, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "TXS", { INV,  0x9a, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }},
   { "TYA", { INV,  0x98, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV, INV, INV }}
};

instruction_desc itbl_65c02[98] = {
   { "ADC",  { INV,  INV,  0x69, INV,  0x65, 0x75, INV,  0x6d, 0x7d, 0x79, INV,  0x61, 0x71, 0x72, INV,  INV }},
   { "AND",  { INV,  INV,  0x29, INV,  0x25, 0x35, INV,  0x2d, 0x3d, 0x39, INV,  0x21, 0x31, 0x32, INV,  INV }},
   { "ASL",  { 0x0a, INV,  INV,  INV,  0x06, 0x16, INV,  0x0e, 0x1e, INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "BBR0", { INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  0x0f }},
   { "BBR1", { INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  0x1f }},
   { "BBR2", { INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  0x2f }},
   { "BBR3", { INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  0x3f }},
   { "BBR4", { INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  0x4f }},
   { "BBR5", { INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  0x5f }},
   { "BBR6", { INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  0x6f }},
   { "BBR7", { INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  0x7f }},
   { "BBS0", { INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  0x8f }},
   { "BBS1", { INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  0x9f }},
   { "BBS2", { INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  0xaf }},
   { "BBS3", { INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  0xbf }},
   { "BBS4", { INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  0xcf }},
   { "BBS5", { INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  0xdf }},
   { "BBS6", { INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  0xef }},
   { "BBS7", { INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  0xff }},
   { "BCC",  { INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "BCS",  { INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "BEQ",  { INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "BIT",  { INV,  INV,  0x89, INV,  0x24, 0x34, INV,  0x2c, 0x3c, INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "BMI",  { INV,  INV,  INV,  0x30, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "BNE",  { INV,  INV,  INV,  0xd0, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "BPL",  { INV,  INV,  INV,  0x10, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "BRA",  { INV,  INV,  INV,  0x80, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "BRK",  { INV,  0x00, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "BVC",  { INV,  INV,  INV,  0x50, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "BVS",  { INV,  INV,  INV,  0x70, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "CLC",  { INV,  0x18, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "CLD",  { INV,  0xd8, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "CLI",  { INV,  0x58, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "CLV",  { INV,  0xb8, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "CMP",  { INV,  INV,  0xc9, INV,  0xc5, 0xd5, INV,  0xcd, 0xdd, 0xd9, INV,  0xc1, 0xd1, 0xd2, INV,  INV }},
   { "CPX",  { INV,  INV,  0xe0, INV,  0xe4, INV,  INV,  0xec, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "CPY",  { INV,  INV,  0xc0, INV,  0xc4, INV,  INV,  0xcc, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "DEC",  { 0x3a, INV,  INV,  INV,  0xc6, 0xd6, INV,  0xce, 0xde, INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "DEX",  { INV,  0xca, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "DEY",  { INV,  0x88, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "EOR",  { INV,  INV,  0x49, INV,  0x45, 0x55, INV,  0x4d, 0x5d, 0x59, INV,  0x41, 0x51, 0x52, INV,  INV }},
   { "INC",  { 0xee, INV,  INV,  INV,  0xe6, 0xf6, INV,  0xee, 0xfe, INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "INX",  { INV,  0xe8, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "INY",  { INV,  0xc8, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "JMP",  { INV,  INV,  INV,  INV,  INV,  INV,  INV,  0x4c, INV,  INV,  0x6c, INV,  INV,  INV,  0x7c, INV }},
   { "JSR",  { INV,  INV,  INV,  INV,  INV,  INV,  INV,  0x20, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "LDA",  { INV,  INV,  0xa9, INV,  0xa5, 0xb5, INV,  0xad, 0xbd, 0xb9, INV,  0xa1, 0xb1, 0xb2, INV,  INV }},
   { "LDX",  { INV,  INV,  0xa2, INV,  0xa6, INV,  0xb6, 0xae, INV,  0xbe, INV,  INV,  INV,  INV,  INV,  INV }},
   { "LDY",  { INV,  INV,  0xa0, INV,  0xa4, 0xb4, INV,  0xac, 0xbc, INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "LSR",  { 0x4a, INV,  INV,  INV,  0x46, 0x56, INV,  0x4e, 0x5e, INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "NOP",  { INV,  0xea, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "ORA",  { INV,  INV,  0x09, INV,  0x05, 0x15, INV,  0x0d, 0x1d, 0x19, INV,  0x01, 0x11, 0x12, INV,  INV }},
   { "PHA",  { INV,  0x48, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "PHP",  { INV,  0x08, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "PHX",  { INV,  0xda, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "PHY",  { INV,  0x5a, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "PLA",  { INV,  0x68, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "PLP",  { INV,  0x28, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "PLX",  { INV,  0xfa, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "PLY",  { INV,  0x7a, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "RMB0", { INV,  INV,  INV,  INV,  0x07,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "RMB1", { INV,  INV,  INV,  INV,  0x17,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "RMB2", { INV,  INV,  INV,  INV,  0x27,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "RMB3", { INV,  INV,  INV,  INV,  0x37,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "RMB4", { INV,  INV,  INV,  INV,  0x47,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "RMB5", { INV,  INV,  INV,  INV,  0x57,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "RMB6", { INV,  INV,  INV,  INV,  0x67,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "RMB7", { INV,  INV,  INV,  INV,  0x77,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "ROL",  { 0x2a, INV,  INV,  INV,  0x26, 0x36, INV,  0x2e, 0x3e, INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "ROR",  { 0x6a, INV,  INV,  INV,  0x66, 0x76, INV,  0x6e, 0x7e, INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "RTI",  { INV,  0x40, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "RTS",  { INV,  0x60, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "SBC",  { INV,  INV,  0xe9, INV,  0xe5, 0xf5, INV,  0xed, 0xfd, 0xf9, INV,  0xe1, 0xf1, 0xf2, INV,  INV }},
   { "SEC",  { INV,  0x38, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "SED",  { INV,  0xf8, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "SEI",  { INV,  0x78, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "SMB0", { INV,  INV,  INV,  INV,  0x87,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "SMB1", { INV,  INV,  INV,  INV,  0x97,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "SMB2", { INV,  INV,  INV,  INV,  0xa7,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "SMB3", { INV,  INV,  INV,  INV,  0xb7,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "SMB4", { INV,  INV,  INV,  INV,  0xc7,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "SMB5", { INV,  INV,  INV,  INV,  0xd7,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "SMB6", { INV,  INV,  INV,  INV,  0xe7,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "SMB7", { INV,  INV,  INV,  INV,  0xf7,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "STA",  { INV,  INV,  INV,  INV,  0x85, 0x95, INV,  0x8d, 0x9d, 0x99, INV,  0x81, 0x91, 0x92, INV,  INV }},
   { "STP",  { INV,  0xdb, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "STX",  { INV,  INV,  INV,  INV,  0x86, INV,  0x96, 0x8e, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "STY",  { INV,  INV,  INV,  INV,  0x84, 0x94, INV,  0x8c, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "STZ",  { INV,  INV,  INV,  INV,  0x64, 0x74, INV,  0x9c, 0x9e, INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "TAX",  { INV,  0xaa, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "TAY",  { INV,  0xa8, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "TRB",  { INV,  INV,  INV,  INV,  0x14, INV,  INV,  0x1c, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "TSB",  { INV,  INV,  INV,  INV,  0x04, INV,  INV,  0x0c, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "TSX",  { INV,  0xba, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "TXA",  { INV,  0x8a, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "TXS",  { INV,  0x9a, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "TYA",  { INV,  0x98, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }},
   { "WAI",  { INV,  0xcb, INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV,  INV }}
};
