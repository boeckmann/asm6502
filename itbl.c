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

u16 am_size[13] = { 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 2, 2 };

const unsigned itbl_size = 56;

instruction_desc instruction_tbl[56] = {
   { "ADC", { INV,  INV, 0x69, INV, 0x65, 0x75, INV, 0x6d, 0x7d, 0x79, INV, 0x61, 0x71 }},
   { "AND", { INV,  INV, 0x29, INV, 0x25, 0x35, INV, 0x2d, 0x3d, 0x39, INV, 0x21, 0x31 }},
   { "ASL", { 0x0a, INV, INV,  INV, 0x06, 0x16, INV, 0x0e, 0x1e, INV,  INV, INV, INV }},
   { "BCC", { INV,  INV, INV, 0x90, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "BCS", { INV,  INV, INV, 0xb0, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "BEQ", { INV,  INV, INV, 0xf0, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "BIT", { INV,  INV, INV,  INV, 0x24, INV,  INV, 0x2c, INV,  INV,  INV, INV, INV }},
   { "BMI", { INV,  INV, INV, 0x30, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "BNE", { INV,  INV, INV, 0xd0, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "BPL", { INV,  INV, INV, 0x10, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "BRK", { INV, 0x00, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "BVC", { INV,  INV, INV, 0x50, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "BVS", { INV,  INV, INV, 0x70, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "CLC", { INV, 0x18, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "CLD", { INV, 0xd8, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "CLI", { INV, 0x58, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "CLV", { INV, 0xb8, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "CMP", { INV,  INV, 0xc9, INV, 0xc5, 0xd5, INV, 0xcd, 0xdd, 0xd9, INV, 0xc1, 0xd1 }},
   { "CPX", { INV,  INV, 0xe0, INV, 0xe4, INV,  INV, 0xec, INV,  INV,  INV, INV, INV }},
   { "CPY", { INV,  INV, 0xc0, INV, 0xc4, INV,  INV, 0xcc, INV,  INV,  INV, INV, INV }},
   { "DEC", { INV,  INV, INV,  INV, 0xc6, 0xd6, INV, 0xce, 0xde, INV,  INV, INV, INV }},
   { "DEX", { INV, 0xca, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "DEY", { INV, 0x88, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "EOR", { INV,  INV, 0x49, INV, 0x45, 0x55, INV, 0x4d, 0x5d, 0x59, INV, 0x41, 0x51 }},
   { "INC", { INV,  INV, INV,  INV, 0xe6, 0xf6, INV, 0xee, 0xfe, INV,  INV, INV, INV }},
   { "INX", { INV, 0xe8, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "INY", { INV, 0xc8, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "JMP", { INV,  INV, INV,  INV, INV,  INV,  INV, 0x4c, INV,  INV, 0x6c, INV, INV }},
   { "JSR", { INV,  INV, INV,  INV, INV,  INV,  INV, 0x20, INV,  INV,  INV, INV, INV }},
   { "LDA", { INV,  INV, 0xa9, INV, 0xa5, 0xb5, INV, 0xad, 0xbd, 0xb9, INV, 0xa1, 0xb1 }},
   { "LDX", { INV,  INV, 0xa2, INV, 0xa6, INV, 0xb6, 0xae, INV,  0xbe, INV, INV, INV }},
   { "LDY", { INV,  INV, 0xa0, INV, 0xa4, 0xb4, INV, 0xac, 0xbc, INV,  INV, INV, INV }},
   { "LSR", { 0x4a, INV, INV,  INV, 0x46, 0x56, INV, 0x4e, 0x5e, INV,  INV, INV, INV }},
   { "NOP", { INV, 0xea, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "ORA", { INV,  INV, 0x09, INV, 0x05, 0x15, INV, 0x0d, 0x1d, 0x19, INV, 0x01, 0x11 }},
   { "PHA", { INV, 0x48, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "PHP", { INV, 0x08, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "PLA", { INV, 0x68, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "PLP", { INV, 0x28, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "ROL", { 0x2a, INV, INV,  INV, 0x26, 0x36, INV, 0x2e, 0x3e, INV,  INV, INV, INV }},
   { "ROR", { 0x6a, INV, INV,  INV, 0x66, 0x76, INV, 0x6e, 0x7e, INV,  INV, INV, INV }},
   { "RTI", { INV, 0x40, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "RTS", { INV, 0x60, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "SBC", { INV,  INV, 0xe9, INV, 0xe5, 0xf5, INV, 0xed, 0xfd, 0xf9, INV, 0xe1, 0xf1 }},
   { "SEC", { INV, 0x38, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "SED", { INV, 0xf8, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "SEI", { INV, 0x78, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "STA", { INV,  INV, INV,  INV, 0x85, 0x95, INV, 0x8d, 0x9d, 0x99, INV, 0x81, 0x91 }},
   { "STX", { INV,  INV, INV,  INV, 0x86, INV, 0x96, 0x8e, INV,  INV,  INV, INV, INV }},
   { "STY", { INV,  INV, INV,  INV, 0x84, 0x94, INV, 0x8c, INV,  INV,  INV, INV, INV }},
   { "TAX", { INV, 0xaa, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "TAY", { INV, 0xa8, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "TSX", { INV, 0xba, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "TXA", { INV, 0x8a, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "TXS", { INV, 0x9a, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }},
   { "TYA", { INV, 0x98, INV,  INV, INV,  INV,  INV, INV,  INV,  INV,  INV, INV, INV }}
};
