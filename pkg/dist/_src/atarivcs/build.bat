@rem Build script for the C64 hello world example
@rem Procudes the program HELLOVCS.BIN and a listing HELLOVCS.LST

..\..\asm6502.exe hellovcs.a65 -o hellovcs.bin -l hellovcs.lst
