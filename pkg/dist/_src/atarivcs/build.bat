@rem Build script for the Atari VCS hello world example
@rem Procudes the program HELLOVCS.BIN and a listing HELLOVCS.LST

@echo.
@echo     Make sure to adjust the path to ASM6502.EXE or
@echo     the following command will fail!!
@echo.

asm6502.exe hellovcs.a65 -o hellovcs.bin -l hellovcs.lst
