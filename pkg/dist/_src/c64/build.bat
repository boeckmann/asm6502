@rem Build script for the C64 hello world example
@rem Procudes the program HELLOC64.PRG and a listing HELLOC64.LST

@echo.
@echo     Make sure to adjust the path to ASM6502.EXE or
@echo     the following command will fail!!
@echo.

asm6502.exe helloc64.a65 -o helloc64.prg -l helloc64.lst
