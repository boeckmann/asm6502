@rem Build script for the C64 hello world example
@rem Procudes the program HELLOC64.PRG and a listing HELLOC64.LST

..\..\asm6502.exe helloc64.a65 -o helloc64.prg -l helloc64.lst
@echo.
@echo You may run the generated HELLOC64.PRG with the VICE emulator.
@echo VICE is included in the SvarDOS repository. See the SvarDOS
@echo documentation on how to install it. VICE needs a DPMI host.
@echo The SvarDOS CWSDPMI package contains one.
@echo.
@echo To run the example, type:
@echo.
@echo    C:\VICE\X64.EXE HELLOC64.PRG
@echo.
@echo Adjust the path to X64, if you installed VICE to another location.
