@rem Build script for the Atari VCS hello world example
@rem Procudes the program HELLOVCS.BIN and a listing HELLOVCS.LST

..\..\asm6502.exe hellovcs.a65 -o hellovcs.bin -l hellovcs.lst
@echo.
@echo You may run the generated HELLOVCS.BIN with the STELLA emulator.
@echo STELLA is included in the SvarDOS repository. See the SvarDOS
@echo documentation on how to install it.
@echo.
@echo To run the example, type:
@echo.
@echo    C:\STELLA\STELLA.EXE HELLOVCS.BIN
@echo.
@echo Adjust the path to STELLA, if you installed it to another location.
