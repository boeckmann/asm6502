@echo off
echo (Open) Watcom build script for DOS, Win32 and OS/2

if not exist DOS\NUL md DOS
if not exist OS2\NUL md OS2
if not exist WIN32\NUL md WIN32

echo Building DOS executable...
wcl -bt=dos -q -0 -os -s -ml -fe=dos\asm6502.exe ..\src\asm6502.c

echo Building Win32 executable...
wcl386 -q -os -s -fe=win32\asm6502.exe ..\src\asm6502.c

echo Building OS/2 executable...
wcl386 -l=os2v2 -q -os -s -fe=os2\asm6502.exe ..\src\asm6502.c

if exist asm6502.obj del asm6502.obj