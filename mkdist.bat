REM BUILD DOS, WIN32 and OS/2 executables and create a ZIP file
REM Expects Open Watcom 1.9 dev environment and InfoZIP in path

cd project\watcom\dos
ide2make -p asm6502.wpj
wmake -h -f asm6502.mk
cd ..\os2
ide2make -p asm6502.wpj
wmake -h -f asm6502.mk
cd ..\win32
ide2make -p asm6502.wpj
wmake -h -f asm6502.mk
cd ..\..\..
copy project\watcom\dos\asm6502.exe output\dos\asm6502.exe
copy project\watcom\os2\asm6502.exe output\os2\asm6502.exe
copy project\watcom\win32\asm6502.exe output\win32\asm6502.exe
copy README.md output\README.md
cd output
zip -r ..\asm6502.zip *.*
cd ..
