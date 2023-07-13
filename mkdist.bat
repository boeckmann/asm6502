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
copy project\watcom\dos\asm6502.exe bin\dos\asm6502.exe
copy project\watcom\os2\asm6502.exe bin\os2\asm6502.exe
copy project\watcom\win32\asm6502.exe bin\win32\asm6502.exe
copy readme.txt doc\asm6502.txt
copy LICENSE doc\license.txt
del asm6502.zip
zip -r asm6502.zip bin doc\asm6502.txt doc\asm6502.pdf doc\instrtbl.pdf support
