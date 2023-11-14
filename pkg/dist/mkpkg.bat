del asm6502.zip

deltree /Y bin
deltree /Y doc
deltree /Y examples
deltree /Y support
deltree /Y source

md bin
md bin\dos
md bin\win32
md bin\os2
md doc
md examples
md examples\atarivcs
md examples\c64
md support
md support\atarivcs
md support\c64
md source

copy ..\..\bin\dos\asm6502.exe bin\dos\asm6502.exe
copy ..\..\bin\win32\asm6502.exe bin\win32\asm6502.exe
copy ..\..\bin\os2\asm6502.exe bin\os2\asm6502.exe

copy ..\..\LICENSE doc\license.txt
copy ..\..\doc\asm6502.txt doc\manual.txt
copy ..\..\doc\asm6502.pdf doc\manual.pdf
copy ..\..\doc\instrtbl.pdf doc

copy _src\c64\build.bat examples\c64
copy ..\..\examples\c64\*.a65 examples\c64
copy _src\atarivcs\build.bat examples\atarivcs
copy ..\..\examples\atarivcs\*.a65 examples\atarivcs

copy ..\..\support\atarivcs\*.i65 support\atarivcs
copy ..\..\support\c64\*.i65 support\c64

copy ..\..\src\asm6502.c source\asm6502.c

zip -9rkDX asm6502.zip bin doc examples support source
