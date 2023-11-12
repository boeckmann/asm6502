del asm6502.svp
deltree /Y appinfo
deltree /Y devel

md appinfo
md devel
md devel\asm6502
md devel\asm6502\examples
md devel\asm6502\examples\atarivcs
md devel\asm6502\examples\c64
md devel\asm6502\support
md devel\asm6502\support\atarivcs
md devel\asm6502\support\c64

copy ..\..\bin\dos\asm6502.exe devel\asm6502\asm6502.exe
copy ..\..\LICENSE devel\asm6502\license.txt
copy src\readme.1st devel\asm6502\readme.1st
copy ..\..\appinfo\asm6502.lsm appinfo\asm6502.lsm
copy ..\..\doc\asm6502.txt devel\asm6502\manual.txt

copy src\c64\build.bat devel\asm6502\examples\c64
copy ..\..\examples\c64\helloc64.a65 devel\asm6502\examples\c64
copy src\atarivcs\build.bat devel\asm6502\examples\atarivcs
copy ..\..\examples\atarivcs\hellovcs.a65 devel\asm6502\examples\atarivcs

copy ..\..\support\atarivcs\*.* devel\asm6502\support\atarivcs
copy ..\..\support\c64\*.* devel\asm6502\support\c64

zip -9rkDX asm6502.svp appinfo devel
