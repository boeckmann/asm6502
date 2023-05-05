Copy one of these folders to your ASM6502 project folder, depending for
which platform you develop. You may then include these files relative
to your assembler source, like:

.nosym
.include "c64/prg.i65"

The .nosym prevents the many defined variables and labels to show up in
the your symbol map.
