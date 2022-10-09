OBJ=asm6502.o itbl.o
LIBS=

CC=cc
CFLAGS=--std=c89 -Wall
DEL=rm

.PHONY: clean
	
asm6502: $(OBJ)
	$(CC) -o $@ $(OBJ) $(LIBS)

%.o: %.c
	$(CC) -c $(CFLAGS) $<

clean:
	${DEL} asm6502 *.bak *.exe *.gch *.il? *.o *.obj *.tds 2>/dev/null || true
