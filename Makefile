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
	${DEL} asm6502 *.bak *.exe *.gch *.il? *.lk1 *.map *.mk *.mk1 *.o *.obj *.ppx *.sym *.tag *.tds 2>/dev/null || true
