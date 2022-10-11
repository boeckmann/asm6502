; relative jump tests

start:
.org 126
	bne start
lul:	bne lul
	beq end
.org 259
end:
