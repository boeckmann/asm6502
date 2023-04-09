; C64 .PRG file header and BASIC SYS command
.nolist
LOAD_ADDR = $0801

        .word LOAD_ADDR         ; .PRG header: load address
        .org  LOAD_ADDR

.list
basic_upstart:                  ; BASIC code: 10 SYS 2062
        .word @end, 10          ; ptr to next basic line and line number 10
        .byte $9E, " 2062", 0   ; SYS token and address string of subroutine
@end    .word 0                 ; null ptr to indicate end of basic text

start:                          ; this is at address 2062 ($080E)
