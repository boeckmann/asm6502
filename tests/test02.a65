; Some label and variable reference tests

.org 0                  ; redundant but given for documentation purposes

VAR_A = 42              ; define variable with value 42

lbl_b .byte 42          ; define label with address 0 to byte containing 42

      jmp   prolog      ; should emit 3-byte instruction bacause prolog is
prolog:                 ; is forward referenced
      .fill $200 - @    ; fill rest of zero page and stack with zero

start:
      lda   VAR_A       ; should emit 2-byte instruction
      lda   lbl_b       ; should also emit 2-byte instruction

hello:
  @len      .byte @end - @msg - 1         ; here @end is a forward reference
  @msg      .byte "Hello, World"          ; to a local label to calculate the
            @end                          ; string length and store it as first
                                          ; byte

D6510 .org @+1           ;6510 DATA DIRECTION REGISTER
R6510 .org @+1           ;6510 DATA REGISTER
