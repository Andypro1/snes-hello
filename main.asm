incsrc "snes_test.inc"

org $00ffc0

db "ASAR EXAMPLE         " ; ROM name

; .segment "ROMINFO"   ; +$7FD5 in file
org $00ffd5

db $30            ; LoROM, fast-capable
db 0              ; no battery RAM
db $07            ; 128K ROM
db 0,0,0,0
dw $AAAA,$5555    ; dummy checksum and complement

org $008000

incsrc "text/print-string.asm"
incsrc "init.asm"
