incsrc "snes_test.inc"

org $00ffc0

db "ZELDA1 DMC SAMPLES   " ; ROM name

; .segment "ROMINFO"   ; +$7FD5 in file
org $00ffd5

db $30            ; LoROM, fast-capable
db 0              ; no battery RAM
db $0d            ; 8192KB ROM
db 0,0,0,0
dw $AAAA,$5555    ; dummy checksum and complement

org $008000

incsrc "text/print-string.asm"
incsrc "sound/convert-dmc-to-brr.asm"
incsrc "sound/convert-dmc-to-brr-f1.asm"
incsrc "init.asm"

org $1f8000

dmc_start:
incsrc "rom.asm"