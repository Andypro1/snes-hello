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

; .segment "CODE"
org $008000

jmp start

!VRAM_CHARSET   = $0000 ; must be at $1000 boundary
!VRAM_BG1       = $1000 ; must be at $0400 boundary
; !VRAM_BG2       = $1400 ; must be at $0400 boundary
; !VRAM_BG3       = $1800 ; must be at $0400 boundary
; !VRAM_BG4       = $1C00 ; must be at $0400 boundary
!START_X        #= 0
!START_Y        #= 1

!tm_addr_offset  #= 32*!START_Y+!START_X
!START_TM_ADDR  = !VRAM_BG1+!tm_addr_offset

hello_str: 
db "01234567890123456789012345678901", $00

start:
   clc             ; native mode
   xce
   rep #$10        ; X/Y 16-bit
   sep #$20        ; A 8-bit

   ; Clear registers
   ldx.w #$33

   jsr ClearVRAM

loop:
   stz !INIDISP,x
   stz !NMITIMEN,x
   dex
   bpl loop

   lda #%10000000   ; [7]: forced blanking
   sta !INIDISP ; undo the accidental stz to 2100h due to BPL actually being a branch on nonnegative

   ; Set palette to black background and 3 shades of red
    stz !CGADD ; start with color 0 (background)
    stz !CGDATA ; None more black
    stz !CGDATA

    stz !CGDATA
    stz !CGDATA

   ; test
   ;  rep #$20

   ;  lda #%1110011100011100
   ;  sta !CGDATA
   ;  sta !CGDATA

   ; sep #$20        ; A 8-bit
   ; /test



    lda #%11100111  ;  test
    sta !CGDATA
    lda #%00011100
    sta !CGDATA

   ; Setup Graphics Mode 0, 8x8 tiles all layers
   stz !BGMODE
   lda.b #!VRAM_BG1>>8
   sta !BG1SC ; BG1 at VRAM_BG1, only single 32x32 map (4-way mirror)
   
   vramval_calc:
   !vramval #= ((!VRAM_CHARSET>>12)|(!VRAM_CHARSET>>8&$F0))
   
;    lda #((!VRAM_CHARSET >> 4) | (!VRAM_CHARSET & $F0))
    lda !vramval
   sta !BG12NBA ; BG 1 and 2 both use char tiles

   ; Load character set into VRAM
   lda #$80
   sta !VMAIN   ; VRAM stride of 1 word
   ldx.w #!VRAM_CHARSET
   stx !VMADDL
   ldx.w #0

charset_loop:
   lda NESfont,x  ; $80e9 right?  cc65 has $80e8.
   stz !VMDATAL ; color index low bit = 0
   sta !VMDATAH ; color index high bit set -> neutral red (2)
   inx
   cpx.w #(128*8)
   bne charset_loop

   ; Place string tiles in background
   ldx #!START_TM_ADDR  ; #$11c9
   stx !VMADDL
   ldx.w #0



;    ; Place string tiles in background
; ;    ldx #!START_TM_ADDR  ; #$11c9
;     ldx !VRAM_BG1+32*2+!START_X
;    stx !VMADDL
;    ldx.w #0

string_loop:
   lda hello_str,x  ; $8003, looks right?
   beq enable_display
   sta !VMDATAL
   lda #$20 ; priority 1
   sta !VMDATAH
   inx
   bra string_loop

enable_display:
   ; Show BG1
   lda #$01
   sta !TM  ; [0]: enable BG1
   ; Maximum screen brightness
   lda #$0F
   sta !INIDISP

   ; enable NMI for Vertical Blank
   lda #$80
   sta !NMITIMEN

game_loop:
   wai ; Pause until next interrupt complete (i.e. V-blank processing is done)
   ; Do something
   jmp game_loop


nmi:
   rep #$10        ; X/Y 16-bit
   sep #$20        ; A 8-bit
   phd
   pha
   phx
   phy
   ; Do stuff that needs to be done during V-Blank
   lda !RDNMI ; reset NMI flag
   ply
   plx
   pla
   pld
return_int:
   rti

;----------------------------------------------------------------------------
; ClearVRAM -- Sets every byte of VRAM to zero
; from bazz's VRAM tutorial
; In: None
; Out: None
; Modifies: flags
;----------------------------------------------------------------------------
ClearVRAM:
   pha
   phx
   php

   REP #$30		; mem/A = 8 bit, X/Y = 16 bit
   SEP #$20

   LDA #$80
   STA $2115         ;Set VRAM port to word access
   LDX #$1809
   STX $4300         ;Set DMA mode to fixed source, WORD to $2118/9
   LDX #$0000
   STX $2116         ;Set VRAM port address to $0000
   STX $0000         ;Set $00:0000 to $0000 (assumes scratchpad ram)
   STX $4302         ;Set source address to $xx:0000
   LDA #$00
   STA $4304         ;Set source bank to $00
   LDX #$FFFF
   STX $4305         ;Set transfer size to 64k-1 bytes
   LDA #$01
   STA $420B         ;Initiate transfer

   STZ $2119         ;clear the last byte of the VRAM

   plp
   plx
   pla
   RTS

charset_asm_here:
incsrc "charset_test.asm"

; .segment "VECTORS"
org $00FFE0

dw 0, 0        ;Native mode vectors
dw return_int  ;COP
dw return_int  ;BRK
dw return_int  ;ABORT
dw nmi         ;NMI
dw start       ;RST
dw return_int  ;IRQ

dw 0, 0        ;Emulation mode vectors
dw return_int  ;COP
dw 0
dw return_int  ;ABORT
dw nmi         ;NMI
dw start       ;RST
dw return_int  ;IRQ