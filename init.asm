!VRAM_CHARSET   = $0000 ; must be at $1000 boundary
!VRAM_BG1       = $1000 ; must be at $0400 boundary
!VRAM_BG2       = $1400 ; must be at $0400 boundary
!VRAM_BG3       = $1800 ; must be at $0400 boundary
!VRAM_BG4       = $1C00 ; must be at $0400 boundary
!START_X        #= 4
!START_Y        #= 1

!tm_addr_offset  #= 32*!START_Y+!START_X
!START_BG1_ADDR  = !VRAM_BG1+!tm_addr_offset
!START_BG2_ADDR = !VRAM_BG2+!tm_addr_offset
!START_BG3_ADDR = !VRAM_BG3+!tm_addr_offset
!START_BG4_ADDR = !VRAM_BG4+!tm_addr_offset
!START_TM2      #= 32+!START_BG1_ADDR

; constants to use as masks
!UP_BUTTON       = $0800
!DOWN_BUTTON     = $0400
!LEFT_BUTTON     = $0200
!RIGHT_BUTTON    = $0100



start:
   ;----- Memory Map WRAM
   ;TODO: make a memory map wram init to 0 loop
   !keylast = $0302           
   STZ $0302            ; data read from joypad 1
   STZ $0303            ; data read from joypad 1
   !keynew = $0304
   STZ $0304     ; trigger read from joypad 1
   STZ $0305            ; data read from joypad 1
   !keydown = $0306
   STZ $0306     ; held buttons read from joypad 1
   STZ $0307            ; data read from joypad 1
   !menuitem = $0308    ; Byte which holds the currently selected menu item (0 to 4)
   stz $0308

   clc             ; native mode
   xce
   rep #$10        ; X/Y 16-bit
   sep #$20        ; A 8-bit

   ; Clear registers
   ldx.w #$33

   jsr ClearVRAM

; setStackPtr:
   ; set the stack pointer to $1fff
   ldx #$1fff              ; load X with $1fff
   txs                     ; copy X to stack pointer

init_loop:
   stz !INIDISP,x
   stz !NMITIMEN,x
   dex
   bpl init_loop

   lda #%10000000   ; [7]: forced blanking
   sta !INIDISP ; undo the accidental stz to 2100h due to BPL actually being a branch on nonnegative

   incsrc "text/pallettes.asm"   ;  load color pallettes

graphics_setup:
   ; Setup Graphics Mode 0, 8x8 tiles all layers
   stz !BGMODE
   lda.b #((!VRAM_BG1>>8)&$fc)
   sta !BG1SC ; BG0 at VRAM_BG1, only single 32x32 map (4-way mirror)

   lda.b #((!VRAM_BG2>>8)&$fc)
   sta !BG2SC ; BG1 at VRAM_BG2, only single 32x32 map (4-way mirror)

   lda.b #((!VRAM_BG3>>8)&$fc)
   sta !BG3SC ; BG2 at VRAM_BG3, only single 32x32 map (4-way mirror)

   lda.b #((!VRAM_BG4>>8)&$fc)
   sta !BG4SC ; BG3 at VRAM_BG4, only single 32x32 map (4-way mirror)

vramval_calc:
   !vramval #= ((!VRAM_CHARSET>>12)|(!VRAM_CHARSET>>8&$F0))
   lda !vramval
   sta !BG12NBA ; BG 1 and 2 both use char tiles
   sta !BG34NBA ; BG 3 and 4 both use char tiles

   ; Load character set into VRAM
   lda #$80
   sta !VMAIN   ; 16 bit write to VMDATAL & VMDATAH	addr increment:1   used for:2/4/8 bpp tile data, tilemap data and offset-per-tile data
   ldx.w #!VRAM_CHARSET
   stx !VMADDL   ;  (Presumably) writing here to both VMADDL and VMADDH because we're writing a word
   ldx.w #0000

charset_loop:
   lda NESfont,x  ;  charset data start
   stz !VMDATAL ; color index low bit = 0
   sta !VMDATAH ; color index high bit set
   inx
   cpx.w #(128*8)
   bne charset_loop

print_menu:
   ; print-string subroutine call with params
   ldx #!START_BG3_ADDR  ; Initial vram write location
   jsr WriteString             ; jump to subroutine
   db "Zelda 1 DPCM Samples", $00

   ldx #!START_BG1_ADDR+96  ; Initial vram write location
   lda !menuitem
   cmp #$00
   bne menuitem1
   jsr WriteString   ; Clear this section of vram
   db "                 ", $00
   ldx #!START_BG2_ADDR+96  ; Point to the highlighted layer
menuitem1:
   jsr WriteString             ; and write
   db "$c000: sword beam", $00

   ldx #!START_BG1_ADDR+192  ; Initial vram write location
   lda !menuitem
   cmp #$01
   bne menuitem2
   jsr WriteString   ; Clear this section of vram
   db "                ", $00
   ldx #!START_BG2_ADDR+192  ; Point to the highlighted layer
menuitem2:
   jsr WriteString             ; and write
   db "$c740: link hurt", $00

   ldx #!START_BG1_ADDR+288  ; Initial vram write location
   lda !menuitem
   cmp #$02
   bne menuitem3
   jsr WriteString   ; Clear this section of vram
   db "                        ", $00
   ldx #!START_BG2_ADDR+288  ; Point to the highlighted layer
menuitem3:
   jsr WriteString             ; and write
   db "$c800: ganon/gleeok roar", $00

   ldx #!START_BG1_ADDR+384  ; Initial vram write location
   lda !menuitem
   cmp #$03
   bne menuitem4
   jsr WriteString   ; Clear this section of vram
   db "                     ", $00
   ldx #!START_BG2_ADDR+384  ; Point to the highlighted layer
menuitem4:
   jsr WriteString             ; and write
   db "$d300: manhandla roar", $00

   ldx #!START_BG1_ADDR+480  ; Initial vram write location
   lda !menuitem
   cmp #$04
   bne menuitem5
   jsr WriteString   ; Clear this section of vram
   db "                     ", $00
   ldx #!START_BG2_ADDR+480  ; Point to the highlighted layer
menuitem5:
   jsr WriteString             ; and write
   db "$e000: keydoor unlock", $00

enable_display:
   ; Show BG1 -> BG4
   lda #$0f
   sta !TM  ; [0]: enable BG1->4
   ; Maximum screen brightness
   lda #$0F
   sta !INIDISP

   lda #%10000001	; Enable NMI and Auto Joypad read
	sta !NMITIMEN   	; Interrupt Enable Flags

game_loop:
   wai ; Pause until next interrupt complete (i.e. V-blank processing is done)

read_input:
   ; ensure auto-read has finished
   lda !HVBJOY
   and #$01
   bne read_input    ; Wait until hvbjoy returns [0] == 0 to safely begin read
   
   ;  Update controller values
   ; 16-bit accumulator
   rep #$20        ; A 16-bit
   lda !keydown
   sta !keylast
   lda !JOY1L
   sta !keydown ; buttons currently pressed
   eor !keylast
   and !keydown
   sta !keynew  ; buttons newly pressed this frame (0->1)

;  Input handler ------------------------------------------------
CheckUpButton:
   lda #$0000                          ; set A to zero
   ora !keynew                     ; check whether the up button was pressed this frame...
   and #(!UP_BUTTON+!LEFT_BUTTON)
   beq CheckUpButtonDone               ; if neither has occured, move on

   sep #$20                            ; set A to 8-bit

MenuUp:
   sep #$20        ; A 8-bit
   lda !menuitem
   dec
   bpl DoneMenuUp
   ;  menuitem is below 0; reset it to the end of the list (4)
   lda #$04

DoneMenuUp:
   sta !menuitem  ; [0]: disable all BG layers
   jmp print_menu  ; Redraw menu

CheckUpButtonDone:
   rep #$20        ; A 16-bit
CheckDownButton:
   lda #$0000                          ; set A to zero
   ora !keynew                     ; check whether the up button was pressed this frame...
   and #(!DOWN_BUTTON+!RIGHT_BUTTON)
   beq CheckDownButtonDone               ; if neither has occured, move on

   sep #$20                            ; set A to 8-bit

MenuDown:
   sep #$20        ; A 8-bit
   lda !menuitem
   inc
   cmp #$05
   bcc DoneMenuDown
   ;  menuitem is greater than 4; reset it to the start of the list (0)
   lda #$00

DoneMenuDown:
   sta !menuitem  ; [0]: disable all BG layers
   jmp print_menu  ; Redraw menu

CheckDownButtonDone:
   sep #$20                            ; set A to 8-bit
;  end Input handler --------------------------------------------


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