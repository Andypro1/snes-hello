jmp start

incsrc "sound/macros.asm"
incsrc "sound/dsp-send.asm"
; incsrc "sound/sound-play-wait-loop.asm"

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

; constants to use as masks (hi:bystudlr lo:axlriiii)
!UP_BUTTON       = $0800
!DOWN_BUTTON     = $0400
!LEFT_BUTTON     = $0200
!RIGHT_BUTTON    = $0100
!A_BUTTON        = $0080
!B_BUTTON        = $8000
!X_BUTTON        = $0040
!Y_BUTTON        = $4000

start:
zeroPageAllocation:
   !dmc_change_after_16 = $00
   stz !dmc_change_after_16
   !brr_half_sample = $01
   stz !brr_half_sample
   stz !brr_half_sample+1
   !dmc_working_b1b2 = $03
   stz !dmc_working_b1b2
   stz !dmc_working_b1b2+1
   !brr_loop_counter = $05
   stz !brr_loop_counter
   stz !brr_loop_counter+1


memoryAllocation:
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
   !menuitem = $0308    ; Byte which holds the currently selected menu item (0 to 4)
   stz $0308

   ;  Variables used in convert-dcm-to-brr.asm
   ;  Please move this initialization somewhere sensible
   !brr_first_last = $030a    ;  TODO: use better boolean name.  0 or 1
                              ;  depending on whether we're working on an even sample
                              ;  nibble (0..14) or an odd one (1..15)
   stz $030a
   !dmc_sample_length = $030b ; 16-bit length value
   stz $030b
   stz $030c
   !brr_new_sample_pointer = $030d  ; 16-bit spc-700 address value
   stz $030d
   stz $030e
   !brr_cur_shift = $030f
   stz $030f
   stz $0310
   !dmc_working_value = $0311
   stz $0311
   !brr_high_nibble = $0312
   stz $0312
   !brr_scaled_value = $0313
   stz $0313
   !brr_isBackBlock = $0314
   stz $0314
   !sbs_minimum = $0315
   stz $0315
   stz $0316
   !sbs_maximum = $0317
   stz $0317
   stz $0318
   !cdb_SV = $0319
   stz $0319
   !cdb_VminusSV = $0320
   stz $0320
   !cdb_Sminus8 = $0321
   stz $0321
   !brr_start_location = $0322
   stz $0322
   stz $0323
   !playback_pitch_high = $0324
   !playback_pitch_low = $0325
   stz $0324
   stz $0325
   !dmc_running_value = $0326
   stz $0326
   stz $0327
   !dmc_working_value_16 = $0328
   stz $0328
   stz $0329

   ; !brr_cur_upload_index = $0311
   ; stz $0311
   ; stz $0312
   ; !brr_cur_scaled_value = $0313
   ; stz $0313
   ; stz $0314

snesboot:
   clc             ; native mode
   xce
   rep #$10        ; X/Y 16-bit
   sep #$20        ; A 8-bit

jumpTablesInit:
   ; %copyEarlyRomToRam(shiftJumpTable, $0000, $0020)  ;  Load shiftJumpTable to 0-page

   ; Clear registers
   ldx.w #$33

   jsr ClearVRAM

setStackPtr:
   ; set the stack pointer to $1fff
   ldx #$1fff              ; load X with $1fff
   txs                     ; copy X to stack pointer

   %copyRomToRam($8000, $0400, $0750)  ;  Load $c000 sword beam dmc

init_sound:
reset:
    jsr spc_wait_boot

;  Init aram base start location
ldy #$2000
sty !brr_new_sample_pointer

init_swordshot:
   ldy #$0200    ; directory entry for this sample
   ; jsr spc_begin_upload
   %spc_begin_upload()
   lda !brr_new_sample_pointer
   ; jsr spc_upload_byte
   %spc_upload_byte()
   lda !brr_new_sample_pointer+1
   ; jsr spc_upload_byte
   %spc_upload_byte()
   lda !brr_new_sample_pointer
   ; jsr spc_upload_byte
   %spc_upload_byte()
   lda !brr_new_sample_pointer+1
   ; jsr spc_upload_byte
   %spc_upload_byte()

   ldy !brr_new_sample_pointer : phy  ; audio ram write location
   ldx #$0400    ; sword shot sample location
   ldy #$0750      ; sword shot sample length
   ; jsr ConvertDMCtoBRR
   jsr ConvertDMCtoBRR_F1

%copyRomToRam($8740, $0400, $00a0)  ;  Load $c740 link hurt dmc

init_linkhurt:
   ldy #$0204    ; directory entry for this sample
   ; jsr spc_begin_upload
   %spc_begin_upload()
   lda !brr_new_sample_pointer
   ; jsr spc_upload_byte
   %spc_upload_byte()
   lda !brr_new_sample_pointer+1
   ; jsr spc_upload_byte
   %spc_upload_byte()
   lda !brr_new_sample_pointer
   ; jsr spc_upload_byte
   %spc_upload_byte()
   lda !brr_new_sample_pointer+1
   ; jsr spc_upload_byte
   %spc_upload_byte()
   
   ldy !brr_new_sample_pointer : phy  ; audio ram write location
   ldx #$0400    ; sample location
   ldy #$00a0      ; sample length
   ; jsr ConvertDMCtoBRR
   jsr ConvertDMCtoBRR_F1

%copyRomToRam($8800, $0400, $0b00)  ;  Load $c800 boss 1 dmc

init_boss1:
   ldy #$0208    ; directory entry for this sample
   ; jsr spc_begin_upload
   %spc_begin_upload()
   lda !brr_new_sample_pointer
   ; jsr spc_upload_byte
   %spc_upload_byte()
   lda !brr_new_sample_pointer+1
   ; jsr spc_upload_byte
   %spc_upload_byte()
   lda !brr_new_sample_pointer
   ; jsr spc_upload_byte
   %spc_upload_byte()
   lda !brr_new_sample_pointer+1
   ; jsr spc_upload_byte
   %spc_upload_byte()
   
   ldy !brr_new_sample_pointer : phy  ; audio ram write location
   ldx #$0400    ; sample location
   ldy #$0b00      ; sample length
   ; jsr ConvertDMCtoBRR
   jsr ConvertDMCtoBRR_F1

%copyRomToRam($9300, $0400, $0d00)  ;  Load $d300 boss 2 dmc

init_boss2:
   ldy #$020c    ; directory entry for this sample
   ; jsr spc_begin_upload
   %spc_begin_upload()
   lda !brr_new_sample_pointer
   ; jsr spc_upload_byte
   %spc_upload_byte()
   lda !brr_new_sample_pointer+1
   ; jsr spc_upload_byte
   %spc_upload_byte()
   lda !brr_new_sample_pointer
   ; jsr spc_upload_byte
   %spc_upload_byte()
   lda !brr_new_sample_pointer+1
   ; jsr spc_upload_byte
   %spc_upload_byte()
   
   ldy !brr_new_sample_pointer : phy  ; audio ram write location
   ldx #$0400    ; sample location
   ldy #$0d00      ; sample length
   ; jsr ConvertDMCtoBRR
   jsr ConvertDMCtoBRR_F1

%copyRomToRam($a000, $0400, $0400)  ;  Load $e000 door unlock dmc

init_doorunlock:
   ldy #$0210    ; directory entry for this sample
   ; jsr spc_begin_upload
   %spc_begin_upload()
   lda !brr_new_sample_pointer
   ; jsr spc_upload_byte
   %spc_upload_byte()
   lda !brr_new_sample_pointer+1
   ; jsr spc_upload_byte
   %spc_upload_byte()
   lda !brr_new_sample_pointer
   ; jsr spc_upload_byte
   %spc_upload_byte()
   lda !brr_new_sample_pointer+1
   ; jsr spc_upload_byte
   %spc_upload_byte()
   
   ldy !brr_new_sample_pointer : phy  ; audio ram write location
   ldx #$0400    ; sample location
   ldy #$0400      ; sample length
   ; jsr ConvertDMCtoBRR
   jsr ConvertDMCtoBRR_F1

menuDingLoad:
    ; Upload menu navigation ding
   ldy #$0214
   ; jsr spc_begin_upload
   %spc_begin_upload()
   lda !brr_new_sample_pointer
   jsr spc_upload_byte
   lda !brr_new_sample_pointer+1
   jsr spc_upload_byte
   lda !brr_new_sample_pointer
   jsr spc_upload_byte
   lda !brr_new_sample_pointer+1
   jsr spc_upload_byte

;  TODO: fix
;    ldy.w !brr_new_sample_pointer : phy  ; audio ram write location
; continueSampleLoad:
;    lda beepSample,y
;    jsr spc_upload_byte
;    cpy #(beepSampleEnd-beepSample)     ; Length of sample data
;    bne continueSampleLoad

playSample:
    ; Do DSP writes to prep voice 0 for playback
    ldx #$206C    ;DSPFLAGS
    jsr write_dsp
    ldx #$004C    ;KEYON
    jsr write_dsp
    ldx #$FF5C    ;KEYOFF
    jsr write_dsp
    ldx #$025D    ;SRCOFFSET
    jsr write_dsp
    ldx #$7F00    ;VOLLEFT (voice 0)
    jsr write_dsp
    ldx #$7F01    ;VOLRIGHT (voice 0)
    jsr write_dsp

    ldx #$0002    ;PITCHLOW (voice 0)
    jsr write_dsp
    ldx #$1003    ;PITCHHIGH (voice 0)
    jsr write_dsp

    ldx #$0004    ;SRCN
    jsr write_dsp

   ldx #$0005
   jsr write_dsp
   ldx #$0006
   jsr write_dsp
   ldx #$7f07     ;GAIN
   jsr write_dsp

    ldx #$005C    ;KEYOFF
    jsr write_dsp
    ldx #$003D    ;NOISEON
    jsr write_dsp
    ldx #$004D    ;ECHOON
    jsr write_dsp
    ldx #$7F0C    ;MAINVOLLEFT
    jsr write_dsp
    ldx #$7F1C    ;MAINVOLRIGHT
    jsr write_dsp
    ldx #$002C    ;ECHOVOLLEFT
    jsr write_dsp
    ldx #$003C    ;ECHOVOLRIGHT
    jsr write_dsp

;  TODO: REMOVE HARDCODED VOICES
;  Prep voice #2 for playback:
    ldx #$025D    ;SRCOFFSET
    jsr write_dsp
    ldx #$7F10    ;VOLLEFT (voice 1)
    jsr write_dsp
    ldx #$7F11    ;VOLRIGHT (voice 1)
    jsr write_dsp

    ldx #$0012    ;PITCHLOW (voice 1)
    jsr write_dsp
    ldx #$1013    ;PITCHHIGH (voice 1)
    jsr write_dsp

    ldx #$0114    ;SRCN (voice 1)
    jsr write_dsp

   ldx #$0015
   jsr write_dsp
   ldx #$0016
   jsr write_dsp
   ldx #$7f17     ;GAIN
   jsr write_dsp

;  Prep voice #3 for playback:
    ldx #$025D    ;SRCOFFSET
    jsr write_dsp
    ldx #$7F20    ;VOLLEFT (voice 2)
    jsr write_dsp
    ldx #$7F21    ;VOLRIGHT (voice 2)
    jsr write_dsp

    ldx #$0022    ;PITCHLOW (voice 2)
    jsr write_dsp
    ldx #$1023    ;PITCHHIGH (voice 2)
    jsr write_dsp

    ldx #$0224    ;SRCN (voice 2)
    jsr write_dsp

   ldx #$0025
   jsr write_dsp
   ldx #$0026
   jsr write_dsp
   ldx #$7f27     ;GAIN
   jsr write_dsp

;  Prep voice #4 for playback:
    ldx #$025D    ;SRCOFFSET
    jsr write_dsp
    ldx #$7F30    ;VOLLEFT (voice 3)
    jsr write_dsp
    ldx #$7F31    ;VOLRIGHT (voice 3)
    jsr write_dsp

    ldx #$0032    ;PITCHLOW (voice 3)
    jsr write_dsp
    ldx #$1033    ;PITCHHIGH (voice 3)
    jsr write_dsp

    ldx #$0334    ;SRCN (voice 3)
    jsr write_dsp

   ldx #$0035
   jsr write_dsp
   ldx #$0036
   jsr write_dsp
   ldx #$7f37     ;GAIN
   jsr write_dsp

;  Prep voice #5 for playback:
    ldx #$025D    ;SRCOFFSET
    jsr write_dsp
    ldx #$7F40    ;VOLLEFT (voice 4)
    jsr write_dsp
    ldx #$7F41    ;VOLRIGHT (voice 4)
    jsr write_dsp

    ldx #$0042    ;PITCHLOW (voice 4)
    jsr write_dsp
    ldx #$1043    ;PITCHHIGH (voice 4)
    jsr write_dsp

    ldx #$0444    ;SRCN (voice 4)
    jsr write_dsp

   ldx #$0045
   jsr write_dsp
   ldx #$0046
   jsr write_dsp
   ldx #$7f47     ;GAIN
   jsr write_dsp

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
CheckABXY:
   lda #$0000
   ora !keynew
   and #(!A_BUTTON+!B_BUTTON+!X_BUTTON+!Y_BUTTON)

    beq checkABXYJumpPoint     ; TODO: Fix loop length so a brl
    bra ActivateSound          ;       is not required here.

checkABXYJumpPoint:
   brl CheckABXYDone

ActivateSound:
   sep #$20        ; A 8-bit
   lda !menuitem
   %playsoundinvoiceA($10, $92) ; Play selected voice
DoneActivateSound:
   ; jsr soundPlayWaitLoop
   jmp print_menu  ; Redraw menu

CheckABXYDone:

CheckUpButton:
   lda #$0000                          ; set A to zero
   ora !keynew                     ; check whether the up button was pressed this frame...
   and #(!UP_BUTTON+!LEFT_BUTTON)
   beq CheckUpButtonDone               ; if neither has occured, move on

   sep #$20                            ; set A to 8-bit

MenuUp:
   sep #$20        ; A 8-bit
   ; %playsound(0, $14, $28) ; Play amazing menu sound

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
   ; %playsound(0, $10, $00) ; Play amazing menu sound
   ; %playsound(0, $10, $92) ; Play amazing menu sound
   ; %playsound(0, $0c, $6e) ; Play dmc sound at 24,858hz  3182 ($0c6e) (manhandla; gleeok)
   ; %playsound(0, $0a, $a7) ; Play dmc sound at 21,307hz  2727 ($0aa7) (keydoor unlock)

   ;  0.128 * Hz.

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

hardLockTrap:
   bra hardLockTrap

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

;  Write to DSP subroutine
incsrc "sound/dsp-write.asm"

charset_asm_here:
incsrc "charset_test.asm"

;  Lookup tables for convert-dmc-to-brr-f1
org $009900
count1sTable:
db $00,$01,$01,$02,$01,$02,$02,$03,$01,$02,$02,$03,$02,$03,$03,$04,
   $01,$02,$02,$03,$02,$03,$03,$04,$02,$03,$03,$04,$03,$04,$04,$05,
   $01,$02,$02,$03,$02,$03,$03,$04,$02,$03,$03,$04,$03,$04,$04,$05,
   $02,$03,$03,$04,$03,$04,$04,$05,$03,$04,$04,$05,$04,$05,$05,$06,
   $01,$02,$02,$03,$02,$03,$03,$04,$02,$03,$03,$04,$03,$04,$04,$05,
   $02,$03,$03,$04,$03,$04,$04,$05,$03,$04,$04,$05,$04,$05,$05,$06,
   $02,$03,$03,$04,$03,$04,$04,$05,$03,$04,$04,$05,$04,$05,$05,$06,
   $03,$04,$04,$05,$04,$05,$05,$06,$04,$05,$05,$06,$05,$06,$06,$07,
   $01,$02,$02,$03,$02,$03,$03,$04,$02,$03,$03,$04,$03,$04,$04,$05,
   $02,$03,$03,$04,$03,$04,$04,$05,$03,$04,$04,$05,$04,$05,$05,$06,
   $02,$03,$03,$04,$03,$04,$04,$05,$03,$04,$04,$05,$04,$05,$05,$06,
   $03,$04,$04,$05,$04,$05,$05,$06,$04,$05,$05,$06,$05,$06,$06,$07,
   $02,$03,$03,$04,$03,$04,$04,$05,$03,$04,$04,$05,$04,$05,$05,$06,
   $03,$04,$04,$05,$04,$05,$05,$06,$04,$05,$05,$06,$05,$06,$06,$07,
   $03,$04,$04,$05,$04,$05,$05,$06,$04,$05,$05,$06,$05,$06,$06,$07,
   $04,$05,$05,$06,$05,$06,$06,$07,$05,$06,$06,$07,$06,$07,$07,$08

;  Table stores precomputed sample values for an incoming 7-bit value $00-$7f.
;  First half of table assumes a descending sample ("from above"),
;  second half assumes an ascending sample ("from below"), indicated by the 8th bit (msb) being set
dmcBRR_F0S8_and_F1S7_coeffLookup:
   ;  Low table (descending sample):
db $08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,
   $08,$08,$08,$08,$09,$09,$09,$09,$09,$09,$09,$09,$0a,$0a,$0a,$0a,
   $0a,$0a,$0a,$0a,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0c,$0c,$0c,$0c,
   ;  Next 8 bytes unused:
   $00,$00,$00,$00,$00,$00,$00,$00,
   $08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$00,$01,$02,$03,$04,$05,$06,$07,
   ;  Next 8 bytes unused:
   $00,$00,$00,$00,$00,$00,$00,$00,
   $00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$02,$02,$02,$02,
   $02,$02,$02,$02,$03,$03,$03,$03,$03,$03,$03,$03,$04,$04,$04,$04,
   $04,$04,$04,$04,$05,$05,
   ;  Next 10 bytes unused:
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,

   ;  High table (ascending sample):
   $08,$0a,$0a,$0a,$0a,$0a,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0c,$0c,
   $0c,$0c,$0c,$0c,$0c,$0c,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0e,$0e,
   $0e,$0e,$0e,$0e,$0e,$0e,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$00,$00,
   ;  Next 8 bytes unused:
   $00,$00,$00,$00,$00,$00,$00,$00,
   $08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$00,$01,$02,$03,$04,$05,$06,$07,
   ;  Next 8 bytes unused:
   $00,$00,$00,$00,$00,$00,$00,$00,
   $04,$04,$04,$04,$04,$04,$05,$05,$05,$05,$05,$05,$05,$05,$06,$06,
   $06,$06,$06,$06,$06,$06,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,
   $07,$07,$07,$07,$07,$07,
   ;  Next 10 bytes unused:
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00

dmcBRR_F1S6_and_F1S8_coeffLookup:
   ;  Low table (descending sample):
db $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,
   ;  Next 16 bytes unused:
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
   $08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,
   $08,$08,$09,$09,$09,$09,$0a,$0a,$0a,$0a,$0b,$0b,$0b,$0b,$0c,$0c,
   $0c,$0c,$0d,$0d,$0d,$0d,$0e,$0e,$0e,$0e,$0f,$0f,$0f,$0f,$00,$00,
   $00,$00,$01,$01,$01,$01,$02,$02,$02,$02,$03,$03,$03,$03,$04,$04,
   ;  Next 6 bytes unused:
   $00,$00,$00,$00,$00,$00,$01,$01,$02,$02,$02,$02,$02,$02,$02,$02,
   $02,$02,$02,$02,$02,$02,$02,$02,$03,$03,$03,$03,$03,$03,$03,$04,

   ;  High table (ascending sample):
   $0c,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0e,$0e,$0e,$0e,$0e,$0e,
   ;  Next 16 bytes unused:
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
   $0c,$0c,$0c,$0c,$0d,$0d,$0d,$0d,$0e,$0e,$0e,$0e,$0f,$0f,$0f,$0f,
   $00,$00,$00,$00,$01,$01,$01,$01,$02,$02,$02,$02,$03,$03,$03,$03,
   $04,$04,$04,$04,$05,$05,$05,$05,$06,$06,$06,$06,$07,$07,$07,$07,
   $07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,
   ;  Next 6 bytes unused:
   $00,$00,$00,$00,$00,$00,
   $03,$03,$03,$03,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,
   $04,$04,$04,$04,$05,$05,$05,$05,$05,$05

dmcRelativeChangeTable:
db $F0,$F2,$F4,$F6,$F8,$FA,$FC,$FE,$00,$02,$04,$06,$08,$0A,$0C,$0E,
   $10,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

;  Lookup table ranges (the table outputs the relevant brr header byte):
;   $38->$47
;       filter 0; shift 8
;   $28->$57
;       filter 1; shift 6
;   $08->$6d
;       filter 1; shift 7
;   $00->$7f
;       filter 1; shift 8
dmcSampleBRRHeaderTable:
db $84,$84,$84,$84,$84,$84,$84,$84,
   $74,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74,
   $74,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74,
   $64,$64,$64,$64,$64,$64,$64,$64,$64,$64,$64,$64,$64,$64,$64,$64,
   ; $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,
   ;  Replacing the $80 shift entries with $00 for nicer comparison in the subroutine
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
   $64,$64,$64,$64,$64,$64,$64,$64,$64,$64,$64,$64,$64,$64,$64,$64,
   $74,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74,
   $74,$74,$74,$74,$74,$74,
   $84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,
   $84,$84

;l48
;   $00->$7f
;       filter 1; shift 8
;   $08->$6d
;       filter 1; shift 7
;   $28->$57
;       filter 1; shift 6
;   $38->$47
;       filter 0; shift 8
;
;   S6    S7    S8
;    0     0     0   0
;   64	128	256   1  f
;  128	256	512   2  e
;  192	384	768   3  d
;  256	512	1024  4  c
;  320	640	1280  5  b
;  384	768	1536  6  a
;  448	896	1792  7  9
;-)512  1024   2048     8








;  Lookup tables to avoid scaled sample conversion
;  floor(-8 + (V - SV)>>(S-8)) calculation
org $009df0

shift8Table:  ; covers input $38->$47
db $08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$00,$01,$02,$03,$04,$05,$06,$07

shift9Table:  ; covers input $30->$4f
db $08,$08,$09,$09,$0a,$0a,$0b,$0b,$0c,$0c,$0d,$0d,$0e,$0e,$0f,$0f
db $00,$00,$01,$01,$02,$02,$03,$03,$04,$04,$05,$05,$06,$06,$07,$07

shiftATable:  ; covers input $20->$5f
db $08,$08,$08,$08,$09,$09,$09,$09,$0a,$0a,$0a,$0a,$0b,$0b,$0b,$0b
db $0c,$0c,$0c,$0c,$0d,$0d,$0d,$0d,$0e,$0e,$0e,$0e,$0f,$0f,$0f,$0f
db $00,$00,$00,$00,$01,$01,$01,$01,$02,$02,$02,$02,$03,$03,$03,$03
db $04,$04,$04,$04,$05,$05,$05,$05,$06,$06,$06,$06,$07,$07,$07,$07

shiftBTable:  ; covers input $00->$7f
db $08,$08,$08,$08,$08,$08,$08,$08,$09,$09,$09,$09,$09,$09,$09,$09
db $0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
db $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
db $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
db $00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01
db $02,$02,$02,$02,$02,$02,$02,$02,$03,$03,$03,$03,$03,$03,$03,$03
db $04,$04,$04,$04,$04,$04,$04,$04,$05,$05,$05,$05,$05,$05,$05,$05
db $06,$06,$06,$06,$06,$06,$06,$06,$07,$07,$07,$07,$07,$07,$07,$07

shiftJumpTable:
dw $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
dw shift8Table-$38,shift9Table-$30,shiftATable-$20,shiftBTable
dw $0000,$0000,$0000,$0000


;  Lookup tables linking brr voices to desired pitches
;  Dummy entries at [0] to accommodate %playsound() macro
org $009ff0
pitchHighTable:
db #$10, #$10, #$10, #$0c, #$0c, #$0a
pitchLowTable:
db #$00, #$92, #$92, #$6e, #$6e, #$a7

org $00a000

; samples:
beepSample:
   ;  These bytes represent the directory of brr samples written
   ;  to audio ram.  The first word is the start location; the second
   ;  the loop location (set to same for non-looping samples).
   ;  dw $0204      ; start
   ;  dw $0204      ; loop
   ;;;  DEBUG:  test out new sound!
   dw $2000    ; start of swordshoot brr
   dw $2000    ;
   dw $2750    ; start of linkhurt brr
   dw $2750
incbin "sound/samples/ding.brr"
; incbin "resources/output-wavs/ref-dmc-sine.brr"
beepSampleEnd:
   nop
; incsrc "sound/samples/beep.asm"

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