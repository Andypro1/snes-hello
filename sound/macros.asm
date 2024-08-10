if not(defined("macros"))
    !macros = 1

macro playsound(voice, pitchhigh, pitchlow)
    !playbit #= <voice>+1
    !konvoice := #$0!playbit
    !konvoice += 4C

    ldx #<pitchhigh><voice>3    ;PITCHHIGH (voice 0)
    jsr write_dsp
    ldx #<pitchlow><voice>2    ;PITCHLOW (voice 0)
    jsr write_dsp
    ldx !konvoice
    jsr write_dsp
endmacro


macro playsoundinvoiceA(pitchhigh, pitchlow)
    ; !playbit #= <voice>+1
    ; !konvoice := #$0!playbit
    ; !konvoice += 4C
playsoundinvoiceA:
    inc  ;  Voice = Voice param # + 1
    pha
    pha
    pha
    pha
    rep #$20        ; A 16-bit

    tay     ; move voice to y as index
    sep #$10 : rep #$10     ;  zero out high byte of [Y] by forcing 8-bit then 16-bit mode
    lda pitchHighTable,y  ; load from pitch lookup table
    xba
    and #$ff00  ; [A] now has #<pitchhigh>00
    ; lda #<pitchhigh>00
    ; xba
    sep #$20        ; A 8-bit
    pla
    dec   ; TODO:  what's going on herE??
    asl : asl : asl : asl
    clc
    adc #$03
    rep #$20
    tax
    sep #$20

    ; ldx #<pitchhigh><voice>3    ;PITCHHIGH (voice 0)
    jsr write_dsp
    pla

    rep #$20
    tay     ; move voice to y as index
    sep #$10 : rep #$10     ;  zero out high byte of [Y] by forcing 8-bit then 16-bit mode
    lda pitchLowTable,y  ; load from pitch lookup table
    xba
    and #$ff00  ; [A] now has #<pitchlow>00
    sep #$20        ; A 8-bit
    pla
    dec
    asl : asl : asl : asl
    clc
    adc #$02
    rep #$20
    tax
    sep #$20

    ; ldx #<pitchlow><voice>2    ;PITCHLOW (voice 0)
    jsr write_dsp

    pla
    cmp #$05
    beq flags5
    cmp #$04
    beq flags4
    cmp #$03
    beq flags3
    cmp #$02
    beq flags2
    cmp #$01
    beq flags1
    bra flags0

flags5:
    lda #$10
    bra flagsDone
flags4:
    lda #$08
    bra flagsDone
flags3:
    lda #$04
    bra flagsDone
flags2:
    lda #$02
    bra flagsDone
flags1:
    lda #$01
    bra flagsDone
flags0:
    lda #$00
    bra flagsDone


flagsDone:
    rep #$20
    xba
    and #$ff00
    clc
    adc #$004c
    tax
    sep #$20

    ; ldx !konvoice
    jsr write_dsp
endmacro


macro copyRomToRam(romSourcePage, ramDestPage, length)
   ;  See https://en.wikibooks.org/wiki/Super_NES_Programming/SNES_memory_map#LoROM
   ;  when deciding where to load into ram (or whether it's even worth it)
   PHB                ; Preserve data bank
   REP #$30           ; 16-bit AXY
   LDA #<length>; #$0b00 ;#$081f ;#$00a0;  #$0400 ;#$00a0 ;#$0750         ; \
   LDX #<romSourcePage>; #$8800; #$8740; #$a000 ;#$8740 ;#$8000  ;#$9300     ;  |  subtract $4000 from nes locations
   LDY #<ramDestPage>         ;  | Move [A] bytes of data from $1f8000 to $000400
   MVN $00, $1f       ; /
   SEP #$30           ; 8-bit AXY
   PLB                ; Recover data bank

   rep #$10        ; X/Y 16-bit
   sep #$20        ; A 8-bit
endmacro


macro copyEarlyRomToRam(romSourcePage, ramDestPage, length)
   PHB                ; Preserve data bank
   REP #$30           ; 16-bit AXY
   LDA #<length>; #$0b00 ;#$081f ;#$00a0;  #$0400 ;#$00a0 ;#$0750         ; \
   LDX #<romSourcePage>; #$8800; #$8740; #$a000 ;#$8740 ;#$8000  ;#$9300     ;  |  subtract $4000 from nes locations
   LDY #<ramDestPage>         ;  | Move [A] bytes of data from $1f8000 to $000400
   MVN $00, $00       ; /
   SEP #$30           ; 8-bit AXY
   PLB                ; Recover data bank

   rep #$10        ; X/Y 16-bit
   sep #$20        ; A 8-bit
endmacro


; Starts upload to SPC addr Y and sets Y to
; 0 for use as index with spc_upload_byte.
; Preserved: X
macro spc_begin_upload()
    sty $2142

    ; Send command
    lda $2140
    clc
    adc #$22
    bne ?skip       ; special case fully verified
    inc
?skip:
    sta $2141
    sta $2140

    ; Wait for acknowledgement
?waitUploadStartAck:
    cmp $2140
    bne ?waitUploadStartAck

    ; Initialize index
    ldy #$0000
endmacro


; Uploads byte A to SPC and increments Y. The low byte
; of Y must not changed between calls.
; Preserved: X
macro spc_upload_byte()
    sta $2141

    ; Signal that it's ready
    tya
    sta $2140
    iny

    ; Wait for acknowledgement
?waitUploadByteAck:
    cmp $2140
    bne ?waitUploadByteAck
endmacro

endif