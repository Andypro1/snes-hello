if not(defined("sound_play_wait_loop"))
    !sound_play_wait_loop = 1

;-------------------------------------------------------------------------------
;   Description: Waits for the voice (number in [A]) to stop playing and return
;                to the main program.
;   Parameters: [A] - voice # to monitor
;   Returns:    No return value
;-------------------------------------------------------------------------------

soundPlayWaitLoop:
; x8h - VxENVX
read_dsp:
    ; Just do a two-byte upload to $00F2-$00F3, so we
    ; set the DSP address, then write the byte into that.
    ldy #$F208    ; dsp address $F2, desired dsp register $08

    sty $2142

    ; Send command
    lda $2140
    clc
    adc #$22
    bne skipSC       ; special case fully verified
    inc
skipSC:  sta $2141
    sta $2140

    ; Wait for acknowledgement
waitToGetSpcData:  
    cmp $2140
    bne waitToGetSpcData

    ; Load read spc data
    lda $2143
    beq soundPlayWaitLoop
rts




    ; pla

    ; sta $2141

    ; ; Signal that it's ready
    ; tya
    ; sta $2140
    ; iny

;     ; Wait for acknowledgement
; waitUploadByteAck:
;     cmp $2140
;     bne waitUploadByteAck

;     pla

;     sta $2141

;     ; Signal that it's ready
;     tya
;     sta $2140
;     iny

;     ; Wait for acknowledgement
; waitUploadByteAck2:
;     cmp $2140
;     bne waitUploadByteAck2


endif