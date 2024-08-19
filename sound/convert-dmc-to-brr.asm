   !dmc_running_value = $0326
   !brr_first_last = $030a    ;  TODO: use better boolean name.  0 or 1 
                              ;  depending on whether we're working on an even sample
                              ;  nibble (0..14) or an odd one (1..15)
   !dmc_sample_length = $030b ; 16-bit length value
   !brr_new_sample_pointer = $030d  ;  16-bit spc-700 address for storing the new brr sample
   !brr_cur_shift = $030f   ;  16-bit current brr block's shift value
   !dmc_working_value = $0311   ;  Area for the current dmc byte being worked on
   !brr_high_nibble = $0312    ;  Storage for the high sample while we calculate the low sample
   !brr_scaled_value = $0313    ;  Temporary storage for the scaled value
   !brr_isBackBlock = $0314    ;  1 if we're on Samples IIII->PPPP of the BRR block, 0 otherwise
   !cdb_SV = $0319          ; The calculated SV value based on the brr shift
   !cdb_VminusSV = $0320
   !cdb_Sminus8 = $0321
   !brr_start_location = $0322  ;  The initial audio ram write location

    incsrc "macros.asm"
    incsrc "select-brr-shift.asm"

;-------------------------------------------------------------------------------
;   Description: Loads NES DMC dpcm audio data of length [Y] from address [X].
;       Converts the data to a BRR sample and loads into Voice #2 (TODO: genericize)
;       This routine has no return value (remains $00).
;   Parameters: [X] - 16 bit DMC sample start location
;               [Y] - 16 bit DMC sample length
;               [S-top] - 16 bit audio ram write start location
;   Variables:  
;   Returns:    no value
;-------------------------------------------------------------------------------
ConvertDMCtoBRR:
    STY.w !dmc_sample_length  ;  Store the DMC sample length param [Y]

;  DMC zero-crossing point (chosen in the middle of the 7-bit range)
    ;  This is now done by the calling code (each z1 sample starts differently!)

;     start the brrFirstLast bit at 1 (first 8 samples)
    lda #$01
    sta !brr_first_last

    ;  Start with the front of a BRR block
    stz !brr_isBackBlock

;     set indexX register to dmc sample start location
    ;  Already done via param [X]

;     set start write location in audio ram
    rep #$20 : ply : tya : ply  ;  Grab the stack-top aram parameter
    sty !brr_start_location         ; saving original aram start location
    sty !brr_new_sample_pointer     ; prepping aram location
    pha : sep #$20  ;  Restore stack and reset to 8-bit

;   initialize audio ram writes
    ldy.w !brr_new_sample_pointer
    ; jsr spc_begin_upload
    %spc_begin_upload()
    sty.w !brr_new_sample_pointer

processDcmSample:
    ldy #$0008      ; init indexY register to 8 (number of dmc samples in a byte)

    lda !brr_isBackBlock
    bne loadNewDcmByte  ;  skip brr shift calculation (only done before front block)

calcBrrShiftVal:
    phy     ;  Preserve indexY
    phx     ;  Preserve indexX

    rep #$20    ; A 16-bit
    lda $00,x  ;  Fetch next two dcm bytes (16 samples)
    xba         ;  Byte-swappy
    ldx !dmc_running_value
    sep #$10 : rep #$10     ;  zero out high byte of [X] by forcing 8-bit then 16-bit mode
    
    jsr SelectBRRShift      ;  loads [A] with new shift value
    sep #$20    ; [A] back to 8-bit

    sta !brr_cur_shift      ;  Store the value for this brr block

;     Write new brr header byte
    ldy.w !dmc_sample_length    ;  TODO: make macro
    cpy #$0003
    bcs skipEndFlag    ;  If 3 or more bytes left, no BRR end flag
    ; adc #$01        ;  Else, add end flag
    inc     ; [OPT]
skipEndFlag:

    ldy.w !brr_new_sample_pointer
;    jsr spc_upload_byte             ;  Write the brr block header byte to audio ram
   %spc_upload_byte()
    sty.w !brr_new_sample_pointer

    plx     ;  Restore indexX
    ply     ;  Restore indexY

loadNewDcmByte:
    ;  Begin working on the waveform value
    lda $00,x  ;  Fetch current dcm byte
    sta !dmc_working_value

nextDmcBit:
;       Floor check.  If sample has underflowed, new sample = old;
;       Ceiling check.  If sample > $7f, new sample = old;
    and #$80    ;  Bitmask the msb, because we're shifting the byte left in this loop.
                ;  The bit in question will always be in the msb.
    cmp #$80
    bmi sub2

add2:
    lda !dmc_running_value
    cmp #$7f
    beq scaleToOutputRange    ;  Skip addition if we are at the upper bound $7f
    inc !dmc_running_value ;adc #$01
    inc !dmc_running_value
    ; sta !dmc_running_value
    bra scaleToOutputRange
sub2:
    lda !dmc_running_value
    cmp #$01
    bcc scaleToOutputRange    ;  Skip subtraction if we are at/below the lower bound $00
    dec !dmc_running_value ;sbc #$01
    dec !dmc_running_value
    ; sta !dmc_running_value
    bra scaleToOutputRange

 scaleToOutputRange:
     phx            ;  Preserve indexX and Y

    ;  This next part takes 11 lines to lookup an doubly-indexed value.
    ;  Can't this be done better?
    lda !brr_cur_shift
    clc : ror : ror : ror   ;  Move shift value to low nibble, then x2 to represent words
    rep #$20
    and #$00ff
    tax
    lda $00,x       ;  Load the correct shift table address
    clc
    adc !dmc_running_value  ;  Add the sample offset
    tax     ; [X] has the running value offset into a particular shift table
    sep #$20
    lda $00,x   ;  Load the specific value from the lookup table

    plx             ;  Restore indexX and Y

    ; and #$0f                  ;  Scale value down to 1 nibble.  Negative values
    ;                           ;  will have data in the high nibble due to 2s complement
    sta !brr_scaled_value     ;  Preserve new brr value

    ;  Determine whether we have two samples ready to send
    lda !brr_first_last
    beq send2Samples        ;  Have two nibbles; ready to send
    lda !brr_scaled_value   ;  else, move this value to !brr_high_nibble for next loop pass
    sta !brr_high_nibble
    bra prepNextLoop

;     Store 2 new brr samples (1 byte) to audio ram
send2Samples:
    phy     ;  Preserve indexY
    
    ;  Load in the new two-sample BRR byte into [A]
    lda !brr_high_nibble
    asl #$04    ;  Bump the data up to the high nibble
    clc
    adc !brr_scaled_value   ;  Add in the low nibble data

    ldy.w !brr_new_sample_pointer
    ; jsr spc_upload_byte
    %spc_upload_byte()
    sty.w !brr_new_sample_pointer

    ply     ;  Restore indexY

    ;  Prepare the next loop iteration
prepNextLoop:
    ;     toggle brrFirstLast bit
    lda !brr_first_last
    eor #$01        ;  Toggle 0->1 and 1->0
    sta !brr_first_last

;     Shift dpcm sample left (working from msb to lsb)
    ; clc   [OPT]: not functional
    ; asl !dmc_working_value
    lda !dmc_working_value  ;  Fetch current dmc "working shift" byte
    asl     ;  Shift it MORE
    sta !dmc_working_value  ;  Save new "working shift"


    dey     ; decrement indexY
    ; cpy #$0000  ;     if indexY register > 0 [OPT-ed out]
;     Loop to nextDmcBit
    beq prepNextDmcByte     ; TODO: Fix loop length so a brl
    brl nextDmcBit          ;       is not required here.

prepNextDmcByte:
    inx     ; indexX to next dmc sample byte
    
    rep #$20        ; 16-bit
    dec !dmc_sample_length
    beq EndConvertDMCtoBRR  ;  done with this sample; exit subroutine
    sep #$20        ; 8-bit

    ;     toggle BRR block half
    lda !brr_isBackBlock
    eor #$01            ;  Toggle 0->1 and 1->0
    sta !brr_isBackBlock

    jmp processDcmSample    ;  top of loop

EndConvertDMCtoBRR:
;  Set !brr_new_sample_pointer to the next valid brr audio ram start location
    lda.w !brr_start_location 
    clc
    adc !brr_new_sample_pointer
    sta !brr_new_sample_pointer
    sep #$20        ; 8-bit
rts




nextDmcBitSub:

rts



;  UNUSED (remove)
; scaleToOutputRangeSub:
; ;  general case scaling from dmc 7-bit unsigned value ($00->$7f) to brr range:
; ;   floor(-8 + (V - SV)>>(S-8))
; ;   where V is incoming value
; ;         SV is "shift starting number"  (s8: 38, s9: 30, sA: 20, sB: 0)
; ;         S is shift number
; ;         SV pattern:  %00a9 8000
; ;                         98
; ;                         8
; ;         each shift lower than b shifts a 1 into bit 6, then 5, then 4, etc.

;     ;  Calculate SV value
;     lda #$00
;     ldx !brr_cur_shift
;     cpx #$00b0
;     beq doneSVcalc
;     cpx #$00a0
;     beq SVaddA
;     cpx #$0090
;     beq SVadd9
;     cpx #$0080
;     beq SVadd8
;     jml hardLockTrap  ;  SOMETHING WENT WRONG

; SVadd8:
;     ora #$08
; SVadd9:
;     ora #$10
; SVaddA:
;     ora #$20
; doneSVcalc:
;     sta !cdb_SV     ;  Save calculated SV

; ;  (V - SV) part
;     lda !dmc_running_value
;     sec
;     sbc !cdb_SV     ;  UNDERFLOW ERROR HERE(?).  at SPC mem $266e
;                     ;  [dmc_running_value was $36, and $38 was subtracted (shift $80) resulting in [A]=$fe]
;     sta !cdb_VminusSV   ;  Save calculated (V - SV)

; ;  >>(S-8)
;     lda !brr_cur_shift
;     sec
;     sbc #$80
;     sta !cdb_Sminus8

; ;  (V - SV)>>(S-8)
;     ;  [X] still has !brr_cur_shift here;
;     ;  use it instead of [A] so we can pre-store [A] with !cdb_VminusSV
;     lda !cdb_VminusSV

;     cpx #$00b0
;     beq shift3
;     cpx #$00a0
;     beq shift2
;     cpx #$0090
;     beq shift1
;     cpx #$0080
;     beq doneShifting
;     jml hardLockTrap  ;  SOMETHING WENT WRONG

; shift3:
;     cmp #$80
;     ror
; shift2:
;     cmp #$80
;     ror
; shift1:
;     cmp #$80
;     ror

; doneShifting:
; ;  (-8 + (V - SV)>>(S-8))
;     sec
;     sbc #$08
;     sta !brr_scaled_value     ;  Preserve new brr value
; rts