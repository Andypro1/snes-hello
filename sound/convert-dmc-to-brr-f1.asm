   !dmc_running_value = $0326
   !dmc_sample_length = $030b ; 16-bit length value
   !brr_new_sample_pointer = $030d  ;  16-bit spc-700 address for storing the new brr sample
   !brr_cur_shift = $030f   ;  16-bit current brr block's shift value
   !dmc_working_value_16 = $0328
   !brr_high_nibble = $0312    ;  Storage for the high sample while we calculate the low sample
   !brr_scaled_value = $0313    ;  Temporary storage for the scaled value
;    !brr_isBackBlock = $0314    ;  1 if we're on Samples IIII->PPPP of the BRR block, 0 otherwise
;    !cdb_SV = $0319          ; The calculated SV value based on the brr shift
;    !cdb_VminusSV = $0320
;    !cdb_Sminus8 = $0321
   !brr_start_location = $0322  ;  The initial audio ram write location

   !dmc_change_after_16 = $00
   !brr_half_sample = $01       ;  Saves the first half of the working brr sample byte
   !dmc_working_b1b2 = $03     ;  16-bit temp storage for working [B1][B2] values
   !brr_loop_counter = $05      ;  8->0 inner loop counter representing brr bytes

    incsrc "macros.asm"
    ; incsrc "select-brr-shift.asm"

;-------------------------------------------------------------------------------
;   Description: Loads NES DMC dpcm audio data of length [Y] from address [X].
;       Converts the data to a BRR sample and loads to audio ram starting at [S-top]
;       This routine has no return value (remains $00).
;   Parameters: [X] - 16 bit DMC sample start location
;               [Y] - 16 bit DMC sample length
;               [S-top] - 16 bit audio ram write start location
;   Variables:  
;   Returns:    no value
;-------------------------------------------------------------------------------
ConvertDMCtoBRR_F1:
;  General strategy:

;  TODO: DEBUGGING:
;       dmc source checks/changes:
;           - Each DM sample does increase or decrease by 2 instead of 1.
;           - Implement correct $00->$7f bounds checks
;           - Add "initial 0 block" which prints 15 0 samples followed by
;               the initial dmc value sample in an F0 BRR block
;           - use $c0 $00 $00 $00 $00 $00 $00 $00 $0c  for start value of $00 (-16,384, 0 error)
;           - try using $b0 $00 $00 $00 $00 $00 $00 $00 $07  for start value of $7f (14,336, -1,792 error, ouch)
;           * There may be a clever way to use a second non-f0 block to climb up to the desired values instead...
;           - examine new waveform output after the changes above

    sty.w !dmc_sample_length  ;  Store the DMC sample length param in [Y]

;     set start write location in audio ram
    rep #$20 : ply : tya : ply  ;  Grab the stack-top aram parameter
    pha     ;  Restore stack
    sty !brr_start_location         ; saving original aram start location
    sty !brr_new_sample_pointer     ; prepping aram location (TODO: opt dupe)

    ;   initialize audio ram writes
    ; jsr spc_begin_upload
    sep #$20
    %spc_begin_upload()
    sty.w !brr_new_sample_pointer   ; (opt: just iny?)

    ;  Initialize !brr_loop_counter
    lda #$08
    sta !brr_loop_counter

    rep #$20

;  Establish running value [R]
    ;  This is now done by the calling code (each z1 sample starts differently!)

;  (16-bit?) Get 2 dmc bytes, [B1] and [B2]
    lda $00,x
    sep #$20 : tay  ;  Store [B2] in [Y]
    stx !dmc_working_value_16   ; Save indexX

convertOuterLoop:
;  (8-bit) [B1]: lookup number of 1s in lookup table result is [N1]
    xba     ;  Begin operating on [B1]
    rep #$20 : and #$00ff   ;  Prep for 16-bit math on low byte
    clc : adc #count1sTable
    tax     ;  Store the [B1] offset into count1sTable in [X]

    sep #$20
    lda $00,x
    tax     ;  Store [N2] result in [X]

;  (8-bit) [B2]: lookup 1s count in (same) table, result is [N2]
    tya
    rep #$20 : and #$00ff   ;  Prep for 16-bit math on low byte
    adc #count1sTable
    tay     ;  Store the [B2] offset into count1sTable in [Y]
    
    sep #$20
    txa         ;  Move [N2] to [A]
    tyx
;  (any-bit) Add [N1]+[N2] = [B]: a brr block's worth of 1s count
    adc $00,x   ;  [A] now has [N1]+[N2] = [B]


;  bignote!:  number of bits always dictates the final relative change!
;  worst case is the intermediary bit counting result for TWO bytes is
;  8 higher or lower than the final relative change.
;  so, just assume a less precise shift if the final relative change is within 8 of a shift boundary.

;  (8-bit) Lookup table; index into with [B].  Returns relative change [C].
;  0    1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16 (decimal)
;  F0   F2  F4  F6  F8  FA  FC  FE  00  02  04  06  08  0A  0C  0E  10 (all in $hex)
    rep #$20 : and #$00ff   ;  Prep for 16-bit math on low byte
    adc #dmcRelativeChangeTable
    tax

    sep #$20
    lda $00,x   ;  [A] now has relative change [C]
    sta !dmc_change_after_16    ; TODO: opt?
    tay         ;  Store [C] in [Y]

;  (8-bit) Add [C]+[R] = [M]: the final relative change at end of brr block.
    lda !dmc_running_value
    tax     ; Duplicate [M] in [X]

;  (8-bit) Same table; index into with [M] = [S2]
    rep #$20 : and #$00ff   ;  Prep for 16-bit math on low byte
    adc #dmcSampleBRRHeaderTable
    ; pha         ;  Store shift lookup index on stack
    tax

    sep #$20
    lda $00,x   ;  [S2] now in [A]
    pha         ;  Store [S2] on stack

;  (8-bit) Lookup table of shift value; index into with [R] = [S1]
    tya
    rep #$20 : and #$00ff   ;  Prep for 16-bit math on low byte
    adc !dmc_running_value  ;  Add [M] to [C]
    adc #dmcSampleBRRHeaderTable    ;  Add table pointer
    tax

    sep #$20
    lda $00,x   ;  [S1] now in [A]

;  Lookup table ranges (the table should output the relevant brr header byte):
;   $38->$47
;       filter 0; shift 8
;   $28->$57
;       filter 1; shift 6
;   $08->$6d
;       filter 1; shift 7
;   $00->$7f
;       filter 1; shift 8
;
;  (any-bit) Pick the greater of [S1] and [S2] = [S]
    cmp $01,s
    bcs writeBRRHeaderByte     ;  If [S1] >= [S2]

;   (8-bit) Write header byte to spc
    pla     ;  Choose [S2] and return it to [A]

    ;  Alter $00 -> $80 for nicer math above
    cmp #$00
    bne writeHeader1
    lda #$80

writeHeader1:
    sta !brr_cur_shift  ; [todo: opt out]

    ldy.w !dmc_sample_length    ;  TODO: make macro
    cpy #$0003
    bcs skipEndFlag1    ;  If 3 or more bytes left, no BRR end flag
    inc     ; add end flag

skipEndFlag1:
    ldy.w !brr_new_sample_pointer
    %spc_upload_byte()
    sty.w !brr_new_sample_pointer
    bra afterHeaderWritten

writeBRRHeaderByte:
    ;  Alter $00 -> $80 for nicer math above
    cmp #$00
    bne writeHeader2
    lda #$80

writeHeader2:
    sta !brr_cur_shift  ; [todo: opt out]

    ldy.w !dmc_sample_length    ;  TODO: make macro
    cpy #$0003
    bcs skipEndFlag2    ;  If 3 or more bytes left, no BRR end flag
    inc     ; add end flag

skipEndFlag2:

    ldy.w !brr_new_sample_pointer
    %spc_upload_byte()
    sty.w !brr_new_sample_pointer
    pla     ;  Discard unused stack value

afterHeaderWritten:
;  need for loop:  running sample [i] (7 bits), prior sample direction (1 bit), [Y]
;                  current shift table index (16-bit),  [X]
;                  current shift type (separate loop code for each shift type)
;                  shift table offset (8-bit), ??
;                  2 dmc working bytes (16-bit),    [A]
;                  loop counter of #$10    , [ stack??]
;
;   (16-bit?) Loop [i] through the 16 bits in [B1] and [B2]

    ;  Load 2 dmc working bytes
    rep #$20
    ldx !dmc_working_value_16
    lda $00,x   ; [B1][B2]  (opt: better addressing mode?)

    ;  Choose from among four loops based on current shift type
    ldx !brr_cur_shift
    cpx #$0080    ;  Filter 0, Shift 8
    bne nextCompare1
    brl F0S8_BlockCreation
nextCompare1:
    cpx #$0064
    ; TODO:  subroutine(s) to avoid brls?
    bne nextCompare2
    brl F1S6_BlockCreation
nextCompare2:
    cpx #$0074
    bne F1S8_BlockCreation
    brl F1S7_BlockCreation

;  TODO:  See if any of these inner loop sections are shift agnostic and can be de-duped.
;;  Filter-1, Shift-8 inner loop  ;;
;----------------------------------;
F1S8_BlockCreation:
    tay
    ;  Prep sample index in [X]
    lda #dmcBRR_F1S6_and_F1S8_coeffLookup
    clc
    adc !dmc_running_value
    tax
    tya

F1S8_NextSamples:
    asl
    bcs F1S8_LoadFromBelow

F1S8_LoadFromAbove:
    tay
    txa
    and #$ff7f  ;  Point to low half of sample index table
    tax
    dex     ;  Move to the new working value (previous-1)
    tya     ;  Restore !dmc_working_value_16 to [A]

    ldy $00,x   ; indexed table load
    sty !brr_half_sample    ;  Save first half of sample byte
    asl !brr_half_sample    ;  Bump the data up to the high nibble
    asl !brr_half_sample
    asl !brr_half_sample
    asl !brr_half_sample    ;  (opt: obviously, this is bad)

    ;  Second brr sample unrolled
    asl
    bcs F1S8_LoadFromBelow2
    bra F1S8_LoadFromAbove2  ;  Next sample

F1S8_LoadFromBelow:
    tay
    txa
    ora #$0080  ;  Point to high half of sample index table
    tax
    inx     ;  Move to the new working value (previous+1)
    tya     ;  Restore working [B1][B2]

    ldy $00,x   ; indexed table load
    sty !brr_half_sample    ;  Save first half of sample byte
    asl !brr_half_sample    ;  Bump the data up to the high nibble
    asl !brr_half_sample
    asl !brr_half_sample
    asl !brr_half_sample    ;  (opt: obviously, this is bad)

    ;  Second brr sample unrolled
    asl
    bcs F1S8_LoadFromBelow2

F1S8_LoadFromAbove2:
    tay
    txa
    and #$ff7f  ;  Point to low half of sample index table
    tax
    dex     ;  Move to the new working value (previous-1)

    lda $00,x   ; indexed table load

    sep #$20
    clc             ; (opt: check for consistent carry flag)
    adc !brr_half_sample    ;  Restore first half of sample byte

    sty !dmc_working_b1b2
    
    ;  Send 2 samples
    ldy.w !brr_new_sample_pointer
    %spc_upload_byte()
    sty.w !brr_new_sample_pointer

    dec !brr_loop_counter
    bne F1S8a_notFinished
    jml afterInnerLoops
    
F1S8a_notFinished:
    rep #$20
    lda !dmc_working_b1b2
    bra F1S8_NextSamples  ;  Next sample

F1S8_LoadFromBelow2:
    tay
    txa
    ora #$0080  ;  Point to high half of sample index table
    tax
    inx     ;  Move to the new working value (previous+1)

    lda $00,x   ; indexed table load

    sep #$20
    clc             ; (opt: check for consistent carry flag)
    adc !brr_half_sample    ;  Restore first half of sample byte

    sty !dmc_working_b1b2

    ;  Send 2 samples
    ldy.w !brr_new_sample_pointer
    %spc_upload_byte()
    sty.w !brr_new_sample_pointer

    dec !brr_loop_counter
    bne F1S8b_notFinished
    jml afterInnerLoops

F1S8b_notFinished:
    rep #$20
    lda !dmc_working_b1b2
    brl F1S8_NextSamples  ;  Next sample


;;  Filter-0, Shift-8 inner loop  ;;
;----------------------------------;
F0S8_BlockCreation:
    tay
    ;  Prep sample index in [X]
    lda #dmcBRR_F0S8_and_F1S7_coeffLookup
    clc
    adc !dmc_running_value  ; TODO: add to other loops
    ; adc #$0038  ;  Add offset $38 for the correct table block
    tax
    tya

F0S8_NextSamples:
    asl
    bcs F0S8_LoadFromBelow

F0S8_LoadFromAbove:
    tay
    txa
    and #$ff7f  ;  Point to low half of sample index table
    tax
    dex     ;  Move to the new working value (previous-1)
    tya     ;  Restore !dmc_working_value_16 to [A]

    ldy $00,x   ; indexed table load
    ;  TODO: If $00 detected here, we have broken the hard floor/ceiling of F0S8.
    ;  Call a subroutine that resets all relevant indexes and pointers, sets the current
    ;  block shift to F1S6, and start again from convertOuterLoop.
    bne F0S8_continueInBounds
    jml F0S8_resetOutOfBounds
F0S8_continueInBounds:
    sty !brr_half_sample    ;  Save first half of sample byte
    asl !brr_half_sample    ;  Bump the data up to the high nibble
    asl !brr_half_sample
    asl !brr_half_sample
    asl !brr_half_sample    ;  (opt: obviously, this is bad)

    ;  Second brr sample unrolled
    asl
    bcs F0S8_LoadFromBelow2
    bra F0S8_LoadFromAbove2  ;  Next sample

F0S8_LoadFromBelow:
    tay
    txa
    ora #$0080  ;  Point to high half of sample index table
    tax
    inx     ;  Move to the new working value (previous+1)
    tya     ;  Restore working [B1][B2]

    ldy $00,x   ; indexed table load
    ;  TODO: If $00 detected here, we have broken the hard floor/ceiling of F0S8.
    ;  Call a subroutine that resets all relevant indexes and pointers, sets the current
    ;  block shift to F1S6, and start again from convertOuterLoop.
    beq F0S8_resetOutOfBounds
    sty !brr_half_sample    ;  Save first half of sample byte
    asl !brr_half_sample    ;  Bump the data up to the high nibble
    asl !brr_half_sample
    asl !brr_half_sample
    asl !brr_half_sample    ;  (opt: obviously, this is bad)

    ;  Second brr sample unrolled
    asl
    bcs F0S8_LoadFromBelow2

F0S8_LoadFromAbove2:
    tay
    txa
    and #$ff7f  ;  Point to low half of sample index table
    tax
    dex     ;  Move to the new working value (previous-1)

    lda $00,x   ; indexed table load
    
    ;  Capture the two OOB states
    beq F0S8_resetOutOfBounds
    cmp #$0008
    beq F0S8_resetOutOfBounds

    sep #$20
    clc             ; (opt: check for consistent carry flag)
    adc !brr_half_sample    ;  Restore first half of sample byte

    sty !dmc_working_b1b2
    
    ;  Send 2 samples
    ldy.w !brr_new_sample_pointer
    %spc_upload_byte()
    sty.w !brr_new_sample_pointer

    dec !brr_loop_counter
    bne F0S8a_notFinished
    jml afterInnerLoops
    
F0S8a_notFinished:

    rep #$20
    lda !dmc_working_b1b2
    bra F0S8_NextSamples  ;  Next sample

F0S8_LoadFromBelow2:
    tay
    txa
    ora #$0080  ;  Point to high half of sample index table
    tax
    inx     ;  Move to the new working value (previous+1)

    lda $00,x   ; indexed table load

    ;  Capture the two OOB states
    beq F0S8_resetOutOfBounds
    cmp #$0008
    beq F0S8_resetOutOfBounds

    sep #$20
    clc             ; (opt: check for consistent carry flag)
    adc !brr_half_sample    ;  Restore first half of sample byte

    sty !dmc_working_b1b2

    ;  Send 2 samples
    ldy.w !brr_new_sample_pointer
    %spc_upload_byte()
    sty.w !brr_new_sample_pointer

    dec !brr_loop_counter
    bne F0S8b_notFinished
    jml afterInnerLoops

F0S8b_notFinished:

    rep #$20
    lda !dmc_working_b1b2
    brl F0S8_NextSamples  ;  Next sample

F0S8_resetOutOfBounds:
    jsr resetOutOfBounds
    brl afterHeaderWritten


;;  Filter-1, Shift-6 inner loop  ;;
;----------------------------------;
F1S6_BlockCreation:
    tay
    ;  Prep sample index in [X]
    lda #dmcBRR_F1S6_and_F1S8_coeffLookup
    clc
    adc !dmc_running_value
    tax
    tya

F1S6_NextSamples:
    asl
    bcs F1S6_LoadFromBelow

F1S6_LoadFromAbove:
    tay
    txa
    and #$ff7f  ;  Point to low half of sample index table
    tax
    dex     ;  Move to the new working value (previous-1)
    tya     ;  Restore !dmc_working_value_16 to [A]

    ldy $00,x   ; indexed table load
    sty !brr_half_sample    ;  Save first half of sample byte
    asl !brr_half_sample    ;  Bump the data up to the high nibble
    asl !brr_half_sample
    asl !brr_half_sample
    asl !brr_half_sample    ;  (opt: obviously, this is bad)

    ;  Second brr sample unrolled
    asl
    bcs F1S6_LoadFromBelow2
    bra F1S6_LoadFromAbove2  ;  Next sample

F1S6_LoadFromBelow:
    tay
    txa
    ora #$0080  ;  Point to high half of sample index table
    tax
    inx     ;  Move to the new working value (previous+1)
    tya     ;  Restore working [B1][B2]

    ldy $00,x   ; indexed table load
    sty !brr_half_sample    ;  Save first half of sample byte
    asl !brr_half_sample    ;  Bump the data up to the high nibble
    asl !brr_half_sample
    asl !brr_half_sample
    asl !brr_half_sample    ;  (opt: obviously, this is bad)

    ;  Second brr sample unrolled
    asl
    bcs F1S6_LoadFromBelow2

F1S6_LoadFromAbove2:
    tay
    txa
    and #$ff7f  ;  Point to low half of sample index table
    tax
    dex     ;  Move to the new working value (previous-1)

    lda $00,x   ; indexed table load

    sep #$20
    clc             ; (opt: check for consistent carry flag)
    adc !brr_half_sample    ;  Restore first half of sample byte

    sty !dmc_working_b1b2
    
    ;  Send 2 samples
    ldy.w !brr_new_sample_pointer
    %spc_upload_byte()
    sty.w !brr_new_sample_pointer

    dec !brr_loop_counter
    bne F1S6a_notFinished
    jml afterInnerLoops
    
F1S6a_notFinished:
    rep #$20
    lda !dmc_working_b1b2
    bra F1S6_NextSamples  ;  Next sample

F1S6_LoadFromBelow2:
    tay
    txa
    ora #$0080  ;  Point to high half of sample index table
    tax
    inx     ;  Move to the new working value (previous+1)

    lda $00,x   ; indexed table load

    sep #$20
    clc             ; (opt: check for consistent carry flag)
    adc !brr_half_sample    ;  Restore first half of sample byte

    sty !dmc_working_b1b2

    ;  Send 2 samples
    ldy.w !brr_new_sample_pointer
    %spc_upload_byte()
    sty.w !brr_new_sample_pointer

    dec !brr_loop_counter
    bne F1S6b_notFinished
    jml afterInnerLoops

F1S6b_notFinished:
    rep #$20
    lda !dmc_working_b1b2
    brl F1S6_NextSamples  ;  Next sample


;;  Filter-1, Shift-7 inner loop  ;;
;----------------------------------;
F1S7_BlockCreation:
    tay
    ;  Prep sample index in [X]
    lda #dmcBRR_F0S8_and_F1S7_coeffLookup
    clc
    adc !dmc_running_value
    tax
    tya

F1S7_NextSamples:
    asl
    bcs F1S7_LoadFromBelow

F1S7_LoadFromAbove:
    tay
    txa
    and #$ff7f  ;  Point to low half of sample index table
    tax
    dex     ;  Move to the new working value (previous-1)
    tya     ;  Restore !dmc_working_value_16 to [A]

    ldy $00,x   ; indexed table load
    sty !brr_half_sample    ;  Save first half of sample byte
    asl !brr_half_sample    ;  Bump the data up to the high nibble
    asl !brr_half_sample
    asl !brr_half_sample
    asl !brr_half_sample    ;  (opt: obviously, this is bad)

    ;  Second brr sample unrolled
    asl
    bcs F1S7_LoadFromBelow2
    bra F1S7_LoadFromAbove2  ;  Next sample

F1S7_LoadFromBelow:
    tay
    txa
    ora #$0080  ;  Point to high half of sample index table
    tax
    inx     ;  Move to the new working value (previous+1)
    tya     ;  Restore working [B1][B2]

    ldy $00,x   ; indexed table load
    sty !brr_half_sample    ;  Save first half of sample byte
    asl !brr_half_sample    ;  Bump the data up to the high nibble
    asl !brr_half_sample
    asl !brr_half_sample
    asl !brr_half_sample    ;  (opt: obviously, this is bad)

    ;  Second brr sample unrolled
    asl
    bcs F1S7_LoadFromBelow2

F1S7_LoadFromAbove2:
    tay
    txa
    and #$ff7f  ;  Point to low half of sample index table
    tax
    dex     ;  Move to the new working value (previous-1)

    lda $00,x   ; indexed table load

    sep #$20
    clc             ; (opt: check for consistent carry flag)
    adc !brr_half_sample    ;  Restore first half of sample byte

    sty !dmc_working_b1b2
    
    ;  Send 2 samples
    ldy.w !brr_new_sample_pointer
    %spc_upload_byte()
    sty.w !brr_new_sample_pointer

    dec !brr_loop_counter
    bne F1S7a_notFinished
    jml afterInnerLoops
    
F1S7a_notFinished:
    rep #$20
    lda !dmc_working_b1b2
    bra F1S7_NextSamples  ;  Next sample

F1S7_LoadFromBelow2:
    tay
    txa
    ora #$0080  ;  Point to high half of sample index table
    tax
    inx     ;  Move to the new working value (previous+1)

    lda $00,x   ; indexed table load

    sep #$20
    clc             ; (opt: check for consistent carry flag)
    adc !brr_half_sample    ;  Restore first half of sample byte

    sty !dmc_working_b1b2

    ;  Send 2 samples
    ldy.w !brr_new_sample_pointer
    %spc_upload_byte()
    sty.w !brr_new_sample_pointer

    dec !brr_loop_counter
    bne F1S7b_notFinished
    jml afterInnerLoops

F1S7b_notFinished:
    rep #$20
    lda !dmc_working_b1b2
    brl F1S7_NextSamples  ;  Next sample


afterInnerLoops:
    ;  End check
    rep #$20        ; 16-bit    (opt: needed?)
    dec !dmc_sample_length
    dec !dmc_sample_length
    beq EndConvertDMCtoBRRF1  ;  done with this sample; exit subroutine

    sep #$20    ;  Prep for 8-bit math
    lda #$08
    sta !brr_loop_counter   ;  Reset the inner loop counter

    lda !dmc_change_after_16
    clc
    adc !dmc_running_value
    sta !dmc_running_value  ;  Ready for next 16 samples

    rep #$20
    ldx !dmc_working_value_16   ; [B1][B2]
    inx : inx   ;  Move to next 16 samples
    stx !dmc_working_value_16
    lda $00,x   ;  And load them
    tay         ;  Copy to [Y] for next loop
    brl convertOuterLoop

nooop:
    jmp nooop

EndConvertDMCtoBRRF1:
;  Set !brr_new_sample_pointer to the next valid brr audio ram start location
    lda.w !brr_start_location 
    clc
    adc !brr_new_sample_pointer
    sta !brr_new_sample_pointer
    sep #$20        ; 8-bit
rts



;  [OPT] ideas:
;           - Break %spc_upload_byte() into two, and cram X cycles of actual work in the middle
;             while waiting for spc-700 confirmation.  See if the actual # of cycles is documented.
;           - MVN some or all of the lookup tables into wram (will this reduce cycles?  doubtful)
;           - Skip every nth sample


;-------------------------------------------------------------------------------
;   Description: This short subroutine is a band-aid to catch the one
;                edge case that the conversion algorithm can't handle:
;                if the current brr block is filter 0 and we exceed the range,
;                we have to rollback the pointers and rewrite this block from
;                the start, now that we know the valid shift for this block
;                must be $64.
;   Parameters: none
;   Variables:  
;   Returns:    no value
;-------------------------------------------------------------------------------
resetOutOfBounds:
    rep #$20    ; 16-bit math
    lda !brr_new_sample_pointer     ;  Where in the BRR we were when we broke out of bounds
    sec : sbc #$0009  ;  Loop counter max+1
    clc : adc !brr_loop_counter  ;  Where we were when we broke out of bounds
    clc : adc !brr_start_location  ;  Add the start offset to above to get the aram address
    sta !brr_new_sample_pointer  ;  Sample pointer is now reset to the head of the current block
    sta !brr_start_location      ;  This also has to be updated to the most recent reset for future calls
    tay                          ;  Copy pointer to [Y]

    sep #$20     ;  Prep for 8-bit math

    ;  Re-init audio ram writes
    %spc_begin_upload()
    sty.w !brr_new_sample_pointer   ; (opt: just iny?)

    lda #$08
    sta !brr_loop_counter   ;  Reset the inner loop counter

    lda #$64    ;  The correct shift value for this block
    sta !brr_cur_shift

    ;  Overwrite the incorrect $80 header with the correct $64 header
    ldy.w !brr_new_sample_pointer
    %spc_upload_byte()
    sty.w !brr_new_sample_pointer
rts