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
    lda #$0040
    sta !dmc_running_value

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
    cmp $00,s
    bcs writeBRRHeaderByte     ;  If [S1] >= [S2]

;   (8-bit) Write header byte to spc
    pla     ;  Choose [S2] and return it to [A]
    sta !brr_cur_shift  ; [todo: opt out]
    ldy.w !brr_new_sample_pointer
    %spc_upload_byte()
    sty.w !brr_new_sample_pointer
    bra afterHeaderWritten

writeBRRHeaderByte:
    sta !brr_cur_shift  ; [todo: opt out]
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
    cpx #$0080    ;  Filter 0, Shift 8 table with a table offset of $38
    beq F0S8_BlockCreation
    cpx #$0064
    ; TODO:  subroutines?
    ; beq F1S6_BlockCreation
    ; cpx #$0074
    ; beq F1S7_BlockCreation

;  Inner loops TODO:s
;   - Add a way to break out of the loops after 16 iterations (dec a zero-page counter and then beq ?)

F1S8_BlockCreation:
    ;  Prep sample index in [X]
    ldx #dmcBRR_F1S6_and_F1S8_coeffLookup   ;  offset $00 - no change needed

    asl
    bcs F1S8_LoadFromBelow

F1S8_LoadFromAbove:
    tay
    txa
    and #$ff7f  ;  Point to low half of sample index table
    tax
    dex     ;  Move to the new working value (previous-1)
    tya     ;  Restore !dmc_working_value_16 to [A]
    bra F1S8_BlockCreation  ;  Next sample

F1S8_LoadFromBelow:
    tay
    txa
    ora #$0080  ;  Point to high half of sample index table
    tax
    inx     ;  Move to the new working value (previous+1)
    tya     ;  Restore !dmc_working_value_16 to [A]
    bra F1S8_BlockCreation  ;  Next sample


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

    sep #$20
    lda $00,x   ; indexed table load
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

    sep #$20
    lda $00,x   ; indexed table load
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


F1S6_BlockCreation:
    tay
    ;  Prep sample index in [X]
    lda #dmcBRR_F1S6_and_F1S8_coeffLookup
    adc #$0020  ;  Add offset $20 for the correct table block
    tax

    tya
    asl
    bcs F1S6_LoadFromBelow

F1S6_LoadFromAbove:
    tay
    txa
    and #$ff7f  ;  Point to low half of sample index table
    tax
    dex     ;  Move to the new working value (previous-1)
    tya     ;  Restore !dmc_working_value_16 to [A]
    bra F1S6_BlockCreation  ;  Next sample

F1S6_LoadFromBelow:
    tay
    txa
    ora #$0080  ;  Point to high half of sample index table
    tax
    inx     ;  Move to the new working value (previous+1)
    tya     ;  Restore !dmc_working_value_16 to [A]
    bra F1S6_BlockCreation  ;  Next sample


F1S7_BlockCreation:
    ;  Prep sample index in [X]
    ldx #dmcBRR_F0S8_and_F1S7_coeffLookup   ;  offset $00 - no change needed

    asl
    bcs F1S7_LoadFromBelow

F1S7_LoadFromAbove:
    tay
    txa
    and #$ff7f  ;  Point to low half of sample index table
    tax
    dex     ;  Move to the new working value (previous-1)
    tya     ;  Restore !dmc_working_value_16 to [A]
    bra F1S7_BlockCreation  ;  Next sample

F1S7_LoadFromBelow:
    tay
    txa
    ora #$0080  ;  Point to high half of sample index table
    tax
    inx     ;  Move to the new working value (previous+1)
    tya     ;  Restore !dmc_working_value_16 to [A]
    bra F1S7_BlockCreation  ;  Next sample


afterInnerLoops:
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
    brl convertOuterLoop

nooop:
    jmp nooop

rts



;  [OPT] ideas:
;           - MVN some or all of the lookup tables into wram (will this reduce cycles?  doubtful)
;           - Skip every nth sample