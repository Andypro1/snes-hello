   !dmc_running_value = $0309
   !brr_first_last = $030a    ;  TODO: use better boolean name.  0 or 1 
                              ;  depending on whether we're working on an even sample
                              ;  nibble (0..14) or an odd one (1..15)
   !dmc_sample_length = $030b ; 16-bit length value
   !brr_new_sample_pointer = $030d  ;  16-bit spc-700 address for storing the new brr sample
   !brr_cur_shift = $030f   ;  16-bit current brr block's shift value
;    !brr_cur_upload_index = $0311    ;  16-bit Y register index required preserved by spc_upload_byte
;    !brr_cur_scaled_value = $0313    ;  Working memory for computing the scaled value of the current sample
   !dmc_working_value = $0311   ;  Area for the current dmc byte being worked on
   !brr_high_nibble = $0312    ;  Storage for the high sample while we calculate the low sample
   !brr_scaled_value = $0313    ;  Temporary storage for the scaled value
   !brr_isBackBlock = $0314    ;  1 if we're on Samples IIII->PPPP of the BRR block, 0 otherwise
   !cdb_SV = $0319          ; The calculated SV value based on the brr shift
   !cdb_VminusSV = $0320
   !cdb_Sminus8 = $0321
   !brr_start_location = $0322  ;  The initial audio ram write location


    macro asr(howManyShifts)
        !a #= 0

        while !a < <howManyShifts>
            cmp #$8000
            ror
        endif
    endmacro

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

; algorithm for decoding:

;     start the running sample value at either $00 or $40?,
    lda #$40        ;  Experiment with the starting value (represents dmc wave zero-crossing)
    sta !dmc_running_value

;     start the brrFirstLast bit at 1 (first 8 samples)
    lda #$01
    sta !brr_first_last

    ;  Start with the front of a BRR block
    stz !brr_isBackBlock

;     set indexX register to dcm sample start location
    ;  Already done via param [X]

;     set start write location in audio ram
    rep #$20 : ply : tya : ply  ;  Grab the stack-top aram parameter
    sty !brr_start_location         ; saving original aram start location
    sty !brr_new_sample_pointer     ; prepping aram location
    pha : sep #$20  ;  Restore stack and reset to 8-bit

;   initialize audio ram writes
    ldy.w !brr_new_sample_pointer
    jsr spc_begin_upload
    sty.w !brr_new_sample_pointer

processDcmSample:
;     set indexY register to 8
    ldy #$0008

    lda !brr_isBackBlock
    bne loadNewDcmByte;          ;  skip brr shift calculation (only done before front block)

calcBrrShiftVal:
;     Use running sample value to calculate brr shift nibble (lookup table?)

;scaled_value= 64×32767   /  127
; ≈16409
;  answer = S * 1<<15  /  1<<7
;  answer = S * 1<<8
;  answer = S<<8
;  now divide by two to bring the unsigned range into the signed range:
;   answer = S<<7  ; no, won't work because we need to take the FULL 15 bit range and
;       subtract the zero crossing instead of dividing values.
;  So,
;  answer = S<<8 - !zero-crossing-in-15-bit-land (16,384)

    ;  brr shift nibble calculation
    ;  Max incoming value of $7f<<8 - $4000 (16,384) = $3f00 (16,128)
    ;  shift of %1010 (a) is max shift that loses least resolution and still fits sample nibble of $F

    ;  Min incoming value of $00<<8 - $4000 (16,384) = $c000 (-16,384)
    ;  shift of %1100 (c) or %1101 (d) or %1110 (e) all work for this case

    ;  Find equation which represents this problem:
    ;  Solve for the shift value S in the following equation.
    ;  (I << 8 - $4000) >> S = O
    ;  where 0 <= S <= 0x0c
    ;  where 0 <= I <= 0x7f
    ;  and where $c006 <= O <= $3ff8

    ;  Sleep on it, then maybe make a GSheet which calculates the error for all possible combinations

    ;  Future safety step could be:
    ;       Use not only the !dmc_running_value to calculate the proper shift,
    ;       but also add up the bits in the incoming 8 samples to see where we might end up.
    ;       e.g.:  !dmc_running_value = $3f , incoming byte = $ff = %1111 1111
    ;       The running value will increase to $4f.  Then can choose the optimal value from
    ;       a lookup table.
    ;  BETTER YET:  Do this for the next full word of data, which will give you bounds
    ;  for the entire BRR block.

    ;  Mapping from a 7-bit to a 13-bit range:  -$1000..+$0fc0
    ;  Mapping table:
    ;       input range:    0:32    33:48   49:79   80:96   97:127
    ;       shift:          8       7       6       7       8

    phy     ;  Preserve indexY
    phx     ;  Preserve indexX
    ;   Parameters: [A] - 16 bit, next two DMC sample bytes (16 delta samples)
;               [X] - Current 7-bit waveform value
;   Variables:  Stack bytes to store maximum and minimum
;   Returns:    BRR shift value in [A] ($00 to $0a)
;-------------------------------------------------------------------------------
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
    adc #$01        ;  Else, add end flag
skipEndFlag:

    ldy.w !brr_new_sample_pointer
    jsr spc_upload_byte             ;  Write the brr block header byte to audio ram
    sty.w !brr_new_sample_pointer
    ;  done with shifting

    plx     ;  Restore indexX
    ply     ;  Restore indexY

loadNewDcmByte:
    ;  Begin working on the waveform value
    lda $00,x  ;  Fetch current dcm byte
    sta !dmc_working_value

nextDcmBit:
;     then mask lsb.  If 1, new sample == old+2;  If 0, new sample == old-2.
;       Floor check.  If sample has underflowed, new sample = old;
;       Ceiling check.  If sample > $7f, new sample = old;
    and #$80    ;  Bitmask the msb, because we're shifting the byte left in this loop.
                ;  The bit in question will always be in the msb.
    cmp #$80
    beq add2
    bra sub2

add2:
    lda !dmc_running_value
    cmp #$7f
    beq scaleToOutputRange    ;  Skip addition if we are at the upper bound $7f
    adc #$01 ; TESTING 1 #$02
    sta !dmc_running_value
    bra scaleToOutputRange
sub2:
    lda !dmc_running_value
    cmp #$01
    bcc scaleToOutputRange    ;  Skip subtraction if we are at/below the lower bound $00
    sbc #$01 ; TESTING 1 #$02
    sta !dmc_running_value
    bra scaleToOutputRange

scaleToOutputRange:
    phx             ;  Preserve indexX

    ;  general case shift:
    ;   floor(-8 + (V - SV)>>(S-8))
    ;   where V is incoming value
    ;         SV is "shift starting number"  (s8: 38, s9: 30, sA: 20, sB: 0)
    ;         S is shift number
    ;         SV pattern:  %00a9 8000
    ;                         98
    ;                         8
    ;         each shift lower than b shifts a 1 into bit 6, then 5, then 4, etc.

    ;  Calculate SV value
    lda #$00
    ldx !brr_cur_shift
    cpx #$00b0
    beq doneSVcalc
    cpx #$00a0
    beq SVaddA
    cpx #$0090
    beq SVadd9
    cpx #$0080
    beq SVadd8
    brk         ;  SOMETHING WENT WRONG

SVadd8:
    ora #$08
SVadd9:
    ora #$10
SVaddA:
    ora #$20
doneSVcalc:
    sta !cdb_SV     ;  Save calculated SV

;  (V - SV) part
    lda !dmc_running_value
    sec
    sbc !cdb_SV     ;  UNDERFLOW ERROR HERE(?).  at SPC mem $266e
                    ;  [dmc_running_value was $36, and $38 was subtracted (shift $80) resulting in [A]=$fe]
    sta !cdb_VminusSV   ;  Save calculated (V - SV)

;  >>(S-8)
    lda !brr_cur_shift
    sec
    sbc #$80
    sta !cdb_Sminus8

;  (V - SV)>>(S-8)
    ;  [X] still has !brr_cur_shift here;
    ;  use it instead of [A] so we can pre-store [A] with !cdb_VminusSV
    lda !cdb_VminusSV

    cpx #$00b0
    beq shift3
    cpx #$00a0
    beq shift2
    cpx #$0090
    beq shift1
    cpx #$0080
    beq doneShifting
    brk         ;  SOMETHING WENT WRONG

shift3:
    cmp #$80
    ror
shift2:
    cmp #$80
    ror
shift1:
    cmp #$80
    ror

doneShifting:
;  (-8 + (V - SV)>>(S-8))
    sec
    sbc #$08
    sta !brr_scaled_value     ;  Preserve new brr value

;     ldx !brr_cur_shift
;     cpx #$00b0
;     beq doshiftB
;     cpx #$00a0
;     beq doshiftA
;     cpx #$0090
;     beq doshift9
;     cpx #$0080
;     beq doshift8
;     cpx #$0070
;     beq doshift7
;     cpx #$0060
;     beq doshift6
;     brk         ;  SOMETHING WENT WRONG AGAIN
;     jmp doneUnshifting

; doshiftB:
;     asl
; doshiftA:
;     asl
; doshift9:
;     asl
; doshift8:
;     asl
; doshift7:
;     asl
; doshift6:
;     asl : asl : asl : asl : asl : asl

;     sec             ;  Set carry flag to avoid off-by-1 issue with the SBC below
;     sbc #$2000      ;  Center range on zero-crossing.

;     ; ldx !brr_cur_shift
;     cpx #$00b0
;     beq unshiftB
;     cpx #$00a0
;     beq unshiftA
;     cpx #$0090
;     beq unshift9
;     cpx #$0080
;     beq unshift8
;     cpx #$0070
;     beq unshift7
;     cpx #$0060
;     beq unshift6
;     brk         ;  SOMETHING WENT WRONG AGAIN x2
;     jmp doneUnshifting

; unshiftB:
;     ; %asr(1)
;                 cmp #$8000
;             ror
; unshiftA:
;     ; %asr(1)
;                 cmp #$8000
;             ror
; unshift9:
;     ; %asr(1)
;                 cmp #$8000
;             ror
; unshift8:
;     ; %asr(1)
;                 cmp #$8000
;             ror
; unshift7:
;     ; %asr(1)
;                 cmp #$8000
;             ror
; unshift6:
;     ; %asr(6)
;                 cmp #$8000
;             ror
;                         cmp #$8000
;             ror
;                         cmp #$8000
;             ror
;                         cmp #$8000
;             ror
;                         cmp #$8000
;             ror
;                         cmp #$8000
;             ror

; doneUnshifting:
    plx             ;  Restore indexX
    ; sep #$20        ; back to A 8-bit

    and #$0f                  ;  Scale value down to 1 nibble.  Negative values
                              ;  will have data in the high nibble due to 2s complement
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
    asl #$04       ;  Bump the data up to the high nibble
    clc
    adc !brr_scaled_value       ;  Add in the low nibble data

    ldy.w !brr_new_sample_pointer
    jsr spc_upload_byte
    sty.w !brr_new_sample_pointer

    ply     ;  Restore indexY

    ;  Prepare the next loop iteration
prepNextLoop:
    ;     toggle brrFirstLast bit
    lda !brr_first_last
    eor #$01        ;  Toggle 0->1 and 1->0
    sta !brr_first_last

;     Shift dpcm sample left (working from msb to lsb)
    lda !dmc_working_value  ;  Fetch current dcm "working shift" byte
    asl     ;  Shift it MORE
    sta !dmc_working_value  ;  Save new "working shift"

;     decrement indexY register
    dey

;     if indexY register > 0
    cpy #$0000
;     Loop to nextDcmBit
    beq prepNextDmcByte     ; TODO: Fix loop length so a brl
    brl nextDcmBit          ;       is not required here.

prepNextDmcByte:
;     increment indexX register   ; next dcm sample byte
    inx
    
    rep #$20        ; 16-bit
    dec !dmc_sample_length  ;  does this require 16-bit mode?
    beq EndConvertDMCtoBRR  ;  done with this sample; exit subroutine
    sep #$20        ; 8-bit

    ;     toggle BRR block half
    lda !brr_isBackBlock
    eor #$01            ;  Toggle 0->1 and 1->0
    sta !brr_isBackBlock

    jmp processDcmSample
;     loop to processDcmSample

EndConvertDMCtoBRR:
;  Set !brr_new_sample_pointer to the next valid brr audio ram start location
    lda.w !brr_start_location 
    clc
    adc !brr_new_sample_pointer
    sta !brr_new_sample_pointer
    sep #$20        ; 8-bit
rts