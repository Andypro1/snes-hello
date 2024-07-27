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

;-------------------------------------------------------------------------------
;   Description: Loads NES DMC dpcm audio data of length [Y] from address [X].
;       Converts the data to a BRR sample and loads into Voice #2 (TODO: genericize)
;       This routine has no return value (remains $00).
;   Parameters: [X] - 16 bit DMC sample start location
;               [Y] - 16 bit DMC sample length
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
    ldy #$2000
    sty !brr_new_sample_pointer     ; prepping aram location $2000

;   initialize audio ram writes
    ldy.w !brr_new_sample_pointer
    jsr spc_begin_upload
    sty.w !brr_new_sample_pointer

processDcmSample:
;     set indexY register to 8
    ldy #$0008

    lda !brr_isBackBlock
    bne nextDcmBit;          ;  skip brr shift calculation (only done before front block)

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

    lda !dmc_running_value
    cmp #33
    bcc shift8
    lda !dmc_running_value
    cmp #49
    bcc shift7
    lda !dmc_running_value
    cmp #80
    bcc shift6
    lda !dmc_running_value
    cmp #97
    bcc shift7
    lda !dmc_running_value
    cmp #128
    bcc shift8

;     Write new brr header byte
shift6:
    lda #$06
    sta !brr_cur_shift              ;  Store the current shift value for this brr block
    lda #$60

    ldy.w !dmc_sample_length    ;  TODO: make macro
    cpy #$0003
    bcs skipEnd6    ;  If 3 or more bytes left, no BRR end flag
    adc #$01        ;  Else, add end flag
skipEnd6:

    ldy.w !brr_new_sample_pointer
    jsr spc_upload_byte             ;  Write the brr block header byte to audio ram
    sty.w !brr_new_sample_pointer
    bra doneShifting
shift7:
    lda #$07
    sta !brr_cur_shift
    lda #$70

    ldy.w !dmc_sample_length    ;  TODO: make macro
    cpy #$0003
    bcs skipEnd7    ;  If 3 or more bytes left, no BRR end flag
    adc #$01        ;  Else, add end flag
skipEnd7:

    ldy.w !brr_new_sample_pointer
    jsr spc_upload_byte
    sty.w !brr_new_sample_pointer
    bra doneShifting
shift8:
    lda #$08
    sta !brr_cur_shift
    lda #$80

    ldy.w !dmc_sample_length    ;  TODO: make macro
    cpy #$0003
    bcs skipEnd8    ;  If 3 or more bytes left, no BRR end flag
    adc #$01        ;  Else, add end flag
skipEnd8:

    ldy.w !brr_new_sample_pointer
    jsr spc_upload_byte
    sty.w !brr_new_sample_pointer

doneShifting:
    ply     ;  Restore indexY

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
    adc #$02
    sta !dmc_running_value
    bra scaleToOutputRange
sub2:
    lda !dmc_running_value
    cmp #$01
    bcc scaleToOutputRange    ;  Skip subtraction if we are at/below the lower bound $00
    sbc #$02
    sta !dmc_running_value
    bra scaleToOutputRange

scaleToOutputRange:
;     Apply scaling factor to new brr sample.  (7-bit range to 15-bit range; this again depends where the zero-crossing is for dcm)
    rep #$20        ; A 16-bit
    and #$00ff      ; Clear upper byte garbage

    asl #$06        ;  sample<<6  ... FIX this hardcode.
    sec             ;  Set carry flag to avoid off-by-1 issue with the SBC below
    sbc #$1000      ;  Center range on zero-crossing.  Check for proper 2s complement for values under $40
    ;sta !brr_cur_scaled_value
    ;lsr !brr_cur_scaled_value      ;  fix it)

    phx             ;  Preserve indexX
    ldx !brr_cur_shift
    cpx #$0008
    beq unshift8
    cpx #$0007
    beq unshift7
    cpx #$0006
    beq unshift6
    jmp doneUnshifting

unshift6:
    lsr : lsr : lsr : lsr : lsr : lsr
    jmp doneUnshifting
unshift7:
    lsr : lsr : lsr : lsr : lsr : lsr : lsr
    jmp doneUnshifting
unshift8:
    lsr : lsr : lsr : lsr : lsr : lsr : lsr : lsr
doneUnshifting:
    plx         ;  Restore indexX

    sep #$20        ; back to A 8-bit

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
        sep #$20        ; 8-bit
        rts                     ; return to caller