; High-level interface to SPC-700 bootloader
;
; 1. Call spc_wait_boot
; 2. To upload data:
;       A. Call spc_begin_upload
;       B. Call spc_upload_byte any number of times
;       C. Go back to A to upload to different addr
; 3. To begin execution, call spc_execute
;
; Have your SPC code jump to $FFC0 to re-run bootloader.
; Be sure to call spc_wait_boot after that.


; Waits for SPC to finish booting. Call before first
; using SPC or after bootrom has been re-run.
; Preserved: X, Y
spc_wait_boot:
    lda #$AA
waitAck1Boot:  cmp $2140
    bne waitAck1Boot

    ; Clear in case it already has $CC in it
    ; (this actually occurred in testing)
    sta $2140

    lda #$BB
waitBootAck2: cmp $2141
    bne waitBootAck2

    rts


; Starts upload to SPC addr Y and sets Y to
; 0 for use as index with spc_upload_byte.
; Preserved: X
spc_begin_upload:
    sty $2142

    ; Send command
    lda $2140
    clc
    adc #$22
    bne skip       ; special case fully verified
    inc
skip:  sta $2141
    sta $2140

    ; Wait for acknowledgement
waitUploadStartAck:  cmp $2140
    bne waitUploadStartAck

    ; Initialize index
    ldy #$0000

    rts


; Uploads byte A to SPC and increments Y. The low byte
; of Y must not changed between calls.
; Preserved: X
spc_upload_byte:
    sta $2141

    ; Signal that it's ready
    tya
    sta $2140
    iny

    ; Wait for acknowledgement
waitUploadByteAck:
    cmp $2140
    bne waitUploadByteAck

    rts


; Starts executing at SPC addr Y
; Preserved: X, Y
spc_execute:
    sty $2142

    stz $2141

    lda $2140
    clc
    adc #$22
    sta $2140

    ; Wait for acknowledgement
wait:  cmp $2140
    bne wait

    rts