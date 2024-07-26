; Writes high byte of X to SPC-700 DSP register in low byte of X
write_dsp:
    phx
    ; Just do a two-byte upload to $00F2-$00F3, so we
    ; set the DSP address, then write the byte into that.
    ldy #$00F2
    jsr spc_begin_upload
    pla
    jsr spc_upload_byte     ; low byte of X to $F2
    pla
    jsr spc_upload_byte     ; high byte of X to $F3
rts