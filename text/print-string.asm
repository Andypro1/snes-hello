;-------------------------------------------------------------------------------
;   Description: Prints string data immediately following the subroutine call
;       to VRAM memory position in register X.
;       This routine has no return value (remains $00).
;   Parameters: [X] - 16 bit VRAM location to write to
;               <byte after the jsr call> - the string to write
;   Returns:    no value
;-------------------------------------------------------------------------------
WriteString:
        stx !VMADDL ;  Prepare VRAM write at specified [X]

        plx : inx   ;  Pull sub return and increment to get string to write

        string_loop:
        lda 0,x   ;  initial loop param - start of string to write
        beq Return
        sta !VMDATAL
        lda #$20 ; priority 1
        sta !VMDATAH
        inx
        bra string_loop

Return:
        phx                     ; push end of string pointer as return location
        rts                     ; return to caller