;  Define pallettes for the four mode-0 BG layers (four text colors)
;  TODO: use DMA to efficiently store these sequential values... later

;  Pallette 0: Unselected menu options
    stz !CGADD      ; BG0
    stz !CGDATA     ;  transparent
    stz !CGDATA

    stz !CGDATA     ;  black
    stz !CGDATA

    lda #%11100111  ;  dark gray
    sta !CGDATA
    lda #%00011100
    sta !CGDATA

    lda #%11100111  ;  dark gray
    sta !CGDATA
    lda #%00011100
    sta !CGDATA

    stz !CGDATA     ;  transparent
    stz !CGDATA

    stz !CGDATA     ;  black
    stz !CGDATA

    lda #%11100111  ;  dark gray
    sta !CGDATA
    lda #%00011100
    sta !CGDATA

    lda #%11100111  ;  dark gray
    sta !CGDATA
    lda #%00011100
    sta !CGDATA

    stz !CGDATA     ;  transparent
    stz !CGDATA

    stz !CGDATA     ;  black
    stz !CGDATA

    lda #%11100111  ;  dark gray
    sta !CGDATA
    lda #%00011100
    sta !CGDATA

    lda #%11100111  ;  dark gray
    sta !CGDATA
    lda #%00011100
    sta !CGDATA

    stz !CGDATA     ;  transparent
    stz !CGDATA

    stz !CGDATA     ;  black
    stz !CGDATA

    lda #%11100111  ;  dark gray
    sta !CGDATA
    lda #%00011100
    sta !CGDATA

    lda #%11100111  ;  dark gray
    sta !CGDATA
    lda #%00011100
    sta !CGDATA

    stz !CGDATA     ;  transparent
    stz !CGDATA

    stz !CGDATA     ;  black
    stz !CGDATA

    lda #%11100111  ;  dark gray
    sta !CGDATA
    lda #%00011100
    sta !CGDATA

    lda #%11100111  ;  dark gray
    sta !CGDATA
    lda #%00011100
    sta !CGDATA

    stz !CGDATA     ;  transparent
    stz !CGDATA

    stz !CGDATA     ;  black
    stz !CGDATA

    lda #%11100111  ;  dark gray
    sta !CGDATA
    lda #%00011100
    sta !CGDATA

    lda #%11100111  ;  dark gray
    sta !CGDATA
    lda #%00011100
    sta !CGDATA

    stz !CGDATA     ;  transparent
    stz !CGDATA

    stz !CGDATA     ;  black
    stz !CGDATA

    lda #%11100111  ;  dark gray
    sta !CGDATA
    lda #%00011100
    sta !CGDATA

    lda #%11100111  ;  dark gray
    sta !CGDATA
    lda #%00011100
    sta !CGDATA

    stz !CGDATA     ;  transparent
    stz !CGDATA

    stz !CGDATA     ;  black
    stz !CGDATA

    lda #%11100111  ;  dark gray
    sta !CGDATA
    lda #%00011100
    sta !CGDATA

    lda #%11100111  ;  dark gray
    sta !CGDATA
    lda #%00011100
    sta !CGDATA

;  Pallette 1: Selected menu option
    ; lda #$01
    ; sta !CGADD  ; BG1

    stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%11111111  ;  white
    sta !CGDATA
    lda #%01111111
    sta !CGDATA

    lda #%11111111  ;  white
    sta !CGDATA
    lda #%01111111
    sta !CGDATA

    stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%11111111  ;  white
    sta !CGDATA
    lda #%01111111
    sta !CGDATA

    lda #%11111111  ;  white
    sta !CGDATA
    lda #%01111111
    sta !CGDATA

        stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%11111111  ;  white
    sta !CGDATA
    lda #%01111111
    sta !CGDATA

    lda #%11111111  ;  white
    sta !CGDATA
    lda #%01111111
    sta !CGDATA

        stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%11111111  ;  white
    sta !CGDATA
    lda #%01111111
    sta !CGDATA

    lda #%11111111  ;  white
    sta !CGDATA
    lda #%01111111
    sta !CGDATA

        stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%11111111  ;  white
    sta !CGDATA
    lda #%01111111
    sta !CGDATA

    lda #%11111111  ;  white
    sta !CGDATA
    lda #%01111111
    sta !CGDATA

        stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%11111111  ;  white
    sta !CGDATA
    lda #%01111111
    sta !CGDATA

    lda #%11111111  ;  white
    sta !CGDATA
    lda #%01111111
    sta !CGDATA

        stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%11111111  ;  white
    sta !CGDATA
    lda #%01111111
    sta !CGDATA

    lda #%11111111  ;  white
    sta !CGDATA
    lda #%01111111
    sta !CGDATA

        stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%11111111  ;  white
    sta !CGDATA
    lda #%01111111
    sta !CGDATA

    lda #%11111111  ;  white
    sta !CGDATA
    lda #%01111111
    sta !CGDATA

;  Pallette 2: Title text
    ; lda #$02
    ; sta !CGADD  ; BG2

    stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%11111111  ;  gold
    sta !CGDATA
    lda #%00101010
    sta !CGDATA

    lda #%11111111  ;  gold
    sta !CGDATA
    lda #%00101010
    sta !CGDATA

    stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%11111111  ;  gold
    sta !CGDATA
    lda #%00101010
    sta !CGDATA

    lda #%11111111  ;  gold
    sta !CGDATA
    lda #%00101010
    sta !CGDATA

        stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%11111111  ;  gold
    sta !CGDATA
    lda #%00101010
    sta !CGDATA

    lda #%11111111  ;  gold
    sta !CGDATA
    lda #%00101010
    sta !CGDATA

        stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%11111111  ;  gold
    sta !CGDATA
    lda #%00101010
    sta !CGDATA

    lda #%11111111  ;  gold
    sta !CGDATA
    lda #%00101010
    sta !CGDATA

        stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%11111111  ;  gold
    sta !CGDATA
    lda #%00101010
    sta !CGDATA

    lda #%11111111  ;  gold
    sta !CGDATA
    lda #%00101010
    sta !CGDATA

        stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%11111111  ;  gold
    sta !CGDATA
    lda #%00101010
    sta !CGDATA

    lda #%11111111  ;  gold
    sta !CGDATA
    lda #%00101010
    sta !CGDATA

        stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%11111111  ;  gold
    sta !CGDATA
    lda #%00101010
    sta !CGDATA

    lda #%11111111  ;  gold
    sta !CGDATA
    lda #%00101010
    sta !CGDATA

        stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%11111111  ;  gold
    sta !CGDATA
    lda #%00101010
    sta !CGDATA

    lda #%11111111  ;  gold
    sta !CGDATA
    lda #%00101010
    sta !CGDATA

;  Pallette 3: Active (playing) menu item
    ; lda #$04
    ; sta !CGADD  ; BG3

    stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%00011111  ;  red
    sta !CGDATA
    lda #%00000000
    sta !CGDATA

    lda #%00011111  ;  red
    sta !CGDATA
    lda #%00000000
    sta !CGDATA

        stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%00011111  ;  red
    sta !CGDATA
    lda #%00000000
    sta !CGDATA

    lda #%00011111  ;  red
    sta !CGDATA
    lda #%00000000
    sta !CGDATA

        stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%00011111  ;  red
    sta !CGDATA
    lda #%00000000
    sta !CGDATA

    lda #%00011111  ;  red
    sta !CGDATA
    lda #%00000000
    sta !CGDATA

        stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%00011111  ;  red
    sta !CGDATA
    lda #%00000000
    sta !CGDATA

    lda #%00011111  ;  red
    sta !CGDATA
    lda #%00000000
    sta !CGDATA

        stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%00011111  ;  red
    sta !CGDATA
    lda #%00000000
    sta !CGDATA

    lda #%00011111  ;  red
    sta !CGDATA
    lda #%00000000
    sta !CGDATA

        stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%00011111  ;  red
    sta !CGDATA
    lda #%00000000
    sta !CGDATA

    lda #%00011111  ;  red
    sta !CGDATA
    lda #%00000000
    sta !CGDATA

        stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%00011111  ;  red
    sta !CGDATA
    lda #%00000000
    sta !CGDATA

    lda #%00011111  ;  red
    sta !CGDATA
    lda #%00000000
    sta !CGDATA

        stz !CGDATA ;  transparent
    stz !CGDATA

    stz !CGDATA ;  black
    stz !CGDATA

    lda #%00011111  ;  red
    sta !CGDATA
    lda #%00000000
    sta !CGDATA

    lda #%00011111  ;  red
    sta !CGDATA
    lda #%00000000
    sta !CGDATA