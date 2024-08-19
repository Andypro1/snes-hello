if not(defined("select_brr_shift"))
    !select_brr_shift = 1

!sbs_minimum = $0315
!sbs_maximum = $0317

;-------------------------------------------------------------------------------
;   Description: Selects the appropriate BRR block shift value
;       given the current waveform value in [X] and the next two DCM bytes in [A].
;       Requires [A] to be in 16-bit mode.
;   Parameters: [A] - 16 bit, next two DMC sample bytes (16 delta samples)
;               [X] - Current 7-bit waveform value
;   Variables:  Stack bytes to store maximum and minimum
;   Returns:    BRR shift value in [A] ($00 to $0a)
;-------------------------------------------------------------------------------
SelectBRRShift:
    ;  Initialize maximum and minimum values at initial value
    stx !sbs_minimum
    stx !sbs_maximum

    ;  Initialize loop counter in [Y]
    ldy #$0010

selLoop:
    asl
    bcs selectInc
selectDec:
    dex : dex       ;  Change to delta of 2
    bra selectDone
selectInc:
    inx : inx       ;  Change to delta of 2

selectDone:
    cpx !sbs_minimum
    bcc setMinimum
    cpx !sbs_maximum
    bcs setMaximum
    bra setDone

setMinimum:
    stx !sbs_minimum
    bra setDone
setMaximum:
    stx !sbs_maximum
setDone:
    dey
    beq allDone
    bra selLoop

allDone:
    lda !sbs_maximum
;   if max > 5f, b
    cmp #$0060
    bcs retB
;   if max > 4f, a
    cmp #$0050
    bcs retA
;   if max > 47, 9
    cmp #$0048
    bcs ret9
;   if max > 43, 8 OR 9
    cmp #$0044
    bcs sel8or9

;  [shift 7 not needed]
; ;   if max > 41, 7 OR 8 OR 9
;     cmp #$0042
;     bcs sel7or8or9

    lda !sbs_minimum
;   if min < 20,    b
    cmp #$0020
    bcc retB
;   if min < 30,    a
    cmp #$0030
    bcc retA
;   if min < 38,    9
    cmp #$0038          ;  CHANGED typo to 38
    bcc ret9
;   if min < 3d,    8
    cmp #$003d
    bcc ret8

;  [shifts 7 and 6 not needed]
; ;   if min < 3f,    7
;     cmp #$003f
;     bcc ret7
; ;   if min < 42,    6
;     cmp #$0042
;     bcc ret6

sel8or9:
    lda !sbs_minimum
;       if min > 38, 8
    cmp #$0038
    bcs ret8                ;  CHANGED typo to bcs
;       else         9
    bra ret9

;  [shift7 not needed]
; sel7or8or9:
;     lda !sbs_minimum
; ;       if min > 3c, 7
;     cmp #$003c
;     bcc ret7
; ;       if min > 39, 8
;     cmp #$0039
;     bcc ret8
; ;       else         9
;     bra ret9

;   else        BRK.
    brk     ;  SOMETHING WENT WRONGGGGGGGGGGG

; [shifts 6 and 7 not needed]
; ret6:
;     lda #$0060
;     bra goReturn
; ret7:
;     lda #$0070
;     bra goReturn
ret8:
    lda #$0080
    bra goReturn
ret9:
    lda #$0090
    bra goReturn
retA:
    lda #$00a0
    bra goReturn
retB:
    lda #$00b0

goReturn:
;  all 51 possible (positive, filter-0) sample values:
;  0    1   2   3   4   5   6   7   8       9       a       b
;   1	2	4	8	16	32	64	128	256	    512	    1024	2048
;   2	4	8	16	32	64	128	256	512	    1024	2048	4096
;   3	6	12	24	48	96	192	384	768	    1536	3072	6144								        	    	    
;  4	8	16	32	64	128	256	512	1024	2048	4096	8192
;  5	10	20	40	80	160	320	640	1280	2560	5120	10240
;  6	12	24	48	96	192	384	768	1536	3072	6144	12288
;  7	14	28	56	112	224	448	896	1792	3584	7168	14336

; consider:
;   $40 $41 $42 $42 $44 $45 $46 $47 $48 $49 $4a $4b $4c $4d $4e $4f
;   0     0   1   1   2   2   3   3   4   4   5   5   6   6   7   7
;   $40 $3f $3e $3d $3c $3b $3a $39 $38 $37 $36 $35 $34 $33 $32 $31
;   0    -1  -1  -2  -2  -3  -3  -4  -4  -5  -5  -6  -6  -7  -7  -8

;  distribution will be different for every shift

;  possible lookup table organization:

;  shift8:
;  f8 8  f9 9  fa a  fb b  fc c  fd d  fe e  ff f
;  00 0  01 1  02 2  03 3  04 4  05 5  06 6  07 7
;  shortcut: "return the low nibble of input-$40."

;  shift9:
;  f0 8 f1 8  f2 9  f3 9  f4 a  f5 a  f6 b  f7 b  f8 c  f9 c  fa d  fb d  fc e  fd e  fe f  ff f
;  shortcut:  
;  zero values (one):        0
;  positive values (seven):  1 2 3 4 5 6 7
;  negative values (eight):  f e d c b a 9 8
;  
;  shift8: =floor(-8+((value(hex2dec(V)) - value(hex2dec(38)))/1))  8:38
;  shift9: =floor(-8+((value(hex2dec(V)) - value(hex2dec(30)))/2))  9:30
;  shiftA: =floor(-8+((value(hex2dec(V)) - value(hex2dec(20)))/4))  A:20
;  shiftB: =floor(-8+((value(hex2dec(V)) - value(hex2dec(0)))/8))    B:0

; 8:1, 9:2, A:4, B:8
; 8>>0, 9>>1, A>>2, B>>3

;  general case shift:
;   floor(-8 + (V - SV)>>(S-8))
;   where V is incoming value
;         SV is "shift starting number"  (s8: 38, s9: 30, sA: 20, sB: 0)
;         S is shift number

;  case:
;      min >= 3f && max <= 41       6   3 values
;      min >= 3d && max <= 43       7   7 values
;      min >= 39 && max <= 47       8   15 values
;      min >= 30 && max <= 4f       9   31 values
;      min >= 20 && max <= 5f       a   63 values
;      min >=  0 && max <= 7f       b   127 values

;   if max > 5f, b
;   if max > 4f, a
;   if max > 47, 9
;   if max > 43, 8 OR 9
;       if min > 38, 8
;       else         9
;   if max > 41, 7 OR 8 OR 9
;       if min > 3c, 7
;       if min > 39, 8
;       else         9
;   if min < 20,    b
;   if min < 30,    a
;   if min < 39,    9
;   if min < 3d,    8
;   if min < 3f,    7
;   if min < 42,    6
;   else        BRK.
rts

endif