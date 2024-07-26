macro playsound(voice, pitchhigh, pitchlow)
    !playbit #= <voice>+1
    !konvoice := #$0!playbit
    !konvoice += 4C

    ldx #<pitchhigh><voice>3    ;PITCHHIGH (voice 0)
    jsr write_dsp
    ldx #<pitchlow><voice>2    ;PITCHLOW (voice 0)
    jsr write_dsp
    ldx !konvoice
    jsr write_dsp
endmacro