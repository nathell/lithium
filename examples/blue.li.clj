(mov :ax 0x13)
(int 0x10)
(mov :ax 0xa000)
(mov :es :ax)
(xor :di :di)
(mov :al 1)
(mov :cx 64000)
:loop
(stosb)
(loop :loop)
:forever
(jmp :forever)
