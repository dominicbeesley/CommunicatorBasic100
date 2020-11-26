; Target assembler: cc65 v2.18.0 [--target none -C ARITH100.bin_cc65.cfg]
; 6502bench SourceGen v1.7.3
                .setcpu "65816"
_HDMMT          =       2                               ;Task Memory Pool Handle
_MMASA          =       $0a                             ;Allocate Small Ascending
COP_0F_OPERR    =       $0f                             ;Throw an error
COP_10_OPADP    =       $10                             ;Allocate Direct Page, followed by 2-byte inline size

DP_ART_PtrA     =       $06
DP_ART_INTA     =       $34
DP_ART_FPA      =       $38
DP_43_QRY       =       $43                             ;Used to store INT?
DP_ART_FPB      =       $45
DP_47_QRY       =       $47                             ;INT wkspa?
DP_ART_FPTMP    =       $4e
DP_ART_PtrC     =       $55
DP_ART_PtrB     =       $6a
DP_70_QRY       =       $70
DP_75_QRY       =       $75
DP_7A_QRY       =       $7a
DP_7F_QRY       =       $7f
DP_ART_WKSPptr  =       $84                             ;Pointer to Private Workspace
_MM             =       $ff8c                           ;Memory Management

;                .segment "SEG000"
                .org    $af00
                .a16
                .i16
                brl     jmpServ

                .byte   $93
                .byte   $12
                .byte   $00
                .byte   $00
                .byte   $01
                .byte   $02
                .res    5,$00
                .byte   "Arithmetic"
                .byte   $00
                .byte   $00

jmpServ:        brl     ServiceHandler

fpConst_MinPiDiv2:
                .byte   $81,$c9,$10,$00,$00             ;-PI/2
fpConst_4_454e_6:
                .byte   $6f,$15,$77,$7a,$61             ;4.4544551105e-06
fpConst_PIdiv2: .byte   $81,$49,$0f,$da,$a2
fpConst_2DivPi: .byte   $80,$22,$f9,$83,$6e             ;2/PI = 0.6366
fpConst_Deg2Rad:
                .byte   $7b,$0e,$fa,$35,$12             ;1.74E-2 - 1 deg in rads
fpConst_Rad2Deg:
                .byte   $86,$65,$2e,$e0,$d3             ;57.29 - 1 rad in degrees
fpConst_0_43429:
                .byte   $7f,$5e,$5b,$d8,$aa
fpConst_e:      .byte   $82,$2d,$f8,$54,$58             ;e = 2.7182818279e+00
fpConst_ln_2:   .byte   $80,$31,$72,$17,$f8             ;ln(2) = 6.9314718130e-01
fpConst_0_54625:
                .byte   $80,$0b,$d7,$50,$29             ;5.4625416757e-01
                .byte   $7c
                .byte   $d2
                .byte   $7c
                .byte   $86
                .byte   $05
                .byte   $80
                .byte   $15
                .byte   $52
                .byte   $b6
                .byte   $36
                .byte   $7c
                .byte   $99
                .byte   $98
                .byte   $36
                .byte   $04
                .byte   $80
                .byte   $40
                .byte   $00
                .byte   $01
                .byte   $10
                .byte   $7f
                .byte   $2a
                .byte   $aa
                .byte   $aa
                .byte   $e3
fpConst_min_0_5:
                .byte   $7f,$ff,$ff,$ff,$ff
fpConst_min_0_011909:
                .byte   $7a,$c3,$1e,$18,$be             ;-1.1909031069e-02 ; Used in SIN/COS
                .byte   "saqU-{"
                .byte   $8c
                .byte   $9b
                .byte   $91
                .byte   $88
                .byte   $77
                .byte   $2b
                .byte   $a4
                .byte   $c4
                .byte   $53
                .byte   $7c
                .byte   $4c
                .byte   $cc
                .byte   $ca
                .byte   $b7
                .byte   $7e
.macro          HiAscii Arg
                .repeat .strlen(Arg), I
                .byte   .strat(Arg, I) | $80
                .endrep
.endmacro
                HiAscii "***&"
fpConst_1_0:    .byte   $81,$00,$00,$00,$00
fpConst_min0_08005:
                .byte   $7d,$a3,$f2,$ef,$44             ;-8.0053204787e-02
                .byte   $7e
                .byte   $1f
                .byte   $01
                .byte   $a1
                .byte   $4d
                .byte   $7f
                .byte   $61
                .byte   $6d
                .byte   $f4
                .byte   $3f
                .byte   $7e
                .byte   $5c
                .byte   $91
                .byte   $23
                .byte   $ac
                .byte   $7e
                .byte   $76
                .byte   $b8
                .byte   $8d
                .byte   $1a
                .byte   $7d
                .byte   $1d
                .byte   $3e
                .byte   $ab
                .byte   $2c
                .byte   $81
                .byte   $09
                .byte   $41
                .byte   $81
                .byte   $d2
                .byte   $80
                .byte   $74
                .byte   $df
                .byte   $bd
                .byte   $20
                .byte   $80
                .byte   $83
                .byte   $8b
                .byte   $1f
                .byte   $b5
                .byte   $7f
                .byte   $82
                .byte   $59
                .byte   $ad
                .byte   $ab
fpConst_0_9273: .byte   $80,$6d,$63,$38,$2c             ;9.2729521822e-01
; following 6 scanned together in ???
fpConst_0_07121:
                .byte   $7d,$11,$d4,$b1,$d1             ;7.1206463996e-02
                .byte   $79,$68,$bc,$4f,$59             ;7.10252642e-03
                .byte   $75,$05,$2c,$9e,$39             ;2.54009799e-04
                .byte   $7b,$08,$88,$3b,$a6             ;1.66665235e-02
                .byte   $6c,$31,$cf,$d1,$8c             ;6.6240054064e-07
                .byte   $7d,$2a,$aa,$aa,$89             ;8.33
                .byte   $7f,$ff,$ff,$ff,$e8             ;-0.5
                .byte   $81,$00,$00,$00,$00             ;1.0
; together?
fpConst_1_2:    .byte   $81,$00,$00,$00,$00             ;TODO: remove?
                .byte   $f3,$bf,$1e,$b7,$fd
                .byte   $b9,$ed,$b9,$53,$bc
                .byte   $9f,$bc,$e3,$bc,$55
                .byte   $38                             ;TODO: reove?
tblDispatchFN:  .word   $0000                           ;0
                .word   TB082
                .word   TB12C
                .word   TB150
                .word   TB174                           ;8
                .word   TB18F
                .word   TB1B3
                .word   TB1E1
                .word   do_FNpow                        ;16
                .word   do_FNsin
                .word   do_FNcos
                .word   do_FNtan
                .word   do_FNacs                        ;24
                .word   do_FNasn
                .word   do_FNatn
                .word   do_FNdeg
                .word   do_FNrad                        ;32
                .word   do_FNlog
                .word   do_FNln
                .word   do_FNexp
                .word   do_FNsqr                        ;40
                .word   TB299
                .word   do_FNcompare
                .word   TB2EC
                .word   do_FNint                        ;48
                .word   do_FN_addYtomant7thenRound
                .word   do_FNint2real

ServiceHandler: pld
                phd
                pei     ($04)
                pld
                php
                sep     #$30
                .a8
                .i8
                bne     @sk
                jsr     AllocNewDP
@sk:            pha
                txa
                ror     A
                pla
                bcc     @skok1
                brl     brk_42_BadFN

@skok1:         cpx     #$38
                bcc     @skok2
                brl     brk_42_BadFN

@skok2:         jsr     BHAtoPtrA
                lda     $06,S
                xba
                lda     $05,S
                phk
                plb
                phk
                phk
                jsr     clrFPA
                phk
                jsr     clrFPB
                jsr     (tblDispatchFN,x)
                lda     DP_ART_PtrA
                pha
                lda     DP_ART_PtrA+1
                pha
                lda     DP_ART_PtrA+2
                pha
                plb
                pla
                xba
                pla
                bcc     @plpclcrtl
                plp
                sec
                rtl

@plpclcrtl:     plp
                clc
                rtl

TB082:          clc
                rts

AllocNewDP:     sep     #$30
                phb
                pha
                xba
                pha
                phx
                phy
                cop     COP_10_OPADP
                .word   $0100
                bcs     throwDPAlloc
                rep     #$30
                .a16
                .i16
                tcd
                ldy     #$0004
                pea     $0000
                plb
                plb
                sta     ($09,S),y
                phd
                lda     #$0200
                ldy     #_HDMMT
                ldx     #_MMASA
                sep     #$30
                .a8
                .i8
                jsr     call_MM
                bcs     throwMemAlloc
                sta     DP_ART_WKSPptr
                xba
                sta     DP_ART_WKSPptr+1
                phb
                pla
                sta     DP_ART_WKSPptr+2
                pld
                ply
                plx
                pla
                xba
                pla
                plb
                rts

throwMemAlloc:  cop     COP_0F_OPERR
                .asciiz "Memory allocation failure for Arithmetic package"
throwDPAlloc:   cop     COP_0F_OPERR
                .asciiz "Direct page allocation failure for Arithmetic package"
TB12C:          jsr     copyWKSPtoPtrB
                ldy     #$07
                jsr     copyYpl1PtrAtoPtrB
                lda     #$04
                jsr     addAtoPtrA
                jsr     swapPtrAandPtrB
                phk
                jsr     LBE11
                phk
                jsr     LB3F9
                jsr     swapPtrAandPtrB
                lda     #$04
                jsr     addAtoPtrA
                jsr     storeINTAatPtrA
                rtl

TB150:          jsr     copyWKSPtoPtrB
                ldy     #$07
                jsr     copyYpl1PtrAtoPtrB
                lda     #$04
                jsr     addAtoPtrA
                jsr     swapPtrAandPtrB
                phk
                jsr     LBE11
                phk
                jsr     LB427
                jsr     swapPtrAandPtrB
                lda     #$04
                jsr     addAtoPtrA
                jsr     storeINTAatPtrA
                rtl

TB174:          jsr     ptrBpointAtDP_INTA
                ldy     #$03
                jsr     copyYpl1PtrAtoPtrB
                jsr     swapPtrAandPtrB
                phk
                jsr     LB444
                jsr     swapPtrAandPtrB
                lda     #$04
                jsr     addAtoPtrA
                jsr     storeINTAatPtrA
                rtl

TB18F:          jsr     copyWKSPtoPtrB
                ldy     #$07
                jsr     copyYpl1PtrAtoPtrB
                lda     #$04
                jsr     addAtoPtrA
                jsr     swapPtrAandPtrB
                phk
                jsr     LBE11
                phk
                jsr     LB468
                jsr     swapPtrAandPtrB
                lda     #$04
                jsr     addAtoPtrA
                jsr     storeINTAatPtrA
                rtl

TB1B3:          jsr     copyInToStackAndUnpackToFPA
                jsr     swapPtrAandPtrB
                lda     #$05
                jsr     addAtoPtrA
                jsr     copyWKSPtoPtrB
                ldy     #$05
                jsr     copyYpl1PtrAtoPtrB
                jsr     swapPtrAandPtrB
                phk
                jsr     ptrCeqptrAPtrAinc5
                phk
                jsr     LB4CC
                phk
                jsr     FPAmantRound7
packFPAtoPtrBswapPtrAPtrB:
                jsr     swapPtrAandPtrB                 ;TODO: optimize - this seems like a pissing about
                lda     #$05
                jsr     addAtoPtrA
                jsr     ptrAdec5packFPAtoPtrA
                rtl

TB1E1:          jsr     copyInToStackAndUnpackToFPA
                jsr     swapPtrAandPtrB
                lda     #$05
                jsr     addAtoPtrA
                jsr     copyWKSPtoPtrB
                ldy     #$05
                jsr     copyYpl1PtrAtoPtrB
                jsr     swapPtrAandPtrB
                phk
                jsr     ptrCeqptrAPtrAinc5
                phk
                jsr     divPtrCByFPA
                bra     packFPAtoPtrBswapPtrAPtrB

do_FNpow:       jsr     copyInToStackAndUnpackToFPA
                jsr     swapPtrAandPtrB
                lda     #$05
                jsr     addAtoPtrA
                jsr     copyWKSPtoPtrB
                ldy     #$05
                jsr     copyYpl1PtrAtoPtrB
                jsr     swapPtrAandPtrB
                phk
                jsr     LBA31
                bra     packFPAtoPtrBswapPtrAPtrB

do_FNsin:       clc
LB21E:          php
                jsr     copyInToStackAndUnpackToFPA
                plp
                php
                phk
                jsr     LC0AB
                plp
                phk
                jsr     LB7E9
                bra     packFPAtoPtrBswapPtrAPtrB

do_FNcos:       sec
                bra     LB21E

do_FNtan:       jsr     copyInToStackAndUnpackToFPA
                phk
                jsr     LC0AB
                phk
                jsr     LB7F2
                bra     packFPAtoPtrBswapPtrAPtrB

do_FNacs:       jsr     copyInToStackAndUnpackToFPA
                phk
                jsr     LB92D
                brl     packFPAtoPtrBswapPtrAPtrB

do_FNasn:       jsr     copyInToStackAndUnpackToFPA
                phk
                jsr     LB936
                brl     packFPAtoPtrBswapPtrAPtrB

do_FNatn:       jsr     copyInToStackAndUnpackToFPA
                phk
                jsr     LB961
                brl     packFPAtoPtrBswapPtrAPtrB

do_FNdeg:       jsr     copyInToStackAndUnpackToFPA
                phk
                jsr     setPtrCConstRad2Deg
                brl     packFPAtoPtrBswapPtrAPtrB

do_FNrad:       jsr     copyInToStackAndUnpackToFPA
                phk
                jsr     setPtrCConstDeg2Rad
                brl     packFPAtoPtrBswapPtrAPtrB

do_FNlog:       jsr     copyInToStackAndUnpackToFPA
                phk
                jsr     LB9CB
                brl     packFPAtoPtrBswapPtrAPtrB

do_FNln:        jsr     copyInToStackAndUnpackToFPA
                phk
                jsr     LB9D6
                brl     packFPAtoPtrBswapPtrAPtrB

do_FNsqr:       jsr     copyInToStackAndUnpackToFPA
                phk
                jsr     LBFF3
                brl     packFPAtoPtrBswapPtrAPtrB

do_FNexp:       jsr     copyInToStackAndUnpackToFPA
                phk
                jsr     LB9DB
                brl     packFPAtoPtrBswapPtrAPtrB

TB299:          jsr     copyInToStackAndUnpackToFPA
                jsr     swapPtrAandPtrB
                lda     #$05
                jsr     addAtoPtrA
                jsr     copyWKSPtoPtrB
                ldy     #$05
                jsr     copyYpl1PtrAtoPtrB
                jsr     swapPtrAandPtrB
                phk
                jsr     ptrCeqptrAPtrAinc5
                phk
                jsr     FPAeqFPAminusPtrCround7
                brl     packFPAtoPtrBswapPtrAPtrB

do_FNcompare:   jsr     copyInToStackAndUnpackToFPA
                jsr     swapPtrAandPtrB
                lda     #$05
                jsr     addAtoPtrA
                jsr     copyWKSPtoPtrB
                ldy     #$05
                jsr     copyYpl1PtrAtoPtrB
                jsr     swapPtrAandPtrB
                phk
                jsr     ptrCeqptrAPtrAinc5
                phk
                jsr     unpackPtrCtoFPB
                phk
                jsr     compareFPAFPBresinCy
                pha
                php
                jsr     swapPtrAandPtrB
                lda     #$04
                jsr     addAtoPtrA
                ply                                     ;TODO: really?
                tya
                sta     [DP_ART_PtrA]
                pla
                rtl

TB2EC:          phy
                jsr     ptrBpointAt_FP_FPAplus2
                ldy     #$04
                jsr     copyYpl1PtrAtoPtrB
                jsr     swapPtrAandPtrB
                ply
                sty     DP_ART_FPA+7
                phk
                jsr     normFPAmant2
                lda     DP_ART_FPA+7
                pha
                ldy     #$04
                jsr     copyYpl1PtrAtoPtrB
                jsr     swapPtrAandPtrB
                ply
                rtl

do_FNint:       jsr     copyInToStackAndUnpackToFPA
                jsr     swapPtrAandPtrB
                lda     #$05
                jsr     addAtoPtrA
                jsr     copyWKSPtoPtrB
                jsr     swapPtrAandPtrB
                phk
                jsr     FPAtoINTAasint
                jsr     swapPtrAandPtrB
                jsr     storeINTAatPtrA
                rtl

do_FN_addYtomant7thenRound:
                phy                                     ;what's in Y?
                jsr     copyInToStackAndUnpackToFPA
                ply
                sty     DP_ART_FPA+7
                stz     DP_ART_FPA
                phk
                jsr     FPAmantRound7
                brl     packFPAtoPtrBswapPtrAPtrB

do_FNint2real:  jsr     ptrBpointAtDP_INTA
                ldy     #$03
                jsr     copyYpl1PtrAtoPtrB              ;get integer to PtrB
                lda     #$04
                jsr     addAtoPtrA
                jsr     swapPtrAandPtrB
                phk
                jsr     intA2RealFPA
                jsr     swapPtrAandPtrB
                jsr     ptrAdec5packFPAtoPtrA
                rtl

BHAtoPtrA:      pha
                xba
                pha
                phb
                pla
                sta     DP_ART_PtrA+2
                pla
                sta     DP_ART_PtrA+1
                pla
                sta     DP_ART_PtrA
                rts

ptrBpointAtDP_INTA:
                lda     #DP_ART_INTA
                pha
LB364:          stz     DP_ART_PtrB+2
                tdc
                sta     DP_ART_PtrB
                xba
                sta     DP_ART_PtrB+1
                pla
                clc
                adc     DP_ART_PtrB
                sta     DP_ART_PtrB
                lda     #$00
                adc     DP_ART_PtrB+1
                sta     DP_ART_PtrB+1
                rts

ptrBpointAt_FP_FPAplus2:
                lda     #DP_ART_FPA+2
                pha
                bra     LB364

                .byte   $60

addAtoPtrA:     clc
                adc     DP_ART_PtrA
                sta     DP_ART_PtrA
                lda     DP_ART_PtrA+1
                adc     #$00
                sta     DP_ART_PtrA+1
                lda     DP_ART_PtrA+2
                adc     #$00
                sta     DP_ART_PtrA+2
                rts

subAminusAfromPtrA:
                clc
                adc     DP_ART_PtrA
                sta     DP_ART_PtrA
                bcs     @LB39E
                dec     DP_ART_PtrA+1
                bcc     @LB39E

                .byte   $c6
                .byte   $08

@LB39E:         rts

ptrAdec5packFPAtoPtrA:
                sec                                     ;TODO: optimize this crap?
                lda     #$00
                sbc     #$05
                jsr     subAminusAfromPtrA
                ldy     #$00
                lda     DP_ART_FPA+2
                sta     [DP_ART_PtrA],y
                iny
                lda     DP_ART_FPA
                eor     DP_ART_FPA+3
                and     #$80
                eor     DP_ART_FPA+3
                sta     [DP_ART_PtrA],y
                iny
                lda     DP_ART_FPA+4
                sta     [DP_ART_PtrA],y
                iny
                lda     DP_ART_FPA+5
                sta     [DP_ART_PtrA],y
                iny
                lda     DP_ART_FPA+6
                sta     [DP_ART_PtrA],y
                rts

storeINTAatPtrA:
                sec
                lda     #$00
                sbc     #$04
                jsr     subAminusAfromPtrA
                ldy     #$03
                lda     DP_ART_INTA+3
                sta     [DP_ART_PtrA],y
                dey
                lda     DP_ART_INTA+2
                sta     [DP_ART_PtrA],y
                dey
                lda     DP_ART_INTA+1
                sta     [DP_ART_PtrA],y
                lda     DP_ART_INTA
                sta     [DP_ART_PtrA]
                rts

copyInToStackAndUnpackToFPA:
                jsr     copyWKSPtoPtrB
                ldy     #$05
                jsr     copyYpl1PtrAtoPtrB
                jsr     swapPtrAandPtrB
                phk
                jsr     ptrCeqptrAPtrAinc5
                phk
                jsr     unpackPtrCtoFPA
                rts

LB3F9:          clc
                lda     [DP_ART_PtrA]
                adc     DP_ART_INTA
                sta     DP_ART_INTA
                ldy     #$01
                lda     [DP_ART_PtrA],y
                adc     DP_ART_INTA+1
                sta     DP_ART_INTA+1
                iny
                lda     [DP_ART_PtrA],y
                adc     DP_ART_INTA+2
                sta     DP_ART_INTA+2
                iny
                lda     [DP_ART_PtrA],y
                adc     DP_ART_INTA+3
LB414:          sta     DP_ART_INTA+3
                lda     #$40
                rtl

                pha                                     ;TODO: dead code?
                clc
                lda     DP_ART_PtrA
                adc     #$04
                sta     DP_ART_PtrA
                bcc     @LB425
                inc     DP_ART_PtrA+1
@LB425:         pla
                rtl

LB427:          sec
                lda     [DP_ART_PtrA]
                sbc     DP_ART_INTA
                sta     DP_ART_INTA
                ldy     #$01
                lda     [DP_ART_PtrA],y
                sbc     DP_ART_INTA+1
                sta     DP_ART_INTA+1
                iny
                lda     [DP_ART_PtrA],y
                sbc     DP_ART_INTA+2
                sta     DP_ART_INTA+2
                iny
                lda     [DP_ART_PtrA],y
                sbc     DP_ART_INTA+3
                bra     LB414

LB444:          sec
                lda     #$00
                tay
                sbc     DP_ART_INTA
                sta     DP_ART_INTA
                tya
                sbc     DP_ART_INTA+1
                sta     DP_ART_INTA+1
                tya
                sbc     DP_ART_INTA+2
                sta     DP_ART_INTA+2
                tya
                sbc     DP_ART_INTA+3
                sta     DP_ART_INTA+3
LB45B:          lda     #$40
                rtl

LB45E:          bit     DP_ART_INTA+3
                bpl     @LB465
                jmp     LB444

@LB465:         jmp     LB45B

LB468:          ldy     DP_ART_INTA+1
                phy
                phk
                jsr     LB45E
                stx     $30
                ldx     #DP_43_QRY
                phk
                jsr     stINTAatDPX
                phk
                jsr     LBE11
                pla
                eor     DP_ART_INTA+3
                sta     $41
                phk
                jsr     LB45E
                ldy     #$00
                ldx     #$00
                stz     DP_ART_FPB+4
                stz     DP_ART_FPB+5
@LB48C:         lsr     DP_43_QRY+1
                ror     DP_43_QRY
                bcc     @LB4A7
                clc
                tya
                adc     DP_ART_INTA
                tay
                txa
                adc     DP_ART_INTA+1
                tax
                lda     DP_ART_FPB+4
                adc     DP_ART_INTA+2
                sta     DP_ART_FPB+4
                lda     DP_ART_FPB+5
                adc     DP_ART_INTA+3
                sta     DP_ART_FPB+5
@LB4A7:         asl     DP_ART_INTA
                rol     DP_ART_INTA+1
                rol     DP_ART_INTA+2
                rol     DP_ART_INTA+3
                lda     DP_43_QRY
                ora     DP_43_QRY+1
                bne     @LB48C
                sty     DP_47_QRY
                stx     DP_47_QRY+1
                lda     $41
                php
                ldx     #DP_47_QRY
                phk
                jsr     storeDPXatINTA
                plp
                bpl     @LB4C9
                phk
                jsr     LB444
@LB4C9:         ldx     $30
                rtl

LB4CC:          lda     DP_ART_FPA+3
                bne     @LB4D3
                jmp     rtlLBAD7

@LB4D3:         phk
                jsr     unpackPtrCtoFPB
                bne     @LB4DC
                jmp     clrFPA

@LB4DC:         phk
                plb
                clc
                lda     DP_ART_FPA+2
                adc     DP_47_QRY
                rol     DP_ART_FPA+1
                sbc     #$7f
                sta     DP_ART_FPA+2
                bcs     @LB4ED
                dec     DP_ART_FPA+1
@LB4ED:         phx
                ldx     #$05
                ldy     #$00
@LB4F2:         lda     DP_ART_FPA+2,x
                sta     DP_ART_FPB+7,x
                sty     DP_ART_FPA+2,x
                dex
                bne     @LB4F2
                lda     DP_ART_FPA
                eor     DP_ART_FPB
                sta     DP_ART_FPA
                ldy     #$20
@LB503:         lsr     DP_47_QRY+1
                ror     DP_ART_FPB+4
                ror     DP_ART_FPB+5
                ror     DP_ART_FPB+6
                ror     DP_ART_FPB+7
                asl     DP_ART_FPTMP+2
                rol     DP_ART_FPTMP+1
                rol     DP_ART_FPTMP
                rol     $4d
                bcc     @LB51C
                clc
                phk
                jsr     @LB52B
@LB51C:         dey
                bne     @LB503
                plx
                lda     DP_ART_FPA+3
                bpl     @LB527
                jmp     rtlLBAD7

@LB527:         jmp     normFPAmant3

                .byte   $6b

@LB52B:         lda     DP_ART_FPA+7
                adc     DP_ART_FPB+7
                sta     DP_ART_FPA+7
                lda     DP_ART_FPA+6
                adc     DP_ART_FPB+6
                sta     DP_ART_FPA+6
                lda     DP_ART_FPA+5
                adc     DP_ART_FPB+5
                sta     DP_ART_FPA+5
                lda     DP_ART_FPA+4
                adc     DP_ART_FPB+4
                sta     DP_ART_FPA+4
                lda     DP_ART_FPA+3
                adc     DP_47_QRY+1
                sta     DP_ART_FPA+3
                rtl

intA2RealFPA:   stz     DP_ART_FPA+7
                stz     DP_ART_FPA+1
LB54E:          lda     DP_ART_INTA+3
                sta     DP_ART_FPA
                bpl     @LB55A
                phk
                jsr     LB444
                lda     DP_ART_INTA+3
@LB55A:         bne     @LB582
                stz     DP_ART_FPA+6
                lda     DP_ART_INTA+2
                bne     @LB576
                stz     DP_ART_FPA+5
                lda     DP_ART_INTA+1
                bne     @LB56E
                stz     DP_ART_FPA+4
                lda     DP_ART_INTA
                bra     LB5A7

@LB56E:         ldy     DP_ART_INTA
                sty     DP_ART_FPA+4
                ldy     #$90
                bra     LB5A9

@LB576:         ldy     DP_ART_INTA+1
                sty     DP_ART_FPA+4
                ldy     DP_ART_INTA
                sty     DP_ART_FPA+5
                ldy     #$98
                bra     LB5A9

@LB582:         ldy     DP_ART_INTA+2
                sty     DP_ART_FPA+4
                ldy     DP_ART_INTA+1
                sty     DP_ART_FPA+5
                ldy     DP_ART_INTA
                sty     DP_ART_FPA+6
                ldy     #$a0
                bra     LB5A9

LB592:          stz     DP_ART_FPA
                stz     DP_ART_FPA+2
                stz     DP_ART_FPA+1
                stz     DP_ART_FPA+3
rtlLB59A:       rtl

LB59B:          phk
                jsr     clrFPA
                tay
                bpl     LB5A7
                sta     DP_ART_FPA
                eor     #$ff
                inc     A
LB5A7:          ldy     #$88
LB5A9:          ora     #$00
                bmi     @LB5B9
                beq     LB592
@LB5AF:         dey
                asl     DP_ART_FPA+6
                rol     DP_ART_FPA+5
                rol     DP_ART_FPA+4
                rol     A
                bpl     @LB5AF
@LB5B9:         sta     DP_ART_FPA+3
                sty     DP_ART_FPA+2
                rtl

normFPAmant2:   lda     DP_ART_FPA+3
normFPAmant:    bmi     rtlLB59A
normFPAmant3:   bne     normFPAmant_bitwise
                ora     DP_ART_FPA+4
                ora     DP_ART_FPA+5
                ora     DP_ART_FPA+6
                ora     DP_ART_FPA+7
                beq     LB592
                lda     DP_ART_FPA+2
@bytelp:        ldy     DP_ART_FPA+4
                sty     DP_ART_FPA+3
                ldy     DP_ART_FPA+5
                sty     DP_ART_FPA+4
                ldy     DP_ART_FPA+6
                sty     DP_ART_FPA+5
                ldy     DP_ART_FPA+7
                sty     DP_ART_FPA+6
                stz     DP_ART_FPA+7
                sec
                sbc     #$08
                bcs     @sk
                dec     DP_ART_FPA+1
@sk:            ldy     DP_ART_FPA+3
                beq     @bytelp
                bmi     _skLB606
                bra     _skLB5F3

normFPAmant_bitwise:
                lda     DP_ART_FPA+2
_skLB5F3:       clc
@lp:            sbc     #$00
                bcs     @sk
                dec     DP_ART_FPA+1
@sk:            asl     DP_ART_FPA+7
                rol     DP_ART_FPA+6
                rol     DP_ART_FPA+5
                rol     DP_ART_FPA+4
                rol     DP_ART_FPA+3
                bpl     @lp
_skLB606:       sta     DP_ART_FPA+2
                rtl

compareFPAFPBresinCy:
                ldy     #$00
                lda     #$7f
                trb     DP_ART_FPB
                lda     DP_ART_FPA
                and     #$80
                cmp     DP_ART_FPB
                bne     @LB635
                lda     DP_47_QRY
                cmp     DP_ART_FPA+2
                bne     @LB636
                lda     DP_47_QRY+1
                cmp     DP_ART_FPA+3
                bne     @LB636
                lda     DP_ART_FPB+4
                cmp     DP_ART_FPA+4
                bne     @LB636
                lda     DP_ART_FPB+5
                cmp     DP_ART_FPA+5
                bne     @LB636
                lda     DP_ART_FPB+6
                cmp     DP_ART_FPA+6
                bne     @LB636
@LB635:         rtl

@LB636:         ror     A
                eor     DP_ART_FPB
                rol     A
                lda     #$01
                rtl

LB63D:          phk
                jsr     LBAE7
jmpClrFPA:      jmp     clrFPA

FPAmantToInt:   lda     DP_ART_FPA+2
                bpl     jmpClrFPA
                ldy     DP_ART_FPA+3
                beq     beqifFPAsgnnveNegateMant
@bit1ror:       lsr     DP_ART_FPA+3
                ror     DP_ART_FPA+4
                ror     DP_ART_FPA+5
                ror     DP_ART_FPA+6
                inc     A
                beq     jmpBrk14TooBig2
@bit8rorlp:     cmp     #$a0
                bcs     bneTooBigElseNegIfNeeded
                cmp     #$99
                bcs     @bit1ror
                adc     #$08
                ldy     DP_ART_FPA+5
                sty     DP_ART_FPA+6
                ldy     DP_ART_FPA+4
                sty     DP_ART_FPA+5
                ldy     DP_ART_FPA+3
                sty     DP_ART_FPA+4
                stz     DP_ART_FPA+3
                bra     @bit8rorlp

LB671:          lda     DP_ART_FPA+2
                bpl     LB63D
                phk
                jsr     clrFPB
                ldy     DP_ART_FPA+3
beqifFPAsgnnveNegateMant:
                beq     ifFPAsgnnveNegateMant
@rorFPAFPBmant1bit:
                lsr     DP_ART_FPA+3
                ror     DP_ART_FPA+4
                ror     DP_ART_FPA+5
                ror     DP_ART_FPA+6
                ror     DP_47_QRY+1
                ror     DP_ART_FPB+4
                ror     DP_ART_FPB+5
                ror     DP_ART_FPB+6
                inc     A
                beq     jmpBrk14TooBig2
@rorFPAFPBmant8bit:
                cmp     #$a0
                bcs     bneTooBigElseNegIfNeeded
                cmp     #$99
                bcs     @rorFPAFPBmant1bit
                adc     #$08
                ldy     DP_ART_FPB+5
                sty     DP_ART_FPB+6
                ldy     DP_ART_FPB+4
                sty     DP_ART_FPB+5
                ldy     DP_47_QRY+1
                sty     DP_ART_FPB+4
                ldy     DP_ART_FPA+6
                sty     DP_47_QRY+1
                ldy     DP_ART_FPA+5
                sty     DP_ART_FPA+6
                ldy     DP_ART_FPA+4
                sty     DP_ART_FPA+5
                ldy     DP_ART_FPA+3
                sty     DP_ART_FPA+4
                stz     DP_ART_FPA+3
                bra     @rorFPAFPBmant8bit

jmpBrk14TooBig2:
                jmp     brk_14_TooBig

bneTooBigElseNegIfNeeded:
                bne     jmpBrk14TooBig2
                sta     DP_ART_FPA+2
ifFPAsgnnveNegateMant:
                lda     DP_ART_FPA
                bpl     rtlLB6DC
FPAnegateMant:  sec
                ldy     #$00
                tya
                sbc     DP_ART_FPA+6
                sta     DP_ART_FPA+6
                tya
                sbc     DP_ART_FPA+5
                sta     DP_ART_FPA+5
                tya
                sbc     DP_ART_FPA+4
                sta     DP_ART_FPA+4
                tya
                sbc     DP_ART_FPA+3
                sta     DP_ART_FPA+3
rtlLB6DC:       rtl

LB6DD:          lda     DP_ART_FPA+2
                bmi     @LB6E6
                stz     $54
                jmp     LBDE7

@LB6E6:         phk
                jsr     LB671
                lda     DP_ART_FPA+6
                sta     $54
                phk
                jsr     copyFPBmantToFPA
                lda     #$80
                sta     DP_ART_FPA+2
                ldx     DP_ART_FPA+3
                bpl     @LB70A
                eor     DP_ART_FPA
                sta     DP_ART_FPA
                bpl     @LB704
                inc     $54
                bra     @LB706

@LB704:         dec     $54
@LB706:         phk
                jsr     FPAnegateMant
@LB70A:         jmp     normFPAmant2

                inc     DP_ART_FPA+6                    ;TODO: dead code?
                bne     @LB71D
                inc     DP_ART_FPA+5
                bne     @LB71D
                inc     DP_ART_FPA+4
                bne     @LB71D
                inc     DP_ART_FPA+3
                beq     jmpBrk14TooBig2
@LB71D:         rtl

divPtrCByFPA:   lda     DP_ART_FPA+3
                bne     @skNotDiv0
                jmp     jmpbrk12DivisionByZero          ;TODO: jump straight to brk!

@skNotDiv0:     phk
                jsr     unpackPtrCtoFPB
                bne     @skNotZ
                jmp     clrFPA

@skNotZ:        phk
                plb
                lda     DP_ART_FPB
                eor     DP_ART_FPA
                sta     DP_ART_FPA
                sec
                lda     DP_47_QRY
                adc     #$81
                rol     DP_ART_FPA+1
                sbc     DP_ART_FPA+2
                bcs     @sk
                dec     DP_ART_FPA+1
@sk:            sta     DP_ART_FPA+2
                ldy     #$04
                sty     DP_47_QRY
                lda     DP_47_QRY+1
                ldx     #$08
                bra     @divLpEnterX

@divlp:         phb
                phk
                plb
                stx     DP_ART_FPTMP,y
                ldx     tblDivConsts,y
                sty     DP_47_QRY
                plb
@divlp2:        bcs     @skCS
@divLpEnterX:   cmp     DP_ART_FPA+3
                bne     @cmpsk
                ldy     DP_ART_FPB+4
                cpy     DP_ART_FPA+4
                bne     @cmpsk
                ldy     DP_ART_FPB+5
                cpy     DP_ART_FPA+5
                bne     @cmpsk
                ldy     DP_ART_FPB+6
                cpy     DP_ART_FPA+6
@cmpsk:         bcc     @skCC
@skCS:          tay
                lda     DP_ART_FPB+6
                sbc     DP_ART_FPA+6
                sta     DP_ART_FPB+6
                lda     DP_ART_FPB+5
                sbc     DP_ART_FPA+5
                sta     DP_ART_FPB+5
                lda     DP_ART_FPB+4
                sbc     DP_ART_FPA+4
                sta     DP_ART_FPB+4
                tya
                sbc     DP_ART_FPA+3
                sec
@skCC:          rol     DP_ART_FPB
                asl     DP_ART_FPB+6
                rol     DP_ART_FPB+5
                rol     DP_ART_FPB+4
                rol     A
                dex
                bne     @divlp2
                ldx     DP_ART_FPB
                ldy     DP_47_QRY
                dey
                bpl     @divlp
                ora     DP_ART_FPB+4
                ora     DP_ART_FPB+5
                ora     DP_ART_FPB+6
                beq     @skEQ
                sec
@skEQ:          txa
                ror     A
                ror     A
                ror     A
                and     #$e0
                sta     DP_ART_FPA+7
                lda     DP_ART_FPTMP
                sta     DP_ART_FPA+6
                lda     DP_ART_FPTMP+1
                sta     DP_ART_FPA+5
                lda     DP_ART_FPTMP+2
                sta     DP_ART_FPA+4
                lda     DP_ART_FPTMP+3
                sta     DP_ART_FPA+3
                bmi     FPAmantRound7
                phk
                jsr     normFPAmant_bitwise
                bra     FPAmantRound7

FPAmantRound7:  lda     DP_ART_FPA+7
                cmp     #$80
                bcc     @skCheckOv
                beq     @skSetLowBit
                inc     DP_ART_FPA+6
                bne     @skCheckOv
                phk
                jsr     incFPAmant5_3_IncExpIfOv
                bra     @skCheckOv

@skSetLowBit:   rol     A
                tsb     DP_ART_FPA+6                    ;set bottom bit of FPA6
@skCheckOv:     lda     DP_ART_FPA+1
                bne     @dealWithOverFlowExp
                jmp     zeroFPA7rtl

@dealWithOverFlowExp:
                bpl     jmpBrk14TooBig
                jmp     clrFPA

jmpBrk14TooBig: jmp     brk_14_TooBig

LB7E9:          bcc     @LB7ED
                inc     $54
@LB7ED:         phk
                jsr     LC127
                rtl

LB7F2:          lda     #DP_7F_QRY
                phk
                jsr     LBCD4
                phk
                jsr     LC127
                lda     #DP_7A_QRY
                phk
                jsr     LBCD4
                lda     #DP_7F_QRY
                phk
                jsr     LBC90
                inc     $54
                phk
                jsr     LC127
LB80E:          lda     #DP_7A_QRY
                phk
                jsr     LBAB7
                phk
                jsr     divPtrCByFPA
                lda     #$ff
                rtl

                lda     $69                             ;TODO: DEAD CODE?
                pha
                plb
                rts

copyFPBtoFPA:   lda     DP_ART_FPB
                sta     DP_ART_FPA
                stz     DP_ART_FPA+1
                lda     DP_47_QRY
                sta     DP_ART_FPA+2
copyFPBmantToFPA:
                lda     DP_47_QRY+1
                sta     DP_ART_FPA+3
                lda     DP_ART_FPB+4
                sta     DP_ART_FPA+4
                lda     DP_ART_FPB+5
                sta     DP_ART_FPA+5
                lda     DP_ART_FPB+6
                sta     DP_ART_FPA+6
                lda     DP_ART_FPB+7
                sta     DP_ART_FPA+7
rtlLB83E:       rtl

FPAeqFPAminusFPB:
                lda     DP_ART_FPA+3
                beq     copyFPBtoFPA
                sec
                lda     DP_ART_FPA+2
                sbc     DP_47_QRY
                beq     @cmpsign
                bcc     @rorFPAtoMatchExps
                cmp     #$25
                bcs     rtlLB83E
                tay
                and     #$38
                beq     @skrorFPBno8
                sec
@rorFPB8bitlp:  ldx     DP_ART_FPB+6
                stx     DP_ART_FPB+7
                ldx     DP_ART_FPB+5
                stx     DP_ART_FPB+6
                ldx     DP_ART_FPB+4
                stx     DP_ART_FPB+5
                ldx     DP_47_QRY+1
                stx     DP_ART_FPB+4
                stz     DP_47_QRY+1
                sbc     #$08
                bne     @rorFPB8bitlp
@skrorFPBno8:   tya
                and     #$07
                beq     @cmpsign
@rorFPB1bitlp:  lsr     DP_47_QRY+1
                ror     DP_ART_FPB+4
                ror     DP_ART_FPB+5
                ror     DP_ART_FPB+6
                ror     DP_ART_FPB+7
                dec     A
                bne     @rorFPB1bitlp
                bra     @cmpsign

@rorFPAtoMatchExps:
                eor     #$ff
                inc     A
                cmp     #$25
                bcs     copyFPBtoFPA
                ldy     DP_47_QRY
                sty     DP_ART_FPA+2
                tay
                and     #$38
                beq     @skrorFPAno8
                sec
@rorFPA8bitlp:  ldx     DP_ART_FPA+6
                stx     DP_ART_FPA+7
                ldx     DP_ART_FPA+5
                stx     DP_ART_FPA+6
                ldx     DP_ART_FPA+4
                stx     DP_ART_FPA+5
                ldx     DP_ART_FPA+3
                stx     DP_ART_FPA+4
                stz     DP_ART_FPA+3
                sbc     #$08
                bne     @rorFPA8bitlp
@skrorFPAno8:   tya
                and     #$07
                beq     @cmpsign
@rorFPA1bitlp:  lsr     DP_ART_FPA+3
                ror     DP_ART_FPA+4
                ror     DP_ART_FPA+5
                ror     DP_ART_FPA+6
                ror     DP_ART_FPA+7
                dec     A
                bne     @rorFPA1bitlp
@cmpsign:       lda     DP_ART_FPA
                eor     DP_ART_FPB
                bmi     @cmpMant0FPAifSame
                clc
                jmp     addFPBtoFPAmantAndIncExpIfCy

@cmpMant0FPAifSame:
                lda     DP_ART_FPA+3
                cmp     DP_47_QRY+1
                bne     @FPAeqFPBminusFPA
                lda     DP_ART_FPA+4
                cmp     DP_ART_FPB+4
                bne     @FPAeqFPBminusFPA
                lda     DP_ART_FPA+5
                cmp     DP_ART_FPB+5
                bne     @FPAeqFPBminusFPA
                lda     DP_ART_FPA+6
                cmp     DP_ART_FPB+6
                bne     @FPAeqFPBminusFPA
                lda     DP_ART_FPA+7
                cmp     DP_ART_FPB+7
                bne     @FPAeqFPBminusFPA
                jmp     clrFPA

@FPAeqFPBminusFPA:
                bcs     @FPAeqFPAminusFPB
                lda     DP_ART_FPB
                sta     DP_ART_FPA
                sec
                lda     DP_ART_FPB+7
                sbc     DP_ART_FPA+7
                sta     DP_ART_FPA+7
                lda     DP_ART_FPB+6
                sbc     DP_ART_FPA+6
                sta     DP_ART_FPA+6
                lda     DP_ART_FPB+5
                sbc     DP_ART_FPA+5
                sta     DP_ART_FPA+5
                lda     DP_ART_FPB+4
                sbc     DP_ART_FPA+4
                sta     DP_ART_FPA+4
                lda     DP_47_QRY+1
                sbc     DP_ART_FPA+3
                sta     DP_ART_FPA+3
                jmp     normFPAmant

@FPAeqFPAminusFPB:
                lda     DP_ART_FPA+7
                sbc     DP_ART_FPB+7
                sta     DP_ART_FPA+7
                lda     DP_ART_FPA+6
                sbc     DP_ART_FPB+6
                sta     DP_ART_FPA+6
                lda     DP_ART_FPA+5
                sbc     DP_ART_FPB+5
                sta     DP_ART_FPA+5
                lda     DP_ART_FPA+4
                sbc     DP_ART_FPB+4
                sta     DP_ART_FPA+4
                lda     DP_ART_FPA+3
                sbc     DP_47_QRY+1
                sta     DP_ART_FPA+3
                jmp     normFPAmant

LB92D:          phk
                jsr     LB936
                phk
                jsr     LB988
                rtl

LB936:          lda     DP_ART_FPA
                bpl     @LB945
                stz     DP_ART_FPA
                phk
                jsr     @LB945
                lda     #$ff
                sta     DP_ART_FPA
                rtl

@LB945:         phk
                jsr     LBCCE
                phk
                jsr     LC13C
                lda     DP_ART_FPA+3
                beq     @LB95A
                phk
                jsr     LB80E
                phk
                jsr     LB966
                rtl

@LB95A:         lda     #<fpConst_PIdiv2
                phk
                jsr     FPAeqConstA
                rtl

LB961:          phk
                jsr     LB966
                rtl

LB966:          phk
                jsr     LBDE7
                bne     @LB96F
                brl     LC139

@LB96F:         bpl     @LB97A
                stz     DP_ART_FPA
                phk
                jsr     @LB97A
                sta     DP_ART_FPA
                rtl

@LB97A:         lda     DP_ART_FPA+2
                cmp     #$81
                bcc     LB993
                phk
                jsr     FPAeq1divFPA
                phk
                jsr     LB993
LB988:          phk
                jsr     PtrCeqPIdiv2
                phk
                jsr     LB9E9
                lda     #$ff
                rtl

LB993:          lda     DP_ART_FPA+2
                cmp     #$73
                bcs     @LB99C
                brl     LC139

@LB99C:         phk
                jsr     LBCCE
                phk
                jsr     LBADE
                lda     #$80
                sta     DP_47_QRY
                sta     DP_47_QRY+1
                sta     DP_ART_FPB
                phk
                jsr     FPAeqFPAminusFPBround7
                ldx     #<fpConst_min0_08005
                lda     #<fpConst_0_9273
                ldy     #$04
                phk
                jsr     qryqryDoSomatWiConstAandX_Ytimes
                jmp     PtrCtoDP7A

setPtrCConstRad2Deg:
                lda     #<fpConst_Rad2Deg
                phk
                jsr     const2PtrCandSomething
                rtl

setPtrCConstDeg2Rad:
                lda     #<fpConst_Deg2Rad
                phk
                jsr     const2PtrCandSomething
                rtl

LB9CB:          phk
                jsr     LBE76
                lda     #<fpConst_0_43429
                phk
                jsr     const2PtrCandSomething
                rtl

LB9D6:          phk
                jsr     LBE76
                rtl

LB9DB:          phk
                jsr     LBF1C
                rtl

FPAeq1divFPA:   lda     #<fpConst_1_0
                phk
                jsr     PtrCeqFPConstA
                jmp     divPtrCByFPA

LB9E9:          phk
                jsr     LBC53
FPAeqFPAminusPtrCround7:
                phk
                jsr     unpackPtrCtoFPB
                bne     FPAeqFPAminusFPBround7
                jmp     rtlLBAD7

FPAeqFPAminusFPBround7:
                phk
                jsr     FPAeqFPAminusFPB
                jmp     FPAmantRound7

LB9FD:          phk
                jsr     LB4CC
                jmp     FPAmantRound7

                phk                                     ;TODO: dead code?
                jsr     intA2RealFPA
                phk
                jsr     LBE11
                phk
                jsr     LBE4D
                phk
                jsr     intA2RealFPA
                bra     @LBA23

                phk                                     ;TODO: dead code?
                jsr     intA2RealFPA
                phk
                jsr     LBE4D
                tay
                phk
                jsr     LBC81
@LBA23:         phk
                jsr     ptrCeqptrAPtrAinc5
                phk
                jsr     LB9FD
                lda     #$ff
                rtl

                jmp     brk_06_TypeMismatch             ;TODO: dead code?

LBA31:          lda     DP_ART_FPA+2
                cmp     #$87
                bcs     @LBA90
                phk
                jsr     LB6DD
                bne     @LBA4F
                phk
                jsr     ptrCeqptrAPtrAinc5
                phk
                jsr     unpackPtrCtoFPA
                phk
                plb
                lda     $54
                phk
                jsr     LBD19
                bra     @LBA8D

@LBA4F:         phk
                jsr     LBCCE
                lda     DP_ART_PtrA
                sta     DP_ART_PtrC
                lda     DP_ART_PtrA+1
                sta     DP_ART_PtrC+1
                lda     DP_ART_PtrA+2
                sta     DP_ART_PtrC+2
                phk
                jsr     unpackPtrCtoFPA
                phk
                plb
                lda     $54
                phk
                jsr     LBD19
@LBA6B:         lda     #DP_75_QRY
                phk
                jsr     LBCD4
                phk
                jsr     ptrCeqptrAPtrAinc5
                phk
                jsr     unpackPtrCtoFPA
                phk
                plb
                phk
                jsr     LBE76
                phk
                jsr     PtrCtoDP7A
                phk
                jsr     LBF1C
                lda     #DP_75_QRY
                phk
                jsr     PtrCtoDPinA
@LBA8D:         lda     #$ff
                rtl

@LBA90:         phk
                jsr     LBCCE
                phk
                jsr     LBD36
                bra     @LBA6B

add5ToConstPtrLSB:
                clc
                lda     $58
                adc     #$05
                sta     $58
                sta     DP_ART_PtrC
                rtl

PtrCeqPIdiv2:   lda     #<fpConst_PIdiv2
PtrCeqFPConstA: sta     DP_ART_PtrC
                lda     #>fpConst_MinPiDiv2
                sta     DP_ART_PtrC+1
                phk
                pla
                sta     DP_ART_PtrC+2
                rtl

                lda     #DP_75_QRY                      ;TODO: DP pointer - DEAD CODE?
                bra     LBAB7

PtrCeqDP70:     lda     #DP_70_QRY                      ;TODO: DP pointer
LBAB7:          phd
                clc
                adc     $01,S
                sta     DP_ART_PtrC
                lda     $02,S
                adc     #$00
                sta     DP_ART_PtrC+1
                stz     DP_ART_PtrC+2
                pld
                rtl

clrFPA:         stz     DP_ART_FPA+2
                stz     DP_ART_FPA+3
LBACB:          stz     DP_ART_FPA
                stz     DP_ART_FPA+1
                stz     DP_ART_FPA+4
                stz     DP_ART_FPA+5
                stz     DP_ART_FPA+6
zeroFPA7rtl:    stz     DP_ART_FPA+7
rtlLBAD7:       rtl

clrFPB:         stz     DP_ART_FPB
                stz     DP_47_QRY
                stz     DP_47_QRY+1
LBADE:          stz     DP_ART_FPB+4
                stz     DP_ART_FPB+5
                stz     DP_ART_FPB+6
                stz     DP_ART_FPB+7
                rtl

LBAE7:          lda     DP_ART_FPA
                sta     DP_ART_FPB
                lda     DP_ART_FPA+2
                sta     DP_47_QRY
                lda     DP_ART_FPA+3
                sta     DP_47_QRY+1
                lda     DP_ART_FPA+4
                sta     DP_ART_FPB+4
                lda     DP_ART_FPA+5
                sta     DP_ART_FPB+5
                lda     DP_ART_FPA+6
                sta     DP_ART_FPB+6
                lda     DP_ART_FPA+7
                sta     DP_ART_FPB+7
                rtl

brk_14_TooBig:  brk     $14

                .asciiz "Too big"
                lda     DP_ART_FPA+4                    ;TODO: DEAD CODE?
                sta     DP_ART_INTA+3
                and     #$80
                ora     DP_ART_FPA+3
                beq     @LBB1B
                jmp     @LBB2B

@LBB1B:         lda     DP_ART_FPA+7
                sta     DP_ART_INTA
                lda     DP_ART_FPA+6
                sta     DP_ART_INTA+1
                lda     DP_ART_FPA+5
                sta     DP_ART_INTA+2
                lda     #$40
                sec
                rtl

@LBB2B:         lda     #$a8
                sta     DP_ART_FPA+2
                stz     DP_ART_FPA+1
                stz     DP_ART_FPA
                phk
                jsr     normFPAmant2
                lda     $53
                bmi     @LBB47
                beq     @LBB4F
@LBB3D:         phk
                jsr     LBB6A
                dec     $53
                bne     @LBB3D
                bra     @LBB4F

@LBB47:         phk
                jsr     LBBAE
                inc     $53
                bne     @LBB47
@LBB4F:         phk
                jsr     FPAmantRound7
                sec
                lda     #$ff
                rtl

storeDPXatINTA: lda     $00,x                           ;TODO: optimize - only called once inline?
                sta     DP_ART_INTA
                lda     $01,x
                sta     DP_ART_INTA+1
                lda     $02,x
                sta     DP_ART_INTA+2
                lda     $03,x
                sta     DP_ART_INTA+3
                lda     #$40
                rtl

LBB6A:          clc
                lda     DP_ART_FPA+2
                adc     #$03
                sta     DP_ART_FPA+2
                bcc     @LBB75
                inc     DP_ART_FPA+1
@LBB75:         phk
                jsr     LBD91
                phk
                jsr     LBD95
addFPBtoFPAmantAndIncExpIfCy:
                lda     DP_ART_FPA+7
                adc     DP_ART_FPB+7
                sta     DP_ART_FPA+7
                lda     DP_ART_FPA+6
                adc     DP_ART_FPB+6
                sta     DP_ART_FPA+6
                lda     DP_ART_FPA+5
                adc     DP_ART_FPB+5
                sta     DP_ART_FPA+5
                lda     DP_ART_FPA+4
                adc     DP_ART_FPB+4
                sta     DP_ART_FPA+4
                lda     DP_ART_FPA+3
                adc     DP_47_QRY+1
                sta     DP_ART_FPA+3
                bcc     rtl1
FPAmantrorIncExp:
                ror     DP_ART_FPA+3
                ror     DP_ART_FPA+4
                ror     DP_ART_FPA+5
                ror     DP_ART_FPA+6
                ror     DP_ART_FPA+7
                inc     DP_ART_FPA+2
                bne     rtl1
                inc     DP_ART_FPA+1
rtl1:           rtl

LBBAE:          sec
                lda     DP_ART_FPA+2
                sbc     #$04
                sta     DP_ART_FPA+2
                bcs     @LBBB9
                dec     DP_ART_FPA+1
@LBBB9:         phk
                jsr     LBD91
                phk
                jsr     addFPBtoFPAmantAndIncExpIfCy
                phk
                jsr     LBD91
                phk
                jsr     LBD95
                phk
                jsr     LBD95
                phk
                jsr     LBD95
                phk
                jsr     addFPBtoFPAmantAndIncExpIfCy
                stz     DP_47_QRY+1
                lda     DP_ART_FPA+3
                sta     DP_ART_FPB+4
                lda     DP_ART_FPA+4
                sta     DP_ART_FPB+5
                lda     DP_ART_FPA+5
                sta     DP_ART_FPB+6
                lda     DP_ART_FPA+6
                sta     DP_ART_FPB+7
                lda     DP_ART_FPA+7
                rol     A
                phk
                jsr     addFPBtoFPAmantAndIncExpIfCy
                stz     DP_ART_FPB+4
                lda     DP_ART_FPA+3
                sta     DP_ART_FPB+5
                lda     DP_ART_FPA+4
                sta     DP_ART_FPB+6
                lda     DP_ART_FPA+5
                sta     DP_ART_FPB+7
                lda     DP_ART_FPA+6
                rol     A
                phk
                jsr     addFPBtoFPAmantAndIncExpIfCy
                lda     DP_ART_FPA+4
                rol     A
                lda     DP_ART_FPA+3
                adc     DP_ART_FPA+7
                sta     DP_ART_FPA+7
                bcc     rtl2
                inc     DP_ART_FPA+6
                bne     rtl2
incFPAmant5_3_IncExpIfOv:
                inc     DP_ART_FPA+5
                bne     rtl2
                inc     DP_ART_FPA+4
                bne     rtl2
                inc     DP_ART_FPA+3
                bne     rtl2
                jmp     FPAmantrorIncExp

rtl2:           rtl

unpackPtrCtoFPB:
                stz     DP_ART_FPB+7
                ldy     #$04
                lda     [DP_ART_PtrC],y
                sta     DP_ART_FPB+6
                dey
                lda     [DP_ART_PtrC],y
                sta     DP_ART_FPB+5
                dey
                lda     [DP_ART_PtrC],y
                sta     DP_ART_FPB+4
                dey
                lda     [DP_ART_PtrC],y
                sta     DP_ART_FPB
                tay
                lda     [DP_ART_PtrC]
                sta     DP_47_QRY
                bne     @notZ
                tya
                ora     DP_ART_FPB+4
                ora     DP_ART_FPB+5
                ora     DP_ART_FPB+6
                beq     @zero
@notZ:          tya
                ora     #$80
@zero:          sta     DP_47_QRY+1
                rtl

                phk                                     ;TODO: dead code to LBC53
                jsr     LB9E9
LBC53:          lda     DP_ART_FPA+3
                beq     @LBC5D
                lda     DP_ART_FPA
                eor     #$80
                sta     DP_ART_FPA
@LBC5D:         lda     #$ff
                rtl

                jmp     brk_06_TypeMismatch             ;TODO: DEAD CODE?

                lda     DP_ART_PtrA                     ;TODO: DEAD CODE?
                sec
                sbc     #$04
                phk
                jsr     LBD03
                ldy     #$03
                lda     DP_ART_INTA+3
                sta     [DP_ART_PtrA],y
                dey
                lda     DP_ART_INTA+2
                sta     [DP_ART_PtrA],y
                dey
                lda     DP_ART_INTA+1
                sta     [DP_ART_PtrA],y
                lda     DP_ART_INTA
                sta     [DP_ART_PtrA]
                rtl

LBC81:          bne     @LBC86
                jmp     LBDCD

@LBC86:         bpl     @LBC8B
                jmp     LBDCC

@LBC8B:         jmp     intA2RealFPA

LBC8E:          lda     #$70
LBC90:          phd
                clc
                adc     $01,S
                sta     DP_ART_PtrC
                lda     $02,S
                adc     #$00
                sta     DP_ART_PtrC+1
                stz     DP_ART_PtrC+2
                pld
unpackPtrCtoFPA:
                stz     DP_ART_FPA+7
                stz     DP_ART_FPA+1
                ldy     #$04
                lda     [DP_ART_PtrC],y
                sta     DP_ART_FPA+6
                dey
                lda     [DP_ART_PtrC],y
                sta     DP_ART_FPA+5
                dey
                lda     [DP_ART_PtrC],y
                sta     DP_ART_FPA+4
                dey
                lda     [DP_ART_PtrC],y
                sta     DP_ART_FPA
                tay
                lda     [DP_ART_PtrC]
                sta     DP_ART_FPA+2
                bne     @skNotZ
                tya
                ora     DP_ART_FPA+4
                ora     DP_ART_FPA+5
                ora     DP_ART_FPA+6
                beq     @skzero
@skNotZ:        tya
                ora     #$80
@skzero:        sta     DP_ART_FPA+3
                rtl

LBCCE:          lda     #DP_7A_QRY
                bra     LBCD4

LBCD2:          lda     #DP_70_QRY
LBCD4:          phd
                clc
                adc     $01,S
                sta     DP_ART_PtrC
                lda     $02,S
                adc     #$00
                sta     DP_ART_PtrC+1
                stz     DP_ART_PtrC+2
                pld
LBCE3:          lda     DP_ART_FPA+2
                sta     [DP_ART_PtrC]
                ldy     #$01
                lda     DP_ART_FPA
                eor     DP_ART_FPA+3
                and     #$80
                eor     DP_ART_FPA+3
                sta     [DP_ART_PtrC],y
                lda     DP_ART_FPA+4
                iny
                sta     [DP_ART_PtrC],y
                lda     DP_ART_FPA+5
                iny
                sta     [DP_ART_PtrC],y
                lda     DP_ART_FPA+6
                iny
                sta     [DP_ART_PtrC],y
                rtl

LBD03:          sta     DP_ART_PtrA
                bcs     @LBD09
                dec     DP_ART_PtrA+1
@LBD09:         ldy     DP_ART_PtrA+1
                cpy     $04
                bcc     @LBD16
                bne     @LBD15
                cmp     $03
                bcc     @LBD16
@LBD15:         rtl

@LBD16:         jmp     LBF9D

LBD19:          tax
                bpl     @LBD25
                dec     A
                eor     #$ff
                pha
                phk
                jsr     FPAeq1divFPA
                plx
@LBD25:         beq     LBD36
                phk
                jsr     LBCD2
                dex
                beq     @LBD35
@LBD2E:         phk
                jsr     LB9FD
                dex
                bne     @LBD2E
@LBD35:         rtl

LBD36:          lda     #$80
                sta     DP_ART_FPA+3
                inc     A
                sta     DP_ART_FPA+2
                jmp     LBACB

                lda     DP_ART_FPA+3                    ;TODO: DEAD CODE?
                lsr     A
                lsr     A
                lsr     A
                lsr     A
                phk
                jsr     LBF9D
                lda     #$f0
                trb     DP_ART_FPA+3
                pha
                ldx     DP_ART_FPA+6
                lda     DP_ART_FPA+3
                pha
                lda     DP_ART_FPA+4
                pha
                lda     DP_ART_FPA+5
                pha
                lda     DP_ART_FPA+7
                asl     A
                rol     DP_ART_FPA+6
                rol     DP_ART_FPA+5
                rol     DP_ART_FPA+4
                rol     DP_ART_FPA+3
                asl     A
                rol     DP_ART_FPA+6
                rol     DP_ART_FPA+5
                rol     DP_ART_FPA+4
                rol     DP_ART_FPA+3
                adc     DP_ART_FPA+7
                sta     DP_ART_FPA+7
                txa
                adc     DP_ART_FPA+6
                sta     DP_ART_FPA+6
                pla
                adc     DP_ART_FPA+5
                sta     DP_ART_FPA+5
                pla
                adc     DP_ART_FPA+4
                sta     DP_ART_FPA+4
                pla
                adc     DP_ART_FPA+3
                asl     DP_ART_FPA+7
                rol     DP_ART_FPA+6
                rol     DP_ART_FPA+5
                rol     DP_ART_FPA+4
                rol     A
                sta     DP_ART_FPA+3
                pla
                rtl

LBD91:          phk
                jsr     LBAE7
LBD95:          lsr     DP_47_QRY+1
                ror     DP_ART_FPB+4
                ror     DP_ART_FPB+5
                ror     DP_ART_FPB+6
                ror     DP_ART_FPB+7
                rtl

jmpbrk12DivisionByZero:
                jmp     brk_12_DivisionByZero

tblDivConsts:   .byte   $02,$08,$08,$08

                phk                                     ;TODO: DEAD CODE
                jsr     LBDD0
                bra     @LBDB4

                phk                                     ;TODO: DEAD CODE entry to LBDB4
                jsr     LBF9D
                lda     $30
                tay
@LBDB4:         beq     LBDCD
                bpl     LBDCC
FPAtoINTAasint: phk
                jsr     FPAmantToInt
                lda     DP_ART_FPA+3
                sta     DP_ART_INTA+3
                lda     DP_ART_FPA+4
                sta     DP_ART_INTA+2
                lda     DP_ART_FPA+5
                sta     DP_ART_INTA+1
                lda     DP_ART_FPA+6
                sta     DP_ART_INTA
LBDCC:          rtl

LBDCD:          jmp     brk_06_TypeMismatch

LBDD0:          rtl

ptrCeqptrAPtrAinc5:
                lda     DP_ART_PtrA
                clc
                sta     DP_ART_PtrC
                adc     #$05
                sta     DP_ART_PtrA
                lda     DP_ART_PtrA+1
                sta     DP_ART_PtrC+1
                adc     #$00
                sta     DP_ART_PtrA+1
                lda     DP_ART_PtrA+2
                sta     DP_ART_PtrC+2
                rtl

LBDE7:          lda     DP_ART_FPA+3
                ora     DP_ART_FPA+4
                ora     DP_ART_FPA+5
                ora     DP_ART_FPA+6
                ora     DP_ART_FPA+7
                beq     @LBDF9
                lda     DP_ART_FPA
                bne     @LBDFF
                inc     A
                rtl

@LBDF9:         stz     DP_ART_FPA
                stz     DP_ART_FPA+2
                stz     DP_ART_FPA+1
@LBDFF:         rtl

stINTAatDPX:    lda     DP_ART_INTA                     ;TODO: optimize - only called once - inline?
                sta     $00,x
                lda     DP_ART_INTA+1
                sta     $01,x
                lda     DP_ART_INTA+2
                sta     $02,x
                lda     DP_ART_INTA+3
                sta     $03,x
                rtl

LBE11:          ldy     #$03
                lda     [DP_ART_PtrA],y
                sta     DP_ART_INTA+3
                dey
                lda     [DP_ART_PtrA],y
                sta     DP_ART_INTA+2
                dey
                lda     [DP_ART_PtrA],y
                sta     DP_ART_INTA+1
                lda     [DP_ART_PtrA]
                sta     DP_ART_INTA
@LBE25:         clc
                lda     #$04
                adc     DP_ART_PtrA
                sta     DP_ART_PtrA
                bcc     @LBE34
                inc     DP_ART_PtrA+1
                bcc     @LBE34
                inc     DP_ART_PtrA+2
@LBE34:         rtl

                ldx     #$41                            ;TODO: DEAD CODE
                ldy     #$03
                lda     [DP_ART_PtrA],y
                sta     $03,x
                dey
                lda     [DP_ART_PtrA],y
                sta     $02,x
                dey
                lda     [DP_ART_PtrA],y
                sta     $01,x
                lda     [DP_ART_PtrA]
                sta     $00,x
                bra     @LBE25

LBE4D:          lda     DP_ART_PtrA
                sec
                sbc     #$05
                phk
                jsr     LBD03
                lda     DP_ART_FPA+2
                sta     [DP_ART_PtrA]
                ldy     #$01
                lda     DP_ART_FPA
                eor     DP_ART_FPA+3
                and     #$80
                eor     DP_ART_FPA+3
                sta     [DP_ART_PtrA],y
                iny
                lda     DP_ART_FPA+4
                sta     [DP_ART_PtrA],y
                iny
                lda     DP_ART_FPA+5
                sta     [DP_ART_PtrA],y
                iny
                lda     DP_ART_FPA+6
                sta     [DP_ART_PtrA],y
                rtl

LBE76:          phk
                jsr     LBDE7
                beq     @LBE7E
                bpl     LBE94
@LBE7E:         brk     $16

                .byte   "Log range"

brk_15_nve_root:
                brk     $15

                .byte   "-ve root"
                .byte   $00

LBE94:          phk
                jsr     LBADE
                ldy     #$80
                sty     DP_ART_FPB
                sty     DP_47_QRY+1
                iny
                sty     DP_47_QRY
                ldx     DP_ART_FPA+2
                beq     @LBEAB
                lda     DP_ART_FPA+3
                cmp     #$b5
                bcc     @LBEAD
@LBEAB:         inx
                dey
@LBEAD:         phx
                sty     DP_ART_FPA+2
                phk
                jsr     FPAeqFPAminusFPBround7
                lda     #DP_7F_QRY
                phk
                jsr     LBCD4
                ldx     #<fpConst_0_54625
                lda     #<fpConst_min_0_5
                ldy     #$02
                phk
                jsr     qryqryDoSomatWiConstAandX_Ytimes
                lda     #DP_7F_QRY
                phk
                jsr     PtrCtoDPinA
                phk
                jsr     LB9FD
                phk
                jsr     FPAeqFPAminusPtrCround7
                phk
                jsr     LBCD2
                pla
                sec
                sbc     #$81
                phk
                jsr     LB59B
                lda     #<fpConst_ln_2
                phk
                jsr     const2PtrCandSomething
                phk
                jsr     PtrCeqDP70
                phk
                jsr     FPAeqFPAminusPtrCround7
LBEEC:          lda     #$ff
                rtl

PtrCtoDP7A:     lda     #DP_7A_QRY
PtrCtoDPinA:    phd
                clc
                adc     $01,S
                sta     DP_ART_PtrC
                lda     $02,S
                adc     #$00
                sta     DP_ART_PtrC+1
                stz     DP_ART_PtrC+2
                pld
                bra     LBF09

LBF02:          sty     DP_ART_PtrC+1
                sta     DP_ART_PtrC
                stx     DP_ART_PtrC+2
                plx
LBF09:          phk
                jsr     LB9FD
                lda     #$ff
                rtl

brk_18_ExpRange:
                brk     $18

                .asciiz "Exp range"

LBF1C:          lda     DP_ART_FPA+2
                cmp     #$87
                bcc     @LBF31
                bne     @LBF2A
                ldy     DP_ART_FPA+3
                cpy     #$b3
                bcc     @LBF31
@LBF2A:         lda     DP_ART_FPA
                bpl     brk_18_ExpRange
                jmp     clrFPA

@LBF31:         phk
                jsr     LB6DD
                ldx     #<fpConst_0_07121
                lda     #<fpConst_1_2
                ldy     #$03
                phk
                jsr     qryqryDoSomatWiConstAandX_Ytimes
                phk
                jsr     LBCCE
                lda     #<fpConst_e
                phk
                jsr     FPAeqConstA
                lda     $54
                phk
                jsr     LBD19
                bra     PtrCtoDP7A

qryqryDoSomatWiConstAandX_Ytimes:
                sty     $52
                stx     $58
                ldx     DP_ART_FPA+2
                cpx     #$40
                bcc     FPAeqConstA
                phk
                jsr     FPAeq1divFPA
                phk
                jsr     LBCD2
                lda     $58
                phk
                jsr     PtrCeqFPConstA
                phk
                jsr     FPAeqFPAminusPtrCround7
@LBF6D:         phk
                jsr     @LBF7D
                phk
                jsr     PtrCeqDP70
                phk
                jsr     FPAeqFPAminusPtrCround7
                dec     $52
                bne     @LBF6D
@LBF7D:         lda     #>fpConst_MinPiDiv2
                sta     DP_ART_PtrC+1
                pha                                     ;TODO: pointless?
                phk
                pla
                sta     DP_ART_PtrC+2
                pla                                     ;TODO: pointless?
                phk
                jsr     add5ToConstPtrLSB
                phk
                jsr     divPtrCByFPA
                phk
                jsr     add5ToConstPtrLSB
                jmp     FPAeqFPAminusPtrCround7

FPAeqConstA:    phk
                jsr     PtrCeqFPConstA
                jmp     unpackPtrCtoFPA

LBF9D:          rtl

const2PtrCandSomething:
                ldy     #>fpConst_MinPiDiv2
                phx
                phk
                plx
                jmp     LBF02

                rtl                                     ;TODO: DEAD?

copyYpl1PtrAtoPtrB:
                lda     [DP_ART_PtrA],y
                sta     [DP_ART_PtrB],y
                dey
                bpl     copyYpl1PtrAtoPtrB
                rts

swapPtrAandPtrB:
                pha
                lda     DP_ART_PtrA
                pha
                lda     DP_ART_PtrA+1
                pha
                lda     DP_ART_PtrA+2
                pha
                lda     DP_ART_PtrB
                sta     DP_ART_PtrA
                lda     DP_ART_PtrB+1
                sta     DP_ART_PtrA+1
                lda     DP_ART_PtrB+2
                sta     DP_ART_PtrA+2
                pla
                sta     DP_ART_PtrB+2
                pla
                sta     DP_ART_PtrB+1
                pla
                sta     DP_ART_PtrB
                pla
                rts

copyWKSPtoPtrB: pha
                lda     DP_ART_WKSPptr
                sta     DP_ART_PtrB
                lda     DP_ART_WKSPptr+1
                sta     DP_ART_PtrB+1
                lda     DP_ART_WKSPptr+2
                sta     DP_ART_PtrB+2
                pla
                rts

                lda     DP_ART_PtrA                     ;TODO: dead code
                clc
                adc     #$05
                sta     DP_ART_PtrA
                lda     DP_ART_PtrA+1
                adc     #$00
                sta     DP_ART_PtrA+1
                lda     DP_ART_PtrA+2
                adc     #$00
                sta     DP_ART_PtrA+2
                rtl

LBFF3:          phk
                jsr     LBDE7
                bne     @LBFFC
                brl     LBEEC

@LBFFC:         bpl     @LC001
                brl     brk_15_nve_root

@LC001:         lda     DP_ART_FPA+2
                lsr     A
                php
                adc     #$41
                sta     DP_ART_FPA+2
                plp
                bcc     @LC016
                lsr     DP_ART_FPA+3
                ror     DP_ART_FPA+4
                ror     DP_ART_FPA+5
                ror     DP_ART_FPA+6
                ror     DP_ART_FPA+7
@LC016:         phk
                jsr     clrFPB
                stz     DP_ART_FPTMP
                stz     DP_ART_FPTMP+1
                stz     DP_ART_FPTMP+2
                stz     DP_ART_FPTMP+3
                lda     #$40
                sta     DP_47_QRY+1
                sta     $4d
                ldx     #$00
                ldy     #$10
                sec
                lda     DP_ART_FPA+3
                sbc     #$40
                sta     DP_ART_FPA+3
@LC033:         tya
                eor     DP_47_QRY+1,x
                sta     $4d,x
                lda     DP_ART_FPA+3
                cmp     $4d
                bne     @LC04D
                phx
                ldx     #$00
@LC041:         lda     DP_ART_FPA+4,x
                cmp     DP_ART_FPTMP,x
                bne     @LC04C
                inx
                cpx     #$04
                bne     @LC041
@LC04C:         plx
@LC04D:         bcc     @LC078
                lda     DP_ART_FPA+7
                sbc     DP_ART_FPTMP+3
                sta     DP_ART_FPA+7
                lda     DP_ART_FPA+6
                sbc     DP_ART_FPTMP+2
                sta     DP_ART_FPA+6
                lda     DP_ART_FPA+5
                sbc     DP_ART_FPTMP+1
                sta     DP_ART_FPA+5
                lda     DP_ART_FPA+4
                sbc     DP_ART_FPTMP
                sta     DP_ART_FPA+4
                lda     DP_ART_FPA+3
                sbc     $4d
                sta     DP_ART_FPA+3
                tya
                asl     A
                bcc     @LC07C
                inc     A
                eor     DP_47_QRY,x
                sta     DP_47_QRY,x
                sta     DP_ART_FPB+7,x
@LC078:         lda     DP_47_QRY+1,x
                bra     @LC080

@LC07C:         eor     DP_47_QRY+1,x
                sta     DP_47_QRY+1,x
@LC080:         sta     $4d,x
                asl     DP_ART_FPA+7
                rol     DP_ART_FPA+6
                rol     DP_ART_FPA+5
                rol     DP_ART_FPA+4
                rol     DP_ART_FPA+3
                tya
                lsr     A
                tay
                bcc     @LC033
                ldy     #$80
                inx
                cpx     #$05
                bne     @LC033
                phk
                jsr     copyFPBmantToFPA
                lda     DP_ART_FPA+3
                bmi     @LC0A4
                phk
                jsr     normFPAmant3
@LC0A4:         phk
                jsr     FPAmantRound7
                lda     #$ff
                rtl

LC0AB:          lda     DP_ART_FPA+2
                cmp     #$98
                bcc     @LC0B4
                brl     LC173

@LC0B4:         phk
                jsr     LBCD2
                phk
                jsr     PtrCeqPIdiv2
                phk
                jsr     unpackPtrCtoFPB
                lda     DP_ART_FPA
                sta     DP_ART_FPB
                dec     DP_47_QRY
                phk
                jsr     FPAeqFPAminusFPBround7
                lda     #<fpConst_2DivPi
                phk
                jsr     const2PtrCandSomething
                phk
                jsr     FPAtoINTAasint
                sta     $54
                ora     DP_ART_INTA+1
                ora     DP_ART_INTA+2
                beq     @LC10E
                phk
                jsr     LB54E
                lda     #DP_75_QRY
                phk
                jsr     LBCD4
                lda     #<fpConst_MinPiDiv2
                phk
                jsr     const2PtrCandSomething
                phk
                jsr     PtrCeqDP70
                phk
                jsr     FPAeqFPAminusPtrCround7
                phk
                jsr     LBCE3
                lda     #DP_75_QRY
                phk
                jsr     LBC90
                lda     #<fpConst_4_454e_6
                phk
                jsr     const2PtrCandSomething
                phk
                jsr     PtrCeqDP70
                phk
                jsr     FPAeqFPAminusPtrCround7
                bra     @LC112

@LC10E:         phk
                jsr     LBC8E
@LC112:         phk
                jsr     LBCCE
                phk
                jsr     LB9FD
                ldx     #<fpConst_min_0_011909
                lda     #<fpConst_1_0
                ldy     #$02
                phk
                jsr     qryqryDoSomatWiConstAandX_Ytimes
                brl     PtrCtoDP7A

LC127:          lda     #$02
                bit     $54
                beq     @LC134
                phk
                jsr     @LC134
                jmp     LBC53

@LC134:         lda     $54
                lsr     A
                bcs     LC13C
LC139:          lda     #$ff
                rtl

LC13C:          phk
                jsr     LBCD2
                phk
                jsr     LB9FD
                lda     #<fpConst_1_0
                phk
                jsr     PtrCeqFPConstA
                phk
                jsr     LB9E9
                jmp     LBFF3

brk_06_TypeMismatch:
                brk     $06

                .byte   "Type mismatch"

brk_12_DivisionByZero:
                brk     $12

                .byte   "Division by zero"
                .byte   $00

LC173:          brk     $17

                .byte   "Accuracy lost"
                .byte   $00

brk_42_BadFN:   brk     $42

                .byte   "Error!!!"
                .byte   $00

call_MM:        jsl     _MM
                rts

; CRC
                .byte   $d2
                .byte   $8e
                .byte   $ff
