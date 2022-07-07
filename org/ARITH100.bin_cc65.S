; Target assembler: cc65 v2.18.0 [--target none -C ARITH100.bin_cc65.cfg]
; 6502bench SourceGen v1.7.3
                .setcpu "65816"
_HDMMT          =       2                               ;Task Memory Pool Handle
_MMASA          =       $0a                             ;Allocate Small Ascending
COP_0F_OPERR    =       $0f                             ;Throw an error
COP_10_OPADP    =       $10                             ;Allocate Direct Page, followed by 2-byte inline size
RETV_INT        =       $40
RETV_REAL       =       $ff

DP_ART_PtrA     =       $06
DP_ATL_X_SAV    =       $30
DP_ART_INTA     =       $34
DP_ART_FPA      =       $38
DP_43_QRY       =       $43                             ;Used to store INT?
DP_ART_FPB      =       $45
DP_47_QRY       =       $47                             ;INT wkspa?
DP_ART_FPTMP    =       $4d
DP_52_QRY       =       $52
DP_53_QRY       =       $53
DP_ART_thetaQuadrant =  $54
DP_ART_PtrC     =       $55
DP_58_QRY       =       $58
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
                .word   do_FNnul
                .word   do_FNintADD
                .word   do_FNintSUB
                .word   do_FNINTnegate                  ;8
                .word   do_FNint16mulint32
                .word   do_FNmul
                .word   do_FNdiv
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
                .word   do_FNsub
                .word   do_FNcompare
                .word   do_FNYtoreal
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

do_FNnul:       clc
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
do_FNintADD:    jsr     copyWKSPtoPtrB
                ldy     #$07
                jsr     copyYpl1PtrAtoPtrB
                lda     #$04
                jsr     addAtoPtrA
                jsr     swapPtrAandPtrB
                phk
                jsr     copy4atPtrAtoINTAincPtr4
                phk
                jsr     INTAeqINTAplusPTRA
                jsr     swapPtrAandPtrB
                lda     #$04
                jsr     addAtoPtrA
                jsr     storeINTAatPtrA
                rtl

do_FNintSUB:    jsr     copyWKSPtoPtrB
                ldy     #$07
                jsr     copyYpl1PtrAtoPtrB
                lda     #$04
                jsr     addAtoPtrA
                jsr     swapPtrAandPtrB
                phk
                jsr     copy4atPtrAtoINTAincPtr4
                phk
                jsr     INTAeqPtrAsubINTA
                jsr     swapPtrAandPtrB
                lda     #$04
                jsr     addAtoPtrA
                jsr     storeINTAatPtrA
                rtl

do_FNINTnegate: jsr     ptrBpointAtDP_INTA
                ldy     #$03
                jsr     copyYpl1PtrAtoPtrB
                jsr     swapPtrAandPtrB
                phk
                jsr     INTAeqMinusINTA
                jsr     swapPtrAandPtrB
                lda     #$04
                jsr     addAtoPtrA
                jsr     storeINTAatPtrA
                rtl

do_FNint16mulint32:
                jsr     copyWKSPtoPtrB
                ldy     #$07
                jsr     copyYpl1PtrAtoPtrB
                lda     #$04
                jsr     addAtoPtrA
                jsr     swapPtrAandPtrB
                phk
                jsr     copy4atPtrAtoINTAincPtr4
                phk
                jsr     INTAeqPtrAmulINTA16
                jsr     swapPtrAandPtrB
                lda     #$04
                jsr     addAtoPtrA
                jsr     storeINTAatPtrA
                rtl

do_FNmul:       jsr     copyInToStackAndUnpackToFPA
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
                jsr     FPAeqFPAmulFPB
                phk
                jsr     FPAmantRound7
packFPAtoPtrBswapPtrAPtrB:
                jsr     swapPtrAandPtrB                 ;TODO: optimize - this seems like a pissing about
                lda     #$05
                jsr     addAtoPtrA
                jsr     ptrAdec5packFPAtoPtrA
                rtl

do_FNdiv:       jsr     copyInToStackAndUnpackToFPA
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
doSinCos_internal:
                php
                jsr     copyInToStackAndUnpackToFPA
                plp
                php
                phk
                jsr     trigNormaltheta
                plp
                phk
                jsr     doSINCOS_internal
                bra     packFPAtoPtrBswapPtrAPtrB

do_FNcos:       sec
                bra     doSinCos_internal

do_FNtan:       jsr     copyInToStackAndUnpackToFPA
                phk
                jsr     trigNormaltheta
                phk
                jsr     doTan_internal
                bra     packFPAtoPtrBswapPtrAPtrB

do_FNacs:       jsr     copyInToStackAndUnpackToFPA
                phk
                jsr     do_acs_internal
                brl     packFPAtoPtrBswapPtrAPtrB

do_FNasn:       jsr     copyInToStackAndUnpackToFPA
                phk
                jsr     do_asn_internal
                brl     packFPAtoPtrBswapPtrAPtrB

do_FNatn:       jsr     copyInToStackAndUnpackToFPA
                phk
                jsr     do_atn_internal
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
                jsr     fpFPAeq_sqrt_FPA
                brl     packFPAtoPtrBswapPtrAPtrB

do_FNexp:       jsr     copyInToStackAndUnpackToFPA
                phk
                jsr     LB9DB
                brl     packFPAtoPtrBswapPtrAPtrB

do_FNsub:       jsr     copyInToStackAndUnpackToFPA
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

do_FNYtoreal:   phy
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
skLB364:        stz     DP_ART_PtrB+2
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
                bra     skLB364

                rts                                     ;TODO: dead code

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
                bcs     @Sk
                dec     DP_ART_PtrA+1
                bcc     @Sk                             ;TODO: bug!

                dec     DP_ART_PtrA+2
@Sk:            rts

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

INTAeqINTAplusPTRA:
                clc
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
staINTA3_retINTV:
                sta     DP_ART_INTA+3
                lda     #RETV_INT
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

INTAeqPtrAsubINTA:
                sec
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
                bra     staINTA3_retINTV

INTAeqMinusINTA:
                sec
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
rtlRETV_INT:    lda     #RETV_INT
                rtl

INTAeqMagINTA:  bit     DP_ART_INTA+3
                bpl     jmpRtlRETV_INT
                jmp     INTAeqMinusINTA

jmpRtlRETV_INT: jmp     rtlRETV_INT

INTAeqPtrAmulINTA16:
                ldy     DP_ART_INTA+1
                phy
                phk
                jsr     INTAeqMagINTA
                stx     DP_ATL_X_SAV
                ldx     #DP_43_QRY
                phk
                jsr     stINTAatDPX
                phk
                jsr     copy4atPtrAtoINTAincPtr4
                pla
                eor     DP_ART_INTA+3
                sta     $41
                phk
                jsr     INTAeqMagINTA
                ldy     #$00
                ldx     #$00
                stz     DP_ART_FPB+4
                stz     DP_ART_FPB+5
@lp:            lsr     DP_43_QRY+1
                ror     DP_43_QRY
                bcc     @shlINTA
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
@shlINTA:       asl     DP_ART_INTA
                rol     DP_ART_INTA+1
                rol     DP_ART_INTA+2
                rol     DP_ART_INTA+3
                lda     DP_43_QRY
                ora     DP_43_QRY+1
                bne     @lp
                sty     DP_47_QRY
                stx     DP_47_QRY+1
                lda     $41
                php
                ldx     #DP_47_QRY
                phk
                jsr     storeDPXatINTA
                plp
                bpl     @negsk
                phk
                jsr     INTAeqMinusINTA
@negsk:         ldx     DP_ATL_X_SAV
                rtl

FPAeqFPAmulFPB: lda     DP_ART_FPA+3
                bne     @sk
                jmp     rtlLBAD7

@sk:            phk
                jsr     unpackPtrCtoFPB
                bne     @sk2
                jmp     clrFPA

@sk2:           phk
                plb
                clc
                lda     DP_ART_FPA+2                    ;TODO: cf 6809 basic use 16bits
                adc     DP_ART_FPB+2
                rol     DP_ART_FPA+1
                sbc     #$7f
                sta     DP_ART_FPA+2
                bcs     @sk3
                dec     DP_ART_FPA+1
@sk3:           phx
                ldx     #$05
                ldy     #$00
@cpylp:         lda     DP_ART_FPA+2,x
                sta     DP_ART_FPTMP-1,x
                sty     DP_ART_FPA+2,x
                dex
                bne     @cpylp
                lda     DP_ART_FPA
                eor     DP_ART_FPB
                sta     DP_ART_FPA
                ldy     #$20
@lp:            lsr     DP_ART_FPB+3
                ror     DP_ART_FPB+4
                ror     DP_ART_FPB+5
                ror     DP_ART_FPB+6
                ror     DP_ART_FPB+7
                asl     DP_ART_FPTMP+3
                rol     DP_ART_FPTMP+2
                rol     DP_ART_FPTMP+1
                rol     DP_ART_FPTMP
                bcc     @skadd
                clc
                phk
                jsr     @FPAmantaddFPBmant
@skadd:         dey
                bne     @lp
                plx
                lda     DP_ART_FPA+3
                bpl     @normret
                jmp     rtlLBAD7

@normret:       jmp     normFPAmant3

                .byte   $6b

@FPAmantaddFPBmant:
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
                adc     DP_ART_FPB+3
                sta     DP_ART_FPA+3
                rtl

intA2RealFPA:   stz     DP_ART_FPA+7
                stz     DP_ART_FPA+1
LB54E:          lda     DP_ART_INTA+3
                sta     DP_ART_FPA
                bpl     @LB55A
                phk
                jsr     INTAeqMinusINTA
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
                stz     DP_ART_thetaQuadrant
                jmp     fpCheckMant0SetSignExp0

@LB6E6:         phk
                jsr     LB671
                lda     DP_ART_FPA+6
                sta     DP_ART_thetaQuadrant
                phk
                jsr     copyFPBmantToFPA
                lda     #$80
                sta     DP_ART_FPA+2
                ldx     DP_ART_FPA+3
                bpl     @LB70A
                eor     DP_ART_FPA
                sta     DP_ART_FPA
                bpl     @LB704
                inc     DP_ART_thetaQuadrant
                bra     @LB706

@LB704:         dec     DP_ART_thetaQuadrant
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
                stx     DP_ART_FPTMP+1,y
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
                lda     DP_ART_FPTMP+1
                sta     DP_ART_FPA+6
                lda     DP_ART_FPTMP+2
                sta     DP_ART_FPA+5
                lda     DP_ART_FPTMP+3
                sta     DP_ART_FPA+4
                lda     DP_ART_FPTMP+4
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

doSINCOS_internal:
                bcc     @skSIN
                inc     DP_ART_thetaQuadrant
@skSIN:         phk
                jsr     doSINCOS_internal2              ;TODO: jmp FFS
                rtl

doTan_internal: lda     #DP_7F_QRY
                phk
                jsr     FPAcopyNormToDPA
                phk
                jsr     doSINCOS_internal2
                lda     #DP_7A_QRY
                phk
                jsr     FPAcopyNormToDPA
                lda     #DP_7F_QRY
                phk
                jsr     unpackDPAtoFPA
                inc     DP_ART_thetaQuadrant
                phk
                jsr     doSINCOS_internal2
divDP7AbyFPA:   lda     #DP_7A_QRY
                phk
                jsr     PtrCeqDPA
                phk
                jsr     divPtrCByFPA
                lda     #RETV_REAL
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

do_acs_internal:
                phk
                jsr     do_asn_internal
                phk
                jsr     FPAeqminusFPAminusPIdiv2
                rtl

do_asn_internal:
                lda     DP_ART_FPA
                bpl     do_asn_internal2
                stz     DP_ART_FPA
                phk
                jsr     do_asn_internal2
                lda     #RETV_REAL
                sta     DP_ART_FPA
                rtl

do_asn_internal2:
                phk
                jsr     FPAcopyNormToDP7A
                phk
                jsr     doSINCOS_internal3
                lda     DP_ART_FPA+3
                beq     FPAeqPIdiv2
                phk
                jsr     divDP7AbyFPA
                phk
                jsr     do_atn_internal2
                rtl

FPAeqPIdiv2:    lda     #<fpConst_PIdiv2
                phk
                jsr     FPAeqConstA
                rtl

do_atn_internal:
                phk                                     ;TODO: just drop through FFS?
                jsr     do_atn_internal2
                rtl

do_atn_internal2:
                phk
                jsr     fpCheckMant0SetSignExp0
                bne     @sk
                brl     retRETV_REAL

@sk:            bpl     @sk2
                stz     DP_ART_FPA
                phk
                jsr     @sk2
                sta     DP_ART_FPA
                rtl

@sk2:           lda     DP_ART_FPA+2
                cmp     #$81
                bcc     do_atn_internal3
                phk
                jsr     FPAeq1divFPA
                phk
                jsr     do_atn_internal3
FPAeqminusFPAminusPIdiv2:
                phk
                jsr     PtrCeqPIdiv2
                phk
                jsr     FPAeqminusFPAminusPtrCround7
                lda     #RETV_REAL
                rtl

do_atn_internal3:
                lda     DP_ART_FPA+2
                cmp     #$73
                bcs     @sk
                brl     retRETV_REAL

@sk:            phk
                jsr     FPAcopyNormToDP7A
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
                jsr     trigMagicWiConstAandX_Ytimes
                jmp     PtrCtoDP7A

setPtrCConstRad2Deg:
                lda     #<fpConst_Rad2Deg
                phk
                jsr     FPAmulbyPiDiv2
                rtl

setPtrCConstDeg2Rad:
                lda     #<fpConst_Deg2Rad
                phk
                jsr     FPAmulbyPiDiv2
                rtl

LB9CB:          phk
                jsr     LBE76
                lda     #<fpConst_0_43429
                phk
                jsr     FPAmulbyPiDiv2
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

FPAeqminusFPAminusPtrCround7:
                phk
                jsr     FPAnegate
FPAeqFPAminusPtrCround7:
                phk
                jsr     unpackPtrCtoFPB
                bne     FPAeqFPAminusFPBround7
                jmp     rtlLBAD7

FPAeqFPAminusFPBround7:
                phk
                jsr     FPAeqFPAminusFPB
                jmp     FPAmantRound7

FPAeqFPAmulFPBandRound7:
                phk
                jsr     FPAeqFPAmulFPB
                jmp     FPAmantRound7

                phk                                     ;TODO: dead code?
                jsr     intA2RealFPA
                phk
                jsr     copy4atPtrAtoINTAincPtr4
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
                jsr     FPAeqFPAmulFPBandRound7
                lda     #RETV_REAL
                rtl

                jmp     brk_06_TypeMismatch             ;TODO: dead code?

LBA31:          lda     DP_ART_FPA+2
                cmp     #$87
                bcs     LBA90
                phk
                jsr     LB6DD
                bne     @LBA4F
                phk
                jsr     ptrCeqptrAPtrAinc5
                phk
                jsr     unpackPtrCtoFPA
                phk
                plb
                lda     DP_ART_thetaQuadrant
                phk
                jsr     LBD19
                bra     rtlRETV_REAL2

@LBA4F:         phk
                jsr     FPAcopyNormToDP7A
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
                lda     DP_ART_thetaQuadrant
                phk
                jsr     LBD19
LBA6B:          lda     #DP_75_QRY
                phk
                jsr     FPAcopyNormToDPA
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
rtlRETV_REAL2:  lda     #RETV_REAL
                rtl

LBA90:          phk
                jsr     FPAcopyNormToDP7A
                phk
                jsr     LBD36
                bra     LBA6B

add5ToConstPtrLSB:
                clc
                lda     DP_58_QRY
                adc     #$05
                sta     DP_58_QRY
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
                bra     PtrCeqDPA

PtrCeqDP70:     lda     #DP_70_QRY                      ;TODO: DP pointer
PtrCeqDPA:      phd
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
                lda     #RETV_INT
                sec
                rtl

@LBB2B:         lda     #$a8
                sta     DP_ART_FPA+2
                stz     DP_ART_FPA+1
                stz     DP_ART_FPA
                phk
                jsr     normFPAmant2
                lda     DP_53_QRY
                bmi     @LBB47
                beq     @LBB4F
@LBB3D:         phk
                jsr     LBB6A
                dec     DP_53_QRY
                bne     @LBB3D
                bra     @LBB4F

@LBB47:         phk
                jsr     LBBAE
                inc     DP_53_QRY
                bne     @LBB47
@LBB4F:         phk
                jsr     FPAmantRound7
                sec
                lda     #RETV_REAL
                rtl

storeDPXatINTA: lda     $00,x                           ;TODO: optimize - only called once inline?
                sta     DP_ART_INTA
                lda     $01,x
                sta     DP_ART_INTA+1
                lda     $02,x
                sta     DP_ART_INTA+2
                lda     $03,x
                sta     DP_ART_INTA+3
                lda     #RETV_INT
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
                jsr     FPAeqminusFPAminusPtrCround7
FPAnegate:      lda     DP_ART_FPA+3
                beq     @sk
                lda     DP_ART_FPA
                eor     #$80
                sta     DP_ART_FPA
@sk:            lda     #RETV_REAL
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

unpackDP70toFPA:
                lda     #DP_70_QRY
unpackDPAtoFPA: phd
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

FPAcopyNormToDP7A:
                lda     #DP_7A_QRY
                bra     FPAcopyNormToDPA

FPAcopyNormToDP70:
                lda     #DP_70_QRY
FPAcopyNormToDPA:
                phd
                clc
                adc     $01,S
                sta     DP_ART_PtrC
                lda     $02,S
                adc     #$00
                sta     DP_ART_PtrC+1
                stz     DP_ART_PtrC+2
                pld
FPAcopyNormToPtrC:
                lda     DP_ART_FPA+2
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

@LBD16:         jmp     rtlLBF9D

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
                jsr     FPAcopyNormToDP70
                dex
                beq     @LBD35
@LBD2E:         phk
                jsr     FPAeqFPAmulFPBandRound7
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
                jsr     rtlLBF9D
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
                jsr     rtlLBF9D
                lda     DP_ATL_X_SAV
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

fpCheckMant0SetSignExp0:
                lda     DP_ART_FPA+3
                ora     DP_ART_FPA+4
                ora     DP_ART_FPA+5
                ora     DP_ART_FPA+6
                ora     DP_ART_FPA+7
                beq     @sk
                lda     DP_ART_FPA
                bne     @rtl
                inc     A
                rtl

@sk:            stz     DP_ART_FPA
                stz     DP_ART_FPA+2
                stz     DP_ART_FPA+1
@rtl:           rtl

stINTAatDPX:    lda     DP_ART_INTA                     ;TODO: optimize - only called once - inline?
                sta     $00,x
                lda     DP_ART_INTA+1
                sta     $01,x
                lda     DP_ART_INTA+2
                sta     $02,x
                lda     DP_ART_INTA+3
                sta     $03,x
                rtl

copy4atPtrAtoINTAincPtr4:
                ldy     #$03
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
PtrAinc4:       clc
                lda     #$04
                adc     DP_ART_PtrA
                sta     DP_ART_PtrA
                bcc     @rtl
                inc     DP_ART_PtrA+1
                bcc     @rtl
                inc     DP_ART_PtrA+2
@rtl:           rtl

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
                bra     PtrAinc4

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
                jsr     fpCheckMant0SetSignExp0
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
                jsr     FPAcopyNormToDPA
                ldx     #<fpConst_0_54625
                lda     #<fpConst_min_0_5
                ldy     #$02
                phk
                jsr     trigMagicWiConstAandX_Ytimes
                lda     #DP_7F_QRY
                phk
                jsr     PtrCtoDPinA
                phk
                jsr     FPAeqFPAmulFPBandRound7
                phk
                jsr     FPAeqFPAminusPtrCround7
                phk
                jsr     FPAcopyNormToDP70
                pla
                sec
                sbc     #$81
                phk
                jsr     LB59B
                lda     #<fpConst_ln_2
                phk
                jsr     FPAmulbyPiDiv2
                phk
                jsr     PtrCeqDP70
                phk
                jsr     FPAeqFPAminusPtrCround7
rtlRETV_REAL:   lda     #RETV_REAL
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
                bra     FPAeqFPAmulFPBandRound7retREAL

XYAtoPTRCFPAmulFPBround:
                sty     DP_ART_PtrC+1
                sta     DP_ART_PtrC
                stx     DP_ART_PtrC+2
                plx
FPAeqFPAmulFPBandRound7retREAL:
                phk
                jsr     FPAeqFPAmulFPBandRound7
                lda     #RETV_REAL
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
                jsr     trigMagicWiConstAandX_Ytimes
                phk
                jsr     FPAcopyNormToDP7A
                lda     #<fpConst_e
                phk
                jsr     FPAeqConstA
                lda     DP_ART_thetaQuadrant
                phk
                jsr     LBD19
                bra     PtrCtoDP7A

trigMagicWiConstAandX_Ytimes:
                sty     DP_52_QRY
                stx     DP_58_QRY
                ldx     DP_ART_FPA+2
                cpx     #$40
                bcc     FPAeqConstA
                phk
                jsr     FPAeq1divFPA
                phk
                jsr     FPAcopyNormToDP70
                lda     DP_58_QRY
                phk
                jsr     PtrCeqFPConstA
                phk
                jsr     FPAeqFPAminusPtrCround7
@lp:            phk
                jsr     @sk
                phk
                jsr     PtrCeqDP70
                phk
                jsr     FPAeqFPAminusPtrCround7
                dec     DP_52_QRY
                bne     @lp
@sk:            lda     #>fpConst_MinPiDiv2
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

rtlLBF9D:       rtl

FPAmulbyPiDiv2: ldy     #>fpConst_MinPiDiv2
                phx
                phk
                plx
                jmp     XYAtoPTRCFPAmulFPBround

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

fpFPAeq_sqrt_FPA:
                phk
                jsr     fpCheckMant0SetSignExp0
                bne     @sk
                brl     rtlRETV_REAL

@sk:            bpl     @sk2
                brl     brk_15_nve_root

@sk2:           lda     DP_ART_FPA+2
                lsr     A
                php
                adc     #$41
                sta     DP_ART_FPA+2
                plp
                bcc     @sk6
                lsr     DP_ART_FPA+3
                ror     DP_ART_FPA+4
                ror     DP_ART_FPA+5
                ror     DP_ART_FPA+6
                ror     DP_ART_FPA+7
@sk6:           phk
                jsr     clrFPB
                stz     DP_ART_FPTMP+1
                stz     DP_ART_FPTMP+2
                stz     DP_ART_FPTMP+3
                stz     DP_ART_FPTMP+4
                lda     #$40
                sta     DP_ART_FPB+3
                sta     DP_ART_FPB+8
                ldx     #$00
                ldy     #$10
                sec
                lda     DP_ART_FPA+3
                sbc     #$40
                sta     DP_ART_FPA+3
@lp1:           tya
                eor     DP_ART_FPB+3,x
                sta     DP_ART_FPTMP,x
                lda     DP_ART_FPA+3
                cmp     DP_ART_FPTMP
                bne     @sk21
                phx
                ldx     #$00
@lp2:           lda     DP_ART_FPA+4,x
                cmp     DP_ART_FPTMP+1,x
                bne     @sk1
                inx
                cpx     #$04
                bne     @lp2
@sk1:           plx
@sk21:          bcc     @sk3
                lda     DP_ART_FPA+7
                sbc     DP_ART_FPTMP+4
                sta     DP_ART_FPA+7
                lda     DP_ART_FPA+6
                sbc     DP_ART_FPTMP+3
                sta     DP_ART_FPA+6
                lda     DP_ART_FPA+5
                sbc     DP_ART_FPTMP+2
                sta     DP_ART_FPA+5
                lda     DP_ART_FPA+4
                sbc     DP_ART_FPTMP+1
                sta     DP_ART_FPA+4
                lda     DP_ART_FPA+3
                sbc     DP_ART_FPTMP
                sta     DP_ART_FPA+3
                tya
                asl     A
                bcc     @sk4
                inc     A
                eor     DP_ART_FPB+2,x
                sta     DP_ART_FPB+2,x
                sta     DP_ART_FPB+7,x
@sk3:           lda     DP_ART_FPB+3,x
                bra     @sk5

@sk4:           eor     DP_47_QRY+1,x
                sta     DP_47_QRY+1,x
@sk5:           sta     DP_ART_FPTMP,x
                asl     DP_ART_FPA+7
                rol     DP_ART_FPA+6
                rol     DP_ART_FPA+5
                rol     DP_ART_FPA+4
                rol     DP_ART_FPA+3
                tya
                lsr     A
                tay
                bcc     @lp1
                ldy     #$80
                inx
                cpx     #$05
                bne     @lp1
                phk
                jsr     copyFPBmantToFPA
                lda     DP_ART_FPA+3
                bmi     @normRetFPA
                phk
                jsr     normFPAmant3
@normRetFPA:    phk
                jsr     FPAmantRound7
                lda     #RETV_REAL
                rtl

trigNormaltheta:
                lda     DP_ART_FPA+2
                cmp     #$98
                bcc     @sk
                brl     brk_17_accuracyLost

@sk:            phk
                jsr     FPAcopyNormToDP70
                phk
                jsr     PtrCeqPIdiv2
                phk
                jsr     unpackPtrCtoFPB
                lda     DP_ART_FPA
                sta     DP_ART_FPB
                dec     DP_47_QRY
                phk
                jsr     FPAeqFPAminusFPBround7
                lda     #<fpConst_2DivPi                ;TODO: not needed?
                phk
                jsr     FPAmulbyPiDiv2
                phk
                jsr     FPAtoINTAasint
                sta     DP_ART_thetaQuadrant
                ora     DP_ART_INTA+1
                ora     DP_ART_INTA+2
                beq     @sk0theta
                phk
                jsr     LB54E
                lda     #DP_75_QRY
                phk
                jsr     FPAcopyNormToDPA
                lda     #<fpConst_MinPiDiv2
                phk
                jsr     FPAmulbyPiDiv2
                phk
                jsr     PtrCeqDP70
                phk
                jsr     FPAeqFPAminusPtrCround7
                phk
                jsr     FPAcopyNormToPtrC
                lda     #DP_75_QRY
                phk
                jsr     unpackDPAtoFPA
                lda     #<fpConst_4_454e_6
                phk
                jsr     FPAmulbyPiDiv2
                phk
                jsr     PtrCeqDP70
                phk
                jsr     FPAeqFPAminusPtrCround7
                bra     @sk2theta

@sk0theta:      phk
                jsr     unpackDP70toFPA
@sk2theta:      phk
                jsr     FPAcopyNormToDP7A
                phk
                jsr     FPAeqFPAmulFPBandRound7
                ldx     #<fpConst_min_0_011909
                lda     #<fpConst_1_0
                ldy     #$02
                phk
                jsr     trigMagicWiConstAandX_Ytimes
                brl     PtrCtoDP7A

doSINCOS_internal2:
                lda     #$02
                bit     DP_ART_thetaQuadrant
                beq     @sk1
                phk
                jsr     @sk1
                jmp     FPAnegate

@sk1:           lda     DP_ART_thetaQuadrant
                lsr     A
                bcs     doSINCOS_internal3
retRETV_REAL:   lda     #RETV_REAL
                rtl

doSINCOS_internal3:
                phk
                jsr     FPAcopyNormToDP70
                phk
                jsr     FPAeqFPAmulFPBandRound7
                lda     #<fpConst_1_0
                phk
                jsr     PtrCeqFPConstA
                phk
                jsr     FPAeqminusFPAminusPtrCround7
                jmp     fpFPAeq_sqrt_FPA

brk_06_TypeMismatch:
                brk     $06

                .byte   "Type mismatch"

brk_12_DivisionByZero:
                brk     $12

                .byte   "Division by zero"
                .byte   $00

brk_17_accuracyLost:
                brk     $17

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
