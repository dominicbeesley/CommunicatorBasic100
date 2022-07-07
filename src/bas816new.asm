

;*******************************************************************************
;* Disassembly and annotations by Dominic Beesley November 2020                *
; This was taken originally from a Disassembly of Communicator BASIC v100      *
;*******************************************************************************
                .setcpu "65816"
                .smart

                .include "bas816new.inc"
        .IFDEF BLITTER
                .include "bas816new_BLITTER.inc"
        .ENDIF

        .IFDEF BEEB816
                .include "bas816new_BEEB816.inc"
        .ENDIF

        .IFDEF DOSSY
                .include "bas816new_DOSSY.inc"
        .ENDIF

        .IFDEF MOS 
                .export list_printHexByte
                .export printStringAfter
                .export call_OSWRCH

                .import arith_enter
                .import arith_init
moduleCallARITHref = arith_enter
                
        .ENDIF

                .CODE


        .IFDEF MOS



;DEBUG and TODO macros

        .macro DEBUG str
                .local @DEBUGstraddr, @DEBUGcallit
                .segment "DEBUG"
@DEBUGstraddr:  .byte str,0
@DEBUGcallit:   pha
                lda     #^@DEBUGstraddr
                sta     DP_BAS_BL_DEBUGPTR+2
                lda     #>@DEBUGstraddr
                sta     DP_BAS_BL_DEBUGPTR+1
                lda     #<@DEBUGstraddr
                sta     DP_BAS_BL_DEBUGPTR+0
                jsr     BL_debug_print
                pla
                rts
                .code
                jsr     @DEBUGcallit
        .endmacro

        .macro TODO str
                .local @TODOhere
                DEBUG str
        @TODOhere:   bra @TODOhere
        .endmacro

        .ENDIF


        .IFDEF COMMUNICATOR
                .a16
                .i16
                brl     L4B1B

                .word   $52ef                           ;TODO - module length, calculate
                .byte   $00                             ;module length top byte
                .word   $0100                           ;Version
                .byte   $02
                .byte   $01
                .byte   $00
                .byte   $00
                .byte   $00
                .byte   $00
                .byte   "BASIC"
                .byte   $00
                .byte   "[size]"
                .byte   $00

L4B1B:          brl     ServiceDispatch

                .byte   $60
                .byte   $26
                .byte   $04
                .byte   "BASIC"
                .byte   $00
strCopyright:   .byte   "(C)1986 Acorn"
                .byte   $0a
                .byte   $0d
                .byte   $00
                .byte   $00
                .byte   $4b
        .ENDIF

        .IFDEF MOS
                brl     MOSStart
strCopyright:
                .byte   "65816 BASIC (c) 2020 Dossy",$0a,$0d,$00                
        .ENDIF


; Floating point constants - TODO: make macros?
; TODO: add assert to make not cross page boundary, constant loading code is
; page sensitive
fpConstMinPiDiv2:
                .byte   $81
                .byte   $c9
                .byte   $10
                .byte   $00
                .byte   $00
fpConst4_454e6: .byte   $6f
                .byte   $15
                .byte   $77
                .byte   $7a
                .byte   $61
fpConstPiDiv2:  .byte   $81
                .byte   $49
                .byte   $0f
                .byte   $da
                .byte   $a2
;*******************************************************************************
;* Following is a two-part table of Hi, Lo bytes of 16bit words that are       *
;* succcessively subtracted from INT WA to convert binary to decimal for       *
;* printing line numbers                                                       *
;*******************************************************************************
tblDigitsHi:    .byte   $00
                .byte   $00
                .byte   $00
                .byte   $03
                .byte   $27
tblDigitsLo:    .byte   $01
                .byte   $0a
                .byte   $64
                .byte   $e8
                .byte   $10
;*******************************************************************************
;* Following is a list of constants to form the workspace pointers given a     *
;* base address.                                                               *
;*******************************************************************************
tblWkspSizes:   .word   $0100                           ;MEMBASE to STRWKSP (+100)
                .word   $0100                           ;STRWKSP to INTVARS
                .word   $006c                           ;STRWKSP to VARS (+26C)
                .word   $00b7                           ;VARS to PRINTHASH (+323)
                .word   $0014                           ;PRINTHASH to FORSTACK (+337)
                .word   $00aa                           ;FORSTACK to GOSUBSTACK (+3E1)
                .word   $004e                           ;GOSUBSTACK to REPEATSTACK (+42F)
        .IFDEF COMMUNICATOR                
                .word   $003c                           ;REPEATSTACK to SPECIAL (+46B)
                .word   $001e                           ;SPECIAL TO PAGE (+500)
        .ELSE
                .word   $005a                           ;REPEATSTACK to PAGE (+500)
        .ENDIF
                .word   $0000

        .IFDEF COMMUNICATOR
;*******************************************************************************
;* Service call dispatcher                                                     *
;*******************************************************************************
ServiceDispatch:
                rep     #$30
                cpx     #$0008
                bcs     @ServiceUnk
                jsr     (tblServiceDispatch,x)
                clc
                ldx     #$0000
                jsl     _CWT
                bra     ServiceDispatch

@ServiceUnk:    phk
                plb
                lda     #strUkServ
                ldx     #$0002
                clc
                jsl     _CWT
                bra     ServiceDispatch

strUkServ:      .byte   "BASIC - unrecognised reason code",$0d,$00
;*******************************************************************************
;* Service call jump table 0..7                                                *
;*******************************************************************************
tblServiceDispatch:
                .word   ServiceCall_0_2
                .word   ServiceCall_0_2
                .word   ServiceCall_4
                .word   ServiceCall_6
;*******************************************************************************
;* Service Call 4                                                              *
;*******************************************************************************
;* Looks like it kills BASIC?                                                  *
;*******************************************************************************
ServiceCall_4:  jsr     HELPENV
                pla
                jsr     L4C14
                bcs     @L4BD0
                jsr     memFreeDP
                bcs     L4BF2
                lda     #$0000
                pha
                plb
                plb
                ldy     #$0004
                sta     ($01,S),y
                ldx     #$0000
                rtl

@L4BD0:         lda     #strCannotFindMemPool
L4BD3:          phk
                plb
                ldx     #$0002
                rtl

strCannotFindMemPool:
                .byte   "Cannot find memory pool",$0d
                .byte   $00

L4BF2:          lda     #strCannotFreeDP
                bra     L4BD3

strCannotFreeDP:
                .byte   "Cannot free direct page",$0d
                .byte   $00

                .a8
                .i8
memFreeDP:      tdc
                cop     COP_1A_OPFZB
                rts

L4C14:          rep     #$30
                .a16
                .i16
                lda     DP_BAS_MEMEND+2
                and     #$ff00
                pei     (DP_BAS_LOMEM_LIM_PAG)
                plb
                plb
                ldx     #$0014
                jsl     _MM
                bcs     @L4C2F
                ldx     #$0002
                jsl     _MM
@L4C2F:         rts

;*******************************************************************************
;* Service Call 0                                                              *
;*******************************************************************************
;* Args [size] [????]                                                          *
;*******************************************************************************
ServiceCall_0_2:
                phb
                pha
                cop     COP_11_OPADF
                .word   $0100                           ;Alloc 256 byte DP
                bcc     L4C6B
                phk
                plb
                lda     #ABRT_DP_FAIL
                ldx     #$0002
                sec
                jsl     _CWT
                cop     COP_0F_OPERR
ABRT_DP_FAIL:   .byte   "BASIC failed to allocate zero page",$0d,$00

L4C6B:          pea     DP_BAS_00_REAL_UK
                plb
                plb
                ldy     #$0004
; Store new DP at a location pointed to just ABOVE our return address + 4?
; The above is in bank 0 somewhere?
                sta     ($06,S),y                       ;TODO: WTF is going on here?
                tcd                                     ;use new DP
                sep     #$30
                .a8
                .i8
                phk
                plb
                ldy     #$00
                ldx     #_COENV
                jsl     _CO                             ;Set DP / environment for coroutine
                tsc
                sta     DP_stack_save
                xba
                sta     DP_stack_save+1
                jsr     arith_get_reference             ;Get ARITHMETIC module reference
                lda     #$01
                and     DP_BAS_RAND+4
                ora     DP_BAS_RAND
                ora     DP_BAS_RAND+1
                ora     DP_BAS_RAND+2
                ora     DP_BAS_RAND+3
                bne     @skiprndseed
                lda     #$41
                sta     DP_BAS_RAND
                eor     #$13
                sta     DP_BAS_RAND+1
                eor     #$05
                sta     DP_BAS_RAND+2
@skiprndseed:   per     strCopyright                    ;Set default for REPORT
                pla
                sta     DP_BAS_Report
                pla
                sta     DP_BAS_Report+1
                phk
                pla
                sta     DP_BAS_Report+2
                rep     #$30
                .a16
                .i16
                pla
                plb
                ldx     #DP_BAS_MEMSIZE
                ldy     #$0006
                cop     COP_18_OPRLH                    ;Parse HEX string
                phb
                pha
                sep     #$30
                .a8
                .i8
                bcs     @skNOMEMS
; Progsize passed in - add 000700 to it (room for basic language workspace)
                clc
                lda     DP_BAS_MEMSIZE
                adc     #$00
                sta     DP_BAS_MEMSIZE
                pha
                lda     DP_BAS_MEMSIZE+1
                adc     #$07
                sta     DP_BAS_MEMSIZE+1
                xba
                lda     DP_BAS_MEMSIZE+2
                adc     #$00
                sta     DP_BAS_MEMSIZE+2
                pha
                plb
                pla
                bra     @skMEMS                         ;BHA now contains MEMSIZE

@skNOMEMS:      lda     #$00
                sta     DP_BAS_MEMSIZE+2
                pha
                plb
                lda     #$80
                sta     DP_BAS_MEMSIZE+1
                xba
                lda     #$00
                sta     DP_BAS_MEMSIZE                  ;MEMSIZE/BHA now contains 008000
@skMEMS:        phd
                ldx     #_MMALA
                ldy     #$02
                jsl     _MM                             ;Allocate a large ascending area of memory
                bcc     memAllocOK
                jsr     memFreeDP
                phk
                plb
                lda     #>strFailAllocWksp
                xba
                lda     #<strFailAllocWksp
                ldx     #$02
                sec
                jsl     _CWT
                cop     COP_0F_OPERR
strFailAllocWksp:
                .byte   "BASIC failed to allocate workspace",$0d,$00

memAllocOK:     pld
                phb
                sta     DP_BAS_MEMBASE
                xba
                sta     DP_BAS_MEMBASE+1
                pla
                sta     DP_BAS_MEMBASE+2
                xba
                beq     memOnPage
                cop     COP_0F_OPERR
strWkspNotPageBound:
                .asciiz "BASIC work space not on page boundary"

memOnPage:      stz     DP_BAS_ARG2_FLAG
                rep     #$30
                .a16
                .i16
                pla
                plb
                ldx     #DP_BAS_ARG2
                ldy     #$0006
                cop     COP_18_OPRLH
                sep     #$30
                .a8
                .i8
                bcs     @noarg2
                tax
                lda     DP_BAS_ARG2
                ora     DP_BAS_ARG2+1
                ora     DP_BAS_ARG2+2
                bne     @gotarg2_ne
                stx     DP_BAS_ARG2
                xba
                sta     DP_BAS_ARG2+1
                phb
                pla
                sta     DP_BAS_ARG2+2
@lptoend:       lda     [DP_BAS_ARG2]                   ;if ARG2 wasnt a hex number then skip past it to <=32
                cmp     #' '
                inc     DP_BAS_ARG2
                bne     @L4D96
                inc     DP_BAS_ARG2+1
                bne     @L4D96
                inc     DP_BAS_ARG2+2
@L4D96:         bcs     @lptoend
@gotarg2_ne:    inc     DP_BAS_ARG2_FLAG
@noarg2:        clc
                lda     DP_BAS_MEMBASE
                adc     DP_BAS_MEMSIZE
                sta     DP_BAS_HIMEM
                sta     DP_BAS_MEMEND
                lda     DP_BAS_MEMBASE+1
                adc     DP_BAS_MEMSIZE+1
                sta     DP_BAS_HIMEM+1
                sta     DP_BAS_MEMEND+1
                lda     DP_BAS_MEMBASE+2
                adc     DP_BAS_MEMSIZE+2
                sta     DP_BAS_HIMEM+2
                sta     DP_BAS_MEMEND+2
                lda     DP_BAS_MEMBASE+1
; Not entirely sure what is going on here but it looks like a horrendous bodge
; to nuge membase up by a page if it was alloced at the end of a bank - must be
; something that breaks?
                cmp     #$ff
                bne     @awful1
                inc     DP_BAS_MEMBASE+2
                inc     A
@awful1:        cmp     #$00
                bne     @awful2
                inc     A
                sta     DP_BAS_MEMBASE+1
@awful2:        phk
                plb
; Setup pointers to various workspace areas by adding values from a table
                ldx     #DP_BAS_MEMBASE
                ldy     #$00
                jsr     mem_add_16bconsts_to_3b_pointers
                clc
                lda     DP_BAS_00_REAL_UK,x
                adc     tblWkspSizes,y
                pha
                lda     DP_BAS_00_REAL_UK+1,x
                adc     tblWkspSizes+1,y
                sta     DP_BAS_PAGE+1
                lda     DP_BAS_00_REAL_UK+2,x
                adc     #$00
                sta     DP_BAS_PAGE+2
                pla
; If page doesn't lie on a page boundary bump up to the next page
                beq     @L4DE9
                inc     DP_BAS_PAGE+1
                bne     @L4DE9
                inc     DP_BAS_PAGE+2
; Copy PAGE into memory pointed to by ???
; TODO
@L4DE9:         ldy     #$00
                lda     #$00
                sta     [DP_BAS_SPECIAL_VARSptr],y
                iny
                stz     DP_BAS_PAGE
                lda     DP_BAS_PAGE+1
                sta     [DP_BAS_SPECIAL_VARSptr],y
                iny
                sta     DP_BAS_LOMEM_LIM_PAG
                lda     DP_BAS_PAGE+2
                sta     [DP_BAS_SPECIAL_VARSptr],y
                iny
                sta     DP_BAS_LOMEM_LIM_PAG+1
                ldy     #$06
                sty     DP_BAS_SPECIALVAR_SZ
                stz     DP_BAS_LISTO
                stz     DP_BAS_SET_TO_Z_AT_EOS
                lda     #$00
                ldy     #$02
                sta     [DP_BAS_INTVARSptr],y
                iny
                sta     [DP_BAS_INTVARSptr],y
                ldx     #$ff
                stx     DP_BAS_WIDTH
                tay
                lda     #$0a
                sta     [DP_BAS_INTVARSptr]
                dec     A
                iny
                sta     [DP_BAS_INTVARSptr],y
                lda     #$03
                sta     $43
                stz     $44
                stz     $45
                stz     $46
                lda     #>BRK_HANDLER
                xba
                lda     #<BRK_HANDLER
                phk
                plb
                ldx     #_COBRK
                jsl     _CO
                bcc     L4E64
                jsr     L4C14
                jsr     memFreeDP
                .a16
                .i16
                phk
                plb
                lda     #ABRT_CANNOTSETBRK
                ldx     #$0002
                sec
                jsl     _CWT
                cop     COP_0F_OPERR
ABRT_CANNOTSETBRK:
                .byte   "Cannot set BRK handler",$0d,$00

                .a8
                .i8
L4E64:          cli
                jmp     braNEW2

;*******************************************************************************
;* Service Call 6                                                              *
;*******************************************************************************
;* This looks to create a coroutine                                            *
;*******************************************************************************
ServiceCall_6:  pla
                php
                rep     #$30
                .a16
                .i16
                phd
                lda     #$0100
                tcd
                per     COROUTINE_ENTRY
                pla
                phk
                plb
                ldx     #_COCRE
                jsl     _CO
                pld
                bcc     @ok
                ldx     #$0006
                plp
                .a8
                .i8
                sec
                rtl

                .a16
                .i16
@ok:            phd
                phy
                phb
                ldx     #_COENV
                jsl     _CO                             ;Set environment for coroutine
                plb
                ply
                pld
                bcc     @ok2
                ldx     #$0006
                bra     @notok

@ok2:           phy
                jsl     _CCO                            ;Call the coroutine we just created
                ply
@notok:         php
                pha
                phx
                ldx     #_CODEL                         ;Delete coroutine
                jsl     _CO
                plx
                pla
                plp
                bcc     @retCLC
                plp
                sec
                rtl

                .a8
                .i8
@retCLC:        plp
                clc
                rtl

                .a16
                .i16
COROUTINE_ENTRY:
                phd
                cop     COP_11_OPADF                    ;Allocate DP of 256 bytes
                .word   $0100

                plx
                bcs     L4F0D
                tay
                tcd
                phx
                lda     #DP_BAS_ENDDP-1                 ;TODO: fix up set to DP_BAS_ENDDP
                mvn     #$00,#$00                       ;Copy D5 bytes to new DP from old DP
                pla
                sta     DP_BAS_OLD_DP
                lda     DP_stack_save
                sta     DP_BAS_STACKSAVE2
                lda     #BRK_HANDLER_2 >> 8             ;TODO - WTF going on with XBA here? wrong mode?
                xba
                lda     #BRK_HANDLER_2-$4f00            ;this will overwrite B from above? TODO: fix to use <
                phk
                plb
                ldx     #_COBRK
                jsl     _CO
                bcs     L4EF1
                tsc
                sta     DP_stack_save
                phk
                plb
                sep     #$30
                .a8
                .i8
                jsr     ackESCThenDoSomethingWithatHELP
                bra     L4EEF

L4EEC:          clc
                phk
                plb
L4EEF:          rep     #$30
                .a16
                .i16
L4EF1:          pei     (DP_BAS_VARTOP+1)
                pei     (DP_BAS_VARTOP)
                pei     (DP_BAS_STACKSAVE2)
                pei     (DP_BAS_OLD_DP)
                php
                tdc
                cop     COP_1A_OPFZB                    ;Free DP
                plp
                pld                                     ;Restore old DP
                pla
                sta     DP_stack_save                   ;Restore old Stack Save
                pla
                plx
                sta     DP_BAS_VARTOP
                stx     DP_BAS_VARTOP+1
                ldx     #$0004
                bcc     L4F10
L4F0D:          ldx     #$0006
L4F10:          jsl     _CWT
BRK_HANDLER_2:  cli
                phk
                plb
                sep     #$30
                .a8
                .i8
                lda     DP_stack_save+1
                xba
                lda     DP_stack_save
                tcs
                lda     #$40
                trb     DP_BAS_CO_FLAGS
                bra     L4EEC

;*******************************************************************************
;* Very unsure of this - looks like the HELP entry point maybe.                *
;*******************************************************************************
;* Looks to create a new copy of direct page, call something then free it      *
;*******************************************************************************
                .a16
                .i16
HELPENV:        php
                rep     #$30
                phd
                cop     COP_11_OPADF                    ;Allocate 256 byte page of DP
                .word   $0100
                plx                                     ;Get back old D in X
                bcs     @fail                           ;Didn't allocate - fail
                phx                                     ;Re-stack original D
                tay                                     ;put allocated DP base in Y
                tcd                                     ;Set D
                lda     #$00ff
                mvn     #$00,#$00                       ;Move 256 from old DP to new DP
                tsc                                     ;Stack into C
                sta     DP_stack_save
                phk
                plb
                sep     #$30
                .a8
                .i8
                jsr     HELP
                rep     #$30
                .a16
                .i16
                pei     (DP_BAS_VARTOP+1)
                pei     (DP_BAS_VARTOP)
                php
                tdc
                cop     COP_1A_OPFZB
                plp
                pla
                plx
                pld
                sta     DP_BAS_VARTOP                   ;keep lomem if changed by HELP?
                stx     DP_BAS_VARTOP+1
@fail:          plp
                rts
        .ENDIF ;COMMUNICATOR ; end of communicator module handlers

        .IFDEF MOS
MOSStart:
                jsr     MOS_shims_init

                ; Now enter native mode
                clc
                xce

                rep     #$30
                .a16
                .i16

                ; TODO: API for assigning DP, for now just set to $1900 in Bank0!

                pea     MOS_BASIC_DP
                pld


                tsc
                sta     DP_stack_save



                ; 
                sep     #$30
                .a8
                .i8


                lda     #$01
                and     DP_BAS_RAND+4
                ora     DP_BAS_RAND
                ora     DP_BAS_RAND+1
                ora     DP_BAS_RAND+2
                ora     DP_BAS_RAND+3
                bne     @skiprndseed
                lda     #$41
                sta     DP_BAS_RAND
                eor     #$13
                sta     DP_BAS_RAND+1
                eor     #$05
                sta     DP_BAS_RAND+2
@skiprndseed:   per     strCopyright                    ;Set default for REPORT
                pla
                sta     DP_BAS_Report
                pla
                sta     DP_BAS_Report+1
                phk
                pla
                sta     DP_BAS_Report+2


                ; TODO get this using OSWORD 99?
                lda     #<MOS_BASIC_MEMSZ
                sta     DP_BAS_MEMSIZE
                lda     #>MOS_BASIC_MEMSZ
                sta     DP_BAS_MEMSIZE+1
                lda     #^MOS_BASIC_MEMSZ
                sta     DP_BAS_MEMSIZE+2

                lda     #<MOS_BASIC_MEMBASE
                sta     DP_BAS_MEMBASE
                lda     #>MOS_BASIC_MEMBASE
                sta     DP_BAS_MEMBASE+1
                lda     #^MOS_BASIC_MEMBASE
                sta     DP_BAS_MEMBASE+2


                clc
                lda     DP_BAS_MEMBASE
                adc     DP_BAS_MEMSIZE
                sta     DP_BAS_HIMEM
                sta     DP_BAS_MEMEND
                lda     DP_BAS_MEMBASE+1
                adc     DP_BAS_MEMSIZE+1
                sta     DP_BAS_HIMEM+1
                sta     DP_BAS_MEMEND+1
                lda     DP_BAS_MEMBASE+2
                adc     DP_BAS_MEMSIZE+2
                sta     DP_BAS_HIMEM+2
                sta     DP_BAS_MEMEND+2
                lda     DP_BAS_MEMBASE+1
; Not entirely sure what is going on here but it looks like a horrendous bodge
; to nuge membase up by a page if it was alloced at the end of a bank - must be
; something that breaks?
; TODO: check why this is needed and get rid?
                cmp     #$ff
                bne     @awful1
                inc     DP_BAS_MEMBASE+2
                inc     A
@awful1:        cmp     #$00
                bne     @awful2
                inc     A
                sta     DP_BAS_MEMBASE+1
@awful2:        phk
                plb
; Setup pointers to various workspace areas by adding values from a table
                ldx     #DP_BAS_MEMBASE
                ldy     #$00
                jsr     mem_add_16bconsts_to_3b_pointers
                clc
                lda     DP_BAS_00_REAL_UK,x
                adc     tblWkspSizes,y
                pha
                lda     DP_BAS_00_REAL_UK+1,x
                adc     tblWkspSizes+1,y
                sta     DP_BAS_PAGE+1
                lda     DP_BAS_00_REAL_UK+2,x
                adc     #$00
                sta     DP_BAS_PAGE+2
                pla
; If page doesn't lie on a page boundary bump up to the next page
                beq     @L4DE9
                inc     DP_BAS_PAGE+1
                bne     @L4DE9
                inc     DP_BAS_PAGE+2
; Copy PAGE into memory pointed to by ???
; TODO
@L4DE9:         ldy     #$00
                lda     #$00
                stz     DP_BAS_PAGE
                lda     DP_BAS_PAGE+1
                sta     DP_BAS_LOMEM_LIM_PAG
                lda     DP_BAS_PAGE+2
                sta     DP_BAS_LOMEM_LIM_PAG+1


                stz     DP_BAS_LISTO
                stz     DP_BAS_SET_TO_Z_AT_EOS
                lda     #$00
                ldy     #$02
                sta     [DP_BAS_INTVARSptr],y
                iny
                sta     [DP_BAS_INTVARSptr],y
                ldx     #$ff
                stx     DP_BAS_WIDTH
                tay
                lda     #$0a
                sta     [DP_BAS_INTVARSptr]
                dec     A
                iny
                sta     [DP_BAS_INTVARSptr],y
                lda     #$03
                sta     $43
                stz     $44
                stz     $45
                stz     $46


                lda     #^BRK_HANDLER
                sta     f:NATVEC_BRK+2
                lda     #>BRK_HANDLER
                sta     f:NATVEC_BRK+1
                lda     #<BRK_HANDLER
                sta     f:NATVEC_BRK

                lda     #^BRK_HANDLER_EMU
                sta     f:NATVEC_BRK_EMU+2
                lda     #>BRK_HANDLER_EMU
                sta     f:NATVEC_BRK_EMU+1
                lda     #<BRK_HANDLER_EMU
                sta     f:NATVEC_BRK_EMU


                lda     #^strCopyright
                sta     DP_BAS_BL_ERRPTR+2
                lda     #>strCopyright
                sta     DP_BAS_BL_ERRPTR+1
                lda     #<strCopyright
                sta     DP_BAS_BL_ERRPTR+0

                jsr     arith_init

                jmp     braNEW2


        .ENDIF

                .a8
                .i8
mem_add_16bconsts_to_3b_pointers:
                lda     tblWkspSizes+2,y                ;Check ahead for 0 (don't do the last entry)
                ora     tblWkspSizes+3,y
                beq     @rts
                clc
                lda     $00,x
                adc     tblWkspSizes,y
                sta     $03,x
                lda     $01,x
                adc     tblWkspSizes+1,y
                sta     $04,x
                lda     $02,x
                adc     #$00
                sta     $05,x
                iny
                iny
                inx
                inx
                inx
                bra     mem_add_16bconsts_to_3b_pointers

@rts:           rts

varFindPROCFN:  iny
                sty     DP_BAS_TMP6+3
                ldy     #$01
                lda     [DP_BAS_TMP6],y
                ldy     #VAR_OFFS_PROC
                cmp     #tknPROC
                beq     varSearchLinkedList
                ldy     #VAR_OFFS_FN
                bra     varSearchLinkedList

varFind_00:     iny
                sty     DP_BAS_TMP6+3
                ldy     #$01
                lda     [DP_BAS_TMP6],y
                sec
                sbc     #'@'
                pha
                asl     A
                clc
                adc     $01,S
                sta     $01,S
                pla
                tay
varSearchLinkedList:
                sta     DP_FP_TMP+2
                clv
                lda     [DP_BAS_VARS_BASE],y
                iny
                ora     [DP_BAS_VARS_BASE],y
                iny
                ora     [DP_BAS_VARS_BASE],y
                beq     @rts                            ;linked list head empty
                lda     [DP_BAS_VARS_BASE],y
                sta     DP_BAS_INT_WA+2
                dey
                lda     [DP_BAS_VARS_BASE],y
                sta     DP_BAS_INT_WA+1
                dey
                lda     [DP_BAS_VARS_BASE],y
                bra     varLinkedListSearchStart

@rts:           rts

varLinkedListSearchCompareChar:
                dey
                dey
                lda     [DP_BAS_TMP6],y
                cmp     #'@'
                bne     L4FCC
                sep     #$40                            ;set V
                phb
                pla
                sta     DP_BAS_INT_WA+2                 ;override the bank from the builtins table to be our current bank
L4FCA:          ldy     #$01
L4FCC:          lda     [DP_BAS_INT_WA],y
                iny
                ora     [DP_BAS_INT_WA],y
                ora     [DP_BAS_INT_WA]
                beq     rtsL503D
                lda     DP_FP_TMP+2
                bne     @L4FDD
                phb
                pla
                bra     @L4FDF

@L4FDD:         lda     [DP_BAS_INT_WA],y
@L4FDF:         xba
                dey
                lda     [DP_BAS_INT_WA],y
                tay
                lda     [DP_BAS_INT_WA]
                xba
                sta     DP_BAS_INT_WA+2
                xba
                sty     DP_BAS_INT_WA+1
varLinkedListSearchStart:
                sta     DP_BAS_INT_WA
                ldy     #$03
                lda     [DP_BAS_INT_WA],y
                bne     @notendofvarnameinll
                cpy     DP_BAS_TMP6+3                   ;check to see if that is the length we were after
                bne     varLinkedListSearchCompareChar
                bra     varLinkedListSearchFound

@L4FFA:         lda     [DP_BAS_INT_WA],y
                beq     L4FCA
@notendofvarnameinll:
                dey
                cmp     [DP_BAS_TMP6],y
                bne     L4FCA
                iny
                iny
                cpy     DP_BAS_TMP6+3
                bne     @L4FFA
                lda     [DP_BAS_INT_WA],y
                bne     L4FCA
varLinkedListSearchFound:
                php
                tya
                adc     DP_BAS_INT_WA
                sta     DP_BAS_INT_WA
                bcc     @sk
                inc     DP_BAS_INT_WA+1
                bne     @sk
                inc     DP_BAS_INT_WA+2
@sk:            plp
                lda     #RETV_REAL
                bvc     rtsL503D

                lda     DP_BAS_INT_WA+3
                pha
                phx
                jsr     GetVarValInt
                lda     DP_BAS_INT_WA+2
                and     DP_BAS_INT_WA+3
                inc     A
                bne     @L5034
                phb
                pla
                sta     DP_BAS_INT_WA+2
                bra     @L5037

@L5034:         jsr     ConvertAddressIfDP
@L5037:         plx
                pla
                sta     DP_BAS_INT_WA+3
                lda     #RETV_REAL
rtsL503D:       rts

        .IFDEF COMMUNICATOR
_ukCallEv23:    phx
                lda     #$00
                xba
                lda     #$03
                ldx     #$02
                jsl     _EV
                plx
                rts
        .ENDIF

prog_search_lineno:
        .IFDEF OPTIMIZE_SIZE
                rep     #$20
                .a16
                lda     DP_BAS_PAGE
                sta     DP_FPB_exp
                ldy     DP_BAS_PAGE+2
                sty     DP_FPB_mant+1                   ;Point at start of program
@lp:
                ldy     #$01
                lda     [DP_FPB_exp],y                  ;get line no hi byte
                xba
                cmp     DP_BAS_INT_WA                   ;compare to line number in WA
                bcs     @skCkLo                         ;greater or equal
@nextline:      ldy     #$03
                lda     [DP_FPB_exp],y                  ;get line length
                and     #$FF                            ; we only want an 8 bit number!
                adc     DP_FPB_exp                      ;add to pointer and repeat
                sta     DP_FPB_exp
                bcc     @lp
                inc     DP_FPB_mant+1
                bra     @lp

@skCkLo:        sep     #$20
                .a8
                bne     @retCLCY2                       ;greater - exit
                iny
                rts                                     ;returns with Cy=1 - exact match

@retCLCY2:      iny
                clc
                rts                                     ;failed to find return with Cy=0

        .ELSEIF .defined (OPTIMIZE)
                ; attempt to speed up by using 16bit DP pointers and databank register
                ; assumption: databank wrapping won't cause bother!

                clc
                lda     DP_BAS_PAGE
                adc     #1
                sta     DP_FPB_exp
                lda     DP_BAS_PAGE+1
                adc     #0
                sta     DP_FPB_exp+1
                lda     DP_BAS_PAGE+2
                adc     #0
                pha
                plb                                     ; bank is now at start of program

                ldy     #2

@lp:            lda     (DP_FPB_exp)                    ;get line no hi byte
                cmp     DP_BAS_INT_WA+1                 ;compare to line number in WA
                bcs     @skCkLo                         ;;greater or equal
@nextline:      lda     (DP_FPB_exp),y                  ;get line length
                adc     DP_FPB_exp                      ;add to pointer and repeat
                sta     DP_FPB_exp
                bcc     @lp
                inc     DP_FPB_exp+1
                bne     @lp
                ; next bank
                phb
                pla
                inc     A
                pha
                plb
                bra     @lp

@skCkLo:        bne     @retCLCY2                       ;greater - exit
                dey
                lda     (DP_FPB_exp),y
                iny
                cmp     DP_BAS_INT_WA                   ;compare lo
                bcc     @nextline                       ;was less continue search
                beq     @skclc
@retCLCY2:
                clc
@skclc:         php
                clc
                lda     DP_FPB_exp
                sbc     #0
                sta     DP_FPB_exp
                lda     DP_FPB_exp+1
                sbc     #0
                sta     DP_FPB_exp+1
                phb
                pla
                sbc     #0
                sta     DP_FPB_exp+2
@rts:           phk
                plb
                plp                
                rts


        .ELSE

                lda     DP_BAS_PAGE
                sta     DP_FPB_exp
                lda     DP_BAS_PAGE+1
                sta     DP_FPB_mant
                lda     DP_BAS_PAGE+2
                sta     DP_FPB_mant+1                   ;Point at start of program
@lp:
                ldy     #$01
                lda     [DP_FPB_exp],y                  ;get line no hi byte
                cmp     DP_BAS_INT_WA+1                 ;compare to line number in WA
                bcs     @skCkLo                         ;;greater or equal
@nextline:      ldy     #$03
                lda     [DP_FPB_exp],y                  ;get line length
                adc     DP_FPB_exp                      ;add to pointer and repeat
                sta     DP_FPB_exp
                bcc     @lp
                inc     DP_FPB_mant
                bne     @lp
                inc     DP_FPB_mant+1
                bra     @lp

@skCkLo:        bne     @retCLCY2                       ;greater - exit
                iny
                lda     [DP_FPB_exp],y
                cmp     DP_BAS_INT_WA                   ;compare lo
                bcc     @nextline                       ;was less continue search
                bne     @retCLCY2
                rts                                     ;returns with Cy=1 - exact match

@retCLCY2:      ldy     #$02
                clc
                rts                                     ;failed to find return with Cy=0
        .ENDIF

fpRealDivide:   jsr     checkTypeInAConvertToINT
                lda     DP_BAS_INT_WA+3
                pha
                jsr     L83A4
                jsr     stackINTEvalLevel2
                stx     DP_BAS_40_VARTYPE
                jsr     checkTypeInAConvertToINT
                pla
                sta     DP_BAS_TMP6+1
                eor     DP_BAS_INT_WA+3
                sta     DP_BAS_TMP6
                jsr     L83A4
                ldx     #DB_BAS_UNK_5A_6
                jsr     stack_copy6_to_X_uns4
                stz     DP_FPB_exp
                stz     DP_FPB_mant
                stz     DP_FPB_mant+1
                stz     DP_BAS_61_UK
                lda     DP_BAS_INT_WA+3
                ora     DP_BAS_INT_WA
                ora     DP_BAS_INT_WA+1
                ora     DP_BAS_INT_WA+2
                beq     brk_12_DivisionBy0
                ldy     #$20
@lp20:          dey
                beq     @rtsL50FA
                asl     DB_BAS_UNK_5A_6
                rol     DB_BAS_UNK_5A_6+1
                rol     DP_FPB_sgn
                rol     DB_BAS_UNK_5A_6+3
                bpl     @lp20
@lpRolDiv:      rol     DB_BAS_UNK_5A_6
                rol     DB_BAS_UNK_5A_6+1
                rol     DP_FPB_sgn
                rol     DB_BAS_UNK_5A_6+3
                rol     DP_FPB_exp
                rol     DP_FPB_mant
                rol     DP_FPB_mant+1
                rol     DP_BAS_61_UK
                sec
                lda     DP_FPB_exp
                sbc     DP_BAS_INT_WA
                pha
                lda     DP_FPB_mant
                sbc     DP_BAS_INT_WA+1
                pha
                lda     DP_FPB_mant+1
                sbc     DP_BAS_INT_WA+2
                tax
                lda     DP_BAS_61_UK
                sbc     DP_BAS_INT_WA+3
                bcc     @sk1
                sta     DP_BAS_61_UK
                stx     DP_FPB_mant+1
                pla
                sta     DP_FPB_mant
                pla
                sta     DP_FPB_exp
                bcs     @sk2

@sk1:           pla
                pla
@sk2:           dey
                bne     @lpRolDiv
@rtsL50FA:      rts

brk_12_DivisionBy0:
                brk     $12

                .asciiz "Division by zero"

zeroFPA:        stz     DP_FPA_sgn
                stz     DP_FPA_exp+1
                stz     DP_FPA_exp
                stz     DP_FPA_mant
rtsL5116:       rts

braIntToReal:   bra     IntToReal                       ;TODO: DEAD code?

                bra     brk_46_IFLT2

                bra     brk_47_IFLT4

brk_46_IFLT2:   brk     $46

                .byte   " Iflt + 2 error "

brk_47_IFLT4:   brk     $47

                .asciiz " Iflt + 4 error "

IntToReal:      stz     DP_FPA_mant+4
                stz     DP_FPA_exp
                lda     DP_BAS_INT_WA+3
                sta     DP_FPA_sgn
                bpl     @L5151
                jsr     negateWA
                lda     DP_BAS_INT_WA+3
@L5151:         bne     @L5179
                stz     DP_FPA_mant+3
                lda     DP_BAS_INT_WA+2
                bne     @L516D
                stz     DP_FPA_mant+2
                lda     DP_BAS_INT_WA+1
                bne     @L5165
                stz     DP_FPA_mant+1
                lda     DP_BAS_INT_WA
                bra     @L5189

@L5165:         ldy     DP_BAS_INT_WA
                sty     DP_FPA_mant+1
                ldy     #$90
                bra     @L518B

@L516D:         ldy     DP_BAS_INT_WA+1
                sty     DP_FPA_mant+1
                ldy     DP_BAS_INT_WA
                sty     DP_FPA_mant+2
                ldy     #$98
                bra     @L518B

@L5179:         ldy     DP_BAS_INT_WA+2
                sty     DP_FPA_mant+1
                ldy     DP_BAS_INT_WA+1
                sty     DP_FPA_mant+2
                ldy     DP_BAS_INT_WA
                sty     DP_FPA_mant+3
                ldy     #$a0
                bra     @L518B

@L5189:         ldy     #$88
@L518B:         ora     #$00
                bmi     @L519E
                bne     @L5194
                brl     zeroFPA

@L5194:         dey
                asl     DP_FPA_mant+3
                rol     DP_FPA_mant+2
                rol     DP_FPA_mant+1
                rol     A
                bpl     @L5194
@L519E:         sta     DP_FPA_mant
                sty     DP_FPA_exp+1
                rts

fpNormalizeFPA_bra:
                bra     NormaliseRealA                  ;TODO - optimize away!

                bra     brk_2b_Fnrm2                    ;TODO: not called - optimize away

                bra     brk_2c_Fnrm4                    ;TODO: not called - optimize away

                bra     brk_2d_fnrm6                    ;TODO: not called - optimize away

NormaliseRealA: lda     DP_FPA_mant
NormaliseRealA_2:
                bpl     NormaliseRealA_3
                brl     rtsL5116

NormaliseRealA_3:
                bne     FPANormalizeSlow
                ora     DP_FPA_mant+1
                ora     DP_FPA_mant+2
                ora     DP_FPA_mant+3
                ora     DP_FPA_mant+4
                bne     @sknz
                brl     zeroFPA

@sknz:          lda     DP_FPA_exp+1
@lp:            ldy     DP_FPA_mant+1
                sty     DP_FPA_mant
                ldy     DP_FPA_mant+2
                sty     DP_FPA_mant+1
                ldy     DP_FPA_mant+3
                sty     DP_FPA_mant+2
                ldy     DP_FPA_mant+4
                sty     DP_FPA_mant+3
                stz     DP_FPA_mant+4
                sec
                sbc     #$08
                bcs     @sk
                dec     DP_FPA_exp
@sk:            ldy     DP_FPA_mant
                beq     @lp
                bmi     staExp1rts
                bra     skL51E6

FPANormalizeSlow:
                lda     DP_FPA_exp+1
skL51E6:        clc
@lp:            sbc     #$00
                bcs     @sk2
                dec     DP_FPA_exp
@sk2:           asl     DP_FPA_mant+4
                rol     DP_FPA_mant+3
                rol     DP_FPA_mant+2
                rol     DP_FPA_mant+1
                rol     DP_FPA_mant
                bpl     @lp
staExp1rts:     sta     DP_FPA_exp+1
                rts

brk_2b_Fnrm2:   brk     $2b

                .asciiz "Fnrm+2 entry error"            ;extra 0

brk_2c_Fnrm4:   brk     $2c

                .asciiz "Fnrm+4 entry error"            ;TODO: optimize away extra 0

brk_2d_fnrm6:   brk     $2d

                .asciiz "Fnrm+6 entry error"            ;TODO: extra 0

L523B:          jsr     fpCopyFPAtoFPB
                jmp     zero_FPA

fpFPAMant2Int_remainder_inFPB:
                lda     DP_FPA_exp+1
                bpl     L523B
                jsr     fpSetFPBto0
                ldy     DP_FPA_mant
                beq     L5290
@L524C:         lsr     DP_FPA_mant
                ror     DP_FPA_mant+1
                ror     DP_FPA_mant+2
                ror     DP_FPA_mant+3
                ror     DP_FPB_mant
                ror     DP_FPB_mant+1
                ror     DP_BAS_61_UK
                ror     DP_FPB_mant+3
                inc     A
                beq     L5289
@L525F:         cmp     #$a0
                bcs     L528C
                cmp     #$99
                bcs     @L524C
                adc     #$08
                ldy     DP_BAS_61_UK
                sty     DP_FPB_mant+3
                ldy     DP_FPB_mant+1
                sty     DP_BAS_61_UK
                ldy     DP_FPB_mant
                sty     DP_FPB_mant+1
                ldy     DP_FPA_mant+3
                sty     DP_FPB_mant
                ldy     DP_FPA_mant+2
                sty     DP_FPA_mant+3
                ldy     DP_FPA_mant+1
                sty     DP_FPA_mant+2
                ldy     DP_FPA_mant
                sty     DP_FPA_mant+1
                stz     DP_FPA_mant
                bra     @L525F

L5289:          jmp     brk_14_TooBigMaybe

L528C:          bne     L5289
                sta     DP_FPA_exp+1
L5290:          lda     DP_FPA_sgn
                bpl     L52AB
fpReal2Int_NegateMantissa:
                sec
                ldy     #$00
                tya
                sbc     DP_FPA_mant+3
                sta     DP_FPA_mant+3
                tya
                sbc     DP_FPA_mant+2
                sta     DP_FPA_mant+2
                tya
                sbc     DP_FPA_mant+1
                sta     DP_FPA_mant+1
                tya
                sbc     DP_FPA_mant
                sta     DP_FPA_mant
L52AB:          rts

fpIncrementFPAMantissa:
                inc     DP_FPA_mant+3
                bne     @rts
                inc     DP_FPA_mant+2
                bne     @rts
                inc     DP_FPA_mant+1
                bne     @rts
                inc     DP_FPA_mant
                beq     L5289
@rts:           rts

rndNext:        ldy     #$04
@lp:            ror     DP_BAS_RAND+4
                lda     DP_BAS_RAND+3
                tax
                ror     A
                sta     DP_BAS_RAND+4
                lda     DP_BAS_RAND+2
                sta     DP_BAS_RAND+3
                lsr     A
                eor     DP_BAS_RAND+1
                and     #$0f
                eor     DP_BAS_RAND+1
                ror     A
                ror     A
                ror     A
                ror     A
                eor     DP_BAS_RAND+4
                stx     DP_BAS_RAND+4
                ldx     DP_BAS_RAND+1
                stx     DP_BAS_RAND+2
                ldx     DP_BAS_RAND
                stx     DP_BAS_RAND+1
                sta     DP_BAS_RAND
                dey
                bne     @lp
                rts

L52E8:          lda     DP_FPB_sgn
                sta     DP_FPA_sgn
                stz     DP_FPA_exp
                lda     DP_FPB_exp
                sta     DP_FPA_exp+1
                lda     DP_FPB_mant
                sta     DP_FPA_mant
                lda     DP_FPB_mant+1
                sta     DP_FPA_mant+1
                lda     DP_BAS_61_UK
                sta     DP_FPA_mant+2
                lda     DP_FPB_mant+3
                sta     DP_FPA_mant+3
                lda     DP_FPB_mant+4
                sta     DP_FPA_mant+4
L5306:          rts

L5307:          lda     DP_FPA_mant
                beq     L52E8
                sec
                lda     DP_FPA_exp+1
                sbc     DP_FPB_exp
                beq     @L5381
                bcc     @L5348
                cmp     #'%'
                bcs     L5306
                tay
                and     #$38
                beq     @L5334
                sec
@L531E:         ldx     DP_FPB_mant+3
                stx     DP_FPB_mant+4
                ldx     DP_BAS_61_UK
                stx     DP_FPB_mant+3
                ldx     DP_FPB_mant+1
                stx     DP_BAS_61_UK
                ldx     DP_FPB_mant
                stx     DP_FPB_mant+1
                stz     DP_FPB_mant
                sbc     #$08
                bne     @L531E
@L5334:         tya
                and     #$07
                beq     @L5381
@L5339:         lsr     DP_FPB_mant
                ror     DP_FPB_mant+1
                ror     DP_BAS_61_UK
                ror     DP_FPB_mant+3
                ror     DP_FPB_mant+4
                dec     A
                bne     @L5339
                bra     @L5381

@L5348:         eor     #$ff
                inc     A
                cmp     #$25
                bcs     L52E8
                ldy     DP_FPB_exp
                sty     DP_FPA_exp+1
                tay
                and     #$38
                beq     @L536F
                sec
@L5359:         ldx     DP_FPA_mant+3
                stx     DP_FPA_mant+4
                ldx     DP_FPA_mant+2
                stx     DP_FPA_mant+3
                ldx     DP_FPA_mant+1
                stx     DP_FPA_mant+2
                ldx     DP_FPA_mant
                stx     DP_FPA_mant+1
                stz     DP_FPA_mant
                sbc     #$08
                bne     @L5359
@L536F:         tya
                and     #$07
                beq     @L5381
@L5374:         lsr     DP_FPA_mant
                ror     DP_FPA_mant+1
                ror     DP_FPA_mant+2
                ror     DP_FPA_mant+3
                ror     DP_FPA_mant+4
                dec     A
                bne     @L5374
@L5381:         lda     DP_FPA_sgn
                eor     DP_FPB_sgn
                bmi     @L538B
                clc
                jmp     fpAddAtoBstoreinA_sameExp

@L538B:         lda     DP_FPA_mant
                cmp     DP_FPB_mant
                bne     @L53AC
                lda     DP_FPA_mant+1
                cmp     DP_FPB_mant+1
                bne     @L53AC
                lda     DP_FPA_mant+2
                cmp     DP_BAS_61_UK
                bne     @L53AC
                lda     DP_FPA_mant+3
                cmp     DP_FPB_mant+3
                bne     @L53AC
                lda     DP_FPA_mant+4
                cmp     DP_FPB_mant+4
                bne     @L53AC
                jmp     zero_FPA

@L53AC:         bcs     @L53D4
                lda     DP_FPB_sgn
                sta     DP_FPA_sgn
                sec
                lda     DP_FPB_mant+4
                sbc     DP_FPA_mant+4
                sta     DP_FPA_mant+4
                lda     DP_FPB_mant+3
                sbc     DP_FPA_mant+3
                sta     DP_FPA_mant+3
                lda     DP_BAS_61_UK
                sbc     DP_FPA_mant+2
                sta     DP_FPA_mant+2
                lda     DP_FPB_mant+1
                sbc     DP_FPA_mant+1
                sta     DP_FPA_mant+1
                lda     DP_FPB_mant
                sbc     DP_FPA_mant
                sta     DP_FPA_mant
                brl     NormaliseRealA_2

@L53D4:         lda     DP_FPA_mant+4
                sbc     DP_FPB_mant+4
                sta     DP_FPA_mant+4
                lda     DP_FPA_mant+3
                sbc     DP_FPB_mant+3
                sta     DP_FPA_mant+3
                lda     DP_FPA_mant+2
                sbc     DP_BAS_61_UK
                sta     DP_FPA_mant+2
                lda     DP_FPA_mant+1
                sbc     DP_FPB_mant+1
                sta     DP_FPA_mant+1
                lda     DP_FPA_mant
                sbc     DP_FPB_mant
                sta     DP_FPA_mant
                brl     NormaliseRealA_2

tblTokensAsc:   .byte   "AND"
                .word   $0080
                .byte   "ABS"
                .word   $0094
                .byte   "ACS"
                .word   $0095
                .byte   "ADVAL"
                .word   $0096
                .byte   "ASC"
                .word   $0097
                .byte   "ASN"
                .word   $0098
                .byte   "ATN"
                .word   $0099
                .byte   "AUTO"
                .word   $10c6
                .byte   "BGET"
                .word   $019a
                .byte   "BPUT"
                .word   $03d5
                .byte   "COLOUR"
                .word   $02fb
                .byte   "CALL"
                .word   $02d6
                .byte   "CHAIN"
                .word   $02d7
                .byte   "CHR$"
                .word   $00bd
                .byte   "CLEAR"
                .word   $01d8
                .byte   "CLOSE"
                .word   $03d9
                .byte   "CLG"
                .word   $01da
                .byte   "CLS"
                .word   $01db
                .byte   "COS"
                .word   $009b
                .byte   "COUNT"
                .word   $019c
                .byte   "COLOR"
                .word   $02fb
                .byte   "DATA"
                .word   $20dc
                .byte   "DEG"
                .word   $009d
                .byte   "DEF"
                .word   $00dd
                .byte   "DELETE"
                .word   $10c7
                .byte   "DIV"
                .word   $0081
                .byte   "DIM"
                .word   $02de
                .byte   "DRAW"
                .word   $02df
                .byte   "ENDPROC"
                .word   $01e1
                .byte   "END"
                .word   $01e0
                .byte   "ENVELOPE"
                .word   $02e2
                .byte   "ELSE"
                .word   $148b
                .byte   "EVAL"
                .word   $00a0
                .byte   "ERL"
                .word   $019e
                .byte   "ERROR"
                .word   $0485
                .byte   "EOF"
                .word   $01c5
                .byte   "EOR"
                .word   $0082
                .byte   "ERR"
                .word   $019f
                .byte   "EXP"
                .word   $00a1
                .byte   "EXT"
                .word   $01a2
                .byte   "EDIT"
                .word   $10ce
                .byte   "FOR"
                .word   $02e3
                .byte   "FALSE"
                .word   $01a3
                .byte   "FN"
                .word   $08a4
                .byte   "GOTO"
                .word   $12e5
                .byte   "GET$"
                .word   $00be
                .byte   "GET"
                .word   $00a5
                .byte   "GOSUB"
                .word   $12e4
                .byte   "GCOL"
                .word   $02e6
                .byte   "HIMEM"
                .word   $4393
                .byte   "INPUT"
                .word   $02e8
                .byte   "IF"
                .word   $02e7
                .byte   "INKEY$"
                .word   $00bf
                .byte   "INKEY"
                .word   $00a6
                .byte   "INT"
                .word   $00a8
                .byte   "INSTR("
                .word   $00a7
                .byte   "LIST"
                .word   $10c9
                .byte   "LINE"
                .word   $0086
                .byte   "LOAD"
                .word   $02c8
                .byte   "LOMEM"
                .word   $4392
                .byte   "LOCAL"
                .word   $02ea
                .byte   "LEFT$("
                .word   $00c0
                .byte   "LEN"
                .word   $00a9
                .byte   "LET"
                .word   $04e9
                .byte   "LOG"
                .word   $00ab
                .byte   "LN"
                .word   $00aa
                .byte   "MID$("
                .word   $00c1
                .byte   "MODE"
                .word   $02eb
                .byte   "MOD"
                .word   $0083
                .byte   "MOVE"
                .word   $02ec
                .byte   "NEXT"
                .word   $02ed
                .byte   "NEW"
                .word   $01ca
                .byte   "NOT"
                .word   $00ac
                .byte   "OLD"
                .word   $01cb
                .byte   "ON"
                .word   $02ee
                .byte   "OFF"
                .word   $0087
                .byte   "OR"
                .word   $0084
                .byte   "OPENIN"
                .word   $008e
                .byte   "OPENOUT"
                .word   $00ae
                .byte   "OPENUP"
                .word   $00ad
                .byte   "OPSYS"
                .word   $02ff
                .byte   "OSCLI"
                .word   $02ff
                .byte   "PRINT"
                .word   $02f1
                .byte   "PAGE"
                .word   $4390
                .byte   "PTR"
                .word   $438f
                .byte   "PI"
                .word   $01af
                .byte   "PLOT"
                .word   $02f0
                .byte   "POINT("
                .word   $00b0
                .byte   "PROC"
                .word   $0af2
                .byte   "POS"
                .word   $01b1
                .byte   "RETURN"
                .word   $01f8
                .byte   "REPEAT"
                .word   $00f5
                .byte   "REPORT"
                .word   $01f6
                .byte   "READ"
                .word   $02f3
                .byte   "REM"
                .word   $20f4
                .byte   "RUN"
                .word   $01f9
                .byte   "RAD"
                .word   $00b2
                .byte   "RESTORE"
                .word   $12f7
                .byte   "RIGHT$("
                .word   $00c2
                .byte   "RND"
                .word   $01b3
                .byte   "RENUMBER"
                .word   $10cc
                .byte   "STEP"
                .word   $0088
                .byte   "SAVE"
                .word   $02cd
                .byte   "SGN"
                .word   $00b4
                .byte   "SIN"
                .word   $00b5
                .byte   "SQR"
                .word   $00b6
                .byte   "SPC"
                .word   $0089
                .byte   "STR$"
                .word   $00c3
                .byte   "STRING$("
                .word   $00c4
                .byte   "SOUND"
                .word   $02d4
                .byte   "STOP"
                .word   $01fa
                .byte   "TAN"
                .word   $00b7
                .byte   "THEN"
                .word   $148c
                .byte   "TO"
                .word   $00b8
                .byte   "TAB("
                .word   $008a
                .byte   "TRACE"
                .word   $12fc
                .byte   "TIME"
                .word   $4391
                .byte   "TRUE"
                .word   $01b9
                .byte   "UNTIL"
                .word   $02fd
                .byte   "USR"
                .word   $00ba
                .byte   "VDU"
                .word   $02ef
                .byte   "VAL"
                .word   $00bb
                .byte   "VPOS"
                .word   $01bc
                .byte   "WIDTH"
                .word   $02fe
                .byte   "PAGE"
                .word   $00d0
                .byte   "PTR"
                .word   $00cf
                .byte   "TIME"
                .word   $00d1
                .byte   "LOMEM"
                .word   $00d2
                .byte   "HIMEM"
                .word   $00d3
                .byte   "Missing "
                .word   $008d
;*******************************************************************************
;* Dispatch table for tokens 8E-FF                                             *
;*******************************************************************************
tblFnDispatch:  .word   .LOWORD(exec_OPENIN)                     ;8E
                .word   .LOWORD(exec_PTR)                        ;8F
                .word   .LOWORD(exec_PAGE)                       ;90
                .word   .LOWORD(exec_TIME)
                .word   .LOWORD(exec_LOMEM)
                .word   .LOWORD(exec_HIMEM)
                .word   .LOWORD(exec_ABS)
                .word   .LOWORD(exec_ACS)
                .word   .LOWORD(exec_ADVAL)
                .word   .LOWORD(exec_ASC)
                .word   .LOWORD(exec_ASN)                        ;98
                .word   .LOWORD(exec_ATN)
                .word   .LOWORD(exec_BGET)
                .word   .LOWORD(exec_COS)
                .word   .LOWORD(exec_COUNT)
                .word   .LOWORD(exec_DEG)
                .word   .LOWORD(exec_ERL)
                .word   .LOWORD(exec_ERR)
                .word   .LOWORD(exec_EVAL)                       ;A0
                .word   .LOWORD(exec_EXP)
                .word   .LOWORD(exec_EXT)
                .word   .LOWORD(exec_FALSE)
                .word   .LOWORD(exec_FN)
                .word   .LOWORD(exec_GET)
                .word   .LOWORD(exec_INKEY)
                .word   .LOWORD(exec_INSTR)
                .word   .LOWORD(exec_INT)                        ;A8
                .word   .LOWORD(exec_LEN)
                .word   .LOWORD(exec_LN)
                .word   .LOWORD(exec_LOG)
                .word   .LOWORD(exec_NOT)
                .word   .LOWORD(exec_OPENUP)
                .word   .LOWORD(exec_OPENOUT)
                .word   .LOWORD(exec_PI)
                .word   .LOWORD(exec_POINT)                      ;B0
                .word   .LOWORD(exec_POS)
                .word   .LOWORD(exec_RAD)
                .word   .LOWORD(exec_RND)
                .word   .LOWORD(exec_SGN)
                .word   .LOWORD(exec_SIN)
                .word   .LOWORD(exec_SQR)
                .word   .LOWORD(exec_TAN)
                .word   .LOWORD(exec_TO)                         ;B8
                .word   .LOWORD(exec_TRUE)
                .word   .LOWORD(exec_USR)
                .word   .LOWORD(exec_VAL)
                .word   .LOWORD(exec_VPOS)
                .word   .LOWORD(exec_CHR)
                .word   .LOWORD(exec_GETDollar)
                .word   .LOWORD(exec_INKEYDollar)
                .word   .LOWORD(exec_LEFT)                       ;C0
                .word   .LOWORD(exec_MID)
                .word   .LOWORD(exec_RIGHT)
                .word   .LOWORD(exec_STR)
                .word   .LOWORD(exec_STRING)
                .word   .LOWORD(exec_EOF)
                .word   .LOWORD(exec_AUTO)
                .word   .LOWORD(exec_DELETE)
                .word   .LOWORD(exec_LOAD)                       ;C8
                .word   .LOWORD(exec_LIST)
                .word   .LOWORD(exec_NEW)
                .word   .LOWORD(exec_OLD)
                .word   .LOWORD(exec_RENUMBER)
                .word   .LOWORD(exec_SAVE)
                .word   .LOWORD(exec_EDIT)
                .word   .LOWORD(exec_PTRc)
                .word   .LOWORD(exec_PAGE_set)                   ;DO
                .word   .LOWORD(exec_TIME_set)
                .word   .LOWORD(exec_LOMEM_set)
                .word   .LOWORD(exec_HIMEM_set)
                .word   .LOWORD(exec_SOUND)
                .word   .LOWORD(exec_BPUT)
                .word   .LOWORD(exec_CALL)
                .word   .LOWORD(exec_CHAIN)
                .word   .LOWORD(exec_CLEAR)                      ;D8
                .word   .LOWORD(exec_CLOSE)
                .word   .LOWORD(exec_CLG)
                .word   .LOWORD(exec_CLS)
                .word   .LOWORD(parse_skip_EOL)                  ;DATA
                .word   .LOWORD(parse_skip_EOL)                  ;DEF - skip like DATA
                .word   .LOWORD(exec_DIM)
                .word   .LOWORD(exec_DRAW)
                .word   .LOWORD(exec_END)                        ;EO
                .word   .LOWORD(exec_ENDPROC)
                .word   .LOWORD(exec_ENVELOPE)
                .word   .LOWORD(exec_FOR)
                .word   .LOWORD(exec_GOSUB)
                .word   .LOWORD(exec_GOTO)
                .word   .LOWORD(exec_GCOL)
                .word   .LOWORD(exec_IF)
                .word   .LOWORD(exec_INPUT)                      ;E8
                .word   .LOWORD(exec_LET)
                .word   .LOWORD(exec_LOCAL)
                .word   .LOWORD(exec_MODE)
                .word   .LOWORD(exec_MOVE)
                .word   .LOWORD(exec_NEXT)
                .word   .LOWORD(exec_ON)
                .word   .LOWORD(exec_VDU)
                .word   .LOWORD(exec_PLOT)                       ;F0
                .word   .LOWORD(exec_PRINT)
                .word   .LOWORD(exec_PROC)
                .word   .LOWORD(exec_READ)
                .word   .LOWORD(parse_skip_EOL)                  ;REM
                .word   .LOWORD(exec_REPEAT)
                .word   .LOWORD(exec_REPORT)
                .word   .LOWORD(exec_RESTORE)
                .word   .LOWORD(exec_RETURN)
                .word   .LOWORD(exec_RUN)
                .word   .LOWORD(exec_STOP)
                .word   .LOWORD(exec_COLOUR)
                .word   .LOWORD(exec_TRACE)
                .word   .LOWORD(exec_UNTIL)
                .word   .LOWORD(exec_WIDTH)
                .word   .LOWORD(exec_OSCLI)
;*******************************************************************************
;* Tokenizer token (see tokenizer routine at                                   *
;*******************************************************************************
;* First 3 chars of a candidate string for tokenization have their bottom 5    *
;* bytes packed into 2 byte work compared against this and the next table      *
;*******************************************************************************

tblMnePackSize  =       $6e

tblAsmMnePack:  .byte   $4b
                .byte   $83
                .byte   $84
                .byte   $89
                .byte   $96
                .byte   $b8
                .byte   $b9
                .byte   $d8
                .byte   $d9
                .byte   $f0
                .byte   $01
                .byte   $10
                .byte   $81
                .byte   $90
                .byte   $89
                .byte   $93
                .byte   $a3
                .byte   $a4
                .byte   $a9
                .byte   $38
                .byte   $39
                .byte   $78
                .byte   $01
                .byte   $13
                .byte   $21
                .byte   $a1
                .byte   $c1
                .byte   $19
                .byte   $18
                .byte   $99
                .byte   $98
                .byte   $82
                .byte   $84
                .byte   $02
                .byte   $04
                .byte   $0b
                .byte   $64
                .byte   $24
                .byte   $83
                .byte   $81
                .byte   $73
                .byte   $33
                .byte   $63
                .byte   $61
                .byte   $19
                .byte   $38
                .byte   $e1
                .byte   $41
                .byte   $65
                .byte   $8c
                .byte   $f0
                .byte   $90
                .byte   $29
                .byte   $8d
                .byte   $63
                .byte   $94
                .byte   $73
                .byte   $e5
                .byte   $b1
                .byte   $a9
                .byte   $c5
                .byte   $0c
                .byte   $c3
                .byte   $d3
                .byte   $41
                .byte   $c4
                .byte   $f2
                .byte   $41
                .byte   $83
                .byte   $b0
                .byte   $01
                .byte   $81
                .byte   $43
                .byte   $6c
                .byte   $72
                .byte   $ec
                .byte   $f2
                .byte   $a3
                .byte   $c3
                .byte   $92
                .byte   $9a
                .byte   $18
                .byte   $19
                .byte   $62
                .byte   $42
                .byte   $34
                .byte   $b0
                .byte   $72
                .byte   $98
                .byte   $99
                .byte   $81
                .byte   $98
                .byte   $99
                .byte   $70
                .byte   $b0
                .byte   $b0
                .byte   $a9
                .byte   $b2
                .byte   $4c
                .byte   $a1
                .byte   $ce
                .byte   $d0
                .byte   $35
                .byte   $14
                .byte   $b5
                .byte   $34
                .byte   $44
                .byte   $cb
                .byte   $b0
tblAsmMnePack2: .byte   $0a
                .byte   $0d
                .byte   $0d
                .byte   $0d
                .byte   $0d
                .byte   $10
                .byte   $10
                .byte   $25
                .byte   $25
                .byte   $39
                .byte   $41
                .byte   $41
                .byte   $41
                .byte   $41
                .byte   $4a
                .byte   $4a
                .byte   $4c
                .byte   $4c
                .byte   $4c
                .byte   $50
                .byte   $50
                .byte   $52
                .byte   $53
                .byte   $53
                .byte   $53
                .byte   $10
                .byte   $25
                .byte   $41
                .byte   $41
                .byte   $41
                .byte   $41
                .byte   $41
                .byte   $41
                .byte   $41
                .byte   $41
                .byte   $41
                .byte   $50
                .byte   $50
                .byte   $50
                .byte   $50
                .byte   $50
                .byte   $50
                .byte   $52
                .byte   $52
                .byte   $53
                .byte   $53
                .byte   $4e
                .byte   $60
                .byte   $60
                .byte   $4a
                .byte   $0d
                .byte   $4e
                .byte   $5c
                .byte   $5c
                .byte   $08
                .byte   $09
                .byte   $08
                .byte   $08
                .byte   $08
                .byte   $09
                .byte   $09
                .byte   $0a
                .byte   $0a
                .byte   $0a
                .byte   $0a
                .byte   $05
                .byte   $15
                .byte   $3e
                .byte   $04
                .byte   $0d
                .byte   $0e
                .byte   $30
                .byte   $4c
                .byte   $06
                .byte   $32
                .byte   $49
                .byte   $49
                .byte   $10
                .byte   $25
                .byte   $0d
                .byte   $4e
                .byte   $0e
                .byte   $0e
                .byte   $52
                .byte   $52
                .byte   $09
                .byte   $29
                .byte   $2a
                .byte   $30
                .byte   $30
                .byte   $4e
                .byte   $4e
                .byte   $4e
                .byte   $4a
                .byte   $48
                .byte   $4c
                .byte   $40
                .byte   $40
                .byte   $0a
                .byte   $40
                .byte   $36
                .byte   $36
                .byte   $16
                .byte   $3e
                .byte   $15
                .byte   $0b
                .byte   $5e
                .byte   $09
                .byte   $12
;*******************************************************************************
;* The following table contains the base opcodes for the assembler mnemonics   *
;* in the tables before this                                                   *
;*******************************************************************************
;* The order of the items in this table is important as it defines what        *
;* addressing modes are available for each set of opcodes                      *
;*******************************************************************************
;* TODO: add labels/ranges to tables and code rather than hard-coded           *
;*******************************************************************************
tblAssOpCodes:  .byte   $00
                .byte   $18
                .byte   $d8
                .byte   $58
                .byte   $b8
                .byte   $ca
                .byte   $88
                .byte   $e8
                .byte   $c8
                .byte   $ea
                .byte   $48
                .byte   $08
                .byte   $68
                .byte   $28
                .byte   $40
                .byte   $60
                .byte   $38
                .byte   $f8
                .byte   $78
                .byte   $aa
                .byte   $a8
                .byte   $ba
                .byte   $8a
                .byte   $9a
                .byte   $98
                .byte   $3a
                .byte   $1a
                .byte   $5a
                .byte   $da
                .byte   $7a
                .byte   $fa
                .byte   $ab
                .byte   $2b
                .byte   $8b
                .byte   $0b
                .byte   $4b
                .byte   $5b
                .byte   $5b
                .byte   $7b
                .byte   $7b
                .byte   $1b
                .byte   $1b
                .byte   $3b
                .byte   $3b
                .byte   $9b
                .byte   $bb
                .byte   $eb
                .byte   $eb
                .byte   $fb
                .byte   $6b
                .byte   $02
                .byte   $db
                .byte   $cb
                .byte   $42
                .byte   $90
                .byte   $90
                .byte   $b0
                .byte   $b0
                .byte   $f0
                .byte   $30
                .byte   $d0
                .byte   $10
                .byte   $50
                .byte   $70
                .byte   $80
                .byte   $21
                .byte   $41
                .byte   $01
                .byte   $61
                .byte   $c1
                .byte   $c1
                .byte   $a1
                .byte   $e1
                .byte   $06
                .byte   $46
                .byte   $26
                .byte   $66
                .byte   $c6
                .byte   $e6
                .byte   $9c
                .byte   $9c
                .byte   $e0
                .byte   $c0
                .byte   $00
                .byte   $10
                .byte   $24
                .byte   $4c
                .byte   $20
                .byte   $a2
                .byte   $a0
                .byte   $81
                .byte   $86
                .byte   $84
                .byte   $c2
                .byte   $c2
                .byte   $e2
                .byte   $d4
                .byte   $62
                .byte   $82
                .byte   $f4
                .byte   $54
                .byte   $44

L5933:          jmp     skipSpacesExecImmed

assScanEnter:   jsr     parse_skip_spaces_PTR2
                eor     #']'
                beq     L5933
                jsr     parse_updPTRfromPTR2_yield
                dec     DP_BAS_TXTPTR2_OFF
                jsr     L5A33
                dec     DP_BAS_TXTPTR2_OFF
                lda     DP_BAS_OPT
                lsr     A
                bcc     @L59C9
                lda     DP_BAS_COUNT
                adc     #$06
                sta     DB_BAS_UNK_5A_6+3
                lda     DP_BAS_TMP6+2
                jsr     list_printHexByte
                lda     DP_BAS_TMP6+1
                jsr     list_printHexByte
                lda     DP_BAS_TMP6
                jsr     list_printHexByteAndSpace
                ldx     #$fb
                ldy     DB_BAS_UNK_5A_6+1
                bpl     @L5969
                ldy     DP_BAS_STRLEN
@L5969:         sty     DP_FPB_sgn
                beq     @L5986
                ldy     #$00
@L596F:         inx
                bne     @L597C
                jsr     PrintCRLFresetCOUNT
                ldx     DB_BAS_UNK_5A_6+3
                jsr     list_printXSpaces
                ldx     #$fc
@L597C:         lda     [DP_BAS_TMP6+3],y
                jsr     list_printHexByteAndSpace
                iny
                dec     DP_FPB_sgn
                bne     @L596F
@L5986:         txa
                tay
@L5988:         iny
                beq     @L5992
                ldx     #$03
                jsr     list_printXSpaces
                bra     @L5988

@L5992:         ldx     #$08
                lda     [DP_BAS_TXTPTR2]
                cmp     #'.'
                bne     @L59A9
@L599A:         jsr     doListPrintTokenA
                dex
                bne     @L59A2
                ldx     #$01
@L59A2:         iny
                lda     [DP_BAS_TXTPTR2],y
                cpy     $71
                bne     @L599A
@L59A9:         jsr     list_printXSpaces
                dey
@L59AD:         iny
                cmp     [DP_BAS_TXTPTR2],y
                beq     @L59AD
@L59B2:         lda     [DP_BAS_TXTPTR2],y
                cmp     #':'
                beq     @L59C2
                cmp     #$0d
                beq     @L59C6
@L59BC:         jsr     doListPrintTokenA
                iny
                bra     @L59B2

@L59C2:         cpy     DP_BAS_TXTPTR2_OFF
                bcc     @L59BC
@L59C6:         jsr     PrintCRLFresetCOUNT
@L59C9:         ldy     DP_BAS_TXTPTR2_OFF
                dey
@L59CC:         iny
                lda     [DP_BAS_TXTPTR2],y
                cmp     #':'
                beq     @L59D7
                cmp     #$0d
                bne     @L59CC
@L59D7:         jsr     parse_nextstmt_yield_TXTPTR2_Y
                lda     [DP_BAS_TXTPTR2]
                cmp     #':'
                beq     @L59F2
                lda     DP_BAS_TXTPTR2+1
                cmp     DP_BAS_MEMBASE+1
                bne     @L59EF
                lda     DP_BAS_TXTPTR2+2
                cmp     DP_BAS_MEMBASE+2
                bne     @L59EF
                jmp     reset_prog_prompt

@L59EF:         jsr     CheckEndOfProgContinueThisLn
@L59F2:         jmp     assScanEnter

brk46_Label:    brk     $46

                .byte   "Label"
                .byte   $00

L59FD:          lda     DP_BAS_OPT
                and     #OPT_8_UK
                bne     @L5A1C
                jsr     findVarAtPTR2
                beq     @L5A1C
                jsr     GetVarVal
                jsr     checkTypeInAConvertToINT
                ldx     #$04
                ldy     #INTVAR_P+3
@lp:            lda     DP_BAS_INT_WA-1,x
                cmp     [DP_BAS_INTVARSptr],y
                bne     brk46_Label
                dey
                dex
                bne     @lp
@L5A1C:         jsr     findVarOrAllocEmpty
                beq     L5A7D
                bcs     L5A7D
                jsr     pushINTWA_on_hw_stack
                jsr     getPpctAsINT
                sta     DP_BAS_40_VARTYPE
                jsr     storeEvaledExprinStackedVarPTr
                jsr     parseOFF2eqYeqOFF
                sty     $71
L5A33:          jsr     parse_skip_spaces_PTR2
                ldy     #$00
                stz     DP_FPB_sgn
                cmp     #':'
                beq     L5AA2
                cmp     #$0d
                beq     L5AA2
                cmp     #'\'
                beq     L5AA2
                cmp     #'.'
                beq     L59FD
                dec     DP_BAS_TXTPTR2_OFF
                ldx     #$03
; pack three char's lowest 5 bits into a packed word and compare against lookup
; table to try and find a mnemonic
@packlp:        ldy     DP_BAS_TXTPTR2_OFF
                inc     DP_BAS_TXTPTR2_OFF
                lda     [DP_BAS_TXTPTR2],y
                bmi     L5A80
                cmp     #' '
                beq     @L5A6A
                ldy     #$05
                asl     A
                asl     A
                asl     A
@packlp2:       asl     A
                rol     DP_FPB_sgn
                rol     DB_BAS_UNK_5A_6+3
                dey
                bne     @packlp2
                dex
                bne     @packlp
@L5A6A:         ldx     #tblMnePackSize-1
                lda     DP_FPB_sgn
@L5A6E:         cmp     tblAsmMnePack-1,x
                bne     @L5A7A
                ldy     tblAsmMnePack2-1,x
                cpy     DB_BAS_UNK_5A_6+3
                beq     L5A97
@L5A7A:         dex
                bne     @L5A6E
L5A7D:          jmp     brk10_Syntax

L5A80:          ldx     #$42
                cmp     #tknAND
                beq     L5A97
                inx
                cmp     #tknEOR
                beq     L5A97
                inx
                cmp     #tknEOR+2
                bne     L5A7D
                lda     #'A'
                jsr     newCheckForLetter
                bne     L5A7D
L5A97:          lda     tblAssOpCodes-1,x
                sta     DP_BAS_47_ASS_UK
                ldy     #$01
                cpx     #$37
                bcs     L5B1E
L5AA2:          sty     DB_BAS_UNK_5A_6+1
                ldy     #INTVAR_P+2                     ;TODO: check and optimize
                lda     [DP_BAS_INTVARSptr],y
                sta     DP_BAS_TMP6+2
                xba
                dey
                lda     [DP_BAS_INTVARSptr],y
                tax
                dey
                stx     DP_BAS_TMP6+1
                lda     [DP_BAS_INTVARSptr],y
                tay                                     ;does this do anything?
                sty     DP_BAS_TMP6
                lda     DP_BAS_OPT
                and     #$04
                cmp     #$01
                bcc     @L5ACC
                ldy     #INTVAR_O+2
                lda     [DP_BAS_INTVARSptr],y
                xba
                dey
                lda     [DP_BAS_INTVARSptr],y
                tax
                dey
                lda     [DP_BAS_INTVARSptr],y
                tay
@L5ACC:         sty     DP_BAS_TMP6+3
                stx     DP_BAS_TMP6+4
                xba
                sta     DB_BAS_UNK_5A_6
                ldy     DB_BAS_UNK_5A_6+1
                beq     L5B1D
                bpl     L5ADD
                ldy     DP_BAS_STRLEN
                beq     L5B1D
L5ADD:          dey
                tyx
                lda     DP_BAS_INT_WA-1,x
                bit     DB_BAS_UNK_5A_6+1
                bpl     @L5AE7
                lda     [DP_BAS_STRWKSP_L],y
@L5AE7:         eor     #$ff
                sta     [DP_BAS_TMP6+3],y
                cmp     [DP_BAS_TMP6+3],y
                bne     brk_47_BadMemory
                eor     #$ff
                sta     [DP_BAS_TMP6+3],y
                cmp     [DP_BAS_TMP6+3],y
                beq     L5B04
brk_47_BadMemory:
                brk     $47

                .asciiz "Bad Memory"

L5B04:          phy
                clc
                ldy     #INTVAR_P
                jsr     assAdd1ToPtrAtY
                lda     DP_BAS_OPT
                and     #$04
                cmp     #$01
                bcc     @L5B19
                clc
                ldy     #DP_BAS_WIDTH
                jsr     assAdd1ToPtrAtY
@L5B19:         ply
                tya
                bne     L5ADD
L5B1D:          rts

L5B1E:          cpx     #$42
                bcs     L5B84
                lda     #$00
L5B24:          pha
                jsr     evalForceINT
                clc
                ldy     #INTVAR_P
                lda     DP_BAS_INT_WA
                sbc     [DP_BAS_INTVARSptr],y
                xba
                lda     DP_BAS_INT_WA+1
                iny
                sbc     [DP_BAS_INTVARSptr],y
                iny
                tax
                lda     DP_BAS_INT_WA+2
                sbc     [DP_BAS_INTVARSptr],y
                tay
                xba
                clc
                sbc     $01,S
                bcs     @L5B48
                cpx     #$01
                dex
                bcs     @L5B48
                dey
@L5B48:         sta     DP_BAS_INT_WA
                stx     DP_BAS_INT_WA+1
                sty     DP_BAS_INT_WA+2
                ldy     #$03
                pla
                bne     @L5B7A
                dey
                txa
                ldx     DP_BAS_INT_WA
                ora     DP_BAS_INT_WA+2
                beq     @L5B7E
                lda     DP_BAS_INT_WA+1
                ora     DP_BAS_INT_WA+2
@L5B5F:         inc     A
                bne     @L5B65
                txa
                bmi     L5B81
@L5B65:         lda     DP_BAS_OPT
                and     #$02
                beq     L5B81
                brk     $01

                .asciiz "Out of range"

@L5B7A:         lda     DP_BAS_INT_WA+2
                bne     @L5B5F
@L5B7E:         txa
                bmi     @L5B65
L5B81:          jmp     L5AA2

L5B84:          cpx     #$4a
                bcs     L5BBF
                jsr     newCheckForL
                pha
                beq     L5BC7
                jsr     parse_skipSpacesPTR2_cmp_HASH
                bne     L5BCA
                pla
                jsr     L5F52
L5B97:          lda     #OPT_20_UK
L5B99:          and     DP_BAS_OPT
                beq     L5BA0
                jmp     L5E02

L5BA0:          jsr     evalForceINT
L5BA3:          lda     DP_BAS_INT_WA+1
                ora     DP_BAS_INT_WA+2
                ora     DP_BAS_INT_WA+3
                bne     brk_02_Byte
L5BAB:          ldy     #$02
                bra     L5B81

brk_02_Byte:    brk     $02

                .byte   "Byte"

brk_48_Word:    brk     $48

                .asciiz "Word"

L5BBC:          jmp     L5CAE

L5BBF:          cpx     #$5b
                bne     L5BBC
                jsr     newCheckForL
                pha
L5BC7:          jsr     parse_skip_spaces_PTR2
L5BCA:          cmp     #'('
                bne     L5C3B
                jsr     evalForceINT
                jsr     parse_skip_spaces_PTR2
                cmp     #')'
                bne     L5C0F
                jsr     L5F4F
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                beq     L5BFD
                pla
                bne     @L5BEA
                lda     DP_BAS_47_ASS_UK
                sec
                sbc     #$0b
                sta     DP_BAS_47_ASS_UK
@L5BEA:         inc     DP_BAS_47_ASS_UK
L5BEC:          bra     L5BA3

L5BEE:          lda     DP_BAS_47_ASS_UK
                adc     #$11
                sta     DP_BAS_47_ASS_UK
                pla
                beq     L5C30
                pha
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                bne     brk_03_Index
L5BFD:          pla
                bne     @L5C06
                lda     DP_BAS_47_ASS_UK
                adc     #$05
                sta     DP_BAS_47_ASS_UK
@L5C06:         jsr     parseCheckForX
                cmp     #'Y'
                beq     L5BEC
                bra     brk_03_Index

L5C0F:          cmp     #','
                bne     brk_03_Index
                jsr     parseCheckForX
                beq     @L5C1C
                cmp     #'S'
                bne     brk_03_Index
@L5C1C:         tax
                jsr     parse_skip_spaces_PTR2
                cmp     #')'
                bne     brk_03_Index
                cpx     #$53
                beq     L5BEE
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                beq     brk_03_Index
                pla
                bne     L5BEC
L5C30:          jmp     brk10_Syntax

brk_03_Index:   brk     $03

                .byte   "Index"
                .byte   $00

L5C3B:          jsr     decOff2EvalForceInt
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                bne     L5C86
                jsr     L5F4F
                jsr     parseCheckForX
                beq     L5C86
                cmp     #'Y'
                beq     L5C5F
                cmp     #'S'
                bne     brk_03_Index
                pla
                beq     L5C30
                lda     DP_BAS_47_ASS_UK
                sbc     #$0e
                sta     DP_BAS_47_ASS_UK
                bra     L5BEC

L5C5E:          pha
L5C5F:          jsr     L5F52
                ldy     #$03
                pla
                cmp     #$01
                bcc     @L5C6F
                lda     DP_BAS_INT_WA+2
                eor     DP_ASS_ASS_FLAGS_UK
                beq     @L5C7C
@L5C6F:         lda     DP_BAS_47_ASS_UK
                and     #$0f
                eor     #$0d
                bne     @L5C81
                lda     #$0f
                tsb     DP_BAS_47_ASS_UK
                iny
@L5C7C:         lda     DP_BAS_INT_WA+3
                beq     L5CE5
                sec
@L5C81:         bcc     L5C30
                jmp     brk_99_Bank

L5C86:          jsr     L5F55
                pla
                beq     L5C5E
                bra     L5C91

L5C8E:          jsr     L5F55
L5C91:          jsr     AeqWA1OR2OR3
                bne     L5C5E
                jmp     L5BAB

assAdd1ToPtrAtY:
                lda     [DP_BAS_INTVARSptr],y
                adc     #$01
                sta     [DP_BAS_INTVARSptr],y
                iny
                lda     [DP_BAS_INTVARSptr],y
                adc     #$00
                sta     [DP_BAS_INTVARSptr],y
                iny
                lda     [DP_BAS_INTVARSptr],y
                adc     #$00
                sta     [DP_BAS_INTVARSptr],y
                rts

L5CAE:          cpx     #$50
                bcs     L5CE8
                jsr     parseCheckForX
                cmp     #'A'
                beq     L5CCC
L5CB9:          jsr     decOff2EvalForceInt
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                bne     L5C91
                jsr     L5F4F
                jsr     parseCheckForX
                beq     L5C91
L5CC9:          brl     brk_03_Index

L5CCC:          iny
                lda     [DP_BAS_TXTPTR2],y
                jsr     checkIsValidVariableNameChar
                bcs     L5CB9
                ldy     #$16
                cpx     #$4e
                bcc     L5CE0
                bne     @L5CDE
                ldy     #$36
@L5CDE:         sty     DP_BAS_47_ASS_UK
L5CE0:          jsr     L5F55
                ldy     #$01
L5CE5:          jmp     L5AA2

L5CE8:          cpx     #$52
                bcs     @L5D16
                jsr     evalForceINT
                ldy     #$03
                ldx     #$01
                lda     DP_BAS_INT_WA+1
                ora     DP_BAS_INT_WA+2
                ora     DP_BAS_INT_WA+3
                bne     @L5D02
                ldx     #$0f
                lda     #$64
                sta     DP_BAS_47_ASS_UK
                dey
@L5D02:         phy
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                bne     @L5D12
                jsr     parseCheckForX
                bne     L5CC9
                txa
                adc     DP_BAS_47_ASS_UK
                sta     DP_BAS_47_ASS_UK
@L5D12:         ply
                brl     L5AA2

@L5D16:         cpx     #$56
                bcs     L5D3C
                cpx     #$54
                bcs     @L5D25
                jsr     parse_skipSpacesPTR2_cmp_HASH
                beq     L5D9D
                dec     DP_BAS_TXTPTR2_OFF
@L5D25:         jsr     evalForceINT
L5D28:          jmp     L5C8E

L5D2B:          jsr     parse_skipSpacesPTR2_cmp_HASH
                bne     L5CB9
                ldy     #$89
                sty     DP_BAS_47_ASS_UK
                jmp     L5B97

                .byte   $10                             ;TODO: dead?
                .byte   $02

L5D39:          jmp     L5DCA

L5D3C:          beq     L5D2B
                cpx     #$59
                bcs     L5D39
                jsr     newCheckForL
                pha
                jsr     parse_skip_spaces_PTR2
                cmp     #'('
                beq     L5DA2
                ldy     #$03
                pla
                bne     @L5D5B
                lda     DP_BAS_47_ASS_UK
                clc
                adc     L5CE0,x
                sta     DP_BAS_47_ASS_UK
                iny
@L5D5B:         phy
                jsr     decOff2EvalForceInt
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                beq     L5D83
                ply
L5D65:          lda     DP_BAS_INT_WA+3
                bne     brk_99_Bank
                cpy     #$04
                beq     @L5D77
                phy
                ldy     #$42
                lda     DP_BAS_INT_WA+2
                cmp     [DP_BAS_INTVARSptr],y
                bne     brk_99_Bank
                ply
@L5D77:         jmp     L5AA2

brk_99_Bank:    brk     $49

                .byte   $42
                .byte   $61
                .byte   $6e
                .byte   $6b
                .byte   $00

L5D81:          cpx     #$58
L5D83:          beq     L5DEF
                pla
                bne     L5E05
                lda     #$dc
                sta     DP_BAS_47_ASS_UK
                bra     L5E05

L5D8E:          pla
                beq     L5DEF
                cpx     #$58
                bne     @L5D99
                lda     #$fc
                sta     DP_BAS_47_ASS_UK
@L5D99:         ldy     #$03
                bra     L5D65

L5D9D:          lda     #OPT_10_UK
                jmp     L5B99

L5DA2:          phx
                jsr     L5F4F
                jsr     L5F4F
                jsr     evalForceINT
                plx
                jsr     parse_skip_spaces_PTR2
                cmp     #')'
                beq     L5D81
                cmp     #','
                bne     L5DC7
                jsr     L5F4F
                jsr     parseCheckForX
                bne     L5DC7
                jsr     parse_skip_spaces_PTR2
                cmp     #')'
                beq     L5D8E
L5DC7:          jmp     brk_03_Index

L5DCA:          cpx     #$5e
                bcs     @L5DE4
                lda     DP_FPB_sgn
                eor     #$01
                and     #$1f
                pha
                cpx     #$5b
                bcs     L5E2B
                jsr     parse_skipSpacesPTR2_cmp_HASH
                bne     L5E13
                pla
                bra     L5D9D

@L5DE1:         jmp     L5BA0

@L5DE4:         cpx     #$61
                beq     @L5DE1
                bcs     L5DF2
                jsr     parse_skipSpacesPTR2_cmp_HASH
                beq     @L5DE1
L5DEF:          jmp     brk10_Syntax

L5DF2:          cpx     #$64
                beq     @L5DFD
                bcs     L5E47
                lda     #$01
                jmp     L5B24

@L5DFD:         jsr     parse_skipSpacesPTR2_cmp_HASH
                bne     L5DEF
L5E02:          jsr     evalForceINT
L5E05:          ldy     #$03
                lda     DP_BAS_INT_WA+2
                ora     DP_BAS_INT_WA+3
                beq     @L5E10
                jmp     brk_48_Word

@L5E10:         jmp     L5AA2

L5E13:          jsr     decOff2EvalForceInt
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                bne     @L5E27
                jsr     parse_skip_spaces_PTR2
                and     #$1f
                cmp     $01,S
                bne     L5DC7
                jsr     L5F4F
@L5E27:         pla
                jmp     L5D28

L5E2B:          jsr     evalForceINT
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                bne     L5E97
                jsr     parse_skip_spaces_PTR2
                and     #$1f
                cmp     $01,S
                bne     L5DC7
                jsr     L5F4F
                jsr     AeqWA1OR2OR3
                beq     L5E97
                jmp     brk_02_Byte

L5E47:          cpx     #$67
                bcs     @L5E6B
                jsr     evalForceINT
                jsr     AeqWA1OR2OR3
                bne     L5E9B
                lda     DP_BAS_INT_WA
                pha
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                bne     L5DEF
                jsr     evalForceINT
                jsr     AeqWA1OR2OR3
                bne     L5E9B
                pla
                sta     DP_BAS_INT_WA+1
                bra     L5E05

@L5E68:         jmp     L5F12

@L5E6B:         beq     @L5E68
                phx
                jsr     evalForceINT
                plx
                cpx     #$69
                bcc     L5EDC
                beq     L5EE7
                cpx     #$6b
                bcs     @L5E7F
                brl     L5F00

@L5E7F:         beq     L5EFA
                cpx     #$6d
                beq     L5EA4
                jsr     AeqWA1OR2OR3
                bne     L5E9B
                lda     DP_BAS_INT_WA
                sta     DP_ASS_ASS_FLAGS_UK
L5E8E:          lda     DP_BAS_OPT
                sta     $43
                ldy     #$00
                jmp     L5F3D

L5E97:          pla
                jmp     L5C91

L5E9B:          jmp     brk_99_Bank

L5E9E:          jmp     brk_02_Byte

L5EA1:          jmp     brk19_bad_MODE

L5EA4:          jsr     parseEor0Next
                and     #$df                            ;;to upper?
                cmp     #'B'
                beq     @L5EB0
                brl     L5F30

@L5EB0:         jsr     evalForceINT
                lda     DP_BAS_INT_WA+3
                bmi     L5E8E
                jsr     AeqWA1OR2OR3
                bne     L5E9E
                lda     DP_BAS_INT_WA
                pha
                stz     DP_BAS_INT_WA
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                bne     @L5ECE
                jsr     evalForceINT
                jsr     AeqWA1OR2OR3
                bne     L5E9E
@L5ECE:         plx
                stx     DP_BAS_STRLEN
                lda     DP_BAS_INT_WA
@L5ED3:         txy
                dey
                sta     [DP_BAS_STRWKSP_L],y
                dex
                bne     @L5ED3
                bra     L5F4B

L5EDC:          jsr     AeqWA1OR2OR3
                bne     L5E9E
                lda     DP_BAS_INT_WA
L5EE3:          sta     DP_BAS_OPT
                bra     L5E8E

L5EE7:          lda     #OPT_40_UK
                trb     DP_BAS_OPT
                jsr     AeqWA1OR2OR3
                ora     DP_BAS_INT_WA
                beq     L5E8E
                lda     DP_BAS_OPT
                and     #OPT_8F_MASK
                ora     #OPT_40_UK
                bra     L5EE3

L5EFA:          lda     DP_BAS_OPT
                and     #OPT_40_UK
                bne     L5EA1
L5F00:          jsr     AeqWA1OR2OR3
                bne     L5E9E
                lda     DP_BAS_INT_WA
                and     #OPT_30_MASK
                trb     DP_BAS_OPT
                bcc     L5E8E
                tsb     DP_BAS_OPT
                brl     L5E8E

L5F12:          ldx     #$01
                jsr     parseEor0Next
                and     #$df                            ;to upper
                cmp     #'B'
                beq     L5F33
                inx
                cmp     #'W'
                beq     L5F33
                inx
                cmp     #'L'
                beq     L5F33
                inx
                cmp     #'D'
                beq     L5F33
                cmp     #'S'
                beq     L5F43
L5F30:          jmp     brk10_Syntax

L5F33:          phx
                jsr     evalForceINT
                ldx     #$47
                jsr     storeWAatDPX
                ply
L5F3D:          jmp     L5AA2

jmp_brk06_type_mismatch:
                jmp     brk06_type_mismatch

L5F43:          jsr     evalExpressionMAIN
                bne     jmp_brk06_type_mismatch
                jsr     parseOFF2eqYeqOFF
L5F4B:          ldy     #$ff
                bra     L5F3D

L5F4F:          jsr     L5F52
L5F52:          jsr     L5F55
L5F55:          lda     DP_BAS_47_ASS_UK
                clc
                adc     #$04
                sta     DP_BAS_47_ASS_UK
                rts

parseCheckForX: jsr     parse_skip_spaces_PTR2
                and     #$df                            ;to upper
                cmp     #'X'
                rts

parse_skipSpacesPTR2_cmp_HASH:
                jsr     parse_skip_spaces_PTR2
                cmp     #'#'
                rts

parse_SkipSpacesPTR2_cmp_COMMA:
                jsr     parse_skip_spaces_PTR2
                cmp     #','
                rts

parseEor0Next:  lda     #$00
parseEorANext:  ldy     DP_BAS_TXTPTR2_OFF
                inc     DP_BAS_TXTPTR2_OFF
                eor     [DP_BAS_TXTPTR2],y
                rts

newCheckForL:   lda     #'L'                            ;TODO: optimize away?
newCheckForLetter:
                jsr     parseEorANext                   ;TODO: optimize away cf USR CALL
                and     #$df
                beq     rtsL5F87
                dec     DP_BAS_TXTPTR2_OFF
                eor     #$00
rtsL5F87:       rts

AeqWA1OR2OR3:   lda     DP_BAS_INT_WA+1
                ora     DP_BAS_INT_WA+2
                ora     DP_BAS_INT_WA+3
                rts

storeTokenAndCloseUpLine:
                sta     [DP_BAS_TMP6]
                clc
                tya
                adc     DP_BAS_TMP6
                sta     DP_BAS_TMP6+3
                ldy     #$00
                tya
                adc     DP_BAS_TMP6+1
                sta     DP_BAS_TMP6+4
                tya
                adc     DP_BAS_TMP6+2
                sta     DP_BAS_TMP6+5
@lp:            iny
                lda     [DP_BAS_TMP6+3],y
                sta     [DP_BAS_TMP6],y
                cmp     #$0d
                bne     @lp
                rts

tokenizeLineNo: and     #$0f
                sta     DP_FPB_exp
                ldx     #$00
                ldy     #$00
tokLinLp:       iny
                lda     [DP_BAS_TMP6],y
                jsr     checkIsNumeric
                bcc     tokLineNoSk1
                and     #$0f
                pha
                stx     DP_FPB_mant
                lda     DP_FPB_exp
                asl     A
                rol     DP_FPB_mant
                bmi     tokLinOv
                asl     A
                rol     DP_FPB_mant
                bmi     tokLinOv
                adc     DP_FPB_exp
                sta     DP_FPB_exp
                txa
                adc     DP_FPB_mant
                asl     DP_FPB_exp
                rol     A
                bmi     tokLinOv
                bcs     tokLinOv
                tax
                pla
                adc     DP_FPB_exp
                sta     DP_FPB_exp
                bcc     tokLinLp
                inx
                bpl     tokLinLp
                pha
tokLinOv:       pla
                sec
                rts

tokLineNoSk1:   dey
                lda     #tknLineNo
                jsr     storeTokenAndCloseUpLine
                lda     DP_BAS_TMP6
                sta     DP_BAS_TMP6+3
                lda     DP_BAS_TMP6+1
                sta     DP_BAS_TMP6+4
                lda     DP_BAS_TMP6+2
                sta     DB_BAS_UNK_5A_6
                jsr     incTMP6ptr
                jsr     incTMP6ptr
                jsr     incTMP6ptr
@lp:            lda     [DP_BAS_TMP6+3],y
                sta     [DP_BAS_TMP6],y
                dey
                bne     @lp
                ldy     #$03
int16atFPB2toBUFasTOKENIZED:
                txa
                ora     #$40
                sta     [DP_BAS_TMP6+3],y
                dey
                lda     DP_FPB_exp
                and     #$3f
                ora     #$40
                sta     [DP_BAS_TMP6+3],y
                dey
                lda     #$3f
                trb     DP_FPB_exp
                txa
                and     #$c0
                lsr     A
                lsr     A
                ora     DP_FPB_exp
                lsr     A
                lsr     A
                eor     #$54
                sta     [DP_BAS_TMP6+3],y
clcRTSL602F:    clc
                rts

checkIsValidVariableNameChar:
                cmp     #'{'
                bcs     clcRTSL602F
                cmp     #'_'
                bcs     rtsL6047
                cmp     #'['
                bcs     clcRTSL602F
                cmp     #'@'
                bcs     rtsL6047
checkIsNumeric: cmp     #':'
                bcs     clcRTSL602F
                cmp     #'0'
rtsL6047:       rts

checkDotOrNumeric:
                cmp     #'.'
                bne     checkIsNumeric
                rts

getAatTMP6ptrAndInc:
                lda     [DP_BAS_TMP6]
incTMP6ptr:     inc     DP_BAS_TMP6
                bne     rtsL6090
                inc     DP_BAS_TMP6+1
                bne     rtsL6090
                inc     DP_BAS_TMP6+2
                rts

incTMP6ptrLDA:  jsr     incTMP6ptr
                lda     [DP_BAS_TMP6]
                rts

tokenizeAtTMP6ptr_plus1:
                jsr     incTMP6ptr
tokenizeAtTMP6ptr:
                lda     [DP_BAS_TMP6]
                cmp     #$0d
                beq     rtsL6090
                cmp     #' '
                beq     tokenizeAtTMP6ptr_plus1
                cmp     #'&'
                bne     tokNotAmper
@lp:            jsr     incTMP6ptrLDA
                jsr     checkIsNumeric
                bcs     @lp
                cmp     #'A'
                bcc     tokenizeAtTMP6ptr
                cmp     #'G'
                bcc     @lp
tokNotAmper:    cmp     #'"'
                bne     tokNotQuot
tokQuotLp:      jsr     incTMP6ptrLDA
                cmp     #'"'
                beq     tokenizeAtTMP6ptr_plus1
                cmp     #$0d
                bne     tokQuotLp
rtsL6090:       rts

tokNotQuot:     cmp     #':'
                bne     tokNotColon
                jsr     incTMP6ptr
tokNotExpectLineNo:
                stz     DP_FPB_sgn
tokNotExpectLineNo2:
                stz     DB_BAS_UNK_5A_6+3
                bra     tokenizeAtTMP6ptr

tokNotColon:    cmp     #','
                beq     tokenizeAtTMP6ptr_plus1
                cmp     #'*'
                bne     tokNotStar
                lda     DP_FPB_sgn
                beq     rtsL6090
tokNotKeyword:  ldx     #$ff
                stx     DP_FPB_sgn
                stz     DB_BAS_UNK_5A_6+3
                bra     tokenizeAtTMP6ptr_plus1

tokNotStar:     cmp     #'.'
                beq     tokDot
                jsr     checkIsNumeric
                bcc     tokNotNum
                ldx     DB_BAS_UNK_5A_6+3
                beq     tokDot
                jsr     tokenizeLineNo
                bcc     tokenizeAtTMP6ptr_plus1
tokDot:         lda     [DP_BAS_TMP6]
                jsr     checkDotOrNumeric
                bcc     tokNextSetFlag0_FF
                jsr     incTMP6ptr
                bra     tokDot

tokNextSetFlag0_FF:
                ldx     #$ff
                stx     DP_FPB_sgn
                bra     tokNotExpectLineNo2

tokNotKey2:     jsr     checkIsValidVariableNameChar
                bcc     tokNotKeyword
tokNotKey_SkipVarName:
                lda     [DP_BAS_TMP6]
                jsr     checkIsValidVariableNameChar
                bcc     tokNextSetFlag0_FF
                jsr     incTMP6ptr
                bra     tokNotKey_SkipVarName

tokNotNum:      cmp     #'@'
                bcc     tokNotKeyword
                cmp     #'X'
                bcs     tokNotKey2
                ldx     #<tblTokensAsc
                stx     DP_BAS_TMP6+3
                ldx     #>tblTokensAsc
                stx     DP_BAS_TMP6+4
tokKeyCmp:      ldy     #$00
                cmp     (DP_BAS_TMP6+3)
                bcc     tokNotKey_SkipVarName
                bne     tokSkipEndTryNext
tokKeyCmpLp:    iny
                lda     (DP_BAS_TMP6+3),y
                bmi     tokKeyFound
                cmp     [DP_BAS_TMP6],y
                beq     tokKeyCmpLp
                lda     [DP_BAS_TMP6],y
                cmp     #'.'
                beq     tokKeyAbbrevLp0
tokSkipEndTryNext:
                iny
                lda     (DP_BAS_TMP6+3),y
                bpl     tokSkipEndTryNext
                cmp     #tknWIDTH
                bne     tokMoveNextTkn
                bcs     tokNotKey_SkipVarName
tokKeyAbbrevLp0:
                iny
tokKeyAbbrevLp: lda     (DP_BAS_TMP6+3),y
                bmi     tokKeyFound
                inc     DP_BAS_TMP6+3
                bne     tokKeyAbbrevLp
                inc     DP_BAS_TMP6+4
                bra     tokKeyAbbrevLp

tokMoveNextTkn: sec
                iny
                tya
                adc     DP_BAS_TMP6+3
                sta     DP_BAS_TMP6+3
                bcc     @sk
                inc     DP_BAS_TMP6+4
@sk:            lda     [DP_BAS_TMP6]
                bra     tokKeyCmp

tokKeyFound:    tax
                iny
                lda     (DP_BAS_TMP6+3),y
                sta     DP_FPB_exp
                dey
                lsr     A
                bcc     tokKeyFound2
                lda     [DP_BAS_TMP6],y
                jsr     checkIsValidVariableNameChar
                bcs     tokNotKey_SkipVarName
tokKeyFound2:   txa
                bit     DP_FPB_exp
                bvc     tokNOTPSEUDO
                ldx     DP_FPB_sgn
                bne     tokNOTPSEUDO
                adc     #$40
tokNOTPSEUDO:   dey
                jsr     storeTokenAndCloseUpLine
                ldx     #$ff
                lda     DP_FPB_exp
                lsr     A
                lsr     A
                bcc     tokNOTNEXTMID
                stx     DP_FPB_sgn
                stz     DB_BAS_UNK_5A_6+3
tokNOTNEXTMID:  lsr     A
                bcc     tokNOTNEXTSTART
                stz     DP_FPB_sgn
                stz     DB_BAS_UNK_5A_6+3
tokNOTNEXTSTART:
                lsr     A
                bcc     tokSkipNotFNPROC
                pha
                ldy     #$01
@tokSkipPROCNAMElp:
                lda     [DP_BAS_TMP6],y
                jsr     checkIsValidVariableNameChar
                bcc     tokSkipNotFNPROCpla
                jsr     incTMP6ptr
                bra     @tokSkipPROCNAMElp

tokSkipNotFNPROCpla:
                pla
tokSkipNotFNPROC:
                lsr     A
                bcc     tokNotNEXTLINENO
                stx     DB_BAS_UNK_5A_6+3
tokNotNEXTLINENO:
                lsr     A
                bcs     rtsL6190
                jmp     tokenizeAtTMP6ptr_plus1

parse_skip_spaces:
                ldy     DP_BAS_TXTPTR_OFF
                inc     DP_BAS_TXTPTR_OFF
                lda     [DP_BAS_TXTPTR],y
                cmp     #' '
                beq     parse_skip_spaces
rtsL6190:       rts

parse_skip_spaces_PTR2:
                ldy     DP_BAS_TXTPTR2_OFF
                inc     DP_BAS_TXTPTR2_OFF
                lda     [DP_BAS_TXTPTR2],y
                cmp     #' '
                beq     parse_skip_spaces_PTR2
L619B:          rts

parse_skip_space_CMPcomma:
                jsr     parse_skip_spaces
                cmp     #','
                rts

parse_skip_spaces_CMPcommaBRK:
                jsr     parse_skip_space_CMPcomma
                beq     L619B
brk_05_missing_comma:
                brk     $05

                .byte   tknMissing
                .byte   ','
                .byte   $00

exec_CHAIN:     jsr     doLOAD
               brl     doRUN

exec_OLD:       jsr     parse_nextstmt_yield_TXTOFF2
                jsr     doOLD
                brl     resetVarsImmedPrompt

doOLD:          lda     DP_BAS_PAGE
                sta     DP_BAS_TMP6
                lda     DP_BAS_PAGE+1
                sta     DP_BAS_TMP6+1
                lda     DP_BAS_PAGE+2
                sta     DP_BAS_TMP6+2
                lda     #$00
                sta     [DP_BAS_TMP6],y
                jmp     findTOP

        .IFDEF COMMUNICATOR
resetSpecialPAGEsandTOPs:
                ldy     #$00
@lp:            jsr     setPAGEFromSpecialVarY
                phy
                jsr     findTOP
                ply
                lda     DP_BAS_TOP
                sta     [DP_BAS_SPECIAL_VARSptr],y
                iny
                lda     DP_BAS_TOP+1
                sta     [DP_BAS_SPECIAL_VARSptr],y
                iny
                lda     DP_BAS_TOP+2
                sta     [DP_BAS_SPECIAL_VARSptr],y
                iny
                cpy     DP_BAS_SPECIALVAR_SZ
                bne     @lp
                rts

setPAGEFromSpecialVarY:
                lda     [DP_BAS_SPECIAL_VARSptr],y
                sta     DP_BAS_PAGE
                iny
                lda     [DP_BAS_SPECIAL_VARSptr],y
                sta     DP_BAS_PAGE+1
                iny
                lda     [DP_BAS_SPECIAL_VARSptr],y
                sta     DP_BAS_PAGE+2
                iny
                rts

setTOPfromSpecialVarY:
                lda     [DP_BAS_SPECIAL_VARSptr],y
                sta     DP_BAS_TOP
                iny
                lda     [DP_BAS_SPECIAL_VARSptr],y
                sta     DP_BAS_TOP+1
                iny
                lda     [DP_BAS_SPECIAL_VARSptr],y
                sta     DP_BAS_TOP+2
                iny
                rts

; not sure of the purpose of this looks like separate program segments
setPAGETOPfromTXTPTR:
                ldy     #$00                            ;not sure of the purpose of this looks like separate program segments
@lp:            jsr     compareXtoSpecialVarY
                bcc     @L6223
                jsr     compareXtoSpecialVarY
                bcs     @L6226
                sec
                tya
                sbc     #$06
                tay
@L621D:         jsr     setPAGEFromSpecialVarY
                brl     setTOPfromSpecialVarY

@L6223:         iny
                iny
                iny
@L6226:         cpy     DP_BAS_SPECIALVAR_SZ
                bne     @lp
                ldy     #$00
                bra     @L621D

compareXtoSpecialVarY:
                lda     $00,x
                cmp     [DP_BAS_SPECIAL_VARSptr],y
                iny
                lda     $01,x
                sbc     [DP_BAS_SPECIAL_VARSptr],y
                iny
                lda     $02,x
                sbc     [DP_BAS_SPECIAL_VARSptr],y
                iny
                rts

qrySetPAGETOPSpecial:
                lda     #$80
                tsb     DP_BAS_CO_FLAGS
                bne     @rts
                jsr     resetSpecialPAGEsandTOPs
                ldy     #$00
                jsr     setPAGEFromSpecialVarY
                jsr     setTOPfromSpecialVarY
@rts:           rts
        .ENDIF

exec_RUN:       jsr     parse_nextstmt_yield_TXTOFF2
doRUN:          
        .IFDEF COMMUNICATOR
                jsr     qrySetPAGETOPSpecial
        .ENDIF
                jsr     int_CLEAR
                lda     DP_BAS_PAGE+2
                sta     DP_BAS_TXTPTR2+2
                lda     DP_BAS_PAGE+1
                sta     DP_BAS_TXTPTR2+1
                lda     DP_BAS_PAGE
                sta     DP_BAS_TXTPTR2
                brl     runFromTXTPTR

exec_LOAD:      jsr     doLOAD
               bra     resetVarsImmedPrompt

exec_END:       jsr     parse_nextstmt_yield_TXTOFF2
                jsr     findTOP
                bra     reset_prog_prompt

braNEW2:        bra     exec_NEW2                       ;TODO: optimize away

exec_NEW:       jsr     parse_nextstmt_yield_TXTOFF2
exec_NEW2:      jsr     NEW_int_reset_TOP_empty_prog
resetVarsImmedPrompt:
                jsr     int_CLEAR
reset_prog_prompt:
        .IFDEF COMMUNICATOR
                stz     DP_BAS_CO_FLAGS
        .ENDIF
                ldy     DP_BAS_MEMBASE
                sty     DP_BAS_TXTPTR2
                ldy     DP_BAS_MEMBASE+1
                sty     DP_BAS_TXTPTR2+1
                ldy     DP_BAS_MEMBASE+2
                sty     DP_BAS_TXTPTR2+2
                jsr     exec_ERROR_OFF_reset_ERRORPTR
        .IFDEF COMMUNICATOR
                lda     DP_BAS_ARG2_FLAG
                beq     immedPrompt
                lda     DP_stack_save+1
                xba
                lda     DP_stack_save
                tcs
                lda     DP_BAS_ARG2
                sta     DP_BAS_INT_WA
                lda     DP_BAS_ARG2+1
                sta     DP_BAS_INT_WA+1
                lda     DP_BAS_ARG2+2
                sta     DP_BAS_INT_WA+2
                jsr     PAGEset_int
                ldy     #$01
                jsr     doOLD
                brl     doRUN
        .ENDIF

immedPrompt:    
                lda     #'>'
                jsr     call_OSWRCH
                jsr     ReadKeysTo_MEMBASE
runFromTXTPTR:  lda     DP_stack_save+1
                xba
                lda     DP_stack_save
                tcs
                jsr     exec_ERROR_OFF_reset_ERRORPTR
                jsr     tokenizeAndStore
                bcs     resetVarsImmedPrompt
                jmp     execImmediateLine
doOSCLIatPTR2:  jsr     parse_updPTRfromPTR2_yield

        .IFDEF COMMUNICATOR
                phb
                lda     DP_BAS_TXTPTR2+2
                pha
                plb
                rep     #$30
                .a16
                .i16
                lda     DP_BAS_TXTPTR2
                cop     COP_5D_OPSTAR
                jsr     setREPORT_BHA_Cy
                bcc     @skok
                brk     $c8

                .asciiz "Command failed"

                .a8
                .i8
@skok:          sep     #$30
                plb
        .ENDIF ;COMMUNICATOR
        .IFDEF MOS
                ;TODO move the copy to the native handler?
                ; copy down to bank 0 and run from there
                phb
                ldy     #0
                phy
                plb
@lposc:         lda     [DP_BAS_TXTPTR2],Y
                sta     BANK0_SCRATCH_PAGE,Y
                cmp     #$0D
                beq     @skosc
                iny
                bne     @lposc
@skosc:         ldx     #<BANK0_SCRATCH_PAGE
                ldy     #>BANK0_SCRATCH_PAGE
                plb
                jsl     nat_OSCLI
        .ENDIF
parse_skip_EOL: lda     #$0d
                ldy     DP_BAS_TXTPTR2_OFF
                dey
@lp:            iny                                     ;just skip to the end of the line
                cmp     [DP_BAS_TXTPTR2],y
                bne     @lp
parse_yield_cont_or_prompt:
                jsr     parse_updPTRfromPTR2_yield
                bra     cont_or_prompt

parseEOL_yield_cont_or_prompt:
                cmp     #$0d
                bne     parse_skip_EOL
cont_or_prompt: lda     DP_BAS_TXTPTR2+2
                cmp     DP_BAS_MEMBASE+2
                bne     @checkEndOfProg
                lda     DP_BAS_TXTPTR2+1
                cmp     DP_BAS_MEMBASE+1
                bne     @checkEndOfProg
                brl     reset_prog_prompt

@checkEndOfProg:
                ldy     #$01
                lda     [DP_BAS_TXTPTR2],y
                bpl     @notEndOfProg
                brl     reset_prog_prompt

@notEndOfProg:  ldx     DP_BAS_TRACEFLAG
                beq     @notrace
                sta     DP_BAS_INT_WA+1
                iny
                lda     [DP_BAS_TXTPTR2],y
                sta     DP_BAS_INT_WA
                jsr     doTracePrintLineNoIfInRange
@notrace:       ldy     #$04
                sty     DP_BAS_TXTPTR2_OFF
                bra     skipSpacesAtYExecImmed

enterAssembler: lda     #$03
                sta     DP_BAS_OPT
                ldy     #INTVAR_P+2
                lda     [DP_BAS_INTVARSptr],y
                sta     DP_ASS_ASS_FLAGS_UK
                jmp     assScanEnter

jmpDoEXT:       jmp     doEXT

scanTryStarAssEXTEq:
                ldy     DP_BAS_TXTPTR2_OFF
                dey
                lda     [DP_BAS_TXTPTR2],y
                cmp     #'*'
                beq     doOSCLIatPTR2
                cmp     #'['
                beq     enterAssembler
                cmp     #tknEXT-64                      ;TODO tknEXT?
                beq     jmpDoEXT
                cmp     #'='
                bne     decOff2scanNextContinue
                brl     cmdEquals

decOff2scanNextContinue:
                dec     DP_BAS_TXTPTR2_OFF
scanNextContinue:
                jsr     parse_nextstmt_yield_TXTOFF2
continue:       lda     [DP_BAS_TXTPTR2]
                cmp     #':'
                bne     parseEOL_yield_cont_or_prompt
skipSpacesExecImmed:
                ldy     DP_BAS_TXTPTR2_OFF
skipSpacesAtYExecImmed:
        .IFDEF COMMUNICATOR
                phx
                phy
                phx
                lda     #$00
                xba
                lda     #$03
                ldx     #$02
                jsl     _EV
                plx
                bcc     @_skNoEV
                lda     #$01
                ldx     #$02
                jsl     _EV
                bcs     @jmp_brk_11_Escape
                jsr     ONERROROFF
                lda     #$03
                cop     COP_16_OPAEV
                bra     @_skNoEV
        .ENDIF
        .IFDEF MOS
                phx
                phy
                lda     f:MOS_ZP_ESC_FLAG
                bpl     @_skNoEV               
        .ENDIF

@jmp_brk_11_Escape:
                brl     brk_11_Escape

@_skNoEV:       ply
                plx
                inc     DP_BAS_TXTPTR2_OFF
                lda     [DP_BAS_TXTPTR2],y
                cmp     #' '
                beq     skipSpacesExecImmed
                cmp     #'.'
                bne     @notdot
                lda     DP_BAS_LAST_EOSEOR_D_
                beq     qryLabelLookup
                brl     brk_53_IllegalLabelDef

@notdot:        ldx     #$ff
                stx     DP_BAS_LAST_EOSEOR_D_
                cmp     #tknPTR
                bcc     execTryVarAssign
;*******************************************************************************
;* A contains a builtin function token, dispatch to the function based on A    *
;*******************************************************************************
evalDispatchFN: 
                asl     A
                tax
                phk
                lda     tblFnDispatch-27,x
                pha
                lda     tblFnDispatch-28,x
                pha
                php
                txa
                rti

execImmediateLine:
                jsr     parse_skip_spaces_PTR2
                cmp     #tknAUTO
                bcs     evalDispatchFN
execTryVarAssign:
                ldx     DP_BAS_TXTPTR2
                stx     DP_BAS_TXTPTR
                ldx     DP_BAS_TXTPTR2+1
                stx     DP_BAS_TXTPTR+1
                ldx     DP_BAS_TXTPTR2+2
                stx     DP_BAS_TXTPTR+2
                sty     DP_BAS_TXTPTR_OFF
                jsr     findVarAtYMinus1
                bne     assignVarAtINT_WA
                bcc     @L63D7
                brl     scanTryStarAssEXTEq

@L63D7:         stx     DP_BAS_TXTPTR_OFF
                jsr     parseEqualsOrMistake
                jsr     L6E93
                ldx     #$05
                cpx     DP_BAS_INT_WA+3
                bne     varAss_sk1
                inx
varAss_sk1:     jsr     AllocVarSpaceOnHeap
                dec     DP_BAS_TXTPTR2_OFF
exec_LET:       jsr     findVarOrAllocEmpty
                beq     jmp_brk_10_Syntax
assignVarAtINT_WA:
                bcc     cmdSetVarNumeric
                jsr     stack_INTWA
                jsr     parse_expectEQ_PTR_OFF
                lda     DP_BAS_40_VARTYPE
                bne     brk06_type_mismatch
                jsr     copyStringToVar
                brl     continue

qryLabelLookup: lda     #$ff
                sta     DP_BAS_LAST_EOSEOR_D_
                jsr     findVarOrAllocEmpty
                bcs     brk06_type_mismatch
                lda     DP_BAS_INT_WA+3
                beq     brk06_type_mismatch
                jsr     pushINTWA_on_hw_stack
                lda     DP_BAS_TXTPTR2
                sta     DP_BAS_INT_WA
                lda     DP_BAS_TXTPTR2+1
                sta     DP_BAS_INT_WA+1
                lda     DP_BAS_TXTPTR2+2
                sta     DP_BAS_INT_WA+2
                lda     #$ff
                sta     DP_BAS_INT_WA+3
                lda     #$04
                sta     DP_BAS_40_VARTYPE
                jsr     storeEvaledExprinStackedVarPTr
                ldy     DP_BAS_TXTPTR_OFF
                sty     DP_BAS_TXTPTR2_OFF
                brl     skipSpacesExecImmed

cmdEquals:      sec
                tsc
                sbc     DP_stack_save
                cmp     #$fb
                bcs     brk_07_no_FN
                phb
                lda     #$00
                pha
                plb
                lda     (DP_stack_save)
                plb
                cmp     #$a4
                bne     brk_07_no_FN
                jsr     evalExpressionMAIN
                jmp     scanNextStmt

cmdSetVarNumeric:
                lda     DB_BAS_UNK_5A_6+1
                cmp     #$40
                beq     jmp_brk_10_Syntax
                jsr     pushINTWA_on_hw_stack
                jsr     parse_expectEQ_PTR_OFF
                jsr     storeEvaledExprinStackedVarPTr
                jmp     continue

jmp_brk_10_Syntax:
                jmp     brk10_Syntax

exec_STOP:      jsr     parse_nextstmt_yield_TXTOFF2
                brk     $00

                .byte   $fa                             ;STOP

brk_07_no_FN:   brk     $07

                .byte   "No "
                .byte   $a4                             ;FN

brk06_type_mismatch:
                brk     $06

                .byte   "Type mismatch"

brk_00_NoRoom:  brk     $00

                .byte   "No room"
                .byte   $00

brk_53_IllegalLabelDef:
                brk     $53

                .asciiz "Illegal label definition"

copyStringTerm: jsr     strTerm
                cpy     #$00
                beq     @sk
@lp:            lda     [DP_BAS_STRWKSP_L],y
                sta     [DP_BAS_INT_WA],y
                dey
                bne     @lp
                lda     [DP_BAS_STRWKSP_L]
@sk:            sta     [DP_BAS_INT_WA]
                rts

copyStringToVar:
                jsr     popIntA
copyStringToVar2:
                lda     DP_BAS_INT_WA+3
                sta     DP_FP_TMP+2
                cmp     #$80
                beq     copyStringTerm
                ldy     #$03
                lda     [DP_BAS_INT_WA],y
                cmp     DP_BAS_STRLEN
                bcs     @L653A
                lda     DP_BAS_VARTOP
                sta     DP_BAS_TMP64
                lda     DP_BAS_VARTOP+1
                sta     DP_FP_TMP
                lda     DP_BAS_VARTOP+2
                sta     DP_FP_TMP+1
                lda     DP_BAS_STRLEN
                cmp     #$08
                bcc     @sk
                adc     #$07
                bcc     @sk
                lda     #$ff
@sk:            clc
                pha
                tax
                lda     [DP_BAS_INT_WA],y
                adc     [DP_BAS_INT_WA]
                eor     DP_BAS_VARTOP
                bne     @sk2
                ldy     #$01
                adc     [DP_BAS_INT_WA],y
                eor     DP_BAS_VARTOP+1
                bne     @sk2
                iny
                adc     [DP_BAS_INT_WA],y
                eor     DP_BAS_VARTOP+2
                bne     @sk2
                sta     DP_FP_TMP+2
                txa
                iny
                sec
                sbc     [DP_BAS_INT_WA],y
                tax
@sk2:           txa
                clc
                adc     DP_BAS_VARTOP
                tay
                lda     DP_BAS_VARTOP+1
                adc     #$00
                tax
                xba
                lda     DP_BAS_VARTOP+2
                adc     #$00
                pha
                xba
                cpy     DP_BAS_STACK
                sbc     DP_BAS_STACK+1
                xba
                sbc     DP_BAS_STACK+2
                bcc     @skNoRoom
                brl     brk_00_NoRoom

@skNoRoom:      sty     DP_BAS_VARTOP
                stx     DP_BAS_VARTOP+1
                pla
                sta     DP_BAS_VARTOP+2
                pla
                ldy     #$03
                sta     [DP_BAS_INT_WA],y
                dey
                lda     DP_FP_TMP+2
                beq     @L653A
                lda     DP_FP_TMP+1
                sta     [DP_BAS_INT_WA],y
                dey
                lda     DP_FP_TMP
                sta     [DP_BAS_INT_WA],y
                lda     DP_BAS_TMP64
                sta     [DP_BAS_INT_WA]
@L653A:         ldy     #$04
                lda     DP_BAS_STRLEN
                sta     [DP_BAS_INT_WA],y
                beq     rtsL655B
                ldy     #$02
                lda     [DP_BAS_INT_WA],y
                sta     DP_FP_TMP+1
                dey
                lda     [DP_BAS_INT_WA],y
                sta     DP_FP_TMP
                lda     [DP_BAS_INT_WA]
                sta     DP_BAS_TMP64
                dey
@cpylp:         lda     [DP_BAS_STRWKSP_L],y            ;TODO: TFM?
                sta     [DP_BAS_TMP64],y
                iny
                cpy     DP_BAS_STRLEN
                bne     @cpylp
rtsL655B:       rts

exec_PRINT_HASH:
                jsr     parse_decOff2_fileHandleHash
                rep     #$10
                .i16
                ldy     DP_BAS_INT_WA
                sty     DP_BAS_CURCHAN
                sep     #$10
                .i8
cmdPRINTHAS_lp: jsr     parse_skip_space_CMPcomma
                bne     cmdPRINTHASH_exit
                jsr     evalAtOFF
                jsr     fpCopyFPAtoPrintPTR
                lda     DP_BAS_40_VARTYPE
                jsr     BPUT_A_to_CURCHAN
                tax
                beq     cmdPRINTHASH_STR
                bmi     cmdPRINTHASH_FP
                ldx     #$03
@lp:            lda     DP_BAS_INT_WA,x
                jsr     BPUT_A_to_CURCHAN
                dex
                bpl     @lp
                bra     cmdPRINTHAS_lp

BPUT_A_to_CURCHAN:
        .IFDEF COMMUNICATOR
                phb
                phx
                rep     #$10
                .i16
                ldy     DP_BAS_CURCHAN
                cop     COP_0B_OPBPT
                jsr     setREPORT_BHA_Cy
                sep     #$10
                .i8
                plx
                plb
        .ENDIF
        .IFDEF MOS
                phx
                ldy     DP_BAS_CURCHAN
                jsl     nat_OSBPUT
                plx
        .ENDIF
                rts

cmdPRINTHASH_FP:
                ldx     #$04
                ldy     #$05
                phy
@lp:            ply
                dey
                lda     [DP_BAS_FPTMPptr3],y
                phy
                jsr     BPUT_A_to_CURCHAN
                dex
                bpl     @lp
                ply
                bra     cmdPRINTHAS_lp

cmdPRINTHASH_STR:
                lda     DP_BAS_STRLEN
                jsr     BPUT_A_to_CURCHAN
                tax
                beq     cmdPRINTHAS_lp
                phx
@lp:            ply
                dey
                lda     [DP_BAS_STRWKSP_L],y
                phy
                jsr     BPUT_A_to_CURCHAN
                dex
                bne     @lp
                ply
                bra     cmdPRINTHAS_lp

cmdPRINTHASH_exit:
                sty     DP_BAS_TXTPTR2_OFF
                jmp     scanNextContinue

exec_PRINT:     jsr     parse_skipSpacesPTR2_cmp_HASH
                beq     exec_PRINT_HASH
                dec     DP_BAS_TXTPTR2_OFF
                bra     cmdPRINT_skStart

cmdPRINT_padToNextField:
                lda     [DP_BAS_INTVARSptr]
                beq     cmdPRINT_skStart
                lda     DP_BAS_COUNT
cmdPRINT_padToNextField_lp:
                beq     cmdPRINT_skStart
                sbc     [DP_BAS_INTVARSptr]
                bcs     cmdPRINT_padToNextField_lp
                tay
cmdPRINT_padToNextField_lp2:
                jsr     list_print1Space
                iny
                bne     cmdPRINT_padToNextField_lp2
cmdPRINT_skStart:
                clc
                lda     [DP_BAS_INTVARSptr]
                sta     DP_BAS_TMP2A
cmdPRINT_setHexFlagFromCarry:
                ror     DP_BAS_TMP2A+1
cmdPRINT_lp1:   jsr     parse_skip_spaces_PTR2
                cmp     #':'
                beq     cmdPRINT_endcmd
                cmp     #$0d
                beq     cmdPRINT_endcmd
                cmp     #tknELSE
                bne     cmdPRINT_sk0
cmdPRINT_endcmd:
                jsr     PrintCRLFresetCOUNT
cmdPRINT_endcmd_noCR:
                jmp     decOff2scanNextContinue

cmdPRINT_lp2_checkendcmd:
                stz     DP_BAS_TMP2A
                stz     DP_BAS_TMP2A+1
                jsr     parse_skip_spaces_PTR2
                cmp     #':'
                beq     cmdPRINT_endcmd_noCR
                cmp     #$0d
                beq     cmdPRINT_endcmd_noCR
                cmp     #tknELSE
                beq     cmdPRINT_endcmd_noCR
cmdPRINT_sk0:   cmp     #'~'
                beq     cmdPRINT_setHexFlagFromCarry
                cmp     #','
                beq     cmdPRINT_padToNextField
                cmp     #';'
                beq     cmdPRINT_lp2_checkendcmd
                jsr     cmdPRINT_checkaposTABSPC
                bcc     cmdPRINT_lp1
                lda     DP_BAS_TMP2A
                pha
                lda     DP_BAS_TMP2A+1
                pha
                dec     DP_BAS_TXTPTR_OFF
                jsr     evalAtOFF
                pla
                sta     DP_BAS_TMP2A+1
                pla
                sta     DP_BAS_TMP2A
                lda     DP_BAS_TXTPTR_OFF
                sta     DP_BAS_TXTPTR2_OFF
                tya
                beq     cmdPRINT_printString
                jsr     cmdPRINT_num2str
                lda     DP_BAS_TMP2A
                sec
                sbc     DP_BAS_STRLEN
                bcc     cmdPRINT_printString
                beq     cmdPRINT_printString
                tay
cmdPRINT_padlp1:
                jsr     list_print1Space
                dey
                bne     cmdPRINT_padlp1
cmdPRINT_printString:
                lda     DP_BAS_STRLEN
                beq     cmdPRINT_lp1
                ldy     #$00
cmdPRINT_printString_lp:
                lda     [DP_BAS_STRWKSP_L],y
                jsr     list_printANoEDIT
                iny
                cpy     DP_BAS_STRLEN
                bne     cmdPRINT_printString_lp
                bra     cmdPRINT_lp1

jmp_brk05MissingComma:
                jmp     brk_05_missing_comma

cmdPRINT_TAB_comma:
                lda     DP_BAS_INT_WA
                pha
                jsr     evalL1BracketAlreadyOpenConv2INT
                lda     #VDU_CURS_CHAR
                jsr     call_OSWRCH
                pla
                jsr     call_OSWRCH
                jsr     exec_VDU_OSWRCH_WA0
                bra     cmdPRINT_clcexit

cmdPRINT_TAB:   jsr     evalAtYcheckTypeInAConvert2INT
                jsr     parse_skip_space_CMPcomma
                beq     cmdPRINT_TAB_comma
                cmp     #')'
                bne     jmp_brk05MissingComma
                lda     DP_BAS_INT_WA
                sbc     DP_BAS_COUNT
                beq     cmdPRINT_clcexit
                tax
                bcs     cmdPRINT_X_SPACESCLCRTS
                jsr     PrintCRLFresetCOUNT
                bra     cmdPRINT_SPACESatINT_WA

cmdPRINT_SPC:   jsr     evalLevel1checkTypeStoreAsINT
cmdPRINT_SPACESatINT_WA:
                ldx     DP_BAS_INT_WA
                beq     cmdPRINT_clcexit
cmdPRINT_X_SPACESCLCRTS:
                jsr     list_printXSpaces
                bra     cmdPRINT_clcexit

cmdPRINT_CR:    jsr     PrintCRLFresetCOUNT
cmdPRINT_clcexit:
                clc
                bra     parseOFF2eqYeqOFF

decOff2EvalForceInt:
                dec     DP_BAS_TXTPTR2_OFF
evalForceINT:   jsr     evalExpressionMAIN
                jsr     checkTypeConvertToINT
parseOFF2eqYeqOFF:
                ldy     DP_BAS_TXTPTR_OFF
                sty     DP_BAS_TXTPTR2_OFF
                rts

cmdPRINT_checkaposTABSPC:
                ldx     DP_BAS_TXTPTR2
                stx     DP_BAS_TXTPTR
                ldx     DP_BAS_TXTPTR2+1
                stx     DP_BAS_TXTPTR+1
                ldx     DP_BAS_TXTPTR2+2
                stx     DP_BAS_TXTPTR+2
                ldx     DP_BAS_TXTPTR2_OFF
                stx     DP_BAS_TXTPTR_OFF
                cmp     #$27
                beq     cmdPRINT_CR
                cmp     #tknTAB
                beq     cmdPRINT_TAB
                cmp     #tknSPC
                beq     cmdPRINT_SPC
secrtsL66CC:    sec
rtsL66CD:       rts

brk_09_missing_quote:
                brk     $09

                .byte   tknMissing
                .byte   '"'
                .byte   $00

cmdINPUT_PRINT_prompt:
                jsr     parse_skip_spaces_PTR2
                jsr     cmdPRINT_checkaposTABSPC
                bcc     rtsL66CD
                cmp     #'"'
                bne     secrtsL66CC
@prStrLp:       iny
                lda     [DP_BAS_TXTPTR],y
                cmp     #$0d
                beq     brk_09_missing_quote
                cmp     #'"'
                bne     @sk
                iny
                sty     DP_BAS_TXTPTR_OFF
                lda     [DP_BAS_TXTPTR],y
                cmp     #'"'
                bne     cmdPRINT_clcexit
@sk:            jsr     list_printANoEDIT
                bra     @prStrLp

mem_check_in_bounds:
                jsr     mem_check_MEMEND
                bcs     mem_check_2
brk2d_Too_High: brk     $2d

                .byte   "Too High"
                .byte   $00

brk2e_Too_Low:  brk     $2e

                .byte   "Too Low"
                .byte   $00

mem_check_2:    jsr     mem_check_MEMBOT
                bcc     brk2e_Too_Low
                rts

mem_check_MEMBOT:
                lda     DP_BAS_INT_WA
                cmp     #$00
                lda     DP_BAS_INT_WA+1
                sbc     DP_BAS_LOMEM_LIM_PAG
                lda     DP_BAS_INT_WA+2
                sbc     DP_BAS_LOMEM_LIM_PAG+1
                rts

mem_check_MEMEND:
                sec
                lda     DP_BAS_INT_WA+3
                bne     @rts
                lda     DP_BAS_MEMEND
                cmp     DP_BAS_INT_WA
                lda     DP_BAS_MEMEND+1
                sbc     DP_BAS_INT_WA+1
                lda     DP_BAS_MEMEND+2
                sbc     DP_BAS_INT_WA+2
@rts:           rts

exec_CALL_checkLI:
                jsr     parseEorANext                   ;TODO - optimize away duplicate of USR?
                beq     @rtsL6740
                dec     DP_BAS_TXTPTR2_OFF
                eor     #$00
@rtsL6740:      rts

jmp_brk1a_no_such_var1:
                jmp     brk1a_no_such_var

exec_CALL:      lda     #'L'
                jsr     exec_CALL_checkLI
                cmp     #$01
                bcc     @sk
                lda     #'I'
                jsr     exec_CALL_checkLI
@sk:            php
                jsr     evalExpressionMAIN
                jsr     checkTypeInVARTYPEConv2INT
                jsr     stack_INTWA
                lda     #$00
                sta     [DP_BAS_STRWKSP_L]
                ldy     #$00
@varloop:       phy
                jsr     parse_skip_space_CMPcomma
                bne     @skDone
                ldy     DP_BAS_TXTPTR_OFF
                jsr     findVarAtYSkipSpaces
                beq     jmp_brk1a_no_such_var1
                ply
                iny
                lda     DP_BAS_INT_WA
                sta     [DP_BAS_STRWKSP_L],y
                iny
                lda     DP_BAS_INT_WA+1
                sta     [DP_BAS_STRWKSP_L],y
                iny
                lda     DP_BAS_INT_WA+2
                sta     [DP_BAS_STRWKSP_L],y
                iny
                lda     DP_BAS_INT_WA+3
                sta     [DP_BAS_STRWKSP_L],y
                lda     [DP_BAS_STRWKSP_L]
                inc     A
                sta     [DP_BAS_STRWKSP_L]
                bra     @varloop

@skDone:        ply
                dec     DP_BAS_TXTPTR_OFF
                jsr     parse_nextstmt_yield_PTR2_OFF
                jsr     popIntA
                plp
                bcc     @noextraPHP
                bne     @noextraPHP
                lda     DP_BAS_STRWKSP_L+2
                pha
                lda     DP_BAS_STRWKSP_L+1
                pha
                lda     DP_BAS_STRWKSP_L
                pha
                phd
                phk
                pea     @ret
                php
                bra     USRCALL_enterCode

@noextraPHP:    lda     DP_BAS_STRWKSP_L+2              ;TODO: optimize - shared code, cf USR
                pha
                lda     DP_BAS_STRWKSP_L+1
                pha
                lda     DP_BAS_STRWKSP_L
                pha
                phd
                phk
                jsr     USRCALL_enterCode
@ret:           jsr     USRCALL_GetReturnedRegsToDP
                pld
                pla
                pla
                pla
                jmp     continue

; HERE HERE HERE working backwards
USRCALL_GetReturnedRegsToDP:
                php
                phb
                phd
                rep     #$30
                .a16
                .i16
                pha
                phx
                phy
                lda     $0d,S                           ;this pushed in caller code
                tcd                                     ;restore original (our) D
                phk
                plb                                     ;reset bank
                pla
                sta     USR_RET_Y
                pla
                sta     USR_RET_X
                pla
                sta     USR_RET_C
                sta     USR_RET_C2
                pla
                sta     USR_RET_D
                stz     USR_RET_B
                stz     USR_RET_B2
                stz     USR_RET_BFLAG
                stz     USR_RET_Y+2
                stz     USR_RET_X+2
                stz     USR_RET_C+2
                stz     USR_RET_B+2
                stz     USR_RET_D+2
                stz     USR_RET_B2+2
                stz     USR_RET_BFLAG+2
                stz     USR_RET_C2+2
                sep     #$30
                .a8
                .i8
                pla
                sta     USR_RET_B+2
                sta     USR_RET_C2+2
                pla
                sta     USR_RET_B2
                and     #$01
                sta     USR_RET_BFLAG
                cli
                cld
                rts

USRCALL_enterCode:
                lda     DP_BAS_INT_WA+2
                pha
                pei     (DP_BAS_INT_WA)
                lda     #$01
                trb     DP_BAS_INT_WA+3
                ldy     #$0c
                and     [DP_BAS_INTVARSptr],y
                ora     DP_BAS_INT_WA+3
                eor     #$30
                pha
                rep     #$30
                .a16
                .i16
                ldy     #INTVAR_Y
                lda     [DP_BAS_INTVARSptr],y
                pha
                ldy     #INTVAR_X
                lda     [DP_BAS_INTVARSptr],y
                tax
                ldy     #INTVAR_A
                lda     [DP_BAS_INTVARSptr],y
                pha
                lda     DP_BAS_INT_WA+3
                pha
                ldy     #INTVAR_A+1
                and     #USR_FLAGS_80_USEB
                beq     @skNotUSEB
                ldy     #INTVAR_B+1
@skNotUSEB:     lda     [DP_BAS_INTVARSptr],y
                pha
                plb
                plb
                pla
                and     #USR_02_USED
                beq     @notUSED
                ldy     #INTVAR_D
                lda     [DP_BAS_INTVARSptr],y
                tcd
@notUSED:       pla
                ply
                rti

                .a8
                .i8
jmp_brk10_Syntax:
                jmp     brk10_Syntax

exec_DELETE:    jsr     parse_lineno_atOFF
                bcc     jmp_brk10_Syntax
                jsr     stack_INTWA
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                bne     jmp_brk10_Syntax
                jsr     parse_lineno_atOFF
                bcc     jmp_brk10_Syntax
                jsr     parse_nextstmt_yield_TXTOFF2
                lda     DP_BAS_INT_WA
                sta     DP_BAS_TMP6+3
                lda     DP_BAS_INT_WA+1
                sta     DP_BAS_TMP6+4
                jsr     popIntA
@L686F:         jsr     findLineAndDelete
                jsr     parse_yield
                jsr     INT_inc_WA
                lda     DP_BAS_TMP6+3
                cmp     DP_BAS_INT_WA
                lda     DP_BAS_TMP6+4
                sbc     DP_BAS_INT_WA+1
                bcs     @L686F
                jmp     resetVarsImmedPrompt

L6885:          lda     #$0a
                jsr     retA_8bit_INT
                jsr     parse_lineno_atOFF
                jsr     stack_INTWA
                lda     #$0a
                jsr     retA_8bit_INT
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                bne     @L68A8
                jsr     parse_lineno_atOFF
                lda     DP_BAS_INT_WA+1
                bne     brk_00_Silly
                lda     DP_BAS_INT_WA
                beq     brk_00_Silly
                jmp     parse_nextstmt_yield_TXTOFF2

@L68A8:         jmp     parsecheckEOSTXTPTReqPTR2_yield

L68AB:          lda     DP_BAS_TOP
                sta     DB_BAS_UNK_5A_6+1
                lda     DP_BAS_TOP+1
                sta     DP_FPB_sgn
                lda     DP_BAS_TOP+2
                sta     DB_BAS_UNK_5A_6+3
L68B7:          clc
                lda     DP_BAS_PAGE
                adc     #$01
                sta     DP_BAS_TMP6
                lda     DP_BAS_PAGE+1
                adc     #$00
                sta     DP_BAS_TMP6+1
                lda     DP_BAS_PAGE+2
                adc     #$00
                sta     DP_BAS_TMP6+2
                ldy     #$01
                rts

; TODO: renumber decipher what's going on and get working in 6x09 bas
exec_RENUMBER:  jsr     L6885
                ldx     #DP_BAS_TMP6+3
                jsr     stack_copy6_to_X_uns4
                jsr     findTOP
                jsr     L68AB
@L68DB:         lda     [DP_BAS_TMP6]
                bmi     L6915
                sta     [DB_BAS_UNK_5A_6+1]
                lda     [DP_BAS_TMP6],y
                sta     [DB_BAS_UNK_5A_6+1],y
                sec
                tya
                adc     DB_BAS_UNK_5A_6+1
                sta     DB_BAS_UNK_5A_6+1
                bcc     @L68F3
                inc     DP_FPB_sgn
                bne     @L68F3
                inc     DB_BAS_UNK_5A_6+3
@L68F3:         cmp     DP_BAS_HIMEM
                lda     DP_FPB_sgn
                sbc     DP_BAS_HIMEM+1
                lda     DB_BAS_UNK_5A_6+3
                sbc     DP_BAS_HIMEM+2
                bcs     brk_cc_RENUMBER_space
                jsr     L69D9
                bra     @L68DB

brk_cc_RENUMBER_space:
                brk     $00

                .byte   $cc
                .byte   $20
                .byte   $73
                .byte   $70
                .byte   $61
                .byte   $63
                .byte   $65

brk_00_Silly:   brk     $00

                .byte   $53
                .byte   $69
                .byte   $6c
                .byte   $6c
                .byte   $79
                .byte   $00

L6915:          jsr     L68B7
@L6918:         lda     [DP_BAS_TMP6]
                bmi     @L6938
                lda     DP_BAS_TMP6+4
                sta     [DP_BAS_TMP6]
                lda     DP_BAS_TMP6+3
                sta     [DP_BAS_TMP6],y
                clc
                lda     DP_BAS_TMP6+3
                adc     DP_BAS_INT_WA
                sta     DP_BAS_TMP6+3
                lda     #$00
                adc     DP_BAS_TMP6+4
                and     #$7f
                sta     DP_BAS_TMP6+4
                jsr     L69D9
                bra     @L6918

@L6938:         lda     DP_BAS_PAGE+2
                sta     DP_BAS_TXTPTR2+2
                lda     DP_BAS_PAGE+1
                sta     DP_BAS_TXTPTR2+1
                lda     DP_BAS_PAGE
                sta     DP_BAS_TXTPTR2
@L6944:         ldy     #$01
                lda     [DP_BAS_TXTPTR2],y
                bmi     @L69B9
                ldy     #$04
                stz     DP_BAS_INT_WA+3
@L694E:         lda     [DP_BAS_TXTPTR2],y
                ldx     DP_BAS_INT_WA+3
                bne     @L695C
                cmp     #tknLineNo
                beq     @L6972
                cmp     #tknREM
                beq     @L6969
@L695C:         iny
                cmp     #'"'
                bne     @L6965
                eor     DP_BAS_INT_WA+3
                sta     DP_BAS_INT_WA+3
@L6965:         cmp     #$0d
                bne     @L694E
@L6969:         ldy     #$03
                lda     [DP_BAS_TXTPTR2],y
                jsr     addAToPTR2
                bra     @L6944

@L6972:         jsr     parse_lineno_atPTR2_Y
                jsr     L68AB
@L6978:         lda     [DP_BAS_TMP6]
                bmi     @L69BB
                lda     [DB_BAS_UNK_5A_6+1]
                cmp     DP_BAS_INT_WA+1
                bne     @L69A5
                lda     [DB_BAS_UNK_5A_6+1],y
                cmp     DP_BAS_INT_WA
                bne     @L69A5
                lda     [DP_BAS_TMP6],y
                sta     DP_FPB_exp
                lda     [DP_BAS_TMP6]
                tax
                ldy     DP_BAS_TXTPTR2_OFF
                dey
                lda     DP_BAS_TXTPTR2
                sta     DP_BAS_TMP6+3
                lda     DP_BAS_TXTPTR2+1
                sta     DP_BAS_TMP6+4
                lda     DP_BAS_TXTPTR2+2
                sta     DB_BAS_UNK_5A_6
                jsr     int16atFPB2toBUFasTOKENIZED
@L69A1:         ldy     DP_BAS_TXTPTR2_OFF
                bra     @L694E

@L69A5:         clc
                jsr     L69D9
                lda     DB_BAS_UNK_5A_6+1
                adc     #$02
                sta     DB_BAS_UNK_5A_6+1
                bcc     @L6978
                inc     DP_FPB_sgn
                bne     @L6978
                inc     DB_BAS_UNK_5A_6+3
                bra     @L6978

@L69B9:         bra     jmpClearResetProgrPrompt

@L69BB:         jsr     printStringAfter
                .byte   "Failed at "
                lda     [DP_BAS_TXTPTR2],y
                sta     DP_BAS_INT_WA+1
                iny
                lda     [DP_BAS_TXTPTR2],y
                sta     DP_BAS_INT_WA
                jsr     int16print_AnyLen
                jsr     PrintCRLFresetCOUNT
                bra     @L69A1

L69D9:          iny
                lda     [DP_BAS_TMP6],y
                ldy     #$01
                adc     DP_BAS_TMP6
                sta     DP_BAS_TMP6
                bcc     @L69EB
                inc     DP_BAS_TMP6+1
                bne     @L69EA
                inc     DP_BAS_TMP6+2
@L69EA:         clc
@L69EB:         rts

exec_AUTO:      jsr     L6885
                lda     DP_BAS_INT_WA
                pha
                jsr     popIntA
@lp:            jsr     stack_INTWA
                jsr     int16print_fmt5
                jsr     ReadKeysTo_MEMBASE
                jsr     popIntA
                jsr     tokNotExpectLineNo
                ldy     #$00
                jsr     tokAndStoreAlreadyLineNoDecoded
                jsr     int_CLEAR
                pla
                pha
                clc
                adc     DP_BAS_INT_WA
                sta     DP_BAS_INT_WA
                bcc     @lp
                inc     DP_BAS_INT_WA+1
                bpl     @lp
jmpClearResetProgrPrompt:
                jmp     resetVarsImmedPrompt

jmpbrk0bDIMspace:
                jmp     brk_0b_DIM_space

cmdDIM_reserve_mem:
                dec     DP_BAS_TXTPTR2_OFF
                jsr     findVarOrAllocEmpty
                beq     brk_0a_Bad_DIM
                bcs     brk_0a_Bad_DIM
                jsr     pushINTWA_on_hw_stack
                jsr     evalAtYcheckTypeInAConvert2INT
                jsr     INT_inc_WA
                lda     DP_BAS_INT_WA+3
                ora     DP_BAS_INT_WA+2
                bne     brk_0a_Bad_DIM
                clc
                lda     DP_BAS_INT_WA
                adc     DP_BAS_VARTOP
                xba
                lda     DP_BAS_INT_WA+1
                adc     DP_BAS_VARTOP+1
                tax
                ldy     DP_BAS_VARTOP+2
                bcc     @skiny
                iny
@skiny:         xba
                cmp     DP_BAS_STACK
                xba
                txa
                sbc     DP_BAS_STACK+1
                tya
                sbc     DP_BAS_STACK+2
                bcs     jmpbrk0bDIMspace
                lda     DP_BAS_VARTOP
                sta     DP_BAS_INT_WA
                lda     DP_BAS_VARTOP+1
                sta     DP_BAS_INT_WA+1
                lda     DP_BAS_VARTOP+2
                sta     DP_BAS_INT_WA+2
                xba
                sta     DP_BAS_VARTOP
                stx     DP_BAS_VARTOP+1
                sty     DP_BAS_VARTOP+2
                lda     #RETV_INT
                sta     DP_BAS_40_VARTYPE
                jsr     storeEvaledExprinStackedVarPTr
                jsr     parseOFF2eqYeqOFF
cmdDIM_more_dims_q:
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                beq     exec_DIM
                jmp     decOff2scanNextContinue

mul16fromStack: ldx     #DP_BAS_61_UK
                jsr     stack_copy6_to_X_uns4
mul16:          rep     #$30
                .a16
                .i16
                ldx     #$0000
@lp:            lsr     DP_BAS_61_UK
                bcc     @sk
                clc
                txa
                adc     DP_BAS_INT_WA
                tax
                bcs     brk_0a_Bad_DIM
@sk:            lda     DP_BAS_61_UK
                beq     @sk2
                asl     DP_BAS_INT_WA
                bcs     brk_0a_Bad_DIM
                bra     @lp

@sk2:           stx     DP_BAS_INT_WA
                sep     #$30
                .a8
                .i8
                rts

brk_0a_Bad_DIM: brk     $0a

                .byte   "Bad "
                .byte   tknDIM
                .byte   $00

exec_DIM:       jsr     parse_skip_spaces_PTR2
                tya
                clc
                adc     DP_BAS_TXTPTR2
                ldx     DP_BAS_TXTPTR2+1
                ldy     DP_BAS_TXTPTR2+2
                bcc     @sk1
                inx
                bne     @sk0
                iny
@sk0:           clc
@sk1:           sbc     #$00
                sta     DP_BAS_TMP6
                txa
                sbc     #$00
                sta     DP_BAS_TMP6+1
                tya
                sbc     #$00
                sta     DP_BAS_TMP6+2
                ldx     #$05
                stx     DP_BAS_61_UK
                ldx     DP_BAS_TXTPTR2_OFF
                jsr     varScanNameAtTMP6
                cpy     #$01
                beq     brk_0a_Bad_DIM
                cmp     #'('
                beq     cmdDIM_realArray
                cmp     #'$'
                beq     @skdollar
                cmp     #'%'
                bne     @skpct
                dec     DP_BAS_61_UK
@skdollar:      iny
                inx
                lda     [DP_BAS_TMP6],y
                cmp     #'('
                beq     cmdDIM_realArray
@skpct:         jmp     cmdDIM_reserve_mem

cmdDIM_realArray:
                iny                                     ;or string!
                stx     DP_BAS_TXTPTR2_OFF
                jsr     varFind_00
@dobrk:         bne     brk_0a_Bad_DIM
                jsr     L6E93
                ldx     #$01
                jsr     AllocVarSpaceOnHeap
                lda     DP_BAS_61_UK
                pha
                lda     #$01
                pha
                jsr     retA_8bit_INT
@dimlp:         jsr     stack_INTWA
                jsr     evalForceINT
                lda     DP_BAS_INT_WA+1
                and     #$c0
                ora     DP_BAS_INT_WA+2
                ora     DP_BAS_INT_WA+3
                bne     brk_0a_Bad_DIM
                jsr     INT_inc_WA
                ply
                lda     DP_BAS_INT_WA
                sta     [DP_BAS_VARTOP],y
                iny
                lda     DP_BAS_INT_WA+1
                sta     [DP_BAS_VARTOP],y
                iny
                phy
                jsr     mul16fromStack
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                beq     @dimlp
                cmp     #')'
                bne     @dobrk
                plx
                pla
                phx
                sta     DP_BAS_61_UK
                stz     DP_FPB_mant+3
                jsr     mul16
                pla
                pha
                adc     DP_BAS_INT_WA
                sta     DP_BAS_INT_WA
                bcc     @sknotbrk
                inc     DP_BAS_INT_WA+1
                bne     @sknotbrk
                brl     brk_0a_Bad_DIM

@sknotbrk:      lda     DP_BAS_VARTOP+2
                sta     DP_BAS_TMP6+2
                lda     DP_BAS_VARTOP+1
                sta     DP_BAS_TMP6+1
                lda     DP_BAS_VARTOP
                sta     DP_BAS_TMP6
                clc
                adc     DP_BAS_INT_WA
                xba
                lda     DP_BAS_INT_WA+1
                adc     DP_BAS_VARTOP+1
                tax
                ldy     DP_BAS_VARTOP+2
                bcc     @skiny
                iny
@skiny:         xba
                cmp     DP_BAS_STACK
                xba
                txa
                sbc     DP_BAS_STACK+1
                tya
                sbc     DP_BAS_STACK+2
                bcs     brk_0b_DIM_space
                xba
                sta     DP_BAS_VARTOP
                stx     DP_BAS_VARTOP+1
                sty     DP_BAS_VARTOP+2
                pla
                sta     [DP_BAS_TMP6]
                adc     DP_BAS_TMP6
                tay
                stz     DP_BAS_TMP6
                bcc     @ski2
                inc     DP_BAS_TMP6+1
                bne     @ski2
                inc     DP_BAS_TMP6+2
@ski2:          lda     #$00
@lp0:           sta     [DP_BAS_TMP6],y
                iny
                bne     @ski1
                inc     DP_BAS_TMP6+1
                bne     @ski1
                inc     DP_BAS_TMP6+2
@ski1:          cpy     DP_BAS_VARTOP
                bne     @lp0
                cpx     DP_BAS_TMP6+1
                bne     @lp0
                lda     DP_BAS_TMP6+2
                cmp     DP_BAS_VARTOP+2
                bne     @ski2
                jmp     cmdDIM_more_dims_q

brk_0b_DIM_space:
                brk     $0b

                .byte   tknDIM
                .byte   " space"
                .byte   $00

exec_HIMEM_set: jsr     evalAssignEqInteger
                jsr     mem_check_in_bounds
                lda     DP_BAS_INT_WA
                sta     DP_BAS_HIMEM
                sta     DP_BAS_STACK
                lda     DP_BAS_INT_WA+1
                sta     DP_BAS_HIMEM+1
                sta     DP_BAS_STACK+1
                lda     DP_BAS_INT_WA+2
                sta     DP_BAS_HIMEM+2
                sta     DP_BAS_STACK+2
                bra     jmp_parse_EOS_TXTPTR2

exec_LOMEM_set: jsr     evalAssignEqInteger
                jsr     mem_check_in_bounds
                lda     DP_BAS_INT_WA
                sta     DP_BAS_LOMEM
                sta     DP_BAS_VARTOP
                lda     DP_BAS_INT_WA+1
                sta     DP_BAS_LOMEM+1
                sta     DP_BAS_VARTOP+1
                lda     DP_BAS_INT_WA+2
                sta     DP_BAS_LOMEM+2
                sta     DP_BAS_VARTOP+2
                jsr     clearVARS
                bra     jmp_parse_EOS_TXTPTR2

exec_PAGE_set:  jsr     evalAssignEqInteger
                jsr     PAGEset_int
jmp_parse_EOS_TXTPTR2:
                jmp     continue

PAGEset_int:
        .IFDEF COMMUNICATOR
                ldy     #$00
                lda     DP_BAS_INT_WA
                sta     DP_BAS_PAGE

                sta     [DP_BAS_SPECIAL_VARSptr],y
                iny
                lda     DP_BAS_INT_WA+1
                sta     DP_BAS_PAGE+1
                sta     [DP_BAS_SPECIAL_VARSptr],y
                iny
                lda     DP_BAS_INT_WA+2
                sta     DP_BAS_PAGE+2
                sta     [DP_BAS_SPECIAL_VARSptr],y
        .ENDIF
        .IFDEF MOS
                lda     DP_BAS_INT_WA
                sta     DP_BAS_PAGE

                lda     DP_BAS_INT_WA+1
                sta     DP_BAS_PAGE+1

                lda     DP_BAS_INT_WA+2
                sta     DP_BAS_PAGE+2
        .ENDIF        

                rts

exec_CLEAR:     jsr     parse_nextstmt_yield_TXTOFF2
                jsr     int_CLEAR
                bra     jmp_parse_EOS_TXTPTR2

exec_TRACE:     jsr     parse_lineno_atOFF
                bcs     exec_TRACE_lineno
                cmp     #tknON
                beq     exec_TRACE_ON
                cmp     #tknOFF
                beq     exec_TRACE_OFF
                jsr     evalForceINT
exec_TRACE_lineno:
                jsr     parse_nextstmt_yield_TXTOFF2
                lda     DP_BAS_INT_WA
                sta     DP_BAS_TRACE_LIM
                lda     DP_BAS_INT_WA+1
exec_TRACE_go2: sta     DP_BAS_TRACE_LIM+1
                lda     #$ff
exec_TRACE_go:  sta     DP_BAS_TRACEFLAG
                bra     jmp_parse_EOS_TXTPTR2

exec_TRACE_ON:  inc     DP_BAS_TXTPTR2_OFF
                jsr     parse_nextstmt_yield_TXTOFF2
                lda     #$ff
                bne     exec_TRACE_go2

exec_TRACE_OFF: inc     DP_BAS_TXTPTR2_OFF
                jsr     parse_nextstmt_yield_TXTOFF2
                lda     #$00
                bra     exec_TRACE_go

exec_TIME_set:  iny
                lda     [DP_BAS_TXTPTR2],y
                cmp     #'$'
                beq     exec_TIMEdollar_set
                jsr     evalAssignEqInteger
                stz     DP_FPA_sgn
                ldx     #DP_BAS_INT_WA
                ldy     #$00
                lda     #$02
                jmp     OSWORD_continue

exec_TIMEdollar_set:
                inc     DP_BAS_TXTPTR2_OFF
                jsr     PTR_eq_PTR2_exeptEquals
                lda     DP_BAS_40_VARTYPE
                bne     jmp_brk06_type_mismatch16
                ldy     DP_BAS_STRLEN
                lda     #$0d
                sta     [DP_BAS_STRWKSP_L],y
                ldx     #$08
                jsr     BHAeqSTRWKSP
        .IFDEF COMMUNICATOR
                cop     COP_28_OPCMD
                .byte   "CLOCK"
                .byte   $00
                phk
                plb
        .ENDIF
        .IFDEF MOS
                TODO    "CLOCK"
        .ENDIF
                jmp     continue

evalstackStringExpectINTCloseBra:
                jsr     StackString
evalL1BracketAlreadyOpenConv2INT:
                jsr     evalL1BracketAlreadyOpen
                bra     checkTypeConvertToINT

ckCommaThnEvalAtYckTypACnv2INT:
                jsr     parse_skip_spaces_CMPcommaBRK
evalAtYcheckTypeInAConvert2INT:
                jsr     evalAtOFF
                bra     checkTypeConvertToINT

evalLevel1checkTypeStoreAsINT:
                jsr     evalLevel1
                bra     checkTypeConvertToINT

evalAssignEqInteger:
                jsr     PTR_eq_PTR2_exeptEquals
checkTypeInVARTYPEConv2INT:
                lda     DP_BAS_40_VARTYPE
checkTypeInAConvertToINT:
                tay
checkTypeConvertToINT:
                beq     jmp_brk06_type_mismatch16
IfMIConvertToINT:
                bpl     rtsL6CC0
eval_real2INT:  php
                phy
                phx
                phb
                jsr     stack_REAL
                jsr     BHAeqDP_BAS_STACKptr
                ldx     #ARITH_FN_INT
                phk
                jsr     moduleCallARITHref
                jsr     DP_BAS_STACKptreqBHA
                plb
                jsr     popIntA
                lda     #RETV_INT
                plx
                ply
                plp
                rts

fpCopyAmant2intWA:
                lda     DP_FPA_mant
                sta     DP_BAS_INT_WA+3
                lda     DP_FPA_mant+1
                sta     DP_BAS_INT_WA+2
                lda     DP_FPA_mant+2
                sta     DP_BAS_INT_WA+1
                lda     DP_FPA_mant+3
                sta     DP_BAS_INT_WA
rtsL6CC0:       rts

jmp_brk06_type_mismatch16:
                jmp     brk06_type_mismatch

evalLevel1ConvertReal:
                jsr     evalLevel1
ckTypeIntToReal:
                beq     jmp_brk06_type_mismatch16
                bmi     rtsL6CC0
                jsr     braIntToReal
                rts

exec_PROC:      lda     DP_BAS_TXTPTR2
                sta     DP_BAS_TXTPTR
                lda     DP_BAS_TXTPTR2+1
                sta     DP_BAS_TXTPTR+1
                lda     DP_BAS_TXTPTR2+2
                sta     DP_BAS_TXTPTR+2
                lda     DP_BAS_TXTPTR2_OFF
                sta     DP_BAS_TXTPTR_OFF
                lda     #tknPROC                        ;;PROC
                jsr     exec_FN_PROC
                jsr     parse_nextstmt_yield_PTR2_OFF
                jmp     continue

st0_at_PTR_WA_plus4:
                ldy     #$04
                lda     #$00
                sta     [DP_BAS_INT_WA],y
                beq     L6D17

exec_LOCAL:     tsc
                sec
                sbc     DP_stack_save
                xba
                sbc     DP_stack_save+1
                bcs     brk_0c_Not_LOCAL
                xba
                cmp     #$fd
                bcs     brk_0c_Not_LOCAL
                jsr     findVarOrAllocEmpty
                beq     L6D2C
                jsr     localVarAtIntA
                ldy     DP_BAS_INT_WA+3
                bmi     st0_at_PTR_WA_plus4
                jsr     pushINTWA_on_hw_stack
                jsr     exec_FALSE
                sta     DP_BAS_40_VARTYPE
                jsr     storeEvaledExprinStackedVarPTr
L6D17:          tsx
                phd
                tsc
                lda     #$00
                tcd
                inc     $07,x
                pld
                ldy     DP_BAS_TXTPTR_OFF
                sty     DP_BAS_TXTPTR2_OFF
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                beq     exec_LOCAL
                jmp     decOff2scanNextContinue

L6D2C:          jmp     scanNextContinue

brk_0c_Not_LOCAL:
                brk     $0c

                .byte   "Not "
                .byte   tknLOCAL

brk19_bad_MODE: brk     $19

                .byte   $42
                .byte   $61
                .byte   $64
                .byte   $20
                .byte   tknMODE
                .byte   $00

exec_GCOL:      jsr     evalForceINT
                lda     DP_BAS_INT_WA
                pha
                jsr     ckCommaThnEvalAtYckTypACnv2INT
                jsr     parse_nextstmt_yield_PTR2_OFF
                lda     #VDU_GCOL
                jsr     call_OSWRCH                     ;gcol escape code
                pla
                bra     OSWRCH_A_then_WA0               ;jump forward to OSWRCH A, WA+0

exec_COLOUR:    jsr     evalForceINT
                jsr     parse_nextstmt_yield_TXTOFF2
                lda     #VDU_COLOUR                     ;escape code for COLOUR
                bra     OSWRCH_A_then_WA0

exec_MODE:      jsr     evalForceINT
                jsr     parse_nextstmt_yield_TXTOFF2
                jsr     L9B1D
                inx
                bne     @skipHIMEMcheck
                iny
                bne     @skipHIMEMcheck
                lda     DP_BAS_STACK
                cmp     DP_BAS_HIMEM
                bne     brk19_bad_MODE
                lda     DP_BAS_STACK+1
                cmp     DP_BAS_HIMEM+1
                bne     brk19_bad_MODE
                lda     DP_BAS_STACK+2
                cmp     DP_BAS_HIMEM+2
                bne     brk19_bad_MODE
@skipHIMEMcheck:
                stz     DP_BAS_COUNT
                lda     #VDU_MODE
OSWRCH_A_then_WA0:
                jsr     call_OSWRCH
                lda     DP_BAS_INT_WA
                bra     call_OSWRCH_EOS

exec_MOVE:      lda     #$04
                bra     doMOVEDRAW

exec_DRAW:      lda     #$05
doMOVEDRAW:     pha
                jsr     evalExpressionMAIN
                jsr     checkTypeInVARTYPEConv2INT
                bra     skPLOTrest

exec_PLOT:      jsr     evalForceINT
                lda     DP_BAS_INT_WA
                pha
                jsr     ckCommaThnEvalAtYckTypACnv2INT
skPLOTrest:     jsr     stack_INTWA
                jsr     ckCommaThnEvalAtYckTypACnv2INT
                jsr     parse_nextstmt_yield_PTR2_OFF
                lda     #VDU_PLOT
                jsr     call_OSWRCH                     ;OSWRCH plot escape code
                pla
                jsr     call_OSWRCH                     ;OSWRCH plot reason
                jsr     stack_copy6_to_DP_BAS_TMP66_uns4
                lda     DP_BAS_TMP6
                jsr     call_OSWRCH
                lda     DP_BAS_TMP6+1
                jsr     call_OSWRCH
                jsr     exec_VDU_OSWRCH_WA0
                lda     DP_BAS_INT_WA+1
                bra     call_OSWRCH_EOS

exec_CLG:       jsr     parse_nextstmt_yield_TXTOFF2
                lda     #VDU_CLG
                bra     call_OSWRCH_EOS

exec_CLS:       jsr     parse_nextstmt_yield_TXTOFF2
                stz     DP_BAS_COUNT
                lda     #VDU_CLS
call_OSWRCH_EOS:
                jsr     call_OSWRCH
                jmp     continue

exec_REPORT:    jsr     parse_nextstmt_yield_TXTOFF2
                jsr     PrintCRLFresetCOUNT
                jsr     doREPORT
                jmp     continue

doREPORT:
        .IFDEF COMMUNICATOR
                phx
                ldx     #_STBMB
                jsl     _ST
                cop     COP_2C_OPFMA
                bcc     @L6DFC
                ldy     #$00
                tyx
                cop     COP_2E_OPFPO
                bcs     @L6E14

                cop     COP_2C_OPFMA
                bcs     @L6E14
@L6DFC:         xba
                pha
                xba
                pha
                ldy     #$00
@L6E02:         lda     ($01,S),y
                beq     @sksl
                cmp     #'/'
                beq     @sksl
                jsr     list_printA
                iny
                bne     @L6E02
@sksl:          pla
                pla
                bra     @L6E19

@L6E14:         lda     #'?'
                jsr     list_printA
@L6E19:         lda     #':'
                jsr     list_printA
                jsr     list_print1Space
                phk
                plb
                ldx     #_STSBK                         ;Get BRK number
                jsl     _ST
@lp:            ldx     #_STTBK                         ;Get BRK text
                jsl     _ST
                bcs     @skDone
                jsr     doListPrintTokenA
                bra     @lp

@skDone:        plx
                rts
        .ENDIF
        .IFDEF MOS
                ldy     #0
@lp:            lda     [DP_BAS_BL_ERRPTR],y
                beq     @sk
                jsr     doListPrintTokenA
                iny
                bra     @lp
@sk:            rts
        .ENDIF

exec_VDU_hibyte:
                lda     DP_BAS_INT_WA+1
                jsr     call_OSWRCH
exec_VDU:       jsr     parse_skip_spaces_PTR2
@vdulp:         cmp     #':'
                beq     L6E6F
                cmp     #$0d
                beq     L6E6F
                cmp     #tknELSE                        ;ELSE?
                beq     L6E6F
                dec     DP_BAS_TXTPTR2_OFF
                jsr     evalForceINT
                jsr     exec_VDU_OSWRCH_WA0
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                beq     exec_VDU                        ;had a comma, go again
                cmp     #';'
                beq     exec_VDU_hibyte                 ;had a semi, output hi byte
                cmp     #'|'
                beq     exec_VDU_8_zeros                ;had a pipe output 8 zeroes
                bra     @vdulp

exec_VDU_8_zeros:
                lda     #$00
                ldy     #$09
@lp:            jsr     call_OSWRCH
                dey
                bne     @lp
                bra     exec_VDU

L6E6F:          jmp     decOff2scanNextContinue

exec_VDU_OSWRCH_WA0:
                lda     DP_BAS_INT_WA
                jmp     call_OSWRCH

                phk                                     ;TODO: DEAD code?
                xba
                lda     $020f
                pha
                lda     $020e
                pha
                xba
                php
                rti

L6E84:          ldy     #$01
                lda     [DP_BAS_TMP6],y
                tax
                lda     #VAR_OFFS_PROC
                cpx     #tknPROC
                beq     varAllocInt
                lda     #VAR_OFFS_FN
                bra     varAllocInt

L6E93:          ldy     #$01
                lda     [DP_BAS_TMP6],y
                cmp     #'@'
                bne     allocVAR
                brl     brk10_Syntax

allocVAR:       sec
                sbc     #'@'
                pha
                asl     A
                clc                                     ;TODO: is this needed?
                adc     $01,S                           ;multiply var offs by 3
                sta     $01,S
                pla
varAllocInt:    clc
                adc     DP_BAS_VARS_BASE
                ldx     DP_BAS_VARS_BASE+1
                ldy     DP_BAS_VARS_BASE+2
                bcc     @allocVARint_lp1
                inx
                bne     @allocVARint_lp1
                iny
@allocVARint_lp1:
                sta     DP_BAS_TMP6+4                   ;look for empty entry in variable table or end of linked list
                stx     DP_BAS_TMP6+5
                sty     DP_BAS_TMP6+6
                ldy     #$01
                lda     [DP_BAS_TMP6+4],y
                iny
                ora     [DP_BAS_TMP6+4],y
                ora     [DP_BAS_TMP6+4]
                beq     @allocVARint_sk1
                dey
                lda     [DP_BAS_TMP6+4],y
                tax
                iny
                lda     [DP_BAS_TMP6+4],y
                tay
                lda     [DP_BAS_TMP6+4]
                bra     @allocVARint_lp1

@allocVARint_sk1:
                lda     DP_BAS_VARTOP+2
                sta     [DP_BAS_TMP6+4],y
                dey
                lda     DP_BAS_VARTOP+1
                sta     [DP_BAS_TMP6+4],y
                lda     DP_BAS_VARTOP
                sta     [DP_BAS_TMP6+4]
                lda     #$00
                sta     [DP_BAS_VARTOP]
                sta     [DP_BAS_VARTOP],y
                iny
                sta     [DP_BAS_VARTOP],y
                iny
                cpy     DP_BAS_TMP6+3
                beq     rtsL6F2B
@lp:            dey
                lda     [DP_BAS_TMP6],y
                iny
                sta     [DP_BAS_VARTOP],y
                iny
                cpy     DP_BAS_TMP6+3
                bne     @lp
                rts

AllocVarSpaceOnHeap:
                lda     #$00
@lp:            sta     [DP_BAS_VARTOP],y
                iny
                dex
                bne     @lp
CheckVarFitsY:  clc
                tya
                adc     DP_BAS_VARTOP
                bcc     @sk
                inc     DP_BAS_VARTOP+1
                bne     @sk
                inc     DP_BAS_VARTOP+2
@sk:            ldy     DP_BAS_VARTOP+2
                cpy     DP_BAS_STACK+2
                bcc     @sk3
                bne     @sk2
                ldy     DP_BAS_VARTOP+1
                cpy     DP_BAS_STACK+1
                bcc     @sk3
                cmp     DP_BAS_STACK
                bcc     @sk3
@sk2:           lda     #$00
                ldy     #$01
                sta     [DP_BAS_TMP6+4],y
                jmp     brk_00_NoRoom

@sk3:           sta     DP_BAS_VARTOP
rtsL6F2B:       rts

fvAllocX:       jsr     AllocVarSpaceOnHeap
findVarOrAllocEmpty:
                jsr     findVarAtPTR2
                bne     rtsL6F51
                bcs     rtsL6F51
                jsr     L6E93
                ldx     #$05
                cpx     DP_BAS_INT_WA+3
                bne     fvAllocX
                inx
                bra     fvAllocX

findVARTreatAsMemoryAccess:
                cmp     #'!'
                beq     memacc32
                cmp     #'$'
                beq     memaccStr
                eor     #'?'
                beq     memacc8
                lda     #$00
                sec
rtsL6F51:       rts

memacc32:       lda     #$04
memacc8:        pha                                     ;A=0 for byte return type
                inc     DP_BAS_TXTPTR_OFF
                jsr     evalLevel1checkTypeStoreAsINT
                jmp     retIndVal

memaccStr:      inc     DP_BAS_TXTPTR_OFF
                jsr     evalLevel1checkTypeStoreAsINT
                lda     DP_BAS_INT_WA+1
                ora     DP_BAS_INT_WA+2
                beq     brk_08_dollar_range
                lda     #$80
                sta     DP_BAS_INT_WA+3
                sec
                rts

brk_08_dollar_range:
                brk     $08

                .byte   "$ range"
                .byte   $00

findVarAtPTR2:  lda     DP_BAS_TXTPTR2
                sta     DP_BAS_TXTPTR
                lda     DP_BAS_TXTPTR2+1
                sta     DP_BAS_TXTPTR+1
                lda     DP_BAS_TXTPTR2+2
                sta     DP_BAS_TXTPTR+2
                ldy     DP_BAS_TXTPTR2_OFF
                dey
findVarLp1:     iny
findVarAtYSkipSpaces:
                sty     DP_BAS_TXTPTR_OFF
                lda     [DP_BAS_TXTPTR],y
                cmp     #' '
                beq     findVarLp1
findVarAtYMinus1:
                sta     DB_BAS_UNK_5A_6+1
                cmp     #'@'
                bcc     findVARTreatAsMemoryAccess
                cmp     #'['
                bcs     skfindVarDynamic
                asl     A
                asl     A
                sta     DP_BAS_INT_WA
                iny
                lda     [DP_BAS_TXTPTR],y
                cmp     #'%'
                bne     skfindVarDynamic
                stz     DB_BAS_UNK_5A_6+1
                lda     DP_BAS_INTVARSptr+1
                sta     DP_BAS_INT_WA+1
                lda     DP_BAS_INTVARSptr+2
                sta     DP_BAS_INT_WA+2
                ldx     #$04
                stx     DP_BAS_INT_WA+3
                iny
                lda     [DP_BAS_TXTPTR],y
                cmp     #'('
                bne     findVarCheckForIndirectAfter
skfindVarDynamic:
                ldx     #$05
                stx     DP_BAS_INT_WA+3
                clc
                ldy     DP_BAS_TXTPTR+1
                ldx     DP_BAS_TXTPTR+2
                lda     DP_BAS_TXTPTR_OFF
                bne     @skof0
                dec     A
                adc     DP_BAS_TXTPTR
                bcs     @skxy
                dey
                cpy     #$ff
                bne     @skxy
                dex
                bra     @skxy

@skof0:         dec     A
                adc     DP_BAS_TXTPTR
                bcc     @skxy
                iny
                bne     @skxy
                inx
@skxy:          sta     DP_BAS_TMP6
                sty     DP_BAS_TMP6+1
                stx     DP_BAS_TMP6+2
                ldy     #$00
                ldx     DP_BAS_TXTPTR_OFF
                dex
findVarDyn_lp:  inx
                iny
                lda     [DP_BAS_TMP6],y
                cmp     #'@'
                bcs     findVarDyn_sk2
                cmp     #'0'
                bcc     findVarDyn_skEnd
                cmp     #':'
                bcc     findVarDyn_lp
                bra     findVarDyn_skEnd

findVarDyn_skArray:
                jsr     findVarDyn_Subscripts
                bra     findVarDyn_skGotArrayTryInd

findVarDyn_sk2: cmp     #'['
                bcc     findVarDyn_lp
                cmp     #'_'
                bcc     findVarDyn_skEnd
                cmp     #'{'
                bcc     findVarDyn_lp
findVarDyn_skEnd:
                cpy     #$01
                beq     findVarDyn_skInvalid
                cmp     #'$'
                beq     findVarDyn_gotDollar
                cmp     #'%'
                bne     findVarDyn_skNotInt
                dec     DP_BAS_INT_WA+3
                inx
                iny
                lda     [DP_BAS_TMP6],y
findVarDyn_skNotInt:
                cmp     #'('
                beq     findVarDyn_skArray
                jsr     varFind_00
                beq     findVarDyn_skNotFound
                stx     DP_BAS_TXTPTR_OFF
findVarDyn_skGotArrayTryInd:
                ldy     DP_BAS_TXTPTR_OFF
                lda     [DP_BAS_TXTPTR],y
findVarCheckForIndirectAfter:
                cmp     #'!'
                beq     findVarIndWord
                eor     #'?'
                beq     findVarIndByte
                clc
                sty     DP_BAS_TXTPTR_OFF
                lda     #RETV_REAL
                rts

findVarDyn_skInvalid:
                lda     #$00
                sec
                rts

findVarDyn_skNotFound:
                lda     #$00
                clc
                rts

findVarIndWord: lda     #$04
findVarIndByte: pha
                iny
                sty     DP_BAS_TXTPTR_OFF
                jsr     GetVarVal
                jsr     checkTypeConvertToINT
                ldx     #$04
@lp:            lda     DP_BAS_47_ASS_UK,x
                pha
                dex
                bne     @lp
                jsr     evalLevel1checkTypeStoreAsINT
                ldx     #$00
                ldy     #$04
                clc
@lp2:           pla
                adc     DP_BAS_INT_WA,x
                sta     DP_BAS_INT_WA,x
                inx
                dey
                bne     @lp2
retIndVal:      jsr     ConvertAddressIfDP
                pla
                sta     DP_BAS_INT_WA+3
                stz     DB_BAS_UNK_5A_6+1
                clc
                lda     #$ff
                rts                                     ;return NE/CC for var found

ConvertAddressIfDP:
                jsr     AeqWA1OR2OR3                    ;if any of the MSbytes in WA are set then add to DP
                bne     rtsL708A
                phd
                clc
                pla
                adc     DP_BAS_INT_WA
                sta     DP_BAS_INT_WA
                pla
                adc     DP_BAS_INT_WA+1
                sta     DP_BAS_INT_WA+1
                lda     #$00
                adc     DP_BAS_INT_WA+2
                sta     DP_BAS_INT_WA+2
rtsL708A:       rts

findVarDyn_gotDollar:
                inx
                iny
                lda     [DP_BAS_TMP6],y
                cmp     #'('
                beq     findVarDyn_skArrayStr
                jsr     varFind_00
                beq     findVarDyn_skNotFound
                stx     DP_BAS_TXTPTR_OFF
findVarDyn_retDynStr:
                lda     #$81
                sta     DP_BAS_INT_WA+3
                sec
                rts

findVarDyn_skArrayStr:
                jsr     findVarDyn_Subscripts
                bra     findVarDyn_retDynStr

brk_0e_Array:   brk     $0e

                .byte   $41
                .byte   $72
                .byte   $72
                .byte   $61
                .byte   $79
                .byte   $00

; TODO: This is quite different 6x09 work out what it is doing from BAS432 or
; optimize to be like 6x09?
findVarDyn_Subscripts:
                inx
                iny
                jsr     varFind_00
                beq     brk_0e_Array
                stx     DP_BAS_TXTPTR_OFF
                lda     DP_BAS_INT_WA+3
                pha
                lda     DP_BAS_INT_WA
                pha
                lda     DP_BAS_INT_WA+1
                pha
                lda     DP_BAS_INT_WA+2
                pha
                lda     [DP_BAS_INT_WA]
                cmp     #$04
                bcs     @L70CB
                brl     @L714D

@L70CB:         jsr     exec_FALSE
                lda     #$01
                sta     DP_BAS_INT_WA+3
@L70D2:         jsr     stack_INTWA
                jsr     evalAtYcheckTypeInAConvert2INT
                inc     DP_BAS_TXTPTR_OFF
                cpx     #','
                bne     brk_0e_Array
                ldx     #DB_BAS_UNK_5A_6-2
                jsr     stack_copy6_to_X_uns4
                ldy     DB_BAS_UNK_5A_6+1
                pla
                sta     DP_BAS_TMP6+2
                pla
                sta     DP_BAS_TMP6+1
                pla
                sta     DP_BAS_TMP6
                pha
                lda     DP_BAS_TMP6+1
                pha
                lda     DP_BAS_TMP6+2
                pha
                jsr     findVarDyn_SubsCheck
                sty     DP_BAS_INT_WA+3
                lda     [DP_BAS_TMP6],y
                sta     DP_BAS_61_UK
                iny
                lda     [DP_BAS_TMP6],y
                sta     DP_FPB_mant+3
                lda     DP_BAS_INT_WA
                adc     DP_BAS_TMP6+3
                sta     DP_BAS_INT_WA
                lda     DP_BAS_INT_WA+1
                adc     DP_BAS_TMP6+4
                sta     DP_BAS_INT_WA+1
                bcc     @L7113
                inc     DP_BAS_INT_WA+2
@L7113:         jsr     mul16
                sec
                lda     [DP_BAS_TMP6]
                sbc     DP_BAS_INT_WA+3
                cmp     #$03
                bcs     @L70D2
                jsr     stack_INTWA
                jsr     evalL1BracketAlreadyOpenConv2INT
                pla
                sta     DP_BAS_TMP6+2
                pla
                sta     DP_BAS_TMP6+1
                pla
                sta     DP_BAS_TMP6
                ldx     #DB_BAS_UNK_5A_6-2
                jsr     stack_copy6_to_X_uns4
                ldy     DB_BAS_UNK_5A_6+1
                jsr     findVarDyn_SubsCheck
                clc
                lda     DP_BAS_TMP6+3
                adc     DP_BAS_INT_WA
                sta     DP_BAS_INT_WA
                lda     DP_BAS_TMP6+4
                adc     DP_BAS_INT_WA+1
                sta     DP_BAS_INT_WA+1
                lda     #$00
                adc     DP_BAS_INT_WA+2
                sta     DP_BAS_INT_WA+2
                bcc     @L7161
@L714D:         jsr     evalL1BracketAlreadyOpen
                jsr     checkTypeConvertToINT
                pla
                sta     DP_BAS_TMP6+2
                pla
                sta     DP_BAS_TMP6+1
                pla
                sta     DP_BAS_TMP6
                ldy     #$01
                jsr     findVarDyn_SubsCheck
@L7161:         pla
                sta     DP_BAS_INT_WA+3
                cmp     #$05
                bne     TMP6pluseqWAmul4plusY
                phy
                ldy     DP_BAS_INT_WA+2
                ldx     DP_BAS_INT_WA+1
                lda     DP_BAS_INT_WA
                asl     DP_BAS_INT_WA
                rol     DP_BAS_INT_WA+1
                rol     DP_BAS_INT_WA+2
                asl     DP_BAS_INT_WA
                rol     DP_BAS_INT_WA+1
                rol     DP_BAS_INT_WA+2
                adc     DP_BAS_INT_WA
                sta     DP_BAS_INT_WA
                txa
                adc     DP_BAS_INT_WA+1
                sta     DP_BAS_INT_WA+1
                tya
                adc     DP_BAS_INT_WA+2
                sta     DP_BAS_INT_WA+2
                ply
                bra     TMP6pluseqWApluseqY

TMP6pluseqWAmul4plusY:
                asl     DP_BAS_INT_WA                   ;WA*=4
                rol     DP_BAS_INT_WA+1
                rol     DP_BAS_INT_WA+2
                asl     DP_BAS_INT_WA
                rol     DP_BAS_INT_WA+1
                rol     DP_BAS_INT_WA+2
TMP6pluseqWApluseqY:
                tya                                     ;WA+=Y
                adc     DP_BAS_INT_WA
                sta     DP_BAS_INT_WA
                bcc     @sk1
                inc     DP_BAS_INT_WA+1
                bne     @sk0
                inc     DP_BAS_INT_WA+2
@sk0:           clc
@sk1:           lda     DP_BAS_TMP6
                adc     DP_BAS_INT_WA
                sta     DP_BAS_INT_WA
                lda     DP_BAS_TMP6+1
                adc     DP_BAS_INT_WA+1
                sta     DP_BAS_INT_WA+1
                lda     DP_BAS_TMP6+2
                adc     DP_BAS_INT_WA+2
                sta     DP_BAS_INT_WA+2
                rts

findVarDyn_SubsCheck:
                lda     DP_BAS_INT_WA+1
                and     #$c0
                ora     DP_BAS_INT_WA+2
                ora     DP_BAS_INT_WA+3
                bne     brk_0F_Subscript
                lda     DP_BAS_INT_WA
                cmp     [DP_BAS_TMP6],y
                iny
                lda     DP_BAS_INT_WA+1
                sbc     [DP_BAS_TMP6],y
                bcs     brk_0F_Subscript
                iny
                rts

brk_0F_Subscript:
                brk     $0f

                .asciiz "Subscript"

;*******************************************************************************
;* scan the string pointed at by TMP6,Y (starting at index 1, TMP6 points      *
;* before the string!) for variable name chars                                 *
;*******************************************************************************
;* on exit Y contains length+1 i.e. 1 means nothing found                      *
;*******************************************************************************
varScanNameAtTMP6:
                ldy     #$01
varScanNameAtTMP6_Y:
                lda     [DP_BAS_TMP6],y
                cmp     #'0'
                bcc     @rts                            ;<'0' exit
                cmp     #'@'
                bcs     @sk1                            ;>='@'
                cmp     #':'
                bcs     @rts                            ;between ':' and '?' exit
                cpy     #$01
                beq     @rts                            ;if we got here and it's the first character exit
@again:         inx
                iny
                bne     varScanNameAtTMP6_Y
@sk1:           cmp     #'_'
                bcs     @cklc                           ;>='_'
                cmp     #'['
                bcc     @again                          ;<'[' upper case chars continue
@rts:           rts

@cklc:          cmp     #'{'
                bcc     @again                          ;<';' lower case continue
                rts

;*******************************************************************************
;* Scan for a valid line number at DP_BAS_TXTPTR2                              *
;*******************************************************************************
parse_lineno_atOFFplus1:
                inc     DP_BAS_TXTPTR2_OFF
parse_lineno_atOFF:
                ldy     DP_BAS_TXTPTR2_OFF
                lda     [DP_BAS_TXTPTR2],y
                cmp     #' '
                beq     parse_lineno_atOFFplus1
                cmp     #tknLineNo
                bne     clcrts
parse_lineno_atPTR2_Y:
                iny
                lda     [DP_BAS_TXTPTR2],y
                asl     A
                asl     A
                tax
                and     #$c0
                iny
                eor     [DP_BAS_TXTPTR2],y
                sta     DP_BAS_INT_WA
                txa
                asl     A
                asl     A
                iny
                eor     [DP_BAS_TXTPTR2],y
                sta     DP_BAS_INT_WA+1
                iny
                sty     DP_BAS_TXTPTR2_OFF
                sec
                stz     DP_BAS_INT_WA+2
                stz     DP_BAS_INT_WA+3
                rts

clcrts:         clc
                rts

PTR_eq_PTR2_exeptEquals:
                lda     DP_BAS_TXTPTR2
                sta     DP_BAS_TXTPTR
                lda     DP_BAS_TXTPTR2+1
                sta     DP_BAS_TXTPTR+1
                lda     DP_BAS_TXTPTR2+2
                sta     DP_BAS_TXTPTR+2
                lda     DP_BAS_TXTPTR2_OFF
                sta     DP_BAS_TXTPTR_OFF
parse_expectEQ_PTR_OFF:
                ldy     DP_BAS_TXTPTR_OFF
                inc     DP_BAS_TXTPTR_OFF
                lda     [DP_BAS_TXTPTR],y
                cmp     #' '
                beq     parse_expectEQ_PTR_OFF
                cmp     #'='
                bne     brk_04_Mistake
                brl     L727F

brk_04_Mistake: brk     $04

                .byte   "Mistake"

brk10_Syntax:   brk     $10

                .byte   "Syntax error"

brk_0d_NoPROC:  brk     $0d

                .byte   "No "
                .byte   tknPROC

brk_11_Escape:  brk     $11

                .asciiz "Escape"

parseEqualsOrMistake:
                jsr     parse_skip_spaces
                cmp     #'='
                bne     brk_04_Mistake
                rts

L727F:          jsr     evalAtOFF
scanNextStmt:   txa
                ldy     DP_BAS_TXTPTR_OFF
; Check that we're on an end of statement and update TXTPTR from TXTPTR2/Y/OFF2
parse_checkEOS_updTXTPTRfrom2:
                cmp     #':'
                beq     parse_updTXTPTR_from_TXTPTR2_Y
                cmp     #tknELSE
                beq     parse_updTXTPTR_from_TXTPTR2_Y
                cmp     #$0d
                bne     brk10_Syntax
parse_updTXTPTR_from_TXTPTR2_Y:
                clc
                tya
                adc     DP_BAS_TXTPTR2
                sta     DP_BAS_TXTPTR2
                bcc     parse_setEOSflag_Yeq1
                inc     DP_BAS_TXTPTR2+1
                bne     parse_setEOSflag_Yeq1
                inc     DP_BAS_TXTPTR2+2
parse_setEOSflag_Yeq1:
                lda     [DP_BAS_TXTPTR2]
                eor     #$0d
                sta     DP_BAS_LAST_EOSEOR_D_
                stz     DP_BAS_SET_TO_Z_AT_EOS
                ldy     #$01
                sty     DP_BAS_TXTPTR2_OFF
                rts

parse_updPTRfromPTR2_yield:
                jsr     parse_updTXTPTR_from_TXTPTR2_Y
                bra     parse_yield

checkForESC:    
                jsr     parse_setEOSflag_Yeq1
                bra     parse_yield


parse_nextstmt_yield_PTR2_OFF:
                ldy     DP_BAS_TXTPTR_OFF
                bra     parse_nextstmt_yield_TXTPTR2_Y

exec_ENDPROC:   tsc
                sec
                sbc     DP_stack_save
                tax
                cpx     #$fb
                bcs     brk_0d_NoPROC
                lda     #$00
                phb
                pha
                plb
                lda     (DP_stack_save)
                plb
                cmp     #tknPROC
                bne     brk_0d_NoPROC
parse_nextstmt_yield_TXTOFF2:
                ldy     DP_BAS_TXTPTR2_OFF
parse_nextstmt_yield_TXTPTR2_Y:
                dey
@spcLp:         iny
                lda     [DP_BAS_TXTPTR2],y
                cmp     #' '
                beq     @spcLp
parsecheckEOSTXTPTReqPTR2_yield:
                jsr     parse_checkEOS_updTXTPTRfrom2
parse_yield:    
        .IFDEF COMMUNICATOR
                lda     #$40
                bit     DP_BAS_CO_FLAGS
                bne     @skNoPrempt
                dec     DP_BAS_CO_CTDN
                bne     @skNoPrempt
                cop     COP_21_OPPRE                    ;YIELD
                lda     #$64
                sta     DP_BAS_CO_CTDN
@skNoPrempt:    phx
                lda     #$03
                ldx     #$02
                jsl     _EV
                bcc     @ev3sk
                lda     #$01
                jsl     _EV
                bcc     @ev1sk
                brl     brk_11_Escape

@ev1sk:         jsr     ONERROROFF
                lda     #$03
                cop     COP_16_OPAEV
@ev3sk:         plx
                rts
        .ENDIF
        .IFDEF MOS
                lda     f:MOS_ZP_ESC_FLAG
                bpl     @sknoesc
                brl     brk_11_Escape
@sknoesc:       rts
        .ENDIF

ackESCThenDoSomethingWithatHELP:
                lda     #OSBYTE_126_ESCAPE_ACK
                jsr     call_OSBYTE
        .IFDEF MOS
                sec
                rts
        .ENDIF
        .IFDEF COMMUNICATOR
                bit     DP_BAS_CO_FLAGS
                bmi     @skV
                brl     secRTS

@skV:           bvc     doPROCatHELP
                brl     secRTS

doPROCatHELP:   lda     #<secRTS+1
                sta     DP_BAS_TMP6
                lda     #>secRTS
                sta     DP_BAS_TMP6+1
                phk
                pla
                sta     DP_BAS_TMP6+2
                ldy     #$07
                jsr     varFindPROCFN
                bne     @exePROCatHELP
                jsr     PROCFN_FindDEF
                sec
                beq     rtsL7359
@exePROCatHELP: lda     #$40
                tsb     DP_BAS_CO_FLAGS
                lda     #<basPROCatHELP+1
                sta     DP_BAS_TXTPTR2
                sta     DP_BAS_TXTPTR
                lda     #>basPROCatHELP
                sta     DP_BAS_TXTPTR2+1
                sta     DP_BAS_TXTPTR+1
                phk
                pla
                sta     DP_BAS_TXTPTR2+2
                sta     DP_BAS_TXTPTR+2
                stz     DP_BAS_TXTPTR2_OFF
                stz     DP_BAS_TXTPTR_OFF
                lda     #tknPROC
                jsr     exec_FN_PROC
                lda     #$40
                trb     DP_BAS_CO_FLAGS
                clc
rtsL7359:       rts

secRTS:         sec
                rts

basPROCatHELP:  .byte   tknPROC                         
                .byte   "@HELP",$0d

HELP:           lda     #$10
                bit     DP_BAS_CO_FLAGS
                bpl     @secRTS
                bne     @secRTS
                lda     #<secRTS+79
                sta     DP_BAS_TMP6
                lda     #>secRTS
                sta     DP_BAS_TMP6+1
                phk
                pla
                sta     DP_BAS_TMP6+2
                ldy     #$07
                jsr     varFindPROCFN
                bne     @sk
                jsr     PROCFN_FindDEF
                beq     @secRTS
@sk:            lda     #$10
                tsb     DP_BAS_CO_FLAGS
                lda     #<basPROCatKILL+1
                sta     DP_BAS_TXTPTR2
                sta     DP_BAS_TXTPTR
                lda     #>basPROCatKILL
                sta     DP_BAS_TXTPTR2+1
                sta     DP_BAS_TXTPTR+1
                phk
                pla
                sta     DP_BAS_TXTPTR2+2
                sta     DP_BAS_TXTPTR+2
                stz     DP_BAS_TXTPTR2_OFF
                stz     DP_BAS_TXTPTR_OFF
                lda     #tknPROC
                jsr     exec_FN_PROC
                lda     #$10
                trb     DP_BAS_CO_FLAGS
                clc
                rts

@secRTS:        sec
                rts

basPROCatKILL:  .byte   tknPROC
                .byte   "@KILL",$0d
        .ENDIF ; COMMUNICATOR

scanNextStmtAndTrace:
                jsr     parse_nextstmt_yield_TXTOFF2
                lda     [DP_BAS_TXTPTR2]
                cmp     #':'
                beq     anRTS
                lda     DP_BAS_TXTPTR2+1
                cmp     DP_BAS_MEMBASE+1
                bne     CheckEndOfProgContinueThisLn
                lda     DP_BAS_TXTPTR2+2
                cmp     DP_BAS_MEMBASE+2
                beq     jmpRestProgPrompt
CheckEndOfProgContinueThisLn:
                ldy     #$01
                lda     [DP_BAS_TXTPTR2],y
                bmi     jmpRestProgPrompt
                ldx     DP_BAS_TRACEFLAG
                beq     add3ToPtr2
                sta     DP_BAS_INT_WA+1
                iny
                lda     [DP_BAS_TXTPTR2],y
                sta     DP_BAS_INT_WA
                jsr     doTracePrintLineNoIfInRange
add3ToPtr2:     lda     #$03
addAToPTR2:     clc
                adc     DP_BAS_TXTPTR2
                sta     DP_BAS_TXTPTR2
                bcc     @skinc
                inc     DP_BAS_TXTPTR2+1
                bne     @skinc
                inc     DP_BAS_TXTPTR2+2
@skinc:         ldy     #$01
                sty     DP_BAS_TXTPTR2_OFF
anRTS:          rts

jmpRestProgPrompt:
                jmp     reset_prog_prompt

jmp_brk06_type_mismatch15:
                jmp     brk06_type_mismatch

exec_IF:        jsr     evalExpressionMAIN
                beq     jmp_brk06_type_mismatch15
                bpl     @skpl
                jsr     eval_real2INT
@skpl:          ldy     DP_BAS_TXTPTR_OFF
                sty     DP_BAS_TXTPTR2_OFF
                lda     DP_BAS_INT_WA
                ora     DP_BAS_INT_WA+1
                ora     DP_BAS_INT_WA+2
                ora     DP_BAS_INT_WA+3
                beq     IFdoELSE
                stz     DP_BAS_INT_WA+3
                cpx     #tknTHEN
                beq     THENskip                        ;check if last token parsed was THEN
IFexecthis:     jmp     skipSpacesExecImmed

THENskip:       inc     DP_BAS_TXTPTR2_OFF
L7417:          jsr     parse_lineno_atOFF
                bcc     IFexecthis
                jsr     prog_search_lineno_brknotfnd
                jsr     parse_setEOSflag_Yeq1
                jsr     parse_yield
                jmp     cmdGOTODecodedLineNumber

IFdoELSE:       ldy     DP_BAS_TXTPTR2_OFF
@lpEL:          lda     [DP_BAS_TXTPTR2],y
                cmp     #$0d
                beq     @jmpcont                        ;end of line continue
                iny
                cmp     #tknELSE                        ;got ELSE?
                bne     @lpEL
                sty     DP_BAS_TXTPTR2_OFF
                beq     L7417

@jmpcont:       jmp     parse_yield_cont_or_prompt

doTracePrintLineNoIfInRange:
                lda     DP_BAS_INT_WA
                cmp     DP_BAS_TRACE_LIM
                lda     DP_BAS_INT_WA+1
                sbc     DP_BAS_TRACE_LIM+1
                bcs     anRTS
                lda     #'['
                jsr     list_printANoEDIT
                jsr     int16print_AnyLen
                lda     #']'
                jsr     list_printANoEDIT
                jmp     list_print1Space

evalDoCmpePopIntMStkConvoRealCmp:
                pla
                sta     DP_BAS_INT_WA
                pla
                sta     DP_BAS_INT_WA+1
                pla
                sta     DP_BAS_INT_WA+2
                pla
                sta     DP_BAS_INT_WA+3
                jsr     stack_REAL
                jsr     braIntToReal
                jsr     fpCopyFPAtoFPB
                jsr     popFPFromStackToPTR1
                jsr     fpCopyPTR1toFPA
                bra     evalDoCompareRealFPAwithFPB

evalDoCompareReal:
                jsr     stack_REAL
                jsr     evalLevel4
                tay
                jsr     ckTypeIntToReal
                jsr     popFPFromStackToPTR1
evalDoCompareRealFPAwithPTR1:
                jsr     fpMoveRealAtPTR1toFPB
evalDoCompareRealFPAwithFPB:
        .IF !.defined(OPTIMIZE)
                bra     @unnecessary                    ;TODO - optimize away
@unnecessary:   
        .ENDIF
                ldy     #$00
                lda     #$7f
                trb     DP_FPB_sgn
                lda     DP_FPA_sgn
                and     #$80
                cmp     DP_FPB_sgn
                bne     @rts
                lda     DP_FPB_exp
                cmp     DP_FPA_exp+1
                bne     @roreorrol
                lda     DP_FPB_mant
                cmp     DP_FPA_mant
                bne     @roreorrol
                lda     DP_FPB_mant+1
                cmp     DP_FPA_mant+1
                bne     @roreorrol
                lda     DP_BAS_61_UK
                cmp     DP_FPA_mant+2
                bne     @roreorrol
                lda     DP_FPB_mant+3
                cmp     DP_FPA_mant+3
                bne     @roreorrol
@rts:           rts

@roreorrol:     ror     A
                eor     DP_FPB_sgn
                rol     A
                lda     #$01
                rts

jmp_brk06_type_mismatch14:
                jmp     brk06_type_mismatch

evalDoCompare:  txa
evalDoCompare2: beq     evalDoCompareString
                bmi     evalDoCompareReal
                lda     DP_BAS_INT_WA+3
                pha
                lda     DP_BAS_INT_WA+2
                pha
                lda     DP_BAS_INT_WA+1
                pha
                lda     DP_BAS_INT_WA
                pha
                jsr     evalLevel4
                tay
                beq     jmp_brk06_type_mismatch14
                bpl     @sk
                brl     evalDoCmpePopIntMStkConvoRealCmp

@sk:            lda     DP_BAS_INT_WA+3
                eor     #$80
                sta     DP_BAS_INT_WA+3
                sec
                pla
                sbc     DP_BAS_INT_WA
                sta     DP_BAS_INT_WA
                pla
                sbc     DP_BAS_INT_WA+1
                tsb     DP_BAS_INT_WA
                pla
                sbc     DP_BAS_INT_WA+2
                tsb     DP_BAS_INT_WA
                pla
                ldy     #$00
                eor     #$80
                sbc     DP_BAS_INT_WA+3
                ora     DP_BAS_INT_WA
                rts

evalDoCompareString:
                jsr     StackString
                jsr     evalLevel4
                tay
                bne     jmp_brk06_type_mismatch14
                lda     [DP_BAS_STACK]
                cmp     DP_BAS_STRLEN
                bcc     @sk1
                lda     DP_BAS_STRLEN
@sk1:           sta     DP_BAS_TMP6
                dey
@eqlp:          iny
                cpy     DP_BAS_TMP6
                beq     @skCmpLen
                iny
                lda     [DP_BAS_STACK],y
                dey
                cmp     [DP_BAS_STRWKSP_L],y
                beq     @eqlp
                bra     @skDeallocRts

@skCmpLen:      lda     [DP_BAS_STACK]
                cmp     DP_BAS_STRLEN
@skDeallocRts:  php
                jsr     deAllocAFromStack
                ldy     #$00
                plp
                rts

evalExpressionMAIN:
                lda     DP_BAS_TXTPTR2
                sta     DP_BAS_TXTPTR
                lda     DP_BAS_TXTPTR2+1
                sta     DP_BAS_TXTPTR+1
                lda     DP_BAS_TXTPTR2+2
                sta     DP_BAS_TXTPTR+2
                lda     DP_BAS_TXTPTR2_OFF
                sta     DP_BAS_TXTPTR_OFF
evalAtOFF:      jsr     evalLevel6
evalLevel7lp0:  cpx     #tknOR
                beq     evalLevel7OR
                cpx     #tknEOR
                beq     evalLevel7EOR
                dec     DP_BAS_TXTPTR_OFF
                tay
                sta     DP_BAS_40_VARTYPE
                rts

evalLevel7OR:   jsr     INTevalLevel6
                jsr     checkTypeInAConvertToINT
                ldy     #$03
                phx
@lp:            lda     [DP_BAS_STACK],y
                tyx
                ora     DP_BAS_INT_WA,x
                sta     DP_BAS_INT_WA,x
                dey
                bpl     @lp
                plx
unstack4_retInt:
                jsr     stack_ADD4
                lda     #RETV_INT
                bra     evalLevel7lp0

evalLevel7EOR:  jsr     INTevalLevel6
                jsr     checkTypeInAConvertToINT
                ldy     #$03
                phx
@lp:            lda     [DP_BAS_STACK],y
                tyx
                eor     DP_BAS_INT_WA,x
                sta     DP_BAS_INT_WA,x
                dey
                bpl     @lp
                plx
                bra     unstack4_retInt

INTevalLevel6:  jsr     checkTypeInAConvertToINT
                jsr     stack_INTWA
evalLevel6:     jsr     evalLevel5
evalLevel6lp0:  cpx     #tknAND
                beq     evalDoAND
                rts

evalDoAND:      jsr     checkTypeInAConvertToINT
                jsr     stack_INTWA
                jsr     evalLevel5
                jsr     checkTypeInAConvertToINT
                ldy     #$03
                phx
@lp:            lda     [DP_BAS_STACK],y
                tyx
                and     DP_BAS_INT_WA,x
                sta     DP_BAS_INT_WA,x
                dey
                bpl     @lp
                plx
                jsr     stack_ADD4
                lda     #RETV_INT
                bra     evalLevel6lp0

evalLevel5:     jsr     evalLevel4
                cpx     #'?'
                bcs     @rts
                cpx     #'<'
                bcs     evalComparison
@rts:           rts

evalComparison: beq     evalCompLt1
                cpx     #'>'
                beq     evalCompGt1
                tax
                jsr     evalDoCompare2
                bne     evalCompRetFALSE
evalCompRetTRUE:
                dey
evalCompRetFALSE:
                sty     DP_BAS_INT_WA
                sty     DP_BAS_INT_WA+1
                sty     DP_BAS_INT_WA+2
                sty     DP_BAS_INT_WA+3
                lda     #RETV_INT
                rts

evalCompLt1:    tax
                ldy     DP_BAS_TXTPTR_OFF
                lda     [DP_BAS_TXTPTR],y
                cmp     #'='
                beq     evalCompLtEq1
                cmp     #'>'
                beq     evalCompNE
                jsr     evalDoCompare
                bcc     evalCompRetTRUE
                bra     evalCompRetFALSE

evalCompLtEq1:  inc     DP_BAS_TXTPTR_OFF
                jsr     evalDoCompare
                beq     evalCompRetTRUE
                bcc     evalCompRetTRUE
                bra     evalCompRetFALSE

evalCompNE:     inc     DP_BAS_TXTPTR_OFF
                jsr     evalDoCompare
                bne     evalCompRetTRUE
                bra     evalCompRetFALSE

evalCompGt1:    tax
                ldy     DP_BAS_TXTPTR_OFF
                lda     [DP_BAS_TXTPTR],y
                cmp     #'='
                beq     evalCompGE1
                jsr     evalDoCompare
                beq     evalCompRetFALSE
                bcs     evalCompRetTRUE
                bra     evalCompRetFALSE

evalCompGE1:    inc     DP_BAS_TXTPTR_OFF
                jsr     evalDoCompare
                bcs     evalCompRetTRUE
                bra     evalCompRetFALSE

brk_13_StringTooLong:
                brk     $13

                .asciiz "String too long"

evalL4StringPlus:
                jsr     StackString
                jsr     evalLevel2
                tay
                beq     @sk
                brl     jmp_brk06_type_mismatch13

@sk:            clc
                phx
                lda     [DP_BAS_STACK]
                adc     DP_BAS_STRLEN
                bcs     brk_13_StringTooLong
                tax
                pha
                ldy     DP_BAS_STRLEN
@lp:            dex
                dey
                lda     [DP_BAS_STRWKSP_L],y
                jsr     storeStringX
                bne     @lp
                jsr     popStackedString
                pla
                sta     DP_BAS_STRLEN
                plx
                lda     #RETV_STR
                bra     _skEvalLevel4NoLevel3

evalLevel4:     jsr     fpStackWAtoStackReal
_skEvalLevel4NoLevel3:
                cpx     #'+'
                beq     evalL4Plus
                cpx     #'-'
                bne     @skRTS
                brl     evalLevel4Minus

@skRTS:         rts

evalL4Plus:     tay
                beq     evalL4StringPlus
                bmi     evalL4RealPlus
                jsr     stackIntThenEvalL3
                tay
                beq     jmp_brk06_type_mismatch13
                bmi     evalL4IntPlusReal
                bra     @skUnnecessary

        .IF !.defined(OPTIMIZE)
                bra     _skEvalLevel4NoLevel3           ;TODO: DEAD?
        .ENDIF

@skUnnecessary: clc
; evalL4IntegerAdd
                lda     [DP_BAS_STACK]
                adc     DP_BAS_INT_WA
                sta     DP_BAS_INT_WA
                ldy     #$01
                lda     [DP_BAS_STACK],y
                adc     DP_BAS_INT_WA+1
                sta     DP_BAS_INT_WA+1
                iny
                lda     [DP_BAS_STACK],y
                adc     DP_BAS_INT_WA+2
                sta     DP_BAS_INT_WA+2
                iny
                lda     [DP_BAS_STACK],y
                adc     DP_BAS_INT_WA+3
evalL4PopIntStackReturnInt:
                sta     DP_BAS_INT_WA+3
                clc
                lda     DP_BAS_STACK
                adc     #$04
                sta     DP_BAS_STACK
                lda     #RETV_INT
                bcc     _skEvalLevel4NoLevel3
                inc     DP_BAS_STACK+1
                bne     _skEvalLevel4NoLevel3
                inc     DP_BAS_STACK+2
                bra     _skEvalLevel4NoLevel3

jmp_brk06_type_mismatch13:
                jmp     brk06_type_mismatch

evalL4RealPlus: jsr     stack_REAL
                jsr     fpStackWAtoStackReal
                tay
                beq     jmp_brk06_type_mismatch13
                stx     DP_BAS_40_VARTYPE
                bmi     sk
                jsr     braIntToReal
sk:             jsr     popFPFromStackToPTR1
                jsr     fpFPAeqPTR1addFPA
evalTokenFromVarTypeReturnReal:
                ldx     DP_BAS_40_VARTYPE
                lda     #RETV_REAL
                brl     _skEvalLevel4NoLevel3

evalL4IntPlusReal:
                stx     DP_BAS_40_VARTYPE
                jsr     popIntA
                jsr     stack_REAL
                jsr     braIntToReal
                bra     sk

evalLevel4Minus:
                tay
                beq     jmp_brk06_type_mismatch13
                bmi     evalL4RealMinus
                jsr     stackIntThenEvalL3
                tay
                beq     jmp_brk06_type_mismatch13
                bmi     evalL4IntMinusReal
        .IF !.defined(OPTIMIZE)
                bra     @skUnnecessary

                php                                     ;TODO: DEAD code
@skUnnecessary: 
        .ENDIF
                sec
                lda     [DP_BAS_STACK]
                sbc     DP_BAS_INT_WA
                sta     DP_BAS_INT_WA
                ldy     #$01
                lda     [DP_BAS_STACK],y
                sbc     DP_BAS_INT_WA+1
                sta     DP_BAS_INT_WA+1
                iny
                lda     [DP_BAS_STACK],y
                sbc     DP_BAS_INT_WA+2
                sta     DP_BAS_INT_WA+2
                iny
                lda     [DP_BAS_STACK],y
                sbc     DP_BAS_INT_WA+3
                brl     evalL4PopIntStackReturnInt

evalL4RealMinus:
                jsr     stack_REAL
                jsr     fpStackWAtoStackReal
                tay
                bne     @sk
                brl     jmp_brk06_type_mismatch13

@sk:            stx     DP_BAS_40_VARTYPE
                bmi     @skReal
                jsr     braIntToReal
@skReal:        jsr     popFPFromStackToPTR1
                jsr     fpFPAeqPTR1subFPA
                bra     evalTokenFromVarTypeReturnReal

evalL4IntMinusReal:
                stx     DP_BAS_40_VARTYPE
                jsr     popIntA
                jsr     stack_REAL
                jsr     braIntToReal
                jsr     popFPFromStackToPTR1
                jsr     fpFPAeqPTR1subFPAnegFPA
                brl     evalTokenFromVarTypeReturnReal

eval3Mul_IntAsRealEvalAndMul:
                jsr     braIntToReal
eval3Mul_StackRealEvalAndMul:
                jsr     popIntA
                jsr     stack_REAL
                jsr     braIntToReal
                bra     _skev3mulrr

forceIntToRealAndMul1:
                jsr     braIntToReal
evalLevel3MulReal:
                jsr     stack_REAL
                jsr     evalLevel2
                tay
                jsr     ckTypeIntToReal
_skev3mulrr:    jsr     popFPFromStackToPTR1
                jsr     fpFPAeqPTR1mulFPA
                lda     #RETV_REAL
                jmp     _skevalLevel3noLevel2

        .IF !.defined(OPTIMIZE)
                php                                     ;TODO: dead code?
                phy
                phx
                phb
                jsr     stack_REAL
                jsr     BHAeqDP_BAS_STACKptr
                ldx     #ARITH_FN_MUL
                phk
                jsr     moduleCallARITHref
                jsr     DP_BAS_STACKptreqBHA
                plb
                jsr     popFPFromStackToPTR1
                jsr     fpCopyPTR1toFPA
                plx
                ply
                plp
                lda     #RETV_REAL
                jmp     _skevalLevel3noLevel2
        .ENDIF

jmp_brk06_type_mismatch12:
                jmp     brk06_type_mismatch

EvalL3Mul:      tay
                beq     jmp_brk06_type_mismatch12
                bmi     evalLevel3MulReal
                ldy     DP_BAS_INT_WA+3
                cpy     DP_BAS_INT_WA+2
                bne     forceIntToRealAndMul1
                lda     DP_BAS_INT_WA+1
                asl     A
                adc     #$00
                tya
                bne     forceIntToRealAndMul1
                jsr     stackINTEvalLevel2
                tay
                beq     jmp_brk06_type_mismatch12
                bmi     eval3Mul_StackRealEvalAndMul
                ldy     DP_BAS_INT_WA+3
                cpy     DP_BAS_INT_WA+2
                bne     eval3Mul_IntAsRealEvalAndMul
                lda     DP_BAS_INT_WA+1
                asl     A
                tya
                adc     #$00
                bne     eval3Mul_IntAsRealEvalAndMul
                phy
                jsr     L83A4
                stx     DP_BAS_40_VARTYPE
        .IF !.defined(OPTIMIZE)
                bra     @skUnnecessary                  ;TODO: DEAD code

@skUnnecessary: 
        .ENDIF
                ldx     #DP_BAS_TMP6+3
                jsr     storeWAatDPX
                jsr     popIntA
                pla
                eor     DP_BAS_INT_WA+3
                sta     DP_BAS_TMP6
                jsr     L83A4
                ldy     #$00
                ldx     #$00
                stz     DP_FPB_mant+1
                stz     DP_BAS_61_UK
@lp:            lsr     DP_BAS_TMP6+4
                ror     DP_BAS_TMP6+3
                bcc     @sk
                clc
                tya
                adc     DP_BAS_INT_WA
                tay
                txa
                adc     DP_BAS_INT_WA+1
                tax
                lda     DP_FPB_mant+1
                adc     DP_BAS_INT_WA+2
                sta     DP_FPB_mant+1
                lda     DP_BAS_61_UK
                adc     DP_BAS_INT_WA+3
                sta     DP_BAS_61_UK
@sk:            asl     DP_BAS_INT_WA
                rol     DP_BAS_INT_WA+1
                rol     DP_BAS_INT_WA+2
                rol     DP_BAS_INT_WA+3
                lda     DP_BAS_TMP6+3
                ora     DP_BAS_TMP6+4
                bne     @lp
                sty     DP_FPB_exp
                stx     DP_FPB_mant
                lda     DP_BAS_TMP6
                php
divmodNegateWAifMI:
                ldx     #DP_FPB_exp
divmodNegateWAifZP_GEN_PTR:
                jsr     copyDPXtoWA
                plp
                bpl     divModFinish
                jsr     negateWA
divModFinish:   ldx     DP_BAS_40_VARTYPE
                brl     _skevalLevel3noLevel2

jmpEvalL3Mul:   jmp     EvalL3Mul

stackIntThenEvalL3:
                jsr     stack_INTWA
fpStackWAtoStackReal:
                jsr     evalLevel2
_skevalLevel3noLevel2:
                cpx     #'*'
                beq     jmpEvalL3Mul
                cpx     #'/'
                beq     evalL3DoRealDiv
                cpx     #tknMOD
                beq     evalL3DoMOD
                cpx     #tknDIV
                beq     evalL3DoDIV
                rts

evalL3DoRealDiv:
                tay
                jsr     ckTypeIntToReal
                jsr     stack_REAL
                jsr     evalLevel2
                stx     DP_BAS_40_VARTYPE
                tay
                jsr     ckTypeIntToReal
                jsr     popFPFromStackToPTR1
                jsr     fpFPAeqPTR1divFPA
                lda     #RETV_REAL
                brl     divModFinish

evalL3DoMOD:    jsr     fpRealDivide
                lda     DP_BAS_TMP6+1
                php
                bra     divmodNegateWAifMI

evalL3DoDIV:    jsr     fpRealDivide
                rol     DB_BAS_UNK_5A_6
                rol     DB_BAS_UNK_5A_6+1
                rol     DP_FPB_sgn
                rol     DB_BAS_UNK_5A_6+3
                bit     DP_BAS_TMP6
                php
                ldx     #DB_BAS_UNK_5A_6
                bra     divmodNegateWAifZP_GEN_PTR

stackINTEvalLevel2:
                jsr     stack_INTWA
evalLevel2:     jsr     evalLevel1
evalLevel2Again:
                pha
@spclp:         ldy     DP_BAS_TXTPTR_OFF
                inc     DP_BAS_TXTPTR_OFF
                lda     [DP_BAS_TXTPTR],y
                cmp     #' '
                beq     @spclp
                tax
                pla
                cpx     #'^'
                beq     evalDoCARET
                rts

evalDoCARET:    tay
                jsr     ckTypeIntToReal
                jsr     stack_REAL
                jsr     evalLevel1ConvertReal
                php
                phy
                phx
                phb
                jsr     stack_REAL
                jsr     BHAeqDP_BAS_STACKptr
                ldx     #ARITH_FN_POWER
                phk
                jsr     moduleCallARITHref
                jsr     DP_BAS_STACKptreqBHA
                plb
                jsr     popFPFromStackToPTR1
                jsr     fpCopyPTR1toFPA
                plx
                ply
                plp
                lda     #RETV_REAL
                bra     evalLevel2Again

int16print_AnyLen:
                lda     #$00
                bra     int16print_fmtA

int16print_fmt5:
                lda     #$05
int16print_fmtA:
                sta     DP_BAS_TMP2A
                ldx     #$04
@lp0:           stz     DP_BAS_61_UK,x
                sec
@lp1:           lda     DP_BAS_INT_WA
                sbc     tblDigitsLo,x
                tay
                lda     DP_BAS_INT_WA+1
                sbc     tblDigitsHi,x
                bcc     @sk0
                sta     DP_BAS_INT_WA+1
                sty     DP_BAS_INT_WA
                inc     DP_BAS_61_UK,x
                bra     @lp1

@sk0:           dex
                bpl     @lp0
                ldx     #$05
@lp2:           dex
                beq     @sk2
                lda     DP_BAS_61_UK,x
                beq     @lp2
@sk2:           stx     DP_BAS_TMP6
                lda     DP_BAS_TMP2A
                beq     @lpPrDigs
                sbc     DP_BAS_TMP6
                beq     @lpPrDigs
                tax
                jsr     list_printXSpaces
                ldx     DP_BAS_TMP6
@lpPrDigs:      lda     DP_BAS_61_UK,x
                ora     #'0'
                jsr     list_printA
                dex
                bpl     @lpPrDigs
                rts

BHAeqDP_BAS_STACKptr:
                lda     DP_BAS_STACK+2
                pha
                lda     DP_BAS_STACK+1
                xba
                lda     DP_BAS_STACK
                plb
                rts

DP_BAS_STACKptreqBHA:
                phb
                sta     DP_BAS_STACK
                xba
                sta     DP_BAS_STACK+1
                pla
                sta     DP_BAS_STACK+2
                rts

cmdPRINT_num2str_hex:
                tya
                bpl     @skRealC
                jsr     eval_real2INT
@skRealC:       ldx     #$00
                ldy     #$00
@lp1:           phx
                tyx
                lda     DP_BAS_INT_WA,x
                plx
                pha
                and     #$0f
                sta     DB_BAS_UNK_5A_6+3,x
                pla
                lsr     A
                lsr     A
                lsr     A
                lsr     A
                inx
                sta     DB_BAS_UNK_5A_6+3,x
                inx
                iny
                cpy     #$04
                bne     @lp1
@lpsk0:         dex
                beq     @lpsk0end
                lda     DB_BAS_UNK_5A_6+3,x
                beq     @lpsk0
@lpsk0end:      lda     DB_BAS_UNK_5A_6+3,x
                cmp     #$0a
                bcc     @skhexdig
                adc     #$06
@skhexdig:      adc     #'0'
                jsr     storeAatEndOfString
                dex
                bpl     @lpsk0end
                rts

cmdPRINT_num2str_realNon0:
                bpl     cmdPRINT_num2str_realNon0_lp1
                lda     #'-'
                stz     DP_FPA_sgn
                jsr     storeAatEndOfString
cmdPRINT_num2str_realNon0_lp1:
                lda     DP_FPA_exp+1
                cmp     #$81
                bcs     cmdPRINT_num2str_digit
                jsr     fpMulBy10
                dec     DP_FP_TMP+5
                bra     cmdPRINT_num2str_realNon0_lp1

cmdPRINT_num2str:
                phy
                ldy     #INTVAR_atsign+2
                lda     [DP_BAS_INTVARSptr],y
                tax
                dey
                lda     [DP_BAS_INTVARSptr],y
                xba
                ply
                cpx     #$03
                bcc     @cmdPRINT_num2str_skFMTOK
                ldx     #$00
@cmdPRINT_num2str_skFMTOK:
                stx     DP_BAS_TMP6
                xba
                beq     @cmdPRINT_num2str_0dp
                cmp     #$0a
                bcs     cmdPRINT_num2str_invaldp
                bra     cmdPRINT_num2str_dp_sk

@cmdPRINT_num2str_0dp:
                cpx     #$02
                beq     cmdPRINT_num2str_dp_sk
cmdPRINT_num2str_invaldp:
                lda     #$0a
cmdPRINT_num2str_dp_sk:
                sta     DP_BAS_TMP6+1
                sta     DP_BAS_70_UK
                stz     DP_BAS_STRLEN
                stz     DP_FP_TMP+5
                bit     DP_BAS_TMP2A+1
                bmi     cmdPRINT_num2str_hex
                tya
                bmi     @cmdPRINT_num2str_dec_sk1
                jsr     braIntToReal
@cmdPRINT_num2str_dec_sk1:
                jsr     fpFPACkMant0SetSgnExp0
                bne     cmdPRINT_num2str_realNon0
                lda     DP_BAS_TMP6
                bne     @braPRINT_num2str_fmtFixedOrExp0
                lda     #'0'
                jmp     storeAatEndOfString

@braPRINT_num2str_fmtFixedOrExp0:
                jmp     cmdPRINT_num2str_fmtFixedOrExp0

setFlA1_cmdPRINT_num2str_digit:
                jsr     fpLoad1
                bra     cmdPRINT_num2str_digit_sk2

cmdPRINT_num2str_digit:
                cmp     #$84
                bcc     cmdPRINT_num2str_digit_1_9
                bne     @cmdPRINT_num2str_digit_sk1
                lda     DP_FPA_mant
                cmp     #$a0
                bcc     cmdPRINT_num2str_digit_1_9
@cmdPRINT_num2str_digit_sk1:
                jsr     fpFloatADiv10
cmdPRINT_num2str_digit_sk2:
                inc     DP_FP_TMP+5
                bra     cmdPRINT_num2str_realNon0_lp1

cmdPRINT_num2str_digit_1_9:
                lda     DP_FPA_mant+4
                sta     DP_BAS_40_VARTYPE
                jsr     fpCopyFPAtoPrintPTR
                lda     DP_BAS_70_UK
                sta     DP_BAS_TMP6+1
                ldx     DP_BAS_TMP6
                cpx     #$02
                bne     @cmdPRINT_num2str_digit_1_9_sknf
                adc     DP_FP_TMP+5
                bmi     cmdPR_num2str_clrZeroFPAandPRF2
                sta     DP_BAS_TMP6+1
                cmp     #$0b
                bcc     @cmdPRINT_num2str_digit_1_9_sknf
                lda     #$0a
                sta     DP_BAS_TMP6+1
                stz     DP_BAS_TMP6
@cmdPRINT_num2str_digit_1_9_sknf:
                jsr     zero_FPA_sign_expO_manlo
                lda     #$a0
                sta     DP_FPA_mant
                lda     #$83
                sta     DP_FPA_exp+1
                ldx     DP_BAS_TMP6+1
                beq     @skEQ1
@divlp:         jsr     fpFloatADiv10
                dex
                bne     @divlp
@skEQ1:         jsr     PrintPTRtoPTR1
                jsr     fpMoveRealAtPTR1toFPB
                lda     DP_BAS_40_VARTYPE
                sta     DP_FPB_mant+4
                jsr     L5307
@rorlp:         lda     DP_FPA_exp+1
                cmp     #$84
                bcs     @skExp84
                ror     DP_FPA_mant
                ror     DP_FPA_mant+1
                ror     DP_FPA_mant+2
                ror     DP_FPA_mant+3
                ror     DP_FPA_mant+4
                inc     DP_FPA_exp+1
                bne     @rorlp
@skExp84:       lda     DP_FPA_mant
                cmp     #$a0
                bcs     setFlA1_cmdPRINT_num2str_digit
                lda     DP_BAS_TMP6+1
                bne     skNE
cmdPRINT_num2str_fmtFixedOrExp0:
                cmp     #$01
                beq     cmdPRINT_numstr_stADigLenPDigs
cmdPR_num2str_clrZeroFPAandPRF2:
                jsr     zero_FPA
                stz     DP_FP_TMP+5
                lda     DP_BAS_70_UK
                inc     A
                sta     DP_BAS_TMP6+1
skNE:           lda     #$01
                cmp     DP_BAS_TMP6
                beq     cmdPRINT_numstr_stADigLenPDigs
                ldy     DP_FP_TMP+5
                bmi     @cmdPRINT_numstr_printLead0point
                cpy     DP_BAS_TMP6+1
                bcs     cmdPRINT_numstr_stADigLenPDigs
                stz     DP_FP_TMP+5
                iny
                tya
                bne     cmdPRINT_numstr_stADigLenPDigs
@cmdPRINT_numstr_printLead0point:
                lda     DP_BAS_TMP6
                cmp     #$02
                beq     @cmdPRINT_numstr_print0point
                lda     #$01
                cpy     #$ff
                bne     cmdPRINT_numstr_stADigLenPDigs
@cmdPRINT_numstr_print0point:
                lda     #'0'
                jsr     storeAatEndOfString
                lda     #'.'
                jsr     storeAatEndOfString
                lda     #'0'
@cmdPRINT_numstr_print0point_lp:
                inc     DP_FP_TMP+5
                beq     @cmdPRINT_numstr_print0point_sk
                jsr     storeAatEndOfString
                bra     @cmdPRINT_numstr_print0point_lp

@cmdPRINT_numstr_print0point_sk:
                lda     #$80
cmdPRINT_numstr_stADigLenPDigs:
                sta     DP_BAS_70_UK
@cmdPRINT_numstr_print_lp2:
                jsr     parseDMul10
                dec     DP_BAS_70_UK
                bne     @cmdPRINT_numstr_print_sk2
                lda     #'.'
                jsr     storeAatEndOfString
@cmdPRINT_numstr_print_sk2:
                dec     DP_BAS_TMP6+1
                bne     @cmdPRINT_numstr_print_lp2
                ldy     DP_BAS_TMP6
                dey
                beq     @cmdPRINT_numstr_print_exp
                dey
                beq     @cmdPRINT_numstr_print_exp_fix
                ldy     DP_BAS_STRLEN
@cmdPRINT_numstr_removetrail0_lp:
                dey
                lda     [DP_BAS_STRWKSP_L],y
                cmp     #'0'
                beq     @cmdPRINT_numstr_removetrail0_lp
                cmp     #'.'
                beq     @cmdPRINT_numstr_removetrail0_sk
                iny
@cmdPRINT_numstr_removetrail0_sk:
                sty     DP_BAS_STRLEN
@cmdPRINT_numstr_print_exp_fix:
                lda     DP_FP_TMP+5
                beq     @rtsL7A92
@cmdPRINT_numstr_print_exp:
                lda     #'E'
                jsr     storeAatEndOfString
                lda     DP_FP_TMP+5
                bpl     @skpl2
                lda     #'-'
                jsr     storeAatEndOfString
                sec
                lda     #$00
                sbc     DP_FP_TMP+5
@skpl2:         jsr     cmdPRINT_numstr_convert_10_1
                lda     DP_BAS_TMP6
                beq     @rtsL7A92
                lda     #' '
                ldy     DP_FP_TMP+5
                bmi     @skmi
                jsr     storeAatEndOfString
@skmi:          cpx     #$00
                beq     storeAatEndOfString
@rtsL7A92:      rts

parseDMul10:    lda     DP_FPA_mant
                lsr     A
                lsr     A
                lsr     A
                lsr     A
                jsr     cmdPR_numstr_prAlow_nyb_asdigit
                lda     #$f0
                trb     DP_FPA_mant
parseDecMantMul10:
                pha
                ldx     DP_FPA_mant+3
                lda     DP_FPA_mant
                pha
                lda     DP_FPA_mant+1
                pha
                lda     DP_FPA_mant+2
                pha
                lda     DP_FPA_mant+4
                asl     A
                rol     DP_FPA_mant+3
                rol     DP_FPA_mant+2
                rol     DP_FPA_mant+1
                rol     DP_FPA_mant
                asl     A
                rol     DP_FPA_mant+3
                rol     DP_FPA_mant+2
                rol     DP_FPA_mant+1
                rol     DP_FPA_mant
                adc     DP_FPA_mant+4
                sta     DP_FPA_mant+4
                txa
                adc     DP_FPA_mant+3
                sta     DP_FPA_mant+3
                pla
                adc     DP_FPA_mant+2
                sta     DP_FPA_mant+2
                pla
                adc     DP_FPA_mant+1
                sta     DP_FPA_mant+1
                pla
                adc     DP_FPA_mant
                asl     DP_FPA_mant+4
                rol     DP_FPA_mant+3
                rol     DP_FPA_mant+2
                rol     DP_FPA_mant+1
                rol     A
                sta     DP_FPA_mant
                pla
                rts

cmdPRINT_numstr_convert_10_1:
                ldx     #$ff
                sec
@lp10:          inx
                sbc     #$0a
                bcs     @lp10
                adc     #$0a
                pha
                txa
                beq     @skeq
                jsr     cmdPR_numstr_prAlow_nyb_asdigit
@skeq:          pla
cmdPR_numstr_prAlow_nyb_asdigit:
                ora     #'0'
storeAatEndOfString:
                phx
                ldx     DP_BAS_STRLEN
                jsr     storeStringX
                plx
                inc     DP_BAS_STRLEN
                rts

parseDecimal_NaN:
                jsr     fpSetFPASgnExp0
                clc                                     ;CLC=no number, return REAL
                lda     #RETV_REAL
                rts

;*******************************************************************************
;* Scan Decimal Number                                                         *
;*******************************************************************************
parseDecimalLiteral:
                stz     DP_FPA_mant
                stz     DP_FPA_mant+1
                stz     DP_FPA_mant+2
                stz     DP_FPA_mant+3
                stz     DP_FPA_mant+4
                stz     DP_FP_TMP+4                     ;Clear Decimal point found flag
                stz     DP_FP_TMP+5                     ;clear 10's exponent
                cmp     #'.'
                beq     @parseDecGotDp
                cmp     #':'
                bcs     parseDecimal_NaN
                sbc     #'/'
                bmi     parseDecimal_NaN
                sta     DP_FPA_mant+4
                iny
; Quickly do next character - faster way of doing 2 digit numbers?
                lda     [DP_BAS_TXTPTR],y
                cmp     #':'
                bcs     @parseDec_skNotDp
                sbc     #'/'                            ;carry is clear so this is effectively subtracting '0'
                bcc     @parseDecSk1                    ;skip forward to proper decode loop
                sta     DP_FPA_sgn                      ;store temporarily in sign
; multiply first digit by 10 and add the one in SGN
                lda     DP_FPA_mant+4
                asl     A
                asl     A
                adc     DP_FPA_mant+4
                asl     A
                adc     DP_FPA_sgn
                sta     DP_FPA_mant+4
; main number parse loop
@parseDecLp1:   iny
@parseDecSk1:   lda     [DP_BAS_TXTPTR],y
                cmp     #'.'
                bne     @parseDec_skNotDp
; had a DP check if we've already had one and exit if so
@parseDecGotDp: lda     DP_FP_TMP+4
                bne     @parseDecSkDone
                inc     DP_FP_TMP+4
                bra     @parseDecLp1

@parseDec_skNotDp:
                cmp     #'E'
                beq     @parseDec_scanExponent
                cmp     #':'
                bcs     @parseDecSkDone
                sbc     #'/'                            ;carry is clear so this is effectively subtracting '0'
                bcc     @parseDecSkDone
                ldx     DP_FPA_mant
                cpx     #$18                            ;check MS byte of mantissa
                bcc     @parseDec_AddToMant             ;mantissa not "full" skip forward to add
                ldx     DP_FP_TMP+4
                bne     @parseDecLp1                    ;we've already parsed a decimal point ignore rest
                inc     DP_FP_TMP+5                     ;we've not had DP increase 10's exponent
                bra     @parseDecLp1

@parseDec_AddToMant:
                ldx     DP_FP_TMP+4
                beq     @sknodp2
                dec     DP_FP_TMP+5                     ;decrement 10's exponent if we've had a dp
@sknodp2:       jsr     parseDecMantMul10
; add A to mantissa
                adc     DP_FPA_mant+4
                sta     DP_FPA_mant+4
                bcc     @parseDecLp1
                inc     DP_FPA_mant+3
                bne     @parseDecLp1
                inc     DP_FPA_mant+2
                bne     @parseDecLp1
                inc     DP_FPA_mant+1
                bne     @parseDecLp1
                inc     DP_FPA_mant
                bra     @parseDecLp1

@parseDec_scanExponent:
                jsr     parseDecScanExpToA
                adc     DP_FP_TMP+5
                sta     DP_FP_TMP+5
; We've now finished scanning digits, fixup into a proper FP number
@parseDecSkDone:
                sty     DP_BAS_TXTPTR_OFF
                lda     DP_FP_TMP+5
                ora     DP_FP_TMP+4
                beq     @parseDec_skReturnAsINT         ;Can we return this as an integer?
                jsr     fpFPACkMant0SetSgnExp0          ;Check for 0
                beq     @parseDec_Exit                  ;if it was 0 exit
@parseDec_Normalize:
                lda     #$a8                            ;default exponent
                sta     DP_FPA_exp+1
                stz     DP_FPA_exp
                stz     DP_FPA_sgn
                jsr     fpNormalizeFPA_bra
                lda     DP_FP_TMP+5
                bmi     @parseDec_ExpDiv10lp
                beq     @parseDec_RoundAndExit
@parseDec_ExpMul10lp:
                jsr     fpMulBy10
                dec     DP_FP_TMP+5
                bne     @parseDec_ExpMul10lp
                bra     @parseDec_RoundAndExit

@parseDec_ExpDiv10lp:
                jsr     fpFloatADiv10
                inc     DP_FP_TMP+5
                bne     @parseDec_ExpDiv10lp
@parseDec_RoundAndExit:
                jsr     fpRountMantFPA_bra
@parseDec_Exit: sec
                lda     #RETV_REAL
                rts

; The parsed number had no exponent and no decimal point, return as an integer
@parseDec_skReturnAsINT:
                lda     DP_FPA_mant+1
                sta     DP_BAS_INT_WA+3
                and     #$80
                ora     DP_FPA_mant
                bne     @parseDec_Normalize             ;oops, too big for an integer, do a real
                lda     DP_FPA_mant+4
                sta     DP_BAS_INT_WA
                lda     DP_FPA_mant+3
                sta     DP_BAS_INT_WA+1
                lda     DP_FPA_mant+2
                sta     DP_BAS_INT_WA+2
                lda     #RETV_INT
                sec
                rts

parseD_skNegExp:
                jsr     parseD_scanExpReadDigits
                eor     #$ff
                sec
                rts

parseDecScanExpToA:
                iny
                lda     [DP_BAS_TXTPTR],y
                cmp     #'-'
                beq     parseD_skNegExp
                cmp     #'+'
                bne     notplus
parseD_scanExpReadDigits:
                iny
                lda     [DP_BAS_TXTPTR],y
notplus:        cmp     #':'                            
                bcs     @retExp0
                sbc     #'/'                            
                bcc     @retExp0
                sta     DP_BAS_FP_TMPEXP
                iny
                lda     [DP_BAS_TXTPTR],y
                cmp     #':'
                bcs     @retA
                sbc     #'/'
                bcc     @retA
                iny
                sta     DP_BAS_TMP64
                lda     DP_BAS_FP_TMPEXP
                asl     A
                asl     A
                adc     DP_BAS_FP_TMPEXP
                asl     A
                adc     DP_BAS_TMP64
                rts

@retA:          lda     DP_BAS_FP_TMPEXP
                clc
                rts

@retExp0:       lda     #$00
                clc
                rts

fpFPACkMant0SetSgnExp0:
                lda     DP_FPA_mant
                ora     DP_FPA_mant+1
                ora     DP_FPA_mant+2
                ora     DP_FPA_mant+3
                ora     DP_FPA_mant+4
                beq     fpSetFPASgnExp0
                lda     DP_FPA_sgn
                bne     anRTS_7c31
                inc     A
                rts

fpSetFPASgnExp0:
                stz     DP_FPA_sgn
                stz     DP_FPA_exp+1
                stz     DP_FPA_exp
anRTS_7c31:     rts

fpCopyFPAtoFPB: lda     DP_FPA_sgn
                sta     DP_FPB_sgn
                lda     DP_FPA_exp+1
                sta     DP_FPB_exp
                lda     DP_FPA_mant
                sta     DP_FPB_mant
                lda     DP_FPA_mant+1
                sta     DP_FPB_mant+1
                lda     DP_FPA_mant+2
                sta     DP_BAS_61_UK
                lda     DP_FPA_mant+3
                sta     DP_FPB_mant+3
                lda     DP_FPA_mant+4
                sta     DP_FPB_mant+4
                rts

fpCopyFPAtoFPBAndShiftRight:
                jsr     fpCopyFPAtoFPB
fpShiftFPBMantRight:
                lsr     DP_FPB_mant
                ror     DP_FPB_mant+1
                ror     DP_BAS_61_UK
                ror     DP_FPB_mant+3
                ror     DP_FPB_mant+4
                rts

fpMulBy10:      clc
                lda     DP_FPA_exp+1
                adc     #$03
                sta     DP_FPA_exp+1
                bcc     @sk1
                inc     DP_FPA_exp
@sk1:           jsr     fpCopyFPAtoFPBAndShiftRight
                jsr     fpShiftFPBMantRight
fpAddAtoBstoreinA_sameExp:
                lda     DP_FPA_mant+4
                adc     DP_FPB_mant+4
                sta     DP_FPA_mant+4
                lda     DP_FPA_mant+3
                adc     DP_FPB_mant+3
                sta     DP_FPA_mant+3
                lda     DP_FPA_mant+2
                adc     DP_BAS_61_UK
                sta     DP_FPA_mant+2
                lda     DP_FPA_mant+1
                adc     DP_FPB_mant+1
                sta     DP_FPA_mant+1
                lda     DP_FPA_mant
                adc     DP_FPB_mant
                sta     DP_FPA_mant
                bcc     anRTS_7c9e
fpRORMantAincExp:
                ror     DP_FPA_mant                     ;TODO - optimize 16bits
                ror     DP_FPA_mant+1
                ror     DP_FPA_mant+2
                ror     DP_FPA_mant+3
                ror     DP_FPA_mant+4
                inc     DP_FPA_exp+1
                bne     anRTS_7c9e
                inc     DP_FPA_exp
anRTS_7c9e:     rts

fpFloatADiv10:  sec
                lda     DP_FPA_exp+1
                sbc     #$04
                sta     DP_FPA_exp+1
                bcs     @sk10
                dec     DP_FPA_exp
@sk10:          jsr     fpCopyFPAtoFPBAndShiftRight
                jsr     fpAddAtoBstoreinA_sameExp
                jsr     fpCopyFPAtoFPBAndShiftRight
                jsr     fpShiftFPBMantRight
                jsr     fpShiftFPBMantRight
                jsr     fpShiftFPBMantRight
                jsr     fpAddAtoBstoreinA_sameExp
                stz     DP_FPB_mant
                lda     DP_FPA_mant
                sta     DP_FPB_mant+1
                lda     DP_FPA_mant+1
                sta     DP_BAS_61_UK
                lda     DP_FPA_mant+2
                sta     DP_FPB_mant+3
                lda     DP_FPA_mant+3
                sta     DP_FPB_mant+4
                lda     DP_FPA_mant+4
                rol     A
                jsr     fpAddAtoBstoreinA_sameExp
                stz     DP_FPB_mant+1
                lda     DP_FPA_mant
                sta     DP_BAS_61_UK
                lda     DP_FPA_mant+1
                sta     DP_FPB_mant+3
                lda     DP_FPA_mant+2
                sta     DP_FPB_mant+4
                lda     DP_FPA_mant+3
                rol     A
                jsr     fpAddAtoBstoreinA_sameExp
                lda     DP_FPA_mant+1
                rol     A
                lda     DP_FPA_mant
                adc     DP_FPA_mant+4
                sta     DP_FPA_mant+4
                bcc     anRtsL7D06
                inc     DP_FPA_mant+3
                bne     anRtsL7D06
fpIncFPAMantMSBs:
                inc     DP_FPA_mant+2
                bne     anRtsL7D06
                inc     DP_FPA_mant+1
                bne     anRtsL7D06
                inc     DP_FPA_mant
                beq     fpRORMantAincExp
anRtsL7D06:     rts

fpMoveRealAtPTR1toFPB:
                stz     DP_FPB_mant+4                   ;TODO - optimize this
                ldy     #$04
                lda     [DP_BAS_FP_PTR1],y
                sta     DP_FPB_mant+3
                dey
                lda     [DP_BAS_FP_PTR1],y
                sta     DP_BAS_61_UK
                dey
                lda     [DP_BAS_FP_PTR1],y
                sta     DP_FPB_mant+1
                dey
                lda     [DP_BAS_FP_PTR1],y
                sta     DP_FPB_sgn
                tay
                lda     [DP_BAS_FP_PTR1]
                sta     DP_FPB_exp
                bne     @sk1
                tya
                ora     DP_FPB_mant+1
                ora     DP_BAS_61_UK
                ora     DP_FPB_mant+3
                beq     @sk2
@sk1:           tya
                ora     #$80
@sk2:           sta     DP_FPB_mant
                rts

fpCopyFPAtoPrintPTR:
                lda     #$00
                clc
                adc     DP_BAS_FPTMPptr3
                sta     DP_BAS_FP_PTR1
                lda     #$00
                adc     DP_BAS_FPTMPptr3+1
                sta     DP_BAS_FP_PTR1+1
                lda     #$00
                adc     DP_BAS_FPTMPptr3+2
                sta     DP_BAS_FP_PTR1+2
fpCopyFPAtoPTR1:
                lda     DP_FPA_exp+1
                sta     [DP_BAS_FP_PTR1]
                ldy     #$01
                lda     DP_FPA_sgn
                eor     DP_FPA_mant
                and     #$80
                eor     DP_FPA_mant
                sta     [DP_BAS_FP_PTR1],y
                lda     DP_FPA_mant+1
                iny
                sta     [DP_BAS_FP_PTR1],y
                lda     DP_FPA_mant+2
                iny
                sta     [DP_BAS_FP_PTR1],y
                lda     DP_FPA_mant+3
                iny
                sta     [DP_BAS_FP_PTR1],y
                rts

fpCopyPrintPTRtoFPA:
                lda     #$00
                clc
                adc     DP_BAS_FPTMPptr3
                sta     DP_BAS_FP_PTR1
                lda     #$00
                adc     DP_BAS_FPTMPptr3+1
                sta     DP_BAS_FP_PTR1+1
                lda     #$00
                adc     DP_BAS_FPTMPptr3+2
                sta     DP_BAS_FP_PTR1+2
fpCopyPTR1toFPA:
                stz     DP_FPA_mant+4
                stz     DP_FPA_exp
                ldy     #$04
                lda     [DP_BAS_FP_PTR1],y
                sta     DP_FPA_mant+3
                dey
                lda     [DP_BAS_FP_PTR1],y
                sta     DP_FPA_mant+2
                dey
                lda     [DP_BAS_FP_PTR1],y
                sta     DP_FPA_mant+1
                dey
                lda     [DP_BAS_FP_PTR1],y
                sta     DP_FPA_sgn
                tay
                lda     [DP_BAS_FP_PTR1]
                sta     DP_FPA_exp+1
                bne     @sk1
                tya
                ora     DP_FPA_mant+1
                ora     DP_FPA_mant+2
                ora     DP_FPA_mant+3
                beq     @sk2
@sk1:           tya
                ora     #$80
@sk2:           sta     DP_FPA_mant
                rts

fpSetFPBto0:    stz     DP_FPB_sgn
                stz     DP_FPB_exp
                stz     DP_FPB_mant
                stz     DP_FPB_mant+1
                stz     DP_BAS_61_UK
                stz     DP_FPB_mant+3
                stz     DP_FPB_mant+4
                rts

fpConstAtoPTR1: sta     DP_BAS_FP_PTR1
                lda     #>fpConstPiDiv2                 ;TODO: check page sensitive!
                sta     DP_BAS_FP_PTR1+1
                phb                                     ;Should this be PHK?
                pla
                sta     DP_BAS_FP_PTR1+2
                rts

PrintPTRtoPTR1: lda     #$00                            ;TODO: optimize this and other superflouous adds away
                clc
                adc     DP_BAS_FPTMPptr3
                sta     DP_BAS_FP_PTR1
                lda     #$00
                adc     DP_BAS_FPTMPptr3+1
                sta     DP_BAS_FP_PTR1+1
                lda     #$00
                adc     DP_BAS_FPTMPptr3+2
                sta     DP_BAS_FP_PTR1+2
                rts

exec_TAN:       jsr     evalLevel1ConvertReal
                phy
                phx
                ldx     #ARITH_FN_TAN
                brl     call_ARITH

fpLoad1:        lda     #$80
                sta     DP_FPA_mant
                inc     A
                sta     DP_FPA_exp+1
                jmp     zero_FPA_sign_expO_manlo

jmp_brk_12_DivBy0:
                jmp     brk_12_DivisionBy0

tblDivConsts:   .byte   $02,$08,$08,$08

fpFPAeqPTR1divFPA:
                lda     DP_FPA_mant
                beq     jmp_brk_12_DivBy0
                jsr     fpMoveRealAtPTR1toFPB
                bne     @sk0
                jmp     zero_FPA

@sk0:           lda     DP_FPB_sgn
                eor     DP_FPA_sgn
                sta     DP_FPA_sgn
                sec
                lda     DP_FPB_exp
                adc     #$81
                rol     DP_FPA_exp
                sbc     DP_FPA_exp+1
                bcs     @skinc
                dec     DP_FPA_exp
@skinc:         sta     DP_FPA_exp+1
                ldy     #$04
                sty     DP_FPB_exp
                lda     DP_FPB_mant
                ldx     #$08
                bra     @sk2

@lp:            stx     DP_FP_TMP,y
                ldx     tblDivConsts,y
                sty     DP_FPB_exp
@lp2:           bcs     @skcs
@sk2:           cmp     DP_FPA_mant
                bne     @skcmpdone
                ldy     DP_FPB_mant+1
                cpy     DP_FPA_mant+1
                bne     @skcmpdone
                ldy     DP_BAS_61_UK
                cpy     DP_FPA_mant+2
                bne     @skcmpdone
                ldy     DP_FPB_mant+3
                cpy     DP_FPA_mant+3
@skcmpdone:     bcc     @skBltA
@skcs:          tay
                lda     DP_FPB_mant+3
                sbc     DP_FPA_mant+3
                sta     DP_FPB_mant+3
                lda     DP_FPB_mant+2
                sbc     DP_FPA_mant+2
                sta     DP_FPB_mant+2
                lda     DP_FPB_mant+1
                sbc     DP_FPA_mant+1
                sta     DP_FPB_mant+1
                tya
                sbc     DP_FPA_mant
                sec
@skBltA:        rol     DP_FPB_sgn
                asl     DP_FPB_mant+3
                rol     DP_BAS_61_UK
                rol     DP_FPB_mant+1
                rol     A
                dex
                bne     @lp2
                ldx     DP_FPB_sgn
                ldy     DP_FPB_exp
                dey
                bpl     @lp
                ora     DP_FPB_mant+1
                ora     DP_BAS_61_UK
                ora     DP_FPB_mant+3
                beq     @skZ
                sec
@skZ:           txa
                ror     A
                ror     A
                ror     A
                and     #$e0
                sta     DP_FPA_mant+4
                lda     DP_FP_TMP
                sta     DP_FPA_mant+3
                lda     DP_FP_TMP+1
                sta     DP_FPA_mant+2
                lda     DP_FP_TMP+2
                sta     DP_FPA_mant+1
                lda     DP_FP_TMP+3
                sta     DP_FPA_mant
                bmi     fpRountMantFPA_bra
                jsr     FPANormalizeSlow
                bra     fpRountMantFPA_bra

fpFPAeqPTR1subFPA:
                jsr     fpNegateFP_A
fpFPAeqPTR1addFPA:
        .IF !.defined(OPTIMIZE)
                bra     @pointless                      
@pointless:     
        .ENDIF
                jsr     fpMoveRealAtPTR1toFPB
                beq     rts7ECC
                jsr     L5307                
fpRountMantFPA_bra:
        .IF !.defined(OPTIMIZE)
                bra     fpRountMantissaFPA              
        .ENDIF
fpRountMantissaFPA:
                lda     DP_FPA_mant+4
                cmp     #$80
                bcc     L7EB6
                beq     L7EB3
                inc     DP_FPA_mant+3
                bne     L7EB6
                jsr     fpIncFPAMantMSBs
                bra     L7EB6

fpFPAeqPTR1mulFPA:
                jsr     fpFPAeqPTRmulFPA_internal
                bra     fpRountMantFPA_bra

L7EB3:          rol     A
                tsb     DP_FPA_mant+3
L7EB6:          lda     DP_FPA_exp
                beq     zero_FPA_matLsb
                bpl     brk_14_TooBigMaybe
zero_FPA:       stz     DP_FPA_exp+1
                stz     DP_FPA_mant
zero_FPA_sign_expO_manlo:
                stz     DP_FPA_sgn
                stz     DP_FPA_exp
                stz     DP_FPA_mant+1
                stz     DP_FPA_mant+2
                stz     DP_FPA_mant+3
zero_FPA_matLsb:
                stz     DP_FPA_mant+4
rts7ECC:        rts

brk_14_TooBigMaybe:
                brk     $14

                .asciiz "Too big may be"

fpFPAeqPTRmulFPA_internal:
                lda     DP_FPA_mant
                bne     @sk
                jmp     rts7ECC

@sk:            jsr     fpMoveRealAtPTR1toFPB
                bne     @sk2
                jmp     zero_FPA

@sk2:           phk
                plb
                clc
                lda     DP_FPA_exp+1
                adc     DP_FPB_exp
                rol     DP_FPA_exp
                sbc     #$7f
                sta     DP_FPA_exp+1
                bcs     @sk3
                dec     DP_FPA_exp
@sk3:           phx
                ldx     #$05
                ldy     #$00
@mullp1:        lda     DP_FPA_exp+1,x
                sta     DP_FPB_mant+4,x
                sty     DP_FPA_exp+1,x
                dex
                bne     @mullp1
                lda     DP_FPA_sgn
                eor     DP_FPB_sgn
                sta     DP_FPA_sgn
                ldy     #$20
@mullp2:        lsr     DP_FPB_mant
                ror     DP_FPB_mant+1
                ror     DP_BAS_61_UK
                ror     DP_FPB_mant+3
                ror     DP_FPB_mant+4
                asl     DP_FP_TMP+2
                rol     DP_FP_TMP+1
                rol     DP_FP_TMP
                rol     DP_BAS_TMP64
                bcc     @mulsk4
                clc
                jsr     dpAddFPBmanttoFPAmant
@mulsk4:        dey
                bne     @mullp2
                plx
                lda     DP_FPA_mant
                bpl     @sk51B2 
        .IF !.defined(OPTIMIZE)                        
                jmp     rts7ECC
        .ELSE
                rts
        .ENDIF

@sk51B2:        jmp     NormaliseRealA_3
        .IF !.defined(OPTIMIZE)
                rts                                     ;TODO - DEAD code?
        .ENDIF

dpAddFPBmanttoFPAmant:
                lda     DP_FPA_mant+4
                adc     DP_FPB_mant+4
                sta     DP_FPA_mant+4
                lda     DP_FPA_mant+3
                adc     DP_FPB_mant+3
                sta     DP_FPA_mant+3
                lda     DP_FPA_mant+2
                adc     DP_BAS_61_UK
                sta     DP_FPA_mant+2
                lda     DP_FPA_mant+1
                adc     DP_FPB_mant+1
                sta     DP_FPA_mant+1
                lda     DP_FPA_mant
                adc     DP_FPB_mant
                sta     DP_FPA_mant
                rts

exec_LN:        jsr     evalLevel1ConvertReal
                phy
                phx
                ldx     #ARITH_FN_LN
                brl     call_ARITH

exec_SQR:       jsr     evalLevel1ConvertReal
                phy
                phx
                ldx     #ARITH_FN_SQR
                brl     call_ARITH

fpNormalizeAndReturnFPA:
                lda     DP_FPA_mant
                bmi     @L7F75
                jsr     fpNormalizeFPA_bra
@L7F75:         jsr     fpRountMantFPA_bra
                lda     #RETV_REAL
                rts

fpLoadFpConstA: jsr     fpConstAtoPTR1
                jmp     fpCopyPTR1toFPA

exec_ACS:       jsr     evalLevel1ConvertReal
                phy
                phx
                ldx     #ARITH_FN_ACS
                brl     call_ARITH

exec_ASN:       jsr     evalLevel1ConvertReal
                phy
                phx
                ldx     #ARITH_FN_ASN
                brl     call_ARITH

fpSetFPAPIdiv2: lda     #<fpConstPiDiv2
                jmp     fpLoadFpConstA                  ;TODO: optimize - move to following!

exec_ATN:       jsr     evalLevel1ConvertReal
                phy
                phx
                ldx     #ARITH_FN_ATN
                brl     call_ARITH

exec_SIN:       clc
exec_COS:       php
                jsr     evalLevel1ConvertReal
                plp
                phy
                phx
                bcc     @sin

                ldx     #ARITH_FN_COS
                bra     call_ARITH

@sin:           ldx     #ARITH_FN_SIN
call_ARITH:     phb
                phx
                phy
                jsr     stack_REAL
                ply
                plx
                jsr     BHAeqDP_BAS_STACKptr               
                phk
                jsr     moduleCallARITHref
                jsr     DP_BAS_STACKptreqBHA
                plb
                jsr     popFPFromStackToPTR1
                jsr     fpCopyPTR1toFPA
                plx
                ply
                lda     #RETV_REAL
                rts

exec_RAD:       jsr     evalLevel1ConvertReal
                phy
                phx
                ldx     #ARITH_FN_RAD
                brl     call_ARITH

exec_LOG:       jsr     evalLevel1ConvertReal
                phy
                phx
                ldx     #ARITH_FN_LOG
                brl     call_ARITH

exec_DEG:       jsr     evalLevel1ConvertReal
                phy
                phx
                ldx     #ARITH_FN_DEG
                brl     call_ARITH

exec_EXP:       jsr     evalLevel1ConvertReal
                phy
                phx
                ldx     #ARITH_FN_EXP
                brl     call_ARITH

        .IFDEF COMMUNICATOR
                .a16
                .i16
doCommandFailed:
                jsr     setREPORT_BHA_Cy
                bcc     @anRTS
                cop     COP_61_OPERC                    ;get error string to BHA
                ldx     #$0000
                cop     COP_02_OPWRA                    ;print string at BHA
                cop     COP_03_OPNLI                    ;OS NEWL
                brk     $c8

                .asciiz "Command failed"

@anRTS:         rts
                .a8
                .i8

setREPORT_BHA_Cy:
                php
                rep     #$30
                .a16
                .i16
                phb
                pha
                stz     DP_BAS_00_REAL_UK
                stz     DP_BAS_00_REAL_UK+2
                rol     DP_BAS_00_REAL_UK
                stx     DP_BAS_00_REAL_UK+4
                stz     $06
                sta     DP_BAS_Report
                stz     DP_BAS_Report+2
                sep     #$20
                .a8
                phb
                pla
                sta     DP_BAS_Report+2
                rep     #$20
                .a16
                pla
                plb
                plp
                rts

                .a8
                .i8

        .ENDIF

doINKEY_int:    jsr     evalLevel1checkTypeStoreAsINT
@lp:            lda     #$81
                ldx     DP_BAS_INT_WA
                ldy     DP_BAS_INT_WA+1
                jsr     call_OSBYTE
                bcc     @sk
                bit     DP_BAS_INT_WA+1
                bmi     @sk
                cpy     #$00
                beq     @lp
@sk:            rts

fnRND_1:        jsr     rndNext
fnRND_0:        stz     DP_FPA_sgn
                stz     DP_FPA_exp
                stz     DP_FPA_mant+4
                lda     #$80
                sta     DP_FPA_exp+1
                ldy     #$00
                ldx     #$03
@lp:            phx
                tyx
                eor     DP_BAS_RAND,x
                plx
                sta     DP_FPA_mant,x
                iny
                dex
                bpl     @lp
                jmp     fpNormalizeAndReturnFPA

fnRND_int:      inc     DP_BAS_TXTPTR_OFF
                jsr     evalL1BracketAlreadyOpenConv2INT
                lda     DP_BAS_INT_WA+3
                bmi     fnRND_randomize
                ora     DP_BAS_INT_WA+2
                ora     DP_BAS_INT_WA+1
                bne     @sk
                lda     DP_BAS_INT_WA
                beq     fnRND_0
                cmp     #$01
                beq     fnRND_1
@sk:            jsr     braIntToReal
                jsr     stack_REAL
                jsr     fnRND_1
                jsr     popFPFromStackToPTR1
                jsr     fpFPAeqPTRmulFPA_internal
                jsr     eval_real2INT
                jsr     INT_inc_WA
                bra     retRETV_INT

fnRND_randomize:
                ldx     #DP_BAS_RAND
                jsr     storeWAatDPX
                lda     #$40
                sta     DP_BAS_RAND+4
                rts

exec_RND:       ldy     DP_BAS_TXTPTR_OFF
                lda     [DP_BAS_TXTPTR],y
                cmp     #'('
                beq     fnRND_int
                jsr     rndNext
                ldx     #DP_BAS_RAND
copyDPXtoWA:    lda     DP_BAS_00_REAL_UK,x
                sta     DP_BAS_INT_WA
                lda     DP_BAS_00_REAL_UK+1,x
                sta     DP_BAS_INT_WA+1
                lda     DP_BAS_00_REAL_UK+2,x
                sta     DP_BAS_INT_WA+2
                lda     $03,x
                sta     DP_BAS_INT_WA+3
retRETV_INT:    lda     #RETV_INT
                rts

exec_NOT:       jsr     evalLevel1checkTypeStoreAsINT
                ldx     #$03
@lp:            lda     DP_BAS_INT_WA,x
                eor     #$ff
                sta     DP_BAS_INT_WA,x
                dex
                bpl     @lp
                bra     retRETV_INT

exec_POS:       jsr     exec_VPOS
                stx     DP_BAS_INT_WA
                rts

exec_USR_checkLI:
                lda     #'L'
                jsr     @checkChrA
                cmp     #$01
                bcc     @rts
                lda     #'I'
@checkChrA:     ldy     DP_BAS_TXTPTR_OFF
                inc     DP_BAS_TXTPTR_OFF
                eor     [DP_BAS_TXTPTR],y
                beq     @rts
                dec     DP_BAS_TXTPTR_OFF
                eor     #$00
@rts:           rts

exec_USR:       jsr     exec_USR_checkLI
                php
                jsr     evalLevel1checkTypeStoreAsINT
                plp
                bcc     @skNotSEI
                bne     @skNotSEI
                phd
                phk
                pea     @readbackregs
                php
                sei
                jmp     USRCALL_enterCode

@skNotSEI:      phd
                phk                                     ;this will be consumed by the RTL in user code
                jsr     USRCALL_enterCode
@readbackregs:  jsr     USRCALL_GetReturnedRegsToDP
                pld
                lda     USR_RET_C
                sta     DP_BAS_INT_WA
                lda     USR_RET_X
                sta     DP_BAS_INT_WA+1
                lda     USR_RET_Y
                sta     DP_BAS_INT_WA+2
                lda     USR_RET_B2
                sta     DP_BAS_INT_WA+3
                bra     retRETV_INT

exec_VPOS:      lda     #OSBYTE_134_POS
                jsr     call_OSBYTE
                tya
retA_8bit_INT_presX:
                phx
                jsr     retA_8bit_INT
                plx
                rts

exec_EXT:       
        .IFDEF COMMUNICATOR
                jsr     parse_handleY_WAeqmin1_HAptWA
                phb
                cop     COP_24_OPCVD
                cop     COP_57_OPRLL
                bra     reportBHACy_retRETV_INT
        .ENDIF
        .IFDEF MOS
                lda     #2                              ; OSARGS FN 
                bra     mosGetFI
        .ENDIF

exec_PTR:       
        .IFDEF COMMUNICATOR
                jsr     parse_handleY_WAeqmin1_HAptWA
                phb
                cop     COP_24_OPCVD
                cop     COP_54_OPRSP
reportBHACy_retRETV_INT:
                jsr     setREPORT_BHA_Cy
                plb
                sep     #$30
                brl     retRETV_INT
        .ENDIF
        .IFDEF MOS
                lda     #0
mosGetFI:       pha
                jsr     parse_fileHandleHash2
                pla
                ldx     MOS_ZP_TMP
                ldy     DP_BAS_INT_WA
                jsl     nat_OSARGS
                ldx     #3
@lp:            lda     f:MOS_ZP_TMP,X
                sta     DP_BAS_INT_WA,X
                dex
                bpl     @lp
                brl     retRETV_INT
        .ENDIF

;*******************************************************************************
;* Parse file handle to INT_WA then set INT_WA=-1 then HA points at WA         *
;*******************************************************************************
parse_handleY_WAeqmin1_HAptWA:
                jsr     parse_fileHandleHash2
                rep     #$30
                .a16
                .i16
                ldy     DP_BAS_INT_WA
                lda     #$ffff
                sta     DP_BAS_INT_WA
                sta     DP_BAS_INT_WA+2
                lda     #DP_BAS_INT_WA
                rts

                .a8
                .i8
exec_BGET:      jsr     parse_fileHandleHash2
                jsr     callOSBGET
                jsr     retA_8bit_INT_presX
                bcc     @sk
                ror     DP_BAS_INT_WA+3                 ;set top bit of INT_WA if error
@sk:            rts

callOSBGET:     
        .IFDEF COMMUNICATOR
                phb
                phx
                rep     #$10
                ldy     DP_BAS_INT_WA
                cop     COP_0A_OPBGT
                jsr     setREPORT_BHA_Cy
                sep     #$10
                plx
                plb
                rts
        .ENDIF
        .IFDEF MOS
                phx
                ldy     DP_BAS_INT_WA
                jsl     nat_OSBGET
                plx
                rts
        .ENDIF

exec_OPENIN:    lda     #$40
                bra     exec_OPEN_A

exec_OPENOUT:   lda     #$80
                bra     exec_OPEN_A

exec_OPENUP:    lda     #$c0
exec_OPEN_A:    phb
                pha
                jsr     evalLevel1
                bne     jmp_brk06_type_mismatch11
                jsr     strTerm
        .IFDEF COMMUNICATOR
                lda     DP_BAS_STRWKSP_L+2
                pha
                plb
                lda     DP_BAS_STRWKSP_L+1
                xba
                lda     DP_BAS_STRWKSP_L
                ply
                rep     #$30
                phb
                cop     COP_44_OPOPN
                .a16
                .i16
                jsr     setREPORT_BHA_Cy
                plb
                bcc     @skCC
                ldy     #$0000
@skCC:          
                sty     DP_BAS_INT_WA
                stz     DP_BAS_INT_WA+2
                sep     #$30
                .a8
                .i8
                plb
                lda     #RETV_INT
                rts
        .ENDIF
        .IFDEF MOS

                ; copy string to bank0

                ldy     DP_BAS_STRLEN
                tyx
                lda     #$0D
                sta     f:BANK0_SCRATCH_PAGE,X
                

@lp:            cpy     #0
                beq     @sk
                dex
                dey                
                lda     [DP_BAS_STRWKSP_L],Y
                sta     f:BANK0_SCRATCH_PAGE,X
                brl     @lp
@sk:
                ; point at low bank0 buffer
                ldx     #<BANK0_SCRATCH_PAGE
                ldy     #>BANK0_SCRATCH_PAGE
                pla                             ; get back function code

                jsl     nat_OSFIND
                plb
                brl     retA_8bit_INT
        .ENDIF

jmp_brk06_type_mismatch11:
                jmp     brk06_type_mismatch

exec_PI:        jsr     fpSetFPAPIdiv2
                inc     DP_FPA_exp+1
                rts

exec_EVAL:      jsr     evalLevel1
                bne     jmp_brk06_type_mismatch10
                ldy     DP_BAS_STRLEN
                inc     DP_BAS_STRLEN
                lda     #$0d
                sta     [DP_BAS_STRWKSP_L],y
                jsr     StackString
                lda     DP_BAS_TXTPTR
                pha
                lda     DP_BAS_TXTPTR+1
                pha
                lda     DP_BAS_TXTPTR+2
                pha
                lda     DP_BAS_TXTPTR_OFF
                pha
                lda     DP_BAS_STACK
                ldx     DP_BAS_STACK+1
                ldy     DP_BAS_STACK+2
                inc     A
                sta     DP_BAS_TXTPTR
                sta     DP_BAS_TMP6
                bne     @sknotinx
                inx
@sknotinx:      stx     DP_BAS_TXTPTR+1
                stx     DP_BAS_TMP6+1
                bne     @sknotiny
                iny
@sknotiny:      sty     DP_BAS_TXTPTR+2
                sty     DP_BAS_TMP6+2
                jsr     tokNextSetFlag0_FF
                stz     DP_BAS_TXTPTR_OFF
                jsr     evalAtOFF
                jsr     deAllocAFromStack
retUnstackTXTPTR:
                pla
                sta     DP_BAS_TXTPTR_OFF
                pla
                sta     DP_BAS_TXTPTR+2
                pla
                sta     DP_BAS_TXTPTR+1
                pla
                sta     DP_BAS_TXTPTR
                lda     DP_BAS_40_VARTYPE
                rts

jmp_brk06_type_mismatch10:
                jmp     brk06_type_mismatch

exec_VAL:       jsr     evalLevel1
                bne     jmp_brk06_type_mismatch10
str2num:        ldx     DP_BAS_STRLEN
                lda     #$00
                jsr     storeStringX
                lda     DP_BAS_TXTPTR
                pha
                lda     DP_BAS_TXTPTR+1
                pha
                lda     DP_BAS_TXTPTR+2
                pha
                lda     DP_BAS_TXTPTR_OFF
                pha
                stz     DP_BAS_TXTPTR_OFF
                lda     DP_BAS_STRWKSP_L
                sta     DP_BAS_TXTPTR
                lda     DP_BAS_STRWKSP_L+1
                sta     DP_BAS_TXTPTR+1
                lda     DP_BAS_STRWKSP_L+2
                sta     DP_BAS_TXTPTR+2
                jsr     parse_skip_spaces
                cmp     #'-'
                beq     @skNeg
                cmp     #'+'
                bne     @sk
                jsr     parse_skip_spaces
@sk:            dec     DP_BAS_TXTPTR_OFF
                jsr     parseDecimalLiteral
                bra     @retVARTYPE

@skNeg:         jsr     parse_skip_spaces
                dec     DP_BAS_TXTPTR_OFF
                jsr     parseDecimalLiteral
                bcc     @retVARTYPE
                jsr     evalLevel1CheckNotStringNegate
@retVARTYPE:    sta     DP_BAS_40_VARTYPE
                bra     retUnstackTXTPTR

exec_INT:       jsr     evalLevel1
                beq     jmp_brk06_type_mismatch9
                bpl     @rtsL8282
                lda     DP_FPA_sgn
                php
                jsr     fpFPAMant2Int_remainder_inFPB
                plp
                bpl     @fpPos
                lda     DP_FPB_mant
                ora     DP_FPB_mant+1
                ora     DP_FPB_mant+2
                ora     DP_FPB_mant+3
                beq     @fpPos
                jsr     fpReal2Int_NegateMantissa
                jsr     fpIncrementFPAMantissa
                jsr     fpReal2Int_NegateMantissa
@fpPos:         jsr     fpCopyAmant2intWA
                lda     #RETV_INT
@rtsL8282:      rts

exec_ASC:       jsr     evalLevel1
                bne     jmp_brk06_type_mismatch9
                lda     DP_BAS_STRLEN
                beq     exec_TRUE
                lda     [DP_BAS_STRWKSP_L]
jmp_retA_8bit_INT:
                jmp     retA_8bit_INT

exec_INKEY:     jsr     doINKEY_int
                tya
                bne     exec_TRUE
                txa
                jmp     retAY_16bit_INT

jmp_brk06_type_mismatch9:
                jmp     brk06_type_mismatch

exec_EOF:       jsr     parse_fileHandleHash2
        .IFDEF COMMUNICATOR
                rep     #$10
                ldy     DP_BAS_INT_WA
                phb
                cop     COP_46_OPEND                    ;OPEND
                jsr     setREPORT_BHA_Cy
                plb
                sep     #$10
        .ENDIF
        .IFDEF MOS
                TODO    "CK EOF"
        .ENDIF
                bcs     exec_TRUE
                ldx     #$00
                lsr     A
                bcc     retxxxx_INT
exec_TRUE:      ldx     #$ff
retxxxx_INT:    stx     DP_BAS_INT_WA
                stx     DP_BAS_INT_WA+1
                stx     DP_BAS_INT_WA+2
                stx     DP_BAS_INT_WA+3
reta40:         lda     #RETV_INT
                rts

exec_FALSE:     ldx     #$00
                bra     retxxxx_INT

exec_SGN_real:  jsr     fpFPACkMant0SetSgnExp0
                beq     exec_FALSE
                bpl     ret1_INT
                bra     exec_TRUE

exec_SGN:       jsr     evalLevel1
                beq     jmp_brk06_type_mismatch9
                bmi     exec_SGN_real
                lda     DP_BAS_INT_WA+3
                ora     DP_BAS_INT_WA+2
                ora     DP_BAS_INT_WA+1
                ora     DP_BAS_INT_WA
                beq     reta40                          ;return 0
                lda     DP_BAS_INT_WA+3
                bmi     exec_TRUE                       ;minus INT return -1
ret1_INT:       lda     #$01
bra_jmp_retA_8bit_INT:
                bra     jmp_retA_8bit_INT               ;pos INT return 1

exec_POINT:     jsr     evalAtYcheckTypeInAConvert2INT
                jsr     stack_INTWA
                jsr     parse_skip_spaces_CMPcommaBRK
                jsr     evalL1BracketAlreadyOpenConv2INT
                lda     DP_BAS_INT_WA
                pha
                ldx     DP_BAS_INT_WA+1
                jsr     popIntA
                stx     DP_BAS_INT_WA+3
                pla
                sta     DP_BAS_INT_WA+2
                ldy     #>DP_BAS_INT_WA
                ldx     #DP_BAS_INT_WA
                lda     #OSWORD_9_POINT
        .IFDEF COMMUNICATOR
                jsr     cop_OSWORD
        .ENDIF
        .IFDEF MOS
                jsr     MOS_OSWORD
        .ENDIF
                lda     DP_FPA_sgn
                bmi     exec_TRUE
                bra     bra_jmp_retA_8bit_INT

exec_INSTR:     jsr     evalAtOFF
                bne     jmp_brk06_type_mismatch9
                cpx     #','
                bne     @skBrk05MissingComma
                inc     DP_BAS_TXTPTR_OFF
                jsr     StackString
                jsr     evalAtOFF
                beq     @skIsString
                brl     jmp_brk06_type_mismatch9

@skIsString:    lda     #$01
                sta     DP_BAS_INT_WA
                inc     DP_BAS_TXTPTR_OFF
                cpx     #')'
                beq     @skNoStart
                cpx     #','
                beq     @evalLength
@skBrk05MissingComma:
                jmp     brk_05_missing_comma

@evalLength:    jsr     evalstackStringExpectINTCloseBra
                jsr     popStackedString
@skNoStart:     ldx     DP_BAS_INT_WA
                bne     @noDefLen
                ldx     #$01
@noDefLen:      stx     DP_BAS_INT_WA
                txa
                dex
                stx     DP_BAS_INT_WA+3
                clc
                adc     DP_BAS_STACK
                sta     DP_BAS_TMP6
                lda     #$00
                adc     DP_BAS_STACK+1
                sta     DP_BAS_TMP6+1
                lda     #$00
                adc     DP_BAS_STACK+2
                sta     DP_BAS_TMP6+2
                lda     [DP_BAS_STACK]
                sec
                sbc     DP_BAS_INT_WA+3
                bcc     @deallocRet0
                sbc     DP_BAS_STRLEN
                bcc     @deallocRet0
                adc     #$00
                sta     DP_BAS_INT_WA+1
                jsr     deAllocAFromStack
@lp:            ldy     #$00
                ldx     DP_BAS_STRLEN
                beq     @skZeroStr
@lp2:           lda     [DP_BAS_TMP6],y
                cmp     [DP_BAS_STRWKSP_L],y
                bne     @incRetDecRemainIncTMP6ptr
                iny
                dex
                bne     @lp2
@skZeroStr:     lda     DP_BAS_INT_WA
@skJmpReturn8bitInt:
                jmp     retA_8bit_INT

@deallocRet0:   jsr     deAllocAFromStack
@skRet0:        lda     #$00
                bra     @skJmpReturn8bitInt

@incRetDecRemainIncTMP6ptr:
                inc     DP_BAS_INT_WA
                dec     DP_BAS_INT_WA+1
                beq     @skRet0
                inc     DP_BAS_TMP6
                bne     @lp
                inc     DP_BAS_TMP6+1
                bne     @lp
                inc     DP_BAS_TMP6+2
                bra     @lp

jmp_brk06_type_mismatch8:
                jmp     brk06_type_mismatch

exec_ABS:       jsr     evalLevel1
                beq     jmp_brk06_type_mismatch8
                bmi     exec_ABS_real
L83A4:          bit     DP_BAS_INT_WA+3
                bmi     negateWA
                bra     ldaRETV_INT_rtsL8417

exec_ABS_real:  stz     DP_FPA_sgn
                rts

fpFPAeqPTR1subFPAnegFPA:
                jsr     fpFPAeqPTR1subFPA
fpNegateFP_A:   lda     DP_FPA_mant
                beq     @sk
                lda     DP_FPA_sgn
                eor     #$80
                sta     DP_FPA_sgn
@sk:            lda     #RETV_REAL
                rts

parseCheckDollarNoSuchVar:
                iny
                lda     [DP_BAS_TXTPTR],y
                cmp     #'$'
                beq     fnREPORTdollar
                brl     parseCheckOPTNoSuchVar

fnREPORTdollar: iny
                sty     DP_BAS_TXTPTR_OFF
                pei     (DP_BAS_VARTOP+1)
                pei     (DP_BAS_VARTOP)
                lda     DP_BAS_STRWKSP_L
                sta     DP_BAS_VARTOP
                lda     DP_BAS_STRWKSP_L+1
                sta     DP_BAS_VARTOP+1
                lda     DP_BAS_STRWKSP_L+2
                sta     DP_BAS_VARTOP+2
                lda     #$80
                tsb     DP_BAS_LISTO
                jsr     doREPORT
                lda     #$80
                trb     DP_BAS_LISTO
                sec
                lda     DP_BAS_VARTOP
                sbc     DP_BAS_STRWKSP_L
                sta     DP_BAS_STRLEN
                pla
                sta     DP_BAS_VARTOP
                pla
                sta     DP_BAS_VARTOP+1
                pla
                pla
                sta     DP_BAS_VARTOP+2
                lda     #RETV_STR
                rts

evalLevel1UnaryMinus:
                jsr     evalSkipSpacesFnOrOthers
evalLevel1CheckNotStringNegate:
                beq     jmp_brk06_type_mismatch8
                bmi     fpNegateFP_A
negateWA:       sec
                lda     #$00
                tay
                sbc     DP_BAS_INT_WA
                sta     DP_BAS_INT_WA
                tya
                sbc     DP_BAS_INT_WA+1
                sta     DP_BAS_INT_WA+1
                tya
                sbc     DP_BAS_INT_WA+2
                sta     DP_BAS_INT_WA+2
                tya
                sbc     DP_BAS_INT_WA+3
                sta     DP_BAS_INT_WA+3
ldaRETV_INT_rtsL8417:
                lda     #RETV_INT
                rts

;*******************************************************************************
;* Strings in DATA statements can be surrounded by quotes or terminated by     *
;* comma                                                                       *
;*******************************************************************************
readCommaSepString:
                jsr     parse_skip_spaces
                cmp     #'"'
                beq     parseStringToStrWksp
                ldx     #$00
@lp:            lda     [DP_BAS_TXTPTR],y
                jsr     storeStringX
                iny
                inx
                cmp     #$0d
                beq     deydexStoreAsStringLen
                cmp     #','
                bne     @lp
deydexStoreAsStringLen:
                dey
decXstoreAsStringLen:
                dex
                stx     DP_BAS_STRLEN
                sty     DP_BAS_TXTPTR_OFF
                lda     #RETV_STR
                rts

storeStringX:   phy
                txy
                sta     [DP_BAS_STRWKSP_L],y
                ply
                rts

; Parse a string to String Workspace expect ending ", double "'s are placed in
; the string as a single quote
parseStringToStrWksp:
                ldx     #$00
@inylp:         iny
@lp:            lda     [DP_BAS_TXTPTR],y
                cmp     #$0d
                beq     @missingQ
                jsr     storeStringX
                iny
                inx
                cmp     #'"'
                bne     @lp
                lda     [DP_BAS_TXTPTR],y
                cmp     #'"'                            ;skip double "
                beq     @inylp
                bne     decXstoreAsStringLen

@missingQ:      jmp     brk_09_missing_quote

evalLevel1:     ldy     DP_BAS_TXTPTR_OFF
                inc     DP_BAS_TXTPTR_OFF
                lda     [DP_BAS_TXTPTR],y
                cmp     #' '
                beq     evalLevel1
                cmp     #'-'
                beq     evalLevel1UnaryMinus
                cmp     #'"'
                beq     parseStringToStrWksp
                cmp     #tknREPORT                      ;REPORT
                bne     @notREPORT
                brl     parseCheckDollarNoSuchVar

@notREPORT:     cmp     #'+'
                bne     __skNotPlus
evalSkipSpacesFnOrOthers:
                jsr     parse_skip_spaces
__skNotPlus:    cmp     #tknOPENIN
                bcc     @notbuiltinFN                   ;< tknOPENIN
                cmp     #tknEOF+1
                bcc     @dispatchFN                     ;OPENIN..EOF are all tokens for builtin FN
                brl     brk1a_no_such_var

@dispatchFN:    jmp     evalDispatchFN

@notbuiltinFN:  cmp     #'?'
                bcs     @skNotOpBra
                cmp     #'.'
                bcs     @skDecimal
                cmp     #'&'
                bne     @skneamp
                brl     evalL1ImmedHex

@skneamp:       cmp     #'('
                bne     @skNotOpBra
                brl     evalL1BracketAlreadyOpen

@skNotOpBra:    
        .IFDEF COMMUNICATOR
                cmp     #'@'
                bne     @skVariable
                iny
                lda     [DP_BAS_TXTPTR],y
                cmp     #'%'
                beq     @skVariable
; @variable lookup using the NAME module
                iny
                lda     [DP_BAS_TXTPTR],y
                cmp     #'%'
                beq     @skVariable
                clc
                lda     DP_BAS_TXTPTR_OFF
                adc     DP_BAS_TXTPTR
                sta     DP_BAS_INT_WA
                xba
                lda     DP_BAS_TXTPTR+1
                adc     #$00
                sta     DP_BAS_INT_WA+1
                lda     DP_BAS_TXTPTR+2
                adc     #$00
                sta     DP_BAS_INT_WA+2
                pha
                plb
                lda     DP_BAS_INT_WA+1
                xba
                ldx     #$08
                cop     COP_28_OPCMD
                .asciiz "NAME"

                bcs     @skVariable
                stx     DP_BAS_INT_WA+3
                tax
                lda     [DP_BAS_INT_WA],y
                cmp     #'%'
                bne     brk1a_no_such_var
                tya
                adc     DP_BAS_TXTPTR_OFF
                sta     DP_BAS_TXTPTR_OFF
                stx     DP_BAS_INT_WA
                xba
                sta     DP_BAS_INT_WA+1
                phb
                pla
                sta     DP_BAS_INT_WA+2
                phk
                plb
                lda     #RETV_INT
                rts

@skVariable:    phk
                plb
        .ENDIF
                dec     DP_BAS_TXTPTR_OFF
                ldy     DP_BAS_TXTPTR_OFF
                lda     [DP_BAS_TXTPTR],y
                jsr     findVarAtYMinus1
                bne     @jmpGetVarVal
                bit     DP_BAS_SET_TO_Z_AT_EOS
                bpl     parseCheckOPTNoSuchVar
                jsr     lineLabelSearch
@jmpGetVarVal:  jmp     GetVarVal

@skDecimal:     jsr     parseDecimalLiteral
                bcc     brk1a_no_such_var
                rts

parseCheckOPTNoSuchVar:
                lda     DP_BAS_OPT
                and     #$02
                bne     brk1a_no_such_var
                bcs     brk1a_no_such_var
                stx     DP_BAS_TXTPTR_OFF
getPpctAsINT:   ldy     #INTVAR_P+2
                lda     [DP_BAS_INTVARSptr],y
                tax
                dey
                lda     [DP_BAS_INTVARSptr],y
                xba
                dey
                lda     [DP_BAS_INTVARSptr],y
                xba
                tay
                xba
                brl     retAYX_24bit_INT

brk1a_no_such_var:
                brk     $1a

                .byte   "No such variable"

brk_1b_missing_closebrack:
                brk     $1b

                .byte   tknMissing
                .byte   ')'

brk_1c_BadHex:  brk     $1c

                .asciiz "Bad Hex"

evalL1BracketAlreadyOpen:
                jsr     evalAtOFF
                inc     DP_BAS_TXTPTR_OFF
                cpx     #')'
                bne     brk_1b_missing_closebrack
                tay
                rts

evalL1ImmedHex: jsr     exec_FALSE
                iny
evalL1ImmedHex_lp:
                lda     [DP_BAS_TXTPTR],y
                cmp     #'0'
                bcc     evalL1ImmedHex_skNotDig
                cmp     #':'
                bcc     evalL1ImmedHex_skGotDig
                sbc     #'7'
                cmp     #$0a
                bcc     evalL1ImmedHex_skNotDig
                cmp     #$10
                bcs     evalL1ImmedHex_skNotDig
evalL1ImmedHex_skGotDig:
                asl     A
                asl     A
                asl     A
                asl     A
                ldx     #$03
evalL1ImmedHex_lpShiftAcc:
                asl     A
                rol     DP_BAS_INT_WA
                rol     DP_BAS_INT_WA+1
                rol     DP_BAS_INT_WA+2
                rol     DP_BAS_INT_WA+3
                dex
                bpl     evalL1ImmedHex_lpShiftAcc
                iny
                bne     evalL1ImmedHex_lp
evalL1ImmedHex_skNotDig:
                txa
                bpl     brk_1c_BadHex
                sty     DP_BAS_TXTPTR_OFF
                lda     #RETV_INT
                rts

exec_ADVAL:     jsr     evalLevel1checkTypeStoreAsINT
                jsr     staWAptrAndWA
                lda     #RETV_INT
                rts

exec_TO:        iny
                lda     [DP_BAS_TXTPTR],y
                cmp     #'P'
                bne     brk1a_no_such_var
                inc     DP_BAS_TXTPTR_OFF
                lda     DP_BAS_TOP
                ldy     DP_BAS_TOP+1
                ldx     DP_BAS_TOP+2
                bra     retAYX_24bit_INT

exec_PAGE:      ldy     DP_BAS_PAGE+1
                lda     DP_BAS_PAGE
                ldx     DP_BAS_PAGE+2
                bra     retAYX_24bit_INT

jmp_brk06_type_mismatch7:
                jmp     brk06_type_mismatch

exec_LEN:       jsr     evalLevel1
                bne     jmp_brk06_type_mismatch7
                lda     DP_BAS_STRLEN
retA_8bit_INT:  ldy     #$00
retAY_16bit_INT:
                ldx     #$00
retAYX_24bit_INT:
                sta     DP_BAS_INT_WA
                sty     DP_BAS_INT_WA+1
                stx     DP_BAS_INT_WA+2
                stz     DP_BAS_INT_WA+3
                lda     #RETV_INT
                rts

exec_COUNT:     lda     DP_BAS_COUNT
                bra     retA_8bit_INT

exec_LOMEM:     lda     DP_BAS_LOMEM
                ldy     DP_BAS_LOMEM+1
                ldx     DP_BAS_LOMEM+2
                bra     retAYX_24bit_INT

exec_HIMEM:     lda     DP_BAS_HIMEM
                ldy     DP_BAS_HIMEM+1
                ldx     DP_BAS_HIMEM+2
                bra     retAYX_24bit_INT

exec_ERL:       ldy     DP_BAS_ERL+1
                lda     DP_BAS_ERL
                bra     retAY_16bit_INT

exec_ERR:       
        .IFDEF COMMUNICATOR
                phx
                ldx     #$14
                jsl     _ST
                plx
        .ENDIF
        .IFDEF MOS
                lda     DP_BAS_BL_ERRNO
        .ENDIF

                bra     retA_8bit_INT

exec_GET:       jsr     call_OSRDCH
                bra     retA_8bit_INT

exec_TIME:      iny
                lda     [DP_BAS_TXTPTR],y
                cmp     #'$'
                beq     exec_TIMEdollar
        .IFDEF COMMUNICATOR
                ldx     #DP_BAS_INT_WA
                ldy     #>DP_BAS_INT_WA
                lda     #OSWORD_1_READTIME
                jsr     cop_OSWORD
        .ENDIF
        .IFDEF MOS
                ldx     #<DP_BAS_INT_WA
                ldy     #0
                lda     #OSWORD_1_READTIME
                jsr     MOS_OSWORD
        .ENDIF
                lda     #RETV_INT
                rts

BHAeqSTRWKSP:   pei     (DP_BAS_STRWKSP_L+1)
                pla
                plb
                xba
                lda     DP_BAS_STRWKSP_L
                rts

exec_TIMEdollar:
                lda     #$ff
                pha
                inc     DP_BAS_TXTPTR_OFF
                iny
                lda     [DP_BAS_TXTPTR],y
                cmp     #'('
                bne     @nobrack
                inc     DP_BAS_TXTPTR_OFF
                jsr     evalL1BracketAlreadyOpenConv2INT
                lda     DP_BAS_INT_WA
                ora     #$80
                sta     $01,S
@nobrack:       
        .IFDEF COMMUNICATOR
                ldx     #_CKREAD
                ply
                jsr     BHAeqSTRWKSP
; The CLOCK driver will be called here for _CKREAD Y will either be the supplied
; value in brackets EORed with $80 or $FF
                cop     COP_28_OPCMD
                .asciiz "CLOCK"

                ldy     #$00
@strlenlp:      lda     [DP_BAS_STRWKSP_L],y
                cmp     #$0d
                beq     @strlensk
                iny
                bne     @strlenlp
@strlensk:      phk
                plb
                tya
        .ENDIF
        .IFDEF MOS
                TODO    "TIME$"
        .ENDIF
                bra     ret_str_lenA

exec_GETDollar: sec
                jsr     parse_fileHandleHash
                bcs     doOSRDCH_ret_as_1ch_str
                ldx     #$00
                ldy     #$00
@L864B:         phy
                jsr     callOSBGET
                ply
                bcs     @L8663
                cpx     #$00
                bne     @L865A
                cmp     #' '
                bcc     @L8663
@L865A:         sta     [DP_BAS_STRWKSP_L],y
                iny
                txa
                beq     @L864B
                dex
                bne     @L864B
@L8663:         tya
                bra     ret_str_lenA

doOSRDCH_ret_as_1ch_str:
                jsr     call_OSRDCH
retA_as_1ch_str:
                sta     [DP_BAS_STRWKSP_L]
                lda     #$01
                bra     ret_str_lenA

exec_LEFT:      clc
exec_RIGHT:     php
                jsr     evalAtOFF
                bne     jmp_brk06_type_mismatch6
                cpx     #','
                bne     jmp_brk_05_missing_comma
                inc     DP_BAS_TXTPTR_OFF
                jsr     evalstackStringExpectINTCloseBra
                jsr     popStackedString
                plp
                bcs     L8690
                lda     DP_BAS_INT_WA
                cmp     DP_BAS_STRLEN
                bcs     retSTR
ret_str_lenA:   sta     DP_BAS_STRLEN
retSTR:         lda     #RETV_STR
rtsL868F:       rts

L8690:          lda     DP_BAS_STRLEN
                sbc     DP_BAS_INT_WA
                bcc     retSTR
                beq     rtsL868F
                tay
                lda     DP_BAS_INT_WA
                sta     DP_BAS_STRLEN
                beq     rtsL868F
                ldx     #$00
@L86A1:         lda     [DP_BAS_STRWKSP_L],y
                jsr     storeStringX
                inx
                iny
                dec     DP_BAS_INT_WA
                bne     @L86A1
                bra     retSTR

exec_INKEYDollar:
                jsr     doINKEY_int
                txa
                bcc     retA_as_1ch_str
retRETV_STR:    lda     #RETV_STR                       ;TODO: optimize bcs retSTR
                bra     ret_str_lenA

jmp_brk06_type_mismatch6:
                jmp     brk06_type_mismatch

jmp_brk_05_missing_comma:
                jmp     brk_05_missing_comma

exec_MID:       jsr     evalAtOFF
                bne     jmp_brk06_type_mismatch6
                cpx     #','
                bne     jmp_brk_05_missing_comma
                jsr     StackString
                inc     DP_BAS_TXTPTR_OFF
                jsr     evalAtYcheckTypeInAConvert2INT
                lda     DP_BAS_INT_WA
                pha
                lda     #$ff
                sta     DP_BAS_INT_WA
                inc     DP_BAS_TXTPTR_OFF
                cpx     #')'
                beq     @sk
                cpx     #','
                bne     jmp_brk_05_missing_comma
                jsr     evalL1BracketAlreadyOpenConv2INT
@sk:            jsr     popStackedString
                pla
                tay
                clc
                beq     @sk2
                sbc     DP_BAS_STRLEN
                bcs     retRETV_STR
                dey
                tya
@sk2:           sta     DP_BAS_INT_WA+2
                tay
                ldx     #$00
                lda     DP_BAS_STRLEN
                sec
                sbc     DP_BAS_INT_WA+2
                cmp     DP_BAS_INT_WA
                bcs     @sk3
                sta     DP_BAS_INT_WA
@sk3:           lda     DP_BAS_INT_WA
                beq     retRETV_STR
@lp:            lda     [DP_BAS_STRWKSP_L],y
                jsr     storeStringX
                iny
                inx
                cpx     DP_BAS_INT_WA
                bne     @lp
                stx     DP_BAS_STRLEN
                bra     retvSTRL8775

exec_STR:       jsr     parse_skip_spaces
                ldy     #$ff
                cmp     #'~'
                beq     @sk1
                ldy     #$00
                dec     DP_BAS_TXTPTR_OFF
@sk1:           phy
                jsr     evalLevel1
                beq     jmp_brk06_type_mismatch5
                tay
                pla
                sta     DP_BAS_TMP2A+1
                phy
                ldy     #$03
                lda     [DP_BAS_INTVARSptr],y
                ply
                tax
                bne     @L873B
                sta     DP_BAS_TMP6
                jsr     cmdPRINT_num2str_invaldp
                bra     retvSTRL8775

@L873B:         jsr     cmdPRINT_num2str
                bra     retvSTRL8775

jmp_brk06_type_mismatch5:
                jmp     brk06_type_mismatch

exec_STRING:    jsr     evalAtYcheckTypeInAConvert2INT
                jsr     stack_INTWA
                jsr     parse_skip_spaces_CMPcommaBRK
                jsr     evalL1BracketAlreadyOpen
                bne     jmp_brk06_type_mismatch5
                jsr     popIntA
                ldx     DP_BAS_STRLEN
                beq     retvSTRL8775
                lda     DP_BAS_INT_WA
                beq     staSTRLENrts
                dec     DP_BAS_INT_WA
                beq     retvSTRL8775
@lp0:           ldy     #$00
@lp2:           lda     [DP_BAS_STRWKSP_L],y
                jsr     storeStringX
                iny
                inx
                beq     L877B
                cpy     DP_BAS_STRLEN
                bcc     @lp2
                dec     DP_BAS_INT_WA
                bne     @lp0
                stx     DP_BAS_STRLEN
retvSTRL8775:   lda     #RETV_STR
                rts

staSTRLENrts:   sta     DP_BAS_STRLEN
                rts

L877B:          jmp     brk_13_StringTooLong

staWAptrAndWA:  ldy     #$00
                lda     [DP_BAS_INT_WA],y
                sta     DP_BAS_INT_WA
                stz     DP_BAS_INT_WA+1
                stz     DP_BAS_INT_WA+2
                stz     DP_BAS_INT_WA+3
                rts

TMP6eqTXTPTR2:  lda     DP_BAS_TXTPTR2
                sta     DP_BAS_TMP6
                lda     DP_BAS_TXTPTR2+1
                sta     DP_BAS_TMP6+1
                lda     DP_BAS_TXTPTR2+2
                sta     DP_BAS_TMP6+2
                rts

addAtoTMP6:     clc
                adc     DP_BAS_TMP6
                sta     DP_BAS_TMP6
                bcc     @rtsL87A5
                inc     DP_BAS_TMP6+1
                bne     @rtsL87A5
                inc     DP_BAS_TMP6+2
@rtsL87A5:      rts

;*******************************************************************************
;* Label Searching                                                             *
;*******************************************************************************
;*                                                                             *
;* This is tentative but it looks like labels such as                          *
;*                                                                             *
;* .HERE                                                                       *
;* PRINT "A LABEL LOOP"                                                        *
;* GOTO HERE                                                                   *
;*                                                                             *
;* When labels are found they are stored as variables containing the address   *
;* of the line with the label or'd with FF000000                               *
;*                                                                             *
;* It doesn't appear to be possible to have labels in the middle of a line     *
;*******************************************************************************
lineLabelSearch:
                jsr     TMP6eqTXTPTR2
                ldx     #$01
                ldy     DP_BAS_TXTPTR_OFF
                jsr     varScanNameAtTMP6_Y
                cmp     #'%'
                bne     @L87B6
                inx
                iny
@L87B6:         stx     DP_BAS_TMP6+3
                phy
                lda     DP_BAS_TXTPTR_OFF
                jsr     addAtoTMP6
                lda     DP_BAS_PAGE
                sta     DP_FPB_mant
                lda     DP_BAS_PAGE+1
                sta     DP_FPB_mant+1
                lda     DP_BAS_PAGE+2
                sta     DP_BAS_61_UK
                ldy     #$03
                bra     sk1

lblSearchNextLine:
                ldy     #$03
                clc
                lda     [DP_FPB_mant],y
                adc     DP_FPB_mant
                sta     DP_FPB_mant
                bcc     sk1
                inc     DP_FPB_mant+1
                bne     sk1
                inc     DP_BAS_61_UK
sk1:            tyx
                ldy     #$01
                lda     [DP_FPB_mant],y
                bpl     @skContNextLine
                brl     parseCheckOPTNoSuchVar

@skContNextLine:
                txy
@spcLp:         iny
                lda     [DP_FPB_mant],y
                cmp     #' '
                beq     @spcLp
                cmp     #'.'
                bne     lblSearchNextLine
                iny
                sty     DP_BAS_TXTPTR2_OFF
                iny
                tya
                pha
                clc
                adc     DP_FPB_mant
                ldy     DP_FPB_mant+1
                ldx     DP_BAS_61_UK
                bcc     @skC2
                iny
                bne     @skC1
                inx
@skC1:          clc
@skC2:          sbc     #$00
                sta     DP_FPB_sgn
                tya
                sbc     #$00
                sta     DB_BAS_UNK_5A_6+3
                txa
                sbc     #$00
                sta     DP_FPB_exp
                plx
                ldy     #$01
@L881A:         inx
                dey
                lda     [DP_FPB_sgn],y
                cmp     [DP_BAS_TMP6],y
                bne     lblSearchNextLine
                iny
                iny
                cpy     DP_BAS_TMP6+3
                bne     @L881A
                dey
                cmp     #'%'
                beq     @L8848
                lda     [DP_FPB_sgn],y
                cmp     #'%'
                beq     lblSearchNextLine
                cmp     #'('
                beq     lblSearchNextLine
                cmp     #'$'
                beq     lblSearchNextLine
                cmp     #'!'
                beq     lblSearchNextLine
                cmp     #'?'
                beq     lblSearchNextLine
                jsr     checkIsValidVariableNameChar
                bcs     lblSearchNextLine
@L8848:         lda     DP_BAS_TXTPTR2
                sta     DP_FPB_sgn
                lda     DP_BAS_TXTPTR2+1
                sta     DB_BAS_UNK_5A_6+3
                lda     DP_BAS_TXTPTR2+2
                sta     DP_FPB_exp
                lda     DP_FPB_mant
                sta     DP_BAS_TXTPTR2
                lda     DP_FPB_mant+1
                sta     DP_BAS_TXTPTR2+1
                lda     DP_BAS_61_UK
                sta     DP_BAS_TXTPTR2+2
                jsr     findVarOrAllocEmpty
                bcs     @setZ_brk_06_type_mismatch
                lda     DP_BAS_INT_WA+3
                beq     @setZ_brk_06_type_mismatch
                jsr     pushINTWA_on_hw_stack
                lda     DP_BAS_TXTPTR2
                sta     DP_BAS_INT_WA
                lda     DP_BAS_TXTPTR2+1
                sta     DP_BAS_INT_WA+1
                lda     DP_BAS_TXTPTR2+2
                sta     DP_BAS_INT_WA+2
                stz     DP_BAS_INT_WA+3
                lda     #$ff
                sta     DP_BAS_INT_WA+3
                lda     #$04
                sta     DP_BAS_40_VARTYPE
                jsr     storeEvaledExprinStackedVarPTr
                lda     #$40
                ora     DP_BAS_SET_TO_Z_AT_EOS
                sta     DP_BAS_SET_TO_Z_AT_EOS
                lda     DP_FPB_sgn
                sta     DP_BAS_TXTPTR2
                sta     DP_BAS_TXTPTR
                lda     DB_BAS_UNK_5A_6+3
                sta     DP_BAS_TXTPTR+1
                sta     DP_BAS_TXTPTR2+1
                lda     DP_FPB_exp
                sta     DP_BAS_TXTPTR2+2
                sta     DP_BAS_TXTPTR+2
                ply
                sty     DP_BAS_TXTPTR_OFF
                sty     DP_BAS_TXTPTR2_OFF
                lda     DP_BAS_40_VARTYPE
                sta     DP_BAS_INT_WA+3
                rts

@setZ_brk_06_type_mismatch:
                stz     DP_BAS_SET_TO_Z_AT_EOS
                jmp     brk06_type_mismatch

findDefUnstackAndBrkNoSuch:
                pla
                sta     DP_BAS_TXTPTR2+2
                pla
                sta     DP_BAS_TXTPTR2+1
                pla
                sta     DP_BAS_TXTPTR2
        .IFDEF COMMUNICATOR
                ldx     #$1f
                jsr     setPAGETOPfromTXTPTR
        .ENDIF
                brk     $1d

                .byte   "No such "
                .byte   tknFN
                .byte   '/'
                .byte   tknPROC
                .byte   $00

PROCFN_FindDEF:
        .IFDEF COMMUNICATOR
                ldy     #$00                            ;Not sure what this is all about?
setPAGEFromSpecialYFindDEF:
                jsr     setPAGEFromSpecialVarY
                phy
                lda     DP_BAS_PAGE+2
                sta     DP_BAS_TXTPTR2+2
                lda     DP_BAS_PAGE+1
                sta     DP_BAS_TXTPTR2+1
                lda     DP_BAS_PAGE
                sta     DP_BAS_TXTPTR2
progFndDEF_linLp:
                ldy     #$01
                lda     [DP_BAS_TXTPTR2],y
                bpl     @skGo
                ply
                iny
                iny
                iny
                cpy     DP_BAS_SPECIALVAR_SZ
                bne     setPAGEFromSpecialYFindDEF
                brl     retFF_L896C
        .ENDIF
        .IFDEF MOS
                ; TODO: check this works then optimize
                lda     DP_BAS_PAGE+2
                sta     DP_BAS_TXTPTR2+2
                lda     DP_BAS_PAGE+1
                sta     DP_BAS_TXTPTR2+1
                lda     DP_BAS_PAGE
                sta     DP_BAS_TXTPTR2
progFndDEF_linLp:
                ldy     #$01
                lda     [DP_BAS_TXTPTR2],y
                bpl     @skGo
                brl     retFF_L896C              
        .ENDIF

@skGo:          ldy     #$03
@lpSpc:         iny
                lda     [DP_BAS_TXTPTR2],y
                cmp     #' '
                beq     @lpSpc
                cmp     #tknDEF
                beq     progFndDEF_skDefFnd
progFndDEF_skNxtLin:
                ldy     #$03                            ;adds line length byte to TXTPTR and loops
                lda     [DP_BAS_TXTPTR2],y
                clc
                adc     DP_BAS_TXTPTR2
                sta     DP_BAS_TXTPTR2
                bcc     progFndDEF_linLp
                inc     DP_BAS_TXTPTR2+1
                bne     progFndDEF_linLp
                inc     DP_BAS_TXTPTR2+2
                bra     progFndDEF_linLp

progFndDEF_skDefFnd:
                iny
                sty     DP_BAS_TXTPTR2_OFF
                jsr     parse_skip_spaces_PTR2
                tya
                pha
                clc
                adc     DP_BAS_TXTPTR2
                ldy     DP_BAS_TXTPTR2+1
                ldx     DP_BAS_TXTPTR2+2
                bcc     @skC2
                iny
                bne     @skC1
                inx
@skC1:          clc
@skC2:          sbc     #$00
                sta     DP_FPB_sgn
                tya
                sbc     #$00
                sta     DB_BAS_UNK_5A_6+3
                txa
                sbc     #$00
                sta     DP_FPB_exp
                plx
                ldy     #$02
@L8932:         inx
                dey
                lda     [DP_FPB_sgn],y
                cmp     [DP_BAS_TMP6],y
                bne     progFndDEF_skNxtLin
                iny
                iny
                cpy     DP_BAS_TMP6+3
                bne     @L8932
                dey
                lda     [DP_FPB_sgn],y
                jsr     checkIsValidVariableNameChar
                bcs     progFndDEF_skNxtLin
                txa
                tay
                jsr     parse_updPTRfromPTR2_yield
                jsr     L6E84
                ldx     #$01
                jsr     AllocVarSpaceOnHeap
                lda     DP_BAS_TXTPTR2
                sta     [DP_BAS_VARTOP]
                ldy     #$01
                lda     DP_BAS_TXTPTR2+1
                sta     [DP_BAS_VARTOP],y
                iny
                lda     DP_BAS_TXTPTR2+2
                sta     [DP_BAS_VARTOP],y
                iny
                jsr     CheckVarFitsY
        .IFDEF COMMUNICATOR
                pla
        .ENDIF
                lda     #RETV_UK1
                rts

retFF_L896C:    lda     #RETV_STR
                rts

brkBadCall:     brk     $1e

                .byte   "Bad call"
                .byte   $00

exec_FN:        lda     #tknFN
exec_FN_PROC:   sta     DP_BAS_40_VARTYPE
        .IFDEF COMMUNICATOR
                jsr     qrySetPAGETOPSpecial
        .ENDIF
                clc
                tsc
                sbc     DP_stack_save
                xba
                sbc     DP_stack_save+1
                xba
                tax
                clc
                adc     DP_BAS_STACK
                jsr     UpdStackToACyCheckFull
                txa
                eor     #$ff
                tax
                sta     [DP_BAS_STACK]
                ldy     #$00
@L8998:         iny
                phd
                tsc
                tcd
                lda     DP_BAS_00_REAL_UK+2,x
                pld
                sta     [DP_BAS_STACK],y
                dex
                bne     @L8998
                lda     DP_stack_save+1
                xba
                lda     DP_stack_save
                tcs
                lda     DP_BAS_40_VARTYPE
                pha
                lda     DP_BAS_TXTPTR2_OFF
                pha
                lda     DP_BAS_TXTPTR2
                pha
                lda     DP_BAS_TXTPTR2+1
                pha
                lda     DP_BAS_TXTPTR2+2
                pha
                lda     DP_BAS_TXTPTR_OFF
                clc
                adc     DP_BAS_TXTPTR
                ldy     DP_BAS_TXTPTR+1
                ldx     DP_BAS_TXTPTR+2
                bcc     @L89C9
                iny
                bne     @L89C8
                inx
@L89C8:         clc
@L89C9:         sbc     #$01
                sta     DP_BAS_TMP6
                tya
                sbc     #$00
                sta     DP_BAS_TMP6+1
                txa
                sbc     #$00
                sta     DP_BAS_TMP6+2

                ldx     DP_BAS_TXTPTR_OFF
                ldy     #$02
                jsr     varScanNameAtTMP6_Y
                cpy     #$02
                beq     brkBadCall
                stx     DP_BAS_TXTPTR_OFF
                jsr     varFindPROCFN
                bne     @L89F1
                jsr     PROCFN_FindDEF
                bne     @L8A01
                brl     findDefUnstackAndBrkNoSuch

@L89F1:         lda     [DP_BAS_INT_WA]
                sta     DP_BAS_TXTPTR2
                ldy     #$01
                lda     [DP_BAS_INT_WA],y
                sta     DP_BAS_TXTPTR2+1
                iny
                lda     [DP_BAS_INT_WA],y
                sta     DP_BAS_TXTPTR2+2
                dey
@L8A01:         
        .IFDEF COMMUNICATOR
                ldx     #DP_BAS_TXTPTR2
                jsr     setPAGETOPfromTXTPTR
        .ENDIF
                lda     #$00
                pha
                stz     DP_BAS_TXTPTR2_OFF
                jsr     parse_skip_spaces_PTR2
                cmp     #'('
                beq     doFNPROCargumentsEntry
                dec     DP_BAS_TXTPTR2_OFF
L8A14:          lda     DP_BAS_TXTPTR_OFF
                pha
                lda     DP_BAS_TXTPTR
                pha
                lda     DP_BAS_TXTPTR+1
                pha
                lda     DP_BAS_TXTPTR+2
                pha
                jsr     skipSpacesExecImmed
                pla
                sta     DP_BAS_TXTPTR+2
                pla
                sta     DP_BAS_TXTPTR+1
                pla
                sta     DP_BAS_TXTPTR
                pla
                sta     DP_BAS_TXTPTR_OFF
                pla
                beq     doFnProcExit_NoParams
                sta     DP_FPB_mant+3
@L8A34:         jsr     stack_copy6_to_DP_BAS_TMP66_uns4
                jsr     L98B8
                dec     DP_FPB_mant+3
                bne     @L8A34
doFnProcExit_NoParams:
                pla
                sta     DP_BAS_TXTPTR2+2
                pla
                sta     DP_BAS_TXTPTR2+1
                pla
                sta     DP_BAS_TXTPTR2
        .IFDEF COMMUNICATOR
                ldx     #DP_BAS_TXTPTR2
                jsr     setPAGETOPfromTXTPTR
        .ENDIF
                pla
                sta     DP_BAS_TXTPTR2_OFF
                pla
                lda     [DP_BAS_STACK]
                pha
                tax
                sec
                lda     DP_stack_save
                sbc     $01,S
                xba
                pla
                lda     DP_stack_save+1
                sbc     #$00
                xba
                tcs
                ldy     #$00
@lp:            iny
                lda     [DP_BAS_STACK],y
                phd
                pha
                tsc
                tcd
                pla
                sta     $03,x
                pld
                dex
                bne     @lp
                tya
                adc     DP_BAS_STACK
                sta     DP_BAS_STACK
                bcc     @sk
                inc     DP_BAS_STACK+1
                bne     @sk
                inc     DP_BAS_STACK+2
@sk:            lda     DP_BAS_40_VARTYPE
                rts

doFNPROCargumentsEntry:
                lda     DP_BAS_TXTPTR_OFF
                pha
                lda     DP_BAS_TXTPTR
                pha
                lda     DP_BAS_TXTPTR+1
                pha
                lda     DP_BAS_TXTPTR+2
                pha
                jsr     findVarOrAllocEmpty
                beq     @L8AEA
                lda     DP_BAS_TXTPTR_OFF
                sta     DP_BAS_TXTPTR2_OFF
                pla
                sta     DP_BAS_TXTPTR+2
                pla
                sta     DP_BAS_TXTPTR+1
                pla
                sta     DP_BAS_TXTPTR
                pla
                sta     DP_BAS_TXTPTR_OFF
                plx
                lda     DP_BAS_INT_WA+3
                pha
                lda     DP_BAS_INT_WA+2
                pha
                lda     DP_BAS_INT_WA+1
                pha
                lda     DP_BAS_INT_WA
                pha
                inx
                phx
                jsr     localVarAtIntA
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                beq     doFNPROCargumentsEntry
                cmp     #')'
                bne     @L8AEA
                lda     #$00
                pha
                jsr     parse_skip_spaces
                cmp     #'('
                bne     @L8AEA
@L8AC7:         
                jsr     evalAtOFF
                jsr     stack_INTorREAL
                lda     DP_BAS_40_VARTYPE
                sta     DP_BAS_INT_WA
                jsr     stack_INTWA
                plx
                inx
                phx
                jsr     parse_skip_space_CMPcomma
                beq     @L8AC7                
                cmp     #')'
                bne     @L8AEA
                pla
                pla
                sta     DP_BAS_6F_UK
                sta     DP_BAS_70_UK
                cpx     DP_BAS_6F_UK
                beq     @L8B0F
@L8AEA:         sec
                lda     DP_stack_save
                sbc     #$05
                pha
                lda     DP_stack_save+1
                sbc     #$00
                xba
                pla
                tcs
                pla
                sta     DP_BAS_TXTPTR2+2
                pla
                sta     DP_BAS_TXTPTR2+1
                pla
                sta     DP_BAS_TXTPTR2
                pla
                sta     DP_BAS_TXTPTR2_OFF
                brk     $1f

                .asciiz "Arguments"

@L8B0F:         jsr     popIntA
                lda     DP_BAS_INT_WA
                sta     DP_BAS_40_VARTYPE
                pla
                sta     DP_BAS_INT_WA
                pla
                sta     DP_BAS_INT_WA+1
                pla
                sta     DP_BAS_INT_WA+2
                pla
                sta     DP_BAS_INT_WA+3
                bmi     @L8B41
                lda     DP_BAS_40_VARTYPE
                beq     @L8AEA
                ldx     #DP_BAS_TMP6
                jsr     storeWAatDPX
                lda     DP_BAS_40_VARTYPE
                bpl     @L8B39
                jsr     popFPFromStackToPTR1
                jsr     fpCopyPTR1toFPA
                bra     @L8B3C

@L8B39:         jsr     popIntA
@L8B3C:         jsr     storeEvaledExprinVarAtTMP6ptr
                bra     @L8B4B

@L8B41:         lda     DP_BAS_40_VARTYPE
                bne     @L8AEA
                jsr     popStackedString
                jsr     copyStringToVar2
@L8B4B:         dec     DP_BAS_6F_UK
                bne     @L8B0F
                lda     DP_BAS_70_UK
                pha
                jmp     L8A14

localVarAtIntA: ldy     DP_BAS_INT_WA+3
                cpy     #$05
                bcs     @sk
                ldx     #DP_BAS_TMP6
                jsr     storeWAatDPX
@sk:            jsr     GetVarVal
                php
                jsr     stack_INTorREAL
                plp
                beq     @sk2
                bmi     @sk2
                ldx     #DP_BAS_TMP6
                jsr     copyDPXtoWA
@sk2:           jmp     stack_INTWA

GetVarVal:      ldy     DP_BAS_INT_WA+3
                bmi     GetVarValStr
                beq     storeWAbyte
                cpy     #$05
                beq     GetVarValReal
GetVarValInt:   ldy     #$03
                lda     [DP_BAS_INT_WA],y
                sta     DP_BAS_INT_WA+3
                dey
                lda     [DP_BAS_INT_WA],y
                xba
                dey
                lda     [DP_BAS_INT_WA],y
                tax
                dey
                lda     [DP_BAS_INT_WA],y
                sta     DP_BAS_INT_WA
                stx     DP_BAS_INT_WA+1
                xba
                sta     DP_BAS_INT_WA+2
                lda     #RETV_INT
                rts

storeWAbyte:    lda     [DP_BAS_INT_WA],y
                jmp     retAY_16bit_INT

GetVarValReal:  stz     DP_FPA_mant+4
                stz     DP_FPA_exp
                dey
                lda     [DP_BAS_INT_WA],y
                sta     DP_FPA_mant+3
                dey
                lda     [DP_BAS_INT_WA],y
                sta     DP_FPA_mant+2
                dey
                lda     [DP_BAS_INT_WA],y
                sta     DP_FPA_mant+1
                dey
                lda     [DP_BAS_INT_WA],y
                sta     DP_FPA_sgn
                tay
                lda     [DP_BAS_INT_WA]
                sta     DP_FPA_exp+1
                bne     @sk1
                tya
                ora     DP_FPA_mant+1
                ora     DP_FPA_mant+2
                ora     DP_FPA_mant+3
                beq     @sk2
@sk1:           tya
                ora     #$80
@sk2:           sta     DP_FPA_mant
                lda     #RETV_REAL
                rts

GetVarValStr:   cpy     #$80
                beq     GetVarValStr_Ind
                ldy     #$04
                lda     [DP_BAS_INT_WA],y
                sta     DP_BAS_STRLEN
                beq     @rtsL8BF3
                ldy     #$02
                lda     [DP_BAS_INT_WA],y
                sta     DP_BAS_TMP6+2
                dey
                lda     [DP_BAS_INT_WA],y
                sta     DP_BAS_TMP6+1
                lda     [DP_BAS_INT_WA]
                sta     DP_BAS_TMP6
                ldy     DP_BAS_STRLEN
@lp:            dey
                lda     [DP_BAS_TMP6],y
                sta     [DP_BAS_STRWKSP_L],y
                tya
                bne     @lp
@rtsL8BF3:      rts

GetVarValStr_Ind:
                lda     DP_BAS_INT_WA+1
                ora     DP_BAS_INT_WA+2
                beq     ret_INT_WA_lo_as_1ch_str
                ldy     #$00
@lp1:           lda     [DP_BAS_INT_WA],y
                bne     @sk1
                lda     #$0d
@sk1:           sta     [DP_BAS_STRWKSP_L],y
                eor     #$0d
                beq     @sk2
                iny
                bne     @lp1
                tya
@sk2:           sty     DP_BAS_STRLEN
                rts

exec_CHR:       jsr     evalLevel1checkTypeStoreAsINT
ret_INT_WA_lo_as_1ch_str:
                lda     DP_BAS_INT_WA
                jmp     retA_as_1ch_str

HandleBRKfindERL:
                ldy     DP_BAS_TXTPTR2_OFF
                beq     @sk2
                dey
@sk2:           jsr     parse_updPTRfromPTR2_yield
                stz     DP_BAS_ERL
                stz     DP_BAS_ERL+1
                ldx     DP_BAS_PAGE+2
                stx     DP_BAS_TMP6+2
                ldx     DP_BAS_PAGE+1
                stx     DP_BAS_TMP6+1
                ldx     DP_BAS_PAGE
                stx     DP_BAS_TMP6
                lda     DP_BAS_TXTPTR2+1
                tay
                cmp     DP_BAS_MEMBASE+1
                bne     @sk
                lda     DP_BAS_TXTPTR2+2
                cmp     DP_BAS_MEMBASE+2
                beq     @L8C6Crts
@sk:            ldx     DP_BAS_TXTPTR2
@lp:            jsr     getAatTMP6ptrAndInc
                cmp     #$0d
                bne     @findErlsk2
                cpx     DP_BAS_TMP6
                tya
                sbc     DP_BAS_TMP6+1
                lda     DP_BAS_TXTPTR2+2
                sbc     DP_BAS_TMP6+2
                bcc     @L8C6Crts
                jsr     getAatTMP6ptrAndInc
                ora     #$00
                bmi     @clearERLrts
                sta     DP_BAS_ERL+1
                jsr     getAatTMP6ptrAndInc
                sta     DP_BAS_ERL
                jsr     getAatTMP6ptrAndInc
@findErlsk2:    cpx     DP_BAS_TMP6
                tya
                sbc     DP_BAS_TMP6+1
                lda     DP_BAS_TXTPTR2+2
                sbc     DP_BAS_TMP6+2
                bcs     @lp
@L8C6Crts:      rts

@clearERLrts:   stz     DP_BAS_ERL
                stz     DP_BAS_ERL+1
                rts

        .IFDEF MOS
                ; A BRK has happened in emulation mode (probably OSCLI?)
                ; BRK vector in emu mode stack will contain:
                ;               + 3             PCH
                ;               + 2             PCL
                ;               + 1             Native mode flags
                ; we will be in emulation mode and the error bank is always assumed to be 0                
BRK_HANDLER_EMU:
                ; we will munge the stack and pretend a native mode BRK
                pha             ; space byte to move down into
                php
                pha

                lda     4,S
                sta     3,S
                lda     5,S
                sta     4,S
                lda     6,S
                sta     5,S
                lda     #0
                sta     6,S
                pla
                clc
                xce
                plp
                bra     BRK_HANDLER

        .ENDIF

BRK_HANDLER:    
        .IFDEF COMMUNICATOR
                cli
                phk
                plb
                sep     #$30
                lda     #$60
                trb     DP_BAS_CO_FLAGS
                stz     DP_BAS_SET_TO_Z_AT_EOS
                lda     DP_stack_save+1
                xba
                lda     DP_stack_save
                tcs
                ldx     #$ff
                stx     DP_BAS_OPT
                inx
                ldy     #$00
                lda     #OSBYTE_222_RW_VDUQLEN
                jsr     call_OSBYTE
                lda     #OSBYTE_126_ESCAPE_ACK
                jsr     call_OSBYTE
                lda     #$03
                cop     COP_16_OPAEV
                jsr     HandleBRKfindERL
                stz     DP_BAS_TRACEFLAG
                phx
                ldx     #$14
                jsl     _ST
                plx
                eor     #$00
                bcc     @sk
                jsr     exec_ERROR_OFF_reset_ERRORPTR
@sk:            
        .ENDIF
        .IFDEF MOS
                cli

                ;TODO remove
                jsr     ShowRegs
                jsr     StackTrace
                ;TODO remove^^^

                sep     #$30

                pea     MOS_BASIC_DP
                pld


        .IFDEF DOSSY
                ; the TUBE host will have set the error pointer
                ; in its own direct page (0FC:0FE:0FD) variables use that
                ; for now
                ; 
                lda     f:$0000FC
                sta     DP_BAS_BL_ERRPTR+2
                lda     f:$0000FD
                sta     DP_BAS_BL_ERRPTR+0
                lda     f:$0000FE
                sta     DP_BAS_BL_ERRPTR+1

        .ELSE

                ; BRK vector in native mode stack will contain:
                ;               + 4             Program bank
                ;               + 3             PCH
                ;               + 2             PCL
                ;               + 1             Native mode flags

                pla                             ; ignore flags

                sec
                pla                             ; pointer-1
                sbc     #1
                sta     DP_BAS_BL_ERRPTR
                pla
                sbc     #0
                sta     DP_BAS_BL_ERRPTR+1
                sbc     #0
                pla
                sta     DP_BAS_BL_ERRPTR+2


        .ENDIF
                lda     [DP_BAS_BL_ERRPTR]
                sta     DP_BAS_BL_ERRNO

                ; put pointer back!
                inc     DP_BAS_BL_ERRPTR
                bne     @skinc
                inc     DP_BAS_BL_ERRPTR+1
                bne     @skinc
                inc     DP_BAS_BL_ERRPTR
@skinc:



                ;reset data bank
                phk
                plb

                stz     DP_BAS_SET_TO_Z_AT_EOS
                lda     DP_stack_save+1
                xba
                lda     DP_stack_save
                tcs
                ldx     #$ff
                stx     DP_BAS_OPT
                inx
                ldy     #$00
                lda     #OSBYTE_222_RW_VDUQLEN
                jsr     call_OSBYTE
                lda     #OSBYTE_126_ESCAPE_ACK
                jsr     call_OSBYTE


                jsr     HandleBRKfindERL
                stz     DP_BAS_TRACEFLAG


        .ENDIF
                lda     DP_BAS_ONERRORPTR
                sta     DP_BAS_TXTPTR2
                lda     DP_BAS_ONERRORPTR+1
                sta     DP_BAS_TXTPTR2+1
                lda     DP_BAS_ONERRORPTR+2
                sta     DP_BAS_TXTPTR2+2
        .IFDEF COMMUNICATOR
                ldx     #$1f                            ;; TODO: pointer into unknown area
                jsr     setPAGETOPfromTXTPTR
        .ENDIF
                stz     DP_BAS_TXTPTR2_OFF
                jsr     ResetStackProgStartRepGosFor
                jmp     skipSpacesExecImmed

exec_ERROR_OFF_reset_ERRORPTR:
                lda     #<basDefaultErrorHandle
                sta     DP_BAS_ONERRORPTR
                lda     #>basDefaultErrorHandle
                sta     DP_BAS_ONERRORPTR+1
                phk
                pla
                sta     DP_BAS_ONERRORPTR+2
                rts

ONERROROFF:     phk                                     ;; TODO: this assumes K<>here for BASIC which might not be true on blitter!?
                pla
                cmp     DP_BAS_ONERRORPTR+2
                bne     @rts
                lda     #>basDefaultErrorHandle
                cmp     DP_BAS_ONERRORPTR+1
                bne     @rts
                lda     #<basDefaultErrorHandle
                cmp     DP_BAS_ONERRORPTR
@rts:           rts

basDefaultErrorHandle:
                .byte   tknREPORT                       ;REPORT
                .byte   ':'
                .byte   tknIF                           ;IF
                .byte   tknERL                          ;ERL
                .byte   tknPRINT                        ;PRINT
                .byte   $22," at line ",$22,";"
                .byte   tknERL                          ;ERL
                .byte   ':'
                .byte   tknEND                          ;END
                .byte   tknELSE                         ;ELSE
                .byte   tknPRINT                        ;PRINT
                .byte   ':'
                .byte   tknEND                          ;END
                .byte   $0d

exec_SOUND:     jsr     evalForceINT
                ldx     #$03
@parmlp:        lda     DP_BAS_INT_WA
                pha
                lda     DP_BAS_INT_WA+1
                pha
                phx
                jsr     ckCommaThnEvalAtYckTypACnv2INT
                plx
                dex
                bne     @parmlp
                jsr     parse_nextstmt_yield_PTR2_OFF
                lda     DP_BAS_INT_WA
                pha
                lda     DP_BAS_INT_WA+1
                pha
                ldx     #$07
                ldy     #OSWORD_7_SOUND
                bra     sndPullBthenAtoTMP6ptr_OSWORD_A

exec_ENVELOPE:  jsr     evalForceINT
                ldx     #$0d
@parmlp:        lda     DP_BAS_INT_WA
                pha
                phx
                jsr     ckCommaThnEvalAtYckTypACnv2INT
                plx
                dex
                bne     @parmlp
                jsr     parse_nextstmt_yield_PTR2_OFF
                lda     DP_BAS_INT_WA
                pha
                ldx     #$0d
                ldy     #OSWORD_8_ENVELOPE
sndPullBthenAtoTMP6ptr_OSWORD_A:
                pla
                sta     DP_BAS_TMP6,x
                dex
                bpl     sndPullBthenAtoTMP6ptr_OSWORD_A
                tya
                ldx     #DP_BAS_TMP6
                ldy     #>DP_BAS_TMP6
OSWORD_continue:
        .IFDEF COMMUNICATOR
                jsr     cop_OSWORD
        .ENDIF
        .IFDEF MOS
                jsr     MOS_OSWORD
        .ENDIF
                bra     jmp_Continue

exec_WIDTH:     jsr     evalForceINT
                jsr     parse_nextstmt_yield_PTR2_OFF
                ldy     DP_BAS_INT_WA
                dey
                sty     DP_BAS_WIDTH
jmp_Continue:   jmp     continue

jmp_brk06_type_mismatch4:
                jmp     brk06_type_mismatch

evalAtYStoreEvaldinStackedVarPTr:
                jsr     evalAtOFF
storeEvaledExprinStackedVarPTr:
                ply
                plx
                pla
                sta     DP_BAS_TMP6+3
                pla
                sta     DP_BAS_TMP6+2
                pla
                sta     DP_BAS_TMP6+1
                pla
                sta     DP_BAS_TMP6
                phx
                phy
storeEvaledExprinVarAtTMP6ptr:
                lda     DP_BAS_TMP6+3
                cmp     #$05
                beq     stEvaledExprinRealVarAtTMP6ptr
                lda     DP_BAS_40_VARTYPE
                beq     jmp_brk06_type_mismatch4
                bpl     storeINT
                jsr     eval_real2INT
storeINT:       lda     DP_BAS_INT_WA
                ldy     #$00
                sta     [DP_BAS_TMP6],y
                lda     DP_BAS_TMP6+3
                beq     rtsL8D94
                lda     DP_BAS_INT_WA+1
                iny
                sta     [DP_BAS_TMP6],y
                lda     DP_BAS_INT_WA+2
                iny
                sta     [DP_BAS_TMP6],y
                lda     DP_BAS_INT_WA+3
                iny
                sta     [DP_BAS_TMP6],y
rtsL8D94:       rts

stEvaledExprinRealVarAtTMP6ptr:
                lda     DP_BAS_40_VARTYPE
                beq     jmp_brk06_type_mismatch4
                bmi     fpCopyFPAtoTMP6ptr
                jsr     braIntToReal
fpCopyFPAtoTMP6ptr:
                lda     DP_FPA_exp+1
                sta     [DP_BAS_TMP6]
                ldy     #$01
                lda     DP_FPA_sgn
                eor     DP_FPA_mant
                and     #$80
                eor     DP_FPA_mant
                sta     [DP_BAS_TMP6],y
                iny
                lda     DP_FPA_mant+1
                sta     [DP_BAS_TMP6],y
                iny
                lda     DP_FPA_mant+2
                sta     [DP_BAS_TMP6],y
                iny
                lda     DP_FPA_mant+3
                sta     [DP_BAS_TMP6],y
                rts

exec_EDIT:      brk     $00

                .asciiz "Not available"

doLIST:         stz     DB_BAS_UNK_5A_6+1
                stz     DP_FPB_sgn
                jsr     exec_FALSE
                jsr     parse_lineno_atOFF
                php
                jsr     stack_INTWA
                jsr     exec_TRUE
                lsr     DP_BAS_INT_WA+1
                plp
                bcc     doListSkNoLineSpec
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                beq     doListSkLineSpec2
                jsr     popIntA
                jsr     stack_INTWA
                dec     DP_BAS_TXTPTR2_OFF
                bra     doListSkStart

doListSkNoLineSpec:
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                beq     doListSkLineSpec2
                dec     DP_BAS_TXTPTR2_OFF
doListSkLineSpec2:
                jsr     parse_lineno_atOFF
doListSkStart:  ldx     #DP_FPA_mant
                jsr     storeWAatDPX
                jsr     parse_skip_spaces_PTR2
                cmp     #tknIF
                bne     doListSkStart2
                jsr     parse_skip_spaces_PTR2
                jsr     parse_updPTRfromPTR2_yield
                bra     doListSkStart3

exec_LIST:      iny
                lda     [DP_BAS_TXTPTR2],y
                cmp     #'O'                            ;Check for 'O'
                bne     doLIST                          ;go and do "LIST"
                inc     DP_BAS_TXTPTR2_OFF              ;Skip 'O'
; Do LISTO
                jsr     evalForceINT
                jsr     parse_nextstmt_yield_TXTOFF2
                lda     DP_BAS_INT_WA
                sta     DP_BAS_LISTO
                jmp     reset_prog_prompt

doListSkStart2: jsr     parsecheckEOSTXTPTReqPTR2_yield
doListSkStart3: lda     DP_BAS_TXTPTR2
                sta     DP_BAS_TXTPTR
                jsr     findTOP
                jsr     popIntA
                jsr     prog_search_lineno
                lda     DP_FPB_exp
                sta     DP_BAS_TXTPTR2
                lda     DP_FPB_mant
                sta     DP_BAS_TXTPTR2+1
                lda     DP_FPB_mant+1
                sta     DP_BAS_TXTPTR2+2
                bcs     doListSkGotCorrectLine
                dey
                bra     doListSkGotNearestLine

doListPrintLFThenLoop:
                jsr     list_printA
                bit     DP_BAS_LISTO
                bmi     doListLoop
                lda     #$0a
                jsr     call_OSWRCH
doListLoop:     jsr     parse_updPTRfromPTR2_yield
doListSkGotNearestLine:
                lda     [DP_BAS_TXTPTR2],y
                sta     DP_BAS_INT_WA+1
                iny
                lda     [DP_BAS_TXTPTR2],y
                sta     DP_BAS_INT_WA
doListSkGotCorrectLine:
                lda     DP_BAS_INT_WA
                clc
                sbc     DP_FPA_mant
                lda     DP_BAS_INT_WA+1
                sbc     DP_FPA_mant+1
                bcc     @doListStartLine
                brl     reset_prog_prompt

@doListStartLine:
                stz     DP_BAS_6F_UK
                stz     DP_BAS_70_UK
                ldy     #$04
                sty     DP_BAS_TXTPTR2_OFF
                sty     DP_BAS_TXTPTR_OFF
                bit     DB_BAS_UNK_5A_6+1
                bpl     @sk0
                stz     DB_BAS_UNK_5A_6+1
@sk0:           bit     DP_FPB_sgn
                bpl     doListScanLoop
                stz     DP_FPB_sgn
doListScanLoop: lda     [DP_BAS_TXTPTR2],y
                cmp     #$0d
                beq     doListScanDone
                cmp     #tknREM
                beq     @doListSkREM
                cmp     #'"'
                bne     @doListSkREMQuot
                eor     DP_BAS_6F_UK
@doListSkREM:   sta     DP_BAS_6F_UK
@doListSkREMQuot:
                ldx     DP_BAS_6F_UK
                bne     @doListSkUntil
                cmp     #tknNEXT
                bne     @doListSkNext
                dec     DB_BAS_UNK_5A_6+1
@doListSkNext:  cmp     #tknUNTIL
                bne     @doListSkUntil
                dec     DP_FPB_sgn
@doListSkUntil: ldx     DP_BAS_TXTPTR
@doListLp_Uk1:  phy
                txy
                lda     [DP_BAS_MEMBASE],y
                ply
                cmp     #$0d
                beq     doListSk_Uk1
                cmp     [DP_BAS_TXTPTR2],y
                bne     doListSk_Uk2
                iny
                inx
                bra     @doListLp_Uk1

bradoListPrintLFThenLoop:
                bra     doListPrintLFThenLoop

doListSk_Uk1:   sta     DP_BAS_70_UK
doListSk_Uk2:   inc     DP_BAS_TXTPTR_OFF
                ldy     DP_BAS_TXTPTR_OFF
                bra     doListScanLoop

doListScanDone: lda     DP_BAS_70_UK
                beq     doListLoop
                jsr     int16print_fmt5
                lda     #$01
                inx
                sec
                jsr     doLISTOSpaces
                ldx     DB_BAS_UNK_5A_6+1
                lda     #$02
                jsr     doLISTOSpacesCLC
                ldx     DP_FPB_sgn
                lda     #$04
                jsr     doLISTOSpacesCLC
                stz     DP_BAS_6F_UK
@doListNextTok2:
                ldy     DP_BAS_TXTPTR2_OFF
@doListNextTok: lda     [DP_BAS_TXTPTR2],y
                cmp     #$0d
                beq     bradoListPrintLFThenLoop
                cmp     #'"'
                bne     @doListSkNotQuot2
                eor     DP_BAS_6F_UK
                sta     DP_BAS_6F_UK
                lda     #'"'
@doListQuoteLp2:
                jsr     list_printA
                iny
                bra     @doListNextTok

@doListSkNotQuot2:
                ldx     DP_BAS_6F_UK
                bne     @doListQuoteLp2
                cmp     #tknLineNo
                bne     @doList_sknotLineNo
                jsr     parse_lineno_atPTR2_Y
                sty     DP_BAS_TXTPTR2_OFF
                jsr     int16print_AnyLen
                bra     @doListNextTok2

@doList_sknotLineNo:
                cmp     #tknFOR
                bne     @doList_sknotFOR
                inc     DB_BAS_UNK_5A_6+1
@doList_sknotFOR:
                cmp     #tknREPEAT
                bne     @doList_sknotREPEAT
                inc     DP_FPB_sgn
@doList_sknotREPEAT:
                cmp     #tknREM
                bne     @doList_sknotREM
                sta     DP_BAS_6F_UK
@doList_sknotREM:
                jsr     doListPrintTokenA
                iny
                bra     @doListNextTok

exec_NEXT:      jsr     findVarAtPTR2
                bne     cmdNextSkSpecdLoopVar
                ldx     DP_BAS_FOR_LVL
                beq     brk_20_NoFOR
                bcs     cmdNEXTTopLoopVar
jmp_brk10_Syntax2:
                jmp     brk10_Syntax

cmdNextSkSpecdLoopVar:
                bcs     jmp_brk10_Syntax2
                ldx     DP_BAS_FOR_LVL
                beq     brk_20_NoFOR
@lp:            ldy     #$00
                lda     DP_BAS_INT_WA
                cmp     [DP_BAS_FOR_STACKBASE],y
                bne     @cmdNEXTstacklpsk1
                lda     DP_BAS_INT_WA+1
                iny
                cmp     [DP_BAS_FOR_STACKBASE],y
                bne     @cmdNEXTstacklpsk1
                iny
                lda     DP_BAS_INT_WA+2
                cmp     [DP_BAS_FOR_STACKBASE],y
                bne     @cmdNEXTstacklpsk1
                lda     DP_BAS_INT_WA+3
                iny
                cmp     [DP_BAS_FOR_STACKBASE],y
                beq     cmdNEXTTopLoopVar
@cmdNEXTstacklpsk1:
                jsr     STACKBASEsub11
                bne     @lp
                brk     $21

                .byte   "Can't match "
                .byte   tknFOR

brk_20_NoFOR:   brk     $20

                .byte   "No "
                .byte   tknFOR
                .byte   $00

cmdNEXTTopLoopVar:
                ldy     #$00
                lda     [DP_BAS_FOR_STACKBASE],y
                sta     DP_BAS_INT_WA
                iny
                lda     [DP_BAS_FOR_STACKBASE],y
                sta     DP_BAS_INT_WA+1
                iny
                lda     [DP_BAS_FOR_STACKBASE],y
                sta     DP_BAS_INT_WA+2
                iny
                lda     [DP_BAS_FOR_STACKBASE],y
                tay
                cpy     #$05
                bne     @skNotReal
                brl     @cmdNEXTdoREAL

@skNotReal:     ldy     #$04
                lda     [DP_BAS_INT_WA]
                adc     [DP_BAS_FOR_STACKBASE],y
                sta     [DP_BAS_INT_WA]
                sta     DP_BAS_TMP6
                ldx     #$01
                txy
                lda     [DP_BAS_INT_WA],y
                ldy     #$05
                adc     [DP_BAS_FOR_STACKBASE],y
                txy
                sta     [DP_BAS_INT_WA],y
                sta     DP_BAS_TMP6+1
                inx
                txy
                lda     [DP_BAS_INT_WA],y
                ldy     #$06
                adc     [DP_BAS_FOR_STACKBASE],y
                txy
                sta     [DP_BAS_INT_WA],y
                sta     DP_BAS_TMP6+2
                inx
                txy
                lda     [DP_BAS_INT_WA],y
                ldy     #$07
                adc     [DP_BAS_FOR_STACKBASE],y
                txy
                sta     [DP_BAS_INT_WA],y
                tax
                ldy     #$09
                lda     DP_BAS_TMP6
                sec
                sbc     [DP_BAS_FOR_STACKBASE],y
                sta     DP_BAS_TMP6
                iny
                lda     DP_BAS_TMP6+1
                sbc     [DP_BAS_FOR_STACKBASE],y
                tsb     DP_BAS_TMP6
                lda     DP_BAS_TMP6+2
                iny
                sbc     [DP_BAS_FOR_STACKBASE],y
                tsb     DP_BAS_TMP6
                txa
                iny
                sbc     [DP_BAS_FOR_STACKBASE],y
                ora     DP_BAS_TMP6
                beq     @cmpNEXTexecLoop
                txa
                ldy     #$07
                eor     [DP_BAS_FOR_STACKBASE],y
                ldy     #$0c
                eor     [DP_BAS_FOR_STACKBASE],y
                bpl     @sk
                bcs     @cmpNEXTexecLoop
                bra     @cmdNEXTloopFinished

@sk:            bcs     @cmdNEXTloopFinished
@cmpNEXTexecLoop:
                ldy     #$0e
                lda     [DP_BAS_FOR_STACKBASE],y
                sta     DP_BAS_TXTPTR2
                iny
                lda     [DP_BAS_FOR_STACKBASE],y
                sta     DP_BAS_TXTPTR2+1
                iny
                lda     [DP_BAS_FOR_STACKBASE],y
                sta     DP_BAS_TXTPTR2+2
                jsr     checkForESC
                jmp     skipSpacesExecImmed

@cmdNEXTloopFinished:
                jsr     STACKBASEsub11
                ldy     DP_BAS_TXTPTR_OFF
                sty     DP_BAS_TXTPTR2_OFF
                jsr     parse_SkipSpacesPTR2_cmp_COMMA
                bne     @braDecOff2ScanNextCont
                jmp     exec_NEXT

@cmdNEXTdoREAL: jsr     GetVarValReal
                lda     #$04
                jsr     FP_PTR1EqSTACKBASEplusA
                jsr     fpFPAeqPTR1addFPA
                lda     DP_BAS_INT_WA
                sta     DP_BAS_TMP6
                lda     DP_BAS_INT_WA+1
                sta     DP_BAS_TMP6+1
                lda     DP_BAS_INT_WA+2
                sta     DP_BAS_TMP6+2
                jsr     fpCopyFPAtoTMP6ptr
                lda     #$09
                jsr     FP_PTR1EqSTACKBASEplusA
                jsr     evalDoCompareRealFPAwithPTR1
                beq     @cmpNEXTexecLoop
                ldy     #$05
                lda     [DP_BAS_FOR_STACKBASE],y
                bmi     @sk1
                bcs     @cmpNEXTexecLoop
                bra     @cmdNEXTloopFinished

@sk1:           bcc     @cmpNEXTexecLoop
                bra     @cmdNEXTloopFinished

@braDecOff2ScanNextCont:
                jmp     decOff2scanNextContinue

brk_22_FOR_variable:
                brk     $22

                .byte   tknFOR
                .byte   " variable"

brk23_TooManyFORs:
                brk     $23

                .byte   "Too many "
                .byte   tknFOR
                .byte   's'

brk24_NoTO:     brk     $24

                .byte   "No "
                .byte   tknTO
                .byte   $00

exec_FOR:       jsr     findVarOrAllocEmpty
                beq     brk_22_FOR_variable
                bcs     brk_22_FOR_variable
                jsr     pushINTWA_on_hw_stack
                jsr     parseEqualsOrMistake
                jsr     evalAtYStoreEvaldinStackedVarPTr
                jsr     parse_skip_spaces
                cmp     #tknTO
                bne     brk24_NoTO
                ldy     DP_BAS_FOR_LVL
                cpy     #$0a
                bcs     brk23_TooManyFORs
                jsr     STACKBASEplus11
                lda     DP_BAS_TMP6
                ldy     #$00
                sta     [DP_BAS_FOR_STACKBASE],y
                lda     DP_BAS_TMP6+1
                iny
                sta     [DP_BAS_FOR_STACKBASE],y
                lda     DP_BAS_TMP6+2
                iny
                sta     [DP_BAS_FOR_STACKBASE],y
                lda     DP_BAS_TMP6+3
                iny
                sta     [DP_BAS_FOR_STACKBASE],y
                cmp     #$05
                beq     cmdFORskipskReal
                jsr     evalAtYcheckTypeInAConvert2INT
                ldy     #$09
                lda     DP_BAS_INT_WA
                sta     [DP_BAS_FOR_STACKBASE],y
                lda     DP_BAS_INT_WA+1
                iny
                sta     [DP_BAS_FOR_STACKBASE],y
                lda     DP_BAS_INT_WA+2
                iny
                sta     [DP_BAS_FOR_STACKBASE],y
                lda     DP_BAS_INT_WA+3
                iny
                sta     [DP_BAS_FOR_STACKBASE],y
                lda     #$01
                jsr     retA_8bit_INT
                jsr     parse_skip_spaces
                cmp     #tknSTEP
                bne     cmdFORskINTnoSTEP
                jsr     evalAtYcheckTypeInAConvert2INT
                ldy     DP_BAS_TXTPTR_OFF
cmdFORskINTnoSTEP:
                sty     DP_BAS_TXTPTR2_OFF
                ldy     #$04
                lda     DP_BAS_INT_WA
                sta     [DP_BAS_FOR_STACKBASE],y
                lda     DP_BAS_INT_WA+1
                iny
                sta     [DP_BAS_FOR_STACKBASE],y
                lda     DP_BAS_INT_WA+2
                iny
                sta     [DP_BAS_FOR_STACKBASE],y
                lda     DP_BAS_INT_WA+3
                iny
                sta     [DP_BAS_FOR_STACKBASE],y
cmdFORskipExecBody:
                jsr     scanNextStmtAndTrace
                ldy     #$0e
                lda     DP_BAS_TXTPTR2
                sta     [DP_BAS_FOR_STACKBASE],y
                lda     DP_BAS_TXTPTR2+1
                iny
                sta     [DP_BAS_FOR_STACKBASE],y
                iny
                lda     DP_BAS_TXTPTR2+2
                sta     [DP_BAS_FOR_STACKBASE],y
                jmp     skipSpacesExecImmed

cmdFORskipskReal:
                jsr     evalAtOFF
                jsr     ckTypeIntToReal
                lda     #$09
                jsr     FP_PTR1EqSTACKBASEplusA
                jsr     fpCopyFPAtoPTR1
                jsr     fpLoad1
                jsr     parse_skip_spaces
                cmp     #tknSTEP
                bne     cmdFORrealNoStep
                jsr     evalAtOFF
                jsr     ckTypeIntToReal
                ldy     DP_BAS_TXTPTR_OFF
cmdFORrealNoStep:
                sty     DP_BAS_TXTPTR2_OFF
                lda     #$04
                jsr     FP_PTR1EqSTACKBASEplusA
                jsr     fpCopyFPAtoPTR1
                bra     cmdFORskipExecBody

STACKBASEplus11:
                clc
                lda     DP_BAS_FOR_STACKBASE
                adc     #$11
                sta     DP_BAS_FOR_STACKBASE
                bcc     @sk
                inc     DP_BAS_FOR_STACKBASE+1
                bne     @sk
                inc     DP_BAS_FOR_STACKBASE+2
@sk:            inc     DP_BAS_FOR_LVL
                rts

STACKBASEsub11: sec
                lda     DP_BAS_FOR_STACKBASE
                sbc     #$11
                sta     DP_BAS_FOR_STACKBASE
                lda     DP_BAS_FOR_STACKBASE+1
                sbc     #$00
                sta     DP_BAS_FOR_STACKBASE+1
                lda     DP_BAS_FOR_STACKBASE+2
                sbc     #$00
                sta     DP_BAS_FOR_STACKBASE+2
                dec     DP_BAS_FOR_LVL
                rts

FP_PTR1EqSTACKBASEplusA:
                clc
                adc     DP_BAS_FOR_STACKBASE
                sta     DP_BAS_FP_PTR1
                lda     #$00
                adc     DP_BAS_FOR_STACKBASE+1
                sta     DP_BAS_FP_PTR1+1
                lda     #$00
                adc     DP_BAS_FOR_STACKBASE+2
                sta     DP_BAS_FP_PTR1+2
                rts

exec_GOSUB:     
        .IFDEF COMMUNICATOR
                jsr     qrySetPAGETOPSpecial
        .ENDIF
                jsr     parse_lineno_searchprog_brknfnd
intGOSUB_FPB_2: jsr     parse_nextstmt_yield_TXTOFF2
                ldy     DP_BAS_GOSUB_LVL
                cpy     #$4e
                bcs     brk25_TooManyGOSUBS
                lda     DP_BAS_TXTPTR2
                sta     [DP_BAS_GOSUB_STACKBASE],y
                iny
                lda     DP_BAS_TXTPTR2+1
                sta     [DP_BAS_GOSUB_STACKBASE],y
                iny
                lda     DP_BAS_TXTPTR2+2
                sta     [DP_BAS_GOSUB_STACKBASE],y
                iny
                sty     DP_BAS_GOSUB_LVL
                bra     cmdGOTODecodedLineNumber

brk25_TooManyGOSUBS:
                brk     $25

                .byte   "Too many "
                .byte   tknGOSUB
                .byte   's'

brk_26_NoGOSUB: brk     $26

                .byte   "No "
                .byte   tknGOSUB
                .byte   $00

exec_RETURN:    jsr     parse_nextstmt_yield_TXTOFF2
        .IFDEF COMMUNICATOR
                bit     DP_BAS_CO_FLAGS
                bmi     @sk
                jsl     _CWT
                phk
                plb
                jmp     continue
@sk:
        .ENDIF

                ldy     DP_BAS_GOSUB_LVL
                beq     brk_26_NoGOSUB
                dey
                lda     [DP_BAS_GOSUB_STACKBASE],y
                sta     DP_BAS_TXTPTR2+2
                dey
                lda     [DP_BAS_GOSUB_STACKBASE],y
                sta     DP_BAS_TXTPTR2+1
                dey
                lda     [DP_BAS_GOSUB_STACKBASE],y
                sta     DP_BAS_TXTPTR2
                sty     DP_BAS_GOSUB_LVL
jmp_parse_EOR_TXTPTR2:
                jmp     continue

exec_GOTO:      
        .IFDEF COMMUNICATOR
                jsr     qrySetPAGETOPSpecial
        .ENDIF
                jsr     parse_lineno_searchprog_brknfnd
                jsr     parse_nextstmt_yield_TXTOFF2
cmdGOTODecodedLineNumber:
                lda     DP_BAS_TRACEFLAG
                beq     @sk
                jsr     doTracePrintLineNoIfInRange
@sk:            ldy     #$04
                sty     DP_BAS_TXTPTR2_OFF
                ldy     DP_FPB_exp
                lda     DP_FPB_mant
                ldx     DP_FPB_mant+1
                stz     DP_BAS_LAST_EOSEOR_D_
                sty     DP_BAS_TXTPTR2
                sta     DP_BAS_TXTPTR2+1
                stx     DP_BAS_TXTPTR2+2
                jmp     skipSpacesExecImmed

exec_ON_ERROR_OFF:
                jsr     parse_nextstmt_yield_TXTOFF2
                jsr     exec_ERROR_OFF_reset_ERRORPTR
                bra     jmp_parse_EOR_TXTPTR2

exec_ON_ERROR:  jsr     parse_skip_spaces_PTR2
                cmp     #tknOFF                         ;check for OFF
                beq     exec_ON_ERROR_OFF
                ldy     DP_BAS_TXTPTR2_OFF
                dey
                jsr     parse_updPTRfromPTR2_yield
                stz     DP_BAS_TXTPTR2_OFF
                lda     DP_BAS_TXTPTR2
                sta     DP_BAS_ONERRORPTR
                lda     DP_BAS_TXTPTR2+1
                sta     DP_BAS_ONERRORPTR+1
                lda     DP_BAS_TXTPTR2+2
                sta     DP_BAS_ONERRORPTR+2
                jmp     parse_skip_EOL

exec_ON:        jsr     parse_skip_spaces_PTR2
                cmp     #tknERROR                       ;check for ERROR
                beq     exec_ON_ERROR
                dec     DP_BAS_TXTPTR2_OFF
                jsr     evalForceINT
                cpx     #tknPROC
                beq     cmdOnGSP
                iny
                cpx     #tknGOTO
                beq     cmdOnGSP
                cpx     #tknGOSUB
                bne     brk_27_ONSyntax
cmdOnGSP:       phx
                lda     DP_BAS_INT_WA+1
                ora     DP_BAS_INT_WA+2
                ora     DP_BAS_INT_WA+3
                bne     cmdONSkipNoMatch
                ldx     DP_BAS_INT_WA
                beq     cmdONSkipNoMatch
                dec     DP_BAS_INT_WA
                beq     cmdONFound
                tax
cmdONcharLp:    lda     [DP_BAS_TXTPTR2],y
                cmp     #$0d
                beq     cmdONSkipNoMatch
                cmp     #':'
                beq     cmdONSkipNoMatch
                cmp     #tknELSE
                beq     cmdONSkipNoMatch
                iny
                cmp     #')'
                bne     @sk2
                dex
@sk2:           cmp     #'('
                bne     @sk1
                inx
@sk1:           cmp     #','
                bne     cmdONcharLp
                txa
                bne     cmdONcharLp
                dec     DP_BAS_INT_WA
                bne     cmdONcharLp
cmdONFound:     pla
                cmp     #tknPROC
                beq     cmdOnFoundPROC
                sty     DP_BAS_TXTPTR2_OFF
                cmp     #tknGOSUB
                beq     cmdONGosub
                jsr     parse_lineno_searchprog_brknfnd
                jsr     checkForESC
                jmp     cmdGOTODecodedLineNumber

cmdONGosub:     jsr     parse_lineno_searchprog_brknfnd
                ldy     DP_BAS_TXTPTR2_OFF
                jsr     findNextStmt
                jmp     intGOSUB_FPB_2

cmdONSkipNoMatch:
                pla
@lp:            lda     [DP_BAS_TXTPTR2],y
                iny
                cmp     #tknELSE
                beq     elseInyDoELSE
                cmp     #$0d
                bne     @lp
                brk     $28

                .byte   tknON
                .byte   " range"

brk_27_ONSyntax:
                brk     $27

                .byte   tknON
                .byte   " syntax"

brk29_NoSuchLine:
                brk     $29

                .byte   "No such line"
                .byte   $00

cmdOnFoundPROC: sty     DP_BAS_TXTPTR_OFF
                jsr     parse_skip_spaces
                cmp     #tknPROC
                bne     brk_27_ONSyntax
                jsr     exec_FN_PROC
                ldy     DP_BAS_TXTPTR_OFF
                jsr     findNextStmt
                jmp     scanNextContinue

elseInyDoELSE:  sty     DP_BAS_TXTPTR2_OFF
                jmp     L7417

inyFindNextStmt:
                iny
findNextStmt:   lda     [DP_BAS_TXTPTR2],y
                cmp     #$0d
                beq     @sk
                cmp     #':'
                bne     inyFindNextStmt
@sk:            sty     DP_BAS_TXTPTR2_OFF
                rts

parse_lineno_searchprog_brknfnd:
                lda     DP_BAS_TXTPTR2_OFF
                sta     DP_BAS_C7_UK
                stz     DP_BAS_LAST_EOSEOR_D_
                jsr     parse_lineno_atOFF
                bcs     prog_search_lineno_brknotfnd
                lda     #$80
                sta     DP_BAS_SET_TO_Z_AT_EOS
                jsr     evalForceINT
                lda     #$80
                trb     DP_BAS_SET_TO_Z_AT_EOS
                bit     DP_BAS_SET_TO_Z_AT_EOS
                bvs     findProgLineOrBRK
                bit     DP_BAS_INT_WA+3
                bmi     L92EE
                lda     #$80
                trb     DP_BAS_INT_WA+1
prog_search_lineno_brknotfnd:
                jsr     prog_search_lineno
                bcc     brk29_NoSuchLine
                rts

L92EE:          lda     DP_BAS_INT_WA
                sta     DP_FPB_exp
                lda     DP_BAS_INT_WA+1
                sta     DP_FPB_mant
                lda     DP_BAS_INT_WA+2
                sta     DP_FPB_mant+1
                bit     DP_BAS_TRACEFLAG
                beq     rtsL930B
                phy
                ldy     #$02
                lda     [DP_FPB_exp],y
                sta     DP_BAS_INT_WA
                dey
                lda     [DP_FPB_exp],y
                sta     DP_BAS_INT_WA+1
                ply
rtsL930B:       rts

findProgLineOrBRK:
                lda     DP_BAS_C7_UK
                sta     DP_BAS_TXTPTR2_OFF
                stz     DP_BAS_SET_TO_Z_AT_EOS
                bra     parse_lineno_searchprog_brknfnd

callOSBGET_2:   
        .IFDEF COMMUNICATOR
                phb                                     
                phx
                rep     #$10
                .i16
                ldy     DP_BAS_CURCHAN
                cop     COP_0A_OPBGT
                jsr     setREPORT_BHA_Cy
                sep     #$10
                .i8
                plx
                plb
                rts
        .ENDIF
        .IFDEF MOS
                phx
                ldy     DP_BAS_CURCHAN
                jsl     nat_OSBGET
                plx
                rts
        .ENDIF

jmp_brk06_type_mismatch3:
                jmp     brk06_type_mismatch

jmp_brk_10_Syntax2:
                jmp     brk10_Syntax

INPUT_HASH_done:
                sty     DP_BAS_TXTPTR2_OFF
braScanNextContinue:
                jmp     scanNextContinue

exec_INPUT_HASH:
                jsr     parse_decOff2_fileHandleHash
                rep     #$10
                .i16
                ldy     DP_BAS_INT_WA
                sty     DP_BAS_CURCHAN
                sep     #$10
                .i8
                jsr     parseOFF2eqYeqOFF
inputHASHlp:    jsr     parse_SkipSpacesPTR2_cmp_COMMA
                bne     INPUT_HASH_done
                jsr     findVarOrAllocEmpty
                beq     jmp_brk_10_Syntax2
                jsr     parseOFF2eqYeqOFF
                php
                jsr     stack_INTWA
                jsr     callOSBGET_2
                sta     DP_BAS_40_VARTYPE
                plp
                bcc     @cmdINPUT_HASH_INT
                lda     DP_BAS_40_VARTYPE
                bne     jmp_brk06_type_mismatch3
                jsr     callOSBGET_2
; it's a string get length byte then string
                sta     DP_BAS_STRLEN
                tax
                beq     @sk0len
@strloop:       jsr     callOSBGET_2
                txy
                dey
                sta     [DP_BAS_STRWKSP_L],y
                dex
                bne     @strloop
@sk0len:        jsr     copyStringToVar
                bra     inputHASHlp

@cmdINPUT_HASH_INT:
                lda     DP_BAS_40_VARTYPE
                beq     jmp_brk06_type_mismatch3
                bmi     @cmdINPUT_HASH_FP
                ldx     #$03
@lp:            jsr     callOSBGET_2
                sta     DP_BAS_INT_WA,x
                dex
                bpl     @lp
                bra     cmdINPUT_HASH_StoreAtVarPtr

@cmdINPUT_HASH_FP:
                ldx     #$04
@lp1:           jsr     callOSBGET_2
                sta     DP_BAS_00_REAL_UK,x
                dex
                bpl     @lp1
                jsr     fpCopyPrintPTRtoFPA
cmdINPUT_HASH_StoreAtVarPtr:
                jsr     stack_copy6_to_DP_BAS_TMP66_uns4
                jsr     storeEvaledExprinVarAtTMP6ptr
                bra     inputHASHlp

plaplabraScNxtCont:
                pla
                pla
                bra     braScanNextContinue

exec_INPUT:     jsr     parse_skipSpacesPTR2_cmp_HASH
                beq     exec_INPUT_HASH
                cmp     #tknLINE                        ;check for LINE
                beq     exec_INPUT_LINE
                dec     DP_BAS_TXTPTR2_OFF
                clc
exec_INPUT_LINE:
                ror     DP_BAS_6F_UK
                lsr     DP_BAS_6F_UK
                lda     #$ff
                sta     DP_BAS_70_UK
cmdINPUT_LINE_lp:
                jsr     cmdINPUT_PRINT_prompt
                bcs     @sk4
@lp2:           jsr     cmdINPUT_PRINT_prompt
                bcc     @lp2
                ldx     #$ff
                stx     DP_BAS_70_UK
                clc
@sk4:           php
                asl     DP_BAS_6F_UK
                plp
                ror     DP_BAS_6F_UK
                cmp     #','
                beq     cmdINPUT_LINE_lp
                cmp     #';'
                beq     cmdINPUT_LINE_lp
                dec     DP_BAS_TXTPTR2_OFF
                lda     DP_BAS_6F_UK
                pha
                lda     DP_BAS_70_UK
                pha
                jsr     findVarOrAllocEmpty
                beq     plaplabraScNxtCont
                pla
                sta     DP_BAS_70_UK
                pla
                sta     DP_BAS_6F_UK
                jsr     parseOFF2eqYeqOFF
                php
                bit     DP_BAS_6F_UK
                bvs     @sk3
                lda     DP_BAS_70_UK
                cmp     #$ff
                bne     @sk41
@sk3:           bit     DP_BAS_6F_UK
                bpl     @sk2
                lda     #'?'
                jsr     call_OSWRCH
@sk2:           jsr     ReadKeysTo_InBuf
                sty     DP_BAS_STRLEN
                asl     DP_BAS_6F_UK
                clc
                ror     DP_BAS_6F_UK
                bit     DP_BAS_6F_UK
                bvs     cmdINPUT_LINE_INPUT
@sk41:          sta     DP_BAS_TXTPTR_OFF
                lda     DP_BAS_STRWKSP_L
                sta     DP_BAS_TXTPTR
                lda     DP_BAS_STRWKSP_L+1
                sta     DP_BAS_TXTPTR+1
                lda     DP_BAS_STRWKSP_L+2
                sta     DP_BAS_TXTPTR+2
                jsr     readCommaSepString
@lp:            jsr     parse_skip_space_CMPcomma
                beq     @sk
                cmp     #$0d
                bne     @lp
                ldy     #$fe
@sk:            iny
                sty     DP_BAS_70_UK
cmdINPUT_LINE_INPUT:
                plp
                bcs     cmdINPUT_LINE_INPUT_STR
                jsr     pushINTWA_on_hw_stack
                jsr     str2num
                jsr     storeEvaledExprinStackedVarPTr
brlINPUT_LINE_lp:
                brl     cmdINPUT_LINE_lp

cmdINPUT_LINE_INPUT_STR:
                stz     DP_BAS_40_VARTYPE
                jsr     copyStringToVar2
                bra     brlINPUT_LINE_lp

exec_RESTORE:   ldy     DP_BAS_PAGE
                sty     DP_FPB_exp
                ldy     DP_BAS_PAGE+1
                sty     DP_FPB_mant
                ldy     DP_BAS_PAGE+2
                sty     DP_FPB_mant+1
                jsr     parse_skip_spaces_PTR2
                dec     DP_BAS_TXTPTR2_OFF
                cmp     #':'
                beq     @sk
                cmp     #$0d
                beq     @sk
                cmp     #tknELSE
                beq     @sk
                lda     DP_BAS_TXTPTR2_OFF
                sta     DP_BAS_C7_UK
                jsr     parse_lineno_searchprog_brknfnd
@sk:            jsr     parse_nextstmt_yield_TXTOFF2
                lda     DP_FPB_exp
                sta     DP_BAS_DATA_PTR
                lda     DP_FPB_mant
                sta     DP_BAS_DATA_PTR+1
                lda     DP_FPB_mant+1
                sta     DP_BAS_DATA_PTR+2
                lda     #$ff
                sta     DP_BAS_DATA_READ_FL
                jmp     continue

cmdREAD_next:   jsr     parse_SkipSpacesPTR2_cmp_COMMA
                beq     exec_READ
                jmp     decOff2scanNextContinue

exec_READ:      jsr     findVarOrAllocEmpty
                beq     cmdREAD_next
                bcs     cmdREAD_readString
                jsr     cmdREAD_findNextDataItem
                jsr     pushINTWA_on_hw_stack
                jsr     evalAtYStoreEvaldinStackedVarPTr
                bra     skL949C

cmdREAD_readString:
                jsr     cmdREAD_findNextDataItem
                jsr     stack_INTWA
                jsr     readCommaSepString
                sta     DP_BAS_40_VARTYPE
                jsr     copyStringToVar
skL949C:        clc
                lda     DP_BAS_TXTPTR_OFF
                adc     DP_BAS_TXTPTR
                sta     DP_BAS_DATA_PTR
                lda     DP_BAS_TXTPTR+1
                adc     #$00
                sta     DP_BAS_DATA_PTR+1
                lda     DP_BAS_TXTPTR+2
                adc     #$00
                sta     DP_BAS_DATA_PTR+2
                bra     cmdREAD_next

cmdREAD_findNextDataItem:
                jsr     parseOFF2eqYeqOFF
                lda     DP_BAS_DATA_PTR
                sta     DP_BAS_TXTPTR
                lda     DP_BAS_DATA_PTR+1
                sta     DP_BAS_TXTPTR+1
                lda     DP_BAS_DATA_PTR+2
                sta     DP_BAS_TXTPTR+2
                stz     DP_BAS_TXTPTR_OFF
                jsr     parse_skip_space_CMPcomma
                beq     rtsL9538
                cmp     #tknDATA
                beq     rtsL9538
                cmp     #$0d
                beq     cmdREAD_CR
@CRlp:          jsr     parse_skip_space_CMPcomma
                beq     rtsL9538
                cmp     #$0d
                bne     @CRlp
cmdREAD_CR:     lda     DP_BAS_TXTPTR
                sta     DP_BAS_TMP6
                lda     DP_BAS_TXTPTR+1
                sta     DP_BAS_TMP6+1
                lda     DP_BAS_TXTPTR+2
                sta     DP_BAS_TMP6+2
                ldy     DP_BAS_TXTPTR_OFF
                lda     [DP_BAS_TXTPTR],y
                bmi     brk_2a_out_of_DATA
                iny
                iny
                lda     [DP_BAS_TXTPTR],y
                pha
@spclp:         iny
                lda     [DP_BAS_TXTPTR],y
                cmp     #'.'
                bne     @skNotDOT
                iny
                jsr     varScanNameAtTMP6_Y
@skNotDOT:      cmp     #' '
                beq     @spclp
                cmp     #tknDATA
                beq     L9534DATA
                pla
                clc
                adc     DP_BAS_TXTPTR
                sta     DP_BAS_TXTPTR
                bcc     cmdREAD_CR
                inc     DP_BAS_TXTPTR+1
                bne     cmdREAD_CR
                inc     DP_BAS_TXTPTR+2
                bra     cmdREAD_CR

brk_2a_out_of_DATA:
                brk     $2a

                .byte   "Out of "
                .byte   tknDATA                         ;DATA

brk_2b_No_REPEAT:
                brk     $2b

                .byte   "No "
                .byte   tknREPEAT                       ;REPEAT

brk2d_MissingHash:
                brk     $2d

                .byte   tknMissing
                .byte   '#'

brk_TooManyREPEATs:
                brk     $2c

                .byte   "Too many "
                .byte   tknREPEAT
                .byte   's'
                .byte   $00

L9534DATA:      plx
                iny
                sty     DP_BAS_TXTPTR_OFF
rtsL9538:       rts

exec_UNTIL:     jsr     evalExpressionMAIN
                jsr     scanNextStmt
                jsr     checkTypeInVARTYPEConv2INT
                ldy     DP_BAS_REPEAT_LVL
                beq     brk_2b_No_REPEAT
                lda     DP_BAS_INT_WA
                ora     DP_BAS_INT_WA+1
                ora     DP_BAS_INT_WA+2
                ora     DP_BAS_INT_WA+3
                beq     @skAgain
                dec     DP_BAS_REPEAT_LVL
                dec     DP_BAS_REPEAT_LVL
                dec     DP_BAS_REPEAT_LVL
                jmp     continue

@skAgain:       dey
                lda     [DP_BAS_REPEAT_STACKBASE],y
                sta     DP_BAS_TXTPTR2+2
                dey
                lda     [DP_BAS_REPEAT_STACKBASE],y
                sta     DP_BAS_TXTPTR2+1
                dey
                lda     [DP_BAS_REPEAT_STACKBASE],y
                sta     DP_BAS_TXTPTR2
                jmp     skipSpacesExecImmed

parse_decOff2_fileHandleHash:
                dec     DP_BAS_TXTPTR2_OFF
parse_fileHandleHash_PTR2:
                lda     DP_BAS_TXTPTR2_OFF
                sta     DP_BAS_TXTPTR_OFF
                lda     DP_BAS_TXTPTR2
                sta     DP_BAS_TXTPTR
                lda     DP_BAS_TXTPTR2+1
                sta     DP_BAS_TXTPTR+1
                lda     DP_BAS_TXTPTR2+2
                sta     DP_BAS_TXTPTR+2
parse_fileHandleHash2:
                clc
parse_fileHandleHash:
                php
                jsr     parse_skip_spaces
                plp
                eor     #'#'
                beq     @evalINTtoYandA
                bcc     brk2d_MissingHash

                dec     DP_BAS_TXTPTR_OFF
                rts

@evalINTtoYandA:
                jsr     evalLevel1checkTypeStoreAsINT
                ldy     DP_BAS_INT_WA
                tya
                clc
                rts

exec_REPEAT:    ldx     DP_BAS_REPEAT_LVL
                cpx     #$3c
                bcs     brk_TooManyREPEATs
                jsr     parse_updPTRfromPTR2_yield
                ldy     DP_BAS_REPEAT_LVL
                lda     DP_BAS_TXTPTR2
                sta     [DP_BAS_REPEAT_STACKBASE],y
                iny
                lda     DP_BAS_TXTPTR2+1
                sta     [DP_BAS_REPEAT_STACKBASE],y
                iny
                lda     DP_BAS_TXTPTR2+2
                sta     [DP_BAS_REPEAT_STACKBASE],y
                iny
                sty     DP_BAS_REPEAT_LVL
                jmp     skipSpacesExecImmed

        .IFDEF COMMUNICATOR
ReadKeysTo_InBuf:
                lda     DP_BAS_STRWKSP_L+2
                phb
                pha
                lda     DP_BAS_STRWKSP_L
                sta     DP_BAS_TMP6
                lda     DP_BAS_STRWKSP_L+1
                bra     L95C9

ReadKeysTo_MEMBASE:
                lda     DP_BAS_MEMBASE+2
                phb
                pha
                lda     DP_BAS_MEMBASE
                sta     DP_BAS_TMP6
                lda     DP_BAS_MEMBASE+1
L95C9:          sta     DP_BAS_TMP6+1
                pla
                sta     DP_BAS_TMP6+2
                stz     DP_BAS_TMP6+3
                lda     #$ee                            ;max char value
                sta     DP_BAS_TMP6+4
                stz     DP_BAS_TMP6+5

                lda     #$20
                tsb     DP_BAS_CO_FLAGS
                bne     @L95E0
                stz     DP_BAS_TMP6+6
                stz     DP_BAS_TMP6+7
@L95E0:         stz     DP_BAS_TMP6+8
                stz     DP_BAS_TMP6+9
                stz     DP_BAS_TMP6+10
                stz     DP_BAS_TMP6+11
@L95E8:         
                lda     #>DP_BAS_TMP6
                xba
                lda     #DP_BAS_TMP6
                cop     COP_24_OPCVD
                ldy     #$00
                cop     COP_22_OPRLN                    ;; read line cf OSWORD 0
                plb
                bcc     L9619
                rep     #$30
                .a16
                .i16
                lda     #$0001
                ldx     #$0002
                jsl     _EV
                sep     #$30
                .a8
                .i8
                bcs     brl_brk_11_Escape
                lda     #$03
                cop     COP_16_OPAEV
                bit     DP_BAS_CO_FLAGS
                bpl     @L9613
                jsr     ONERROROFF
                beq     @L9613
@L9613:         phb
                bra     @L95E8

        .ENDIF
        .IFDEF MOS

ReadKeysTo_InBuf:
                jsr     BL_doOSWORD0
                phy
                ;TODO use TFM?
                tyx
@lp:            lda     f:BANK0_SCRATCH_PAGE,X
                sta     [DP_BAS_STRWKSP_L],Y
                dex
                dey
                cpy     #$FF
                bne     @lp
                bra     L9619


ReadKeysTo_MEMBASE:
                jsr     BL_doOSWORD0
                phy
                tyx
@lp:            lda     f:BANK0_SCRATCH_PAGE,X
                sta     [DP_BAS_MEMBASE],Y
                dex
                dey
                cpy     #$FF
                bne     @lp
                bra     L9619


BL_doOSWORD0:
                ;TODO: consider moving the immediate buffer to Bank 0?
                lda     #<BANK0_SCRATCH_PAGE
                sta     f:BANK0_OSWORD_BLOCK
                lda     #>BANK0_SCRATCH_PAGE
                sta     f:BANK0_OSWORD_BLOCK+1
                lda     #$FF
                sta     f:BANK0_OSWORD_BLOCK+2    ; len
                lda     #' '
                sta     f:BANK0_OSWORD_BLOCK+3    ; min char
                lda     #$EE
                sta     f:BANK0_OSWORD_BLOCK+4    ; max char

                ldx     #<BANK0_OSWORD_BLOCK
                ldy     #>BANK0_OSWORD_BLOCK
                lda     #0
                jsl     nat_OSWORD
                bcs     brl_brk_11_Escape
                rts

        .ENDIF
brl_brk_11_Escape:
                brl     brk_11_Escape


L9619:          stz     DP_BAS_COUNT
        .IFDEF COMMUNICATOR
                lda     #$20
                trb     DP_BAS_CO_FLAGS
                ldy     DP_BAS_TMP6+6
        .ENDIF
        .IFDEF MOS
                ply
        .ENDIF
                lda     #RETV_STR
                rts

PrintCRLFresetCOUNT:
                jsr     printCRLF
COUNTeq0:       stz     DP_BAS_COUNT
                rts

findLineAndDelete:
                jsr     prog_search_lineno
                bcc     rtsL969E
                lda     DP_FPB_exp
                sta     DP_BAS_TMP6
                sta     DP_BAS_TOP
                lda     DP_FPB_mant
                sta     DP_BAS_TMP6+1
                sta     DP_BAS_TOP+1
                lda     DP_FPB_mant+1
                sta     DP_BAS_TMP6+2
                sta     DP_BAS_TOP+2
                ldy     #$03
                lda     [DP_BAS_TMP6],y
                clc
                adc     DP_BAS_TMP6
                sta     DP_BAS_TMP6
                bcc     @L9652
                inc     DP_BAS_TMP6+1
                bne     @L9652
                inc     DP_BAS_TMP6+2
@L9652:         ldy     #$00
@L9654:         lda     [DP_BAS_TMP6],y
                sta     [DP_BAS_TOP],y
                cmp     #$0d
                bne     @L9677
                iny
                bne     @L966B
                inc     DP_BAS_TMP6+1
                bne     @L9665
                inc     DP_BAS_TMP6+2
@L9665:         inc     DP_BAS_TOP+1
                bne     @L966B
                inc     DP_BAS_TOP+2
@L966B:         lda     [DP_BAS_TMP6],y
                sta     [DP_BAS_TOP],y
                bmi     @L9688
                jsr     @L968B
                jsr     @L968B
@L9677:         iny
                bne     @L9654
                inc     DP_BAS_TMP6+1
                bne     @L9680
                inc     DP_BAS_TMP6+2
@L9680:         inc     DP_BAS_TOP+1
                bne     @L9654
                inc     DP_BAS_TOP+2
                bra     @L9654

@L9688:         jmp     addYtoYOP

@L968B:         iny
                bne     @L969A
                inc     DP_BAS_TOP+1
                bne     @L9694
                inc     DP_BAS_TOP+2
@L9694:         inc     DP_BAS_TMP6+1
                bne     @L969A
                inc     DP_BAS_TMP6+2
@L969A:         lda     [DP_BAS_TMP6],y
                sta     [DP_BAS_TOP],y
rtsL969E:       rts

tokenizeAndStore:
                ldx     #$ff
                stx     DP_BAS_OPT
                stx     DB_BAS_UNK_5A_6+3
                jsr     ResetStackProgStartRepGosFor
                lda     DP_BAS_TXTPTR2
                sta     DP_BAS_TMP6
                lda     DP_BAS_TXTPTR2+1
                sta     DP_BAS_TMP6+1
                lda     DP_BAS_TXTPTR2+2
                sta     DP_BAS_TMP6+2
                stz     DP_FPB_sgn
                stz     DP_BAS_TXTPTR2_OFF
                jsr     tokenizeAtTMP6ptr
                jsr     parse_lineno_atOFF
                bcc     rtsL969E
tokAndStoreAlreadyLineNoDecoded:
                lda     DP_BAS_LISTO
                beq     @tokLISTOeq0
@skSpc:         lda     [DP_BAS_MEMBASE],y
                iny
                cmp     #' '
                beq     @skSpc
                dey
@tokLISTOeq0:   tya
                clc
                adc     DP_BAS_MEMBASE
                sta     DB_BAS_UNK_5A_6+1
                lda     DP_BAS_MEMBASE+1
                adc     #$00
                sta     DP_FPB_sgn
                lda     DP_BAS_MEMBASE+2
                adc     #$00
                sta     DB_BAS_UNK_5A_6+3
                jsr     findLineAndDelete
                ldy     #$00
                lda     #$0d
                cmp     [DB_BAS_UNK_5A_6+1]
                beq     rtsL969E
@L96E9:         iny
                cmp     [DB_BAS_UNK_5A_6+1],y
                bne     @L96E9
                lda     #' '
@L96F0:         dey
                beq     @L96F7
                cmp     [DB_BAS_UNK_5A_6+1],y
                beq     @L96F0
@L96F7:         iny
                lda     #$0d
                sta     [DB_BAS_UNK_5A_6+1],y
                iny
                iny
                iny
                iny
                sty     DP_BAS_61_UK
                lda     DP_BAS_TOP
                sta     DP_BAS_TMP6+3
                lda     DP_BAS_TOP+1
                sta     DP_BAS_TMP6+4
                lda     DP_BAS_TOP+2
                sta     DB_BAS_UNK_5A_6
                jsr     clcAddYtoTOP
                sta     DP_BAS_TMP6
                lda     DP_BAS_TOP+1
                sta     DP_BAS_TMP6+1
                lda     DP_BAS_TOP+2
                sta     DP_BAS_TMP6+2
                dey
                lda     DP_BAS_HIMEM
                cmp     DP_BAS_TOP
                lda     DP_BAS_HIMEM+1
                sbc     DP_BAS_TOP+1
                lda     DP_BAS_HIMEM+2
                sbc     DP_BAS_TOP+2
                bcs     @L973A
                jsr     findTOP
                jsr     int_CLEAR
                brk     $00

                .byte   tknLINE
                .byte   " space"
                .byte   $00

@L973A:         lda     [DP_BAS_TMP6+3],y
                sta     [DP_BAS_TMP6],y
                tya
                bne     @L975B
                sec
                lda     DP_BAS_TMP6+4
                sbc     #$01
                sta     DP_BAS_TMP6+4
                lda     DB_BAS_UNK_5A_6
                sbc     #$00
                sta     DB_BAS_UNK_5A_6
                sec
                lda     DP_BAS_TMP6+1
                sbc     #$01
                sta     DP_BAS_TMP6+1
                lda     DP_BAS_TMP6+2
                sbc     #$00
                sta     DP_BAS_TMP6+2
@L975B:         sec
                dey
                phy
                tya
                ldx     DP_BAS_TMP6+4
                ldy     DB_BAS_UNK_5A_6
                adc     DP_BAS_TMP6+3
                bcc     @L976B
                inx
                bne     @L976B
                iny
@L976B:         cmp     DP_FPB_exp
                txa
                sbc     DP_FPB_mant
                tya
                sbc     DP_FPB_mant+1
                ply
                bcs     @L973A
                ldy     #$01
                lda     DP_BAS_INT_WA+1
                sta     [DP_FPB_exp],y
                iny
                lda     DP_BAS_INT_WA
                sta     [DP_FPB_exp],y
                iny
                lda     DP_BAS_61_UK
                sta     [DP_FPB_exp],y
                sec
                tya
                adc     DP_FPB_exp
                sta     DP_FPB_exp
                bcc     @L9794
                inc     DP_FPB_mant
                bne     @L9794
                inc     DP_FPB_mant+1
@L9794:         ldy     #$ff
@L9796:         iny
                lda     [DB_BAS_UNK_5A_6+1],y
                sta     [DP_FPB_exp],y
                cmp     #$0d
                bne     @L9796
                rts

int_CLEAR:      stz     DP_BAS_LOMEM
                lda     DP_BAS_LOMEM_LIM_PAG
                sta     DP_BAS_LOMEM+1
                lda     DP_BAS_LOMEM_LIM_PAG+1
                sta     DP_BAS_LOMEM+2
                lda     DP_BAS_TOP
                cmp     #$00
                lda     DP_BAS_TOP+1
                sbc     DP_BAS_LOMEM_LIM_PAG
                lda     DP_BAS_TOP+2
                sbc     DP_BAS_LOMEM_LIM_PAG+1
                bcc     clearVARSsetVARTOPtoLOMEM
                lda     DP_BAS_MEMEND
                cmp     DP_BAS_TOP
                lda     DP_BAS_MEMEND+1
                sbc     DP_BAS_TOP+1
                lda     DP_BAS_MEMEND+2
                sbc     DP_BAS_TOP+2
                bcc     clearVARSsetVARTOPtoLOMEM
                lda     DP_BAS_TOP
                sta     DP_BAS_LOMEM
                lda     DP_BAS_TOP+1
                sta     DP_BAS_LOMEM+1
                lda     DP_BAS_TOP+2
                sta     DP_BAS_LOMEM+2
clearVARSsetVARTOPtoLOMEM:
                lda     DP_BAS_LOMEM
                sta     DP_BAS_VARTOP
                lda     DP_BAS_LOMEM+1
                sta     DP_BAS_VARTOP+1
                lda     DP_BAS_LOMEM+2
                sta     DP_BAS_VARTOP+2
                jsr     ResetStackProgStartRepGosFor
clearVARS:      ldy     #VAR_OFFS_END-1
                lda     #$00
@lp:            sta     [DP_BAS_VARS_BASE],y
                dey
                bne     @lp
                sta     [DP_BAS_VARS_BASE]              ;TODO: pointless? optimize away?
                lda     #<tblBuiltInsLL
                ldy     #$00
                sta     [DP_BAS_VARS_BASE],y
                iny
                lda     #>tblBuiltInsLL
                sta     [DP_BAS_VARS_BASE],y
                iny
                phb
                pla
                sta     [DP_BAS_VARS_BASE],y
                rts

ResetStackProgStartRepGosFor:
                lda     DP_BAS_PAGE+1
                sta     DP_BAS_DATA_PTR+1
                lda     DP_BAS_PAGE+2
                sta     DP_BAS_DATA_PTR+2
                lda     DP_BAS_HIMEM
                sta     DP_BAS_STACK
                lda     DP_BAS_HIMEM+1
                sta     DP_BAS_STACK+1
                lda     DP_BAS_HIMEM+2
                sta     DP_BAS_STACK+2
                lda     #$80
                trb     DP_BAS_LISTO
                ldx     #DP_BAS_FPTMPptr3
                ldy     #$08
                jsr     mem_add_16bconsts_to_3b_pointers
                stz     DP_BAS_REPEAT_LVL
                stz     DP_BAS_GOSUB_LVL
                lda     DP_BAS_PAGE
                sta     DP_BAS_DATA_PTR
                jsr     STACKBASEsub11
                stz     DP_BAS_FOR_LVL
                rts

popFPFromStackToPTR1:
                lda     DP_BAS_STACK
                clc
                sta     DP_BAS_FP_PTR1
                adc     #$05
                sta     DP_BAS_STACK
                lda     DP_BAS_STACK+1
                sta     DP_BAS_FP_PTR1+1
                adc     #$00
                sta     DP_BAS_STACK+1
                lda     DP_BAS_STACK+2
                sta     DP_BAS_FP_PTR1+2
                adc     #$00
                sta     DP_BAS_STACK+2
                rts

stack_REAL:     lda     DP_BAS_STACK
                sec
                sbc     #$05
                jsr     UpdStackToACyCheckFull
                lda     DP_FPA_exp+1
                sta     [DP_BAS_STACK]
                ldy     #$01
                lda     DP_FPA_sgn
                eor     DP_FPA_mant
                and     #$80
                eor     DP_FPA_mant
                sta     [DP_BAS_STACK],y
                iny
                lda     DP_FPA_mant+1
                sta     [DP_BAS_STACK],y
                iny
                lda     DP_FPA_mant+2
                sta     [DP_BAS_STACK],y
                iny
                lda     DP_FPA_mant+3
                sta     [DP_BAS_STACK],y
                rts

stack_INTorREAL:
                beq     StackString
                bmi     stack_REAL
stack_INTWA:    lda     DP_BAS_STACK
                sec
                sbc     #$04
                jsr     UpdStackToACyCheckFull
                ldy     #$03
                lda     DP_BAS_INT_WA+3
                sta     [DP_BAS_STACK],y
                dey
                lda     DP_BAS_INT_WA+2
                sta     [DP_BAS_STACK],y
                dey
                lda     DP_BAS_INT_WA+1
                sta     [DP_BAS_STACK],y
                lda     DP_BAS_INT_WA
                sta     [DP_BAS_STACK]
                rts

pushINTWA_on_hw_stack:
                ply
                plx
                lda     DP_BAS_INT_WA
                pha
                lda     DP_BAS_INT_WA+1
                pha
                lda     DP_BAS_INT_WA+2
                pha
                lda     DP_BAS_INT_WA+3
                pha
                phx
                phy
                rts

StackString:    clc                                     ;TODO: use MVN
                lda     DP_BAS_STACK
                sbc     DP_BAS_STRLEN
                jsr     UpdStackToACyCheckFull
                ldy     DP_BAS_STRLEN
                beq     @sk
@lp:            dey
                lda     [DP_BAS_STRWKSP_L],y
                iny
                sta     [DP_BAS_STACK],y
                dey
                bne     @lp
@sk:            lda     DP_BAS_STRLEN
                sta     [DP_BAS_STACK]
                rts

L98B8:          lda     DP_BAS_TMP6+3
                cmp     #$80
                beq     deLocalizeatZP_GEN_PTR
                bcc     delocalizeStaticString
                lda     [DP_BAS_STACK]
                tax
                beq     @L98E3
                lda     [DP_BAS_TMP6]
                sbc     #$01
                sta     DP_BAS_TMP6+3
                ldy     #$01
                lda     [DP_BAS_TMP6],y
                sbc     #$00
                sta     DP_BAS_TMP6+4
                iny
                lda     [DP_BAS_TMP6],y
                sbc     #$00
                sta     DB_BAS_UNK_5A_6
                dey
@L98DB:         lda     [DP_BAS_STACK],y
                sta     [DP_BAS_TMP6+3],y
                iny
                dex
                bne     @L98DB
@L98E3:         lda     [DP_BAS_STACK]
                ldy     #$04
L98E7:          sta     [DP_BAS_TMP6],y
                bra     deAllocAFromStack

deLocalizeatZP_GEN_PTR:
                lda     [DP_BAS_STACK]
                tax
                beq     @L98FC
                ldy     #$01
@L98F2:         lda     [DP_BAS_STACK],y
                dey
                sta     [DP_BAS_TMP6],y
                iny
                iny
                dex
                bne     @L98F2
@L98FC:         lda     #$0d
                bne     L98E7

delocalizeStaticString:
                lda     [DP_BAS_STACK]
                sta     [DP_BAS_TMP6]
                ldy     #$04
                lda     DP_BAS_TMP6+3
                beq     @sk
                ldy     #$01
                lda     [DP_BAS_STACK],y
                sta     [DP_BAS_TMP6],y
                iny
                lda     [DP_BAS_STACK],y
                sta     [DP_BAS_TMP6],y
                iny
                lda     [DP_BAS_STACK],y
                sta     [DP_BAS_TMP6],y
                iny
                cpy     DP_BAS_TMP6+3
                bcs     @sk
                lda     [DP_BAS_STACK],y
                sta     [DP_BAS_TMP6],y
                iny
@sk:            tya
                clc
                bra     stack_ADD_A

popStackedString:
                lda     [DP_BAS_STACK]
                sta     DP_BAS_STRLEN
                beq     L9939
                tay
@lp:            lda     [DP_BAS_STACK],y
                dey
                sta     [DP_BAS_STRWKSP_L],y
                tya
                bne     @lp
deAllocAFromStack:
                lda     [DP_BAS_STACK]
L9939:          sec
                bra     stack_ADD_A

popIntA:        ldy     #$03
                lda     [DP_BAS_STACK],y
                sta     DP_BAS_INT_WA+3
                dey
                lda     [DP_BAS_STACK],y
                sta     DP_BAS_INT_WA+2
                dey
                lda     [DP_BAS_STACK],y
                sta     DP_BAS_INT_WA+1
                lda     [DP_BAS_STACK]
                sta     DP_BAS_INT_WA
stack_ADD4:     clc
                lda     #$04
stack_ADD_A:    adc     DP_BAS_STACK
                sta     DP_BAS_STACK
                bcc     @L995F
                inc     DP_BAS_STACK+1
                bne     @L995F
                inc     DP_BAS_STACK+2
@L995F:         rts

stack_copy6_to_DP_BAS_TMP66_uns4:
                ldx     #DP_BAS_TMP6
stack_copy6_to_X_uns4:
                ldy     #$05
                lda     [DP_BAS_STACK],y
                sta     $05,x
                dey
                lda     [DP_BAS_STACK],y
                sta     DP_BAS_00_REAL_UK+4,x
                dey
                lda     [DP_BAS_STACK],y
                sta     $03,x
                dey
                lda     [DP_BAS_STACK],y
                sta     $02,x
                dey
                lda     [DP_BAS_STACK],y
                sta     $01,x
                lda     [DP_BAS_STACK]
                sta     $00,x
                bra     stack_ADD4

UpdStackToACyCheckFull:
                sta     DP_BAS_STACK
                lda     DP_BAS_STACK+1
                sbc     #$00
                sta     DP_BAS_STACK+1
                lda     DP_BAS_STACK+2
                sbc     #$00
                sta     DP_BAS_STACK+2
                lda     DP_BAS_STACK
                ldy     DP_BAS_STACK+2
                cpy     DP_BAS_VARTOP+2
                bcc     jmpBrkNoRoom
                bne     @rts
                ldy     DP_BAS_STACK+1
                cpy     DP_BAS_VARTOP+1
                bcc     jmpBrkNoRoom
                bne     @rts
                cmp     DP_BAS_VARTOP
                bcc     jmpBrkNoRoom
@rts:           rts

clrIntBrkNoRoom:
                jsr     int_CLEAR
jmpBrkNoRoom:   jmp     brk_00_NoRoom

doListPrintTokenA:
                sta     DP_BAS_TMP6
                cmp     #$80
                bcc     list_printA
                lda     #<tblTokensAsc
                sta     DP_BAS_TMP6+1
                lda     #>tblTokensAsc
                sta     DP_BAS_TMP6+2
                phy
@lpNextToken:   ldy     #$00
@lpSkipNonToken:
                iny
                lda     (DP_BAS_TMP6+1),y
                bpl     @lpSkipNonToken
                cmp     DP_BAS_TMP6
                beq     @TokenFound
                iny
                tya
                sec
                adc     DP_BAS_TMP6+1
                sta     DP_BAS_TMP6+1
                bcc     @lpNextToken
                inc     DP_BAS_TMP6+2
                bra     @lpNextToken

@TokenFound:    ldy     #$00
@PrTokLp:       lda     (DP_BAS_TMP6+1),y
                bmi     @skTokEnd
                jsr     list_printA
                iny
                bne     @PrTokLp
@skTokEnd:      ply
                rts

list_printHexByte:
                pha
                lsr     A
                lsr     A
                lsr     A
                lsr     A
                jsr     list_printHexNybble
                pla
                and     #$0f
list_printHexNybble:
                cmp     #$0a
                bcc     list_printAasDigit
                adc     #$06
list_printAasDigit:
                adc     #'0'
list_printCheckPRLINCOUNT:
                pha
                lda     DP_BAS_WIDTH
                cmp     DP_BAS_COUNT
                bcs     list_printCheckPRLINCOUNT_sk1
                jsr     PrintCRLFresetCOUNT
list_printCheckPRLINCOUNT_sk1:
                pla
                inc     DP_BAS_COUNT
                jmp     call_OSWRCH

list_printHexByteAndSpace:
                jsr     list_printHexByte
list_print1Space:
                lda     #' '
list_printA:    bit     DP_BAS_LISTO
                bmi     list_printToVARTOP
list_printANoEDIT:
                cmp     #$0d
                bne     list_printCheckPRLINCOUNT
                jsr     call_OSWRCH
                jmp     COUNTeq0

list_printToVARTOP:
                sta     [DP_BAS_VARTOP]
                inc     DP_BAS_VARTOP
                bne     rtsL9A48
                inc     DP_BAS_VARTOP+1
                bne     @sk2
                inc     DP_BAS_VARTOP+2
@sk2:           pha
                lda     DP_BAS_VARTOP+2
                eor     DP_BAS_HIMEM+2
                bne     @sk
                lda     DP_BAS_VARTOP+1
                eor     DP_BAS_HIMEM+2
                bne     @sk
                brl     clrIntBrkNoRoom

@sk:            pla
                rts

doLISTOSpacesCLC:
                clc
doLISTOSpaces:  and     DP_BAS_LISTO
                beq     rtsL9A48
                txa
                bmi     rtsL9A48
                rol     A
                tax
                beq     rtsL9A48
list_printXSpaces:
                jsr     list_print1Space
                dex
                bne     list_printXSpaces
rtsL9A48:       rts

storeWAatDPX:   lda     DP_BAS_INT_WA
                sta     $00,x
                lda     DP_BAS_INT_WA+1
                sta     $01,x
                lda     DP_BAS_INT_WA+2
                sta     $02,x
                lda     DP_BAS_INT_WA+3
                sta     $03,x
                rts

jmpbrk_00_NoRoom:
                jmp     brk_00_NoRoom

doLOAD:         lda     DP_BAS_PAGE
                cmp     #$00
                lda     DP_BAS_PAGE+1
                sbc     DP_BAS_LOMEM_LIM_PAG
                lda     DP_BAS_PAGE+2
                sbc     DP_BAS_LOMEM_LIM_PAG+1
                bcc     jmpbrk_00_NoRoom
                lda     DP_BAS_PAGE
                cmp     DP_BAS_MEMEND
                lda     DP_BAS_PAGE+1
                sbc     DP_BAS_MEMEND+1
                lda     DP_BAS_PAGE+2
                sbc     DP_BAS_MEMEND+2
                bcs     jmpbrk_00_NoRoom
                jsr     evalYExStringStPAGEat61
        .IFDEF COMMUNICATOR
                rep     #$30
                .a16
                .i16
                sec
                lda     DP_BAS_MEMEND
                sbc     DP_BAS_PAGE
                sta     DP_FP_TMP
                lda     DP_BAS_MEMEND+2
                sbc     DP_BAS_PAGE+2
                and     #$00ff
                sta     DP_FP_TMP+2
                phb
                lda     #DP_BAS_TMP6
                cop     COP_24_OPCVD
                cop     COP_4E_OPLOD
                jsr     doCommandFailed
                plb
                sep     #$30
                .a8
                .i8
        .ENDIF
        .IFDEF MOS
                ; this is a bit convoluted as we need to call mos to load
                ; chunks of the file into low memory then copy to high
                ; TODO use MVN/MVP
                ; Unlike the communicator version, which seems to have bounds
                ; checking this just loads the whole file blind and 
                ; doesn't worry about what it crashes into

                jsr     FileNameToLowMem

        .IFDEF DOSSY
                lda     #<BANK0_SCRATCH_PAGE
                sta     f:BANK0_OSWORD_BLOCK
                lda     #>BANK0_SCRATCH_PAGE
                sta     f:BANK0_OSWORD_BLOCK+1
                
                lda     DP_BAS_PAGE
                sta     f:BANK0_OSWORD_BLOCK+2
                lda     DP_BAS_PAGE+1
                sta     f:BANK0_OSWORD_BLOCK+3
                lda     DP_BAS_PAGE+2
                sta     f:BANK0_OSWORD_BLOCK+4
                lda     #0
                sta     f:BANK0_OSWORD_BLOCK+5
                sta     f:BANK0_OSWORD_BLOCK+6

                ldx     #<BANK0_OSWORD_BLOCK
                ldy     #>BANK0_OSWORD_BLOCK
                dec     A
                jsl     nat_OSFILE
                tay
                beq     brk_D6_fileNotFound


        .ELSE
                ; OPENIN the file
                lda     #OSFIND_OPENIN
                ldx     #<BANK0_SCRATCH_PAGE
                ldy     #>BANK0_SCRATCH_PAGE
                jsl     nat_OSFIND

                tay
                beq     brk_D6_fileNotFound

                ;TODO: this is slow improve with GBPB?
@loadloop:      jsl     nat_OSBGET
                bcs     @eoff
                sta     [DP_BAS_TMP6+12]
                inc     DP_BAS_TMP6+12
                bne     @loadloop
                inc     DP_BAS_TMP6+13
                bne     @loadloop
                inc     DP_BAS_TMP6+14
                bne     @loadloop
@eoff:          lda     #0
                jsl     nat_OSFIND
        .ENDIF

        .ENDIF
findTOP:        lda     DP_BAS_PAGE+2
                sta     DP_BAS_TOP+2
                lda     DP_BAS_PAGE+1
                sta     DP_BAS_TOP+1
                lda     DP_BAS_PAGE
                sta     DP_BAS_TOP
                ldy     #$01
@lp:            lda     [DP_BAS_TOP]
                cmp     #$0d
                bne     printBadProgram
                lda     [DP_BAS_TOP],y
                bmi     incYaddtoTOP
                ldy     #$03
                lda     [DP_BAS_TOP],y
                beq     printBadProgram
                clc
                jsr     addAtoTOP
                bra     @lp

incYaddtoTOP:   iny
clcAddYtoTOP:   clc
addYtoYOP:      tya
addAtoTOP:      adc     DP_BAS_TOP
                sta     DP_BAS_TOP
                bcc     @sk
                inc     DP_BAS_TOP+1
                bne     @sk
                inc     DP_BAS_TOP+2
@sk:            ldy     #$01
                rts

        .IFDEF MOS
brk_D6_fileNotFound:
                brk     $D6
                .byte   "File not found", 0
brk_SaveOpenOutErrpr:
                brk     $FE                     ; TODO: not sure this is right!
                .byte   "Bad command",0
FileNameToLowMem:
                ; copy the filename to low memory
                ldy     #0
                ldx     #0
@lp:            lda     [DP_BAS_TMP6],Y
                sta     f:BANK0_SCRATCH_PAGE,X
                cmp     #$0D
                beq     @sk
                iny
                inx
                bne     @lp
@sk:            rts

        .ENDIF


printBadProgram:
                jsr     printStringAfter
                .byte   $0d,"Bad program",$0d
                nop
        .IFDEF COMMUNICATOR
                stz     DP_BAS_ARG2_FLAG
        .ENDIF
                jmp     reset_prog_prompt

jmp_brk06_type_mismatch2:
                jmp     brk06_type_mismatch

strAtTermPTRatTMP6:     
                lda     DP_BAS_STRWKSP_L
                sta     DP_BAS_TMP6
                lda     DP_BAS_STRWKSP_L+1
                sta     DP_BAS_TMP6+1
                lda     DP_BAS_STRWKSP_L+2
                sta     DP_BAS_TMP6+2
                stz     DP_BAS_TMP6+3
strTerm:        ldy     DP_BAS_STRLEN
                lda     #$0d
                sta     [DP_BAS_STRWKSP_L],y
                rts

evalYExpectString:
                jsr     evalExpressionMAIN
                bne     jmp_brk06_type_mismatch2
                jsr     strAtTermPTRatTMP6
                jmp     scanNextStmt

evalYExStringStPAGEat61:
                jsr     evalYExpectString
                lda     DP_BAS_PAGE
                sta     DP_BAS_TMP6+12
                lda     DP_BAS_PAGE+1
                sta     DP_BAS_TMP6+13
                lda     DP_BAS_PAGE+2
                sta     DP_BAS_TMP6+14
                stz     DP_BAS_TMP6+15
                rts

L9B1D:          ldx     DP_BAS_PAGE+2
                stx     DP_BAS_TMP6+6
                ldy     #$00
                sty     DP_FPB_sgn
                rts

exec_SAVE:      jsr     findTOP
                jsr     evalYExStringStPAGEat61
        .IFDEF COMMUNICATOR
                rep     #$30
                .a16
                .i16
                stz     DP_BAS_TMP6+4
                stz     DB_BAS_UNK_5A_6+1
                stz     DB_BAS_UNK_5A_6+3
                stz     DP_FPB_mant
                sec
                lda     DP_BAS_TOP
                sbc     DP_BAS_PAGE
                sta     DP_FP_TMP
                lda     DP_BAS_TOP+2
                sbc     DP_BAS_PAGE+2
                and     #$00ff
                sta     DP_FP_TMP+2
                phb
                lda     #DP_BAS_TMP6
                cop     COP_24_OPCVD
                cop     COP_4F_OPSAV
                jsr     doCommandFailed
                plb
                sep     #$30
                .a8
                .i8
        .ENDIF
        .IFDEF MOS



        .IFDEF DOSSY

                jsr     FileNameToLowMem

                lda     #<BANK0_SCRATCH_PAGE
                sta     f:BANK0_OSWORD_BLOCK
                lda     #>BANK0_SCRATCH_PAGE
                sta     f:BANK0_OSWORD_BLOCK+1
                
                lda     DP_BAS_PAGE
                sta     f:BANK0_OSWORD_BLOCK+2
                sta     f:BANK0_OSWORD_BLOCK+6
                sta     f:BANK0_OSWORD_BLOCK+10
                lda     DP_BAS_PAGE+1
                sta     f:BANK0_OSWORD_BLOCK+3
                sta     f:BANK0_OSWORD_BLOCK+7
                sta     f:BANK0_OSWORD_BLOCK+11
                lda     DP_BAS_PAGE+2
                sta     f:BANK0_OSWORD_BLOCK+4
                sta     f:BANK0_OSWORD_BLOCK+8
                sta     f:BANK0_OSWORD_BLOCK+12
                lda     #0
                sta     f:BANK0_OSWORD_BLOCK+5
                sta     f:BANK0_OSWORD_BLOCK+9
                sta     f:BANK0_OSWORD_BLOCK+13
                sta     f:BANK0_OSWORD_BLOCK+17

                lda     DP_BAS_TOP
                sta     f:BANK0_OSWORD_BLOCK+14
                lda     DP_BAS_TOP+1
                sta     f:BANK0_OSWORD_BLOCK+15
                lda     DP_BAS_TOP+2
                sta     f:BANK0_OSWORD_BLOCK+16

                ldx     #<BANK0_OSWORD_BLOCK
                ldy     #>BANK0_OSWORD_BLOCK
                lda     #0
                jsl     nat_OSFILE


        .ELSE

                ; make a -ve count
                rep     #$30
                .a16
                .i16
                sec
                lda     DP_BAS_PAGE
                sbc     DP_BAS_TOP
                sta     DP_FP_TMP
                lda     DP_BAS_PAGE+2
                sbc     DP_BAS_TOP+2
                sta     DP_FP_TMP+2
                sep     #$30
                .a8
                .i8


                ; this is a bit convoluted as we need to call mos to save
                ; chunks of the file from low memory after copying from high
                ; TODO use MVN/MVP
                ; TODO set Load/Exec address?

                jsr     FileNameToLowMem

                ; OPENOUT the file
                lda     #OSFIND_OPENOUT
                ldx     #<BANK0_SCRATCH_PAGE
                ldy     #>BANK0_SCRATCH_PAGE
                jsl     nat_OSFIND

                tay
                bne     @skerr
                brl     brk_SaveOpenOutErrpr
@skerr:
                ;TODO: this is slow improve with GBPB?
@saveloop:      lda     [DP_BAS_TMP6+12]
                jsl     nat_OSBPUT
                
                inc     DP_BAS_TMP6+12
                bne     @sk
                inc     DP_BAS_TMP6+13
                bne     @sk
                inc     DP_BAS_TMP6+14
@sk:

                inc     DP_FP_TMP
                bne     @saveloop
                inc     DP_FP_TMP+1
                bne     @saveloop
                inc     DP_FP_TMP+2
                bne     @saveloop
@eoff:          lda     #0
                jsl     nat_OSFIND
        .ENDIF ;DOSSY

        .ENDIF
                bra     jmpEOS

exec_OSCLI:     jsr     evalYExpectString
        .IFDEF COMMUNICATOR
                phb
                lda     DP_BAS_STRWKSP_L+2
                pha
                plb
                lda     DP_BAS_STRWKSP_L+1
                xba
                lda     DP_BAS_STRWKSP_L
                rep     #$30
                .a16
                .i16
                cop     COP_0E_OPCOM
                jsr     setREPORT_BHA_Cy
                sep     #$30
                .a8
                .i8
                plb
        .ENDIF
        .IFDEF MOS
                ;TODO move the copy to the native handler?
                phb
                ldy     #0
                phy
                plb
@lposc:         lda     [DP_BAS_STRWKSP_L],Y
                sta     BANK0_SCRATCH_PAGE,Y
                cmp     #$0D
                beq     @skosc
                iny
                bne     @lposc
@skosc:         ldx     #<BANK0_SCRATCH_PAGE
                ldy     #>BANK0_SCRATCH_PAGE
                plb
                jsl     nat_OSCLI
        .ENDIF
                bra     jmpEOS

doEXT:
        .IFDEF COMMUNICATOR
                jsr     parse_fileHandleHash_PTR2
                pei     (DP_BAS_INT_WA)
                jsr     parse_expectEQ_PTR_OFF
                jsr     checkTypeInVARTYPEConv2INT
                rep     #$30
                .a16
                .i16
                ply
                phb
                lda     #DP_BAS_INT_WA
                cop     COP_24_OPCVD
                cop     COP_58_OPWLL
                jsr     setREPORT_BHA_Cy
                plb
                sep     #$30
                .a8
                .i8
                brl     jmpEOS
        .ENDIF
        .IFDEF MOS
                lda     #3                              ; OSARGS FN num
                bra     mosSetFI
        .ENDIF

exec_PTRc:      
        .IFDEF COMMUNICATOR
                jsr     parse_fileHandleHash_PTR2
                pei     (DP_BAS_INT_WA)
                jsr     parse_expectEQ_PTR_OFF
                jsr     checkTypeInVARTYPEConv2INT
                rep     #$30
                .a16
                .i16
                ply
                phb
                lda     #DP_BAS_INT_WA
                cop     COP_24_OPCVD
                cop     COP_55_OPWSP
                jsr     setREPORT_BHA_Cy
                plb
                sep     #$30
                .a8
                .i8
        .IF !.defined(OPTIMIZE)
                brl     jmpEOS                          ;TODO: dead code
        .ENDIF
jmpEOS:         jmp     continue
        .ENDIF
        .IFDEF MOS
                lda     #1                              ; OSARGS FN num
mosSetFI:       pha
                jsr     parse_fileHandleHash_PTR2
                lda     DP_BAS_INT_WA
                pha
                jsr     parse_expectEQ_PTR_OFF
                jsr     checkTypeInVARTYPEConv2INT    
                ldx     #3
@lp:            lda     DP_BAS_INT_WA,X
                sta     f:MOS_ZP_TMP,X
                dex
                bpl     @lp
                ply
                pla
                ldx     #MOS_ZP_TMP
                jsl     nat_OSARGS
jmpEOS:
                jmp     continue

        .ENDIF

exec_CLOSE:     jsr     parse_fileHandleHash_PTR2
                jsr     parse_nextstmt_yield_PTR2_OFF
        .IFDEF COMMUNICATOR
                rep     #$30
                .a16
                .i16
                ldy     DP_BAS_INT_WA
                phb
                cop     COP_45_OPCLS
                jsr     setREPORT_BHA_Cy
                plb
                sep     #$30
                .a8
                .i8
        .ENDIF
        .IFDEF MOS
                lda     #0                              ; CLOSE
                ldy     DP_BAS_INT_WA
                jsl     nat_OSFIND
        .ENDIF
                bra     jmpEOS

exec_BPUT:      jsr     parse_fileHandleHash_PTR2
                jsr     stack_INTWA
                jsr     parse_skip_spaces_CMPcommaBRK
                jsr     evalAtOFF
                bne     POPfileHandleBPUT_A
                jsr     parse_skip_spaces
                eor     #';'                            ;TODO: work this out
                cmp     #$01
                php
                bcc     @sk
                dec     DP_BAS_TXTPTR_OFF
@sk:            jsr     parse_nextstmt_yield_PTR2_OFF
                jsr     popIntA
                ldy     #$00
                bra     BPUT_string

BPUT_string_lp: lda     [DP_BAS_STRWKSP_L],y
                phy
        .IFDEF COMMUNICATOR
                rep     #$10
                .i16
                ldy     DP_BAS_INT_WA
                phb
                cop     COP_0B_OPBPT
                jsr     setREPORT_BHA_Cy
                plb
                sep     #$10
                .i8
        .ENDIF
        .IFDEF MOS
                ldy     DP_BAS_INT_WA
                jsl     nat_OSBPUT
        .ENDIF
                ply
                iny
BPUT_string:    cpy     DP_BAS_STRLEN
                bcc     BPUT_string_lp
                plp
                bcc     jmpEOS
                lda     #$0d
                bra     BPUT_A

POPfileHandleBPUT_A:
                jsr     IfMIConvertToINT
                jsr     parse_nextstmt_yield_PTR2_OFF
                lda     DP_BAS_INT_WA
                pha
                jsr     popIntA
                pla
BPUT_A:         
        .IFDEF COMMUNICATOR
                rep     #$10
                .i16
                ldy     DP_BAS_INT_WA
                phb
                cop     COP_0B_OPBPT
                jsr     setREPORT_BHA_Cy
                plb
                sep     #$10
                .i8
        .ENDIF
        .IFDEF MOS
                ldy     DP_BAS_INT_WA
                jsl     nat_OSBPUT
        .ENDIF
                bra     jmpEOS

printStringAfter:
                pla
                sta     DP_BAS_TMP6
                pla
                sta     DP_BAS_TMP6+1
                phk
                pla
                sta     DP_BAS_TMP6+2
                bra     printTMP6ptr

L9C2E:          jsr     doOSASCI
printTMP6ptr:   jsr     incTMP6ptrLDA
                bpl     L9C2E
                phk
                pei     (DP_BAS_TMP6)
                php
                rti

;;TODO: looks like dead code possibly!
                lda     #OSWORD_5_READ_IO_MEM
                phx
                ldx     #DP_BAS_INT_WA
                ldy     #>DP_BAS_INT_WA
        .IFDEF COMMUNICATOR
                jsr     cop_OSWORD
        .ENDIF
        .IFDEF MOS
                jsr     MOS_OSWORD
        .ENDIF
                plx
                lda     DP_FPA_sgn
INT_inc_WA:     inc     DP_BAS_INT_WA
                bne     @rtsL9C56
                inc     DP_BAS_INT_WA+1
                bne     @rtsL9C56
                inc     DP_BAS_INT_WA+2
                bne     @rtsL9C56
                inc     DP_BAS_INT_WA+3
@rtsL9C56:      rts

NEW_int_reset_TOP_empty_prog:
                lda     #$0d
                ldy     DP_BAS_PAGE+2
                sty     DP_BAS_TOP+2
                ldy     DP_BAS_PAGE+1
                sty     DP_BAS_TOP+1
                ldy     DP_BAS_PAGE
                sty     DP_BAS_TOP
                stz     DP_BAS_TRACEFLAG
                sta     [DP_BAS_TOP]
                lda     #$ff
                ldy     #$01
                sta     [DP_BAS_TOP],y
                iny
                sty     DP_BAS_TOP
                rts

        .IFDEF COMMUNICATOR
call_OSBYTE:    cop     COP_06_OPOSB
                rts

cop_OSWORD:     cop     COP_07_OPOSW
                rts
        .ENDIF
        .IFDEF MOS
                ; TODO: some space could be saved in the other OSWORD calls
                ; by calling this?
MOS_OSWORD:
                ; need to copy the OSWORD block to somewhere that MOS understands
                ; and then copy params back afterwards
                ; OSWORD 0 is just called straight
                ; OSWORD 1..7F  16 bytes are copied to Bank0 block and copied back after call
                ; OSWORD >=80   block specifes # of input/output params
                ;
                ; assumptions:
                ; All blocks are in current DP!
                ; Y is 0 on entry
                ; No flags are returned (OSWORD 0 will not work)
                ; Y,X are undefined on exit
                ; all OSWORDs called by BASIC are <$80
                ; TODO: this is dangerous in that any OSWORD that doesn't play
                ; nice will overwrite DP stuff...but no worse than normal MOS!
                sta     DP_BAS_BL_OSWORD_CODE           ; save OSWORD code
                phx                                     ; push pointer

                ; block is in DP at X and we will copy 16 bytes
                ; we have to use B=0, MOS_BASIC_DP,Y to access as can't have FAR,Y!
                txy
                ldx     #0
                phx
                plb                                     ; databank to point at DP
@inlp:          lda     MOS_BASIC_DP,Y
                sta     f:BANK0_OSWORD_BLOCK,X
                inx
                iny
                cpx     #16
                bne     @inlp

                lda     DP_BAS_BL_OSWORD_CODE
                ldx     #<BANK0_OSWORD_BLOCK
                ldy     #>BANK0_OSWORD_BLOCK
                jsl     nat_OSWORD

                ply
                ldx     #0
                phx
                plb                                     ; databank to point at DP
@outlp:         lda     f:BANK0_OSWORD_BLOCK,X
                sta     MOS_BASIC_DP,Y
                inx
                iny
                cpx     #16
                bne     @outlp

                phk
                plb

                rts


call_OSBYTE:
                jsl     nat_OSBYTE
                rts
        .ENDIF

printCRLF:      lda     #$0a                            ;;TODO OSNEWL instead?
                jsr     call_OSWRCH
                lda     #$0d
                jsr     call_OSWRCH
                rts

doOSASCI:       jsr     call_OSWRCH                     ;TODO - call OSASCI instead?
                cmp     #$0d
                bne     @ret
                jsr     printCRLF
@ret:           rts

call_OSRDCH:    
        .IFDEF COMMUNICATOR
                phx
                phy
@L9C91:         cop     COP_04_OPRDC                    ;OSRDCH
                bcc     @L9CA2
                cmp     #$1b
                beq     @L9CA2
                jsr     ONERROROFF
                lda     #$03
                cop     COP_16_OPAEV                    ;Ack non-escape error
                bra     @L9C91

@L9CA2:         ply
                plx
                rts
        .ENDIF
        .IFDEF MOS
                ;TODO: call direct in GET?
                jsl     nat_OSRDCH
                rts
        .ENDIF

call_OSWRCH:    
        .IFDEF COMMUNICATOR
                pha
                phy
                phx
                cop     COP_00_OPWRC
                plx
                ply
                pla
                rts
        .ENDIF
        .IFDEF MOS

                jsl     nat_OSWRCH                      ;TODO: check and use EQUate?                
                rts

        .ENDIF

                pha                                     ;TODO: dead code?
                lda     DP_BAS_00_REAL_UK+1,x
                bne     @L9CC2
                stz     DP_BAS_00_REAL_UK+2,x
                clc
                tdc
                adc     DP_BAS_00_REAL_UK,x
                sta     DP_BAS_00_REAL_UK,x
                xba
                adc     DP_BAS_00_REAL_UK+1,x
                sta     DP_BAS_00_REAL_UK+1,x
                pla
                rts

@L9CC2:         phb
                pla
                sta     DP_BAS_00_REAL_UK+2,x
                pla
                rts

; This appears to be a linked list of @...% variables
tblBuiltInsLL:  .faraddr biUSEB
                .asciiz "USED%"
                .word   $0000
                .word   $0200
biUSEB:         .faraddr biWRDAM
                .asciiz "USEB%"
                .word   $0000
                .word   $8000
biWRDAM:        .faraddr biWRDXY
                .asciiz "WRDAM%"
                .word   $0000
                .word   $2000
biWRDXY:        .faraddr biWRD
                .asciiz "WRDXY%"
                .word   $0000
                .word   $1000
biWRD:          .faraddr biNONE
                .asciiz "WRD%"
                .word   $0000
                .word   $3000
biNONE:         .faraddr biA
                .word   $0000
                .word   $0000
biA:            .faraddr biX
                .asciiz "A%"
                .word   $0074
                .word   $0000
biX:            .faraddr biY
                .asciiz "X%"
                .word   $0078
                .word   $0000
biY:            .faraddr biD
                .asciiz "Y%"
                .word   $007c
                .word   $0000
biD:            .faraddr biB
                .asciiz "D%"
                .word   $0080
                .word   $0000
biB:            .faraddr biP
                .asciiz "B%"
                .word   $0084
                .word   $0000
biP:            .faraddr biC
                .asciiz "P%"
                .word   $0088
                .word   $0000
biC:            .faraddr biOPT
                .asciiz "C%"
                .word   $008c
                .word   $0000
biOPT:          .faraddr biBHA
                .asciiz "OPT%"
                .word   $0043
                .word   $0000
biBHA:          .faraddr biDATA
                .asciiz "BHA%"
                .word   $00c2
                .word   $0000
biDATA:         .faraddr biERC
                .asciiz "DATA%"
                .word   $0033
                .word   $0000
biERC:          .faraddr biERX
                .asciiz "ERC%"
                .word   $0000
                .word   $0000
biERX:          .faraddr biERRMSG
                .asciiz "ERX%"
                .word   $0004
                .word   $0000
biERRMSG:       .faraddr biEndOfList
                .asciiz "ERMSG%"
                .word   $0008
                .word   $0000
biEndOfList:    .byte   $00
                .byte   $00
                .byte   $00
        .IFDEF COMMUNICATOR
arith_get_reference:
                ldx     #DP_BAS_ARITHMODREF
                ldy     #$00
                cop     COP_26_OPBHA                    ;get address of "ARITHMETIC"
                .byte   "ARITHMETIC"
                .byte   $00

                cop     COP_29_OPRFR                    ;Get reference to Arithmetic module
                bcc     @L9DC6                          ;all's well return
                lda     #<brk_0_NoArith-1               ;?? Bodge the Arith block to reference the BRK
                sta     DP_BAS_ARITHMODREF              ;and fail when any arithmetic called?
                lda     #>brk_0_NoArith
                sta     DP_BAS_ARITHMODREF+1
                phk
                pla
                sta     DP_BAS_ARITHMODREF+2
@L9DC6:         rts

brk_0_NoArith:  brk     $00

                .byte   "Cannot find ARITHMETIC module"
                .byte   $00

moduleCallARITHref:
                pei     (DP_BAS_ARITHMODREF+4)
                pei     (DP_BAS_ARITHMODREF+2)
                pei     (DP_BAS_ARITHMODREF)
                rtl

                .byte   $ea                             ;CRC?
                .byte   $5f
                .byte   $d4
                .byte   $ff
        .ENDIF ;COMMUNICATOR
        .IFDEF MOS

                .include "bas816new_natshims.asm"
        .ENDIF


