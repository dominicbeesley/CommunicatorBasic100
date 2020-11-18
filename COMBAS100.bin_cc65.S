; Target assembler: cc65 v2.18.0 [--target none -C COMBAS100.bin_cc65.cfg]
;*******************************************************************************
;* Created with 6502bench SourceGen v1.7.3                                     *
;*******************************************************************************
;* Acorn Communicator BASIC 1.00                                               *
;*******************************************************************************
;* Disassembly and annotations by Dominic Beesley November 2020                *
;*******************************************************************************
         .setcpu "65816"
OSWORD_1_READTIME = 1
_COCRE   =       2
_COENV   =       4
_CKREAD  =       6
_CODEL   =       6          ;Delete a coroutine
COP_OSBGT =      $0a        ;Call OSBGET
_COBRK   =       $0c
_MMALA   =       $0c        ;Allocate large ascending
_OPERR   =       $0f
COP_11_OPADF =   $11        ;Allocate a DP
ARITH_FN_SIN =   $12
_STSBK   =       $14        ;Get BRK signature
ARITH_FN_COS =   $14
_STTBK   =       $16        ;Get BRK text
ARITH_FN_TAN =   $16
ARITH_FN_ACS =   $18
COP_OPRLH =      $18
ARITH_FN_ASN =   $1a
COP_1A_OPFZB =   $1a
ARITH_FN_ATN =   $1c
ARITH_FN_DEG =   $1e
_STBMB   =       $20        ;?
ARITH_FN_RAD =   $20
ARITH_FN_LOG =   $22
ARITH_FN_LN =    $24
COP_24_OPCVD =   $24        ;Convert direct page ptr to long pointer
ARITH_FN_EXP =   $26
COP_26_OPBHA =   $26        ;Get string banked address
_OPCMD   =       $28
ARITH_FN_SQR =   $28
COP_29_OPRFR =   $29
COP_2C_OPFMA =   $2c
COP_2E_OPFPO =   $2e        ;Find pool owner
FN_ARITH_INT =   $30
RETV_INT =       $40
COP_54_OPRSP =   $54        ;Get PTR#
COP_57_OPRLL =   $57        ;Get EXT#
COP_5D_OPSTAR =  $5d        ;OSCLI?
COP_61_OPERC =   $61        ;Convert error number to string
tknSTEP  =       $88
tknLineNo =      $8d        ;line number marker
tknMissing =     $8d
tknOPENIN =      $8e
VAR_OFFS_PROC =  $b1        ;Offset in variable linked list pointer table for PROCs
VAR_OFFS_END =   $c0        ;Byte offset after end of variable linked list heads
tknEOF   =       $c5
tknPROC  =       $f2
tknREPORT =      $f6
RETV_REAL =      $ff        ;VarType = real

DP_BAS_Report =  $08
DP_BAS_LOMEM =   $10
DP_BAS_VARTOP =  $13        ;end of variables, above LOMEM, below STACK
DP_BAS_STACK =   $16        ;Set when HIMEM is set
DP_BAS_HIMEM =   $19
DP_BAS_ERL =     $1c
DP_BAS_TXTPTR2_OFF = $1e
DP_BAS_TXTPTR2 = $1f
DP_BAS_RAND =    $22        ;Random seed
DP_BAS_TOP =     $27
DP_BAS_TMP2A =   $2a
DP_BAS_ONERRORPTR = $2c
DP_BAS_TXTPTR =  $2f
DP_BAS_TXTPTR_OFF = $32
DP_BAS_DATA_PTR = $33       ;Unknown pointer, set to PAGE at start
DP_BAS_COUNT =   $37
DP_BAS_LISTO =   $38
DP_BAS_TRACEFLAG = $39
DP_BAS_TRACE_LIM = $3a
DP_BAS_WIDTH =   $3c
DP_BAS_REPEAT_LVL = $3d
DP_BAS_GOSUB_LVL = $3e
DP_BAS_FOR_LVL = $3f
DP_BAS_OPT =     $42
DP_BAS_INT_WA =  $48        ;c.f. 6502 2A
DP_FPA_sgn =     $4c
DP_FPA_exp =     $4d
DP_FPA_mant =    $4f
DP_BAS_STRLEN =  $54
DP_BAS_TMP6 =    $55
DP_FPB_sgn =     $5c
DP_FPB_exp =     $5e
DP_FPB_mant =    $5f
DP_BAS_TMP64 =   $64
DP_FP_TMP =      $65
DP_BAS_FP_TMPEXP = $6b
DP_BAS_FP_PTR1 = $6c
DP_stack_save =  $72
DP_BAS_CURCHAN = $90
DP_BAS_MEMEND =  $92
DP_BAS_CO_FLAGS = $97       ;Seems to be a set of flags to decide what state we're in and whether to yield? - bit $40 set disables yeild
DP_BAS_ARG2_FLAG = $99
DP_BAS_ARG2 =    $9a
DP_BAS_MEMSIZE = $9d
DP_BAS_CO_CTDN = $a0        ;Seems to count down as statements are execed and then yield every 100th?
DP_BAS_MEMBASE = $a4        ;Base of workspace
DP_BAS_STRWKSP_L = $a7
DP_BAS_ASSWKSP = $aa
DP_BAS_VARS_BASE = $ad      ;Pointers to VARS?
DP_BAS_PRINT_FP_PTR = $b0   ;Used in PRINT HASH
DP_BAS_FOR_STACKBASE = $b3
VAR_OFFS_FN =    $b4        ;Offset if variable linked list pointer table for FNs
DP_BAS_GOSUB_STACKBASE = $b6
DP_BAS_REPEAT_STACKBASE = $b9
DP_BAS_pointer_at_copy_of_PAGE = $bc
DP_BAS_PAGE =    $bf
DP_BAS_SET_TO_Z_AT_EOS = $c6
DP_BAS_LAST_EOSEOR_D_ = $c8 ;Set to end of statement marker EOR D
DP_BAS_ARITHMODREF = $c9
DP_BAS_STACKSAVE2 = $d1
DP_BAS_OLD_DP =  $d3
_ST      =       $ff88
_MM      =       $ff8c
_CO      =       $ff90
_CCO     =       $ff94      ;Call coroutine
_CWT     =       $ff98
_EV      =       $ffb0

;         .segment "SEG000"
         .org    $4b00
         .a16
         .i16
         brl     L4B1B

         .word   $52ef      ;TODO - module length, calculate
         .byte   $00        ;module length top byte
         .word   $0100      ;Version
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

L4B1B:   brl     ServiceDispatch

         .byte   $60
         .byte   $26
         .byte   $04
         .byte   "BASIC"
         .byte   $00
strCopyright:
         .byte   "(C)1986 Acorn",$0a,$0d
         .byte   $00
         .byte   $00
         .byte   $4b
; Floating point constants - TODO: make macros?
; TODO: add assert to make not cross page boundary, constant loading code is
; page sensitive
fpConstMinPiDiv2:
         .byte   $81
         .byte   $c9
         .byte   $10
         .byte   $00
         .byte   $00
fpConst4_454e6:
         .byte   $6f
         .byte   $15
         .byte   $77
         .byte   $7a
         .byte   $61
fpConstPiDiv2:
         .byte   $81
         .byte   $49
         .byte   $0f
         .byte   $da
         .byte   $a2
;*******************************************************************************
;* Following is a two-part table of Hi, Lo bytes of 16bit words that are       *
;* succcessively subtracted from INT WA to convert binary to decimal for       *
;* printing line numbers                                                       *
;*******************************************************************************
tblDigitsHi:
         .byte   $00
         .byte   $00
         .byte   $00
         .byte   $03
         .byte   $27
tblDigitsLo:
         .byte   $01
         .byte   $0a
         .byte   $64
         .byte   $e8
         .byte   $10
;*******************************************************************************
;* Following is a list of constants to form the workspace pointers given a     *
;* base address.                                                               *
;*******************************************************************************
tblWkspSizes:
         .word   $0100      ;MEMBASE to STRWKSP (+100)
         .word   $0100      ;STRWKSP to ??? (+200)
         .word   $006c      ;STRWKSP to VARS (+26C)
         .word   $00b7      ;VARS to PRINTHASH (+323)
         .word   $0014      ;PRINTHASH to FORSTACK (+337)
         .word   $00aa      ;FORSTACK to GOSUBSTACK (+3E1)
         .word   $004e      ;GOSUBSTACK to REPEATSTACK (+42F)
         .word   $003c      ;REPEATSTACK to ??? (+46B)
         .word   $001e      ;??? TO PAGE (+500)
         .word   $0000

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

@ServiceUnk:
         phk
         plb
         lda     #strUkServ
         ldx     #$0002
         clc
         jsl     _CWT
         bra     ServiceDispatch

strUkServ:
         .byte   "BASIC - unrecognised reason code",$0d
         .byte   $00
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
ServiceCall_4:
         jsr     HELPENV
         pla
         jsr     L4C14
         bcs     @L4BD0
         jsr     memFreeDP
         bcs     @L4BF2
         lda     #$0000
         pha
         plb
         plb
         ldy     #$0004
         sta     ($01,S),y
         ldx     #$0000
         rtl

@L4BD0:  lda     #$4bd9
@L4BD3:  phk
         plb
         ldx     #$0002
         rtl

         .byte   "Cannot find memory pool",$0d
         .byte   $00

@L4BF2:  lda     #$4bf7
         bra     @L4BD3

         .byte   "Cannot free direct page",$0d
         .byte   $00

         .a8
         .i8
memFreeDP:
         tdc
         cop     COP_1A_OPFZB
         rts

         .a16
         .i16
L4C14:   rep     #$30
         lda     DP_BAS_MEMEND+2
         and     #$ff00
         pei     ($95)
         plb
         plb
         ldx     #$0014
         jsl     _MM
         bcs     @L4C2F
         ldx     #$0002
         jsl     _MM
@L4C2F:  rts

;*******************************************************************************
;* Service Call 0                                                              *
;*******************************************************************************
;* Args [size] [????]                                                          *
;*******************************************************************************
ServiceCall_0_2:
         phb
         pha
         cop     COP_11_OPADF
         .word   $0100      ;Alloc 256 byte DP
         bcc     L4C6B
         phk
         plb
         lda     #ABRT_DP_FAIL
         ldx     #$0002
         sec
         jsl     _CWT
         cop     $0f
ABRT_DP_FAIL:
         .byte   "BASIC failed to allocate zero page",$0d
         .byte   $00

L4C6B:   pea     $0000
         plb
         plb
         ldy     #$0004
; Store new DP at a location pointed to just ABOVE our return address + 4?
; The above is in bank 0 somewhere?
         sta     ($06,S),y  ;TODO: WTF is going on here?
         tcd                ;use new DP
         sep     #$30
         .a8
         .i8
         phk
         plb
         ldy     #$00
         ldx     #_COENV
         jsl     _CO        ;Set DP / environment for coroutine
         tsc
         sta     DP_stack_save
         xba
         sta     DP_stack_save+1
         jsr     arith_get_reference ;Get ARITHMETIC module reference
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
@skiprndseed:
         per     strCopyright ;Set default for REPORT
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
         cop     COP_OPRLH  ;Parse HEX string
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
         bra     @skMEMS    ;BHA now contains MEMSIZE

@skNOMEMS:
         lda     #$00
         sta     DP_BAS_MEMSIZE+2
         pha
         plb
         lda     #$80
         sta     DP_BAS_MEMSIZE+1
         xba
         lda     #$00
         sta     DP_BAS_MEMSIZE ;MEMSIZE/BHA now contains 008000
@skMEMS: phd
         ldx     #_MMALA
         ldy     #$02
         jsl     _MM        ;Allocate a large ascending area of memory
         bcc     @memAllocOK
         jsr     memFreeDP
         phk
         plb
         lda     #$4d
         xba
         lda     #$0b
         ldx     #$02
         sec
         jsl     _CWT
         cop     _OPERR
         .byte   "BASIC failed to allocate workspace",$0d
         .byte   $00

@memAllocOK:
         pld
         phb
         sta     DP_BAS_MEMBASE
         xba
         sta     DP_BAS_MEMBASE+1
         pla
         sta     DP_BAS_MEMBASE+2
         xba
         beq     @memOnPage
         cop     _OPERR
         .byte   "BASIC work space not on page boundary"
         .byte   $00

@memOnPage:
         stz     DP_BAS_ARG2_FLAG
         rep     #$30
         .a16
         .i16
         pla
         plb
         ldx     #DP_BAS_ARG2
         ldy     #$0006
         cop     COP_OPRLH
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
@lptoend:
         lda     [DP_BAS_ARG2] ;if ARG2 wasnt a hex number then skip past it to <=32
         cmp     #$20
         inc     DP_BAS_ARG2
         bne     @L4D96
         inc     DP_BAS_ARG2+1
         bne     @L4D96
         inc     DP_BAS_ARG2+2
@L4D96:  bcs     @lptoend
@gotarg2_ne:
         inc     DP_BAS_ARG2_FLAG
@noarg2: clc
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
@awful1: cmp     #$00
         bne     @awful2
         inc     A
         sta     DP_BAS_MEMBASE+1
@awful2: phk
         plb
; Setup pointers to various workspace areas by adding values from a table
         ldx     #DP_BAS_MEMBASE
         ldy     #$00
         jsr     mem_add_16bconsts_to_3b_pointers
         clc
         lda     $00,x
         adc     tblWkspSizes,y
         pha
         lda     $01,x
         adc     tblWkspSizes+1,y
         sta     DP_BAS_PAGE+1
         lda     $02,x
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
@L4DE9:  ldy     #$00
         lda     #$00
         sta     [DP_BAS_pointer_at_copy_of_PAGE],y
         iny
         stz     DP_BAS_PAGE
         lda     DP_BAS_PAGE+1
         sta     [DP_BAS_pointer_at_copy_of_PAGE],y
         iny
         sta     $95
         lda     DP_BAS_PAGE+2
         sta     [DP_BAS_pointer_at_copy_of_PAGE],y
         iny
         sta     $96
         ldy     #$06
         sty     $98
         stz     DP_BAS_LISTO
         stz     DP_BAS_SET_TO_Z_AT_EOS
         lda     #$00
         ldy     #$02
         sta     [DP_BAS_ASSWKSP],y
         iny
         sta     [DP_BAS_ASSWKSP],y
         ldx     #$ff
         stx     DP_BAS_WIDTH
         tay
         lda     #$0a
         sta     [DP_BAS_ASSWKSP]
         dec     A
         iny
         sta     [DP_BAS_ASSWKSP],y
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
         .a16
         .i16
         bcc     L4E64
         jsr     L4C14
         jsr     memFreeDP
         phk
         plb
         lda     #ABRT_CANNOTSETBRK
         ldx     #$0002
         sec
         jsl     _CWT
         cop     $0f
ABRT_CANNOTSETBRK:
         .byte   "Cannot set BRK handler",$0d
         .byte   $00

L4E64:   cli
         jmp     L6275

;*******************************************************************************
;* Service Call 6                                                              *
;*******************************************************************************
;* This looks to create a coroutine                                            *
;*******************************************************************************
         .a8
         .i8
ServiceCall_6:
         pla
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
@ok:     phd
         phy
         phb
         ldx     #_COENV
         jsl     _CO        ;Set environment for coroutine
         plb
         ply
         pld
         bcc     @ok2
         ldx     #$0006
         bra     @notok

@ok2:    phy
         jsl     _CCO       ;Call the coroutine we just created
         ply
@notok:  php
         pha
         phx
         ldx     #_CODEL    ;Delete coroutine
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
@retCLC: plp
         clc
         rtl

         .a16
         .i16
COROUTINE_ENTRY:
         phd
         cop     COP_11_OPADF ;Allocate DP of 256 bytes
         .word   $0100

         plx
         bcs     L4F0D
         tay
         tcd
         phx
         lda     #$00d4
         mvn     #$00,#$00  ;Copy D5 bytes to new DP from old DP
         pla
         sta     DP_BAS_OLD_DP
         lda     DP_stack_save
         sta     DP_BAS_STACKSAVE2
         lda     #BRK_HANDLER_2 >> 8 ;TODO - WTF going on with XBA here? wrong mode?
         xba
         lda     #BRK_HANDLER_2-$4f00 ;this will overwrite B from above? TODO: fix to use <
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
         jsr     L730B
         bra     L4EEF

L4EEC:   clc
         phk
         plb
L4EEF:   rep     #$30
         .a16
         .i16
L4EF1:   pei     (DP_BAS_VARTOP+1)
         pei     (DP_BAS_VARTOP)
         pei     (DP_BAS_STACKSAVE2)
         pei     (DP_BAS_OLD_DP)
         php
         tdc
         cop     COP_1A_OPFZB ;Free DP
         plp
         pld                ;Restore old DP
         pla
         sta     DP_stack_save ;Restore old Stack Save
         pla
         plx
         sta     DP_BAS_VARTOP
         stx     DP_BAS_VARTOP+1
         ldx     #$0004
         bcc     L4F10
L4F0D:   ldx     #$0006
L4F10:   jsl     _CWT
BRK_HANDLER_2:
         cli
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
HELPENV: php
         rep     #$30
         phd
         cop     COP_11_OPADF ;Allocate 256 byte page of DP
         .word   $0100
         plx                ;Get back old D in X
         bcs     @fail      ;Didn't allocate - fail
         phx                ;Re-stack original D
         tay                ;put allocated DP base in Y
         tcd                ;Set D
         lda     #$00ff
         mvn     #$00,#$00  ;Move 256 from old DP to new DP
         tsc                ;Stack into C
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
         sta     DP_BAS_VARTOP ;keep lomem if changed by HELP?
         stx     DP_BAS_VARTOP+1
@fail:   plp
         rts

         .a8
         .i8
mem_add_16bconsts_to_3b_pointers:
         lda     tblWkspSizes+2,y ;Check ahead for 0 (don't do the last entry)
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

@rts:    rts

varFindPROCFN:
         iny
         sty     DP_BAS_TMP6+3
         ldy     #$01
         lda     [DP_BAS_TMP6],y
         ldy     #VAR_OFFS_PROC
         cmp     #tknPROC
         beq     varSearchLinkedList
         ldy     #VAR_OFFS_FN
         bra     varSearchLinkedList

varFind_00:
         iny
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
         beq     @rts       ;linked list head empty
         lda     [DP_BAS_VARS_BASE],y
         sta     DP_BAS_INT_WA+2
         dey
         lda     [DP_BAS_VARS_BASE],y
         sta     DP_BAS_INT_WA+1
         dey
         lda     [DP_BAS_VARS_BASE],y
         bra     varLinkedListSearchStart

@rts:    rts

varLinkedListSearchCompareChar:
         dey
         dey
         lda     [DP_BAS_TMP6],y
         cmp     #$40
         bne     L4FCC
         sep     #$40
         phb
         pla
         sta     DP_BAS_INT_WA+2
L4FCA:   ldy     #$01
L4FCC:   lda     [DP_BAS_INT_WA],y
         iny
         ora     [DP_BAS_INT_WA],y
         ora     [DP_BAS_INT_WA]
         beq     L503D
         lda     DP_FP_TMP+2
         bne     @L4FDD
         phb
         pla
         bra     @L4FDF

@L4FDD:  lda     [DP_BAS_INT_WA],y
@L4FDF:  xba
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
         cpy     DP_BAS_TMP6+3 ;check to see if that is the length we were after
         bne     varLinkedListSearchCompareChar
         bra     varLinkedListSearchFound

@L4FFA:  lda     [DP_BAS_INT_WA],y
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
         bcc     @L501B
         inc     DP_BAS_INT_WA+1
         bne     @L501B
         inc     DP_BAS_INT_WA+2
@L501B:  plp
         lda     #$ff
         bvc     L503D

         .byte   $a5
         .byte   $4b
         .byte   $48
         .byte   $da
         .byte   $20
         .byte   $7e
         .byte   $8b
         .byte   $a5
         .byte   $4a
         .byte   $25
         .byte   $4b
         .byte   $1a
         .byte   $d0
         .byte   $06
         .byte   $8b
         .byte   $68
         .byte   $85
         .byte   $4a
         .byte   $80
         .byte   $03
         .byte   $20
         .byte   $73
         .byte   $70
         .byte   $fa
         .byte   $68
         .byte   $85
         .byte   $4b
         .byte   $a9
         .byte   $ff

L503D:   rts

_ukCallEv23:
         phx
         lda     #$00
         xba
         lda     #$03
         ldx     #$02
         jsl     _EV
         plx
         rts

prog_search_lineno:
         lda     DP_BAS_PAGE
         sta     DP_FPB_exp
         lda     DP_BAS_PAGE+1
         sta     DP_FPB_mant
         lda     DP_BAS_PAGE+2
         sta     DP_FPB_mant+1 ;Point at start of program
prog_search_lineno_fromPTR:
         ldy     #$01
         lda     [DP_FPB_exp],y ;get line no hi byte
         cmp     DP_BAS_INT_WA+1 ;compare to line number in WA
         bcs     @skCkLo    ;;greater or equal
@nextline:
         ldy     #$03
         lda     [DP_FPB_exp],y ;get line length
         adc     DP_FPB_exp ;add to pointer and repeat
         sta     DP_FPB_exp
         bcc     prog_search_lineno_fromPTR
         inc     DP_FPB_mant
         bne     prog_search_lineno_fromPTR
         inc     DP_FPB_mant+1
         bra     prog_search_lineno_fromPTR

@skCkLo: bne     @retCLCY2  ;greater - exit
         iny
         lda     [DP_FPB_exp],y
         cmp     DP_BAS_INT_WA ;compare lo
         bcc     @nextline  ;was less continue search
         bne     @retCLCY2
         rts                ;returns with Cy=1 - exact match

@retCLCY2:
         ldy     #$02
         clc
         rts                ;failed to find return with Cy=0

L5082:   jsr     L6C8E
         lda     DP_BAS_INT_WA+3
         pha
         jsr     L83A4
         jsr     L784A
         stx     $40
         jsr     L6C8E
         pla
         sta     DP_BAS_TMP6+1
         eor     DP_BAS_INT_WA+3
         sta     DP_BAS_TMP6
         jsr     L83A4
         ldx     #$5a
         jsr     stack_copy6_to_X_uns4
         stz     DP_FPB_exp
         stz     DP_FPB_mant
         stz     DP_FPB_mant+1
         stz     DP_FPB_mant+2
         lda     DP_BAS_INT_WA+3
         ora     DP_BAS_INT_WA
         ora     DP_BAS_INT_WA+1
         ora     DP_BAS_INT_WA+2
         beq     L50FB
         ldy     #$20
@L50B6:  dey
         beq     @L50FA
         asl     DP_BAS_TMP6+5
         rol     $5b
         rol     DP_FPB_sgn
         rol     $5d
         bpl     @L50B6
@L50C3:  rol     DP_BAS_TMP6+5
         rol     $5b
         rol     DP_FPB_sgn
         rol     $5d
         rol     DP_FPB_exp
         rol     DP_FPB_mant
         rol     DP_FPB_mant+1
         rol     DP_FPB_mant+2
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
         lda     DP_FPB_mant+2
         sbc     DP_BAS_INT_WA+3
         bcc     @L50F5
         sta     DP_FPB_mant+2
         stx     DP_FPB_mant+1
         pla
         sta     DP_FPB_mant
         pla
         sta     DP_FPB_exp
         bcs     @L50F7

@L50F5:  pla
         pla
@L50F7:  dey
         bne     @L50C3
@L50FA:  rts

L50FB:   brk     $12

         .byte   "Division by zero"
         .byte   $00

L510E:   stz     DP_FPA_sgn
         stz     DP_FPA_exp+1
         stz     DP_FPA_exp
         stz     DP_FPA_mant
L5116:   rts

L5117:   bra     @L5142

         .byte   $80
         .byte   $02
         .byte   $80
         .byte   $12
         .byte   $00
         .byte   "F Iflt + 2 error "
         .byte   $00
         .byte   "G Iflt + 4 error "
         .byte   $00

@L5142:  stz     DP_FPA_mant+4
         stz     DP_FPA_exp
         lda     DP_BAS_INT_WA+3
         sta     DP_FPA_sgn
         bpl     @L5151
         jsr     L8400
         lda     DP_BAS_INT_WA+3
@L5151:  bne     @L5179
         stz     DP_FPA_mant+3
         lda     DP_BAS_INT_WA+2
         bne     @L516D
         stz     DP_FPA_mant+2
         lda     DP_BAS_INT_WA+1
         bne     @L5165
         stz     DP_FPA_mant+1
         lda     DP_BAS_INT_WA
         bra     @L5189

@L5165:  ldy     DP_BAS_INT_WA
         sty     DP_FPA_mant+1
         ldy     #$90
         bra     @L518B

@L516D:  ldy     DP_BAS_INT_WA+1
         sty     DP_FPA_mant+1
         ldy     DP_BAS_INT_WA
         sty     DP_FPA_mant+2
         ldy     #$98
         bra     @L518B

@L5179:  ldy     DP_BAS_INT_WA+2
         sty     DP_FPA_mant+1
         ldy     DP_BAS_INT_WA+1
         sty     DP_FPA_mant+2
         ldy     DP_BAS_INT_WA
         sty     DP_FPA_mant+3
         ldy     #$a0
         bra     @L518B

@L5189:  ldy     #$88
@L518B:  ora     #$00
         bmi     @L519E
         bne     @L5194
         brl     L510E

@L5194:  dey
         asl     DP_FPA_mant+3
         rol     DP_FPA_mant+2
         rol     DP_FPA_mant+1
         rol     A
         bpl     @L5194
@L519E:  sta     DP_FPA_mant
         sty     DP_FPA_exp+1
         rts

fpNormalizeFPA_bra:
         bra     fpNormalizeFPA ;TODO - optimize away!

         bra     L51FC      ;TODO: not called - optimize away

         bra     L5211      ;TODO: not called - optimize away

         bra     L5226      ;TODO: not called - optimize away

fpNormalizeFPA:
         lda     DP_FPA_mant
L51AD:   bpl     L51B2
         brl     L5116

L51B2:   bne     L51E4
         ora     DP_FPA_mant+1
         ora     DP_FPA_mant+2
         ora     DP_FPA_mant+3
         ora     DP_FPA_mant+4
         bne     @L51C1
         brl     L510E

@L51C1:  lda     DP_FPA_exp+1
@L51C3:  ldy     DP_FPA_mant+1
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
         bcs     @L51DC
         dec     DP_FPA_exp
@L51DC:  ldy     DP_FPA_mant
         beq     @L51C3
         bmi     L51F9
         bra     L51E6

L51E4:   lda     DP_FPA_exp+1
L51E6:   clc
@L51E7:  sbc     #$00
         bcs     @L51ED
         dec     DP_FPA_exp
@L51ED:  asl     DP_FPA_mant+4
         rol     DP_FPA_mant+3
         rol     DP_FPA_mant+2
         rol     DP_FPA_mant+1
         rol     DP_FPA_mant
         bpl     @L51E7
L51F9:   sta     DP_FPA_exp+1
         rts

L51FC:   brk     $2b

         .byte   "Fnrm+2 entry error"
         .byte   $00

L5211:   brk     $2c

         .byte   "Fnrm+4 entry error"
         .byte   $00

L5226:   brk     $2d

         .byte   "Fnrm+6 entry error"
         .byte   $00

L523B:   jsr     fpCopyFPAtoFPB
         jmp     fpzero_FPA

L5241:   lda     DP_FPA_exp+1
         bpl     L523B
         jsr     fpSetFPBto0
         ldy     DP_FPA_mant
         beq     L5290
@L524C:  lsr     DP_FPA_mant
         ror     DP_FPA_mant+1
         ror     DP_FPA_mant+2
         ror     DP_FPA_mant+3
         ror     DP_FPB_mant
         ror     DP_FPB_mant+1
         ror     DP_FPB_mant+2
         ror     DP_FPB_mant+3
         inc     A
         beq     L5289
@L525F:  cmp     #$a0
         bcs     L528C
         cmp     #$99
         bcs     @L524C
         adc     #$08
         ldy     DP_FPB_mant+2
         sty     DP_FPB_mant+3
         ldy     DP_FPB_mant+1
         sty     DP_FPB_mant+2
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

L5289:   jmp     L7ECD

L528C:   bne     L5289
         sta     DP_FPA_exp+1
L5290:   lda     DP_FPA_sgn
         bpl     L52AB
L5294:   sec
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
L52AB:   rts

L52AC:   inc     DP_FPA_mant+3
         bne     @L52BC
         inc     DP_FPA_mant+2
         bne     @L52BC
         inc     DP_FPA_mant+1
         bne     @L52BC
         inc     DP_FPA_mant
         beq     L5289
@L52BC:  rts

L52BD:   ldy     #$04
@L52BF:  ror     DP_BAS_RAND+4
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
         bne     @L52BF
         rts

L52E8:   lda     DP_FPB_sgn
         sta     DP_FPA_sgn
         stz     DP_FPA_exp
         lda     DP_FPB_exp
         sta     DP_FPA_exp+1
         lda     DP_FPB_mant
         sta     DP_FPA_mant
         lda     DP_FPB_mant+1
         sta     DP_FPA_mant+1
         lda     DP_FPB_mant+2
         sta     DP_FPA_mant+2
         lda     DP_FPB_mant+3
         sta     DP_FPA_mant+3
         lda     DP_FPB_mant+4
         sta     DP_FPA_mant+4
L5306:   rts

L5307:   lda     DP_FPA_mant
         beq     L52E8
         sec
         lda     DP_FPA_exp+1
         sbc     DP_FPB_exp
         beq     @L5381
         bcc     @L5348
         cmp     #$25
         bcs     L5306
         tay
         and     #$38
         beq     @L5334
         sec
@L531E:  ldx     DP_FPB_mant+3
         stx     DP_FPB_mant+4
         ldx     DP_FPB_mant+2
         stx     DP_FPB_mant+3
         ldx     DP_FPB_mant+1
         stx     DP_FPB_mant+2
         ldx     DP_FPB_mant
         stx     DP_FPB_mant+1
         stz     DP_FPB_mant
         sbc     #$08
         bne     @L531E
@L5334:  tya
         and     #$07
         beq     @L5381
@L5339:  lsr     DP_FPB_mant
         ror     DP_FPB_mant+1
         ror     DP_FPB_mant+2
         ror     DP_FPB_mant+3
         ror     DP_FPB_mant+4
         dec     A
         bne     @L5339
         bra     @L5381

@L5348:  eor     #$ff
         inc     A
         cmp     #$25
         bcs     L52E8
         ldy     DP_FPB_exp
         sty     DP_FPA_exp+1
         tay
         and     #$38
         beq     @L536F
         sec
@L5359:  ldx     DP_FPA_mant+3
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
@L536F:  tya
         and     #$07
         beq     @L5381
@L5374:  lsr     DP_FPA_mant
         ror     DP_FPA_mant+1
         ror     DP_FPA_mant+2
         ror     DP_FPA_mant+3
         ror     DP_FPA_mant+4
         dec     A
         bne     @L5374
@L5381:  lda     DP_FPA_sgn
         eor     DP_FPB_sgn
         bmi     @L538B
         clc
         jmp     fpAddAtoBstoreinA_sameExp

@L538B:  lda     DP_FPA_mant
         cmp     DP_FPB_mant
         bne     @L53AC
         lda     DP_FPA_mant+1
         cmp     DP_FPB_mant+1
         bne     @L53AC
         lda     DP_FPA_mant+2
         cmp     DP_FPB_mant+2
         bne     @L53AC
         lda     DP_FPA_mant+3
         cmp     DP_FPB_mant+3
         bne     @L53AC
         lda     DP_FPA_mant+4
         cmp     DP_FPB_mant+4
         bne     @L53AC
         jmp     fpzero_FPA

@L53AC:  bcs     @L53D4
         lda     DP_FPB_sgn
         sta     DP_FPA_sgn
         sec
         lda     DP_FPB_mant+4
         sbc     DP_FPA_mant+4
         sta     DP_FPA_mant+4
         lda     DP_FPB_mant+3
         sbc     DP_FPA_mant+3
         sta     DP_FPA_mant+3
         lda     DP_FPB_mant+2
         sbc     DP_FPA_mant+2
         sta     DP_FPA_mant+2
         lda     DP_FPB_mant+1
         sbc     DP_FPA_mant+1
         sta     DP_FPA_mant+1
         lda     DP_FPB_mant
         sbc     DP_FPA_mant
         sta     DP_FPA_mant
         brl     L51AD

@L53D4:  lda     DP_FPA_mant+4
         sbc     DP_FPB_mant+4
         sta     DP_FPA_mant+4
         lda     DP_FPA_mant+3
         sbc     DP_FPB_mant+3
         sta     DP_FPA_mant+3
         lda     DP_FPA_mant+2
         sbc     DP_FPB_mant+2
         sta     DP_FPA_mant+2
         lda     DP_FPA_mant+1
         sbc     DP_FPB_mant+1
         sta     DP_FPA_mant+1
         lda     DP_FPA_mant
         sbc     DP_FPB_mant
         sta     DP_FPA_mant
         brl     L51AD

tblTokensAsc:
         .byte   "AND"
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
tblFnDispatch:
         .word   exec_OPENIN ;8E
         .word   exec_PTR   ;8F
         .word   exec_PAGE  ;90
         .word   exec_TIME
         .word   exec_LOMEM
         .word   exec_HIMEM
         .word   exec_ABS
         .word   exec_ACS
         .word   exec_ADVAL
         .word   exec_ASC
         .word   exec_ASN   ;98
         .word   exec_ATN
         .word   exec_BGET
         .word   exec_COS
         .word   exec_COUNT
         .word   exec_DEG
         .word   exec_ERL
         .word   exec_ERR
         .word   exec_EVAL  ;A0
         .word   exec_EXP
         .word   exec_EXT
         .word   exec_FALSE
         .word   exec_FN
         .word   exec_GET
         .word   exec_INKEY
         .word   exec_INSTR
         .word   exec_INT   ;A8
         .word   exec_LEN
         .word   exec_LN
         .word   exec_LOG
         .word   exec_NOT
         .word   exec_OPENUP
         .word   exec_OPENOUT
         .word   exec_PI
         .word   exec_POINT ;B0
         .word   exec_POS
         .word   exec_RAD
         .word   exec_RND
         .word   exec_SGN
         .word   exec_SIN
         .word   exec_SQR
         .word   exec_TAN
         .word   exec_TO    ;B8
         .word   exec_TRUE
         .word   exec_USR
         .word   exec_VAL
         .word   exec_VPOS
         .word   exec_CHR
         .word   exec_GETDollar
         .word   exec_INKEYDollar
         .word   exec_LEFT  ;C0
         .word   exec_MID
         .word   exec_RIGHT
         .word   exec_STR
         .word   exec_STRING
         .word   exec_EOF
         .word   exec_AUTO
         .word   exec_DELETE
         .word   exec_LOAD  ;C8
         .word   exec_LIST
         .word   exec_NEW
         .word   exec_OLD
         .word   exec_RENUMBER
         .word   exec_SAVE
         .word   exec_EDIT
         .word   exec_PTRc
         .word   exec_PAGE_set ;DO
         .word   exec_TIME_set
         .word   exec_LOMEM_set
         .word   exec_HIMEM_set
         .word   exec_SOUND
         .word   exec_BPUT
         .word   exec_CALL
         .word   exec_CHAIN
         .word   exec_CLEAR ;D8
         .word   exec_CLOSE
         .word   exec_CLG
         .word   exec_CLS
         .word   parse_skip_EOL ;DATA
         .word   parse_skip_EOL ;DEF - skip like DATA
         .word   exec_DIM
         .word   exec_DRAW
         .word   exec_END   ;EO
         .word   exec_ENDPROC
         .word   exec_ENVELOPE
         .word   exec_FOR
         .word   exec_GOSUB
         .word   exec_GOTO
         .word   exec_GCOL
         .word   exec_IF
         .word   exec_INPUT ;E8
         .word   exec_LET
         .word   exec_LOCAL
         .word   exec_MODE
         .word   exec_MOVE
         .word   exec_NEXT
         .word   exec_ON
         .word   exec_VDU
         .word   exec_PLOT  ;F0
         .word   exec_PRINT
         .word   exec_PROC
         .word   exec_READ
         .word   parse_skip_EOL ;REM
         .word   exec_REPEAT
         .word   exec_REPORT
         .word   exec_RESTORE
         .word   exec_RETURN
         .word   exec_RUN
         .word   exec_STOP
         .word   exec_COLOUR
         .word   exec_TRACE
         .word   exec_UNTIL
         .word   exec_WIDTH
         .word   exec_OSCLI
;*******************************************************************************
;* Tokenizer token (see tokenizer routine at                                   *
;*******************************************************************************
;* First 3 chars of a candidate string for tokenization have their bottom 5    *
;* bytes packed into 2 byte work compared against this and the next table      *
;*******************************************************************************
tblAsmMnePack:
         .byte   $4b
         .byte   $83
         .byte   $84
         .byte   $89
         .byte   $96
.macro   HiAscii Arg
         .repeat .strlen(Arg), I
         .byte   .strat(Arg, I) | $80
         .endrep
.endmacro
         HiAscii "89XYp"
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
         .byte   "s3ca"
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
         HiAscii "e1)E"
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
         HiAscii "lr#C"
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
         HiAscii "00)2"
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
tblAsmMnePack2:
         .byte   $0a,$0d,$0d,$0d,$0d
         .byte   $10
         .byte   $10
         .byte   "%%9AAAAJJLLLPPRSSS"
         .byte   $10
         .byte   "%AAAAAAAAAPPPPPPRRSSN``J",$0d,"N\\"
         .byte   $08
         .byte   $09
         .byte   $08
         .byte   $08
         .byte   $08
         .byte   $09
         .byte   $09
         .byte   $0a,$0a,$0a,$0a
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
         .byte   ")*00NNNJHL@@",$0a,"@66"
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
tblAssOpCodes:
         .byte   $00
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
         .byte   "h(@`8"
         .byte   $f8
         .byte   $78
         .byte   $aa,$a8,$ba,$8a
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
         .byte   "K[[{{"
         .byte   $1b
         .byte   $1b
         .byte   $3b
         .byte   $3b
         .byte   $9b
         HiAscii ";kk{"
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
         HiAscii "AA!a"
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
         HiAscii "BBbT"
         .byte   $62
         .byte   $82
         .byte   $f4
         .byte   $54
         .byte   $44

L5933:   jmp     L6364

L5936:   jsr     parse_skip_spaces_PTR2
         eor     #$5d
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
         sta     $5d
         lda     DP_BAS_TMP6+2
         jsr     L99E2
         lda     DP_BAS_TMP6+1
         jsr     L99E2
         lda     DP_BAS_TMP6
         jsr     L9A05
         ldx     #$fb
         ldy     $5b
         bpl     @L5969
         ldy     DP_BAS_STRLEN
@L5969:  sty     DP_FPB_sgn
         beq     @L5986
         ldy     #$00
@L596F:  inx
         bne     @L597C
         jsr     PrintCRLFresetCOUNT
         ldx     $5d
         jsr     L9A42
         ldx     #$fc
@L597C:  lda     [DP_BAS_TMP6+3],y
         jsr     L9A05
         iny
         dec     DP_FPB_sgn
         bne     @L596F
@L5986:  txa
         tay
@L5988:  iny
         beq     @L5992
         ldx     #$03
         jsr     L9A42
         bra     @L5988

@L5992:  ldx     #$08
         lda     [DP_BAS_TXTPTR2]
         cmp     #$2e
         bne     @L59A9
@L599A:  jsr     PrintChOrToken
         dex
         bne     @L59A2
         ldx     #$01
@L59A2:  iny
         lda     [DP_BAS_TXTPTR2],y
         cpy     $71
         bne     @L599A
@L59A9:  jsr     L9A42
         dey
@L59AD:  iny
         cmp     [DP_BAS_TXTPTR2],y
         beq     @L59AD
@L59B2:  lda     [DP_BAS_TXTPTR2],y
         cmp     #':'
         beq     @L59C2
         cmp     #$0d
         beq     @L59C6
@L59BC:  jsr     PrintChOrToken
         iny
         bra     @L59B2

@L59C2:  cpy     DP_BAS_TXTPTR2_OFF
         bcc     @L59BC
@L59C6:  jsr     PrintCRLFresetCOUNT
@L59C9:  ldy     DP_BAS_TXTPTR2_OFF
         dey
@L59CC:  iny
         lda     [DP_BAS_TXTPTR2],y
         cmp     #$3a
         beq     @L59D7
         cmp     #$0d
         bne     @L59CC
@L59D7:  jsr     parse_nextstmt_yield_TXTPTR2_Y
         lda     [DP_BAS_TXTPTR2]
         cmp     #$3a
         beq     @L59F2
         lda     DP_BAS_TXTPTR2+1
         cmp     DP_BAS_MEMBASE+1
         bne     @L59EF
         lda     DP_BAS_TXTPTR2+2
         cmp     DP_BAS_MEMBASE+2
         bne     @L59EF
         jmp     reset_prog_prompt

@L59EF:  jsr     L73C6
@L59F2:  jmp     L5936

brk46_Label:
         brk     $46

         .byte   "Label"
         .byte   $00

L59FD:   lda     DP_BAS_OPT
         and     #$08
         bne     @L5A1C
         jsr     L6F78
         beq     @L5A1C
         jsr     L8B74
         jsr     L6C8E
         ldx     #$04
         ldy     #$43
@L5A12:  lda     $47,x
         cmp     [DP_BAS_ASSWKSP],y
         bne     brk46_Label
         dey
         dex
         bne     @L5A12
@L5A1C:  jsr     var_find
         beq     L5A7D
         bcs     L5A7D
         jsr     pushINTWA_on_hw_stack
         jsr     L851B
         sta     $40
         jsr     L8D5C
         jsr     L66AB
         sty     $71
L5A33:   jsr     parse_skip_spaces_PTR2
         ldy     #$00
         stz     DP_FPB_sgn
         cmp     #$3a
         beq     L5AA2
         cmp     #$0d
         beq     L5AA2
         cmp     #$5c
         beq     L5AA2
         cmp     #$2e
         beq     L59FD
         dec     DP_BAS_TXTPTR2_OFF
         ldx     #$03
; pack three char's lowest 5 bits into a packed word and compare against lookup
; table to try and find a mnemonic
@packlp: ldy     DP_BAS_TXTPTR2_OFF
         inc     DP_BAS_TXTPTR2_OFF
         lda     [DP_BAS_TXTPTR2],y
         bmi     L5A80
         cmp     #$20
         beq     @L5A6A
         ldy     #$05
         asl     A
         asl     A
         asl     A
@packlp2:
         asl     A
         rol     DP_FPB_sgn
         rol     $5d
         dey
         bne     @packlp2
         dex
         bne     @packlp
@L5A6A:  ldx     #$6d
         lda     DP_FPB_sgn
@L5A6E:  cmp     tblAsmMnePack-1,x
         bne     @L5A7A
         ldy     tblAsmMnePack2-1,x
         cpy     $5d
         beq     L5A97
@L5A7A:  dex
         bne     @L5A6E
L5A7D:   jmp     brk10_Syntax

L5A80:   ldx     #$42
         cmp     #$80
         beq     L5A97
         inx
         cmp     #$82
         beq     L5A97
         inx
         cmp     #$84
         bne     L5A7D
         lda     #$41
         jsr     L5F7C
         bne     L5A7D
L5A97:   lda     tblAssOpCodes-1,x
         sta     $47
         ldy     #$01
         cpx     #$37
         bcs     L5B1E
L5AA2:   sty     $5b
         ldy     #$42
         lda     [DP_BAS_ASSWKSP],y
         sta     DP_BAS_TMP6+2
         xba
         dey
         lda     [DP_BAS_ASSWKSP],y
         tax
         dey
         stx     DP_BAS_TMP6+1
         lda     [DP_BAS_ASSWKSP],y
         tay
         sty     DP_BAS_TMP6
         lda     DP_BAS_OPT
         and     #$04
         cmp     #$01
         bcc     @L5ACC
         ldy     #$3e
         lda     [DP_BAS_ASSWKSP],y
         xba
         dey
         lda     [DP_BAS_ASSWKSP],y
         tax
         dey
         lda     [DP_BAS_ASSWKSP],y
         tay
@L5ACC:  sty     DP_BAS_TMP6+3
         stx     DP_BAS_TMP6+4
         xba
         sta     DP_BAS_TMP6+5
         ldy     $5b
         beq     @L5B1D
         bpl     @L5ADD
         ldy     DP_BAS_STRLEN
         beq     @L5B1D
@L5ADD:  dey
         tyx
         lda     $47,x
         bit     $5b
         bpl     @L5AE7
         lda     [DP_BAS_STRWKSP_L],y
@L5AE7:  eor     #$ff
         sta     [DP_BAS_TMP6+3],y
         cmp     [DP_BAS_TMP6+3],y
         bne     @L5AF7
         eor     #$ff
         sta     [DP_BAS_TMP6+3],y
         cmp     [DP_BAS_TMP6+3],y
         beq     @L5B04
@L5AF7:  brk     $47

         .byte   "Bad Memory"
         .byte   $00

@L5B04:  phy
         clc
         ldy     #$40
         jsr     L5C99
         lda     DP_BAS_OPT
         and     #$04
         cmp     #$01
         bcc     @L5B19
         clc
         ldy     #$3c
         jsr     L5C99
@L5B19:  ply
         tya
         bne     @L5ADD
@L5B1D:  rts

L5B1E:   cpx     #$42
         bcs     L5B84
         lda     #$00
L5B24:   pha
         jsr     eval_INTBRK
         clc
         ldy     #$40
         lda     DP_BAS_INT_WA
         sbc     [DP_BAS_ASSWKSP],y
         xba
         lda     DP_BAS_INT_WA+1
         iny
         sbc     [DP_BAS_ASSWKSP],y
         iny
         tax
         lda     DP_BAS_INT_WA+2
         sbc     [DP_BAS_ASSWKSP],y
         tay
         xba
         clc
         sbc     $01,S
         bcs     @L5B48
         cpx     #$01
         dex
         bcs     @L5B48
         dey
@L5B48:  sta     DP_BAS_INT_WA
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
@L5B5F:  inc     A
         bne     @L5B65
         txa
         bmi     L5B81
@L5B65:  lda     DP_BAS_OPT
         and     #$02
         beq     L5B81
         brk     $01

         .byte   "Out of range"
         .byte   $00

@L5B7A:  lda     DP_BAS_INT_WA+2
         bne     @L5B5F
@L5B7E:  txa
         bmi     @L5B65
L5B81:   jmp     L5AA2

L5B84:   cpx     #$4a
         bcs     L5BBF
         jsr     L5F7A
         pha
         beq     L5BC7
         jsr     parse_skipSpacesPTR2_cmp_HASH
         bne     L5BCA
         pla
         jsr     L5F52
L5B97:   lda     #$20
L5B99:   and     DP_BAS_OPT
         beq     L5BA0
         jmp     L5E02

L5BA0:   jsr     eval_INTBRK
L5BA3:   lda     DP_BAS_INT_WA+1
         ora     DP_BAS_INT_WA+2
         ora     DP_BAS_INT_WA+3
         bne     L5BAF
L5BAB:   ldy     #$02
         bra     L5B81

L5BAF:   brk     $02

         .byte   "Byte"

L5BB5:   brk     $48

         .byte   "Word"
         .byte   $00

L5BBC:   jmp     L5CAE

L5BBF:   cpx     #$5b
         bne     L5BBC
         jsr     L5F7A
         pha
L5BC7:   jsr     parse_skip_spaces_PTR2
L5BCA:   cmp     #$28
         bne     L5C3B
         jsr     eval_INTBRK
         jsr     parse_skip_spaces_PTR2
         cmp     #$29
         bne     L5C0F
         jsr     L5F4F
         jsr     parse_SkipSpacesPTR2_cmp_COMMA
         beq     L5BFD
         pla
         bne     @L5BEA
         lda     $47
         sec
         sbc     #$0b
         sta     $47
@L5BEA:  inc     $47
L5BEC:   bra     L5BA3

L5BEE:   lda     $47
         adc     #$11
         sta     $47
         pla
         beq     L5C30
         pha
         jsr     parse_SkipSpacesPTR2_cmp_COMMA
         bne     L5C33
L5BFD:   pla
         bne     @L5C06
         lda     $47
         adc     #$05
         sta     $47
@L5C06:  jsr     L5F5D
         cmp     #$59
         beq     L5BEC
         bra     L5C33

L5C0F:   cmp     #$2c
         bne     L5C33
         jsr     L5F5D
         beq     @L5C1C
         cmp     #$53
         bne     L5C33
@L5C1C:  tax
         jsr     parse_skip_spaces_PTR2
         cmp     #$29
         bne     L5C33
         cpx     #$53
         beq     L5BEE
         jsr     parse_SkipSpacesPTR2_cmp_COMMA
         beq     L5C33
         pla
         bne     L5BEC
L5C30:   jmp     brk10_Syntax

L5C33:   brk     $03

         .byte   "Index"
         .byte   $00

L5C3B:   jsr     L66A3
         jsr     parse_SkipSpacesPTR2_cmp_COMMA
         bne     L5C86
         jsr     L5F4F
         jsr     L5F5D
         beq     L5C86
         cmp     #$59
         beq     L5C5F
         cmp     #$53
         bne     L5C33
         pla
         beq     L5C30
         lda     $47
         sbc     #$0e
         sta     $47
         bra     L5BEC

L5C5E:   pha
L5C5F:   jsr     L5F52
         ldy     #$03
         pla
         cmp     #$01
         bcc     @L5C6F
         lda     DP_BAS_INT_WA+2
         eor     $41
         beq     @L5C7C
@L5C6F:  lda     $47
         and     #$0f
         eor     #$0d
         bne     @L5C81
         lda     #$0f
         tsb     $47
         iny
@L5C7C:  lda     DP_BAS_INT_WA+3
         beq     L5CE5
         sec
@L5C81:  bcc     L5C30
         jmp     L5D7A

L5C86:   jsr     L5F55
         pla
         beq     L5C5E
         bra     L5C91

L5C8E:   jsr     L5F55
L5C91:   jsr     L5F88
         bne     L5C5E
         jmp     L5BAB

L5C99:   lda     [DP_BAS_ASSWKSP],y
         adc     #$01
         sta     [DP_BAS_ASSWKSP],y
         iny
         lda     [DP_BAS_ASSWKSP],y
         adc     #$00
         sta     [DP_BAS_ASSWKSP],y
         iny
         lda     [DP_BAS_ASSWKSP],y
         adc     #$00
         sta     [DP_BAS_ASSWKSP],y
         rts

L5CAE:   cpx     #$50
         bcs     L5CE8
         jsr     L5F5D
         cmp     #$41
         beq     L5CCC
L5CB9:   jsr     L66A3
         jsr     parse_SkipSpacesPTR2_cmp_COMMA
         bne     L5C91
         jsr     L5F4F
         jsr     L5F5D
         beq     L5C91
L5CC9:   brl     L5C33

L5CCC:   iny
         lda     [DP_BAS_TXTPTR2],y
         jsr     L6031
         bcs     L5CB9
         ldy     #$16
         cpx     #$4e
         bcc     L5CE0
         bne     @L5CDE
         ldy     #$36
@L5CDE:  sty     $47
L5CE0:   jsr     L5F55
         ldy     #$01
L5CE5:   jmp     L5AA2

L5CE8:   cpx     #$52
         bcs     @L5D16
         jsr     eval_INTBRK
         ldy     #$03
         ldx     #$01
         lda     DP_BAS_INT_WA+1
         ora     DP_BAS_INT_WA+2
         ora     DP_BAS_INT_WA+3
         bne     @L5D02
         ldx     #$0f
         lda     #$64
         sta     $47
         dey
@L5D02:  phy
         jsr     parse_SkipSpacesPTR2_cmp_COMMA
         bne     @L5D12
         jsr     L5F5D
         bne     L5CC9
         txa
         adc     $47
         sta     $47
@L5D12:  ply
         brl     L5AA2

@L5D16:  cpx     #$56
         bcs     L5D3C
         cpx     #$54
         bcs     @L5D25
         jsr     parse_skipSpacesPTR2_cmp_HASH
         beq     L5D9D
         dec     DP_BAS_TXTPTR2_OFF
@L5D25:  jsr     eval_INTBRK
L5D28:   jmp     L5C8E

L5D2B:   jsr     parse_skipSpacesPTR2_cmp_HASH
         bne     L5CB9
         ldy     #$89
         sty     $47
         jmp     L5B97

         .byte   $10
         .byte   $02

L5D39:   jmp     L5DCA

L5D3C:   beq     L5D2B
         cpx     #$59
         bcs     L5D39
         jsr     L5F7A
         pha
         jsr     parse_skip_spaces_PTR2
         cmp     #$28
         beq     L5DA2
         ldy     #$03
         pla
         bne     @L5D5B
         lda     $47
         clc
         adc     L5CE0,x
         sta     $47
         iny
@L5D5B:  phy
         jsr     L66A3
         jsr     parse_SkipSpacesPTR2_cmp_COMMA
         beq     L5D83
         ply
L5D65:   lda     DP_BAS_INT_WA+3
         bne     L5D7A
         cpy     #$04
         beq     @L5D77
         phy
         ldy     #$42
         lda     DP_BAS_INT_WA+2
         cmp     [DP_BAS_ASSWKSP],y
         bne     L5D7A
         ply
@L5D77:  jmp     L5AA2

L5D7A:   brk     $49

         .byte   "Bank"
         .byte   $00

L5D81:   cpx     #$58
L5D83:   beq     L5DEF
         pla
         bne     L5E05
         lda     #$dc
         sta     $47
         bra     L5E05

L5D8E:   pla
         beq     L5DEF
         cpx     #$58
         bne     @L5D99
         lda     #$fc
         sta     $47
@L5D99:  ldy     #$03
         bra     L5D65

L5D9D:   lda     #$10
         jmp     L5B99

L5DA2:   phx
         jsr     L5F4F
         jsr     L5F4F
         jsr     eval_INTBRK
         plx
         jsr     parse_skip_spaces_PTR2
         cmp     #$29
         beq     L5D81
         cmp     #$2c
         bne     L5DC7
         jsr     L5F4F
         jsr     L5F5D
         bne     L5DC7
         jsr     parse_skip_spaces_PTR2
         cmp     #$29
         beq     L5D8E
L5DC7:   jmp     L5C33

L5DCA:   cpx     #$5e
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

@L5DE1:  jmp     L5BA0

@L5DE4:  cpx     #$61
         beq     @L5DE1
         bcs     L5DF2
         jsr     parse_skipSpacesPTR2_cmp_HASH
         beq     @L5DE1
L5DEF:   jmp     brk10_Syntax

L5DF2:   cpx     #$64
         beq     @L5DFD
         bcs     L5E47
         lda     #$01
         jmp     L5B24

@L5DFD:  jsr     parse_skipSpacesPTR2_cmp_HASH
         bne     L5DEF
L5E02:   jsr     eval_INTBRK
L5E05:   ldy     #$03
         lda     DP_BAS_INT_WA+2
         ora     DP_BAS_INT_WA+3
         beq     @L5E10
         jmp     L5BB5

@L5E10:  jmp     L5AA2

L5E13:   jsr     L66A3
         jsr     parse_SkipSpacesPTR2_cmp_COMMA
         bne     @L5E27
         jsr     parse_skip_spaces_PTR2
         and     #$1f
         cmp     $01,S
         bne     L5DC7
         jsr     L5F4F
@L5E27:  pla
         jmp     L5D28

L5E2B:   jsr     eval_INTBRK
         jsr     parse_SkipSpacesPTR2_cmp_COMMA
         bne     L5E97
         jsr     parse_skip_spaces_PTR2
         and     #$1f
         cmp     $01,S
         bne     L5DC7
         jsr     L5F4F
         jsr     L5F88
         beq     L5E97
         jmp     L5BAF

L5E47:   cpx     #$67
         bcs     @L5E6B
         jsr     eval_INTBRK
         jsr     L5F88
         bne     L5E9B
         lda     DP_BAS_INT_WA
         pha
         jsr     parse_SkipSpacesPTR2_cmp_COMMA
         bne     L5DEF
         jsr     eval_INTBRK
         jsr     L5F88
         bne     L5E9B
         pla
         sta     DP_BAS_INT_WA+1
         bra     L5E05

@L5E68:  jmp     L5F12

@L5E6B:  beq     @L5E68
         phx
         jsr     eval_INTBRK
         plx
         cpx     #$69
         bcc     L5EDC
         beq     L5EE7
         cpx     #$6b
         bcs     @L5E7F
         brl     L5F00

@L5E7F:  beq     L5EFA
         cpx     #$6d
         beq     L5EA4
         jsr     L5F88
         bne     L5E9B
         lda     DP_BAS_INT_WA
         sta     $41
L5E8E:   lda     DP_BAS_OPT
         sta     $43
         ldy     #$00
         jmp     L5F3D

L5E97:   pla
         jmp     L5C91

L5E9B:   jmp     L5D7A

L5E9E:   jmp     L5BAF

L5EA1:   jmp     brk19_bad_MODE

L5EA4:   jsr     L5F71
         and     #$df
         cmp     #$42
         beq     @L5EB0
         brl     L5F30

@L5EB0:  jsr     eval_INTBRK
         lda     DP_BAS_INT_WA+3
         bmi     L5E8E
         jsr     L5F88
         bne     L5E9E
         lda     DP_BAS_INT_WA
         pha
         stz     DP_BAS_INT_WA
         jsr     parse_SkipSpacesPTR2_cmp_COMMA
         bne     @L5ECE
         jsr     eval_INTBRK
         jsr     L5F88
         bne     L5E9E
@L5ECE:  plx
         stx     DP_BAS_STRLEN
         lda     DP_BAS_INT_WA
@L5ED3:  txy
         dey
         sta     [DP_BAS_STRWKSP_L],y
         dex
         bne     @L5ED3
         bra     L5F4B

L5EDC:   jsr     L5F88
         bne     L5E9E
         lda     DP_BAS_INT_WA
L5EE3:   sta     DP_BAS_OPT
         bra     L5E8E

L5EE7:   lda     #$40
         trb     DP_BAS_OPT
         jsr     L5F88
         ora     DP_BAS_INT_WA
         beq     L5E8E
         lda     DP_BAS_OPT
         and     #$8f
         ora     #$40
         bra     L5EE3

L5EFA:   lda     DP_BAS_OPT
         and     #$40
         bne     L5EA1
L5F00:   jsr     L5F88
         bne     L5E9E
         lda     DP_BAS_INT_WA
         and     #$30
         trb     DP_BAS_OPT
         bcc     L5E8E
         tsb     DP_BAS_OPT
         brl     L5E8E

L5F12:   ldx     #$01
         jsr     L5F71
         and     #$df
         cmp     #$42
         beq     L5F33
         inx
         cmp     #$57
         beq     L5F33
         inx
         cmp     #$4c
         beq     L5F33
         inx
         cmp     #$44
         beq     L5F33
         cmp     #$53
         beq     L5F43
L5F30:   jmp     brk10_Syntax

L5F33:   phx
         jsr     eval_INTBRK
         ldx     #$47
         jsr     L9A49
         ply
L5F3D:   jmp     L5AA2

jmp_brk06_type_mismatch:
         jmp     brk06_type_mismatch

L5F43:   jsr     L7527
         bne     jmp_brk06_type_mismatch
         jsr     L66AB
L5F4B:   ldy     #$ff
         bra     L5F3D

L5F4F:   jsr     L5F52
L5F52:   jsr     L5F55
L5F55:   lda     $47
         clc
         adc     #$04
         sta     $47
         rts

L5F5D:   jsr     parse_skip_spaces_PTR2
         and     #$df
         cmp     #$58
         rts

parse_skipSpacesPTR2_cmp_HASH:
         jsr     parse_skip_spaces_PTR2
         cmp     #'#'
         rts

parse_SkipSpacesPTR2_cmp_COMMA:
         jsr     parse_skip_spaces_PTR2
         cmp     #','
         rts

L5F71:   lda     #$00
L5F73:   ldy     DP_BAS_TXTPTR2_OFF
         inc     DP_BAS_TXTPTR2_OFF
         eor     [DP_BAS_TXTPTR2],y
         rts

L5F7A:   lda     #$4c
L5F7C:   jsr     L5F73
         and     #$df
         beq     @L5F87
         dec     DP_BAS_TXTPTR2_OFF
         eor     #$00
@L5F87:  rts

L5F88:   lda     DP_BAS_INT_WA+1
         ora     DP_BAS_INT_WA+2
         ora     DP_BAS_INT_WA+3
         rts

L5F8F:   sta     [DP_BAS_TMP6]
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
@L5FA3:  iny
         lda     [DP_BAS_TMP6+3],y
         sta     [DP_BAS_TMP6],y
         cmp     #$0d
         bne     @L5FA3
         rts

L5FAD:   and     #$0f
         sta     DP_FPB_exp
         ldx     #$00
         ldy     #$00
@L5FB5:  iny
         lda     [DP_BAS_TMP6],y
         jsr     L6041
         bcc     @L5FEB
         and     #$0f
         pha
         stx     DP_FPB_mant
         lda     DP_FPB_exp
         asl     A
         rol     DP_FPB_mant
         bmi     @L5FE8
         asl     A
         rol     DP_FPB_mant
         bmi     @L5FE8
         adc     DP_FPB_exp
         sta     DP_FPB_exp
         txa
         adc     DP_FPB_mant
         asl     DP_FPB_exp
         rol     A
         bmi     @L5FE8
         bcs     @L5FE8
         tax
         pla
         adc     DP_FPB_exp
         sta     DP_FPB_exp
         bcc     @L5FB5
         inx
         bpl     @L5FB5
         pha
@L5FE8:  pla
         sec
         rts

@L5FEB:  dey
         lda     #$8d
         jsr     L5F8F
         lda     DP_BAS_TMP6
         sta     DP_BAS_TMP6+3
         lda     DP_BAS_TMP6+1
         sta     DP_BAS_TMP6+4
         lda     DP_BAS_TMP6+2
         sta     DP_BAS_TMP6+5
         jsr     L604F
         jsr     L604F
         jsr     L604F
@L6006:  lda     [DP_BAS_TMP6+3],y
         sta     [DP_BAS_TMP6],y
         dey
         bne     @L6006
         ldy     #$03
L600F:   txa
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
L602F:   clc
         rts

L6031:   cmp     #$7b
         bcs     L602F
         cmp     #$5f
         bcs     L6047
         cmp     #$5b
         bcs     L602F
         cmp     #$40
         bcs     L6047
L6041:   cmp     #$3a
         bcs     L602F
         cmp     #$30
L6047:   rts

L6048:   cmp     #$2e
         bne     L6041
         rts

L604D:   lda     [DP_BAS_TMP6]
L604F:   inc     DP_BAS_TMP6
         bne     L6090
         inc     DP_BAS_TMP6+1
         bne     L6090
         inc     DP_BAS_TMP6+2
         rts

L605A:   jsr     L604F
         lda     [DP_BAS_TMP6]
         rts

L6060:   jsr     L604F
L6063:   lda     [DP_BAS_TMP6]
         cmp     #$0d
         beq     L6090
         cmp     #$20
         beq     L6060
         cmp     #$26
         bne     @L6081
@L6071:  jsr     L605A
         jsr     L6041
         bcs     @L6071
         cmp     #$41
         bcc     L6063
         cmp     #$47
         bcc     @L6071
@L6081:  cmp     #$22
         bne     L6091
@L6085:  jsr     L605A
         cmp     #$22
         beq     L6060
         cmp     #$0d
         bne     @L6085
L6090:   rts

L6091:   cmp     #$3a
         bne     L609E
         jsr     L604F
L6098:   stz     DP_FPB_sgn
L609A:   stz     $5d
         bra     L6063

L609E:   cmp     #$2c
         beq     L6060
         cmp     #$2a
         bne     L60B2
         lda     DP_FPB_sgn
         beq     L6090
L60AA:   ldx     #$ff
         stx     DP_FPB_sgn
         stz     $5d
         bra     L6060

L60B2:   cmp     #$2e
         beq     @L60C4
         jsr     L6041
         bcc     L60E7
         ldx     $5d
         beq     @L60C4
         jsr     L5FAD
         bcc     L6060
@L60C4:  lda     [DP_BAS_TMP6]
         jsr     L6048
         bcc     L60D0
         jsr     L604F
         bra     @L60C4

L60D0:   ldx     #$ff
         stx     DP_FPB_sgn
         bra     L609A

L60D6:   jsr     L6031
         bcc     L60AA
L60DB:   lda     [DP_BAS_TMP6]
         jsr     L6031
         bcc     L60D0
         jsr     L604F
         bra     L60DB

L60E7:   cmp     #$40
         bcc     L60AA
         cmp     #$58
         bcs     L60D6
         ldx     #$f5
         stx     DP_BAS_TMP6+3
         ldx     #$53
         stx     DP_BAS_TMP6+4
@L60F7:  ldy     #$00
         cmp     (DP_BAS_TMP6+3)
         bcc     L60DB
         bne     @L610E
@L60FF:  iny
         lda     (DP_BAS_TMP6+3),y
         bmi     @L6135
         cmp     [DP_BAS_TMP6],y
         beq     @L60FF
         lda     [DP_BAS_TMP6],y
         cmp     #$2e
         beq     @L6119
@L610E:  iny
         lda     (DP_BAS_TMP6+3),y
         bpl     @L610E
         cmp     #$fe
         bne     @L6126
         bcs     L60DB
@L6119:  iny
@L611A:  lda     (DP_BAS_TMP6+3),y
         bmi     @L6135
         inc     DP_BAS_TMP6+3
         bne     @L611A
         inc     DP_BAS_TMP6+4
         bra     @L611A

@L6126:  sec
         iny
         tya
         adc     DP_BAS_TMP6+3
         sta     DP_BAS_TMP6+3
         bcc     @L6131
         inc     DP_BAS_TMP6+4
@L6131:  lda     [DP_BAS_TMP6]
         bra     @L60F7

@L6135:  tax
         iny
         lda     (DP_BAS_TMP6+3),y
         sta     DP_FPB_exp
         dey
         lsr     A
         bcc     L6146
         lda     [DP_BAS_TMP6],y
L6141:   jsr     L6031
         bcs     L60DB
L6146:   txa
         bit     DP_FPB_exp
         bvc     @L6151
         ldx     DP_FPB_sgn
         bne     @L6151
         adc     #$40
@L6151:  dey
         jsr     L5F8F
         ldx     #$ff
         lda     DP_FPB_exp
         lsr     A
         lsr     A
         bcc     @L6161
         stx     DP_FPB_sgn
         stz     $5d
@L6161:  lsr     A
         bcc     @L6168
         stz     DP_FPB_sgn
         stz     $5d
@L6168:  lsr     A
         bcc     @L617B
         pha
         ldy     #$01
@L616E:  lda     [DP_BAS_TMP6],y
         jsr     L6031
         bcc     @L617A
         jsr     L604F
         bra     @L616E

@L617A:  pla
@L617B:  lsr     A
         bcc     @L6180
         stx     $5d
@L6180:  lsr     A
         bcs     L6190
         jmp     L6060

parse_skip_spaces:
         ldy     DP_BAS_TXTPTR_OFF
         inc     DP_BAS_TXTPTR_OFF
         lda     [DP_BAS_TXTPTR],y
         cmp     #' '
         beq     parse_skip_spaces
L6190:   rts

parse_skip_spaces_PTR2:
         ldy     DP_BAS_TXTPTR2_OFF
         inc     DP_BAS_TXTPTR2_OFF
         lda     [DP_BAS_TXTPTR2],y
         cmp     #$20
         beq     parse_skip_spaces_PTR2
L619B:   rts

parse_skip_space_CMPcomma:
         jsr     parse_skip_spaces
         cmp     #','
         rts

parse_skip_spaces_CMPcommaBRK:
         jsr     parse_skip_space_CMPcomma
         beq     L619B
brk_05_missing_comma:
         brk     $05

         .byte   $8d
         .byte   ','
         .byte   $00

exec_CHAIN:
         jsr     L9A5D
         brl     L6253

exec_OLD:
         jsr     parse_nextstmt_yield_TXTOFF2
         jsr     L61BB
         brl     clear_reset_prog_prompt

L61BB:   lda     DP_BAS_PAGE
         sta     DP_BAS_TMP6
         lda     DP_BAS_PAGE+1
         sta     DP_BAS_TMP6+1
         lda     DP_BAS_PAGE+2
         sta     DP_BAS_TMP6+2
         lda     #$00
         sta     [DP_BAS_TMP6],y
         jmp     do_END

L61CE:   ldy     #$00
@L61D0:  jsr     L61EC
         phy
         jsr     do_END
         ply
         lda     DP_BAS_TOP
         sta     [DP_BAS_pointer_at_copy_of_PAGE],y
         iny
         lda     DP_BAS_TOP+1
         sta     [DP_BAS_pointer_at_copy_of_PAGE],y
         iny
         lda     DP_BAS_TOP+2
         sta     [DP_BAS_pointer_at_copy_of_PAGE],y
         iny
         cpy     $98
         bne     @L61D0
         rts

L61EC:   lda     [DP_BAS_pointer_at_copy_of_PAGE],y
         sta     DP_BAS_PAGE
         iny
         lda     [DP_BAS_pointer_at_copy_of_PAGE],y
         sta     DP_BAS_PAGE+1
         iny
         lda     [DP_BAS_pointer_at_copy_of_PAGE],y
         sta     DP_BAS_PAGE+2
         iny
         rts

L61FC:   lda     [DP_BAS_pointer_at_copy_of_PAGE],y
         sta     DP_BAS_TOP
         iny
         lda     [DP_BAS_pointer_at_copy_of_PAGE],y
         sta     DP_BAS_TOP+1
         iny
         lda     [DP_BAS_pointer_at_copy_of_PAGE],y
         sta     DP_BAS_TOP+2
         iny
         rts

L620C:   ldy     #$00
@L620E:  jsr     @L622E
         bcc     @L6223
         jsr     @L622E
         bcs     @L6226
         sec
         tya
         sbc     #$06
         tay
@L621D:  jsr     L61EC
         brl     L61FC

@L6223:  iny
         iny
         iny
@L6226:  cpy     $98
         bne     @L620E
         ldy     #$00
         bra     @L621D

@L622E:  lda     $00,x
         cmp     [DP_BAS_pointer_at_copy_of_PAGE],y
         iny
         lda     $01,x
         sbc     [DP_BAS_pointer_at_copy_of_PAGE],y
         iny
         lda     $02,x
         sbc     [DP_BAS_pointer_at_copy_of_PAGE],y
         iny
         rts

L623E:   lda     #$80
         tsb     DP_BAS_CO_FLAGS
         bne     @L624F
         jsr     L61CE
         ldy     #$00
         jsr     L61EC
         jsr     L61FC
@L624F:  rts

exec_RUN:
         jsr     parse_nextstmt_yield_TXTOFF2
L6253:   jsr     L623E
         jsr     int_CLEAR
         lda     DP_BAS_PAGE+2
         sta     DP_BAS_TXTPTR2+2
         lda     DP_BAS_PAGE+1
         sta     DP_BAS_TXTPTR2+1
         lda     DP_BAS_PAGE
         sta     DP_BAS_TXTPTR2
         brl     run_from_PTR

exec_LOAD:
         jsr     L9A5D
         bra     clear_reset_prog_prompt

exec_END:
         jsr     parse_nextstmt_yield_TXTOFF2
         jsr     do_END
         bra     reset_prog_prompt

         .a16
         .i16
L6275:   bra     new_prompt

         .a8
         .i8
exec_NEW:
         jsr     parse_nextstmt_yield_TXTOFF2
new_prompt:
         jsr     NEW_int_reset_TOP_empty_prog
clear_reset_prog_prompt:
         jsr     int_CLEAR
reset_prog_prompt:
         stz     DP_BAS_CO_FLAGS
         ldy     DP_BAS_MEMBASE
         sty     DP_BAS_TXTPTR2
         ldy     DP_BAS_MEMBASE+1
         sty     DP_BAS_TXTPTR2+1
         ldy     DP_BAS_MEMBASE+2
         sty     DP_BAS_TXTPTR2+2
         jsr     exec_ERROR_OFF_reset_ERRORPTR
         lda     DP_BAS_ARG2_FLAG
         beq     PROMPT
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
         jsr     L61BB
         brl     L6253

PROMPT:  lda     #'>'
         jsr     call_OSWRCH
         jsr     L95BF
run_from_PTR:
         lda     DP_stack_save+1
         xba
         lda     DP_stack_save
         tcs
         jsr     exec_ERROR_OFF_reset_ERRORPTR
         jsr     L969F
         bcs     clear_reset_prog_prompt
         jmp     L63B8

L62CB:   jsr     parse_updPTRfromPTR2_yield
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

         .byte   "Command failed"
         .byte   $00

         .a8
         .i8
@skok:   sep     #$30
         plb
parse_skip_EOL:
         lda     #$0d
         ldy     DP_BAS_TXTPTR2_OFF
         dey
@lp:     iny                ;just skip to the end of the line
         cmp     [DP_BAS_TXTPTR2],y
         bne     @lp
parse_yield_cont_or_prompt:
         jsr     parse_updPTRfromPTR2_yield
         bra     cont_or_prompt

parseEOL_yield_cont_or_prompt:
         cmp     #$0d
         bne     parse_skip_EOL
cont_or_prompt:
         lda     DP_BAS_TXTPTR2+2
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

@notEndOfProg:
         ldx     DP_BAS_TRACEFLAG
         beq     @notrace
         sta     DP_BAS_INT_WA+1
         iny
         lda     [DP_BAS_TXTPTR2],y
         sta     DP_BAS_INT_WA
         jsr     doTracePrintLineNoIfInRange
@notrace:
         ldy     #$04
         sty     DP_BAS_TXTPTR2_OFF
         bra     L6366

L6331:   lda     #$03
         sta     DP_BAS_OPT
         ldy     #$42
         lda     [DP_BAS_ASSWKSP],y
         sta     $41
         jmp     L5936

L633E:   jmp     L9B6F

L6341:   ldy     DP_BAS_TXTPTR2_OFF
         dey
         lda     [DP_BAS_TXTPTR2],y
         cmp     #$2a
         beq     L62CB
         cmp     #$5b
         beq     L6331
         cmp     #$a2
         beq     L633E
         cmp     #$3d
         bne     L6359
         brl     L6430

L6359:   dec     DP_BAS_TXTPTR2_OFF
L635B:   jsr     parse_nextstmt_yield_TXTOFF2
parse_EOS_TXTPTR2:
         lda     [DP_BAS_TXTPTR2]
         cmp     #':'
         bne     parseEOL_yield_cont_or_prompt
L6364:   ldy     DP_BAS_TXTPTR2_OFF
L6366:   phx
         phy
         phx
         lda     #$00
         xba
         lda     #$03
         ldx     #$02
         jsl     _EV
         plx
         bcc     @L638D
         lda     #$01
         ldx     #$02
         jsl     _EV
         bcs     @L638A
         jsr     cmp_ERRORPTRisDEFAULT
         lda     #$03
         cop     $16
         bra     @L638D

@L638A:  brl     brk_11_Escape

@L638D:  ply
         plx
         inc     DP_BAS_TXTPTR2_OFF
         lda     [DP_BAS_TXTPTR2],y
         cmp     #$20
         beq     L6364
         cmp     #$2e
         bne     @L63A2
         lda     DP_BAS_LAST_EOSEOR_D_
         beq     L6402
         brl     brk_53_IllegalLabelDef

@L63A2:  ldx     #$ff
         stx     DP_BAS_LAST_EOSEOR_D_
         cmp     #$cf
         bcc     L63BF
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

L63B8:   jsr     parse_skip_spaces_PTR2
         cmp     #$c6
         bcs     evalDispatchFN
L63BF:   ldx     DP_BAS_TXTPTR2
         stx     DP_BAS_TXTPTR
         ldx     DP_BAS_TXTPTR2+1
         stx     DP_BAS_TXTPTR+1
         ldx     DP_BAS_TXTPTR2+2
         stx     DP_BAS_TXTPTR+2
         sty     DP_BAS_TXTPTR_OFF
         jsr     L6F90
         bne     L63F0
         bcc     @L63D7
         brl     L6341

@L63D7:  stx     DP_BAS_TXTPTR_OFF
         jsr     parseEqualsOrMistake
         jsr     L6E93
         ldx     #$05
         cpx     DP_BAS_INT_WA+3
         bne     @L63E6
         inx
@L63E6:  jsr     L6EFA
         dec     DP_BAS_TXTPTR2_OFF
exec_LET:
         jsr     var_find
         beq     L645C
L63F0:   bcc     L644A
         jsr     stack_INTWA
         jsr     parse_expectEQ_PTR_OFF
         lda     $40
         bne     brk06_type_mismatch
         jsr     L64B2
         brl     parse_EOS_TXTPTR2

L6402:   lda     #$ff
         sta     DP_BAS_LAST_EOSEOR_D_
         jsr     var_find
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
         sta     $40
         jsr     L8D5C
         ldy     DP_BAS_TXTPTR_OFF
         sty     DP_BAS_TXTPTR2_OFF
         brl     L6364

L6430:   sec
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
         jsr     L7527
         jmp     L7282

L644A:   lda     $5b
         cmp     #$40
         beq     L645C
         jsr     pushINTWA_on_hw_stack
         jsr     parse_expectEQ_PTR_OFF
         jsr     L8D5C
         jmp     parse_EOS_TXTPTR2

L645C:   jmp     brk10_Syntax

exec_STOP:
         jsr     parse_nextstmt_yield_TXTOFF2
         brk     $00

         .byte   $fa        ;STOP

brk_07_no_FN:
         brk     $07

         .byte   "No "
         .byte   $a4        ;FN

brk06_type_mismatch:
         brk     $06

         .byte   "Type mismatch"

brk_00_NoRoom:
         brk     $00

         .byte   "No room"
         .byte   $00

brk_53_IllegalLabelDef:
         brk     $53

         .byte   "Illegal label definition"
         .byte   $00

L649F:   jsr     L9AF9
         cpy     #$00
         beq     @L64AF
@L64A6:  lda     [DP_BAS_STRWKSP_L],y
         sta     [DP_BAS_INT_WA],y
         dey
         bne     @L64A6
         lda     [DP_BAS_STRWKSP_L]
@L64AF:  sta     [DP_BAS_INT_WA]
         rts

L64B2:   jsr     L993C
L64B5:   lda     DP_BAS_INT_WA+3
         sta     DP_FP_TMP+2
         cmp     #$80
         beq     L649F
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
         bcc     @L64DD
         adc     #$07
         bcc     @L64DD
         lda     #$ff
@L64DD:  clc
         pha
         tax
         lda     [DP_BAS_INT_WA],y
         adc     [DP_BAS_INT_WA]
         eor     DP_BAS_VARTOP
         bne     @L64FF
         ldy     #$01
         adc     [DP_BAS_INT_WA],y
         eor     DP_BAS_VARTOP+1
         bne     @L64FF
         iny
         adc     [DP_BAS_INT_WA],y
         eor     DP_BAS_VARTOP+2
         bne     @L64FF
         sta     DP_FP_TMP+2
         txa
         iny
         sec
         sbc     [DP_BAS_INT_WA],y
         tax
@L64FF:  txa
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
         bcc     @L651C
         brl     brk_00_NoRoom

@L651C:  sty     DP_BAS_VARTOP
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
@L653A:  ldy     #$04
         lda     DP_BAS_STRLEN
         sta     [DP_BAS_INT_WA],y
         beq     @L655B
         ldy     #$02
         lda     [DP_BAS_INT_WA],y
         sta     DP_FP_TMP+1
         dey
         lda     [DP_BAS_INT_WA],y
         sta     DP_FP_TMP
         lda     [DP_BAS_INT_WA]
         sta     DP_BAS_TMP64
         dey
@L6552:  lda     [DP_BAS_STRWKSP_L],y
         sta     [DP_BAS_TMP64],y
         iny
         cpy     DP_BAS_STRLEN
         bne     @L6552
@L655B:  rts

exec_PRINT_HASH:
         jsr     L956B
         rep     #$10
         .i16
         ldy     DP_BAS_INT_WA
         sty     DP_BAS_CURCHAN
         sep     #$10
         .i8
L6567:   jsr     parse_skip_space_CMPcomma
         bne     L65C2
         jsr     L7537
         jsr     fpCopyFPAtoPrintPTR
         lda     $40
         jsr     BPUT_A_to_CURCHAN
         tax
         beq     L65AB
         bmi     L6598
         ldx     #$03
@L657E:  lda     DP_BAS_INT_WA,x
         jsr     BPUT_A_to_CURCHAN
         dex
         bpl     @L657E
         bra     L6567

BPUT_A_to_CURCHAN:
         phb
         phx
         rep     #$10
         .i16
         ldy     DP_BAS_CURCHAN
         cop     $0b
         jsr     setREPORT_BHA_Cy
         sep     #$10
         .i8
         plx
         plb
         rts

L6598:   ldx     #$04
         ldy     #$05
         phy
@lp:     ply
         dey
         lda     [DP_BAS_PRINT_FP_PTR],y
         phy
         jsr     BPUT_A_to_CURCHAN
         dex
         bpl     @lp
         ply
         bra     L6567

L65AB:   lda     DP_BAS_STRLEN
         jsr     BPUT_A_to_CURCHAN
         tax
         beq     L6567
         phx
@L65B4:  ply
         dey
         lda     [DP_BAS_STRWKSP_L],y
         phy
         jsr     BPUT_A_to_CURCHAN
         dex
         bne     @L65B4
         ply
         bra     L6567

L65C2:   sty     DP_BAS_TXTPTR2_OFF
         jmp     L635B

exec_PRINT:
         jsr     parse_skipSpacesPTR2_cmp_HASH
         beq     exec_PRINT_HASH
         dec     DP_BAS_TXTPTR2_OFF
         bra     exec_PRINT_int

L65D0:   lda     [DP_BAS_ASSWKSP]
         beq     exec_PRINT_int
         lda     DP_BAS_COUNT
@L65D6:  beq     exec_PRINT_int
         sbc     [DP_BAS_ASSWKSP]
         bcs     @L65D6
         tay
@L65DD:  jsr     printSpcOrSendToLOMEMandINC
         iny
         bne     @L65DD
exec_PRINT_int:
         clc
         lda     [DP_BAS_ASSWKSP]
         sta     DP_BAS_TMP2A
@L65E8:  ror     $2b
@L65EA:  jsr     parse_skip_spaces_PTR2
         cmp     #$3a
         beq     @L65F9
         cmp     #$0d
         beq     @L65F9
         cmp     #$8b
         bne     @L6612
@L65F9:  jsr     PrintCRLFresetCOUNT
@L65FC:  jmp     L6359

@L65FF:  stz     DP_BAS_TMP2A
         stz     $2b
         jsr     parse_skip_spaces_PTR2
         cmp     #$3a
         beq     @L65FC
         cmp     #$0d
         beq     @L65FC
         cmp     #$8b
         beq     @L65FC
@L6612:  cmp     #$7e
         beq     @L65E8
         cmp     #$2c
         beq     L65D0
         cmp     #$3b
         beq     @L65FF
         jsr     L66B0
         bcc     @L65EA
         lda     DP_BAS_TMP2A
         pha
         lda     $2b
         pha
         dec     DP_BAS_TXTPTR_OFF
         jsr     L7537
         pla
         sta     $2b
         pla
         sta     DP_BAS_TMP2A
         lda     DP_BAS_TXTPTR_OFF
         sta     DP_BAS_TXTPTR2_OFF
         tya
         beq     @L664E
         jsr     L793A
         lda     DP_BAS_TMP2A
         sec
         sbc     DP_BAS_STRLEN
         bcc     @L664E
         beq     @L664E
         tay
@L6648:  jsr     printSpcOrSendToLOMEMandINC
         dey
         bne     @L6648
@L664E:  lda     DP_BAS_STRLEN
         beq     @L65EA
         ldy     #$00
@L6654:  lda     [DP_BAS_STRWKSP_L],y
         jsr     printCh
         iny
         cpy     DP_BAS_STRLEN
         bne     @L6654
         bra     @L65EA

L6660:   jmp     brk_05_missing_comma

L6663:   lda     DP_BAS_INT_WA
         pha
         jsr     L6C77
         lda     #$1f
         jsr     call_OSWRCH
         pla
         jsr     call_OSWRCH
         jsr     exec_VDU_OSWRCH_WA0
         bra     L66A0

L6677:   jsr     L6C7F
         jsr     parse_skip_space_CMPcomma
         beq     L6663
         cmp     #$29
         bne     L6660
         lda     DP_BAS_INT_WA
         sbc     DP_BAS_COUNT
         beq     L66A0
         tax
         bcs     L6698
         jsr     PrintCRLFresetCOUNT
         bra     L6694

L6691:   jsr     eval_ret_INT
L6694:   ldx     DP_BAS_INT_WA
         beq     L66A0
L6698:   jsr     L9A42
         bra     L66A0

L669D:   jsr     PrintCRLFresetCOUNT
L66A0:   clc
         bra     L66AB

L66A3:   dec     DP_BAS_TXTPTR2_OFF
eval_INTBRK:
         jsr     L7527
         jsr     L6C8F
L66AB:   ldy     DP_BAS_TXTPTR_OFF
         sty     DP_BAS_TXTPTR2_OFF
         rts

L66B0:   ldx     DP_BAS_TXTPTR2
         stx     DP_BAS_TXTPTR
         ldx     DP_BAS_TXTPTR2+1
         stx     DP_BAS_TXTPTR+1
         ldx     DP_BAS_TXTPTR2+2
         stx     DP_BAS_TXTPTR+2
         ldx     DP_BAS_TXTPTR2_OFF
         stx     DP_BAS_TXTPTR_OFF
         cmp     #$27
         beq     L669D
         cmp     #$8a
         beq     L6677
         cmp     #$89
         beq     L6691
L66CC:   sec
L66CD:   rts

brk_09_missing_quote:
         brk     $09

         .byte   tknMissing
         .byte   '"'
         .byte   $00

L66D3:   jsr     parse_skip_spaces_PTR2
         jsr     L66B0
         bcc     L66CD
         cmp     #$22
         bne     L66CC
@L66DF:  iny
         lda     [DP_BAS_TXTPTR],y
         cmp     #$0d
         beq     brk_09_missing_quote
         cmp     #$22
         bne     @L66F3
         iny
         sty     DP_BAS_TXTPTR_OFF
         lda     [DP_BAS_TXTPTR],y
         cmp     #$22
         bne     L66A0
@L66F3:  jsr     printCh
         bra     @L66DF

mem_check_in_bounds:
         jsr     L6725
         bcs     L6712
brk2d_Too_High:
         brk     $2d

         .byte   "Too High"
         .byte   $00

brk2e_Too_Low:
         brk     $2e

         .byte   "Too Low"
         .byte   $00

L6712:   jsr     @L6718
         bcc     brk2e_Too_Low
         rts

@L6718:  lda     DP_BAS_INT_WA
         cmp     #$00
         lda     DP_BAS_INT_WA+1
         sbc     $95
         lda     DP_BAS_INT_WA+2
         sbc     $96
         rts

L6725:   sec
         lda     DP_BAS_INT_WA+3
         bne     @L6736
         lda     DP_BAS_MEMEND
         cmp     DP_BAS_INT_WA
         lda     DP_BAS_MEMEND+1
         sbc     DP_BAS_INT_WA+1
         lda     DP_BAS_MEMEND+2
         sbc     DP_BAS_INT_WA+2
@L6736:  rts

L6737:   jsr     L5F73
         beq     @L6740
         dec     DP_BAS_TXTPTR2_OFF
         eor     #$00
@L6740:  rts

jmp_brk1a_no_such_var1:
         jmp     brk1a_no_such_var

exec_CALL:
         lda     #$4c
         jsr     L6737
         cmp     #$01
         bcc     @L6752
         lda     #$49
         jsr     L6737
@L6752:  php
         jsr     L7527
         jsr     L6C8C
         jsr     stack_INTWA
         lda     #$00
         sta     [DP_BAS_STRWKSP_L]
         ldy     #$00
@L6762:  phy
         jsr     parse_skip_space_CMPcomma
         bne     @L678B
         ldy     DP_BAS_TXTPTR_OFF
         jsr     L6F88
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
         bra     @L6762

@L678B:  ply
         dec     DP_BAS_TXTPTR_OFF
         jsr     parse_nextstmt_yield_PTR2_OFF
         jsr     L993C
         plp
         bcc     @L67AA
         bne     @L67AA
         lda     DP_BAS_STRWKSP_L+2
         pha
         lda     DP_BAS_STRWKSP_L+1
         pha
         lda     DP_BAS_STRWKSP_L
         pha
         phd
         phk
         pea     @L67B8
         php
         bra     L6804

@L67AA:  lda     DP_BAS_STRWKSP_L+2
         pha
         lda     DP_BAS_STRWKSP_L+1
         pha
         lda     DP_BAS_STRWKSP_L
         pha
         phd
         phk
         jsr     L6804
@L67B8:  jsr     L67C2
         pld
         pla
         pla
         pla
         jmp     parse_EOS_TXTPTR2

L67C2:   php
         phb
         phd
         rep     #$30
         .a16
         .i16
         pha
         phx
         phy
         lda     $0d,S
         tcd
         phk
         plb
         pla
         sta     $7c
         pla
         sta     $78
         pla
         sta     $74
         sta     $c2
         pla
         sta     $80
         stz     $84
         stz     $88
         stz     $8c
         stz     $7e
         stz     $7a
         stz     $76
         stz     $86
         stz     $82
         stz     $8a
         stz     $8e
         stz     $c4
         sep     #$30
         .a8
         .i8
         pla
         sta     $86
         sta     $c4
         pla
         sta     $88
         and     #$01
         sta     $8c
         cli
         cld
         rts

L6804:   lda     DP_BAS_INT_WA+2
         pha
         pei     (DP_BAS_INT_WA)
         lda     #$01
         trb     DP_BAS_INT_WA+3
         ldy     #$0c
         and     [DP_BAS_ASSWKSP],y
         ora     DP_BAS_INT_WA+3
         eor     #$30
         pha
         rep     #$30
         .a16
         .i16
         ldy     #$0064
         lda     [DP_BAS_ASSWKSP],y
         pha
         ldy     #$0060
         lda     [DP_BAS_ASSWKSP],y
         tax
         ldy     #$0004
         lda     [DP_BAS_ASSWKSP],y
         pha
         lda     DP_BAS_INT_WA+3
         pha
         ldy     #$0005
         and     #$0080
         beq     @L6838
         ldy     #$0009
@L6838:  lda     [DP_BAS_ASSWKSP],y
         pha
         plb
         plb
         pla
         and     #$0002
         beq     @L6849
         ldy     #$0010
         lda     [DP_BAS_ASSWKSP],y
         tcd
@L6849:  pla
         ply
         rti

         .a8
         .i8
jmp_brk10_Syntax:
         jmp     brk10_Syntax

exec_DELETE:
         jsr     parse_lineno_atOFF
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
         jsr     L993C
@L686F:  jsr     L962A
         jsr     parse_yield
         jsr     INT_inc_WA
         lda     DP_BAS_TMP6+3
         cmp     DP_BAS_INT_WA
         lda     DP_BAS_TMP6+4
         sbc     DP_BAS_INT_WA+1
         bcs     @L686F
         jmp     clear_reset_prog_prompt

L6885:   lda     #$0a
         jsr     retA_8bit_INT
         jsr     parse_lineno_atOFF
         jsr     stack_INTWA
         lda     #$0a
         jsr     retA_8bit_INT
         jsr     parse_SkipSpacesPTR2_cmp_COMMA
         bne     @L68A8
         jsr     parse_lineno_atOFF
         lda     DP_BAS_INT_WA+1
         bne     L690D
         lda     DP_BAS_INT_WA
         beq     L690D
         jmp     parse_nextstmt_yield_TXTOFF2

@L68A8:  jmp     L72D9

L68AB:   lda     DP_BAS_TOP
         sta     $5b
         lda     DP_BAS_TOP+1
         sta     DP_FPB_sgn
         lda     DP_BAS_TOP+2
         sta     $5d
L68B7:   clc
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

exec_RENUMBER:
         jsr     L6885
         ldx     #$58
         jsr     stack_copy6_to_X_uns4
         jsr     do_END
         jsr     L68AB
@L68DB:  lda     [DP_BAS_TMP6]
         bmi     L6915
         sta     [$5b]
         lda     [DP_BAS_TMP6],y
         sta     [$5b],y
         sec
         tya
         adc     $5b
         sta     $5b
         bcc     @L68F3
         inc     DP_FPB_sgn
         bne     @L68F3
         inc     $5d
@L68F3:  cmp     DP_BAS_HIMEM
         lda     DP_FPB_sgn
         sbc     DP_BAS_HIMEM+1
         lda     $5d
         sbc     DP_BAS_HIMEM+2
         bcs     @L6904
         jsr     L69D9
         bra     @L68DB

@L6904:  brk     $00

         .byte   $cc
         .byte   " space"

L690D:   brk     $00

         .byte   "Silly"
         .byte   $00

L6915:   jsr     L68B7
@L6918:  lda     [DP_BAS_TMP6]
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

@L6938:  lda     DP_BAS_PAGE+2
         sta     DP_BAS_TXTPTR2+2
         lda     DP_BAS_PAGE+1
         sta     DP_BAS_TXTPTR2+1
         lda     DP_BAS_PAGE
         sta     DP_BAS_TXTPTR2
@L6944:  ldy     #$01
         lda     [DP_BAS_TXTPTR2],y
         bmi     @L69B9
         ldy     #$04
         stz     DP_BAS_INT_WA+3
@L694E:  lda     [DP_BAS_TXTPTR2],y
         ldx     DP_BAS_INT_WA+3
         bne     @L695C
         cmp     #$8d
         beq     @L6972
         cmp     #$f4
         beq     @L6969
@L695C:  iny
         cmp     #$22
         bne     @L6965
         eor     DP_BAS_INT_WA+3
         sta     DP_BAS_INT_WA+3
@L6965:  cmp     #$0d
         bne     @L694E
@L6969:  ldy     #$03
         lda     [DP_BAS_TXTPTR2],y
         jsr     L73DC
         bra     @L6944

@L6972:  jsr     parse_lineno_atPTR2_Y
         jsr     L68AB
@L6978:  lda     [DP_BAS_TMP6]
         bmi     @L69BB
         lda     [$5b]
         cmp     DP_BAS_INT_WA+1
         bne     @L69A5
         lda     [$5b],y
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
         sta     DP_BAS_TMP6+5
         jsr     L600F
@L69A1:  ldy     DP_BAS_TXTPTR2_OFF
         bra     @L694E

@L69A5:  clc
         jsr     L69D9
         lda     $5b
         adc     #$02
         sta     $5b
         bcc     @L6978
         inc     DP_FPB_sgn
         bne     @L6978
         inc     $5d
         bra     @L6978

@L69B9:  bra     L6A19

@L69BB:  jsr     L9C22
         lsr     DP_FPB_mant+2
         adc     #$6c
         adc     DP_BAS_TMP64
         jsr     L7461
         jsr     $1fb7
         sta     DP_BAS_INT_WA+1
         iny
         lda     [DP_BAS_TXTPTR2],y
         sta     DP_BAS_INT_WA
         jsr     printWAasDecimal16b
         jsr     PrintCRLFresetCOUNT
         bra     @L69A1

L69D9:   iny
         lda     [DP_BAS_TMP6],y
         ldy     #$01
         adc     DP_BAS_TMP6
         sta     DP_BAS_TMP6
         bcc     @L69EB
         inc     DP_BAS_TMP6+1
         bne     @L69EA
         inc     DP_BAS_TMP6+2
@L69EA:  clc
@L69EB:  rts

exec_AUTO:
         jsr     L6885
         lda     DP_BAS_INT_WA
         pha
         jsr     L993C
@L69F5:  jsr     stack_INTWA
         jsr     L7891
         jsr     L95BF
         jsr     L993C
         jsr     L6098
         ldy     #$00
         jsr     L96C0
         jsr     int_CLEAR
         pla
         pha
         clc
         adc     DP_BAS_INT_WA
         sta     DP_BAS_INT_WA
         bcc     @L69F5
         inc     DP_BAS_INT_WA+1
         bpl     @L69F5
L6A19:   jmp     clear_reset_prog_prompt

L6A1C:   jmp     L6BA2

L6A1F:   dec     DP_BAS_TXTPTR2_OFF
         jsr     var_find
         beq     L6A9C
         bcs     L6A9C
         jsr     pushINTWA_on_hw_stack
         jsr     L6C7F
         jsr     INT_inc_WA
         lda     DP_BAS_INT_WA+3
         ora     DP_BAS_INT_WA+2
         bne     L6A9C
         clc
         lda     DP_BAS_INT_WA
         adc     DP_BAS_VARTOP
         xba
         lda     DP_BAS_INT_WA+1
         adc     DP_BAS_VARTOP+1
         tax
         ldy     DP_BAS_VARTOP+2
         bcc     @L6A47
         iny
@L6A47:  xba
         cmp     DP_BAS_STACK
         xba
         txa
         sbc     DP_BAS_STACK+1
         tya
         sbc     DP_BAS_STACK+2
         bcs     L6A1C
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
         lda     #$40
         sta     $40
         jsr     L8D5C
         jsr     L66AB
L6A70:   jsr     parse_SkipSpacesPTR2_cmp_COMMA
         beq     exec_DIM
         jmp     L6359

L6A78:   ldx     #$61
         jsr     stack_copy6_to_X_uns4
L6A7D:   rep     #$30
         .a16
         .i16
         ldx     #$0000
@L6A82:  lsr     DP_FPB_mant+2
         bcc     @L6A8D
         clc
         txa
         adc     DP_BAS_INT_WA
         tax
         bcs     L6A9C
@L6A8D:  lda     DP_FPB_mant+2
         beq     @L6A97
         asl     DP_BAS_INT_WA
         bcs     L6A9C
         bra     @L6A82

@L6A97:  stx     DP_BAS_INT_WA
         sep     #$30
         .a8
         .i8
         rts

L6A9C:   brk     $0a

         .byte   "Bad "
         .byte   $de
         .byte   $00

exec_DIM:
         jsr     parse_skip_spaces_PTR2
         tya
         clc
         adc     DP_BAS_TXTPTR2
         ldx     DP_BAS_TXTPTR2+1
         ldy     DP_BAS_TXTPTR2+2
         bcc     @L6AB6
         inx
         bne     @L6AB5
         iny
@L6AB5:  clc
@L6AB6:  sbc     #$00
         sta     DP_BAS_TMP6
         txa
         sbc     #$00
         sta     DP_BAS_TMP6+1
         tya
         sbc     #$00
         sta     DP_BAS_TMP6+2
         ldx     #$05
         stx     DP_FPB_mant+2
         ldx     DP_BAS_TXTPTR2_OFF
         jsr     varScanNameAtTMP6
         cpy     #$01
         beq     L6A9C
         cmp     #$28
         beq     @L6AEA
         cmp     #$24
         beq     @L6ADF
         cmp     #$25
         bne     @L6AE7
         dec     DP_FPB_mant+2
@L6ADF:  iny
         inx
         lda     [DP_BAS_TMP6],y
         cmp     #$28
         beq     @L6AEA
@L6AE7:  jmp     L6A1F

@L6AEA:  iny
         stx     DP_BAS_TXTPTR2_OFF
         jsr     varFind_00
@L6AF0:  bne     L6A9C
         jsr     L6E93
         ldx     #$01
         jsr     L6EFA
         lda     DP_FPB_mant+2
         pha
         lda     #$01
         pha
         jsr     retA_8bit_INT
@L6B03:  jsr     stack_INTWA
         jsr     eval_INTBRK
         lda     DP_BAS_INT_WA+1
         and     #$c0
         ora     DP_BAS_INT_WA+2
         ora     DP_BAS_INT_WA+3
         bne     L6A9C
         jsr     INT_inc_WA
         ply
         lda     DP_BAS_INT_WA
         sta     [DP_BAS_VARTOP],y
         iny
         lda     DP_BAS_INT_WA+1
         sta     [DP_BAS_VARTOP],y
         iny
         phy
         jsr     L6A78
         jsr     parse_SkipSpacesPTR2_cmp_COMMA
         beq     @L6B03
         cmp     #$29
         bne     @L6AF0
         plx
         pla
         phx
         sta     DP_FPB_mant+2
         stz     DP_FPB_mant+3
         jsr     L6A7D
         pla
         pha
         adc     DP_BAS_INT_WA
         sta     DP_BAS_INT_WA
         bcc     @L6B47
         inc     DP_BAS_INT_WA+1
         bne     @L6B47
         brl     L6A9C

@L6B47:  lda     DP_BAS_VARTOP+2
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
         bcc     @L6B61
         iny
@L6B61:  xba
         cmp     DP_BAS_STACK
         xba
         txa
         sbc     DP_BAS_STACK+1
         tya
         sbc     DP_BAS_STACK+2
         bcs     L6BA2
         xba
         sta     DP_BAS_VARTOP
         stx     DP_BAS_VARTOP+1
         sty     DP_BAS_VARTOP+2
         pla
         sta     [DP_BAS_TMP6]
         adc     DP_BAS_TMP6
         tay
         stz     DP_BAS_TMP6
         bcc     @L6B84
         inc     DP_BAS_TMP6+1
         bne     @L6B84
         inc     DP_BAS_TMP6+2
@L6B84:  lda     #$00
@L6B86:  sta     [DP_BAS_TMP6],y
         iny
         bne     @L6B91
         inc     DP_BAS_TMP6+1
         bne     @L6B91
         inc     DP_BAS_TMP6+2
@L6B91:  cpy     DP_BAS_VARTOP
         bne     @L6B86
         cpx     DP_BAS_TMP6+1
         bne     @L6B86
         lda     DP_BAS_TMP6+2
         cmp     DP_BAS_VARTOP+2
         bne     @L6B84
         jmp     L6A70

L6BA2:   brk     $0b

         .byte   $de
         .byte   " space"
         .byte   $00

exec_HIMEM_set:
         jsr     eval_force_INT
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

exec_LOMEM_set:
         jsr     eval_force_INT
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

exec_PAGE_set:
         jsr     eval_force_INT
         jsr     PAGEset_int
jmp_parse_EOS_TXTPTR2:
         jmp     parse_EOS_TXTPTR2

PAGEset_int:
         ldy     #$00
         lda     DP_BAS_INT_WA
         sta     DP_BAS_PAGE
         sta     [DP_BAS_pointer_at_copy_of_PAGE],y
         iny
         lda     DP_BAS_INT_WA+1
         sta     DP_BAS_PAGE+1
         sta     [DP_BAS_pointer_at_copy_of_PAGE],y
         iny
         lda     DP_BAS_INT_WA+2
         sta     DP_BAS_PAGE+2
         sta     [DP_BAS_pointer_at_copy_of_PAGE],y
         rts

exec_CLEAR:
         jsr     parse_nextstmt_yield_TXTOFF2
         jsr     int_CLEAR
         bra     jmp_parse_EOS_TXTPTR2

exec_TRACE:
         jsr     parse_lineno_atOFF
         bcs     @L6C1B
         cmp     #$ee
         beq     @L6C2C
         cmp     #$87
         beq     @L6C35
         jsr     eval_INTBRK
@L6C1B:  jsr     parse_nextstmt_yield_TXTOFF2
         lda     DP_BAS_INT_WA
         sta     DP_BAS_TRACE_LIM
         lda     DP_BAS_INT_WA+1
@L6C24:  sta     DP_BAS_TRACE_LIM+1
         lda     #$ff
@L6C28:  sta     DP_BAS_TRACEFLAG
         bra     jmp_parse_EOS_TXTPTR2

@L6C2C:  inc     DP_BAS_TXTPTR2_OFF
         jsr     parse_nextstmt_yield_TXTOFF2
         lda     #$ff
         bne     @L6C24

@L6C35:  inc     DP_BAS_TXTPTR2_OFF
         jsr     parse_nextstmt_yield_TXTOFF2
         lda     #$00
         bra     @L6C28

exec_TIME_set:
         iny
         lda     [DP_BAS_TXTPTR2],y
         cmp     #'$'
         beq     @L6C53
         jsr     eval_force_INT
         stz     DP_FPA_sgn
         ldx     #DP_BAS_INT_WA
         ldy     #$00
         lda     #$02
         jmp     L8D43

@L6C53:  inc     DP_BAS_TXTPTR2_OFF
         jsr     PTR_eq_PTR2_exeptEquals
         lda     $40
         bne     jmp_brk06_type_mismatch16
         ldy     DP_BAS_STRLEN
         lda     #$0d
         sta     [DP_BAS_STRWKSP_L],y
         ldx     #$08
         jsr     L8604
         cop     $28
         eor     $4c,S
         eor     f:fpConstPiDiv2
         phk
         plb
         jmp     parse_EOS_TXTPTR2

L6C74:   jsr     storeStringOnStack
L6C77:   jsr     L854D
         bra     L6C8F

eval_commaINTBRK:
         jsr     parse_skip_spaces_CMPcommaBRK
L6C7F:   jsr     L7537
         bra     L6C8F

eval_ret_INT:
         jsr     evalLevel1
         bra     L6C8F

eval_force_INT:
         jsr     PTR_eq_PTR2_exeptEquals
L6C8C:   lda     $40
L6C8E:   tay
L6C8F:   beq     jmp_brk06_type_mismatch16
L6C91:   bpl     L6CC0
eval_real2INT:
         php
         phy
         phx
         phb
         jsr     stack_REAL
         jsr     BHAeqDP_BAS_STACKptr
         ldx     #FN_ARITH_INT
         phk
         jsr     moduleCallARITHref
         jsr     DP_BAS_STACKptreqBHA
         plb
         jsr     L993C
         lda     #RETV_INT
         plx
         ply
         plp
         rts

L6CB0:   lda     DP_FPA_mant
         sta     DP_BAS_INT_WA+3
         lda     DP_FPA_mant+1
         sta     DP_BAS_INT_WA+2
         lda     DP_FPA_mant+2
         sta     DP_BAS_INT_WA+1
         lda     DP_FPA_mant+3
         sta     DP_BAS_INT_WA
L6CC0:   rts

jmp_brk06_type_mismatch16:
         jmp     brk06_type_mismatch

eval_real:
         jsr     evalLevel1
L6CC7:   beq     jmp_brk06_type_mismatch16
         bmi     L6CC0
         jsr     L5117
         rts

exec_PROC:
         lda     DP_BAS_TXTPTR2
         sta     DP_BAS_TXTPTR
         lda     DP_BAS_TXTPTR2+1
         sta     DP_BAS_TXTPTR+1
         lda     DP_BAS_TXTPTR2+2
         sta     DP_BAS_TXTPTR+2
         lda     DP_BAS_TXTPTR2_OFF
         sta     DP_BAS_TXTPTR_OFF
         lda     #$f2       ;;PROC
         jsr     exec_FN_PROC
         jsr     parse_nextstmt_yield_PTR2_OFF
         jmp     parse_EOS_TXTPTR2

st0_at_PTR_WA_plus4:
         ldy     #$04
         lda     #$00
         sta     [DP_BAS_INT_WA],y
         beq     L6D17

         .a16
         .i16
exec_LOCAL:
         tsc
         sec
         sbc     DP_stack_save
         xba
         sbc     DP_stack_save+1
         bcs     L6D2F
         xba
         cmp     #$b0fd
         and     $6f2f20
         beq     L6D2C
         jsr     L8B55
         ldy     DP_BAS_INT_WA+3
         bmi     st0_at_PTR_WA_plus4
         jsr     pushINTWA_on_hw_stack
         jsr     exec_FALSE
         sta     $40
         jsr     L8D5C
         .a8
         .i8
L6D17:   tsx
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
         jmp     L6359

         .a16
         .i16
L6D2C:   jmp     L635B

L6D2F:   brk     $0c

         .byte   "Not "
         .byte   $ea

         .a8
         .i8
brk19_bad_MODE:
         brk     $19

         .byte   "Bad "
         .byte   $eb
         .byte   $00

exec_GCOL:
         jsr     eval_INTBRK
         lda     DP_BAS_INT_WA
         pha
         jsr     eval_commaINTBRK
         jsr     parse_nextstmt_yield_PTR2_OFF
         lda     #$12
         jsr     call_OSWRCH ;gcol escape code
         pla
         bra     OSWRCH_A_then_WA0 ;jump forward to OSWRCH A, WA+0

exec_COLOUR:
         jsr     eval_INTBRK
         jsr     parse_nextstmt_yield_TXTOFF2
         lda     #$11       ;escape code for COLOUR
         bra     OSWRCH_A_then_WA0

exec_MODE:
         jsr     eval_INTBRK
         jsr     parse_nextstmt_yield_TXTOFF2
         jsr     L9B1D
         inx
         bne     @L6D7D
         iny
         bne     @L6D7D
         lda     DP_BAS_STACK
         cmp     DP_BAS_HIMEM
         bne     brk19_bad_MODE
         lda     DP_BAS_STACK+1
         cmp     DP_BAS_HIMEM+1
         bne     brk19_bad_MODE
         lda     DP_BAS_STACK+2
         cmp     DP_BAS_HIMEM+2
         bne     brk19_bad_MODE
@L6D7D:  stz     DP_BAS_COUNT
         lda     #$16
OSWRCH_A_then_WA0:
         jsr     call_OSWRCH
         lda     DP_BAS_INT_WA
         bra     call_OSWRCH_EOS

exec_MOVE:
         lda     #$04
         bra     doMOVEDRAW

exec_DRAW:
         lda     #$05
doMOVEDRAW:
         pha
         jsr     L7527
         jsr     L6C8C
         bra     L6DA0

exec_PLOT:
         jsr     eval_INTBRK
         lda     DP_BAS_INT_WA
         pha
         jsr     eval_commaINTBRK
L6DA0:   jsr     stack_INTWA
         jsr     eval_commaINTBRK
         jsr     parse_nextstmt_yield_PTR2_OFF
         lda     #$19
         jsr     call_OSWRCH ;OSWRCH plot escape code
         pla
         jsr     call_OSWRCH ;OSWRCH plot reason
         jsr     stack_copy6_to_DP_BAS_TMP66_uns4
         lda     DP_BAS_TMP6
         jsr     call_OSWRCH
         lda     DP_BAS_TMP6+1
         jsr     call_OSWRCH
         jsr     exec_VDU_OSWRCH_WA0
         lda     DP_BAS_INT_WA+1
         bra     call_OSWRCH_EOS

exec_CLG:
         jsr     parse_nextstmt_yield_TXTOFF2
         lda     #$10
         bra     call_OSWRCH_EOS

exec_CLS:
         jsr     parse_nextstmt_yield_TXTOFF2
         stz     DP_BAS_COUNT
         lda     #$0c
call_OSWRCH_EOS:
         jsr     call_OSWRCH
         jmp     parse_EOS_TXTPTR2

exec_REPORT:
         jsr     parse_nextstmt_yield_TXTOFF2
         jsr     PrintCRLFresetCOUNT
         jsr     doREPORT
         jmp     parse_EOS_TXTPTR2

doREPORT:
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
@L6DFC:  xba
         pha
         xba
         pha
         ldy     #$00
@L6E02:  lda     ($01,S),y
         beq     @sksl
         cmp     #'/'
         beq     @sksl
         jsr     printChOrSendtoLOMEMandINC
         iny
         bne     @L6E02
@sksl:   pla
         pla
         bra     @L6E19

@L6E14:  lda     #'?'
         jsr     printChOrSendtoLOMEMandINC
@L6E19:  lda     #':'
         jsr     printChOrSendtoLOMEMandINC
         jsr     printSpcOrSendToLOMEMandINC
         phk
         plb
         ldx     #_STSBK    ;Get BRK number
         jsl     _ST
@lp:     ldx     #_STTBK    ;Get BRK text
         jsl     _ST
         bcs     @skDone
         jsr     PrintChOrToken
         bra     @lp

@skDone: plx
         rts

exec_VDU_hibyte:
         lda     DP_BAS_INT_WA+1
         jsr     call_OSWRCH
exec_VDU:
         jsr     parse_skip_spaces_PTR2
@vdulp:  cmp     #':'
         beq     L6E6F
         cmp     #$0d
         beq     L6E6F
         cmp     #$8b       ;ELSE?
         beq     L6E6F
         dec     DP_BAS_TXTPTR2_OFF
         jsr     eval_INTBRK
         jsr     exec_VDU_OSWRCH_WA0
         jsr     parse_SkipSpacesPTR2_cmp_COMMA
         beq     exec_VDU   ;had a comma, go again
         cmp     #';'
         beq     exec_VDU_hibyte ;had a semi, output hi byte
         cmp     #'|'
         beq     exec_VDU_8_zeros ;had a pipe output 8 zeroes
         bra     @vdulp

exec_VDU_8_zeros:
         lda     #$00
         ldy     #$09
@lp:     jsr     call_OSWRCH
         dey
         bne     @lp
         bra     exec_VDU

L6E6F:   jmp     L6359

exec_VDU_OSWRCH_WA0:
         lda     DP_BAS_INT_WA
         jmp     call_OSWRCH

         .byte   $4b
         .byte   $eb
         .byte   $ad
         .byte   $0f
         .byte   $02
         .byte   $48
         .byte   $ad
         .byte   $0e
         .byte   $02
         .byte   $48
         .byte   $eb
         .byte   $08
         .byte   $40

L6E84:   ldy     #$01
         lda     [DP_BAS_TMP6],y
         tax
         lda     #$b1
         cpx     #$f2
         beq     L6EA9
         lda     #$b4
         bra     L6EA9

L6E93:   ldy     #$01
         lda     [DP_BAS_TMP6],y
         cmp     #$40
         bne     @L6E9E
         brl     brk10_Syntax

@L6E9E:  sec
         sbc     #$40
         pha
         asl     A
         clc
         adc     $01,S
         sta     $01,S
         pla
L6EA9:   clc
         adc     DP_BAS_VARS_BASE
         ldx     DP_BAS_VARS_BASE+1
         ldy     DP_BAS_VARS_BASE+2
         bcc     @L6EB6
         inx
         bne     @L6EB6
         iny
@L6EB6:  sta     DP_BAS_TMP6+4
         stx     DP_BAS_TMP6+5
         sty     $5b
         ldy     #$01
         lda     [DP_BAS_TMP6+4],y
         iny
         ora     [DP_BAS_TMP6+4],y
         ora     [DP_BAS_TMP6+4]
         beq     @L6ED3
         dey
         lda     [DP_BAS_TMP6+4],y
         tax
         iny
         lda     [DP_BAS_TMP6+4],y
         tay
         lda     [DP_BAS_TMP6+4]
         bra     @L6EB6

@L6ED3:  lda     DP_BAS_VARTOP+2
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
         beq     L6F2B
@L6EEE:  dey
         lda     [DP_BAS_TMP6],y
         iny
         sta     [DP_BAS_VARTOP],y
         iny
         cpy     DP_BAS_TMP6+3
         bne     @L6EEE
         rts

L6EFA:   lda     #$00
@L6EFC:  sta     [DP_BAS_VARTOP],y
         iny
         dex
         bne     @L6EFC
L6F02:   clc
         tya
         adc     DP_BAS_VARTOP
         bcc     @L6F0E
         inc     DP_BAS_VARTOP+1
         bne     @L6F0E
         inc     DP_BAS_VARTOP+2
@L6F0E:  ldy     DP_BAS_VARTOP+2
         cpy     DP_BAS_STACK+2
         bcc     @L6F29
         bne     @L6F20
         ldy     DP_BAS_VARTOP+1
         cpy     DP_BAS_STACK+1
         bcc     @L6F29
         cmp     DP_BAS_STACK
         bcc     @L6F29
@L6F20:  lda     #$00
         ldy     #$01
         sta     [DP_BAS_TMP6+4],y
         jmp     brk_00_NoRoom

@L6F29:  sta     DP_BAS_VARTOP
L6F2B:   rts

L6F2C:   jsr     L6EFA
var_find:
         jsr     L6F78
         bne     L6F51
         bcs     L6F51
         jsr     L6E93
         ldx     #$05
         cpx     DP_BAS_INT_WA+3
         bne     L6F2C
         inx
         bra     L6F2C

L6F42:   cmp     #$21
         beq     L6F52
         cmp     #$24
         beq     L6F5D
         eor     #$3f
         beq     L6F54
         lda     #$00
         sec
L6F51:   rts

L6F52:   lda     #$04
L6F54:   pha
         inc     DP_BAS_TXTPTR_OFF
         jsr     eval_ret_INT
         jmp     L7067

L6F5D:   inc     DP_BAS_TXTPTR_OFF
         jsr     eval_ret_INT
         lda     DP_BAS_INT_WA+1
         ora     DP_BAS_INT_WA+2
         beq     @L6F6E
         lda     #$80
         sta     DP_BAS_INT_WA+3
         sec
         rts

@L6F6E:  brk     $08

         .byte   "$ range"
         .byte   $00

L6F78:   lda     DP_BAS_TXTPTR2
         sta     DP_BAS_TXTPTR
         lda     DP_BAS_TXTPTR2+1
         sta     DP_BAS_TXTPTR+1
         lda     DP_BAS_TXTPTR2+2
         sta     DP_BAS_TXTPTR+2
         ldy     DP_BAS_TXTPTR2_OFF
         dey
L6F87:   iny
L6F88:   sty     DP_BAS_TXTPTR_OFF
         lda     [DP_BAS_TXTPTR],y
         cmp     #$20
         beq     L6F87
L6F90:   sta     $5b
         cmp     #$40
         bcc     L6F42
         cmp     #$5b
         bcs     @L6FBA
         asl     A
         asl     A
         sta     DP_BAS_INT_WA
         iny
         lda     [DP_BAS_TXTPTR],y
         cmp     #$25
         bne     @L6FBA
         stz     $5b
         lda     DP_BAS_ASSWKSP+1
         sta     DP_BAS_INT_WA+1
         lda     DP_BAS_ASSWKSP+2
         sta     DP_BAS_INT_WA+2
         ldx     #$04
         stx     DP_BAS_INT_WA+3
         iny
         lda     [DP_BAS_TXTPTR],y
         cmp     #$28
         bne     @L702C
@L6FBA:  ldx     #$05
         stx     DP_BAS_INT_WA+3
         clc
         ldy     DP_BAS_TXTPTR+1
         ldx     DP_BAS_TXTPTR+2
         lda     DP_BAS_TXTPTR_OFF
         bne     @L6FD4
         dec     A
         adc     DP_BAS_TXTPTR
         bcs     @L6FDD
         dey
         cpy     #$ff
         bne     @L6FDD
         dex
         bra     @L6FDD

@L6FD4:  dec     A
         adc     DP_BAS_TXTPTR
         bcc     @L6FDD
         iny
         bne     @L6FDD
         inx
@L6FDD:  sta     DP_BAS_TMP6
         sty     DP_BAS_TMP6+1
         stx     DP_BAS_TMP6+2
         ldy     #$00
         ldx     DP_BAS_TXTPTR_OFF
         dex
@L6FE8:  inx
         iny
         lda     [DP_BAS_TMP6],y
         cmp     #$40
         bcs     @L6FFF
         cmp     #$30
         bcc     @L700B
         cmp     #$3a
         bcc     @L6FE8
         bra     @L700B

@L6FFA:  jsr     L70AD
         bra     @L7028

@L6FFF:  cmp     #$5b
         bcc     @L6FE8
         cmp     #$5f
         bcc     @L700B
         cmp     #$7b
         bcc     @L6FE8
@L700B:  cpy     #$01
         beq     @L703A
         cmp     #$24
         beq     L708B
         cmp     #$25
         bne     @L701D
         dec     DP_BAS_INT_WA+3
         inx
         iny
         lda     [DP_BAS_TMP6],y
@L701D:  cmp     #$28
         beq     @L6FFA
         jsr     varFind_00
         beq     L703E
         stx     DP_BAS_TXTPTR_OFF
@L7028:  ldy     DP_BAS_TXTPTR_OFF
         lda     [DP_BAS_TXTPTR],y
@L702C:  cmp     #$21
         beq     L7042
         eor     #$3f
         beq     L7044
         clc
         sty     DP_BAS_TXTPTR_OFF
         lda     #$ff
         rts

@L703A:  lda     #$00
         sec
         rts

L703E:   lda     #$00
         clc
         rts

L7042:   lda     #$04
L7044:   pha
         iny
         sty     DP_BAS_TXTPTR_OFF
         jsr     L8B74
         jsr     L6C8F
         ldx     #$04
@L7050:  lda     $47,x
         pha
         dex
         bne     @L7050
         jsr     eval_ret_INT
         ldx     #$00
         ldy     #$04
         clc
@L705E:  pla
         adc     DP_BAS_INT_WA,x
         sta     DP_BAS_INT_WA,x
         inx
         dey
         bne     @L705E
L7067:   jsr     @L7073
         pla
         sta     DP_BAS_INT_WA+3
         stz     $5b
         clc
         lda     #$ff
         rts

@L7073:  jsr     L5F88
         bne     @L708A
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
@L708A:  rts

L708B:   inx
         iny
         lda     [DP_BAS_TMP6],y
         cmp     #$28
         beq     @L70A0
         jsr     varFind_00
         beq     L703E
         stx     DP_BAS_TXTPTR_OFF
@L709A:  lda     #$81
         sta     DP_BAS_INT_WA+3
         sec
         rts

@L70A0:  jsr     L70AD
         bra     @L709A

brk_0e_Array:
         brk     $0e

         .byte   "Array"
         .byte   $00

L70AD:   inx
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

@L70CB:  jsr     exec_FALSE
         lda     #$01
         sta     DP_BAS_INT_WA+3
@L70D2:  jsr     stack_INTWA
         jsr     L6C7F
         inc     DP_BAS_TXTPTR_OFF
         cpx     #','
         bne     brk_0e_Array
         ldx     #$58
         jsr     stack_copy6_to_X_uns4
         ldy     $5b
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
         jsr     @L71B9
         sty     DP_BAS_INT_WA+3
         lda     [DP_BAS_TMP6],y
         sta     DP_FPB_mant+2
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
@L7113:  jsr     L6A7D
         sec
         lda     [DP_BAS_TMP6]
         sbc     DP_BAS_INT_WA+3
         cmp     #$03
         bcs     @L70D2
         jsr     stack_INTWA
         jsr     L6C77
         pla
         sta     DP_BAS_TMP6+2
         pla
         sta     DP_BAS_TMP6+1
         pla
         sta     DP_BAS_TMP6
         ldx     #$58
         jsr     stack_copy6_to_X_uns4
         ldy     $5b
         jsr     @L71B9
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
@L714D:  jsr     L854D
         jsr     L6C8F
         pla
         sta     DP_BAS_TMP6+2
         pla
         sta     DP_BAS_TMP6+1
         pla
         sta     DP_BAS_TMP6
         ldy     #$01
         jsr     @L71B9
@L7161:  pla
         sta     DP_BAS_INT_WA+3
         cmp     #$05
         bne     @L718C
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
         bra     @L7198

@L718C:  asl     DP_BAS_INT_WA
         rol     DP_BAS_INT_WA+1
         rol     DP_BAS_INT_WA+2
         asl     DP_BAS_INT_WA
         rol     DP_BAS_INT_WA+1
         rol     DP_BAS_INT_WA+2
@L7198:  tya
         adc     DP_BAS_INT_WA
         sta     DP_BAS_INT_WA
         bcc     @L71A6
         inc     DP_BAS_INT_WA+1
         bne     @L71A5
         inc     DP_BAS_INT_WA+2
@L71A5:  clc
@L71A6:  lda     DP_BAS_TMP6
         adc     DP_BAS_INT_WA
         sta     DP_BAS_INT_WA
         lda     DP_BAS_TMP6+1
         adc     DP_BAS_INT_WA+1
         sta     DP_BAS_INT_WA+1
         lda     DP_BAS_TMP6+2
         adc     DP_BAS_INT_WA+2
         sta     DP_BAS_INT_WA+2
         rts

@L71B9:  lda     DP_BAS_INT_WA+1
         and     #$c0
         ora     DP_BAS_INT_WA+2
         ora     DP_BAS_INT_WA+3
         bne     @L71D0
         lda     DP_BAS_INT_WA
         cmp     [DP_BAS_TMP6],y
         iny
         lda     DP_BAS_INT_WA+1
         sbc     [DP_BAS_TMP6],y
         bcs     @L71D0
         iny
         rts

@L71D0:  brk     $0f

         .byte   "Subscript"
         .byte   $00

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
         bcc     @rts       ;<'0' exit
         cmp     #'@'
         bcs     @sk1       ;>='@'
         cmp     #':'
         bcs     @rts       ;between ':' and '?' exit
         cpy     #$01
         beq     @rts       ;if we got here and it's the first character exit
@again:  inx
         iny
         bne     varScanNameAtTMP6_Y
@sk1:    cmp     #'_'
         bcs     @cklc      ;>='_'
         cmp     #'['
         bcc     @again     ;<'[' upper case chars continue
@rts:    rts

@cklc:   cmp     #'{'
         bcc     @again     ;<';' lower case continue
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

clcrts:  clc
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

brk_04_Mistake:
         brk     $04

         .byte   "Mistake"

brk10_Syntax:
         brk     $10

         .byte   "Syntax error"

brk_0d_NoPROC:
         brk     $0d

         .byte   "No "
         .byte   tknPROC

brk_11_Escape:
         brk     $11

         .byte   "Escape"
         .byte   $00

parseEqualsOrMistake:
         jsr     parse_skip_spaces
         cmp     #'='
         bne     brk_04_Mistake
         rts

L727F:   jsr     L7537
L7282:   txa
         ldy     DP_BAS_TXTPTR_OFF
; Check that we're on an end of statement and update TXTPTR from TXTPTR2/Y/OFF2
parse_checkEOS_updTXTPTRfrom2:
         cmp     #':'
         beq     parse_updTXTPTR_from_TXTPTR2_Y
         cmp     #$8b
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

L72B1:   jsr     parse_setEOSflag_Yeq1
         bra     parse_yield

parse_nextstmt_yield_PTR2_OFF:
         ldy     DP_BAS_TXTPTR_OFF
         bra     parse_nextstmt_yield_TXTPTR2_Y

exec_ENDPROC:
         tsc
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
         cmp     #$f2
         bne     brk_0d_NoPROC
parse_nextstmt_yield_TXTOFF2:
         ldy     DP_BAS_TXTPTR2_OFF
parse_nextstmt_yield_TXTPTR2_Y:
         dey
@L72D2:  iny
         lda     [DP_BAS_TXTPTR2],y
         cmp     #' '
         beq     @L72D2
L72D9:   jsr     parse_checkEOS_updTXTPTRfrom2
parse_yield:
         lda     #$40
         bit     DP_BAS_CO_FLAGS
         bne     @L72EC
         dec     DP_BAS_CO_CTDN
         bne     @L72EC
         cop     $21        ;YIELD
         lda     #$64
         sta     DP_BAS_CO_CTDN
@L72EC:  phx
         lda     #$03
         ldx     #$02
         jsl     _EV
         bcc     @L7309
         lda     #$01
         jsl     _EV
         bcc     @L7302
         brl     brk_11_Escape

@L7302:  jsr     cmp_ERRORPTRisDEFAULT
         lda     #$03
         cop     $16
@L7309:  plx
         rts

L730B:   lda     #$7e
         jsr     call_OSBYTE
         bit     DP_BAS_CO_FLAGS
         bmi     @L7317
         brl     @L735A

@L7317:  bvc     @L731C
         brl     @L735A

@L731C:  lda     #$5b
         sta     DP_BAS_TMP6
         lda     #$73
         sta     DP_BAS_TMP6+1
         phk
         pla
         sta     DP_BAS_TMP6+2
         ldy     #$07
         jsr     varFindPROCFN
         bne     @L7335
         jsr     L88C8
         sec
         beq     @L7359
@L7335:  lda     #$40
         tsb     DP_BAS_CO_FLAGS
         lda     #$5d
         sta     DP_BAS_TXTPTR2
         sta     DP_BAS_TXTPTR
         lda     #$73
         sta     DP_BAS_TXTPTR2+1
         sta     DP_BAS_TXTPTR+1
         phk
         pla
         sta     DP_BAS_TXTPTR2+2
         sta     DP_BAS_TXTPTR+2
         stz     DP_BAS_TXTPTR2_OFF
         stz     DP_BAS_TXTPTR_OFF
         lda     #$f2
         jsr     exec_FN_PROC
         lda     #$40
         trb     DP_BAS_CO_FLAGS
         clc
@L7359:  rts

@L735A:  sec
         rts

         .byte   $f2
         .byte   "@HELP",$0d

HELP:    lda     #$10
         bit     DP_BAS_CO_FLAGS
         bpl     @L73A8
         bne     @L73A8
         lda     #$a9
         sta     DP_BAS_TMP6
         lda     #$73
         sta     DP_BAS_TMP6+1
         phk
         pla
         sta     DP_BAS_TMP6+2
         ldy     #$07
         jsr     varFindPROCFN
         bne     @L7383
         jsr     L88C8
         beq     @L73A8
@L7383:  lda     #$10
         tsb     DP_BAS_CO_FLAGS
         lda     #$ab
         sta     DP_BAS_TXTPTR2
         sta     DP_BAS_TXTPTR
         lda     #$73
         sta     DP_BAS_TXTPTR2+1
         sta     DP_BAS_TXTPTR+1
         phk
         pla
         sta     DP_BAS_TXTPTR2+2
         sta     DP_BAS_TXTPTR+2
         stz     DP_BAS_TXTPTR2_OFF
         stz     DP_BAS_TXTPTR_OFF
         lda     #$f2
         jsr     exec_FN_PROC
         lda     #$10
         trb     DP_BAS_CO_FLAGS
         clc
         rts

@L73A8:  sec
         rts

         .byte   $f2
         .byte   "@KILL",$0d

L73B1:   jsr     parse_nextstmt_yield_TXTOFF2
         lda     [DP_BAS_TXTPTR2]
         cmp     #$3a
         beq     anRTS
         lda     DP_BAS_TXTPTR2+1
         cmp     DP_BAS_MEMBASE+1
         bne     L73C6
         lda     DP_BAS_TXTPTR2+2
         cmp     DP_BAS_MEMBASE+2
         beq     L73EE
L73C6:   ldy     #$01
         lda     [DP_BAS_TXTPTR2],y
         bmi     L73EE
         ldx     DP_BAS_TRACEFLAG
         beq     @L73DA
         sta     DP_BAS_INT_WA+1
         iny
         lda     [DP_BAS_TXTPTR2],y
         sta     DP_BAS_INT_WA
         jsr     doTracePrintLineNoIfInRange
@L73DA:  lda     #$03
L73DC:   clc
         adc     DP_BAS_TXTPTR2
         sta     DP_BAS_TXTPTR2
         bcc     @L73E9
         inc     DP_BAS_TXTPTR2+1
         bne     @L73E9
         inc     DP_BAS_TXTPTR2+2
@L73E9:  ldy     #$01
         sty     DP_BAS_TXTPTR2_OFF
anRTS:   rts

L73EE:   jmp     reset_prog_prompt

jmp_brk06_type_mismatch15:
         jmp     brk06_type_mismatch

exec_IF: jsr     L7527
         beq     jmp_brk06_type_mismatch15
         bpl     @L73FE
         jsr     eval_real2INT
@L73FE:  ldy     DP_BAS_TXTPTR_OFF
         sty     DP_BAS_TXTPTR2_OFF
         lda     DP_BAS_INT_WA
         ora     DP_BAS_INT_WA+1
         ora     DP_BAS_INT_WA+2
         ora     DP_BAS_INT_WA+3
         beq     IFdoELSE
         stz     DP_BAS_INT_WA+3
         cpx     #$8c
         beq     THENskip   ;check if last token parsed was THEN
IFexecthis:
         jmp     L6364

THENskip:
         inc     DP_BAS_TXTPTR2_OFF
L7417:   jsr     parse_lineno_atOFF
         bcc     IFexecthis
         jsr     prog_search_lineno_brknotfnd
         jsr     parse_setEOSflag_Yeq1
         jsr     parse_yield
         jmp     L91C1

IFdoELSE:
         ldy     DP_BAS_TXTPTR2_OFF
@lpEL:   lda     [DP_BAS_TXTPTR2],y
         cmp     #$0d
         beq     @L7439     ;end of line continue
         iny
         cmp     #$8b       ;got ELSE?
         bne     @lpEL
         sty     DP_BAS_TXTPTR2_OFF
         beq     L7417

@L7439:  jmp     parse_yield_cont_or_prompt

doTracePrintLineNoIfInRange:
         lda     DP_BAS_INT_WA
         cmp     DP_BAS_TRACE_LIM
         lda     DP_BAS_INT_WA+1
         sbc     DP_BAS_TRACE_LIM+1
         bcs     anRTS
         lda     #'['
         jsr     printCh
         jsr     printWAasDecimal16b
         lda     #']'
         jsr     printCh
         jmp     printSpcOrSendToLOMEMandINC

L7456:   pla
         sta     DP_BAS_INT_WA
         pla
         sta     DP_BAS_INT_WA+1
         pla
         sta     DP_BAS_INT_WA+2
         pla
         .byte   $85
L7461:   phk
         jsr     stack_REAL
         jsr     L5117
         jsr     fpCopyFPAtoFPB
         jsr     popFPFromStackToPTR1
         jsr     fpCopyPTR1toFPA
         bra     L7483

L7473:   jsr     stack_REAL
         jsr     L764D
         tay
         jsr     L6CC7
         jsr     popFPFromStackToPTR1
L7480:   jsr     fpMoveRealAtPTR1toFPB
L7483:   bra     @L7485

@L7485:  ldy     #$00
         lda     #$7f
         trb     DP_FPB_sgn
         lda     DP_FPA_sgn
         and     #$80
         cmp     DP_FPB_sgn
         bne     @L74B1
         lda     DP_FPB_exp
         cmp     DP_FPA_exp+1
         bne     @L74B2
         lda     DP_FPB_mant
         cmp     DP_FPA_mant
         bne     @L74B2
         lda     DP_FPB_mant+1
         cmp     DP_FPA_mant+1
         bne     @L74B2
         lda     DP_FPB_mant+2
         cmp     DP_FPA_mant+2
         bne     @L74B2
         lda     DP_FPB_mant+3
         cmp     DP_FPA_mant+3
         bne     @L74B2
@L74B1:  rts

@L74B2:  ror     A
         eor     DP_FPB_sgn
         rol     A
         lda     #$01
         rts

jmp_brk06_type_mismatch14:
         jmp     brk06_type_mismatch

L74BC:   txa
L74BD:   beq     @L74F8
         bmi     L7473
         lda     DP_BAS_INT_WA+3
         pha
         lda     DP_BAS_INT_WA+2
         pha
         lda     DP_BAS_INT_WA+1
         pha
         lda     DP_BAS_INT_WA
         pha
         jsr     L764D
         tay
         beq     jmp_brk06_type_mismatch14
         bpl     @L74D8
         brl     L7456

@L74D8:  lda     DP_BAS_INT_WA+3
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

@L74F8:  jsr     storeStringOnStack
         jsr     L764D
         tay
         bne     jmp_brk06_type_mismatch14
         lda     [DP_BAS_STACK]
         cmp     DP_BAS_STRLEN
         bcc     @L7509
         lda     DP_BAS_STRLEN
@L7509:  sta     DP_BAS_TMP6
         dey
@L750C:  iny
         cpy     DP_BAS_TMP6
         beq     @L751B
         iny
         lda     [DP_BAS_STACK],y
         dey
         cmp     [DP_BAS_STRWKSP_L],y
         beq     @L750C
         bra     @L751F

@L751B:  lda     [DP_BAS_STACK]
         cmp     DP_BAS_STRLEN
@L751F:  php
         jsr     L9937
         ldy     #$00
         plp
         rts

L7527:   lda     DP_BAS_TXTPTR2
         sta     DP_BAS_TXTPTR
         lda     DP_BAS_TXTPTR2+1
         sta     DP_BAS_TXTPTR+1
         lda     DP_BAS_TXTPTR2+2
         sta     DP_BAS_TXTPTR+2
         lda     DP_BAS_TXTPTR2_OFF
         sta     DP_BAS_TXTPTR_OFF
L7537:   jsr     @L757F
@L753A:  cpx     #$84
         beq     @L7548
         cpx     #$82
         beq     @L7563
         dec     DP_BAS_TXTPTR_OFF
         tay
         sta     $40
         rts

@L7548:  jsr     @L7579
         jsr     L6C8E
         ldy     #$03
         phx
@L7551:  lda     [DP_BAS_STACK],y
         tyx
         ora     DP_BAS_INT_WA,x
         sta     DP_BAS_INT_WA,x
         dey
         bpl     @L7551
         plx
@L755C:  jsr     stack_ADD4
         lda     #$40
         bra     @L753A

@L7563:  jsr     @L7579
         jsr     L6C8E
         ldy     #$03
         phx
@L756C:  lda     [DP_BAS_STACK],y
         tyx
         eor     DP_BAS_INT_WA,x
         sta     DP_BAS_INT_WA,x
         dey
         bpl     @L756C
         plx
         bra     @L755C

@L7579:  jsr     L6C8E
         jsr     stack_INTWA
@L757F:  jsr     @L75A8
@L7582:  cpx     #$80
         beq     @L7587
         rts

@L7587:  jsr     L6C8E
         jsr     stack_INTWA
         jsr     @L75A8
         jsr     L6C8E
         ldy     #$03
         phx
@L7596:  lda     [DP_BAS_STACK],y
         tyx
         and     DP_BAS_INT_WA,x
         sta     DP_BAS_INT_WA,x
         dey
         bpl     @L7596
         plx
         jsr     stack_ADD4
         lda     #$40
         bra     @L7582

@L75A8:  jsr     L764D
         cpx     #$3f
         bcs     @L75B3
         cpx     #$3c
         bcs     @L75B4
@L75B3:  rts

@L75B4:  beq     @L75CC
         cpx     #$3e
         beq     @L75F4
         tax
         jsr     L74BD
         bne     @L75C1
@L75C0:  dey
@L75C1:  sty     DP_BAS_INT_WA
         sty     DP_BAS_INT_WA+1
         sty     DP_BAS_INT_WA+2
         sty     DP_BAS_INT_WA+3
         lda     #$40
         rts

@L75CC:  tax
         ldy     DP_BAS_TXTPTR_OFF
         lda     [DP_BAS_TXTPTR],y
         cmp     #$3d
         beq     @L75E0
         cmp     #$3e
         beq     @L75EB
         jsr     L74BC
         bcc     @L75C0
         bra     @L75C1

@L75E0:  inc     DP_BAS_TXTPTR_OFF
         jsr     L74BC
         beq     @L75C0
         bcc     @L75C0
         bra     @L75C1

@L75EB:  inc     DP_BAS_TXTPTR_OFF
         jsr     L74BC
         bne     @L75C0
         bra     @L75C1

@L75F4:  tax
         ldy     DP_BAS_TXTPTR_OFF
         lda     [DP_BAS_TXTPTR],y
         cmp     #$3d
         beq     @L7606
         jsr     L74BC
         beq     @L75C1
         bcs     @L75C0
         bra     @L75C1

@L7606:  inc     DP_BAS_TXTPTR_OFF
         jsr     L74BC
         bcs     @L75C0
         bra     @L75C1

L760F:   brk     $13

         .byte   "String too long"
         .byte   $00

L7621:   jsr     storeStringOnStack
         jsr     L784D
         tay
         beq     @L762D
         brl     jmp_brk06_type_mismatch13

@L762D:  clc
         phx
         lda     [DP_BAS_STACK]
         adc     DP_BAS_STRLEN
         bcs     L760F
         tax
         pha
         ldy     DP_BAS_STRLEN
@L7639:  dex
         dey
         lda     [DP_BAS_STRWKSP_L],y
         jsr     storeStringX
         bne     @L7639
         jsr     L9928
         pla
         sta     DP_BAS_STRLEN
         plx
         lda     #$00
         bra     L7650

L764D:   jsr     L7801
L7650:   cpx     #$2b
         beq     @L765C
         cpx     #$2d
         bne     @L765B
         brl     L76CA

@L765B:  rts

@L765C:  tay
         beq     L7621
         bmi     L76A0
         jsr     L77FE
         tay
         beq     jmp_brk06_type_mismatch13
         bmi     L76BD
         bra     @L766D

         .byte   $80
         .byte   $e3

@L766D:  clc
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
L7688:   sta     DP_BAS_INT_WA+3
         clc
         lda     DP_BAS_STACK
         adc     #$04
         sta     DP_BAS_STACK
         lda     #$40
         bcc     L7650
         inc     DP_BAS_STACK+1
         bne     L7650
         inc     DP_BAS_STACK+2
         bra     L7650

jmp_brk06_type_mismatch13:
         jmp     brk06_type_mismatch

L76A0:   jsr     stack_REAL
         jsr     L7801
         tay
         beq     jmp_brk06_type_mismatch13
         stx     $40
         bmi     L76B0
         jsr     L5117
L76B0:   jsr     popFPFromStackToPTR1
         jsr     L7E91
L76B6:   ldx     $40
         lda     #$ff
         brl     L7650

L76BD:   stx     $40
         jsr     L993C
         jsr     stack_REAL
         jsr     L5117
         bra     L76B0

L76CA:   tay
         beq     jmp_brk06_type_mismatch13
         bmi     @L76F8
         jsr     L77FE
         tay
         beq     jmp_brk06_type_mismatch13
         bmi     @L7713
         bra     @L76DA

         .byte   $08

@L76DA:  sec
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
         brl     L7688

@L76F8:  jsr     stack_REAL
         jsr     L7801
         tay
         bne     @L7704
         brl     jmp_brk06_type_mismatch13

@L7704:  stx     $40
         bmi     @L770B
         jsr     L5117
@L770B:  jsr     popFPFromStackToPTR1
         jsr     L7E8E
         bra     L76B6

@L7713:  stx     $40
         jsr     L993C
         jsr     stack_REAL
         jsr     L5117
         jsr     popFPFromStackToPTR1
         jsr     L83AD
         brl     L76B6

L7727:   jsr     L5117
L772A:   jsr     L993C
         jsr     stack_REAL
         jsr     L5117
         bra     L7742

L7735:   jsr     L5117
L7738:   jsr     stack_REAL
         jsr     L784D
         tay
         jsr     L6CC7
L7742:   jsr     popFPFromStackToPTR1
         jsr     L7EAE
         lda     #$ff
         jmp     L7804

         .byte   $08
         .byte   $5a
         .byte   $da
         .byte   $8b
         .byte   $20
         .byte   $44
         .byte   $98
         .byte   $20
         .byte   $d6
         .byte   $78
         .byte   $a2
         .byte   $0c
         .byte   $4b
         .byte   $20
         .byte   $e7
         .byte   $9d
         .byte   $20
         .byte   $e0
         .byte   $78
         .byte   $ab
         .byte   $20
         .byte   $2a
         .byte   $98
         .byte   $20
         .byte   $7a
         .byte   $7d
         .byte   $fa
         .byte   $7a
         .byte   $28
         .byte   $a9
         .byte   $ff
         .byte   $4c
         .byte   $04
         .byte   $78

jmp_brk06_type_mismatch12:
         jmp     brk06_type_mismatch

L7772:   tay
         beq     jmp_brk06_type_mismatch12
         bmi     L7738
         ldy     DP_BAS_INT_WA+3
         cpy     DP_BAS_INT_WA+2
         bne     L7735
         lda     DP_BAS_INT_WA+1
         asl     A
         adc     #$00
         tya
         bne     L7735
         jsr     L784A
         tay
         beq     jmp_brk06_type_mismatch12
         bmi     L772A
         ldy     DP_BAS_INT_WA+3
         cpy     DP_BAS_INT_WA+2
         bne     L7727
         lda     DP_BAS_INT_WA+1
         asl     A
         tya
         adc     #$00
         bne     L7727
         phy
         jsr     L83A4
         stx     $40
         bra     @L77A3

@L77A3:  ldx     #$58
         jsr     L9A49
         jsr     L993C
         pla
         eor     DP_BAS_INT_WA+3
         sta     DP_BAS_TMP6
         jsr     L83A4
         ldy     #$00
         ldx     #$00
         stz     DP_FPB_mant+1
         stz     DP_FPB_mant+2
@L77BB:  lsr     DP_BAS_TMP6+4
         ror     DP_BAS_TMP6+3
         bcc     @L77D6
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
         lda     DP_FPB_mant+2
         adc     DP_BAS_INT_WA+3
         sta     DP_FPB_mant+2
@L77D6:  asl     DP_BAS_INT_WA
         rol     DP_BAS_INT_WA+1
         rol     DP_BAS_INT_WA+2
         rol     DP_BAS_INT_WA+3
         lda     DP_BAS_TMP6+3
         ora     DP_BAS_TMP6+4
         bne     @L77BB
         sty     DP_FPB_exp
         stx     DP_FPB_mant
         lda     DP_BAS_TMP6
         php
L77EB:   ldx     #$5e
L77ED:   jsr     L80B4
         plp
         bpl     L77F6
         jsr     L8400
L77F6:   ldx     $40
         brl     L7804

L77FB:   jmp     L7772

L77FE:   jsr     stack_INTWA
L7801:   jsr     L784D
L7804:   cpx     #$2a
         beq     L77FB
         cpx     #$2f
         beq     @L7815
         cpx     #$83
         beq     @L7830
         cpx     #$81
         beq     @L7838
         rts

@L7815:  tay
         jsr     L6CC7
         jsr     stack_REAL
         jsr     L784D
         stx     $40
         tay
         jsr     L6CC7
         jsr     popFPFromStackToPTR1
         jsr     L7DF2
         lda     #$ff
         brl     L77F6

@L7830:  jsr     L5082
         lda     DP_BAS_TMP6+1
         php
         bra     L77EB

@L7838:  jsr     L5082
         rol     DP_BAS_TMP6+5
         rol     $5b
         rol     DP_FPB_sgn
         rol     $5d
         bit     DP_BAS_TMP6
         php
         ldx     #$5a
         bra     L77ED

L784A:   jsr     stack_INTWA
L784D:   jsr     evalLevel1
@L7850:  pha
@L7851:  ldy     DP_BAS_TXTPTR_OFF
         inc     DP_BAS_TXTPTR_OFF
         lda     [DP_BAS_TXTPTR],y
         cmp     #$20
         beq     @L7851
         tax
         pla
         cpx     #$5e
         beq     @L7862
         rts

@L7862:  tay
         jsr     L6CC7
         jsr     stack_REAL
         jsr     eval_real
         php
         phy
         phx
         phb
         jsr     stack_REAL
         jsr     BHAeqDP_BAS_STACKptr
         ldx     #$10
         phk
         jsr     moduleCallARITHref
         jsr     DP_BAS_STACKptreqBHA
         plb
         jsr     popFPFromStackToPTR1
         jsr     fpCopyPTR1toFPA
         plx
         ply
         plp
         lda     #$ff
         bra     @L7850

printWAasDecimal16b:
         lda     #$00
         bra     L7893

L7891:   lda     #$05
L7893:   sta     DP_BAS_TMP2A
         ldx     #$04
@L7897:  stz     DP_FPB_mant+2,x
         sec
@L789A:  lda     DP_BAS_INT_WA
         sbc     tblDigitsLo,x
         tay
         lda     DP_BAS_INT_WA+1
         sbc     tblDigitsHi,x
         bcc     @L78AF
         sta     DP_BAS_INT_WA+1
         sty     DP_BAS_INT_WA
         inc     DP_FPB_mant+2,x
         bra     @L789A

@L78AF:  dex
         bpl     @L7897
         ldx     #$05
@L78B4:  dex
         beq     @L78BB
         lda     DP_FPB_mant+2,x
         beq     @L78B4
@L78BB:  stx     DP_BAS_TMP6
         lda     DP_BAS_TMP2A
         beq     @L78CB
         sbc     DP_BAS_TMP6
         beq     @L78CB
         tax
         jsr     L9A42
         ldx     DP_BAS_TMP6
@L78CB:  lda     DP_FPB_mant+2,x
         ora     #$30
         jsr     printChOrSendtoLOMEMandINC
         dex
         bpl     @L78CB
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

L78EA:   tya
         bpl     @L78F0
         jsr     eval_real2INT
@L78F0:  ldx     #$00
         ldy     #$00
@L78F4:  phx
         tyx
         lda     DP_BAS_INT_WA,x
         plx
         pha
         and     #$0f
         sta     $5d,x
         pla
         lsr     A
         lsr     A
         lsr     A
         lsr     A
         inx
         sta     $5d,x
         inx
         iny
         cpy     #$04
         bne     @L78F4
@L790C:  dex
         beq     @L7913
         lda     $5d,x
         beq     @L790C
@L7913:  lda     $5d,x
         cmp     #$0a
         bcc     @L791B
         adc     #$06
@L791B:  adc     #$30
         jsr     L7AF7
         dex
         bpl     @L7913
         rts

L7924:   bpl     L792D
         lda     #$2d
         stz     DP_FPA_sgn
         jsr     L7AF7
L792D:   lda     DP_FPA_exp+1
         cmp     #$81
         bcs     L7984
         jsr     fpMulBy10
         dec     DP_FP_TMP+5
         bra     L792D

L793A:   phy
         ldy     #$02
         lda     [DP_BAS_ASSWKSP],y
         tax
         dey
         lda     [DP_BAS_ASSWKSP],y
         xba
         ply
         cpx     #$03
         bcc     @L794B
         ldx     #$00
@L794B:  stx     DP_BAS_TMP6
         xba
         beq     @L7956
         cmp     #$0a
         bcs     L795A
         bra     L795C

@L7956:  cpx     #$02
         beq     L795C
L795A:   lda     #$0a
L795C:   sta     DP_BAS_TMP6+1
         sta     $70
         stz     DP_BAS_STRLEN
         stz     DP_FP_TMP+5
         bit     $2b
         bmi     L78EA
         tya
         bmi     @L796E
         jsr     L5117
@L796E:  jsr     fpFPACkMant0SetSgnExp0
         bne     L7924
         lda     DP_BAS_TMP6
         bne     @L797C
         lda     #$30
         jmp     L7AF7

@L797C:  jmp     L79F8

L797F:   jsr     L7DE1
         bra     L7993

L7984:   cmp     #$84
         bcc     L7997
         bne     @L7990
         lda     DP_FPA_mant
         cmp     #$a0
         bcc     L7997
@L7990:  jsr     fpFPADiv10
L7993:   inc     DP_FP_TMP+5
         bra     L792D

L7997:   lda     DP_FPA_mant+4
         sta     $40
         jsr     fpCopyFPAtoPrintPTR
         lda     $70
         sta     DP_BAS_TMP6+1
         ldx     DP_BAS_TMP6
         cpx     #$02
         bne     @L79B8
         adc     DP_FP_TMP+5
         bmi     L79FC
         sta     DP_BAS_TMP6+1
         cmp     #$0b
         bcc     @L79B8
         lda     #$0a
         sta     DP_BAS_TMP6+1
         stz     DP_BAS_TMP6
@L79B8:  jsr     L7EC0
         lda     #$a0
         sta     DP_FPA_mant
         lda     #$83
         sta     DP_FPA_exp+1
         ldx     DP_BAS_TMP6+1
         beq     @L79CD
@L79C7:  jsr     fpFPADiv10
         dex
         bne     @L79C7
@L79CD:  jsr     PrintPTRtoPTR1
         jsr     fpMoveRealAtPTR1toFPB
         lda     $40
         sta     DP_FPB_mant+4
         jsr     L5307
@L79DA:  lda     DP_FPA_exp+1
         cmp     #$84
         bcs     @L79EE
         ror     DP_FPA_mant
         ror     DP_FPA_mant+1
         ror     DP_FPA_mant+2
         ror     DP_FPA_mant+3
         ror     DP_FPA_mant+4
         inc     DP_FPA_exp+1
         bne     @L79DA
@L79EE:  lda     DP_FPA_mant
         cmp     #$a0
         bcs     L797F
         lda     DP_BAS_TMP6+1
         bne     L7A06
L79F8:   cmp     #$01
         beq     L7A3D
L79FC:   jsr     fpzero_FPA
         stz     DP_FP_TMP+5
         lda     $70
         inc     A
         sta     DP_BAS_TMP6+1
L7A06:   lda     #$01
         cmp     DP_BAS_TMP6
         beq     L7A3D
         ldy     DP_FP_TMP+5
         bmi     @L7A1A
         cpy     DP_BAS_TMP6+1
         bcs     L7A3D
         stz     DP_FP_TMP+5
         iny
         tya
         bne     L7A3D
@L7A1A:  lda     DP_BAS_TMP6
         cmp     #$02
         beq     @L7A26
         lda     #$01
         cpy     #$ff
         bne     L7A3D
@L7A26:  lda     #$30
         jsr     L7AF7
         lda     #$2e
         jsr     L7AF7
         lda     #$30
@L7A32:  inc     DP_FP_TMP+5
         beq     @L7A3B
         jsr     L7AF7
         bra     @L7A32

@L7A3B:  lda     #$80
L7A3D:   sta     $70
@L7A3F:  jsr     @L7A93
         dec     $70
         bne     @L7A4B
         lda     #$2e
         jsr     L7AF7
@L7A4B:  dec     DP_BAS_TMP6+1
         bne     @L7A3F
         ldy     DP_BAS_TMP6
         dey
         beq     @L7A6B
         dey
         beq     @L7A67
         ldy     DP_BAS_STRLEN
@L7A59:  dey
         lda     [DP_BAS_STRWKSP_L],y
         cmp     #$30
         beq     @L7A59
         cmp     #$2e
         beq     @L7A65
         iny
@L7A65:  sty     DP_BAS_STRLEN
@L7A67:  lda     DP_FP_TMP+5
         beq     @L7A92
@L7A6B:  lda     #$45
         jsr     L7AF7
         lda     DP_FP_TMP+5
         bpl     @L7A7E
         lda     #$2d
         jsr     L7AF7
         sec
         lda     #$00
         sbc     DP_FP_TMP+5
@L7A7E:  jsr     L7AE3
         lda     DP_BAS_TMP6
         beq     @L7A92
         lda     #$20
         ldy     DP_FP_TMP+5
         bmi     @L7A8E
         jsr     L7AF7
@L7A8E:  cpx     #$00
         beq     L7AF7
@L7A92:  rts

@L7A93:  lda     DP_FPA_mant
         lsr     A
         lsr     A
         lsr     A
         lsr     A
         jsr     L7AF5
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

L7AE3:   ldx     #$ff
         sec
@L7AE6:  inx
         sbc     #$0a
         bcs     @L7AE6
         adc     #$0a
         pha
         txa
         beq     @L7AF4
         jsr     L7AF5
@L7AF4:  pla
L7AF5:   ora     #$30
L7AF7:   phx
         ldx     DP_BAS_STRLEN
         jsr     storeStringX
         plx
         inc     DP_BAS_STRLEN
         rts

parseDecimal_NaN:
         jsr     fpSetFPASgnExp0
         clc                ;CLC=no number, return REAL
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
         stz     DP_FP_TMP+4 ;Clear Decimal point found flag
         stz     DP_FP_TMP+5 ;clear 10's exponent
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
         sbc     #'/'       ;carry is clear so this is effectively subtracting '0'
         bcc     @parseDecSk1 ;skip forward to proper decode loop
         sta     DP_FPA_sgn ;store temporarily in sign
; multiply first digit by 10 and add the one in SGN
         lda     DP_FPA_mant+4
         asl     A
         asl     A
         adc     DP_FPA_mant+4
         asl     A
         adc     DP_FPA_sgn
         sta     DP_FPA_mant+4
; main number parse loop
@parseDecLp1:
         iny
@parseDecSk1:
         lda     [DP_BAS_TXTPTR],y
         cmp     #'.'
         bne     @parseDec_skNotDp
; had a DP check if we've already had one and exit if so
@parseDecGotDp:
         lda     DP_FP_TMP+4
         bne     @parseDecSkDone
         inc     DP_FP_TMP+4
         bra     @parseDecLp1

@parseDec_skNotDp:
         cmp     #'E'
         beq     @parseDec_scanExponent
         cmp     #':'
         bcs     @parseDecSkDone
         sbc     #'/'       ;carry is clear so this is effectively subtracting '0'
         bcc     @parseDecSkDone
         ldx     DP_FPA_mant
         cpx     #$18       ;check MS byte of mantissa
         bcc     @parseDec_AddToMant ;mantissa not "full" skip forward to add
         ldx     DP_FP_TMP+4
         bne     @parseDecLp1 ;we've already parsed a decimal point ignore rest
         inc     DP_FP_TMP+5 ;we've not had DP increase 10's exponent
         bra     @parseDecLp1

@parseDec_AddToMant:
         ldx     DP_FP_TMP+4
         beq     @sknodp2
         dec     DP_FP_TMP+5 ;decrement 10's exponent if we've had a dp
@sknodp2:
         jsr     parseDecMantMul10
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
         beq     @parseDec_skReturnAsINT ;Can we return this as an integer?
         jsr     fpFPACkMant0SetSgnExp0 ;Check for 0
         beq     @parseDec_Exit ;if it was 0 exit
@parseDec_Normalize:
         lda     #$a8       ;default exponent
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
         jsr     fpFPADiv10
         inc     DP_FP_TMP+5
         bne     @parseDec_ExpDiv10lp
@parseDec_RoundAndExit:
         jsr     fpRountMantFPA_bra
@parseDec_Exit:
         sec
         lda     #RETV_REAL
         rts

; The parsed number had no exponent and no decimal point, return as an integer
@parseDec_skReturnAsINT:
         lda     DP_FPA_mant+1
         sta     DP_BAS_INT_WA+3
         and     #$80
         ora     DP_FPA_mant
         bne     @parseDec_Normalize ;oops, too big for an integer, do a real
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
notplus: cmp     #':'       ;TODO: closer char with arith
         bcs     @retExp0
         sbc     #'/'       ;TODO: closer char with arith
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

@retA:   lda     DP_BAS_FP_TMPEXP
         clc
         rts

@retExp0:
         lda     #$00
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
anRTS_7c31:
         rts

fpCopyFPAtoFPB:
         lda     DP_FPA_sgn
         sta     DP_FPB_sgn
         lda     DP_FPA_exp+1
         sta     DP_FPB_exp
         lda     DP_FPA_mant
         sta     DP_FPB_mant
         lda     DP_FPA_mant+1
         sta     DP_FPB_mant+1
         lda     DP_FPA_mant+2
         sta     DP_FPB_mant+2
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
         ror     DP_FPB_mant+2
         ror     DP_FPB_mant+3
         ror     DP_FPB_mant+4
         rts

fpMulBy10:
         clc
         lda     DP_FPA_exp+1
         adc     #$03
         sta     DP_FPA_exp+1
         bcc     @sk1
         inc     DP_FPA_exp
@sk1:    jsr     fpCopyFPAtoFPBAndShiftRight
         jsr     fpShiftFPBMantRight
fpAddAtoBstoreinA_sameExp:
         lda     DP_FPA_mant+4
         adc     DP_FPB_mant+4
         sta     DP_FPA_mant+4
         lda     DP_FPA_mant+3
         adc     DP_FPB_mant+3
         sta     DP_FPA_mant+3
         lda     DP_FPA_mant+2
         adc     DP_FPB_mant+2
         sta     DP_FPA_mant+2
         lda     DP_FPA_mant+1
         adc     DP_FPB_mant+1
         sta     DP_FPA_mant+1
         lda     DP_FPA_mant
         adc     DP_FPB_mant
         sta     DP_FPA_mant
         bcc     anRTS_7c9e
fpRORMantAIncExp:
         ror     DP_FPA_mant ;TODO - optimize 16bits
         ror     DP_FPA_mant+1
         ror     DP_FPA_mant+2
         ror     DP_FPA_mant+3
         ror     DP_FPA_mant+4
         inc     DP_FPA_exp+1
         bne     anRTS_7c9e
         inc     DP_FPA_exp
anRTS_7c9e:
         rts

fpFPADiv10:
         sec
         lda     DP_FPA_exp+1
         sbc     #$04
         sta     DP_FPA_exp+1
         bcs     @sk10
         dec     DP_FPA_exp
@sk10:   jsr     fpCopyFPAtoFPBAndShiftRight
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
         sta     DP_FPB_mant+2
         lda     DP_FPA_mant+2
         sta     DP_FPB_mant+3
         lda     DP_FPA_mant+3
         sta     DP_FPB_mant+4
         lda     DP_FPA_mant+4
         rol     A
         jsr     fpAddAtoBstoreinA_sameExp
         stz     DP_FPB_mant+1
         lda     DP_FPA_mant
         sta     DP_FPB_mant+2
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
         beq     fpRORMantAIncExp
anRtsL7D06:
         rts

fpMoveRealAtPTR1toFPB:
         stz     DP_FPB_mant+4 ;TODO - optimize this
         ldy     #$04
         lda     [DP_BAS_FP_PTR1],y
         sta     DP_FPB_mant+3
         dey
         lda     [DP_BAS_FP_PTR1],y
         sta     DP_FPB_mant+2
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
         ora     DP_FPB_mant+2
         ora     DP_FPB_mant+3
         beq     @sk2
@sk1:    tya
         ora     #$80
@sk2:    sta     DP_FPB_mant
         rts

fpCopyFPAtoPrintPTR:
         lda     #$00
         clc
         adc     DP_BAS_PRINT_FP_PTR
         sta     DP_BAS_FP_PTR1
         lda     #$00
         adc     DP_BAS_PRINT_FP_PTR+1
         sta     DP_BAS_FP_PTR1+1
         lda     #$00
         adc     DP_BAS_PRINT_FP_PTR+2
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
         adc     DP_BAS_PRINT_FP_PTR
         sta     DP_BAS_FP_PTR1
         lda     #$00
         adc     DP_BAS_PRINT_FP_PTR+1
         sta     DP_BAS_FP_PTR1+1
         lda     #$00
         adc     DP_BAS_PRINT_FP_PTR+2
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
@sk1:    tya
         ora     #$80
@sk2:    sta     DP_FPA_mant
         rts

fpSetFPBto0:
         stz     DP_FPB_sgn
         stz     DP_FPB_exp
         stz     DP_FPB_mant
         stz     DP_FPB_mant+1
         stz     DP_FPB_mant+2
         stz     DP_FPB_mant+3
         stz     DP_FPB_mant+4
         rts

fpConstAtoPTR1:
         sta     DP_BAS_FP_PTR1
         lda     #>fpConstPiDiv2 ;TODO: check page sensitive!
         sta     DP_BAS_FP_PTR1+1
         phb                ;Should this be PHK?
         pla
         sta     DP_BAS_FP_PTR1+2
         rts

PrintPTRtoPTR1:
         lda     #$00       ;TODO: optimize this and other superflouous adds away
         clc
         adc     DP_BAS_PRINT_FP_PTR
         sta     DP_BAS_FP_PTR1
         lda     #$00
         adc     DP_BAS_PRINT_FP_PTR+1
         sta     DP_BAS_FP_PTR1+1
         lda     #$00
         adc     DP_BAS_PRINT_FP_PTR+2
         sta     DP_BAS_FP_PTR1+2
         rts

exec_TAN:
         jsr     eval_real
         phy
         phx
         ldx     #ARITH_FN_TAN
         brl     call_ARITH

L7DE1:   lda     #$80
         sta     DP_FPA_mant
         inc     A
         sta     DP_FPA_exp+1
         jmp     L7EC0

L7DEB:   jmp     L50FB

L7DEE:   .byte   $02
         .byte   $08
         .byte   $08
         .byte   $08

L7DF2:   lda     DP_FPA_mant
         beq     L7DEB
         jsr     fpMoveRealAtPTR1toFPB
         bne     @L7DFE
         jmp     fpzero_FPA

@L7DFE:  lda     DP_FPB_sgn
         eor     DP_FPA_sgn
         sta     DP_FPA_sgn
         sec
         lda     DP_FPB_exp
         adc     #$81
         rol     DP_FPA_exp
         sbc     DP_FPA_exp+1
         bcs     @L7E11
         dec     DP_FPA_exp
@L7E11:  sta     DP_FPA_exp+1
         ldy     #$04
         sty     DP_FPB_exp
         lda     DP_FPB_mant
         ldx     #$08
         bra     @L7E26

@L7E1D:  stx     DP_FP_TMP,y
         ldx     L7DEE,y
         sty     DP_FPB_exp
@L7E24:  bcs     @L7E3C
@L7E26:  cmp     DP_FPA_mant
         bne     @L7E3A
         ldy     DP_FPB_mant+1
         cpy     DP_FPA_mant+1
         bne     @L7E3A
         ldy     DP_FPB_mant+2
         cpy     DP_FPA_mant+2
         bne     @L7E3A
         ldy     DP_FPB_mant+3
         cpy     DP_FPA_mant+3
@L7E3A:  bcc     @L7E53
@L7E3C:  tay
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
@L7E53:  rol     DP_FPB_sgn
         asl     DP_FPB_mant+3
         rol     DP_FPB_mant+2
         rol     DP_FPB_mant+1
         rol     A
         dex
         bne     @L7E24
         ldx     DP_FPB_sgn
         ldy     DP_FPB_exp
         dey
         bpl     @L7E1D
         ora     DP_FPB_mant+1
         ora     DP_FPB_mant+2
         ora     DP_FPB_mant+3
         beq     @L7E6F
         sec
@L7E6F:  txa
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
         jsr     L51E4
         bra     fpRountMantFPA_bra

L7E8E:   jsr     fpNegateFP_A
L7E91:   bra     @L7E93     ;TODO - optimize away!

@L7E93:  jsr     fpMoveRealAtPTR1toFPB
         beq     rts7ECC
         jsr     L5307
fpRountMantFPA_bra:
         bra     fpRountMantFPA ;TODO - optimize away!

fpRountMantFPA:
         lda     DP_FPA_mant+4
         cmp     #$80
         bcc     L7EB6
         beq     L7EB3
         inc     DP_FPA_mant+3
         bne     L7EB6
         jsr     fpIncFPAMantMSBs
         bra     L7EB6

L7EAE:   jsr     fpFPAeqPTRmulFPA_internal
         bra     fpRountMantFPA_bra

L7EB3:   rol     A
         tsb     DP_FPA_mant+3
L7EB6:   lda     DP_FPA_exp
         beq     L7ECA
         bpl     L7ECD
fpzero_FPA:
         stz     DP_FPA_exp+1
         stz     DP_FPA_mant
L7EC0:   stz     DP_FPA_sgn
         stz     DP_FPA_exp
         stz     DP_FPA_mant+1
         stz     DP_FPA_mant+2
         stz     DP_FPA_mant+3
L7ECA:   stz     DP_FPA_mant+4
rts7ECC: rts

L7ECD:   brk     $14

         .byte   "Too big may be"
         .byte   $00

fpFPAeqPTRmulFPA_internal:
         lda     DP_FPA_mant
         bne     @sk
         jmp     rts7ECC

@sk:     jsr     fpMoveRealAtPTR1toFPB
         bne     @sk2
         jmp     fpzero_FPA

@sk2:    phk
         plb
         clc
         lda     DP_FPA_exp+1
         adc     DP_FPB_exp
         rol     DP_FPA_exp
         sbc     #$7f
         sta     DP_FPA_exp+1
         bcs     @sk3
         dec     DP_FPA_exp
@sk3:    phx
         ldx     #$05
         ldy     #$00
@L7F03:  lda     DP_FPA_exp+1,x
         sta     DP_FPB_mant+4,x
         sty     DP_FPA_exp+1,x
         dex
         bne     @L7F03
         lda     DP_FPA_sgn
         eor     DP_FPB_sgn
         sta     DP_FPA_sgn
         ldy     #$20
@L7F14:  lsr     DP_FPB_mant
         ror     DP_FPB_mant+1
         ror     DP_FPB_mant+2
         ror     DP_FPB_mant+3
         ror     DP_FPB_mant+4
         asl     DP_FP_TMP+2
         rol     DP_FP_TMP+1
         rol     DP_FP_TMP
         rol     DP_BAS_TMP64
         bcc     @L7F2C
         clc
         jsr     dpAddFPBmanttoFPAmant
@L7F2C:  dey
         bne     @L7F14
         plx
         lda     DP_FPA_mant
         bpl     @L7F37
         jmp     rts7ECC

@L7F37:  jmp     L51B2

         rts                ;TODO - DEAD code?

dpAddFPBmanttoFPAmant:
         lda     DP_FPA_mant+4
         adc     DP_FPB_mant+4
         sta     DP_FPA_mant+4
         lda     DP_FPA_mant+3
         adc     DP_FPB_mant+3
         sta     DP_FPA_mant+3
         lda     DP_FPA_mant+2
         adc     DP_FPB_mant+2
         sta     DP_FPA_mant+2
         lda     DP_FPA_mant+1
         adc     DP_FPB_mant+1
         sta     DP_FPA_mant+1
         lda     DP_FPA_mant
         adc     DP_FPB_mant
         sta     DP_FPA_mant
         rts

exec_LN: jsr     eval_real
         phy
         phx
         ldx     #ARITH_FN_LN
         brl     call_ARITH

exec_SQR:
         jsr     eval_real
         phy
         phx
         ldx     #ARITH_FN_SQR
         brl     call_ARITH

L7F6E:   lda     DP_FPA_mant
         bmi     @L7F75
         jsr     fpNormalizeFPA_bra
@L7F75:  jsr     fpRountMantFPA_bra
         lda     #$ff
         rts

fpLoadFpConstA:
         jsr     fpConstAtoPTR1
         jmp     fpCopyPTR1toFPA

exec_ACS:
         jsr     eval_real
         phy
         phx
         ldx     #ARITH_FN_ACS
         brl     call_ARITH

exec_ASN:
         jsr     eval_real
         phy
         phx
         ldx     #ARITH_FN_ASN
         brl     call_ARITH

fpSetFPAPIdiv2:
         lda     #<fpConstPiDiv2
         jmp     fpLoadFpConstA ;TODO: optimize - move to following!

exec_ATN:
         jsr     eval_real
         phy
         phx
         ldx     #ARITH_FN_ATN
         brl     call_ARITH

exec_SIN:
         clc
exec_COS:
         php
         jsr     eval_real
         plp
         phy
         phx
         bcc     @sin

         ldx     #ARITH_FN_COS
         bra     call_ARITH

@sin:    ldx     #ARITH_FN_SIN
call_ARITH:
         phb
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

exec_RAD:
         jsr     eval_real
         phy
         phx
         ldx     #ARITH_FN_RAD
         brl     call_ARITH

exec_LOG:
         jsr     eval_real
         phy
         phx
         ldx     #ARITH_FN_LOG
         brl     call_ARITH

exec_DEG:
         jsr     eval_real
         phy
         phx
         ldx     #ARITH_FN_DEG
         brl     call_ARITH

exec_EXP:
         jsr     eval_real
         phy
         phx
         ldx     #ARITH_FN_EXP
         brl     call_ARITH

L7FFA:   jsr     setREPORT_BHA_Cy
         bcc     @anRTS
         cop     COP_61_OPERC
         ldx     #$00       ;TODO: work out this stuff - looks a bit odd not really a brk?
         brk     $02

         .byte   $02
         .byte   $02
         .byte   $03
         .byte   $00
         .byte   $c8
         .byte   "Command failed"
         .byte   $00

@anRTS:  rts

setREPORT_BHA_Cy:
         php
         rep     #$30
         .a16
         .i16
         phb
         pha
         stz     $00
         stz     $02
         rol     $00
         stx     $04
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
         .a8
         .i8
         rts

doINKEY_int:
         jsr     eval_ret_INT
@L803C:  lda     #$81
         ldx     DP_BAS_INT_WA
         ldy     DP_BAS_INT_WA+1
         jsr     call_OSBYTE
         bcc     @L804F
         bit     DP_BAS_INT_WA+1
         bmi     @L804F
         cpy     #$00
         beq     @L803C
@L804F:  rts

L8050:   jsr     L52BD
L8053:   stz     DP_FPA_sgn
         stz     DP_FPA_exp
         stz     DP_FPA_mant+4
         lda     #$80
         sta     DP_FPA_exp+1
         ldy     #$00
         ldx     #$03
@L8061:  phx
         tyx
         eor     DP_BAS_RAND,x
         plx
         sta     DP_FPA_mant,x
         iny
         dex
         bpl     @L8061
         jmp     L7F6E

L806F:   inc     DP_BAS_TXTPTR_OFF
         jsr     L6C77
         lda     DP_BAS_INT_WA+3
         bmi     @L809D
         ora     DP_BAS_INT_WA+2
         ora     DP_BAS_INT_WA+1
         bne     @L8086
         lda     DP_BAS_INT_WA
         beq     L8053
         cmp     #$01
         beq     L8050
@L8086:  jsr     L5117
         jsr     stack_REAL
         jsr     L8050
         jsr     popFPFromStackToPTR1
         jsr     fpFPAeqPTRmulFPA_internal
         jsr     eval_real2INT
         jsr     INT_inc_WA
         bra     L80C4

@L809D:  ldx     #$22
         jsr     L9A49
         lda     #$40
         sta     DP_BAS_RAND+4
         rts

exec_RND:
         ldy     DP_BAS_TXTPTR_OFF
         lda     [DP_BAS_TXTPTR],y
         cmp     #'('
         beq     L806F
         jsr     L52BD
         ldx     #$22
L80B4:   lda     $00,x
         sta     DP_BAS_INT_WA
         lda     $01,x
         sta     DP_BAS_INT_WA+1
         lda     $02,x
         sta     DP_BAS_INT_WA+2
         lda     $03,x
         sta     DP_BAS_INT_WA+3
L80C4:   lda     #$40
         rts

exec_NOT:
         jsr     eval_ret_INT
         ldx     #$03
@L80CC:  lda     DP_BAS_INT_WA,x
         eor     #$ff
         sta     DP_BAS_INT_WA,x
         dex
         bpl     @L80CC
         bra     L80C4

exec_POS:
         jsr     exec_VPOS
         stx     DP_BAS_INT_WA
         rts

L80DD:   lda     #'L'
         jsr     @L80E8
         cmp     #$01
         bcc     @L80F4
         lda     #'I'
@L80E8:  ldy     DP_BAS_TXTPTR_OFF
         inc     DP_BAS_TXTPTR_OFF
         eor     [DP_BAS_TXTPTR],y
         beq     @L80F4
         dec     DP_BAS_TXTPTR_OFF
         eor     #$00
@L80F4:  rts

exec_USR:
         jsr     L80DD
         php
         jsr     eval_ret_INT
         plp
         bcc     @L810B
         bne     @L810B
         phd
         phk
         pea     @L8110
         php
         sei
         jmp     L6804

@L810B:  phd
         phk
         jsr     L6804
@L8110:  jsr     L67C2
         pld
         lda     $74
         sta     DP_BAS_INT_WA
         lda     $78
         sta     DP_BAS_INT_WA+1
         lda     $7c
         sta     DP_BAS_INT_WA+2
         lda     $88
         sta     DP_BAS_INT_WA+3
         bra     L80C4

exec_VPOS:
         lda     #$86
         jsr     call_OSBYTE
         tya
retA_8bit_INT_presX:
         phx
         jsr     retA_8bit_INT
         plx
         rts

exec_EXT:
         jsr     parse_handleY_WAeqmin1_HAptWA
         phb
         cop     COP_24_OPCVD
         cop     COP_57_OPRLL
         bra     L8144

exec_PTR:
         jsr     parse_handleY_WAeqmin1_HAptWA
         phb
         cop     COP_24_OPCVD
         cop     COP_54_OPRSP
L8144:   jsr     setREPORT_BHA_Cy
         plb
         sep     #$30
         brl     L80C4

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
exec_BGET:
         jsr     parse_fileHandleHash2
         jsr     callOSBGET
         jsr     retA_8bit_INT_presX
         bcc     @sk
         ror     DP_BAS_INT_WA+3 ;set top bit of INT_WA if error
@sk:     rts

callOSBGET:
         phb
         phx
         rep     #$10
         ldy     DP_BAS_INT_WA
         cop     COP_OSBGT
         jsr     setREPORT_BHA_Cy
         sep     #$10
         plx
         plb
         rts

exec_OPENIN:
         lda     #$40
         bra     L8187

exec_OPENOUT:
         lda     #$80
         bra     L8187

exec_OPENUP:
         lda     #$c0
L8187:   phb
         pha
         jsr     evalLevel1
         bne     jmp_brk06_type_mismatch11
         jsr     L9AF9
         lda     DP_BAS_STRWKSP_L+2
         pha
         plb
         lda     DP_BAS_STRWKSP_L+1
         xba
         lda     DP_BAS_STRWKSP_L
         ply
         rep     #$30
         phb
         cop     $44
         .a16
         .i16
         jsr     setREPORT_BHA_Cy
         plb
         bcc     @L81A9
         ldy     #$0000
@L81A9:  sty     DP_BAS_INT_WA
         stz     DP_BAS_INT_WA+2
         sep     #$30
         .a8
         .i8
         plb
         lda     #$40
         rts

jmp_brk06_type_mismatch11:
         jmp     brk06_type_mismatch

exec_PI: jsr     fpSetFPAPIdiv2
         inc     DP_FPA_exp+1
         rts

exec_EVAL:
         jsr     evalLevel1
         bne     jmp_brk06_type_mismatch10
         ldy     DP_BAS_STRLEN
         inc     DP_BAS_STRLEN
         lda     #$0d
         sta     [DP_BAS_STRWKSP_L],y
         jsr     storeStringOnStack
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
         bne     @L81E6
         inx
@L81E6:  stx     DP_BAS_TXTPTR+1
         stx     DP_BAS_TMP6+1
         bne     @L81ED
         iny
@L81ED:  sty     DP_BAS_TXTPTR+2
         sty     DP_BAS_TMP6+2
         jsr     L60D0
         stz     DP_BAS_TXTPTR_OFF
         jsr     L7537
         jsr     L9937
L81FC:   pla
         sta     DP_BAS_TXTPTR_OFF
         pla
         sta     DP_BAS_TXTPTR+2
         pla
         sta     DP_BAS_TXTPTR+1
         pla
         sta     DP_BAS_TXTPTR
         lda     $40
         rts

jmp_brk06_type_mismatch10:
         jmp     brk06_type_mismatch

exec_VAL:
         jsr     evalLevel1
         bne     jmp_brk06_type_mismatch10
L8213:   ldx     DP_BAS_STRLEN
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
         cmp     #$2d
         beq     @L8249
         cmp     #$2b
         bne     @L8242
         jsr     parse_skip_spaces
@L8242:  dec     DP_BAS_TXTPTR_OFF
         jsr     parseDecimalLiteral
         bra     @L8256

@L8249:  jsr     parse_skip_spaces
         dec     DP_BAS_TXTPTR_OFF
         jsr     parseDecimalLiteral
         bcc     @L8256
         jsr     evalLevel1CheckNotStringNegate
@L8256:  sta     $40
         bra     L81FC

exec_INT:
         jsr     evalLevel1
         beq     jmp_brk06_type_mismatch9
         bpl     @L8282
         lda     DP_FPA_sgn
         php
         jsr     L5241
         plp
         bpl     @L827D
         lda     DP_FPB_mant
         ora     DP_FPB_mant+1
         ora     DP_FPB_mant+2
         ora     DP_FPB_mant+3
         beq     @L827D
         jsr     L5294
         jsr     L52AC
         jsr     L5294
@L827D:  jsr     L6CB0
         lda     #$40
@L8282:  rts

exec_ASC:
         jsr     evalLevel1
         bne     jmp_brk06_type_mismatch9
         lda     DP_BAS_STRLEN
         beq     exec_TRUE
         lda     [DP_BAS_STRWKSP_L]
jmp_retA_8bit_INT:
         jmp     retA_8bit_INT

exec_INKEY:
         jsr     doINKEY_int
         tya
         bne     exec_TRUE
         txa
         jmp     retAY_16bit_INT

jmp_brk06_type_mismatch9:
         jmp     brk06_type_mismatch

exec_EOF:
         jsr     parse_fileHandleHash2
         rep     #$10
         ldy     DP_BAS_INT_WA
         phb
         cop     $46        ;OPEND
         jsr     setREPORT_BHA_Cy
         plb
         sep     #$10
         bcs     exec_TRUE
         ldx     #$00
         lsr     A
         bcc     retxxxx_INT
exec_TRUE:
         ldx     #$ff
retxxxx_INT:
         stx     DP_BAS_INT_WA
         stx     DP_BAS_INT_WA+1
         stx     DP_BAS_INT_WA+2
         stx     DP_BAS_INT_WA+3
reta40:  lda     #$40
         rts

exec_FALSE:
         ldx     #$00
         bra     retxxxx_INT

L82C6:   jsr     fpFPACkMant0SetSgnExp0
         beq     exec_FALSE
         bpl     ret1_INT
         bra     exec_TRUE

exec_SGN:
         jsr     evalLevel1
         beq     jmp_brk06_type_mismatch9
         bmi     L82C6
         lda     DP_BAS_INT_WA+3
         ora     DP_BAS_INT_WA+2
         ora     DP_BAS_INT_WA+1
         ora     DP_BAS_INT_WA
         beq     reta40     ;return 0
         lda     DP_BAS_INT_WA+3
         bmi     exec_TRUE  ;minus INT return -1
ret1_INT:
         lda     #$01
bra_jmp_retA_8bit_INT:
         bra     jmp_retA_8bit_INT ;pos INT return 1

exec_POINT:
         jsr     L6C7F
         jsr     stack_INTWA
         jsr     parse_skip_spaces_CMPcommaBRK
         jsr     L6C77
         lda     DP_BAS_INT_WA
         pha
         ldx     DP_BAS_INT_WA+1
         jsr     L993C
         stx     DP_BAS_INT_WA+3
         pla
         sta     DP_BAS_INT_WA+2
         ldy     #$00
         ldx     #$48
         lda     #$09
         jsr     cop_OSWORD
         lda     DP_FPA_sgn
         bmi     exec_TRUE
         bra     bra_jmp_retA_8bit_INT

exec_INSTR:
         jsr     L7537
         bne     jmp_brk06_type_mismatch9
         cpx     #','
         bne     @L8334
         inc     DP_BAS_TXTPTR_OFF
         jsr     storeStringOnStack
         jsr     L7537
         beq     @L8326
         brl     jmp_brk06_type_mismatch9

@L8326:  lda     #$01
         sta     DP_BAS_INT_WA
         inc     DP_BAS_TXTPTR_OFF
         cpx     #$29
         beq     @L833D
         cpx     #$2c
         beq     @L8337
@L8334:  jmp     brk_05_missing_comma

@L8337:  jsr     L6C74
         jsr     L9928
@L833D:  ldx     DP_BAS_INT_WA
         bne     @L8343
         ldx     #$01
@L8343:  stx     DP_BAS_INT_WA
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
         bcc     @L8381
         sbc     DP_BAS_STRLEN
         bcc     @L8381
         adc     #$00
         sta     DP_BAS_INT_WA+1
         jsr     L9937
@L836C:  ldy     #$00
         ldx     DP_BAS_STRLEN
         beq     @L837C
@L8372:  lda     [DP_BAS_TMP6],y
         cmp     [DP_BAS_STRWKSP_L],y
         bne     @L8388
         iny
         dex
         bne     @L8372
@L837C:  lda     DP_BAS_INT_WA
@L837E:  jmp     retA_8bit_INT

@L8381:  jsr     L9937
@L8384:  lda     #$00
         bra     @L837E

@L8388:  inc     DP_BAS_INT_WA
         dec     DP_BAS_INT_WA+1
         beq     @L8384
         inc     DP_BAS_TMP6
         bne     @L836C
         inc     DP_BAS_TMP6+1
         bne     @L836C
         inc     DP_BAS_TMP6+2
         bra     @L836C

jmp_brk06_type_mismatch8:
         jmp     brk06_type_mismatch

exec_ABS:
         jsr     evalLevel1
         beq     jmp_brk06_type_mismatch8
         bmi     L83AA
L83A4:   bit     DP_BAS_INT_WA+3
         bmi     L8400
         bra     L8417

L83AA:   stz     DP_FPA_sgn
         rts

L83AD:   jsr     L7E8E
fpNegateFP_A:
         lda     DP_FPA_mant
         beq     @L83BA
         lda     DP_FPA_sgn
         eor     #$80
         sta     DP_FPA_sgn
@L83BA:  lda     #$ff
         rts

parseCheckDollarNoSuchVar:
         iny
         lda     [DP_BAS_TXTPTR],y
         cmp     #'$'
         beq     fnREPORTdollar
         brl     parseCheckOPTNoSuchVar

fnREPORTdollar:
         iny
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
         lda     #$00
         rts

evalLevel1UnaryMinus:
         jsr     evalSkipSpacesFnOrOthers
evalLevel1CheckNotStringNegate:
         beq     jmp_brk06_type_mismatch8
         bmi     fpNegateFP_A
L8400:   sec
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
L8417:   lda     #$40
         rts

;*******************************************************************************
;* Strings in DATA statements can be surrounded by quotes or terminated by     *
;* comma                                                                       *
;*******************************************************************************
parseDataStringToStrWksp:
         jsr     parse_skip_spaces
         cmp     #'"'
         beq     parseStringToStrWksp
         ldx     #$00
@lp:     lda     [DP_BAS_TXTPTR],y
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
         lda     #$00
         rts

storeStringX:
         phy
         txy
         sta     [DP_BAS_STRWKSP_L],y
         ply
         rts

; Parse a string to String Workspace expect ending ", double "'s are placed in
; the string as a single quote
parseStringToStrWksp:
         ldx     #$00
@inylp:  iny
@lp:     lda     [DP_BAS_TXTPTR],y
         cmp     #$0d
         beq     @missingQ
         jsr     storeStringX
         iny
         inx
         cmp     #'"'
         bne     @lp
         lda     [DP_BAS_TXTPTR],y
         cmp     #'"'       ;skip double "
         beq     @inylp
         bne     decXstoreAsStringLen

@missingQ:
         jmp     brk_09_missing_quote

evalLevel1:
         ldy     DP_BAS_TXTPTR_OFF
         inc     DP_BAS_TXTPTR_OFF
         lda     [DP_BAS_TXTPTR],y
         cmp     #' '
         beq     evalLevel1
         cmp     #'-'
         beq     evalLevel1UnaryMinus
         cmp     #'"'
         beq     parseStringToStrWksp
         cmp     #tknREPORT ;REPORT
         bne     @notREPORT
         brl     parseCheckDollarNoSuchVar

@notREPORT:
         cmp     #'+'
         bne     __skNotPlus
evalSkipSpacesFnOrOthers:
         jsr     parse_skip_spaces
__skNotPlus:
         cmp     #tknOPENIN
         bcc     @notbuiltinFN ;< tknOPENIN
         cmp     #tknEOF+1
         bcc     @dispatchFN ;OPENIN..EOF are all tokens for builtin FN
         brl     brk1a_no_such_var

@dispatchFN:
         jmp     evalDispatchFN

@notbuiltinFN:
         cmp     #'?'
         bcs     @L84A2
         cmp     #'.'
         bcs     @L850B
         cmp     #'&'
         bne     @L849B
         brl     L8558

@L849B:  cmp     #$28
         bne     @L84A2
         brl     L854D

@L84A2:  cmp     #'@'
         bne     @L84F4
         iny
         lda     [DP_BAS_TXTPTR],y
         cmp     #'%'
         beq     @L84F4
; @variable lookup using the NAME module
         iny
         lda     [DP_BAS_TXTPTR],y
         cmp     #'%'
         beq     @L84F4
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
         cop     _OPCMD
         .byte   "NAME"
         .byte   $00

         bcs     @L84F4
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
         lda     #$40
         rts

@L84F4:  phk
         plb
         dec     DP_BAS_TXTPTR_OFF
         ldy     DP_BAS_TXTPTR_OFF
         lda     [DP_BAS_TXTPTR],y
         jsr     L6F90
         bne     @L8508
         bit     DP_BAS_SET_TO_Z_AT_EOS
         bpl     parseCheckOPTNoSuchVar
         jsr     L87A6
@L8508:  jmp     L8B74

@L850B:  jsr     parseDecimalLiteral
         bcc     brk1a_no_such_var
         rts

parseCheckOPTNoSuchVar:
         lda     DP_BAS_OPT
         and     #$02
         bne     brk1a_no_such_var
         bcs     brk1a_no_such_var
         stx     DP_BAS_TXTPTR_OFF
L851B:   ldy     #$42
         lda     [DP_BAS_ASSWKSP],y
         tax
         dey
         lda     [DP_BAS_ASSWKSP],y
         xba
         dey
         lda     [DP_BAS_ASSWKSP],y
         xba
         tay
         xba
         brl     retAYX_24bit_INT

brk1a_no_such_var:
         brk     $1a

         .byte   "No such variable"

L853F:   brk     $1b

         .byte   $8d
         .byte   $29

L8543:   brk     $1c

         .byte   "Bad Hex"
         .byte   $00

L854D:   jsr     L7537
         inc     DP_BAS_TXTPTR_OFF
         cpx     #$29
         bne     L853F
         tay
         rts

L8558:   jsr     exec_FALSE
         iny
@L855C:  lda     [DP_BAS_TXTPTR],y
         cmp     #$30
         bcc     @L8585
         cmp     #$3a
         bcc     @L8570
         sbc     #$37
         cmp     #$0a
         bcc     @L8585
         cmp     #$10
         bcs     @L8585
@L8570:  asl     A
         asl     A
         asl     A
         asl     A
         ldx     #$03
@L8576:  asl     A
         rol     DP_BAS_INT_WA
         rol     DP_BAS_INT_WA+1
         rol     DP_BAS_INT_WA+2
         rol     DP_BAS_INT_WA+3
         dex
         bpl     @L8576
         iny
         bne     @L855C
@L8585:  txa
         bpl     L8543
         sty     DP_BAS_TXTPTR_OFF
         lda     #$40
         rts

exec_ADVAL:
         jsr     eval_ret_INT
         jsr     L877E
         lda     #$40
         rts

exec_TO: iny
         lda     [DP_BAS_TXTPTR],y
         cmp     #'P'
         bne     brk1a_no_such_var
         inc     DP_BAS_TXTPTR_OFF
         lda     DP_BAS_TOP
         ldy     DP_BAS_TOP+1
         ldx     DP_BAS_TOP+2
         bra     retAYX_24bit_INT

exec_PAGE:
         ldy     DP_BAS_PAGE+1
         lda     DP_BAS_PAGE
         ldx     DP_BAS_PAGE+2
         bra     retAYX_24bit_INT

jmp_brk06_type_mismatch7:
         jmp     brk06_type_mismatch

exec_LEN:
         jsr     evalLevel1
         bne     jmp_brk06_type_mismatch7
         lda     DP_BAS_STRLEN
retA_8bit_INT:
         ldy     #$00
retAY_16bit_INT:
         ldx     #$00
retAYX_24bit_INT:
         sta     DP_BAS_INT_WA
         sty     DP_BAS_INT_WA+1
         stx     DP_BAS_INT_WA+2
         stz     DP_BAS_INT_WA+3
         lda     #$40
         rts

exec_COUNT:
         lda     DP_BAS_COUNT
         bra     retA_8bit_INT

exec_LOMEM:
         lda     DP_BAS_LOMEM
         ldy     DP_BAS_LOMEM+1
         ldx     DP_BAS_LOMEM+2
         bra     retAYX_24bit_INT

exec_HIMEM:
         lda     DP_BAS_HIMEM
         ldy     DP_BAS_HIMEM+1
         ldx     DP_BAS_HIMEM+2
         bra     retAYX_24bit_INT

exec_ERL:
         ldy     DP_BAS_ERL+1
         lda     DP_BAS_ERL
         bra     retAY_16bit_INT

exec_ERR:
         phx
         ldx     #$14
         jsl     _ST
         plx
         bra     retA_8bit_INT

exec_GET:
         jsr     call_OSRDCH
         bra     retA_8bit_INT

exec_TIME:
         iny
         lda     [DP_BAS_TXTPTR],y
         cmp     #'$'
         beq     exec_TIMEdollar
         ldx     #DP_BAS_INT_WA
         ldy     #>DP_BAS_INT_WA
         lda     #OSWORD_1_READTIME
         jsr     cop_OSWORD
         lda     #RETV_INT
         rts

L8604:   pei     (DP_BAS_STRWKSP_L+1)
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
         jsr     L6C77
         lda     DP_BAS_INT_WA
         ora     #$80
         sta     $01,S
@nobrack:
         ldx     #_CKREAD
         ply
         jsr     L8604
; The CLOCK driver will be called here for _CKREAD Y will either be the supplied
; value in brackets EORed with $80 or $FF
         cop     _OPCMD
         .byte   "CLOCK"
         .byte   $00

         ldy     #$00
@strlenlp:
         lda     [DP_BAS_STRWKSP_L],y
         cmp     #$0d
         beq     @strlensk
         iny
         bne     @strlenlp
@strlensk:
         phk
         plb
         tya
         bra     ret_str_lenA

exec_GETDollar:
         sec
         jsr     parse_fileHandleHash
         bcs     doOSRDCH_ret_as_1ch_str
         ldx     #$00
         ldy     #$00
@L864B:  phy
         jsr     callOSBGET
         ply
         bcs     @L8663
         cpx     #$00
         bne     @L865A
         cmp     #$20
         bcc     @L8663
@L865A:  sta     [DP_BAS_STRWKSP_L],y
         iny
         txa
         beq     @L864B
         dex
         bne     @L864B
@L8663:  tya
         bra     ret_str_lenA

doOSRDCH_ret_as_1ch_str:
         jsr     call_OSRDCH
retA_as_1ch_str:
         sta     [DP_BAS_STRWKSP_L]
         lda     #$01
         bra     ret_str_lenA

exec_LEFT:
         clc
exec_RIGHT:
         php
         jsr     L7537
         bne     jmp_brk06_type_mismatch6
         cpx     #','
         bne     jmp_brk_05_missing_comma
         inc     DP_BAS_TXTPTR_OFF
         jsr     L6C74
         jsr     L9928
         plp
         bcs     retSTR+3
         lda     DP_BAS_INT_WA
         cmp     DP_BAS_STRLEN
         bcs     retSTR
ret_str_lenA:
         sta     DP_BAS_STRLEN
retSTR:  lda     #$00
         rts

         .byte   $a5
         .byte   $54
         .byte   $e5
         .byte   $48
         .byte   $90
         HiAscii "wpw(%"
         .byte   $48
         .byte   $85
         .byte   $54
         .byte   $f0
         .byte   $f0
         .byte   $a2
         .byte   $00
         .byte   $b7
         .byte   $a7
         .byte   $20
         .byte   $3b
         .byte   $84
         .byte   $e8
         .byte   $c8
         .byte   $c6
         .byte   $48
         .byte   $d0
         .byte   $f5
         .byte   $80
         .byte   $df

exec_INKEYDollar:
         jsr     doINKEY_int
         txa
         bcc     retA_as_1ch_str
L86B4:   lda     #$00
         bra     ret_str_lenA

jmp_brk06_type_mismatch6:
         jmp     brk06_type_mismatch

jmp_brk_05_missing_comma:
         jmp     brk_05_missing_comma

exec_MID:
         jsr     L7537
         bne     jmp_brk06_type_mismatch6
         cpx     #','
         bne     jmp_brk_05_missing_comma
         jsr     storeStringOnStack
         inc     DP_BAS_TXTPTR_OFF
         jsr     L6C7F
         lda     DP_BAS_INT_WA
         pha
         lda     #$ff
         sta     DP_BAS_INT_WA
         inc     DP_BAS_TXTPTR_OFF
         cpx     #$29
         beq     @L86E3
         cpx     #$2c
         bne     jmp_brk_05_missing_comma
         jsr     L6C77
@L86E3:  jsr     L9928
         pla
         tay
         clc
         beq     @L86F1
         sbc     DP_BAS_STRLEN
         bcs     L86B4
         dey
         tya
@L86F1:  sta     DP_BAS_INT_WA+2
         tay
         ldx     #$00
         lda     DP_BAS_STRLEN
         sec
         sbc     DP_BAS_INT_WA+2
         cmp     DP_BAS_INT_WA
         bcs     @L8701
         sta     DP_BAS_INT_WA
@L8701:  lda     DP_BAS_INT_WA
         beq     L86B4
@L8705:  lda     [DP_BAS_STRWKSP_L],y
         jsr     storeStringX
         iny
         inx
         cpx     DP_BAS_INT_WA
         bne     @L8705
         stx     DP_BAS_STRLEN
         bra     L8775

exec_STR:
         jsr     parse_skip_spaces
         ldy     #$ff
         cmp     #'~'
         beq     @L8721
         ldy     #$00
         dec     DP_BAS_TXTPTR_OFF
@L8721:  phy
         jsr     evalLevel1
         beq     jmp_brk06_type_mismatch5
         tay
         pla
         sta     $2b
         phy
         ldy     #$03
         lda     [DP_BAS_ASSWKSP],y
         ply
         tax
         bne     @L873B
         sta     DP_BAS_TMP6
         jsr     L795A
         bra     L8775

@L873B:  jsr     L793A
         bra     L8775

jmp_brk06_type_mismatch5:
         jmp     brk06_type_mismatch

exec_STRING:
         jsr     L6C7F
         jsr     stack_INTWA
         jsr     parse_skip_spaces_CMPcommaBRK
         jsr     L854D
         bne     jmp_brk06_type_mismatch5
         jsr     L993C
         ldx     DP_BAS_STRLEN
         beq     L8775
         lda     DP_BAS_INT_WA
         beq     L8778
         dec     DP_BAS_INT_WA
         beq     L8775
@L8760:  ldy     #$00
@L8762:  lda     [DP_BAS_STRWKSP_L],y
         jsr     storeStringX
         iny
         inx
         beq     L877B
         cpy     DP_BAS_STRLEN
         bcc     @L8762
         dec     DP_BAS_INT_WA
         bne     @L8760
         stx     DP_BAS_STRLEN
L8775:   lda     #$00
         rts

L8778:   sta     DP_BAS_STRLEN
         rts

L877B:   jmp     L760F

L877E:   ldy     #$00
         lda     [DP_BAS_INT_WA],y
         sta     DP_BAS_INT_WA
         stz     DP_BAS_INT_WA+1
         stz     DP_BAS_INT_WA+2
         stz     DP_BAS_INT_WA+3
         rts

L878B:   lda     DP_BAS_TXTPTR2
         sta     DP_BAS_TMP6
         lda     DP_BAS_TXTPTR2+1
         sta     DP_BAS_TMP6+1
         lda     DP_BAS_TXTPTR2+2
         sta     DP_BAS_TMP6+2
         rts

L8798:   clc
         adc     DP_BAS_TMP6
         sta     DP_BAS_TMP6
         bcc     @L87A5
         inc     DP_BAS_TMP6+1
         bne     @L87A5
         inc     DP_BAS_TMP6+2
@L87A5:  rts

L87A6:   jsr     L878B
         ldx     #$01
         ldy     DP_BAS_TXTPTR_OFF
         jsr     varScanNameAtTMP6_Y
         cmp     #$25
         bne     @L87B6
         inx
         iny
@L87B6:  stx     DP_BAS_TMP6+3
         phy
         lda     DP_BAS_TXTPTR_OFF
         jsr     L8798
         lda     DP_BAS_PAGE
         sta     DP_FPB_mant
         lda     DP_BAS_PAGE+1
         sta     DP_FPB_mant+1
         lda     DP_BAS_PAGE+2
         sta     DP_FPB_mant+2
         ldy     #$03
         bra     @L87DF

@L87CE:  ldy     #$03
         clc
         lda     [DP_FPB_mant],y
         adc     DP_FPB_mant
         sta     DP_FPB_mant
         bcc     @L87DF
         inc     DP_FPB_mant+1
         bne     @L87DF
         inc     DP_FPB_mant+2
@L87DF:  tyx
         ldy     #$01
         lda     [DP_FPB_mant],y
         bpl     @L87E9
         brl     parseCheckOPTNoSuchVar

@L87E9:  txy
@L87EA:  iny
         lda     [DP_FPB_mant],y
         cmp     #$20
         beq     @L87EA
         cmp     #$2e
         bne     @L87CE
         iny
         sty     DP_BAS_TXTPTR2_OFF
         iny
         tya
         pha
         clc
         adc     DP_FPB_mant
         ldy     DP_FPB_mant+1
         ldx     DP_FPB_mant+2
         bcc     @L8809
         iny
         bne     @L8808
         inx
@L8808:  clc
@L8809:  sbc     #$00
         sta     DP_FPB_sgn
         tya
         sbc     #$00
         sta     $5d
         txa
         sbc     #$00
         sta     DP_FPB_exp
         plx
         ldy     #$01
@L881A:  inx
         dey
         lda     [DP_FPB_sgn],y
         cmp     [DP_BAS_TMP6],y
         bne     @L87CE
         iny
         iny
         cpy     DP_BAS_TMP6+3
         bne     @L881A
         dey
         cmp     #$25
         beq     @L8848
         lda     [DP_FPB_sgn],y
         cmp     #$25
         beq     @L87CE
         cmp     #$28
         beq     @L87CE
         cmp     #$24
         beq     @L87CE
         cmp     #$21
         beq     @L87CE
         cmp     #$3f
         beq     @L87CE
         jsr     L6031
         bcs     @L87CE
@L8848:  lda     DP_BAS_TXTPTR2
         sta     DP_FPB_sgn
         lda     DP_BAS_TXTPTR2+1
         sta     $5d
         lda     DP_BAS_TXTPTR2+2
         sta     DP_FPB_exp
         lda     DP_FPB_mant
         sta     DP_BAS_TXTPTR2
         lda     DP_FPB_mant+1
         sta     DP_BAS_TXTPTR2+1
         lda     DP_FPB_mant+2
         sta     DP_BAS_TXTPTR2+2
         jsr     var_find
         bcs     @L88A7
         lda     DP_BAS_INT_WA+3
         beq     @L88A7
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
         sta     $40
         jsr     L8D5C
         lda     #$40
         ora     DP_BAS_SET_TO_Z_AT_EOS
         sta     DP_BAS_SET_TO_Z_AT_EOS
         lda     DP_FPB_sgn
         sta     DP_BAS_TXTPTR2
         sta     DP_BAS_TXTPTR
         lda     $5d
         sta     DP_BAS_TXTPTR+1
         sta     DP_BAS_TXTPTR2+1
         lda     DP_FPB_exp
         sta     DP_BAS_TXTPTR2+2
         sta     DP_BAS_TXTPTR+2
         ply
         sty     DP_BAS_TXTPTR_OFF
         sty     DP_BAS_TXTPTR2_OFF
         lda     $40
         sta     DP_BAS_INT_WA+3
         rts

@L88A7:  stz     DP_BAS_SET_TO_Z_AT_EOS
         jmp     brk06_type_mismatch

L88AC:   pla
         sta     DP_BAS_TXTPTR2+2
         pla
         sta     DP_BAS_TXTPTR2+1
         pla
         sta     DP_BAS_TXTPTR2
         ldx     #$1f
         jsr     L620C
         brk     $1d

         .byte   "No such "
         .byte   $a4
         .byte   $2f
         .byte   $f2
         .byte   $00

L88C8:   ldy     #$00
@L88CA:  jsr     L61EC
         phy
         lda     DP_BAS_PAGE+2
         sta     DP_BAS_TXTPTR2+2
         lda     DP_BAS_PAGE+1
         sta     DP_BAS_TXTPTR2+1
         lda     DP_BAS_PAGE
         sta     DP_BAS_TXTPTR2
@L88DA:  ldy     #$01
         lda     [DP_BAS_TXTPTR2],y
         bpl     @L88EB
         ply
         iny
         iny
         iny
         cpy     $98
         bne     @L88CA
         brl     @L896C

@L88EB:  ldy     #$03
@L88ED:  iny
         lda     [DP_BAS_TXTPTR2],y
         cmp     #$20
         beq     @L88ED
         cmp     #$dd
         beq     @L890B
@L88F8:  ldy     #$03
         lda     [DP_BAS_TXTPTR2],y
         clc
         adc     DP_BAS_TXTPTR2
         sta     DP_BAS_TXTPTR2
         bcc     @L88DA
         inc     DP_BAS_TXTPTR2+1
         bne     @L88DA
         inc     DP_BAS_TXTPTR2+2
         bra     @L88DA

@L890B:  iny
         sty     DP_BAS_TXTPTR2_OFF
         jsr     parse_skip_spaces_PTR2
         tya
         pha
         clc
         adc     DP_BAS_TXTPTR2
         ldy     DP_BAS_TXTPTR2+1
         ldx     DP_BAS_TXTPTR2+2
         bcc     @L8921
         iny
         bne     @L8920
         inx
@L8920:  clc
@L8921:  sbc     #$00
         sta     DP_FPB_sgn
         tya
         sbc     #$00
         sta     $5d
         txa
         sbc     #$00
         sta     DP_FPB_exp
         plx
         ldy     #$02
@L8932:  inx
         dey
         lda     [DP_FPB_sgn],y
         cmp     [DP_BAS_TMP6],y
         bne     @L88F8
         iny
         iny
         cpy     DP_BAS_TMP6+3
         bne     @L8932
         dey
         lda     [DP_FPB_sgn],y
         jsr     L6031
         bcs     @L88F8
         txa
         tay
         jsr     parse_updPTRfromPTR2_yield
         jsr     L6E84
         ldx     #$01
         jsr     L6EFA
         lda     DP_BAS_TXTPTR2
         sta     [DP_BAS_VARTOP]
         ldy     #$01
         lda     DP_BAS_TXTPTR2+1
         sta     [DP_BAS_VARTOP],y
         iny
         lda     DP_BAS_TXTPTR2+2
         sta     [DP_BAS_VARTOP],y
         iny
         jsr     L6F02
         pla
         lda     #$01
         rts

@L896C:  lda     #$00
         rts

L896F:   brk     $1e

         .byte   "Bad call"
         .byte   $00

exec_FN: lda     #$a4
exec_FN_PROC:
         sta     $40
         jsr     L623E
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
@L8998:  iny
         phd
         tsc
         tcd
         lda     $02,x
         pld
         sta     [DP_BAS_STACK],y
         dex
         bne     @L8998
         lda     DP_stack_save+1
         xba
         lda     DP_stack_save
         tcs
         lda     $40
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
@L89C8:  clc
@L89C9:  sbc     #$01
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
         beq     L896F
         stx     DP_BAS_TXTPTR_OFF
         jsr     varFindPROCFN
         bne     @L89F1
         jsr     L88C8
         bne     @L8A01
         brl     L88AC

@L89F1:  lda     [DP_BAS_INT_WA]
         sta     DP_BAS_TXTPTR2
         ldy     #$01
         lda     [DP_BAS_INT_WA],y
         sta     DP_BAS_TXTPTR2+1
         iny
         lda     [DP_BAS_INT_WA],y
         sta     DP_BAS_TXTPTR2+2
         dey
@L8A01:  ldx     #$1f
         jsr     L620C
         lda     #$00
         pha
         stz     DP_BAS_TXTPTR2_OFF
         jsr     parse_skip_spaces_PTR2
         cmp     #$28
         beq     @L8A81
         dec     DP_BAS_TXTPTR2_OFF
@L8A14:  lda     DP_BAS_TXTPTR_OFF
         pha
         lda     DP_BAS_TXTPTR
         pha
         lda     DP_BAS_TXTPTR+1
         pha
         lda     DP_BAS_TXTPTR+2
         pha
         jsr     L6364
         pla
         sta     DP_BAS_TXTPTR+2
         pla
         sta     DP_BAS_TXTPTR+1
         pla
         sta     DP_BAS_TXTPTR
         pla
         sta     DP_BAS_TXTPTR_OFF
         pla
         beq     @L8A3E
         sta     DP_FPB_mant+3
@L8A34:  jsr     stack_copy6_to_DP_BAS_TMP66_uns4
         jsr     L98B8
         dec     DP_FPB_mant+3
         bne     @L8A34
@L8A3E:  pla
         sta     DP_BAS_TXTPTR2+2
         pla
         sta     DP_BAS_TXTPTR2+1
         pla
         sta     DP_BAS_TXTPTR2
         ldx     #$1f
         jsr     L620C
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
@L8A63:  iny
         lda     [DP_BAS_STACK],y
         phd
         pha
         tsc
         tcd
         pla
         sta     $03,x
         pld
         dex
         bne     @L8A63
         tya
         adc     DP_BAS_STACK
         sta     DP_BAS_STACK
         bcc     @L8A7E
         inc     DP_BAS_STACK+1
         bne     @L8A7E
         inc     DP_BAS_STACK+2
@L8A7E:  lda     $40
         rts

@L8A81:  lda     DP_BAS_TXTPTR_OFF
         pha
         lda     DP_BAS_TXTPTR
         pha
         lda     DP_BAS_TXTPTR+1
         pha
         lda     DP_BAS_TXTPTR+2
         pha
         jsr     var_find
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
         jsr     L8B55
         jsr     parse_SkipSpacesPTR2_cmp_COMMA
         beq     @L8A81
         cmp     #$29
         bne     @L8AEA
         lda     #$00
         pha
         jsr     parse_skip_spaces
         cmp     #$28
         bne     @L8AEA
@L8AC7:  jsr     L7537
         jsr     stack_INTorREAL
         lda     $40
         sta     DP_BAS_INT_WA
         jsr     stack_INTWA
         plx
         inx
         phx
         jsr     parse_skip_space_CMPcomma
         beq     @L8AC7
         cmp     #$29
         bne     @L8AEA
         pla
         pla
         sta     $6f
         sta     $70
         cpx     $6f
         beq     @L8B0F
@L8AEA:  sec
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

         .byte   "Arguments"
         .byte   $00

@L8B0F:  jsr     L993C
         lda     DP_BAS_INT_WA
         sta     $40
         pla
         sta     DP_BAS_INT_WA
         pla
         sta     DP_BAS_INT_WA+1
         pla
         sta     DP_BAS_INT_WA+2
         pla
         sta     DP_BAS_INT_WA+3
         bmi     @L8B41
         lda     $40
         beq     @L8AEA
         ldx     #$55
         jsr     L9A49
         lda     $40
         bpl     @L8B39
         jsr     popFPFromStackToPTR1
         jsr     fpCopyPTR1toFPA
         bra     @L8B3C

@L8B39:  jsr     L993C
@L8B3C:  jsr     L8D6C
         bra     @L8B4B

@L8B41:  lda     $40
         bne     @L8AEA
         jsr     L9928
         jsr     L64B5
@L8B4B:  dec     $6f
         bne     @L8B0F
         lda     $70
         pha
         jmp     @L8A14

L8B55:   ldy     DP_BAS_INT_WA+3
         cpy     #$05
         bcs     @L8B60
         ldx     #$55
         jsr     L9A49
@L8B60:  jsr     L8B74
         php
         jsr     stack_INTorREAL
         plp
         beq     @L8B71
         bmi     @L8B71
         ldx     #$55
         jsr     L80B4
@L8B71:  jmp     stack_INTWA

L8B74:   ldy     DP_BAS_INT_WA+3
         bmi     L8BCE
         beq     @L8B99
         cpy     #$05
         beq     L8B9E
         ldy     #$03
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
         lda     #$40
         rts

@L8B99:  lda     [DP_BAS_INT_WA],y
         jmp     retAY_16bit_INT

L8B9E:   stz     DP_FPA_mant+4
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
         bne     @L8BC6
         tya
         ora     DP_FPA_mant+1
         ora     DP_FPA_mant+2
         ora     DP_FPA_mant+3
         beq     @L8BC9
@L8BC6:  tya
         ora     #$80
@L8BC9:  sta     DP_FPA_mant
         lda     #$ff
         rts

L8BCE:   cpy     #$80
         beq     @L8BF4
         ldy     #$04
         lda     [DP_BAS_INT_WA],y
         sta     DP_BAS_STRLEN
         beq     @L8BF3
         ldy     #$02
         lda     [DP_BAS_INT_WA],y
         sta     DP_BAS_TMP6+2
         dey
         lda     [DP_BAS_INT_WA],y
         sta     DP_BAS_TMP6+1
         lda     [DP_BAS_INT_WA]
         sta     DP_BAS_TMP6
         ldy     DP_BAS_STRLEN
@L8BEB:  dey
         lda     [DP_BAS_TMP6],y
         sta     [DP_BAS_STRWKSP_L],y
         tya
         bne     @L8BEB
@L8BF3:  rts

@L8BF4:  lda     DP_BAS_INT_WA+1
         ora     DP_BAS_INT_WA+2
         beq     ret_INT_WA_lo_as_1ch_str
         ldy     #$00
@L8BFC:  lda     [DP_BAS_INT_WA],y
         bne     @L8C02
         lda     #$0d
@L8C02:  sta     [DP_BAS_STRWKSP_L],y
         eor     #$0d
         beq     @L8C0C
         iny
         bne     @L8BFC
         tya
@L8C0C:  sty     DP_BAS_STRLEN
         rts

exec_CHR:
         jsr     eval_ret_INT
ret_INT_WA_lo_as_1ch_str:
         lda     DP_BAS_INT_WA
         jmp     retA_as_1ch_str

L8C17:   ldy     DP_BAS_TXTPTR2_OFF
         beq     @L8C1C
         dey
@L8C1C:  jsr     parse_updPTRfromPTR2_yield
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
         bne     @L8C3C
         lda     DP_BAS_TXTPTR2+2
         cmp     DP_BAS_MEMBASE+2
         beq     @L8C6C
@L8C3C:  ldx     DP_BAS_TXTPTR2
@L8C3E:  jsr     L604D
         cmp     #$0d
         bne     @L8C61
         cpx     DP_BAS_TMP6
         tya
         sbc     DP_BAS_TMP6+1
         lda     DP_BAS_TXTPTR2+2
         sbc     DP_BAS_TMP6+2
         bcc     @L8C6C
         jsr     L604D
         ora     #$00
         bmi     @L8C6D
         sta     DP_BAS_ERL+1
         jsr     L604D
         sta     DP_BAS_ERL
         jsr     L604D
@L8C61:  cpx     DP_BAS_TMP6
         tya
         sbc     DP_BAS_TMP6+1
         lda     DP_BAS_TXTPTR2+2
         sbc     DP_BAS_TMP6+2
         bcs     @L8C3E
@L8C6C:  rts

@L8C6D:  stz     DP_BAS_ERL
         stz     DP_BAS_ERL+1
         rts

BRK_HANDLER:
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
         lda     #$da
         jsr     call_OSBYTE
         lda     #$7e
         jsr     call_OSBYTE
         lda     #$03
         cop     $16
         jsr     L8C17
         stz     DP_BAS_TRACEFLAG
         phx
         ldx     #$14
         jsl     _ST
         plx
         eor     #$00
         bcc     @L8CAC
         jsr     exec_ERROR_OFF_reset_ERRORPTR
@L8CAC:  lda     DP_BAS_ONERRORPTR
         sta     DP_BAS_TXTPTR2
         lda     DP_BAS_ONERRORPTR+1
         sta     DP_BAS_TXTPTR2+1
         lda     DP_BAS_ONERRORPTR+2
         sta     DP_BAS_TXTPTR2+2
         ldx     #$1f
         jsr     L620C
         stz     DP_BAS_TXTPTR2_OFF
         jsr     L97FD
         jmp     L6364

exec_ERROR_OFF_reset_ERRORPTR:
         lda     #<bas_DEF_ERROR
         sta     DP_BAS_ONERRORPTR
         lda     #>bas_DEF_ERROR
         sta     DP_BAS_ONERRORPTR+1
         phk
         pla
         sta     DP_BAS_ONERRORPTR+2
         rts

cmp_ERRORPTRisDEFAULT:
         phk
         pla
         cmp     DP_BAS_ONERRORPTR+2
         bne     @rts
         lda     #>bas_DEF_ERROR
         cmp     DP_BAS_ONERRORPTR+1
         bne     @rts
         lda     #<bas_DEF_ERROR
         cmp     DP_BAS_ONERRORPTR
@rts:    rts

bas_DEF_ERROR:
         .byte   $f6        ;REPORT
         .byte   ':'
         .byte   $e7        ;IF
         .byte   $9e        ;ERL
         .byte   $f1        ;PRINT
         .byte   $22," at line ",$22,";"
         .byte   $9e        ;ERL
         .byte   ':'
         .byte   $e0        ;END
         .byte   $8b        ;ELSE
         .byte   $f1        ;PRINT
         .byte   ':'
         .byte   $e0        ;END
         .byte   $0d

exec_SOUND:
         jsr     eval_INTBRK
         ldx     #$03
@parmlp: lda     DP_BAS_INT_WA
         pha
         lda     DP_BAS_INT_WA+1
         pha
         phx
         jsr     eval_commaINTBRK
         plx
         dex
         bne     @parmlp
         jsr     parse_nextstmt_yield_PTR2_OFF
         lda     DP_BAS_INT_WA
         pha
         lda     DP_BAS_INT_WA+1
         pha
         ldx     #$07
         ldy     #$07
         bra     L8D38

exec_ENVELOPE:
         jsr     eval_INTBRK
         ldx     #$0d
@parmlp: lda     DP_BAS_INT_WA
         pha
         phx
         jsr     eval_commaINTBRK
         plx
         dex
         bne     @parmlp
         jsr     parse_nextstmt_yield_PTR2_OFF
         lda     DP_BAS_INT_WA
         pha
         ldx     #$0d
         ldy     #$08
L8D38:   pla
         sta     DP_BAS_TMP6,x
         dex
         bpl     L8D38
         tya
         ldx     #$55
         ldy     #$00
L8D43:   jsr     cop_OSWORD
         bra     L8D53

exec_WIDTH:
         jsr     eval_INTBRK
         jsr     parse_nextstmt_yield_PTR2_OFF
         ldy     DP_BAS_INT_WA
         dey
         sty     DP_BAS_WIDTH
L8D53:   jmp     parse_EOS_TXTPTR2

jmp_brk06_type_mismatch4:
         jmp     brk06_type_mismatch

L8D59:   jsr     L7537
L8D5C:   ply
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
L8D6C:   lda     DP_BAS_TMP6+3
         cmp     #$05
         beq     @L8D95
         lda     $40
         beq     jmp_brk06_type_mismatch4
         bpl     @L8D7B
         jsr     eval_real2INT
@L8D7B:  lda     DP_BAS_INT_WA
         ldy     #$00
         sta     [DP_BAS_TMP6],y
         lda     DP_BAS_TMP6+3
         beq     @L8D94
         lda     DP_BAS_INT_WA+1
         iny
         sta     [DP_BAS_TMP6],y
         lda     DP_BAS_INT_WA+2
         iny
         sta     [DP_BAS_TMP6],y
         lda     DP_BAS_INT_WA+3
         iny
         sta     [DP_BAS_TMP6],y
@L8D94:  rts

@L8D95:  lda     $40
         beq     jmp_brk06_type_mismatch4
         bmi     L8D9E
         jsr     L5117
L8D9E:   lda     DP_FPA_exp+1
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

exec_EDIT:
         brk     $00

         .byte   "Not available"
         .byte   $00

L8DCE:   stz     $5b
         stz     DP_FPB_sgn
         jsr     exec_FALSE
         jsr     parse_lineno_atOFF
         php
         jsr     stack_INTWA
         jsr     exec_TRUE
         lsr     DP_BAS_INT_WA+1
         plp
         bcc     @L8DF3
         jsr     parse_SkipSpacesPTR2_cmp_COMMA
         beq     @L8DFA
         jsr     L993C
         jsr     stack_INTWA
         dec     DP_BAS_TXTPTR2_OFF
         bra     @L8DFD

@L8DF3:  jsr     parse_SkipSpacesPTR2_cmp_COMMA
         beq     @L8DFA
         dec     DP_BAS_TXTPTR2_OFF
@L8DFA:  jsr     parse_lineno_atOFF
@L8DFD:  ldx     #$4f
         jsr     L9A49
         jsr     parse_skip_spaces_PTR2
         cmp     #$e7
         bne     L8E27
         jsr     parse_skip_spaces_PTR2
         jsr     parse_updPTRfromPTR2_yield
         bra     L8E2A

exec_LIST:
         iny
         lda     [DP_BAS_TXTPTR2],y
         cmp     #'O'       ;Check for 'O'
         bne     L8DCE      ;go and do "LIST"
         inc     DP_BAS_TXTPTR2_OFF ;Skip 'O'
; Do LISTO
         jsr     eval_INTBRK
         jsr     parse_nextstmt_yield_TXTOFF2
         lda     DP_BAS_INT_WA
         sta     DP_BAS_LISTO
         jmp     reset_prog_prompt

L8E27:   jsr     L72D9
L8E2A:   lda     DP_BAS_TXTPTR2
         sta     DP_BAS_TXTPTR
         jsr     do_END
         jsr     L993C
         jsr     prog_search_lineno
         lda     DP_FPB_exp
         sta     DP_BAS_TXTPTR2
         lda     DP_FPB_mant
         sta     DP_BAS_TXTPTR2+1
         lda     DP_FPB_mant+1
         sta     DP_BAS_TXTPTR2+2
         bcs     @L8E60
         dey
         bra     @L8E57

@L8E48:  jsr     printChOrSendtoLOMEMandINC
         bit     DP_BAS_LISTO
         bmi     @L8E54
         lda     #$0a
         jsr     call_OSWRCH
@L8E54:  jsr     parse_updPTRfromPTR2_yield
@L8E57:  lda     [DP_BAS_TXTPTR2],y
         sta     DP_BAS_INT_WA+1
         iny
         lda     [DP_BAS_TXTPTR2],y
         sta     DP_BAS_INT_WA
@L8E60:  lda     DP_BAS_INT_WA
         clc
         sbc     DP_FPA_mant
         lda     DP_BAS_INT_WA+1
         sbc     DP_FPA_mant+1
         bcc     @L8E6E
         brl     reset_prog_prompt

@L8E6E:  stz     $6f
         stz     $70
         ldy     #$04
         sty     DP_BAS_TXTPTR2_OFF
         sty     DP_BAS_TXTPTR_OFF
         bit     $5b
         bpl     @L8E7E
         stz     $5b
@L8E7E:  bit     DP_FPB_sgn
         bpl     @L8E84
         stz     DP_FPB_sgn
@L8E84:  lda     [DP_BAS_TXTPTR2],y
         cmp     #$0d
         beq     @L8EC3
         cmp     #$f4
         beq     @L8E94
         cmp     #$22
         bne     @L8E96
         eor     $6f
@L8E94:  sta     $6f
@L8E96:  ldx     $6f
         bne     @L8EA6
         cmp     #$ed
         bne     @L8EA0
         dec     $5b
@L8EA0:  cmp     #$fd
         bne     @L8EA6
         dec     DP_FPB_sgn
@L8EA6:  ldx     DP_BAS_TXTPTR
@L8EA8:  phy
         txy
         lda     [DP_BAS_MEMBASE],y
         ply
         cmp     #$0d
         beq     @L8EBB
         cmp     [DP_BAS_TXTPTR2],y
         bne     @L8EBD
         iny
         inx
         bra     @L8EA8

@L8EB9:  bra     @L8E48

@L8EBB:  sta     $70
@L8EBD:  inc     DP_BAS_TXTPTR_OFF
         ldy     DP_BAS_TXTPTR_OFF
         bra     @L8E84

@L8EC3:  lda     $70
         beq     @L8E54
         jsr     L7891
         lda     #$01
         inx
         sec
         jsr     L9A37
         ldx     $5b
         lda     #$02
         jsr     L9A36
         ldx     DP_FPB_sgn
         lda     #$04
         jsr     L9A36
         stz     $6f
@L8EE1:  ldy     DP_BAS_TXTPTR2_OFF
@L8EE3:  lda     [DP_BAS_TXTPTR2],y
         cmp     #$0d
         beq     @L8EB9
         cmp     #$22
         bne     @L8EF9
         eor     $6f
         sta     $6f
         lda     #$22
@L8EF3:  jsr     printChOrSendtoLOMEMandINC
         iny
         bra     @L8EE3

@L8EF9:  ldx     $6f
         bne     @L8EF3
         cmp     #$8d
         bne     @L8F0B
         jsr     parse_lineno_atPTR2_Y
         sty     DP_BAS_TXTPTR2_OFF
         jsr     printWAasDecimal16b
         bra     @L8EE1

@L8F0B:  cmp     #$e3
         bne     @L8F11
         inc     $5b
@L8F11:  cmp     #$f5
         bne     @L8F17
         inc     DP_FPB_sgn
@L8F17:  cmp     #$f4
         bne     @L8F1D
         sta     $6f
@L8F1D:  jsr     PrintChOrToken
         iny
         bra     @L8EE3

exec_NEXT:
         jsr     L6F78
         bne     @L8F31
         ldx     DP_BAS_FOR_LVL
         beq     @L8F68
         bcs     @L8F6F
@L8F2E:  jmp     brk10_Syntax

@L8F31:  bcs     @L8F2E
         ldx     DP_BAS_FOR_LVL
         beq     @L8F68
@L8F37:  ldy     #$00
         lda     DP_BAS_INT_WA
         cmp     [DP_BAS_FOR_STACKBASE],y
         bne     @L8F54
         lda     DP_BAS_INT_WA+1
         iny
         cmp     [DP_BAS_FOR_STACKBASE],y
         bne     @L8F54
         iny
         lda     DP_BAS_INT_WA+2
         cmp     [DP_BAS_FOR_STACKBASE],y
         bne     @L8F54
         lda     DP_BAS_INT_WA+3
         iny
         cmp     [DP_BAS_FOR_STACKBASE],y
         beq     @L8F6F
@L8F54:  jsr     L9132
         bne     @L8F37
         brk     $21

         .byte   "Can't match "
         .byte   $e3

@L8F68:  brk     $20

         .byte   $4e
         .byte   $6f
         .byte   $20
         .byte   $e3
         .byte   $00

@L8F6F:  ldy     #$00
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
         bne     @L8F8A
         brl     @L9010

@L8F8A:  ldy     #$04
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
         beq     @L8FEB
         txa
         ldy     #$07
         eor     [DP_BAS_FOR_STACKBASE],y
         ldy     #$0c
         eor     [DP_BAS_FOR_STACKBASE],y
         bpl     @L8FE9
         bcs     @L8FEB
         bra     @L9001

@L8FE9:  bcs     @L9001
@L8FEB:  ldy     #$0e
         lda     [DP_BAS_FOR_STACKBASE],y
         sta     DP_BAS_TXTPTR2
         iny
         lda     [DP_BAS_FOR_STACKBASE],y
         sta     DP_BAS_TXTPTR2+1
         iny
         lda     [DP_BAS_FOR_STACKBASE],y
         sta     DP_BAS_TXTPTR2+2
         jsr     L72B1
         jmp     L6364

@L9001:  jsr     L9132
         ldy     DP_BAS_TXTPTR_OFF
         sty     DP_BAS_TXTPTR2_OFF
         jsr     parse_SkipSpacesPTR2_cmp_COMMA
         bne     @L9042
         jmp     exec_NEXT

@L9010:  jsr     L8B9E
         lda     #$04
         jsr     L9148
         jsr     L7E91
         lda     DP_BAS_INT_WA
         sta     DP_BAS_TMP6
         lda     DP_BAS_INT_WA+1
         sta     DP_BAS_TMP6+1
         lda     DP_BAS_INT_WA+2
         sta     DP_BAS_TMP6+2
         jsr     L8D9E
         lda     #$09
         jsr     L9148
         jsr     L7480
         beq     @L8FEB
         ldy     #$05
         lda     [DP_BAS_FOR_STACKBASE],y
         bmi     @L903E
         bcs     @L8FEB
         bra     @L9001

@L903E:  bcc     @L8FEB
         bra     @L9001

@L9042:  jmp     L6359

brk22_FOR_variable:
         brk     $22

         .byte   $e3
         .byte   " variable"

brk23_TooManyFORs:
         brk     $23

         .byte   "Too many "
         .byte   $e3
         .byte   's'

brk24_NoTO:
         brk     $24

         .byte   "No "
         .byte   $b8
         .byte   $00

exec_FOR:
         jsr     var_find
         beq     brk22_FOR_variable
         bcs     brk22_FOR_variable
         jsr     pushINTWA_on_hw_stack
         jsr     parseEqualsOrMistake
         jsr     L8D59
         jsr     parse_skip_spaces
         cmp     #$b8
         bne     brk24_NoTO
         ldy     DP_BAS_FOR_LVL
         cpy     #$0a
         bcs     brk23_TooManyFORs
         jsr     @L9120
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
         beq     @L90F4
         jsr     L6C7F
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
         bne     @L90C7
         jsr     L6C7F
         ldy     DP_BAS_TXTPTR_OFF
@L90C7:  sty     DP_BAS_TXTPTR2_OFF
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
@L90DE:  jsr     L73B1
         ldy     #$0e
         lda     DP_BAS_TXTPTR2
         sta     [DP_BAS_FOR_STACKBASE],y
         lda     DP_BAS_TXTPTR2+1
         iny
         sta     [DP_BAS_FOR_STACKBASE],y
         iny
         lda     DP_BAS_TXTPTR2+2
         sta     [DP_BAS_FOR_STACKBASE],y
         jmp     L6364

@L90F4:  jsr     L7537
         jsr     L6CC7
         lda     #$09
         jsr     L9148
         jsr     fpCopyFPAtoPTR1
         jsr     L7DE1
         jsr     parse_skip_spaces
         cmp     #$88
         bne     @L9114
         jsr     L7537
         jsr     L6CC7
         ldy     DP_BAS_TXTPTR_OFF
@L9114:  sty     DP_BAS_TXTPTR2_OFF
         lda     #$04
         jsr     L9148
         jsr     fpCopyFPAtoPTR1
         bra     @L90DE

@L9120:  clc
         lda     DP_BAS_FOR_STACKBASE
         adc     #$11
         sta     DP_BAS_FOR_STACKBASE
         bcc     @L912F
         inc     VAR_OFFS_FN
         bne     @L912F
         inc     DP_BAS_FOR_STACKBASE+2
@L912F:  inc     DP_BAS_FOR_LVL
         rts

L9132:   sec
         lda     DP_BAS_FOR_STACKBASE
         sbc     #$11
         sta     DP_BAS_FOR_STACKBASE
         lda     VAR_OFFS_FN
         sbc     #$00
         sta     VAR_OFFS_FN
         lda     DP_BAS_FOR_STACKBASE+2
         sbc     #$00
         sta     DP_BAS_FOR_STACKBASE+2
         dec     DP_BAS_FOR_LVL
         rts

L9148:   clc
         adc     DP_BAS_FOR_STACKBASE
         sta     DP_BAS_FP_PTR1
         lda     #$00
         adc     VAR_OFFS_FN
         sta     DP_BAS_FP_PTR1+1
         lda     #$00
         adc     DP_BAS_FOR_STACKBASE+2
         sta     DP_BAS_FP_PTR1+2
         rts

exec_GOSUB:
         jsr     L623E
         jsr     parse_lineno_searchprog_brknfnd
L9160:   jsr     parse_nextstmt_yield_TXTOFF2
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
         bra     L91C1

brk25_TooManyGOSUBS:
         brk     $25

         .byte   "Too many "
         .byte   $e4
         .byte   's'

L9189:   brk     $26

         .byte   $4e
         .byte   $6f
         .byte   $20
         .byte   $e4
         .byte   $00

exec_RETURN:
         jsr     parse_nextstmt_yield_TXTOFF2
         bit     DP_BAS_CO_FLAGS
         bmi     @L91A0
         jsl     _CWT
         phk
         plb
         jmp     parse_EOS_TXTPTR2

@L91A0:  ldy     DP_BAS_GOSUB_LVL
         beq     L9189
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
         jmp     parse_EOS_TXTPTR2

exec_GOTO:
         jsr     L623E
         jsr     parse_lineno_searchprog_brknfnd
         jsr     parse_nextstmt_yield_TXTOFF2
L91C1:   lda     DP_BAS_TRACEFLAG
         beq     @L91C8
         jsr     doTracePrintLineNoIfInRange
@L91C8:  ldy     #$04
         sty     DP_BAS_TXTPTR2_OFF
         ldy     DP_FPB_exp
         lda     DP_FPB_mant
         ldx     DP_FPB_mant+1
         stz     DP_BAS_LAST_EOSEOR_D_
         sty     DP_BAS_TXTPTR2
         sta     DP_BAS_TXTPTR2+1
         stx     DP_BAS_TXTPTR2+2
         jmp     L6364

exec_ON_ERROR_OFF:
         jsr     parse_nextstmt_yield_TXTOFF2
         jsr     exec_ERROR_OFF_reset_ERRORPTR
         bra     jmp_parse_EOR_TXTPTR2

exec_ON_ERROR:
         jsr     parse_skip_spaces_PTR2
         cmp     #$87       ;check for OFF
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

exec_ON: jsr     parse_skip_spaces_PTR2
         cmp     #$85       ;check for ERROR
         beq     exec_ON_ERROR
         dec     DP_BAS_TXTPTR2_OFF
         jsr     eval_INTBRK
         cpx     #$f2
         beq     @L921C
         iny
         cpx     #$e5
         beq     @L921C
         cpx     #$e4
         bne     L9286
@L921C:  phx
         lda     DP_BAS_INT_WA+1
         ora     DP_BAS_INT_WA+2
         ora     DP_BAS_INT_WA+3
         bne     @L9271
         ldx     DP_BAS_INT_WA
         beq     @L9271
         dec     DP_BAS_INT_WA
         beq     @L9252
         tax
@L922E:  lda     [DP_BAS_TXTPTR2],y
         cmp     #$0d
         beq     @L9271
         cmp     #$3a
         beq     @L9271
         cmp     #$8b
         beq     @L9271
         iny
         cmp     #$29
         bne     @L9242
         dex
@L9242:  cmp     #$28
         bne     @L9247
         inx
@L9247:  cmp     #$2c
         bne     @L922E
         txa
         bne     @L922E
         dec     DP_BAS_INT_WA
         bne     @L922E
@L9252:  pla
         cmp     #$f2
         beq     L929F
         sty     DP_BAS_TXTPTR2_OFF
         cmp     #$e4
         beq     @L9266
         jsr     parse_lineno_searchprog_brknfnd
         jsr     L72B1
         jmp     L91C1

@L9266:  jsr     parse_lineno_searchprog_brknfnd
         ldy     DP_BAS_TXTPTR2_OFF
         jsr     L92B9
         jmp     L9160

@L9271:  pla
@L9272:  lda     [DP_BAS_TXTPTR2],y
         iny
         cmp     #$8b
         beq     L92B3
         cmp     #$0d
         bne     @L9272
         brk     $28

         .byte   $ee
         .byte   " range"

L9286:   brk     $27

         .byte   $ee
         .byte   " syntax"

brk29_NoSuchLine:
         brk     $29

         .byte   "No such line"
         .byte   $00

L929F:   sty     DP_BAS_TXTPTR_OFF
         jsr     parse_skip_spaces
         cmp     #$f2
         bne     L9286
         jsr     exec_FN_PROC
         ldy     DP_BAS_TXTPTR_OFF
         jsr     L92B9
         jmp     L635B

L92B3:   sty     DP_BAS_TXTPTR2_OFF
         jmp     L7417

L92B8:   iny
L92B9:   lda     [DP_BAS_TXTPTR2],y
         cmp     #$0d
         beq     @L92C3
         cmp     #$3a
         bne     L92B8
@L92C3:  sty     DP_BAS_TXTPTR2_OFF
         rts

parse_lineno_searchprog_brknfnd:
         lda     DP_BAS_TXTPTR2_OFF
         sta     $c7
         stz     DP_BAS_LAST_EOSEOR_D_
         jsr     parse_lineno_atOFF
         bcs     prog_search_lineno_brknotfnd
         lda     #$80
         sta     DP_BAS_SET_TO_Z_AT_EOS
         jsr     eval_INTBRK
         lda     #$80
         trb     DP_BAS_SET_TO_Z_AT_EOS
         bit     DP_BAS_SET_TO_Z_AT_EOS
         bvs     L930C
         bit     DP_BAS_INT_WA+3
         bmi     L92EE
         lda     #$80
         trb     DP_BAS_INT_WA+1
prog_search_lineno_brknotfnd:
         jsr     prog_search_lineno
         bcc     brk29_NoSuchLine
         rts

L92EE:   lda     DP_BAS_INT_WA
         sta     DP_FPB_exp
         lda     DP_BAS_INT_WA+1
         sta     DP_FPB_mant
         lda     DP_BAS_INT_WA+2
         sta     DP_FPB_mant+1
         bit     DP_BAS_TRACEFLAG
         beq     @L930B
         phy
         ldy     #$02
         lda     [DP_FPB_exp],y
         sta     DP_BAS_INT_WA
         dey
         lda     [DP_FPB_exp],y
         sta     DP_BAS_INT_WA+1
         ply
@L930B:  rts

L930C:   lda     $c7
         sta     DP_BAS_TXTPTR2_OFF
         stz     DP_BAS_SET_TO_Z_AT_EOS
         bra     parse_lineno_searchprog_brknfnd

L9314:   phb
         phx
         rep     #$10
         .i16
         ldy     DP_BAS_CURCHAN
         cop     $0a
         jsr     setREPORT_BHA_Cy
         sep     #$10
         .i8
         plx
         plb
         rts

jmp_brk06_type_mismatch3:
         jmp     brk06_type_mismatch

L9327:   jmp     brk10_Syntax

L932A:   sty     DP_BAS_TXTPTR2_OFF
L932C:   jmp     L635B

exec_INPUT_HASH:
         jsr     L956B
         rep     #$10
         .i16
         ldy     DP_BAS_INT_WA
         sty     DP_BAS_CURCHAN
         sep     #$10
         .i8
         jsr     L66AB
@L933D:  jsr     parse_SkipSpacesPTR2_cmp_COMMA
         bne     L932A
         jsr     var_find
         beq     L9327
         jsr     L66AB
         php
         jsr     stack_INTWA
         jsr     L9314
         sta     $40
         plp
         bcc     @L9371
         lda     $40
         bne     jmp_brk06_type_mismatch3
         jsr     L9314
         sta     DP_BAS_STRLEN
         tax
         beq     @L936C
@L9362:  jsr     L9314
         txy
         dey
         sta     [DP_BAS_STRWKSP_L],y
         dex
         bne     @L9362
@L936C:  jsr     L64B2
         bra     @L933D

@L9371:  lda     $40
         beq     jmp_brk06_type_mismatch3
         bmi     @L9383
         ldx     #$03
@L9379:  jsr     L9314
         sta     DP_BAS_INT_WA,x
         dex
         bpl     @L9379
         bra     @L9390

@L9383:  ldx     #$04
@L9385:  jsr     L9314
         sta     $00,x
         dex
         bpl     @L9385
         jsr     fpCopyPrintPTRtoFPA
@L9390:  jsr     stack_copy6_to_DP_BAS_TMP66_uns4
         jsr     L8D6C
         bra     @L933D

L9398:   pla
         pla
         bra     L932C

exec_INPUT:
         jsr     parse_skipSpacesPTR2_cmp_HASH
         beq     exec_INPUT_HASH
         cmp     #$86       ;check for LINE
         beq     exec_INPUT_LINE
         dec     DP_BAS_TXTPTR2_OFF
         clc
exec_INPUT_LINE:
         ror     $6f
         lsr     $6f
         lda     #$ff
         sta     $70
@L93B0:  jsr     L66D3
         bcs     @L93BF
@L93B5:  jsr     L66D3
         bcc     @L93B5
         ldx     #$ff
         stx     $70
         clc
@L93BF:  php
         asl     $6f
         plp
         ror     $6f
         cmp     #$2c
         beq     @L93B0
         cmp     #$3b
         beq     @L93B0
         dec     DP_BAS_TXTPTR2_OFF
         lda     $6f
         pha
         lda     $70
         pha
         jsr     var_find
         beq     L9398
         pla
         sta     $70
         pla
         sta     $6f
         jsr     L66AB
         php
         bit     $6f
         bvs     @L93EE
         lda     $70
         cmp     #$ff
         bne     @L9405
@L93EE:  bit     $6f
         bpl     @L93F7
         lda     #$3f
         jsr     call_OSWRCH
@L93F7:  jsr     L95B3
         sty     DP_BAS_STRLEN
         asl     $6f
         clc
         ror     $6f
         bit     $6f
         bvs     @L9424
@L9405:  sta     DP_BAS_TXTPTR_OFF
         lda     DP_BAS_STRWKSP_L
         sta     DP_BAS_TXTPTR
         lda     DP_BAS_STRWKSP_L+1
         sta     DP_BAS_TXTPTR+1
         lda     DP_BAS_STRWKSP_L+2
         sta     DP_BAS_TXTPTR+2
         jsr     parseDataStringToStrWksp
@L9416:  jsr     parse_skip_space_CMPcomma
         beq     @L9421
         cmp     #$0d
         bne     @L9416
         ldy     #$fe
@L9421:  iny
         sty     $70
@L9424:  plp
         bcs     @L9433
         jsr     pushINTWA_on_hw_stack
         jsr     L8213
         jsr     L8D5C
@L9430:  brl     @L93B0

@L9433:  stz     $40
         jsr     L64B5
         bra     @L9430

exec_RESTORE:
         ldy     DP_BAS_PAGE
         sty     DP_FPB_exp
         ldy     DP_BAS_PAGE+1
         sty     DP_FPB_mant
         ldy     DP_BAS_PAGE+2
         sty     DP_FPB_mant+1
         jsr     parse_skip_spaces_PTR2
         dec     DP_BAS_TXTPTR2_OFF
         cmp     #$3a
         beq     @L945E
         cmp     #$0d
         beq     @L945E
         cmp     #$8b
         beq     @L945E
         lda     DP_BAS_TXTPTR2_OFF
         sta     $c7
         jsr     parse_lineno_searchprog_brknfnd
@L945E:  jsr     parse_nextstmt_yield_TXTOFF2
         lda     DP_FPB_exp
         sta     DP_BAS_DATA_PTR
         lda     DP_FPB_mant
         sta     DP_BAS_DATA_PTR+1
         lda     DP_FPB_mant+1
         sta     DP_BAS_DATA_PTR+2
         lda     #$ff
         sta     $36
         jmp     parse_EOS_TXTPTR2

L9474:   jsr     parse_SkipSpacesPTR2_cmp_COMMA
         beq     exec_READ
         jmp     L6359

exec_READ:
         jsr     var_find
         beq     L9474
         bcs     @L948E
         jsr     @L94B1
         jsr     pushINTWA_on_hw_stack
         jsr     L8D59
         bra     @L949C

@L948E:  jsr     @L94B1
         jsr     stack_INTWA
         jsr     parseDataStringToStrWksp
         sta     $40
         jsr     L64B2
@L949C:  clc
         lda     DP_BAS_TXTPTR_OFF
         adc     DP_BAS_TXTPTR
         sta     DP_BAS_DATA_PTR
         lda     DP_BAS_TXTPTR+1
         adc     #$00
         sta     DP_BAS_DATA_PTR+1
         lda     DP_BAS_TXTPTR+2
         adc     #$00
         sta     DP_BAS_DATA_PTR+2
         bra     L9474

@L94B1:  jsr     L66AB
         lda     DP_BAS_DATA_PTR
         sta     DP_BAS_TXTPTR
         lda     DP_BAS_DATA_PTR+1
         sta     DP_BAS_TXTPTR+1
         lda     DP_BAS_DATA_PTR+2
         sta     DP_BAS_TXTPTR+2
         stz     DP_BAS_TXTPTR_OFF
         jsr     parse_skip_space_CMPcomma
         beq     L9538
         cmp     #$dc
         beq     L9538
         cmp     #$0d
         beq     @L94D8
@L94CF:  jsr     parse_skip_space_CMPcomma
         beq     L9538
         cmp     #$0d
         bne     @L94CF
@L94D8:  lda     DP_BAS_TXTPTR
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
@L94EF:  iny
         lda     [DP_BAS_TXTPTR],y
         cmp     #$2e
         bne     @L94FA
         iny
         jsr     varScanNameAtTMP6_Y
@L94FA:  cmp     #$20
         beq     @L94EF
         cmp     #$dc
         beq     L9534
         pla
         clc
         adc     DP_BAS_TXTPTR
         sta     DP_BAS_TXTPTR
         bcc     @L94D8
         inc     DP_BAS_TXTPTR+1
         bne     @L94D8
         inc     DP_BAS_TXTPTR+2
         bra     @L94D8

brk_2a_out_of_DATA:
         brk     $2a

         .byte   "Out of "
         .byte   $dc        ;DATA

brk_2b_No_REPEAT:
         brk     $2b

         .byte   "No "
         .byte   $f5        ;REPEAT

brk2d_MissingHash:
         brk     $2d

         .byte   $8d
         .byte   $23

L9526:   brk     $2c

         .byte   "Too many "
         .byte   $f5
         .byte   $73
         .byte   $00

L9534:   plx
         iny
         sty     DP_BAS_TXTPTR_OFF
L9538:   rts

exec_UNTIL:
         jsr     L7527
         jsr     L7282
         jsr     L6C8C
         ldy     DP_BAS_REPEAT_LVL
         beq     brk_2b_No_REPEAT
         lda     DP_BAS_INT_WA
         ora     DP_BAS_INT_WA+1
         ora     DP_BAS_INT_WA+2
         ora     DP_BAS_INT_WA+3
         beq     @L9559
         dec     DP_BAS_REPEAT_LVL
         dec     DP_BAS_REPEAT_LVL
         dec     DP_BAS_REPEAT_LVL
         jmp     parse_EOS_TXTPTR2

@L9559:  dey
         lda     [DP_BAS_REPEAT_STACKBASE],y
         sta     DP_BAS_TXTPTR2+2
         dey
         lda     [DP_BAS_REPEAT_STACKBASE],y
         sta     DP_BAS_TXTPTR2+1
         dey
         lda     [DP_BAS_REPEAT_STACKBASE],y
         sta     DP_BAS_TXTPTR2
         jmp     L6364

L956B:   dec     DP_BAS_TXTPTR2_OFF
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
         beq     @L958C
         bcc     brk2d_MissingHash

         .byte   $c6
         .byte   $32
         .byte   $60

@L958C:  jsr     eval_ret_INT
         ldy     DP_BAS_INT_WA
         tya
         clc
         rts

exec_REPEAT:
         ldx     DP_BAS_REPEAT_LVL
         cpx     #$3c
         bcs     L9526
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
         jmp     L6364

L95B3:   lda     DP_BAS_STRWKSP_L+2
         phb
         pha
         lda     DP_BAS_STRWKSP_L
         sta     DP_BAS_TMP6
         lda     DP_BAS_STRWKSP_L+1
         bra     L95C9

L95BF:   lda     DP_BAS_MEMBASE+2
         phb
         pha
         lda     DP_BAS_MEMBASE
         sta     DP_BAS_TMP6
         lda     DP_BAS_MEMBASE+1
L95C9:   sta     DP_BAS_TMP6+1
         pla
         sta     DP_BAS_TMP6+2
         stz     DP_BAS_TMP6+3
         lda     #$ee
         sta     DP_BAS_TMP6+4
         stz     DP_BAS_TMP6+5
         lda     #$20
         tsb     DP_BAS_CO_FLAGS
         bne     @L95E0
         stz     $5b
         stz     DP_FPB_sgn
@L95E0:  stz     $5d
         stz     DP_FPB_exp
         stz     DP_FPB_mant
         stz     DP_FPB_mant+1
@L95E8:  lda     #$00
         xba
         lda     #$55
         cop     $24
         ldy     #$00
         cop     $22
         plb
         bcc     @L9619
         rep     #$30
         .a16
         .i16
         lda     #$0001
         ldx     #$0002
         jsl     _EV
         sep     #$30
         .a8
         .i8
         bcs     @L9616
         lda     #$03
         cop     $16
         bit     DP_BAS_CO_FLAGS
         bpl     @L9613
         jsr     cmp_ERRORPTRisDEFAULT
         beq     @L9613
@L9613:  phb
         bra     @L95E8

@L9616:  brl     brk_11_Escape

@L9619:  stz     DP_BAS_COUNT
         lda     #$20
         trb     DP_BAS_CO_FLAGS
         ldy     $5b
         lda     #$00
         rts

PrintCRLFresetCOUNT:
         jsr     printCRLF
COUNTeq0:
         stz     DP_BAS_COUNT
         rts

L962A:   jsr     prog_search_lineno
         bcc     L969E
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
@L9652:  ldy     #$00
@L9654:  lda     [DP_BAS_TMP6],y
         sta     [DP_BAS_TOP],y
         cmp     #$0d
         bne     @L9677
         iny
         bne     @L966B
         inc     DP_BAS_TMP6+1
         bne     @L9665
         inc     DP_BAS_TMP6+2
@L9665:  inc     DP_BAS_TOP+1
         bne     @L966B
         inc     DP_BAS_TOP+2
@L966B:  lda     [DP_BAS_TMP6],y
         sta     [DP_BAS_TOP],y
         bmi     @L9688
         jsr     @L968B
         jsr     @L968B
@L9677:  iny
         bne     @L9654
         inc     DP_BAS_TMP6+1
         bne     @L9680
         inc     DP_BAS_TMP6+2
@L9680:  inc     DP_BAS_TOP+1
         bne     @L9654
         inc     DP_BAS_TOP+2
         bra     @L9654

@L9688:  jmp     L9AC2

@L968B:  iny
         bne     @L969A
         inc     DP_BAS_TOP+1
         bne     @L9694
         inc     DP_BAS_TOP+2
@L9694:  inc     DP_BAS_TMP6+1
         bne     @L969A
         inc     DP_BAS_TMP6+2
@L969A:  lda     [DP_BAS_TMP6],y
         sta     [DP_BAS_TOP],y
L969E:   rts

L969F:   ldx     #$ff
         stx     DP_BAS_OPT
         stx     $5d
         jsr     L97FD
         lda     DP_BAS_TXTPTR2
         sta     DP_BAS_TMP6
         lda     DP_BAS_TXTPTR2+1
         sta     DP_BAS_TMP6+1
         lda     DP_BAS_TXTPTR2+2
         sta     DP_BAS_TMP6+2
         stz     DP_FPB_sgn
         stz     DP_BAS_TXTPTR2_OFF
         jsr     L6063
         jsr     parse_lineno_atOFF
         bcc     L969E
L96C0:   lda     DP_BAS_LISTO
         beq     @L96CC
@L96C4:  lda     [DP_BAS_MEMBASE],y
         iny
         cmp     #$20
         beq     @L96C4
         dey
@L96CC:  tya
         clc
         adc     DP_BAS_MEMBASE
         sta     $5b
         lda     DP_BAS_MEMBASE+1
         adc     #$00
         sta     DP_FPB_sgn
         lda     DP_BAS_MEMBASE+2
         adc     #$00
         sta     $5d
         jsr     L962A
         ldy     #$00
         lda     #$0d
         cmp     [$5b]
         beq     L969E
@L96E9:  iny
         cmp     [$5b],y
         bne     @L96E9
         lda     #$20
@L96F0:  dey
         beq     @L96F7
         cmp     [$5b],y
         beq     @L96F0
@L96F7:  iny
         lda     #$0d
         sta     [$5b],y
         iny
         iny
         iny
         iny
         sty     DP_FPB_mant+2
         lda     DP_BAS_TOP
         sta     DP_BAS_TMP6+3
         lda     DP_BAS_TOP+1
         sta     DP_BAS_TMP6+4
         lda     DP_BAS_TOP+2
         sta     DP_BAS_TMP6+5
         jsr     L9AC1
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
         jsr     do_END
         jsr     int_CLEAR
         brk     $00

         .byte   $86
         .byte   " space"
         .byte   $00

@L973A:  lda     [DP_BAS_TMP6+3],y
         sta     [DP_BAS_TMP6],y
         tya
         bne     @L975B
         sec
         lda     DP_BAS_TMP6+4
         sbc     #$01
         sta     DP_BAS_TMP6+4
         lda     DP_BAS_TMP6+5
         sbc     #$00
         sta     DP_BAS_TMP6+5
         sec
         lda     DP_BAS_TMP6+1
         sbc     #$01
         sta     DP_BAS_TMP6+1
         lda     DP_BAS_TMP6+2
         sbc     #$00
         sta     DP_BAS_TMP6+2
@L975B:  sec
         dey
         phy
         tya
         ldx     DP_BAS_TMP6+4
         ldy     DP_BAS_TMP6+5
         adc     DP_BAS_TMP6+3
         bcc     @L976B
         inx
         bne     @L976B
         iny
@L976B:  cmp     DP_FPB_exp
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
         lda     DP_FPB_mant+2
         sta     [DP_FPB_exp],y
         sec
         tya
         adc     DP_FPB_exp
         sta     DP_FPB_exp
         bcc     @L9794
         inc     DP_FPB_mant
         bne     @L9794
         inc     DP_FPB_mant+1
@L9794:  ldy     #$ff
@L9796:  iny
         lda     [$5b],y
         sta     [DP_FPB_exp],y
         cmp     #$0d
         bne     @L9796
         rts

int_CLEAR:
         stz     DP_BAS_LOMEM
         lda     $95
         sta     DP_BAS_LOMEM+1
         lda     $96
         sta     DP_BAS_LOMEM+2
         lda     DP_BAS_TOP
         cmp     #$00
         lda     DP_BAS_TOP+1
         sbc     $95
         lda     DP_BAS_TOP+2
         sbc     $96
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
         jsr     L97FD
clearVARS:
         ldy     #VAR_OFFS_END-1
         lda     #$00
@lp:     sta     [DP_BAS_VARS_BASE],y
         dey
         bne     @lp
         sta     [DP_BAS_VARS_BASE] ;TODO: pointless? optimize away?
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

L97FD:   lda     DP_BAS_PAGE+1
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
         ldx     #DP_BAS_PRINT_FP_PTR
         ldy     #$08
         jsr     mem_add_16bconsts_to_3b_pointers
         stz     DP_BAS_REPEAT_LVL
         stz     DP_BAS_GOSUB_LVL
         lda     DP_BAS_PAGE
         sta     DP_BAS_DATA_PTR
         jsr     L9132
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

stack_REAL:
         lda     DP_BAS_STACK
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
         beq     storeStringOnStack
         bmi     stack_REAL
stack_INTWA:
         lda     DP_BAS_STACK
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

storeStringOnStack:
         clc                ;TODO: use MVN
         lda     DP_BAS_STACK
         sbc     DP_BAS_STRLEN
         jsr     UpdStackToACyCheckFull
         ldy     DP_BAS_STRLEN
         beq     @sk
@lp:     dey
         lda     [DP_BAS_STRWKSP_L],y
         iny
         sta     [DP_BAS_STACK],y
         dey
         bne     @lp
@sk:     lda     DP_BAS_STRLEN
         sta     [DP_BAS_STACK]
         rts

L98B8:   lda     DP_BAS_TMP6+3
         cmp     #$80
         beq     @L98EB
         bcc     @L9900
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
         sta     DP_BAS_TMP6+5
         dey
@L98DB:  lda     [DP_BAS_STACK],y
         sta     [DP_BAS_TMP6+3],y
         iny
         dex
         bne     @L98DB
@L98E3:  lda     [DP_BAS_STACK]
         ldy     #$04
@L98E7:  sta     [DP_BAS_TMP6],y
         bra     L9937

@L98EB:  lda     [DP_BAS_STACK]
         tax
         beq     @L98FC
         ldy     #$01
@L98F2:  lda     [DP_BAS_STACK],y
         dey
         sta     [DP_BAS_TMP6],y
         iny
         iny
         dex
         bne     @L98F2
@L98FC:  lda     #$0d
         bne     @L98E7

@L9900:  lda     [DP_BAS_STACK]
         sta     [DP_BAS_TMP6]
         ldy     #$04
         lda     DP_BAS_TMP6+3
         beq     @L9924
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
         bcs     @L9924
         lda     [DP_BAS_STACK],y
         sta     [DP_BAS_TMP6],y
         iny
@L9924:  tya
         clc
         bra     stack_ADD_A

L9928:   lda     [DP_BAS_STACK]
         sta     DP_BAS_STRLEN
         beq     L9939
         tay
@L992F:  lda     [DP_BAS_STACK],y
         dey
         sta     [DP_BAS_STRWKSP_L],y
         tya
         bne     @L992F
L9937:   lda     [DP_BAS_STACK]
L9939:   sec
         bra     stack_ADD_A

L993C:   ldy     #$03
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
stack_ADD4:
         clc
         lda     #$04
stack_ADD_A:
         adc     DP_BAS_STACK
         sta     DP_BAS_STACK
         bcc     @L995F
         inc     DP_BAS_STACK+1
         bne     @L995F
         inc     DP_BAS_STACK+2
@L995F:  rts

stack_copy6_to_DP_BAS_TMP66_uns4:
         ldx     #DP_BAS_TMP6
stack_copy6_to_X_uns4:
         ldy     #$05
         lda     [DP_BAS_STACK],y
         sta     $05,x
         dey
         lda     [DP_BAS_STACK],y
         sta     $04,x
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
@rts:    rts

clrIntBrkNoRoom:
         jsr     int_CLEAR
jmpBrkNoRoom:
         jmp     brk_00_NoRoom

PrintChOrToken:
         sta     DP_BAS_TMP6
         cmp     #$80
         bcc     printChOrSendtoLOMEMandINC
         lda     #<tblTokensAsc
         sta     DP_BAS_TMP6+1
         lda     #>tblTokensAsc
         sta     DP_BAS_TMP6+2
         phy
@lpNextToken:
         ldy     #$00
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

@TokenFound:
         ldy     #$00
@PrTokLp:
         lda     (DP_BAS_TMP6+1),y
         bmi     @skTokEnd
         jsr     printChOrSendtoLOMEMandINC
         iny
         bne     @PrTokLp
@skTokEnd:
         ply
         rts

L99E2:   pha
         lsr     A
         lsr     A
         lsr     A
         lsr     A
         jsr     @L99ED
         pla
         and     #$0f
@L99ED:  cmp     #$0a
         bcc     @L99F3
         adc     #$06
@L99F3:  adc     #$30
printChDoWIDTHformat:
         pha
         lda     DP_BAS_WIDTH
         cmp     DP_BAS_COUNT
         bcs     PrintChIncCOUNT
         jsr     PrintCRLFresetCOUNT
PrintChIncCOUNT:
         pla
         inc     DP_BAS_COUNT
         jmp     call_OSWRCH

L9A05:   jsr     L99E2
printSpcOrSendToLOMEMandINC:
         lda     #$20
printChOrSendtoLOMEMandINC:
         bit     DP_BAS_LISTO
         bmi     L9A18
printCh: cmp     #$0d
         bne     printChDoWIDTHformat
         jsr     call_OSWRCH
         jmp     COUNTeq0

L9A18:   sta     [DP_BAS_VARTOP]
         inc     DP_BAS_VARTOP
         bne     L9A48
         inc     DP_BAS_VARTOP+1
         bne     @L9A24
         inc     DP_BAS_VARTOP+2
@L9A24:  pha
         lda     DP_BAS_VARTOP+2
         eor     DP_BAS_HIMEM+2
         bne     @sk
         lda     DP_BAS_VARTOP+1
         eor     DP_BAS_HIMEM+2
         bne     @sk
         brl     clrIntBrkNoRoom

@sk:     pla
         rts

L9A36:   clc
L9A37:   and     DP_BAS_LISTO
         beq     L9A48
         txa
         bmi     L9A48
         rol     A
         tax
         beq     L9A48
L9A42:   jsr     printSpcOrSendToLOMEMandINC
         dex
         bne     L9A42
L9A48:   rts

L9A49:   lda     DP_BAS_INT_WA
         sta     $00,x
         lda     DP_BAS_INT_WA+1
         sta     $01,x
         lda     DP_BAS_INT_WA+2
         sta     $02,x
         lda     DP_BAS_INT_WA+3
         sta     $03,x
         rts

L9A5A:   jmp     brk_00_NoRoom

L9A5D:   lda     DP_BAS_PAGE
         cmp     #$00
         lda     DP_BAS_PAGE+1
         sbc     $95
         lda     DP_BAS_PAGE+2
         sbc     $96
         bcc     L9A5A
         lda     DP_BAS_PAGE
         cmp     DP_BAS_MEMEND
         lda     DP_BAS_PAGE+1
         sbc     DP_BAS_MEMEND+1
         lda     DP_BAS_PAGE+2
         sbc     DP_BAS_MEMEND+2
         bcs     L9A5A
         jsr     L9B0B
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
         lda     #$0055
         cop     $24
         cop     $4e
         jsr     L7FFA
         plb
         sep     #$30
         .a8
         .i8
do_END:  lda     DP_BAS_PAGE+2
         sta     DP_BAS_TOP+2
         lda     DP_BAS_PAGE+1
         sta     DP_BAS_TOP+1
         lda     DP_BAS_PAGE
         sta     DP_BAS_TOP
         ldy     #$01
@L9AAA:  lda     [DP_BAS_TOP]
         cmp     #$0d
         bne     L9AD2
         lda     [DP_BAS_TOP],y
         bmi     @L9AC0
         ldy     #$03
         lda     [DP_BAS_TOP],y
         beq     L9AD2
         clc
         jsr     L9AC3
         bra     @L9AAA

@L9AC0:  iny
L9AC1:   clc
L9AC2:   tya
L9AC3:   adc     DP_BAS_TOP
         sta     DP_BAS_TOP
         bcc     @L9ACF
         inc     DP_BAS_TOP+1
         bne     @L9ACF
         inc     DP_BAS_TOP+2
@L9ACF:  ldy     #$01
         rts

L9AD2:   jsr     L9C22
         ora     L6141+1
         stz     DP_BAS_TXTPTR2+1
         bvs     L9B4E
         adc     $617267
         adc     $ea0d
         stz     DP_BAS_ARG2_FLAG
         jmp     reset_prog_prompt

jmp_brk06_type_mismatch2:
         jmp     brk06_type_mismatch

L9AEB:   lda     DP_BAS_STRWKSP_L
         sta     DP_BAS_TMP6
         lda     DP_BAS_STRWKSP_L+1
         sta     DP_BAS_TMP6+1
         lda     DP_BAS_STRWKSP_L+2
         sta     DP_BAS_TMP6+2
         stz     DP_BAS_TMP6+3
L9AF9:   ldy     DP_BAS_STRLEN
         lda     #$0d
         sta     [DP_BAS_STRWKSP_L],y
         rts

L9B00:   jsr     L7527
         bne     jmp_brk06_type_mismatch2
         jsr     L9AEB
         jmp     L7282

L9B0B:   jsr     L9B00
         lda     DP_BAS_PAGE
         sta     DP_FPB_mant+2
         lda     DP_BAS_PAGE+1
         sta     DP_FPB_mant+3
         lda     DP_BAS_PAGE+2
         sta     DP_FPB_mant+4
         stz     DP_BAS_TMP64
         rts

L9B1D:   ldx     DP_BAS_PAGE+2
         stx     $5b
         ldy     #$00
         sty     DP_FPB_sgn
         rts

exec_SAVE:
         jsr     do_END
         jsr     L9B0B
         rep     #$30
         .a16
         .i16
         stz     DP_BAS_TMP6+4
         stz     $5b
         stz     $5d
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
         lda     #$0055
         cop     $24
         cop     $4f
         .a8
         .i8
L9B4E:   jsr     L7FFA
         plb
         sep     #$30
         bra     L9BAD

exec_OSCLI:
         jsr     L9B00
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
         cop     $0e
         jsr     setREPORT_BHA_Cy
         sep     #$30
         .a8
         .i8
         plb
         bra     L9BAD

L9B6F:   jsr     parse_fileHandleHash_PTR2
         pei     (DP_BAS_INT_WA)
         jsr     parse_expectEQ_PTR_OFF
         jsr     L6C8C
         rep     #$30
         .a16
         .i16
         ply
         phb
         lda     #$0048
         cop     $24
         cop     $58
         jsr     setREPORT_BHA_Cy
         plb
         sep     #$30
         .a8
         .i8
         brl     L9BAD

exec_PTRc:
         jsr     parse_fileHandleHash_PTR2
         pei     (DP_BAS_INT_WA)
         jsr     parse_expectEQ_PTR_OFF
         jsr     L6C8C
         rep     #$30
         .a16
         .i16
         ply
         phb
         lda     #$0048
         cop     $24
         cop     $55
         jsr     setREPORT_BHA_Cy
         plb
         sep     #$30
         .a8
         .i8
         brl     L9BAD

L9BAD:   jmp     parse_EOS_TXTPTR2

exec_CLOSE:
         jsr     parse_fileHandleHash_PTR2
         jsr     parse_nextstmt_yield_PTR2_OFF
         rep     #$30
         .a16
         .i16
         ldy     DP_BAS_INT_WA
         phb
         cop     $45
         jsr     setREPORT_BHA_Cy
         plb
         sep     #$30
         .a8
         .i8
         bra     L9BAD

exec_BPUT:
         jsr     parse_fileHandleHash_PTR2
         jsr     stack_INTWA
         jsr     parse_skip_spaces_CMPcommaBRK
         jsr     L7537
         bne     @L9C06
         jsr     parse_skip_spaces
         eor     #$3b
         cmp     #$01
         php
         bcc     @L9BDF
         dec     DP_BAS_TXTPTR_OFF
@L9BDF:  jsr     parse_nextstmt_yield_PTR2_OFF
         jsr     L993C
         ldy     #$00
         bra     @L9BFB

@L9BE9:  lda     [DP_BAS_STRWKSP_L],y
         phy
         rep     #$10
         .i16
         ldy     DP_BAS_INT_WA
         phb
         cop     $0b
         jsr     setREPORT_BHA_Cy
         plb
         sep     #$10
         .i8
         ply
         iny
@L9BFB:  cpy     DP_BAS_STRLEN
         bcc     @L9BE9
         plp
         bcc     L9BAD
         lda     #$0d
         bra     @L9C13

@L9C06:  jsr     L6C91
         jsr     parse_nextstmt_yield_PTR2_OFF
         lda     DP_BAS_INT_WA
         pha
         jsr     L993C
         pla
@L9C13:  rep     #$10
         .i16
         ldy     DP_BAS_INT_WA
         phb
         cop     $0b
         jsr     setREPORT_BHA_Cy
         plb
         sep     #$10
         .i8
         bra     L9BAD

L9C22:   pla
         sta     DP_BAS_TMP6
         pla
         sta     DP_BAS_TMP6+1
         phk
         pla
         sta     DP_BAS_TMP6+2
         bra     @L9C31

@L9C2E:  jsr     L9C84
@L9C31:  jsr     L605A
         bpl     @L9C2E
         phk
         pei     (DP_BAS_TMP6)
         php
         rti

         .byte   $a9
         .byte   $05
         .byte   $da
         .byte   $a2
         .byte   $48
         .byte   $a0
         .byte   $00
         .byte   $20
         .byte   $76
         .byte   $9c
         .byte   $fa
         .byte   $a5
         .byte   $4c

INT_inc_WA:
         inc     DP_BAS_INT_WA
         bne     @L9C56
         inc     DP_BAS_INT_WA+1
         bne     @L9C56
         inc     DP_BAS_INT_WA+2
         bne     @L9C56
         inc     DP_BAS_INT_WA+3
@L9C56:  rts

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

call_OSBYTE:
         cop     $06
         rts

cop_OSWORD:
         cop     $07
         rts

printCRLF:
         lda     #$0a
         jsr     call_OSWRCH
         lda     #$0d
         jsr     call_OSWRCH
         rts

L9C84:   jsr     call_OSWRCH
         cmp     #$0d
         bne     @L9C8E
         jsr     printCRLF
@L9C8E:  rts

call_OSRDCH:
         phx
         phy
@L9C91:  cop     $04        ;OSRDCH
         bcc     @L9CA2
         cmp     #$1b
         beq     @L9CA2
         jsr     cmp_ERRORPTRisDEFAULT
         lda     #$03
         cop     $16        ;Ack non-escape error
         bra     @L9C91

@L9CA2:  ply
         plx
         rts

call_OSWRCH:
         pha
         phy
         phx
         cop     $00
         plx
         ply
         pla
         rts

         .byte   $48
         .byte   $b5
         .byte   $01
         .byte   $d0
         .byte   $0f
         .byte   $74
         .byte   $02
         .byte   $18
         .byte   $7b
         .byte   $75
         .byte   $00
         .byte   $95
         .byte   $00
         .byte   $eb
         .byte   $75
         .byte   $01
         .byte   $95
         .byte   $01
         .byte   $68
         .byte   $60
         .byte   $8b
         .byte   $68
         .byte   $95
         .byte   $02
         .byte   $68
         .byte   $60
; This appears to be a linked list of @...% variables
tblBuiltInsLL:
         .faraddr biUSEB
         .byte   "USED%"
         .byte   $00
         .word   $0000
         .word   $0200
biUSEB:  .faraddr biWRDAM
         .byte   "USEB%"
         .byte   $00
         .word   $0000
         .word   $8000
biWRDAM: .faraddr biWRDXY
         .byte   "WRDAM%"
         .byte   $00
         .word   $0000
         .word   $2000
biWRDXY: .faraddr biWRD
         .byte   "WRDXY%"
         .byte   $00
         .word   $0000
         .word   $1000
biWRD:   .faraddr biNONE
         .byte   "WRD%"
         .byte   $00
         .byte   $00
         .byte   $00
         .byte   $00
         .byte   $30
biNONE:  .faraddr biA
         .byte   $00
         .byte   $00
         .byte   $00
         .byte   $00
biA:     .faraddr biX
         .byte   "A%"
         .byte   $00
         .byte   $74
         .byte   $00
         .byte   $00
         .byte   $00
biX:     .faraddr biY
         .byte   "X%"
         .byte   $00
         .byte   $78
         .byte   $00
         .byte   $00
         .byte   $00
biY:     .faraddr biD
         .byte   "Y%"
         .byte   $00
         .byte   $7c
         .byte   $00
         .byte   $00
         .byte   $00
biD:     .faraddr biB
         .byte   "D%"
         .byte   $00
         .byte   $80
         .byte   $00
         .byte   $00
         .byte   $00
biB:     .faraddr biP
         .byte   "B%"
         .byte   $00
         .byte   $84
         .byte   $00
         .byte   $00
         .byte   $00
biP:     .faraddr biC
         .byte   "P%"
         .byte   $00
         .byte   $88
         .byte   $00
         .byte   $00
         .byte   $00
biC:     .faraddr biOPT
         .byte   "C%"
         .byte   $00
         .byte   $8c
         .byte   $00
         .byte   $00
         .byte   $00
biOPT:   .faraddr biBHA
         .byte   "OPT%"
         .byte   $00
         .byte   $43
         .byte   $00
         .byte   $00
         .byte   $00
biBHA:   .faraddr biDATA
         .byte   "BHA%"
         .byte   $00
         .byte   $c2
         .byte   $00
         .byte   $00
         .byte   $00
biDATA:  .faraddr biERC
         .byte   "DATA%"
         .byte   $00
         .byte   $33
         .byte   $00
         .byte   $00
         .byte   $00
biERC:   .faraddr biERX
         .byte   "ERC%"
         .byte   $00
         .byte   $00
         .byte   $00
         .byte   $00
         .byte   $00
biERX:   .faraddr biERRMSG
         .byte   "ERX%"
         .byte   $00
         .byte   $04
         .byte   $00
         .byte   $00
         .byte   $00
biERRMSG:
         .faraddr biEndOfList
         .byte   "ERMSG%"
         .byte   $00
         .byte   $08
         .byte   $00
         .byte   $00
         .byte   $00
biEndOfList:
         .byte   $00
         .byte   $00
         .byte   $00
arith_get_reference:
         ldx     #DP_BAS_ARITHMODREF
         ldy     #$00
         cop     COP_26_OPBHA ;get address of "ARITHMETIC"
         .byte   "ARITHMETIC"
         .byte   $00

         cop     COP_29_OPRFR ;Get reference to Arithmetic module
         bcc     @L9DC6     ;all's well return
         lda     #<brk_0_NoArith-1 ;?? Bodge the Arith block to reference the BRK
         sta     DP_BAS_ARITHMODREF ;and fail when any arithmetic called?
         lda     #>brk_0_NoArith
         sta     DP_BAS_ARITHMODREF+1
         phk
         pla
         sta     DP_BAS_ARITHMODREF+2
@L9DC6:  rts

brk_0_NoArith:
         .byte   $00
         .byte   $00
         .byte   "Cannot find ARITHMETIC module"
         .byte   $00

moduleCallARITHref:
         pei     (DP_BAS_ARITHMODREF+4)
         pei     (DP_BAS_ARITHMODREF+2)
         pei     (DP_BAS_ARITHMODREF)
         rtl

         .byte   $ea
         .byte   $5f
         .byte   $d4
         .byte   $ff
