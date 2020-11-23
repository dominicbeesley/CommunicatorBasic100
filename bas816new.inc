
;=========================================================================
; Communicator specific Defines
;=========================================================================


	.IFDEF COMMUNICATOR

_CCO            =       $ff94                           ;Call coroutine
_CKREAD         =       6
_CO             =       $ff90
_COBRK          =       $0c
_COCRE          =       2
_CODEL          =       6                               ;Delete a coroutine
_COENV          =       4
_CWT            =       $ff98
_EV             =       $ffb0
_MM             =       $ff8c
_MMALA          =       $0c                             ;Allocate large ascending
_ST             =       $ff88
_STBMB          =       $20                             ;?
_STSBK          =       $14                             ;Get BRK signature
_STTBK          =       $16                             ;Get BRK text

COP_00_OPWRC    =       0
COP_02_OPWRA    =       $02
COP_03_OPNLI    =       $03
COP_04_OPRDC    =       4
COP_06_OPOSB    =       $06
COP_07_OPOSW    =       7
COP_0B_OPBPT    =       $0b
COP_0E_OPCOM    =       $0e
COP_0F_OPERR    =       $0f
COP_11_OPADF    =       $11                             ;Allocate a DP
COP_16_OPAEV    =       $16
COP_1A_OPFZB    =       $1a
COP_21_OPPRE    =       $21
COP_22_OPRLN    =       $22
COP_24_OPCVD    =       $24                             ;Convert direct page ptr to long pointer
COP_26_OPBHA    =       $26                             ;Get string banked address
COP_28_OPCMD    =       $28                             ;Call module inline name
COP_29_OPRFR    =       $29
COP_2C_OPFMA    =       $2c
COP_2E_OPFPO    =       $2e                             ;Find pool owner
COP_44_OPOPN    =       $44
COP_45_OPCLS    =       $45
COP_46_OPEND    =       $46
COP_4E_OPLOD    =       $4e
COP_4F_OPSAV    =       $4f
COP_54_OPRSP    =       $54                             ;Get PTR#
COP_55_OPWSP    =       $55
COP_57_OPRLL    =       $57                             ;Get EXT#
COP_58_OPWLL    =       $58                             ;Set EXT#
COP_5D_OPSTAR   =       $5d                             ;OSCLI?
COP_61_OPERC    =       $61                             ;Convert error number to string
COP_OPRLH       =       $18
COP_OSBGT       =       $0a                             ;Call OSBGET

	.ENDIF ;COMMUNICATOR

;=========================================================================
; ARITH module functions
;=========================================================================

ARITH_FN_0C_UK  =       $0c
ARITH_FN_ACS    =       $18
ARITH_FN_ASN    =       $1a
ARITH_FN_ATN    =       $1c
ARITH_FN_COS    =       $14
ARITH_FN_DEG    =       $1e
ARITH_FN_EXP    =       $26
ARITH_FN_LN     =       $24
ARITH_FN_LOG    =       $22
ARITH_FN_POWER  =       $10
ARITH_FN_RAD    =       $20
ARITH_FN_SIN    =       $12
ARITH_FN_SQR    =       $28
ARITH_FN_TAN    =       $16
FN_ARITH_INT    =       $30

;=========================================================================
; DIRECT PAGE
;=========================================================================


DB_BAS_UNK_5A_6 =       $5a
DP_ASS_ASS_FLAGS_UK =   $41
DP_BAS_00_REAL_UK =     0
DP_BAS_40_VARTYPE =     $40
DP_BAS_47_ASS_UK =      $47
DP_BAS_61_UK    =       $61
DP_BAS_6F_UK    =       $6f
DP_BAS_70_UK    =       $70
DP_BAS_ARG2     =       $9a
DP_BAS_ARG2_FLAG =      $99
DP_BAS_ARITHMODREF =    $c9
DP_BAS_C7_UK    =       $c7
	.IFDEF COMMUNICATOR
DP_BAS_CO_CTDN  =       $a0                             ;Seems to count down as statements are execed and then yield every 100th?
DP_BAS_CO_FLAGS =       $97                             ;Seems to be a set of flags to decide what state we're in and whether to yield? - bit $40 set disables yeild
	.ENDIF
DP_BAS_COUNT    =       $37
DP_BAS_CURCHAN  =       $90
DP_BAS_DATA_PTR =       $33                             ;Unknown pointer, set to PAGE at start
DP_BAS_DATA_READ_FL =   $36
DP_BAS_ENDDP    =       $d5                             ;End of direct page
DP_BAS_ERL      =       $1c
DP_BAS_FOR_LVL  =       $3f
DP_BAS_FOR_STACKBASE =  $b3
DP_BAS_FP_PTR1  =       $6c
DP_BAS_FP_TMPEXP =      $6b
DP_BAS_FPTMPptr3 =      $b0                             ;Used in PRINT HASH
DP_BAS_GOSUB_LVL =      $3e
DP_BAS_GOSUB_STACKBASE = $b6
DP_BAS_HIMEM    =       $19
DP_BAS_INT_WA   =       $48                             ;c.f. 6502 2A
DP_BAS_INTVARSptr =     $aa
DP_BAS_LAST_EOSEOR_D_ = $c8                             ;Set to end of statement marker EOR D
DP_BAS_LISTO    =       $38
DP_BAS_LOMEM    =       $10
DP_BAS_LOMEM_LIM_PAG =  $95
DP_BAS_MEMBASE  =       $a4                             ;Base of workspace
DP_BAS_MEMEND   =       $92
DP_BAS_MEMSIZE  =       $9d
DP_BAS_OLD_DP   =       $d3
DP_BAS_ONERRORPTR =     $2c
DP_BAS_OPT      =       $42
DP_BAS_PAGE     =       $bf
DP_BAS_RAND     =       $22                             ;Random seed
DP_BAS_REPEAT_LVL =     $3d
DP_BAS_REPEAT_STACKBASE = $b9
DP_BAS_Report   =       $08
DP_BAS_SET_TO_Z_AT_EOS = $c6
	.IFDEF COMMUNICATOR
DP_BAS_SPECIAL_VARSptr = $bc                            ;Points at copies of PAGE, TOP, etc?
DP_BAS_SPECIALVAR_SZ =  $98
	.ENDIF
DP_BAS_STACK    =       $16                             ;Set when HIMEM is set
DP_BAS_STACKSAVE2 =     $d1
DP_BAS_STRLEN   =       $54
DP_BAS_STRWKSP_L =      $a7
DP_BAS_TMP2A    =       $2a
DP_BAS_TMP6     =       $55
DP_BAS_TMP64    =       $64
DP_BAS_TOP      =       $27
DP_BAS_TRACE_LIM =      $3a
DP_BAS_TRACEFLAG =      $39
DP_BAS_TXTPTR   =       $2f
DP_BAS_TXTPTR2  =       $1f
DP_BAS_TXTPTR2_OFF =    $1e
DP_BAS_TXTPTR_OFF =     $32
DP_BAS_VARS_BASE =      $ad                             ;Pointers to VARS?
DP_BAS_VARTOP   =       $13                             ;end of variables, above LOMEM, below STACK
DP_BAS_WIDTH    =       $3c
DP_FP_TMP       =       $65
DP_FPA_exp      =       $4d
DP_FPA_mant     =       $4f
DP_FPA_sgn      =       $4c
DP_FPB_exp      =       $5e
DP_FPB_mant     =       $5f
DP_FPB_sgn      =       $5c
DP_stack_save   =       $72

USR_RET_B       =       $84
USR_RET_B2      =       $88
USR_RET_BFLAG   =       $8c
USR_RET_C       =       $74
USR_RET_C2      =       $c2
USR_RET_D       =       $80
USR_RET_X       =       $78
USR_RET_Y       =       $7c


;=========================================================================
; USR/CALL flags (set in top byte of 32 bit address)
;=========================================================================
USR_02_USED     =       $02
USR_FLAGS_80_USEB =     $80


;=========================================================================
; Internal Variable types
;=========================================================================

RETV_INT        =       $40
RETV_REAL       =       $ff                             ;VarType = real
RETV_STR        =       0
RETV_UK1        =       1

;=========================================================================
; Dynamic Variable special offsets
;=========================================================================
VAR_OFFS_END    =       $c0                             ;Byte offset after end of variable linked list heads
VAR_OFFS_FN     =       $b4                             ;Offset if variable linked list pointer table for FNs
VAR_OFFS_PROC   =       $b1                             ;Offset in variable linked list pointer table for PROCs


;=========================================================================
; Single letter variable offsets
;=========================================================================

INTVAR_atsign   =       $00
INTVAR_A        =       $04
INTVAR_B        =       $08
INTVAR_D        =       $10
INTVAR_O        =       $3c
INTVAR_P        =       $40
INTVAR_X        =       $60
INTVAR_Y        =       $64

;=========================================================================
; Assembler OPT values
;=========================================================================

OPT_10_UK       =       $10
OPT_20_UK       =       $20
OPT_30_MASK     =       $30
OPT_40_UK       =       $40
OPT_8_UK        =       8                               ;Not sure what this does
OPT_8F_MASK     =       $8f

;=========================================================================
; MOS call arguments
;=========================================================================


OSBYTE_126_ESCAPE_ACK = $7e
OSBYTE_134_POS  =       134
OSBYTE_222_RW_VDUQLEN = $da
OSWORD_1_READTIME =     1
OSWORD_5_READ_IO_MEM =  5
OSWORD_7_SOUND  =       7
OSWORD_8_ENVELOPE =     8
OSWORD_9_POINT  =       9


;=========================================================================
; BASIC tokens
;=========================================================================

tknAND          =       $80
tknAUTO         =       $c6
tknDATA         =       $dc
tknDEF          =       $dd
tknDIM          =       $de
tknDIV          =       $81
tknELSE         =       $8b
tknEND          =       $e0
tknEOF          =       $c5
tknEOR          =       $82
tknERL          =       $9e
tknERROR        =       $85
tknEXT          =       $e2
tknFN           =       $a4
tknFOR          =       $e3
tknGOSUB        =       $e4
tknGOTO         =       $e5
tknIF           =       $e7
tknLINE         =       $86
tknLineNo       =       $8d                             ;line number marker
tknLOCAL        =       $ea
tknMissing      =       $8d
tknMOD          =       $83
tknMODE         =       $eb
tknNEXT         =       $ed
tknOFF          =       $87
tknON           =       $ee
tknOPENIN       =       $8e
tknOR           =       $84
tknPRINT        =       $f1
tknPROC         =       $f2
tknPTR          =       $cf
tknREM          =       $f4
tknREPEAT       =       $f5
tknREPORT       =       $f6
tknSPC          =       $89
tknSTEP         =       $88
tknTAB          =       $8a
tknTHEN         =       $8c
tknTO           =       $b8
tknUNTIL        =       $fd
tknWIDTH        =       $fe

;=========================================================================
; VDU codes
;=========================================================================

VDU_CLG         =       $10
VDU_CLS         =       12
VDU_COLOUR      =       $11
VDU_CURS_CHAR   =       $1f                             ;; PRINT TAB
VDU_GCOL        =       $12
VDU_MODE        =       22
VDU_PLOT        =       $19
