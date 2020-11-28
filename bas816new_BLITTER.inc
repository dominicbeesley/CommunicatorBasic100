
		.import	__SHIMS_SIZE__, __SHIMS_RUN__, __SHIMS_LOAD__
		.import	__NATVEC_SIZE__, __NATVEC_RUN__, __NATVEC_LOAD__



;=========================================================================
; BLITTER specific Defines
;=========================================================================

BLITTER_BASIC_DP		:= $001900
BLITTER_ARITH_DP		:= $001A00
NATVEC_BRK			:= $002000
BANK0_OSWORD_BLOCK		:= $002080
BANK0_SCRATCH_PAGE		:= $002100


BLITTER_ARITH_WKSPC	:=	$01FD00		; just below page
BLITTER_BASIC_MEMBASE	:=	$020000		; next bank after basic TODO: allocate/calculate
BLITTER_BASIC_MEMSZ	:=	$080000		; 8 banks

OSBYTE				:= $00FFF4
OSWRCH				:= $00FFEE
OSWORD				:= $00FFF1
JIM				:= $00FD00
fred_JIM_PAGE_HI		:= $00FCFD
fred_JIM_PAGE_LO		:= $00FCFE
fred_JIM_DEVNO			:= $00FCFF

zp_mos_jimdevsave		:= $EE

JIM_DEVNO_BLITTER		:= $D1


MOS_ZP_ESC_FLAG			:= $0000FF

sheila_MEM_LOMEMTURBO		:= $FE37
