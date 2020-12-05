
		.import	__SHIMS_SIZE__, __SHIMS_RUN__, __SHIMS_LOAD__
		.import	__NATVEC_SIZE__, __NATVEC_RUN__, __NATVEC_LOAD__



;=========================================================================
; BEEB816 specific Defines
;=========================================================================

HWVEC_6502_IRQ  := $00FFFE

MOS_BASIC_DP			:= $001900
MOS_ARITH_DP			:= $001A00
NATVEC_BRK			:= $002000
NATVEC_BRK_EMU			:= $002003
BANK0_OSWORD_BLOCK		:= $002080
BANK0_SCRATCH_PAGE		:= $002100


MOS_ARITH_WKSPC			:=	$F8FD00		; just below page
MOS_BASIC_MEMBASE		:=	$F90000		; next bank after basic TODO: allocate/calculate
MOS_BASIC_MEMSZ			:=	$030000		; 3 banks

OSBYTE				:= $00FFF4
OSWRCH				:= $00FFEE
OSWORD				:= $00FFF1
OSCLI				:= $00FFF7
OSBGET				:= $00FFD7
OSFIND				:= $00FFCE

OSFILE_OPENIN			:= $40

BRKV				:= $000202

JIM				:= $00FD00
fred_JIM_PAGE_HI		:= $00FCFD
fred_JIM_PAGE_LO		:= $00FCFE
fred_JIM_DEVNO			:= $00FCFF

MOS_ZP_ESC_FLAG			:= $0000FF
