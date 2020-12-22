
		.import	__SHIMS_SIZE__, __SHIMS_RUN__, __SHIMS_LOAD__


;=========================================================================
; DOSSY TUBE specific Defines
;=========================================================================

HWVEC_6502_IRQ  := $00FFFE

MOS_BASIC_DP			:= $001900
MOS_ARITH_DP			:= $001A00
NATVEC_BRK			:= $002000
NATVEC_BRK_EMU			:= $002003
BANK0_OSWORD_BLOCK		:= $002080
BANK0_SCRATCH_PAGE		:= $002100


MOS_ARITH_WKSPC			:=	$02FD00		; just below page
MOS_BASIC_MEMBASE		:=	$030000		; next bank after basic TODO: allocate/calculate
MOS_BASIC_MEMSZ			:=	$030000		; 3 banks

OSBYTE				:= $00FFF4
OSWRCH				:= $00FFEE
OSWORD				:= $00FFF1
OSCLI				:= $00FFF7
OSRDCH				:= $00FFE0
OSBGET				:= $00FFD7
OSBPUT				:= $00FFD4
OSFIND				:= $00FFCE
OSARGS				:= $00FFDA
OSFILE				:= $00FFDD

OSFIND_OPENIN			:= $40
OSFIND_OPENOUT			:= $80

BRKV				:= $202

; this is in language part of MOS's DP, we require at least 4 bytes for OSARGS
MOS_ZP_TMP			:= $000000

; Escape flag must be read from MOS DP
MOS_ZP_ESC_FLAG			:= $0000FF
