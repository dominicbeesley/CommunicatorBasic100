
		.import	__SHIMS_SIZE__, __SHIMS_RUN__, __SHIMS_LOAD__
		.import	__NATVEC_SIZE__, __NATVEC_RUN__, __NATVEC_LOAD__

                .global BANK0_OSWORD_BLOCK : far
                .global BANK0_SCRATCH_PAGE : far
                .global NATVEC_BRK	   : far


OSBYTE				:= $00FFF4
OSWRCH				:= $00FFEE
OSWORD				:= $00FFF1
JIM				:= $00FD00
fred_JIM_PAGE_HI		:= $00FCFD
fred_JIM_PAGE_LO		:= $00FCFE
fred_JIM_DEVNO			:= $00FCFF

zp_mos_jimdevsave		:= $EE

JIM_DEVNO_BLITTER		:= $D1

	;TODO where to put these
NATVEC_BRK			:= $002000
BANK0_OSWORD_BLOCK		:= $002080
BANK0_SCRATCH_PAGE		:= $002100

MOS_ZP_ESC_FLAG			:= $0000FF