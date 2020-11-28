




; This sets up interrupt shims at 0A00 in system memory and native vectors
; at ChipRAM 008Fxx and tests interrupt entry/exit by reenabling interrupts and calling 
; MOS functions

NATVEC_CHIP	:= $008FE0

HWVEC_6502_IRQ  := $00FFFE

		.assert __SHIMS_SIZE__ <= $100, error, "SHIMS size must be <=$100"
		.assert __NATVEC_SIZE__ = $10, error, "NATVEC size must be $10"

debug_hex_byte:
		.a8
		.i8

                pha
                lsr     A
                lsr     A
                lsr     A
                lsr     A
                jsr     @nyb
                pla
                and     #$0f
@nyb:
                cmp     #$0a
                bcc     @dig
                adc     #$06
@dig:
                adc     #'0'
                jmp     call_OSWRCH


BL_debug_print:
		lda	[DP_BAS_BL_DEBUGPTR]
		beq	@sk
		jsr	call_OSWRCH
		inc	DP_BAS_BL_DEBUGPTR
		bne	BL_debug_print
		inc	DP_BAS_BL_DEBUGPTR+1
		bne	BL_debug_print
@sk:		lda	#$0A
		jsr	call_OSWRCH
		lda	#$0D
		jmp	call_OSWRCH

		; we assume stack looks like a native interrupt i.e.
                ;               + 6             Program bank
                ;               + 5             PCH
                ;               + 4             PCL
                ;               + 3             Native mode flags
                ;		+ 2		Caller retH
                ;		+ 1		Caller retL

ShowRegs:	php				; save entry flags

                ;               + 7             Program bank
                ;               + 6             PCH
                ;               + 5             PCL
                ;               + 4             Native mode flags
                ;		+ 3		Caller retH
                ;		+ 2		Caller retL
                ;               + 1             Entry flags

		rep	#$30
		.a16
		.i16
		phb
		phd
		phy
		phx
		pha

		pea	BLITTER_BASIC_DP
		pld

                ;               + 16            Program bank
                ;               + 15            PCH
                ;               + 14            PCL
                ;               + 13            Native mode flags
                ;		+ 12		Caller retH
                ;		+ 11		Caller retL
                ;               + 10            Entry flags
                ;		+ 9		B
                ;		+ 7		DP
                ;		+ 5		Y (16)
                ;		+ 3		X (16)
                ;		+ 1		A (16)

                sep	#$30
                .a8
                .i8

		jsr	printStringAfter
		.byte	13,10,"Flags="
		nop
		lda	13,S
		jsr	debug_hex_byte
		lda	#'='
		jsr	call_OSWRCH
		ldx	#7
		lda	13,S
@lp:		rol	A
		pha
		lda	FlagNames,X
		bcc	@skU
		and	#$DF
@skU:		jsr	call_OSWRCH
		pla
		dex
		bpl	@lp

		jsr	printStringAfter		
		.byte	", Stack="
		nop
		tsc
		clc
		adc	#16
		xba
		bcc	@sk1
		inc	A
@sk1:		jsr	debug_hex_byte
		tsc
		clc
		adc	#16
		jsr	debug_hex_byte

		jsr	printStringAfter		
		.byte	", PC="
		nop
		lda	16,S
		jsr	debug_hex_byte
		lda	15,S
		jsr	debug_hex_byte
		lda	14,S
		jsr	debug_hex_byte

		jsr	printStringAfter		
		.byte	", B="
		nop
		lda	9,S
		jsr	debug_hex_byte

		jsr	printStringAfter		
		.byte	", DP="
		nop
		lda	8,S
		jsr	debug_hex_byte
		lda	7,S
		jsr	debug_hex_byte


		jsr	printCRLF

		jsr	printStringAfter
		.byte	"A="
		lda	2,S
		jsr	debug_hex_byte
		lda	1,S
		jsr	debug_hex_byte

		jsr	printStringAfter
		.byte	", X="
		lda	3,S
		jsr	debug_hex_byte
		lda	4,S
		jsr	debug_hex_byte

		jsr	printStringAfter
		.byte	", Y="
		lda	5,S
		jsr	debug_hex_byte
		lda	6,S
		jsr	debug_hex_byte

		jsr	printCRLF

		rep	#$30
		.a16
		.i16
		pla
		plx
		ply
		pld
		plb
		plp
		rts

FlagNames:	.byte	"czidxmvn" 		; backwards nvmxdizc

		; 
		; we assume stack looks like a native interrupt i.e.
                ;               + 6             Program bank
                ;               + 5             PCH
                ;               + 4             PCL
                ;               + 3             Native mode flags
                ;		+ 2		Caller retH
                ;		+ 1		Caller retL
StackTrace:	php
		rep	#$30
		.a16
		.i16
		phd
		pha
		phx
		phy

		; 
		; we assume stack looks like a native interrupt i.e.
                ;               + 15            Program bank
                ;               + 14            PCH
                ;               + 13            PCL
                ;               + 12            Native mode flags
                ;		+ 11		Caller retH
                ;		+ 10		Caller retL

                ;		+ 9		Flags
                ;		+ 7		DP
                ;		+ 5		A
                ;		+ 3		X
                ;		+ 1		Y


		sep	#$30
		.a8
		.i8

		pea	BLITTER_BASIC_DP
		pld

		; assume 8 bit stack!
		tsx
		txa
		clc
		adc	#16			; skip stack crud
		sta	DP_BAS_BL_DEBUGPTR	; store stack start
		and	#$F8
		tax
@lp0:		txa
		and	#7
		bne	@sk0
		jsr	printCRLF
		lda	#'1'			; assumes page 1 stack!
		jsr	call_OSWRCH
		txa
		jsr	debug_hex_byte
@sk0:		jsr	@spc

		cpx	DP_BAS_BL_DEBUGPTR
		bcc	@nop

		lda	f:$000100,X
		jsr	debug_hex_byte
@sk1:		inx
		bne	@lp0
		jsr	printCRLF

		rep	#$30
		.a16
		.i16
		ply
		plx
		pla
		pld
		plp
		rts

		.a8
		.i8

@nop:		jsr	@spc2
		bra	@sk1


@spc2:		jsr	@spc
@spc:		lda	#' '
		jmp	call_OSWRCH

BLITTER_shims_init:
		; we're entering in 8bit emulation mode
		.a8
		.i8

		php
		sei

		ldy	#0
		phy
		plb


		lda	zp_mos_jimdevsave
		pha

		; enable blitter JIM interface
		lda	#JIM_DEVNO_BLITTER
		sta	zp_mos_jimdevsave
		sta	fred_JIM_DEVNO


		; copy shims to sys memory

;		lda	#^__SHIMS_RUN__
		; check whether the page we're using is BLTURBO'd
		lda	sheila_MEM_LOMEMTURBO
		and	#1<<(>__SHIMS_RUN__ >> 4)
		php
		lda	#0
		plp
		bne	@skturbo
		lda	#$FF				; force SYS bank (actually bank 0 logical)
@skturbo:
		sta	fred_JIM_PAGE_HI
		lda	#>__SHIMS_RUN__
		sta	fred_JIM_PAGE_LO

		ldx	#<__SHIMS_SIZE__-1
@l1:		phk
		plb
		lda	__SHIMS_LOAD__,X
		phy
		plb
		sta	JIM+<__SHIMS_RUN__,X

		dex
		cpx	#$FF
		bne	@l1

		phy
		plb

		; copy vectors to ChipRAM 
;		lda	#^NATVEC_CHIP
		lda	#$00				; force 0 bank (actually hidden behind ROMS)
		sta	fred_JIM_PAGE_HI
		lda	#>NATVEC_CHIP
		sta	fred_JIM_PAGE_LO
		ldx	#<__NATVEC_SIZE__-1

@l2:		phk
		plb
		lda	__NATVEC_LOAD__,X
		phy
		plb
		sta	JIM+<__NATVEC_RUN__,X
		dex
		cpx	#$FF
		bne	@l2



                pla
                sta     zp_mos_jimdevsave
                sta     fred_JIM_DEVNO

                ; clear NATVEC_BRK
                lda	#<shim_brk_to_emu
                sta	NATVEC_BRK
                lda	#>shim_brk_to_emu
                sta	NATVEC_BRK+1
                lda	#^shim_brk_to_emu
                sta	NATVEC_BRK+2


		plp
		rts


		.SEGMENT "SHIMS"


		; TODO: need to set D/B/K or do a long jump at least
nat_OSWRCH:	
		php				; save flags			
		phb
		phd

		phk				; reset direct page register for MOSishness
		phk
		pld

		phk
		plb				; reset databank register TODO: is this the most efficient way?

		sec
		xce				; enter emulation mode

		jsr	OSWRCH

		clc
		xce				; back to native mode

		pld
		plb
		plp

		rtl

nat_OSWORD:	
		php				; save flags			
		phd
		phb

		phk				; reset direct page register for MOSishness
		phk
		pld

		phk
		plb				; reset databank register TODO: is this the most efficient way?

		sec
		xce				; enter emulation mode

		jsr	OSWORD
		bcs	ret_SEC
		bcc	ret_CLC




nat_OSBYTE:	; force emulation, we don't care about A,X,Y losing 16bitness, OSBYTE return 8bits in A,X,Y
		php				; but do save flags here
		phd
		phb

		phk
		phk				; reset direct page register for MOSishness
		pld
		phk
		plb				; reset databank register TODO: is this the most efficient way?
		sec
		xce
		jsr	OSBYTE
		bcs	ret_SEC

ret_CLC:
		clc
		xce				; back to native mode
		plb
		pld
		plp
		clc
		rtl
ret_SEC:
		clc
		xce				; back to native mode
		plb
		pld
		plp				; get original flags with Cy
		sec
		rtl




shim_brk:	jml	(NATVEC_BRK)

shim_brk_to_emu:
		; BRK vector in native mode stack will contain:
		;		+ 4		Program bank
		;		+ 3		PCH
		;		+ 2		PCL
		;		+ 1		Native mode flags
		; this handler ignores the program bank (the stack will probably)
		; be reset anyway and just mangles the flags to look like a normal 6502
		; BRK (i.e bit 5 is set and bit 6 is cleared)
		; also this does nothing to try and save any extended registers

		; for a more general Native mode BRK:
		; - add a Native mode BRK vector, either use one of the unused MOS vectors
		;   or add our own in private RAM at 008Fxx?
		; - default N_BRKV would point to this shim

		sep	#%00110000		; 8 bit M, IX
		.a8
		.i8				
		pha				; save A but not sure of the utility of this as it might have been 16 bits!
		phk				; We know K is always 0!
		phk
		pld				; set DP to 0
		phk
		plb				; set data bank to 0
		lda	2,S
		; make the flags look like those of an emulation mode BRK
		and	#%11001111		; clear the M/X flags out of pushed flags
		ora	#%00010000		; force the B bit in flags
		sta	2,S
		pla
		sec
		xce
		jmp	(HWVEC_6502_IRQ)	; jump to emulation mode brk vector
						; TODO - jump straight to BRKV here?

shim_cop:
shim_abort:
shim_nmi:
		rti	;  TODO!
shim_irq:
		;; irq handling code derived from http://cerebro.xu.edu/~ryanr/atari/65816.html
		;; and https://github.com/BigEd/boot816/blob/master/boot816.as
        	;; this is called from the 816 irq vector, so only called in 816 mode
        	;; a beeb-hosted 816 must switch to 6502 mode to use the OS handler
        	;; or it could handle it itself
        	;; note that the machine will already have pushed P and done SEI
        	phd                     	; Save DBR and Direct
        	phb
;        	pea	$0000           	; Clear Direct...
		phk
		phk				; use two phk's, cheaper than a pea?
        	pld                     	;
        	phk                     	; ... and with a single zero byte ...
        	plb                     	; ... clear the DBR
        	php                     	; push 816 P reg for reg width info (I was set as IRQ vector fetched)
        	rep	#%00110000		; 16 bits
        	.a16
        	.i16
        	pha
        	phx                     	; X&Y saved at present width, because resetting them
        	phy                     	;  to 8 bits would destroy the upper half contents.
        	per 	shim_irq_ret		; push return address (then status) for the RTI
        	;; push a fake interrupt frame so we can call the 6502 host interrupt service
        	sep 	#%00110000		; Set 8 bit regs
        	.a8
        	.i8
        	lda 	#%00000100		; we want I set and B clear for the 6502 irq handler
        	pha                     	; saving for sake of 6502 handler and RTI
        	;; everything is safe
        	;; switch to 6502 mode for the host interrupt service routine
        	sec
        	xce        	
        	;;        now jump to the appropriate interrupt vector, such as...
        	jmp	(HWVEC_6502_IRQ) 	; Its RTI will return to IRETURN
        	; done

shim_irq_ret:

		clc                     ; beeb special: return to 816-mode (we're in the 816-mode handler!)
		xce
        	rep	#%00110000		; 16 bits
        	.a16
        	.i16
		ply                     ; now restore X&Y at whatever width they were saved at 16 bits
		plx                     ;
		pla
		plp                     ; recover unmodified 816 status byte - for reg widths
		                        ; we know this saved P has SEI
		                        ; we don't worry about N and Z because the next RTI will pull a real P
		plb                     ; Restore DBR and Direct
		pld
		rti                     ; Return to main program (pulling genuine user-mode P then 3 PC bytes)

				.SEGMENT "NATVEC"
NV_RES0:	.word .LOWORD(0)
NV_RES1:	.word .LOWORD(0)
NV_COP:		.word .LOWORD(shim_cop)
NV_BRK:		.word .LOWORD(shim_brk)
NV_ABORT:	.word .LOWORD(shim_abort)
NV_NMI:		.word .LOWORD(shim_nmi)
NV_RES2:	.word .LOWORD(0)
NV_IRQ:		.word .LOWORD(shim_irq)

	.END