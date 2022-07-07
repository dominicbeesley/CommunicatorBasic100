# This makefile will compile and compare the diassembled communicator BASIC 
# sourcecode against the original BIN

BUILDDIR=./build
CMPDIR=./cmp
ORGDIR=./org
INCDIR=./includes
SCRIPTS=./scripts
NEWSRCDIR=./src
SSDDIR=./ssd

AS=ca65
ASFLAGS=-I $(INCDIR)
LD=ld65
LDFLAGS=
DIFF=diff
DIFFFLAGS=-I '^; Input file' -I '^; Created' -u


GEN_INCS=$(INCDIR)/names.inc $(INCDIR)/names_alpha.txt $(INCDIR)/names_num.txt
INCS+=$(GEN_INCS)

#list of original basics
ORGMODULES=	COMBAS100 ARITH100
#list of recompiled/new basics
BASICS_NEW=	bas816new_COMBAS100 bas816new_BLITTER bas816new_BEEB816 bas816new_DOSSY
ARITHS_NEW=	arith_new_COMARITH100
BASICS_ORG=	COMBAS100
ARITHS_ORG=	ARITH100

#filenames for intermediates and comparison files
BASICS_O_NEW=	$(addprefix $(BUILDDIR)/, $(addsuffix .o, $(BASICS_NEW)))
ARITHS_O_NEW=	$(addprefix $(BUILDDIR)/, $(addsuffix .o, $(ARITHS_NEW)))
BASICS_O_ORG=	$(addprefix $(BUILDDIR)/, $(addsuffix .o, $(BASICS_ORG)))
ARITHS_O_ORG=	$(addprefix $(BUILDDIR)/, $(addsuffix .o, $(ARITHS_ORG)))

TARGETS=	$(addprefix $(BUILDDIR)/,$(addsuffix .bin, $(BASICS_NEW))) \
		$(addprefix $(BUILDDIR)/,$(addsuffix .bin, $(ARITHS_NEW)))

CMPORG=		$(addprefix $(ORGDIR)/, $(addsuffix .bin, $(ORGMODULES)))
CMPORGBASE=	$(basename $(basename $(notdir $(CMPORG))))
CMPDIFF=	$(addsuffix .diff, $(addprefix $(CMPDIR)/, $(CMPORGBASE))) \
		$(CMPDIR)/bas816new_COMBAS100.diff \
		$(CMPDIR)/arith_new_COMARITH100.diff

SSDS=		$(SSDDIR)/BAS816_BLIT.ssd $(SSDDIR)/BAS816_DOSSY.ssd $(SSDDIR)/BAS816_BEEB816.ssd

HOSTFSDIR=~/hostfs
BEMDIR=/cygdrive/e/Users/dominic/BeebEm/DiscIms

NOWT:=$(shell mkdir -p $(BUILDDIR))
NOWT:=$(shell mkdir -p $(CMPDIR))
NOWT:=$(shell mkdir -p $(SSDDIR))

.PRECIOUS: 	$(BUILDDIR)/bas816new_COMBAS100.lst \
		$(BUILDDIR)/bas816new_BLITTER.lst \
		$(BUILDDIR)/bas816new_BEEB816.lst \
		$(BUILDDIR)/bas816new_DOSSY.lst \
		$(BUILDDIR)/bas816new_COMBAS100.da.s \
		$(BUILDDIR)/ORG_COMBAS100.da.s \
		$(BUILDDIR)/arith_new_COMARITH100.da.s \
		$(BUILDDIR)/ORG_ARITH100.da.s \
		$(BUILDDIR)/ARITH100.bin \
		$(BUILDDIR)/COMBAS100.bin

.DELETE_ON_ERROR:

all::	$(TARGETS) $(CMPDIFF) $(SSDS)

cmp::	$(CMPDIFF)

hostfs::$(SSDS)
	$(foreach s, $^, mkdir -p $(HOSTFSDIR)/$(notdir $(basename $(s)));)
	$(foreach s, $^, dfs read -i -d $(HOSTFSDIR)/$(notdir $(basename $(s))) $(s);)


bem::	$(BUILDDIR)/bas816new_DOSSY.bin
	cp bas816new_DOSSY.bin.inf $(BUILDDIR)
	dfs form -80 $(BUILDDIR)/bas816.ssd
	dfs add $(BUILDDIR)/bas816.ssd $(BUILDDIR)/bas816new_DOSSY.bin
	rm $(BUILDDIR)/bas816new_DOSSY.bin.inf
	cp $(BUILDDIR)/bas816.ssd $(BEMDIR)
	# make symbols for b-em
	../../scripts/bemsymbols.pl < $(BUILDDIR)/bas816new_DOSSY.sy2 > $(BUILDDIR)/bas816new_DOSSY.bem
	cygpath -w -a "$(BUILDDIR)/bas816new_DOSSY.bem"


$(addprefix $(BUILDDIR)/, $(addsuffix .da.s, $(BASICS_NEW))): STARTADDR=0x4b00
$(addprefix $(BUILDDIR)/, $(addsuffix .da.s, $(ARITHS_NEW))): STARTADDR=0xAf00

$(addprefix $(BUILDDIR)/, $(addsuffix .da.s, $(BASICS_ORG))): STARTADDR=0x4b00
$(addprefix $(BUILDDIR)/, $(addsuffix .da.s, $(ARITHS_ORG))): STARTADDR=0xAf00

$(addprefix $(BUILDDIR)/ORG_, $(addsuffix .da.s, $(BASICS_ORG))): STARTADDR=0x4b00
$(addprefix $(BUILDDIR)/ORG_, $(addsuffix .da.s, $(ARITHS_ORG))): STARTADDR=0xAf00

$(CMPDIR)/%.diff: $(BUILDDIR)/%.da.s $(BUILDDIR)/ORG_%.da.s 
	$(DIFF) $(DIFFFLAGS) $< $(BUILDDIR)/ORG_$(notdir $(basename $@)).da.s > $@

$(CMPDIR)/bas816new_COMBAS100.diff: $(BUILDDIR)/bas816new_COMBAS100.da.s $(BUILDDIR)/ORG_COMBAS100.da.s 
	$(DIFF) $(DIFFFLAGS) $(BUILDDIR)/bas816new_COMBAS100.da.s $(BUILDDIR)/ORG_COMBAS100.da.s > $@

$(CMPDIR)/arith_new_COMARITH100.diff: $(BUILDDIR)/arith_new_COMARITH100.da.s $(BUILDDIR)/ORG_ARITH100.da.s 
	$(DIFF) $(DIFFFLAGS) $(BUILDDIR)/arith_new_COMARITH100.da.s $(BUILDDIR)/ORG_ARITH100.da.s > $@

$(BUILDDIR)/%.da.s: $(BUILDDIR)/%.bin
	da65 -S $(STARTADDR) --comments 4 -o $@ $<

$(BUILDDIR)/ORG_%.da.s: $(ORGDIR)/%.bin
	da65 -S $(STARTADDR) --comments 4 -o $@ $<



$(BUILDDIR)/COMBAS100.o:		OPTDEF=
$(BUILDDIR)/ARITH100.o:			OPTDEF=
$(BUILDDIR)/bas816new_COMBAS100.o:	OPTDEF=-D COMMUNICATOR -D COMM100
$(BUILDDIR)/bas816new_BLITTER.o:	OPTDEF=-D OPTIMIZE=1 -D BLITTER -D MOS -D BUGFIX
$(BUILDDIR)/bas816new_BEEB816.o:	OPTDEF=-D OPTIMIZE=1 -D BEEB816 -D MOS -D BUGFIX
$(BUILDDIR)/bas816new_DOSSY.o:		OPTDEF=-D OPTIMIZE=1 -D DOSSY -D MOS -D BUGFIX
$(BUILDDIR)/arith_new_COMARITH100.o:	OPTDEF=-D COMMUNICATOR -D COMM100
$(BUILDDIR)/arith_new_blit_bas.o:	OPTDEF=-D OPTIMIZE=1 -D BLITTER -D MOS -D BUGFIX
$(BUILDDIR)/arith_new_beeb816_bas.o:	OPTDEF=-D OPTIMIZE=1 -D BEEB816 -D MOS -D BUGFIX
$(BUILDDIR)/arith_new_dossy_bas.o:	OPTDEF=-D OPTIMIZE=1 -D DOSSY -D MOS -D BUGFIX
$(BUILDDIR)/bas816new_BLITTER.o:	$(NEWSRCDIR)/bas816new_natshims.asm $(NEWSRCDIR)/bas816new_BLITTER.inc
$(BUILDDIR)/bas816new_BEEB816.o:	$(NEWSRCDIR)/bas816new_natshims.asm $(NEWSRCDIR)/bas816new_BEEB816.inc
$(BUILDDIR)/bas816new_DOSSY.o:		$(NEWSRCDIR)/bas816new_natshims.asm $(NEWSRCDIR)/bas816new_DOSSY.inc
$(BUILDDIR)/arith_new_blit_bas.o:	$(NEWSRCDIR)/bas816new_BLITTER.inc
$(BUILDDIR)/arith_new_beeb816_bas.o:	$(NEWSRCDIR)/bas816new_BEEB816.inc
$(BUILDDIR)/arith_new_dossy_bas.o:	$(NEWSRCDIR)/bas816new_DOSSY.inc

bas816new.asm:	bas816new.inc names.inc
arith_new.asm:  names.inc

$(BASICS_O_ORG):	$(ORGDIR)/COMBAS100.bin_cc65.S $(DEPS) $(INCS)
	$(AS) $(ASFLAGS) $(OPTDEF) -o $@ -g -l $(basename $@).lst $<

$(ARITHS_O_ORG):	$(ORGDIR)/ARITH100.bin_cc65.S $(DEPS) $(INCS)
	$(AS) $(ASFLAGS) $(OPTDEF) -o $@ -g -l $(basename $@).lst $<


$(BASICS_O_NEW):	$(NEWSRCDIR)/bas816new.asm $(DEPS) $(INCS)
	$(AS) $(ASFLAGS) $(OPTDEF) -o $@ -g -l $(basename $@).lst $<

$(ARITHS_O_NEW):	$(NEWSRCDIR)/arith_new.asm $(DEPS) $(INCS)
	$(AS) $(ASFLAGS) $(OPTDEF) -o $@ -g -l $(basename $@).lst $<

$(BUILDDIR)/arith_new_blit_bas.o: $(NEWSRCDIR)/arith_new.asm
	$(AS) $(ASFLAGS) $(OPTDEF) -o $@ -g -l $(basename $@).lst $<

$(BUILDDIR)/arith_new_beeb816_bas.o: $(NEWSRCDIR)/arith_new.asm
	$(AS) $(ASFLAGS) $(OPTDEF) -o $@ -g -l $(basename $@).lst $<

$(BUILDDIR)/arith_new_dossy_bas.o: $(NEWSRCDIR)/arith_new.asm
	$(AS) $(ASFLAGS) $(OPTDEF) -o $@ -g -l $(basename $@).lst $<

$(BUILDDIR)/%.bin: $(BUILDDIR)/%.o $(NEWSRCDIR)/%.cfg
	$(LD) -vm -Ln $(basename $@).sy2 -m $(basename $@).map -o $@ -C $(NEWSRCDIR)/$(notdir $(basename $@)).cfg $<

$(BUILDDIR)/%.bin: $(BUILDDIR)/%.o $(ORGDIR)/%.cfg
	$(LD) -vm -Ln $(basename $@).sy2 -m $(basename $@).map -o $@ -C $(ORGDIR)/$(notdir $(basename $@)).cfg $<


$(BUILDDIR)/bas816new_BLITTER.bin: $(BUILDDIR)/bas816new_BLITTER.o $(NEWSRCDIR)/bas816new_BLITTER.cfg $(BUILDDIR)/arith_new_blit_bas.o
	$(LD) -vm -Ln $(basename $@).sy2 -m $(basename $@).map -o $@ -C $(NEWSRCDIR)/$(notdir $(basename $@)).cfg $< $(BUILDDIR)/arith_new_blit_bas.o

$(BUILDDIR)/bas816new_BEEB816.bin: $(BUILDDIR)/bas816new_BEEB816.o $(NEWSRCDIR)/bas816new_BEEB816.cfg $(BUILDDIR)/arith_new_beeb816_bas.o
	$(LD) -vm -Ln $(basename $@).sy2 -m $(basename $@).map -o $@ -C $(NEWSRCDIR)/$(notdir $(basename $@)).cfg $< $(BUILDDIR)/arith_new_beeb816_bas.o

$(BUILDDIR)/bas816new_DOSSY.bin: $(BUILDDIR)/bas816new_DOSSY.o $(NEWSRCDIR)/bas816new_DOSSY.cfg $(BUILDDIR)/arith_new_dossy_bas.o
	$(LD) -vm -Ln $(basename $@).sy2 -m $(basename $@).map -o $@ -C $(NEWSRCDIR)/$(notdir $(basename $@)).cfg $< $(BUILDDIR)/arith_new_dossy_bas.o

$(GEN_INCS): $(ORGDIR)/NAMES100.bin $(SCRIPTS)/decodeNAMES.pl
	$(SCRIPTS)/decodeNAMES.pl $(ORGDIR)/NAMES100.bin $(INCDIR)

$(BUILDDIR)/%.bin.inf: $(NEWSRCDIR)/%.bin.inf $(BUILDDIR)/%.bin
	cp $< $@

$(SSDDIR)/BAS816_BLIT.ssd: $(BUILDDIR)/bas816new_BLITTER.bin.inf $(wildcard extras/blitter/*.inf) $(wildcard extras/shared/*.inf)
	dfs form -80 $@
	dfs title $@ "BAS816BLIT"
	dfs opt4 -3 $@
	dfs add $@ $^

$(SSDDIR)/BAS816_DOSSY.ssd: $(BUILDDIR)/bas816new_DOSSY.bin.inf $(wildcard extras/dossy/*.inf) $(wildcard extras/shared/*.inf)
	dfs form -80 $@
	dfs title $@ "BAS816DOSSY"
	dfs opt4 -3 $@
	dfs add $@ $^

$(SSDDIR)/BAS816_BEEB816.ssd: $(BUILDDIR)/bas816new_BEEB816.bin.inf $(wildcard extras/beeb816/*.inf) $(wildcard extras/shared/*.inf)
	dfs form -80 $@
	dfs title $@ "BAS816BEEB"
	dfs opt4 -3 $@
	dfs add $@ $^


clean::
	-rm $(BASICS_O_NEW) 2>/dev/nul
	-rm $(BASICS_O_ORG) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .bin, $(BASICS_ORG))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .sy2, $(BASICS_ORG))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .map, $(BASICS_ORG))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .lst, $(BASICS_ORG))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .noi, $(BASICS_ORG))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .bin, $(BASICS_NEW))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .sy2, $(BASICS_NEW))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .map, $(BASICS_NEW))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .lst, $(BASICS_NEW))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .noi, $(BASICS_NEW))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .bin, $(ARITHS_ORG))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .sy2, $(ARITHS_ORG))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .map, $(ARITHS_ORG))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .lst, $(ARITHS_ORG))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .noi, $(ARITHS_ORG))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .bin, $(ARITHS_NEW))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .sy2, $(ARITHS_NEW))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .map, $(ARITHS_NEW))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .lst, $(ARITHS_NEW))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .noi, $(ARITHS_NEW))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .da.s, $(CMPORGBASE))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/ORG_, $(addsuffix .da.s, $(CMPORGBASE))) 2>/dev/nul
	-rm build/*
	-rm $(CMPDIFF) 2>/dev/null
	-rm ssd/*

