# This makefile will compile and compare the diassembled communicator BASIC 
# sourcecode against the original BIN

AS=ca65
ASFLAGS=
LD=ld65
LDFLAGS=
DIFF=diff
DIFFFLAGS=-I '^; Input file' -I '^; Created' -u

BUILDDIR=./build
CMPDIR=./cmp
ORGDIR=.


#list of original basics
ORGBASICS=	COMBAS100
#list of recompiled/new basics
BASICS_NEW=	bas816new_COMBAS100 bas816new_BLITTER
BASICS_ORG=	COMBAS100 

#filenames for intermediates and comparison files
BASICS_O_NEW=	$(addprefix $(BUILDDIR)/, $(addsuffix .o, $(BASICS_NEW)))
BASICS_O_ORG=	$(addprefix $(BUILDDIR)/, $(addsuffix .o, $(BASICS_ORG)))
TARGETS=	$(addprefix $(BUILDDIR)/,$(addsuffix .bin, $(BASICS_NEW) $(BASICS_NEW)))
CMPORG=		$(addprefix $(ORGDIR)/, $(addsuffix .bin, $(ORGBASICS)))
CMPORGBASE=	$(basename $(basename $(notdir $(CMPORG))))
CMPDIFF=	$(addsuffix .diff, $(addprefix $(CMPDIR)/, $(CMPORGBASE))) $(addsuffix .diff, $(addprefix $(CMPDIR)/bas816new_, $(CMPORGBASE)))

HOSTFSDIR=~/hostfs/tools816


NOWT:=$(shell mkdir -p $(BUILDDIR))
NOWT:=$(shell mkdir -p $(CMPDIR))

.PRECIOUS: $(BUILDDIR)/bas816new_COMBAS100.lst $(BUILDDIR)/bas816new_BLITTER.lst

all::	$(TARGETS)

cmp::	$(CMPDIFF)

hostfs:: $(BUILDDIR)/bas816new_BLITTER.bin
	cp $(BUILDDIR)/bas816new_BLITTER.bin $(HOSTFSDIR)
	cp bas816new_BLITTER.bin.inf $(HOSTFSDIR)


$(CMPDIR)/%.diff: $(BUILDDIR)/%.da.s $(BUILDDIR)/ORG_%.da.s 
	$(DIFF) $(DIFFFLAGS) $< $(BUILDDIR)/ORG_$(notdir $(basename $@)).da.s > $@

$(CMPDIR)/bas816new_COMBAS100.diff: $(BUILDDIR)/bas816new_COMBAS100.da.s $(BUILDDIR)/ORG_COMBAS100.da.s 
	$(DIFF) $(DIFFFLAGS) $(BUILDDIR)/bas816new_COMBAS100.da.s $(BUILDDIR)/ORG_COMBAS100.da.s > $@


$(BUILDDIR)/%.da.s: $(BUILDDIR)/%.bin
	da65 -S 0x4b00 -o $@ $<

$(BUILDDIR)/ORG_%.da.s: $(ORGDIR)/%.bin
	da65 -S 0x4b00 -o $@ $<



$(BUILDDIR)/COMBAS100.o:		OPTDEF=
$(BUILDDIR)/bas816new_COMBAS100.o:	OPTDEF=-D COMMUNICATOR -D COMM100
$(BUILDDIR)/bas816new_BLITTER.o:	OPTDEF=-D OPTIMIZE=1 -D BLITTER

$(BUILDDIR)/bas816new_BLITTER.o:	bas816new_natshims.asm

bas816new.asm:	bas816new.inc

$(BASICS_O_ORG):	COMBAS100.bin_cc65.S $(DEPS) $(INCS)
	$(AS) $(ASFLAGS) $(OPTDEF) -o $@ -g -l $(basename $@).lst $<

$(BASICS_O_NEW):	bas816new.asm $(DEPS) $(INCS)
	$(AS) $(ASFLAGS) $(OPTDEF) -o $@ -g -l $(basename $@).lst $<


$(BUILDDIR)/%.bin: $(BUILDDIR)/%.o %.cfg
	$(LD) -vm -Ln $(basename $@).sy2 -m $(basename $@).map -o $@ -C $(notdir $(basename $@)).cfg $<

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
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .da.s, $(CMPORGBASE))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/ORG_, $(addsuffix .da.s, $(CMPORGBASE))) 2>/dev/nul
	-rm $(CMPDIFF) 2>/dev/null

