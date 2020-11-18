# This makefile will compile and compare the diassembled communicator BASIC 
# sourcecode against the original BIN

AS=ca65
ASFLAGS=
LD=ld65
LDFLAGS=

BUILDDIR=./build
CMPDIR=./cmp
ORGDIR=.

#list of original basics
ORGBASICS=	COMBAS100
#list of recompiled/new basics
BASICS=		COMBAS100

#filenames for intermediates and comparison files
BASICS_O=	$(addprefix $(BUILDDIR)/, $(addsuffix .o, $(BASICS)))
TARGETS=	$(addprefix $(BUILDDIR)/,$(addsuffix .bin, $(BASICS)))
CMPORG=		$(addprefix $(ORGDIR)/, $(addsuffix .bin, $(ORGBASICS)))
CMPORGBASE=	$(basename $(basename $(notdir $(CMPORG))))
CMPDIFF=	$(addsuffix .diff, $(addprefix $(CMPDIR)/, $(CMPORGBASE)))



NOWT:=$(shell mkdir -p $(BUILDDIR))
NOWT:=$(shell mkdir -p $(CMPDIR))


all::	$(TARGETS)

cmp::	$(CMPDIFF)


$(CMPDIR)/%.diff: $(BUILDDIR)/%.da.s $(BUILDDIR)/ORG_%.da.s 
	-diff -u $< $(BUILDDIR)/ORG_$(notdir $(basename $@)).da.s > $@


$(BUILDDIR)/%.da.s: $(BUILDDIR)/%.bin
	da65 -S 0x4b00 -o $@ $<

$(BUILDDIR)/ORG_%.da.s: $(ORGDIR)/%.bin
	da65 -S 0x4b00 -o $@ $<



$(BUILDDIR)/COMBAS100.o:	OPTDEF=-D COMBAS100

$(BASICS_O):	COMBAS100.bin_cc65.S $(DEPS) $(INCS)
	$(AS) $(ASFLAGS) $(OPTDEF) -o $@ -g -l $(basename $@).lst $<

%.bin: %.o ../$(notdir %.cfg)
	$(LD) -vm -Ln $(basename $@).sy2 -m $(basename $@).map -o $@ -C $(notdir $(basename $@)).cfg $<

clean::
	-rm $(BASICS_O) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .bin, $(BASICS))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .sy2, $(BASICS))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .map, $(BASICS))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .lst, $(BASICS))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .noi, $(BASICS))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/, $(addsuffix .da.s, $(CMPORGBASE))) 2>/dev/nul
	-rm $(addprefix $(BUILDDIR)/ORG_, $(addsuffix .da.s, $(CMPORGBASE))) 2>/dev/nul
	-rm $(CMPDIFF) 2>/dev/null

