SOURCEDIR=../source

HOST:=$(shell gcc -dumpmachine)
TARGET=$(HOST)

BUILDDIR=$(TARGET).noindex

ifneq ($(TARGET),$(HOST))
 GCCPREFIX=$(TARGET)-
 BINLN=
else
 GCCPREFIX=
 BINLN=bin
endif
GCCSUFFIX=
GCC=$(GCCPREFIX)gcc$(GCCSUFFIX)

GFLAGS=
CFLAGS=-ggdb -pipe -gnatef -gnata -gnatwa -gnatyy-3cbhms \
	$(addprefix -gnatec=,$(abspath $(wildcard *.adc)))
MFLAGS=-D $(BUILDDIR) -gnatA
BFLAGS=-E
LFLAGS=-ggdb
HFLAGS=

ifneq ($(findstring linux-gnu,$(TARGET)),)
 ICONVTYPE=glibc
else
 ICONVTYPE=libiconv
endif

ifneq ($(DRAKE_RTSROOT),)
 VERSION:=$(shell $(GCC) -dumpversion)
 DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)
endif
ifneq ($(DRAKE_RTSDIR),)
 IMPORTDIR=
 GFLAGS+=--RTS=$(abspath $(DRAKE_RTSDIR))
else
 ifneq ($(wildcard import/$(TARGET)),)
  IMPORTDIR=import/$(TARGET)
 else
  IMPORTDIR=$(BUILDDIR)/import
 endif
 MFLAGS+=-I$(IMPORTDIR)
endif

ifneq ($(GCC),gcc)
 GFLAGS+=$(filter -m32 -m64,$(GCC))
 MFLAGS+=--GCC="$(GCC)"
 LFLAGS+=--GCC="$(GCC)"
 HFLAGS+=-gcc="$(GCC)"
endif

EXAMPLES=$(basename $(filter-out b~%,$(wildcard *.adb)))
TESTS=$(filter test_%,$(EXAMPLES))

.PHONY: all check clean $(EXAMPLES)

all: $(addprefix $(BUILDDIR)/,$(EXAMPLES)) $(BINLN)

$(BUILDDIR)/%: %.adb $(wildcard $(SOURCEDIR)/*) $(wildcard $(SOURCEDIR)/$(ICONVTYPE)/*) | $(BUILDDIR) $(IMPORTDIR)
	$(GCCPREFIX)gnatmake$(GCCSUFFIX) -c -I$(SOURCEDIR) -I$(SOURCEDIR)/$(ICONVTYPE) $< $(GFLAGS) $(MFLAGS) -cargs $(CFLAGS)
	cd $(BUILDDIR) && $(GCCPREFIX)gnatbind$(GCCSUFFIX) $(basename $(notdir $<)).ali $(GFLAGS) $(BFLAGS)
	cd $(BUILDDIR) && $(GCCPREFIX)gnatlink$(GCCSUFFIX) -o ../$@ $(basename $(notdir $<)).ali $(GFLAGS) $(LFLAGS)

$(BUILDDIR):
	mkdir $@

$(BINLN): | $(BUILDDIR)
	ln -s $(BUILDDIR) $@

$(filter-out test_%,$(EXAMPLES)): %: $(BUILDDIR)/% $(BINLN)

check: all $(TESTS)

$(TESTS): %: $(BUILDDIR)/%
	$<

ifeq ($(IMPORTDIR),$(BUILDDIR)/import)
$(IMPORTDIR): $(SOURCEDIR)/import.h
	headmaster --to ada $(HFLAGS) -p -D $@ $+
	touch $@
endif

clean:
	-rm -r *.noindex bin
