SOURCEDIR=../source

HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)

VERSION=$(shell gcc -dumpversion)

BUILDDIR=$(TARGET).build

ifeq ($(TARGET),$(HOST))
GNATMAKE=gnatmake
GCC=$(dir $(shell which $(GNATMAKE)))gcc
BINLN=bin
else
GNATMAKE=$(TARGET)-gnatmake
GCC=$(TARGET)-gcc
BINLN=
endif

ifneq ($(findstring linux,$(TARGET)),)
ICONVTYPE=lfs
else
ICONVTYPE=libiconv
endif

ifneq ($(DRAKE_RTSROOT),)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)
endif
ifneq ($(DRAKE_RTSDIR),)
IMPORTDIR=
MFLAGS=--RTS=$(abspath $(DRAKE_RTSDIR))
else
IMPORTDIR=$(BUILDDIR)/import
MFLAGS=-I$(abspath $(IMPORTDIR))
endif

CFLAGS=-gnata -gnatwa -gnatyy-3chbs

.PHONY: all clean

all: $(BUILDDIR)/list $(BUILDDIR)/stream_r $(BUILDDIR)/stream_w $(BUILDDIR)/test_nop $(BUILDDIR)/version $(BINLN)

$(BUILDDIR)/%: %.adb $(BUILDDIR) $(IMPORTDIR) $(wildcard $(SOURCEDIR)/*)
	cd $(BUILDDIR) && $(GNATMAKE) -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(SOURCEDIR)/$(ICONVTYPE) $(CFLAGS) ../$< -largs $(LFLAGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(BINLN): $(BUILDDIR)
	ln -s $(BUILDDIR) $(BINLN)

ifneq ($(IMPORTDIR),)
$(IMPORTDIR): $(SOURCEDIR)/import.h
	headmaster --to ada -gcc=$(GCC) $(HFLAGS) -p -D $(IMPORTDIR) $+
endif

clean:
	-rm -rf *.build bin
