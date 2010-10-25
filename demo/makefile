SOURCEDIR=../source
BUILDDIR=build
IMPORTDIR=import

ifeq ($(DRAKE),)
H=$(SOURCEDIR)/import.h
RTSDIR=
MFLAGS=
LFLAGS=
else
H=$(IMPORTDIR)/import2.h $(IMPORTDIR)/import-drake.h $(IMPORTDIR)/import-lib.h
RTSDIR=rts
MFLAGS=--RTS=$(abspath $(RTSDIR)) -a
LFLAGS=-largs -lc-gnat
endif

CFLAGS=-gnata -gnatwa -gnatyy-3chbs

.PHONY: all clean $(BUILDDIR)/list $(BUILDDIR)/stream_r $(BUILDDIR)/stream_w

all: $(BUILDDIR)/list $(BUILDDIR)/stream_r $(BUILDDIR)/stream_w

$(BUILDDIR)/list: list.adb $(BUILDDIR) $(IMPORTDIR)/c.ads $(RTSDIR)
	cd $(BUILDDIR) && gnatmake -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(IMPORTDIR) $(CFLAGS) ../$< $(LFLAGS)

$(BUILDDIR)/stream_r: stream_r.adb $(BUILDDIR) $(IMPORTDIR)/c.ads $(RTSDIR)
	cd $(BUILDDIR) && gnatmake -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(IMPORTDIR) $(CFLAGS) ../$< $(LFLAGS)

$(BUILDDIR)/stream_w: stream_w.adb $(BUILDDIR) $(IMPORTDIR)/c.ads $(RTSDIR)
	cd $(BUILDDIR) && gnatmake -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(IMPORTDIR) $(CFLAGS) ../$< $(LFLAGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(IMPORTDIR)/c.ads: $(H)
	headmaster -p --to ada -gcc=$(dir $(shell which gnatmake))gcc -D $(IMPORTDIR) $<

ifneq ($(DRAKE),)

$(IMPORTDIR)/import2.h:
	-mkdir $(IMPORTDIR)
	echo \#include \"import-drake.h\" > $@
	echo \#include \"import-lib.h\" >> $@

$(IMPORTDIR)/import-drake.h: $(DRAKE)/import.h
	ln -s $(abspath $<) $@

$(IMPORTDIR)/import-lib.h: $(SOURCEDIR)/import.h
	ln -s $(abspath $<) $@

$(RTSDIR):
	make -C $(DRAKE) RTSDIR=$(abspath $(RTSDIR)) IMPORTDIR=$(abspath $(IMPORTDIR))

endif

clean:
	-rm -rf build
	-rm -rf import
	-rm -rf rts
