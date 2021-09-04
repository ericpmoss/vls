MAINTAINERCLEANFILES = Makefile.in
CLEANFILES = *.info* *.html

all: html

info_TEXINFOS = vls.texi

SUFFIXES = .html
html_docs = vls.html
.texi.html:
	texi2html $< || $(MAKEINFO) --html $<
.PHONY: html
html: $(html_docs)

vlsdocdir = $(prefix)/doc/vls
vlsdoc_DATA = vls.html

vls,ps: vls.texi
	$(TEXI2DVI) vls.texi
	$(DVIPS) -f vls.dvi > vls.ps

.PHONY: ps
ps: vls.ps


