#



HTML = index.html cst.html install.html mpml.html systemLog.html observerDuties.html

TEX = SEDesign.tex correlator.tex

DVI = $(TEX:tex=dvi)
PS = $(TEX:tex=ps)
PDF = $(TEX:tex=pdf)

DOXYDIR=../html/carma

.SUFFIXES: .tex .dvi .ps .pdf

.tex.dvi:
	latex $<

.dvi.ps:
	dvips -o $@ $<

.ps.pdf:
	ps2pdf $<

#	ps2pdf $*.ps

pipeline:
	pdflatex pipeline.tex

interfEngine:
	rm -rf interfEnginedox 
	doxygen interf-doxygen.cfg
	( cd interfEnginedox/latex; make all; mv refman.pdf ../../DelayEngineReferenceManual.pdf )
	pslatex interfEngine.tex
	dvips interfEngine.dvi -o interfEngine.ps
	ps2pdf interfEngine.ps InterferometryDesign.pdf

# pdflatex cannot handle eps insertions....
#.tex.pdf:
#	pdflatex $<

all:	$(PS) pipeline interfEngine


html:	$(HTML)
	cp $? ../html

doxy:
	mkdir -p $(DOXYDIR)
	srcdir=`cd ..; pwd` \
	DOXYDIR=$(DOXYDIR) \
	doxygen carma-doxygen.cfg

clean:
	rm -rf interfEnginedox
