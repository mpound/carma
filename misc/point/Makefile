# this makefile has a hardcoded Intel (use 8.0 or 8.1 for now) compiler
# it has not been tested with earlier versions
# It also needs pgplot

SRC = atan3d.for fitxyzz.for \
      matinv.for \
      point.for ptfits.for ptload.for ptplot.for ptutil.for 

SRCC = uiptime.c

OBJ = $(SRC:.for=.o) $(SRCC:.c=.o)

#  some linex are > 80, so use a compiler option that let it go a bit longer
FC = ifort -132
CC = icc

FFLAGS = -g
CFLAGS = -g

PGPLOT_DIR = lib
PLIB = -L$(PGPLOT_DIR) -lpgplot
XLIB = -L/usr/X11R6/lib -lSM -lICE -lXext -lX11

.SUFFIXES:  .for

.for.o:
	$(FC) $(FFLAGS) -c $<

#---------------------------------------------------------------------------

point: $(OBJ)
	$(FC) $(FFLAGS) -o point $(OBJ) $(PLIB) $(XLIB)

clean:
	-(cd lib; make clean)
	rm -f *.o point

veryclean: clean
	rm -rf lib

tidy:
	(cd lib; )

libs:	libpgplot

libpgplot: lib
	(cd lib;\
	 ../pgplot/makemake ../pgplot linux ifort_icc;\
	make -i)

lib:
	mkdir lib
	cp drivers.list lib

pgplot:
	cvs -d :pserver:anonymous@cvs.astro.umd.edu:/home/cvsroot -Q co pgplot

bench1:
	chmod +x run1; ./run1
