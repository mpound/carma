# History:
# pre- Nov 2004:	classic novas-c201 support (SOLSYS=3)
# Dec 2004:			SOLSYS=4 support with ephcom tools

#  2  minor planets
#  3  no external files - no other dependencies
#  4  planet via ephcom (using jpl.eph)
SOLSYS=4

SRC   = invalid_solsys_$(SOLSYS)   
SRC2  = novas.c novascon.c solsys2.c readeph.c allocate.c chby.c jplint.f pleph.f
SRC3  = novas.c novascon.c solsys3.c readeph0.c
SRC4  = novas.c novascon.c solsys4.c readeph0.c
SRC5  = novas.c novascon.c solsys4.c readeph.c allocate.c chby.c
SRC6  = novas.c novascon.c solsys4.c 

ifeq ($(SOLSYS),2)
  SRC = $(SRC2)
  EPHCOM = 
  SUPPORT = minor-planets
endif
ifeq ($(SOLSYS),3)
  SRC = $(SRC3)
  EPHCOM = 
  SUPPORT = N/A, this is the standalone version without external files
endif
ifeq ($(SOLSYS),4)
  SRC = $(SRC4)
  EPHCOM = $(PREFIX)/lib/gnulliver.o $(PREFIX)/lib/ephcom.o
  SUPPORT = ephcom
endif
ifeq ($(SOLSYS),5)
  SRC = $(SRC5)
  EPHCOM = $(PREFIX)/lib/gnulliver.o $(PREFIX)/lib/ephcom.o
  SUPPORT = ephcom + minor-planets
endif

INC = novascon.h novas.h solarsystem.h
CPPFLAGS = -I$(PREFIX)/include

#   trick to chain a mix of .c and .f into .o
#   the compilation also is tricky, since we're depending on the compiler 
#   using the extension (gnu will do that)
OBJ1 = $(SRC:.c=.o)
OBJ  = $(OBJ1:.f=.o)

# shared libs
CFLAGS += -fPIC -ggdb -O2

.PHONY: all
all: libnovas.a libnovas.so

libnovas.a: $(OBJ)
	ar ruv $@ $^ $(EPHCOM)

libnovas.so: $(OBJ)
	$(CC) -shared $(CFLAGS) -o $@ $^ $(EPHCOM)

install: $(INC) libnovas.a libnovas.so
	install -m 644 $(INC) $(PREFIX)/include
	install -m 755 libnovas.a $(PREFIX)/lib
	install -m 755 libnovas.so $(PREFIX)/lib
	@echo "### Reminder: you have installed novas with SOLSYS=$(SOLSYS)"
	@echo "### Be sure the support libraries and data for this level have been installed:"
	@echo "### $(SUPPORT)"
