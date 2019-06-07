SRC = bug.c hio.c key.c pack.c uvio.c xyzio.c dio.c headio.c interface.c maskio.c scrio.c xyio.c
OBJ = $(SRC:.c=.o)

# we want a 64-bit capable shared library
CFLAGS+=-D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE64_SOURCE -fPIC

.PHONY: all install
all: libmir.a libmir.so uvio

libmir.a: $(OBJ)
	ar ruv $@ $^

libmir.so: $(OBJ)
	$(CC) -shared $(CFLAGS) -o $@ $^

# WARNING:
#
# The uvio program needs uvio.c compiled with -DTESTBED, so that we get
# the main() function compiled in.
#
# However, libmir.so also needs some functions from uvio.c, but MUST NOT
# have the main() function compiled in, or programs linking against it
# will be very broken.
#
# Therefore, we use a different object file when compiling the uvio program
# and define -DTESTBED only there. This is exactly how automake would've
# done it for us.
uvio-testbed.o: uvio.c
	$(CC) -c -o $@ $< $(CFLAGS) -DTESTBED

uvio: uvio-testbed.o libmir.a
	$(CC) $(CFLAGS) -o $@ $^ -lm

install: libmir.a libmir.so uvio miriad.h sysdep.h
	install -m 755 libmir.a "$(PREFIX)/lib"
	install -m 755 libmir.so "$(PREFIX)/lib"
	install -m 755 uvio "$(PREFIX)/bin"
	install -m 644 miriad.h sysdep.h "$(PREFIX)/include"
