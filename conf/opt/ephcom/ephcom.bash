#!/bin/bash

PN="ephcom"
PV="1.0-carma2"

src_unpack() {
	eunpack "$CARMA_PKG/$P.tar.gz" || die "extract failed"
}

src_compile() {
	emake CFLAGS="$CFLAGS -fPIC" || die "compile failed"
}

src_install() {
	# the ephcom installer sucks, it doesn't create its directories
	mkdir -p "$PREFIX/bin" || die "mkdir failed"
	mkdir -p "$PREFIX/man" || die "mkdir failed"
	mkdir -p "$PREFIX/man/man1" || die "mkdir failed"

	emake install BINDIR="$PREFIX/bin" MAN1DIR="$PREFIX/man" || die "install failed"
	cp ephcom.h "$PREFIX/include" || die "cp failed"

	# this is a dirty hack for novas
	cp ephcom.o gnulliver.o "$PREFIX/lib" || die "cp failed"

	# install ephemeris files
	EPH_DIR="$PREFIX/data/ephem"
	mkdir -p "$EPH_DIR"
	cp "$CARMA_PKG/unxp2000.405" "$CARMA_PKG/testpo.405" "$EPH_DIR" || die "cp failed"

	pushd "$EPH_DIR"
	rm -f jpl.eph || die "rm jph.eph failed"
	ln -sf unxp2000.405 jpl.eph || die "ln -sf failed"
	popd
}
