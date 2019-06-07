#!/bin/bash

PN="novas"
PV="c201-carma7"

# SOFA libraries:
# http://www.iau-sofa.rl.ac.uk/2001_0331/Timescales.html

src_unpack() {
	eunpack "$CARMA_PKG/$P.tar.gz" || die "extract failed"
}

src_prepare() {
	cp "$FILESDIR/Makefile" .
}

src_compile() {
	emake PREFIX="$PREFIX" || die "compile failed"
}

src_install() {
	emake install PREFIX="$PREFIX" || die "install failed"
}
