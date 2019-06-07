#!/bin/bash

PN="mirlib"
PV="4.1.3b"

src_unpack() {
	eunpack "$CARMA_PKG/$P.tar.gz" || die "extract failed"
}

src_prepare() {
	# the miriad build system doesn't support a library mode, so
	# we manually compile together all the files properly with our
	# own Makefile
	cp "$FILESDIR/Makefile" "Makefile.mirlib" || die "cp failed"
}

src_compile() {
	emake -f "Makefile.mirlib" || die "compile failed"
}

src_install() {
	emake -f "Makefile.mirlib" install || die "install failed"
}
