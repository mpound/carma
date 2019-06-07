#!/bin/bash
# vim: set ts=4 sts=4 sw=4 noet:

PN="comedilib"
PV="0.10.1"

src_unpack() {
	eunpack "$CARMA_PKG/$P.tar.gz" || die "extract failed"
}

src_configure() {
	# Remove some files that prevent autoreconf from running correctly
	rm Makefile Makefile.in libtool aclocal.m4 compile

	# The package has been broken by Dave MacMahon. When he re-packaged
	# it for CARMA, he used the autotools in symlink mode instead of copy
	# mode. Whoops.
	autoreconf -if || die "autoreconf failed"
	econf --disable-{rtai,usb,kbuild} --disable-{python,ruby}-binding || die "configure failed"
}

src_compile() {
	emake || die "compile failed"
}

src_install() {
	emake install || die "install failed"
}
