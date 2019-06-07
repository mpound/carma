#!/bin/bash

PN="ACE"
PV="5.8.1"

source "helpers/autotools-package.bash"

TMPBUILD="build"

src_unpack() {
	eunpack "$CARMA_PKG/$P.tar.bz2" || die "extract failed"
}

src_prepare() {
	mkdir -p $TMPBUILD || die "mkdir failed"
}

src_configure() {
	cd $TMPBUILD || die "cd failed"
	../configure --prefix="$PREFIX" --disable-ssl || die "configure failed"
}

src_compile() {
	cd $TMPBUILD || die "cd failed"
	emake || die "compile failed"
}

src_install() {
	cd $TMPBUILD || die "cd failed"
	emake install || die "install failed"

	# is this even necessary anymore? It causes lots of issues...
	#cd "$PREFIX/lib/pkgconfig" || die "cd failed"
	#sed -i -e 's@-lACE@-Wl,-rpath,$${libdir} &@' ACE.pc || die "sed failed"
}
