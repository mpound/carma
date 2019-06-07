#!/bin/bash

PN="linux-gpib"
PV="3.2.11"

src_unpack() {
	eunpack "$CARMA_PKG/$P.tar.gz" || die "extract failed"
}

src_configure() {
	# fake stuff to quiet down compile without a kernel
	touch .config .hdepend

	econf --with-linux-srcdir="$PWD" || die "configure failed"
}

src_install() {
	for i in drivers/gpib/include include lib; do
		pushd "$i"
		emake || die "make $i failed"
		emake install || die "make install $i failed"
		popd
	done
}
