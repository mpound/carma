#!/bin/bash

PN="units"
PV="1.80"

src_unpack() {
	eunpack "$CARMA_PKG/$P.tar.gz" || die "extract failed"
}

src_prepare() {
	# CARMA patches this to add support for running as a shared library
	epatch "$FILESDIR/$PN-carma.patch" || die "patch failed"
	epatch "$FILESDIR/$PN-static.patch" || die "patch failed"
}

src_configure() {
	econf || die "configure failed"
}

src_compile() {
	emake libunits.so || die "compile failed"
}

src_install() {
	emake install || die "install failed"
}
