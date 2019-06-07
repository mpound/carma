#!/bin/bash

PN="CImg"
PV="1.4.9"

src_unpack() {
	eunpack "$CARMA_PKG/$P.zip" || die "extract failed"
}

src_install() {
	mkdir -p "$PREFIX/include" || die "mkdir failed"
	cp -a CImg.h "$PREFIX/include/" || die "cp failed"
}
