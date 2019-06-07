#!/bin/bash

PN="cobra-bitfiles"
PV="1.7"

src_unpack() {
	eunpack "$CARMA_PKG/$P.tar.gz" || die "extract failed"
}

src_install() {
	mkdir -p "$PREFIX/$PN" || die "mkdir failed"
	cp -r * "$PREFIX/$PN" || die "cp failed"
}
