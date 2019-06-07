#!/bin/bash

PN="jfreechart"
PV="1.0.15"

src_unpack() {
	eunpack "$CARMA_PKG/$P.tar.gz" || die "extract failed"
}

src_install() {
	cp lib/*.jar "$PREFIX/lib" || die "cp failed"
}
