#!/bin/bash

PN="ant-contrib"
PV="1.0b3"

# the ant-contrib package uses a non-versioned directory
S="ant-contrib"

src_unpack() {
	eunpack "$CARMA_PKG/$P-bin.zip" || die "extract failed"
}

src_install() {
	mkdir -p "$PREFIX/ant/lib" || die "mkdir failed"
	cp "$P.jar" "$PREFIX/ant/lib/" || die "cp failed"
}
