#!/bin/bash

PN="launch4j"
PV="3.0.2-linux"

# weird tarball structure
S="launch4j"

src_unpack() {
	eunpack "$CARMA_PKG/$P.tgz" || die "extract failed"
}

src_install() {
	local L4JDIR="$PREFIX/lib/launch4j"

	mkdir -p "$L4JDIR" "$PREFIX/bin" || die "mkdir failed"
	cp -a * "$L4JDIR" || die "cp failed"
	ln -sf "$L4JDIR/launch4j" "$PREFIX/bin/launch4j" || die "ln failed"
	ln -sf "$L4JDIR/launch4j.jar" "$PREFIX/bin/launch4j.jar" || die "ln failed"
}
