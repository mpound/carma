#!/bin/bash

PN="guava"
PV="14.0.1"

src_unpack() {
	# nothing fancy needed, just copy the jar into the work area
	mkdir -p "$P" || die "mkdir failed"
	cp "$CARMA_PKG/$P.jar" "$P" || die "cp failed"
}

src_install() {
	mkdir -p "$PREFIX/lib" || die "mkdir failed"
	cp "$P.jar" "$PREFIX/lib" || die "cp failed"
}
