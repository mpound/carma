#!/bin/bash

PN="apache-ant"
PV="1.8.1"

src_unpack() {
	eunpack "$CARMA_PKG/$P-bin.tar.gz" || die "extract failed"
}

src_install() {
	mkdir -p "$PREFIX/ant" || die "mkdir failed"
	cp -a * "$PREFIX/ant" || die "copy failed"

	mkdir -p "$PREFIX/bin" || die "mkdir failed"
	ln -sf "$PREFIX/ant/bin/ant" "$PREFIX/bin/ant"
}
