#!/bin/bash

PN="idldepend"
PV="1-3-0"

src_unpack() {
	eunpack "$CARMA_PKG/$P.tar.gz" || die "extract failed"
}

src_compile() {
	ANT_HOME="$PREFIX/ant" ant --noconfig || die "ant failed"
}

src_install() {
	mkdir -p "$PREFIX/lib" || die "mkdir failed"
	cp idldepend.jar "$PREFIX/lib/" || die "cp failed"
}
