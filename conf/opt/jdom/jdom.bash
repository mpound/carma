#!/bin/bash

PN="jdom"
PV="1.1.1"

# jdom doesn't use a versioned directory structure
S="jdom"

src_unpack() {
	eunpack "$CARMA_PKG/$P.tar.gz" || die "extract failed"
}

src_compile() {
	ANT_HOME="$PREFIX/ant" ./build.sh || die "build failed"
}

src_install() {
	mkdir -p "$PREFIX/lib" || die "mkdir failed"
	cp build/jdom.jar "$PREFIX/lib" || die "cp failed"
}
