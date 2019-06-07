#!/bin/bash

PN="xz-java"
PV="1.3"

src_unpack() {
	# xz-java didn't package their code normally, they forgot to use
	# a directory named "xz-java-1.3" as the top level inside their zip
	mkdir "$P" || die "mkdir failed"
	cd "$P" || die "cd failed"
	eunpack "$CARMA_PKG/$P.zip" || die "extract failed"
}

src_compile() {
	ANT_HOME="$PREFIX/ant" ant || die "build failed"
}

src_install() {
	mkdir -p "$PREFIX/lib" || die "mkdir failed"
	cp build/jar/xz.jar "$PREFIX/lib" || die "cp failed"
}
