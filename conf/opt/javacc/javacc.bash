#!/bin/bash
# vim: set ts=4 sts=4 sw=4 noet:

PN="javacc"
PV="5.0"

src_unpack() {
	eunpack "$CARMA_PKG/${P}src.tar.gz" || die "extract failed"
}

src_compile() {
	ANT_HOME="$PREFIX/ant" ant --noconfig || die "compile failed"
}

src_install() {
	mkdir -p "$PREFIX/lib" "$PREFIX/bin" || die "mkdir failed"
	cp bin/lib/javacc.jar "$PREFIX/lib" || die "cp failed"
	cp bin/{javacc,jjdoc,jjtree} "$PREFIX/bin" || die "cp failed"

	mkdir -p "$PREFIX/bin/lib" || die "mkdir failed"
	ln -sf "$PREFIX/lib/javacc.jar" "$PREFIX/bin/lib/javacc.jar" || die "ln failed"
}
