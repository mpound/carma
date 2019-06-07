#!/bin/bash

PN="jacorb"
PV="3.2"

src_unpack() {
	eunpack "$CARMA_PKG/$P-binary.zip" || die "extract failed"
}

src_install() {
	mkdir -p "$PREFIX"/{bin,lib,etc} || die "mkdir failed"
	cp bin/jaco "$PREFIX/bin" || die "cp jaco failed"
	for i in dior fixior idl irbrowser lsns nmg pingo qir ; do
		cp "bin/$i" "$PREFIX/bin/jaco_$i" || die "cp $i failed"
	done

	cp lib/*.jar "$PREFIX/lib" || die "cp failed"
	cp etc/jacorb_properties.template "$PREFIX/etc/jacorb.properties" || die "cp failed"
}
