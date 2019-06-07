#!/bin/bash

PN="gt"
PV="4.0.1"

# very non-standard archive layout :(
S="${PN}${PV}-all-source-installer"

# we only need the jars: save extraction time
JARSOURCE="source-trees/wsrf/java/common/source/lib"

src_unpack() {
	# extract as little as possible to save time
	tar xzf "$CARMA_PKG/$P.tar.gz" "$S/$JARSOURCE" || die "extract failed"
}

src_install() {
	cd "$JARSOURCE" || die "cd failed"
	chmod 644 *.jar
	cp *.jar "$PREFIX/lib" || die "cp failed"
}
