#!/bin/bash

PN="bgftp"
PV="1.0.2"

# bgftp is mostly well behaved
source "helpers/autotools-package.bash"

src_prepare() {
	# Fix autotools input scripts to match the recommended practices
	# and allow building in 32-bit on a 64-bit machine
	epatch "$FILESDIR/$P-flags.patch" || die "patch failed"
}

src_configure() {
	ANT_HOME="$PREFIX/ant"
	GLOBUS_DIR="$PREFIX"

	# remove autotools stuff which should not have been packaged
	rm -rf config
	rm -f aclocal.m4 configure
	find . -name Makefile.in -exec rm -f '{}' \;

	autoreconf -if
	econf --with-java-home="$JAVA_HOME" \
		  --with-ant-home="$ANT_HOME" \
		  --with-globus-dir="$GLOBUS_DIR" || die "configure failed"
}
