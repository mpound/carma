#!/bin/bash

PN="janz"
PV="2.3"

src_unpack() {
	eunpack "$CARMA_PKG/$P.tar.gz" || die "extract failed"
}

# The Janz Makefile sucks: it always rebuilds too much and it doesn't
# compile correctly in parallel mode. Ugh.
#
# Therefore, to save time, we do the build as part of the install step.
src_install() {

	# the build system really sucks, and whoever modified it for
	# CARMA really didn't do it correctly. In addition, it doesn't
	# pick up stuff from the environment at all, so we have to
	# explicitly specify it on the command line.
	if is_32on64_build ; then
		export CC="$CC -m32"
		export LDSHARED="$CC -m32 -shared"
		emake -j1 lib_install INSTALLDIR="$PREFIX" \
			CC="$CC" LDSHARED="$LDSHARED" \
			|| die "install failed"
	else
		emake -j1 lib_install INSTALLDIR="$PREFIX" || die "install failed"
	fi
}
