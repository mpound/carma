#!/bin/bash

PN="python"
PV="2.7.2"

MY_PN="Python"
MY_P="$MY_PN-$PV"
S="$MY_PN-$PV"

# this is a mostly well behaved autotooled package (overrides below)
source "helpers/autotools-package.bash"

src_unpack() {
	eunpack "$CARMA_PKG/$MY_P.tgz" || die "extract failed"
}

src_configure() {
	if is_32on64_build ; then
		MY_CFLAGS="-m32"
	fi

	econf CC="$CC $MY_CFLAGS" CXX="$CXX $MY_CFLAGS" || die "configure failed"
}

src_install() {
	# Python dies on parallel install
	emake -j1 install || die "install failed"
}
