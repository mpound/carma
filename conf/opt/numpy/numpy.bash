#!/bin/bash
# vim: set ts=4 sts=4 sw=4 noet:

PN="numpy"
PV="1.6.0"

# fix environment to make 32-on-64 bit builds work correctly
numpy_setup_env() {
	if is_32on64_build ; then
		export CC="$CC -m32"
	fi
}

src_unpack() {
	eunpack "$CARMA_PKG/$P.tar.gz" || die "extract failed"
}

src_prepare() {
	if is_32on64_build ; then
		epatch "${FILESDIR}/${P}-fix-32bit-build.patch" || die "patch failed"
	fi
}

src_configure() {
	numpy_setup_env
	python setup.py config --fcompiler=gnu95
}

src_install() {
	numpy_setup_env
	python setup.py install
}
