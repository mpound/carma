#!/bin/bash
# vim: set ts=4 sts=4 sw=4 noet:

PN="mongo-cxx-driver"
PV="26compat-2014-06-23"

# Run scons appropriately with the given target
# $1: target to build
runscons() {
	EXTRAFLAGS=""
	if is_32on64_build ; then
		EXTRAFLAGS="--32"
	fi

	# This does not actually respect CFLAGS/CXXFLAGS but it was much easier
	# to use the optimizations they hardcode rather than modifying their
	# build script. Seems like a reasonable tradeoff at this time.
	scons --prefix="$PREFIX" --full --jobs=$JOBS $EXTRAFLAGS \
		--use-system-boost --cpppath="$PREFIX/include" --libpath="$PREFIX/lib" \
		--disable-warnings-as-errors --sharedclient --opt=on "$1" \
		|| die "scons failed"
}

src_unpack() {
	eunpack "$CARMA_PKG/${P}.tar.gz" || die "extract failed"
}

src_prepare() {
	epatch "$FILESDIR/$P-force-rpath.patch" || die "patch failed"
}

src_compile() {
	runscons "mongoclient" || die "scons failed"
}

src_install() {
	runscons "install-mongoclient" || die "scons failed"
}
