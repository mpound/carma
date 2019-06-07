#!/bin/bash

PN="omniORB"
PV="4.1.6"

source "helpers/autotools-package.bash"

src_unpack() {
	eunpack "$CARMA_PKG/$P.tar.bz2" || die "extract failed"
}

src_prepare() {
	# shamelessly stolen from the Gentoo build
	sed -i -e 's/^CXXDEBUGFLAGS.*/CXXDEBUGFLAGS = $(OPTCXXFLAGS)/' \
		-e 's/^CDEBUGFLAGS.*/CDEBUGFLAGS = $(OPTCFLAGS)/' \
		mk/beforeauto.mk.in \
		mk/platforms/i586_linux_2.0*.mk || die "sed failed"

	# shamelessly stolen from the Gentoo build
	epatch "$FILESDIR/ldflags.patch" || die "patch failed"
}

src_configure() {
	if is_32on64_build ; then
		export LDFLAGS="-m32"
	fi

	econf --with-omniORB-config="$FILESDIR/omniORB-carma.cfg" || die "configure failed"
}

src_compile() {
	# the x86_64 32-on-64 build doesn't define this. CARMA-tools is
	# only built on x86 and x86_64, so just handle it.
	EXTRA_FLAGS="-D__x86__"
	emake OPTCFLAGS="$CFLAGS $EXTRA_FLAGS" \
		  OPTCXXFLAGS="$CXXFLAGS $EXTRA_FLAGS" \
		  || die "compile failed"
}
