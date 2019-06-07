#!/bin/bash

PN="mysql"
PV="5.1.57"

source "helpers/autotools-package.bash"

# The mysql build system installs its libraries into $PREFIX/lib/mysql/
# by default. To inhibit this behavior, and install straight into
# $PREFIX/lib/, we redefine pkglibdir to the correct directory.
#
# See John Calcote's Autotools Pg 127-128.

src_configure() {
	econf --enable-thread-safe-client || die "configure failed"
}

src_compile() {
	emake pkglibdir='$(libdir)' || die "compile failed"
}

src_install() {
	# mysql places its libraries into $PREFIX/lib/mysql/ by default.
	# To inhibit this behavior, we redefine pkglibdir to a different
	# directory.
	emake pkglibdir='$(libdir)' install || die "install failed"
}
