#!/bin/bash

PN="mysql-connector-odbc"
PV="5.1.8"

source "helpers/autotools-package.bash"

src_configure() {

	# their configure script sucks
	if [[ "$(uname -m)" == "x86_64" ]]; then
		EXTRA_CONF="--with-qt-libraries=/usr/lib64/qt-3.3/lib"
	fi

	econf --with-mysql-path="$PREFIX" \
		  --with-unixODBC="$PREFIX" \
		  --with-unixODBC-includes="$PREFIX/include/unixODBC" \
		  --enable-static=yes --with-gnu-ld \
		  --disable-gui --disable-test \
		  $EXTRA_CONF || die "configure failed"
}
