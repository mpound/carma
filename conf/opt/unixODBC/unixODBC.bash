#!/bin/bash

PN="unixODBC"
PV="2.3.0"

source "helpers/autotools-package.bash"

src_configure() {
	# --x-include and --x-lib are on the configure line because without
	# them, non-iteracitve builds (e.g. those run from crontab jobs) fail
	# to find these areas :(
	econf --x-include="/usr/X11R6/include" --x-lib="/usr/X11R6/lib" \
		  --enable-static=yes --includedir="$PREFIX/include/unixODBC" \
		  || die "configure failed"
}
