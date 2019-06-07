#!/bin/bash

PN="cobra"
PV="2.99.0"

source "helpers/autotools-package.bash"

src_configure() {
	export PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig:$PKG_CONFIG_PATH"

	# matlab support is broken on 32-on-64 builds: it does not provide
	# a way to cross compile correctly. Turn it off.
	if [[ -e "/opt/Matlab/bin/matlab" && ! is_32on64_build ]]; then
		MATLAB_FLAG="--with-matlab=/opt/Matlab"
	fi

	# binaries go into $PREFIX/cobra
	# boost comes from $PREFIX
	econf --bindir="$PREFIX/$PN" --with-boost="$PREFIX" $MATLAB_FLAG || die "configure failed"
}
