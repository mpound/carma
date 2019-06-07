#!/bin/bash

PN="xerces-c"
PV="3.1.1"

# this is a well behaved autotooled package
source "helpers/autotools-package.bash"

src_configure() {
	# disable sse2 support (not supported on cPCI CPUs)
	econf --disable-sse2 || die "configure failed"
}
