#!/bin/bash

PN="protobuf"
PV="2.5.0"

# this is a well behaved autotooled package
source "helpers/autotools-package.bash"

src_install() {
	emake install || die "install failed"

	# During install, sometimes the protocol buffer source is timestamped
	# after the header it created. This causes the CARMA RTS Makefile to
	# try and regenerate the protocol buffer source in CARMA-TOOLS. Fix it
	# by ensuring the timestamp of the header comes after the source.
	#
	# This is probably an NFS issue. There are known issues with timestamps
	# on NFS.
	sleep 5
	find "$PREFIX/include/google/protobuf" -iname '*.pb.h' -exec touch '{}' \;

	# copy the required jar (pre-built)
	cp "${CARMA_PKG}/${PN}-java-${PV}.jar" "$PREFIX/lib/" || die "cp failed"
}
