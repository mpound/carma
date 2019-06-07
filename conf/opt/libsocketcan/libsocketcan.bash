#!/bin/bash

PN="libsocketcan"
PV="carma-1.1"
S="libsocketcan"

source "helpers/autotools-package.bash"

# Only build if support for SocketCAN exists in the Linux headers
if [[ ! -e "/usr/include/linux/can.h" ]]; then
	echo "WARNING: your linux installation lacks support for SocketCAN"
	unset src_prepare
	unset src_configure
	unset src_compile
	unset src_test
	unset src_install
fi
