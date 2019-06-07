#!/bin/bash

PN="libcanberra"
PV="0.22-carma1"

source "helpers/autotools-package.bash"

src_configure() {
	econf --disable-gtk --disable-oss || die "configure failed"
}
