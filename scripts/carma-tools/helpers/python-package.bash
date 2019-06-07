#!/bin/bash
# vim: set ts=4 sts=4 sw=4 noet:

# simple helpers for normal python packages
# which use setup.py
#
# anything can be overridden in the build file itself, just
# by re-defining the necessary function(s)

src_unpack() {
	eunpack "$CARMA_PKG/$P.tar.gz" || die "extract failed"
}

src_install() {
	python setup.py install || die "install failed"
}
