#!/bin/bash
# vim: set ts=4 sts=4 sw=4 noet:

PN="xml2json"
PV="16a11905d078"

src_unpack() {
	eunpack "$CARMA_PKG/${P}.tar.gz" || die "extract failed"
}

src_install() {
	local TARGET="${PREFIX}/include/xml2json"
	rm -rf "${TARGET}"
	mv "include" "${TARGET}"
}
