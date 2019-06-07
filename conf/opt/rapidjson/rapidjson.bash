#!/bin/bash
# vim: set ts=4 sts=4 sw=4 noet:

PN="rapidjson"
PV="df70ee821989"

src_unpack() {
	eunpack "$CARMA_PKG/${P}.tar.gz" || die "extract failed"
}

src_install() {
	local TARGET="${PREFIX}/include/rapidjson"
	rm -rf "${TARGET}"
	mv "include/rapidjson" "${TARGET}"
}
