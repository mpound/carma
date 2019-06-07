#!/bin/bash
# vim: set ts=4 sts=4 sw=4 noet:

PN="mongodb-linux-i686"
PV="2.6.4"

src_unpack() {
	eunpack "$CARMA_PKG/${P}.tgz" || die "extract failed"
}

src_install() {
	mkdir -p "${PREFIX}/mongodb" || die "mkdir failed"
	rm -rf "${PREFIX}/mongodb/${P}" || die "rm failed"
	cd .. || die "cd failed"
	mv "${P}" "${PREFIX}/mongodb/" || die "mv failed"
}
