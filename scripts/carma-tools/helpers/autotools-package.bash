#!/bin/bash
# vim: set ts=4 sts=4 sw=4 noet:

src_unpack() {
	eunpack "$CARMA_PKG/$P.tar.gz" || die "extract failed"
}

src_configure() {
	econf || die "configure failed"
}

src_compile() {
	emake || die "compile failed"
}

src_install() {
	emake install || die "install failed"
}
