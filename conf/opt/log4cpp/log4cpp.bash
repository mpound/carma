#!/bin/bash

PN="log4cpp"
PV="1.0"

source "helpers/autotools-package.bash"

src_prepare() {
	# gentoo patches to fix build errors on newer gcc for version 1.0
	epatch "$FILESDIR/${PV}-asneeded.patch" || die "patch failed"
	epatch "$FILESDIR/${PV}-doc_install_path.patch" || die "patch failed"
	epatch "$FILESDIR/${PV}-gcc43.patch" || die "patch failed"

	# 0.3.4b has this problem (fixed in 1.0)
	#sed -i -e 's@AC_DEFUN(AM_PATH_LOG4CPP,@AC_DEFUN([AM_PATH_LOG4CPP],@' \
	#	log4cpp.m4 || die "sed failed"
}

src_configure() {
	#autoreconf -if
	econf --with-pthreads || die "configure failed"
}
