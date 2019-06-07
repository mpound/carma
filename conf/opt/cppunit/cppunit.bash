#!/bin/bash

PN="cppunit"
PV="1.12.1"

# cppunit is a well behaved autotooled package
source "helpers/autotools-package.bash"

src_prepare() {
	# Gentoo patches - fix build problems on >=gcc-4.6
	epatch "$FILESDIR/$PN-1.10.2-asneeded.patch" || die "patch failed"
	epatch "$FILESDIR/$P-add_missing_include.patch" || die "patch failed"
	epatch "$FILESDIR/$P-warnings.patch" || die "patch failed"

	# fixup so any version of autoreconf works
	rm -f aclocal.m4 configure Makefile.in

	autoreconf -if || die "autoreconf failed"
}
