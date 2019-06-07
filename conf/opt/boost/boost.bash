#!/bin/bash
# vim: set ts=4 sts=4 sw=4 noet:

PN="boost"
PV="1_55_0"

# boost uses a funny directory structure
S="${PN}_${PV}"

src_unpack() {
	eunpack "$CARMA_PKG/${PN}_${PV}.tar.gz" || die "extract failed"
}

src_configure() {
	./bootstrap.sh --prefix="$PREFIX" --without-libraries=mpi,wave,python \
				   --without-icu || die "bootstrap failed"

	# we must add the "-m32" flag correctly when we're doing a
	# 32-on-64 build due to strange buildsystem issues
	if is_32on64_build ; then
		einfo 'writing custom user-config.jam build instructions'
		cat > "tools/build/v2/user-config.jam" <<-EOF
		using gcc : `gcc -dumpversion` : $CXX -m32 ;
		EOF
	fi
}

# Relies on CXXFLAGS for debugging turned on
#
# We use --layout=system to install into $PREFIX/include/boost rather
# than $PREFIX/include/boost_1_43_0

src_compile() {
	./bjam -d+2 -j$JOBS --prefix="$PREFIX" --layout=system \
		   threading=multi link=shared runtime-link=shared \
		   dll-path="$PREFIX/lib" \
		   || die "bjam compile failed"
}

src_install() {
	./bjam -d+2 -j$JOBS --prefix="$PREFIX" --layout=system \
		   threading=multi link=shared runtime-link=shared install \
		   dll-path="$PREFIX/lib" \
		   || die "bjam install failed"
}
