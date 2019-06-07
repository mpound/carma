#!/bin/bash

PN="fftw"
PV="3.2.2"

# enable single and double precision builds
FFTW_DIRS="single double"

src_unpack() {
	eunpack "$CARMA_PKG/$P.tar.gz" || die "extract failed"
}

src_prepare() {
	for x in ${FFTW_DIRS}; do
		mkdir -p "build-$x" || die "mkdir build-$x failed"
	done
}

src_configure() {
	for x in ${FFTW_DIRS}; do
		EXTRA_FLAGS="--enable-shared"
		if [[ "$x" == "single" ]]; then
			EXTRA_FLAGS="${EXTRA_FLAGS} --enable-float --enable-sse"
		elif [[ "$x" == "double" ]]; then
			EXTRA_FLAGS="${EXTRA_FLAGS} --enable-sse2"
		else
			die "precision $x not implemented in this ebuild"
		fi

		einfo "configuring for precision $x"
		pushd "build-$x"
		../configure --prefix="$PREFIX" $EXTRA_FLAGS || die "configure $x failed"
		popd
	done
}

src_compile() {
	for x in ${FFTW_DIRS}; do
		pushd "build-$x"
		emake || die "compile $x failed"
		popd
	done
}

src_install() {
	for x in ${FFTW_DIRS}; do
		pushd "build-$x"
		emake install || die "install $x failed"
		popd
	done
}
