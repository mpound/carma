#!/bin/bash

PN="ACE+TAO"
PV="5.8.1-carma-1.5"

setup_ace_env() {
	export ACE_ROOT="$PWD"
	export TAO_ROOT="$PWD/TAO"
	export LD_LIBRARY_PATH="$PWD/lib"
}

src_unpack() {
	eunpack "$CARMA_PKG/$P.tar.bz2" || die "extract failed"
}

src_configure() {
	setup_ace_env

	# this will cause ACE+TAO to die, their build system is broken
	if [[ "$(readlink -f "$PWD")" != "$PWD" ]]; then
		die "ACE+TAO build path must not contain symlinks"
	elif [[ "$(readlink -f "$PREFIX")" != "$PREFIX" ]]; then
		die "ACE+TAO install path must not contain symlinks"
	fi

	# chose the correct number of build bits
	if is_32on64_build ; then
		BUILDBITS="32"
	else
		BUILDBITS="$(getconf LONG_BIT)"
	fi

	einfo "building in ${BUILDBITS}-bit mode"

	# generate the correct build setup
	PLAT_MACROS="include/makeinclude/platform_macros.GNU"
	cat > "include/makeinclude/platform_macros.GNU" <<-EOF
	debug=1
	optimize=1
	shared_libs_only=1
	threads=1
	buildbits=$BUILDBITS
	INSTALL_PREFIX=$PREFIX
	include $ACE_ROOT/include/makeinclude/platform_linux.GNU
	EOF

	# remove stuff we don't use to speed up compilation
	rm -rf apps/{JAWS*,drwho,Gateway,mkcsregdb,soreduce} contrib
	rm -rf tests examples performance-tests
	rm -rf protocols/{examples,tests}
	rm -rf TAO/{examples,tests,performance-tests}
	rm -rf TAO/orbsvcs/{examples,tests,performance-tests}
	rm -rf TAO/DevGuideExamples TAO/orbsvcs/DevGuideExamples

	# this causes build failures on mpound's system
	rm -rf apps/gperf/tests

	einfo "Generating GNU makefiles with MPC - this takes forever (10+ minutes)"
	$ACE_ROOT/bin/mwc.pl -type gnuace -recurse || die "mwc.pl failed"
}

src_compile() {
	setup_ace_env

	pushd "$ACE_ROOT"
	einfo "compiling ACE"
	emake || die "compile ACE failed"
	popd

	pushd "$TAO_ROOT"
	einfo "compiling TAO"
	emake || die "compile TAO failed"
	popd
}

src_install() {
	setup_ace_env

	pushd "$ACE_ROOT"
	einfo "installing ACE"
	emake install || die "install ACE failed"

	einfo "creating hacked ACE.pc"
	PKGCONFIG_INSTALL_DIR="$PREFIX/lib/pkgconfig"
	mkdir -p "$PKGCONFIG_INSTALL_DIR" || die "mkdir failed"
	echo "prefix=$PREFIX" > "$PKGCONFIG_INSTALL_DIR/ACE.pc"
	/bin/cat ./ACE.pc >> "$PKGCONFIG_INSTALL_DIR/ACE.pc"
	popd

	pushd "$TAO_ROOT"
	einfo "installing TAO"
	emake install || die "install TAO failed"
	popd
}
