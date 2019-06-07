#!/bin/bash
# vim: set ts=4 sts=4 sw=4 noet:

# This script will run a single step of the build. It handles
# setting up the environment correctly, etc. It has minimal
# error checking.

# try to get to our real path
cd "$(dirname "$(readlink -f "$0")")"
export MANAGERPATH="$PWD"

# pull in useful functions
source misc-functions.bash

die() {
	eerror "$@"
	exit 1
}

emake() {
	make -j$JOBS "$@"
}

econf() {
	./configure --prefix="$PREFIX" "$@"
}

cd_extract_dir() {
	cd "$WORKDIR/$S"
	if [[ $? -ne 0 ]]; then
		eerror "ERROR: directory '$WORKDIR/$S' does not exist"
		eerror "ERROR: check your definition of the S variable"
		exit 1
	fi
}

# Do various tasks at the beginning of each step
#
# $1: the string for the step, such as "unpack $P"
start_step() {
	# log the date + time of the start of the step
	echo "$(date) $1" >> "$PREFIX/step.log"

	# set the xterm title to match the current step
	if [[ $TERM == "screen" ]]; then
		echo -ne "\033k$1\033\\"
	else
		echo -ne "\033]0;$1\007"
	fi

	# print the step message to the screen
	estep "$1"
}

# setup environment variables for the build
#
# REQUIRED:
# - PN			package name
# - PV			package version
#
# OPTIONAL:
# - S			source directory after extraction (default to "$PN-$PV")
# - P			package name + version mostly for display (default to "$PN-$PV")
#
# The rationale for diverging from portage and requiring PN and PV to
# be present in the build file is so that changing the version of package
# only requires changes in a single place. New versions often have different
# build setups.
#
# We don't want to have to revert changes in two places:
# - the build script
# - conf/etc/packages.dir (version info)
#
# I myself have run into this problem on several occasions, which is
# why I have provided a solution to the problem here.
#
# Removing the need for packages.dir also has another benefit: people have
# been very lazy and used a single Makefile for multiple packages. The ant
# package is a good example.
#
# This allows people to use as many source files as they need in their
# build script. We don't care anymore.
#
setup_variables() {
	# must have PN in the build script
	if ! have_variable PN ; then
		die "ERROR: build file does not export PN"
	fi

	# must have PV in the build script
	if ! have_variable PV ; then
		die "ERROR: build file does not export PV"
	fi

	# setup the default value for S
	if ! have_variable S ; then
		export S="$PN-$PV"
	fi

	# setup the default value for P
	if ! have_variable P ; then
		export P="$PN-$PV"
	fi

	# we always force the workdir
	export WORKDIR="$BUILDDIR/$PN"

	# we choose the filesdir based on PN
	export FILESDIR="$(dirname "$BUILD_SCRIPT")"

	# always add our tools to the beginning of the path
	export PATH="$PREFIX/usr/bin:$PREFIX/bin:$JAVA_HOME/bin:$PATH"
}

# Check the environment upon entry to this script
check_environment() {
	# FIXME TODO: add DESTDIR when you add support for checked installations
	for var in BUILDDIR CARMA_PKG PREFIX JAVA_HOME JOBS ; do
		if ! have_variable "$var" ; then
			die "ERROR: required environment variable $var missing"
		fi
	done
}

# Sanitize the environment by removing things we know to be bad
#
# If this proves to be too hard to maintain, we can always remove
# everything from the environment and setup our own known-good
# environment with exactly what we want.
sanitize_environment() {
	# Anyone setting this system-wide is insane
	unset LD_LIBRARY_PATH

	# CDPATH is known to cause breakage
	unset CDPATH

	# No non-standard python path, please
	unset PYTHONPATH

	# No stupid paths, non-superuser system binaries only, please
	export PATH="/usr/local/bin:/usr/bin:/bin"
}

run_check_version() {
	# if INSTALLED file does not exist, then exit with an error
	if [[ ! -f "$PREFIX/INSTALLED" ]]; then
		die "ERROR: the file $PREFIX/INSTALLED does not exist"
	fi

	# find latest version of installed package
	INST_PV="$(tac "$PREFIX/INSTALLED" | awk "{ if (\$1 == \"$PN\") { print \$2; exit; }}")"

	if [[ -z "$INST_PV" ]]; then
		die "ERROR: package $PN is not installed"
	fi

	if [[ "$PV" != "$INST_PV" ]]; then
		die "WARNING: package $PN has installed version $INST_PV (latest version is $PV)"
	fi
}

run_unpack() {
	start_step "unpack $P"

	# src_unpack() must exist, no matter what
	if ! have_function src_unpack ; then
		die "ERROR: package $P missing src_unpack()"
	fi

	# make sure there are no stale build files
	rm -rf "$WORKDIR"

	# create a brand new build directory
	mkdir -p "$WORKDIR"

	# change into it and run the src_unpack() function
	cd "$WORKDIR"
	src_unpack
}

run_prepare() {
	start_step "prepare $P"

	# default if src_prepare() is not present
	if ! have_function src_prepare ; then
		echo "package $P does not need preparation"
		return 0
	fi

	# change into the extracted directory, run src_prepare()
	cd_extract_dir
	src_prepare
}

run_configure() {
	start_step "configure $P"

	# default if src_configure() is not present
	if ! have_function src_configure ; then
		echo "package $P does not need configuration"
		return 0
	fi

	# change into the extracted directory, run src_configure()
	cd_extract_dir
	src_configure
}

run_compile() {
	start_step "compile $P"

	# default if src_compile() is not present
	if ! have_function src_compile ; then
		echo "package $P does not need compilation"
		return 0
	fi

	# change into the extracted directory, run src_compile()
	cd_extract_dir
	src_compile
}

run_test() {
	start_step "test $P"

	# default if src_test() is not present
	if ! have_function src_test ; then
		echo "package $P does not have a test suite"
		return 0
	fi

	# change into the extracted directory, run src_test()
	cd_extract_dir
	src_test
}

run_install() {
	start_step "install $P"

	# default if src_install() is not present
	if ! have_function src_install ; then
		echo "package $P does not need installation"
		return 0
	fi

	# change into the extracted directory, run src_install()
	cd_extract_dir
	src_install
}

run_finalize() {
	start_step "finalize $P"

	echo "$PN $PV $(date)" >> "$PREFIX/INSTALLED"
	rm -rf "$WORKDIR"
}

# save the arguments in convenient names
STEP_NAME="$1"
BUILD_SCRIPT="$2"

# check the required environment
check_environment

# sanitize the environment
sanitize_environment

# pull in the build file itself
source "$BUILD_SCRIPT" || die "error in file: $BUILD_SCRIPT"

# setup the environment
setup_variables

# run a single step of the build
run_$STEP_NAME
