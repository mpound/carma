#!/bin/bash
# vim: set ts=4 sts=4 sw=4 noet:

stderr() {
	echo -e "$@" >&2
}

# Insert ctrl character
# ctrl-V then esc will print ^[
# ctrl-V then ctrl-shift-m will print ^M
BACK_UP="\033[1K\033[0G"
NORMAL="\033[0m"
WARN="\033[33;1m"
BAD="\033[31;1m"
BOLD="\033[1m"
GOOD="\033[32;1m"

# msg functions arguments
# $1 string
# $2 hide flag

einfo() {
	msg_string=$1
	msg_string="${msg_string:-...}"
	[ "$2" != 1 ] && echo -e "${GOOD}*${NORMAL}${BOLD} ${msg_string} ${NORMAL}"
}

eerror() {
	msg_string=$1
	msg_string="${msg_string:-...}"
	[ "$2" != 1 ] && echo -e "${BAD}*${NORMAL}${BOLD} ${msg_string} ${NORMAL}"
}

ewarn() {
	msg_string=$1
	msg_string="${msg_string:-...}"
	[ "$2" != 1 ] && echo -e "${WARN}*${NORMAL}${BOLD} ${msg_string} ${NORMAL}"
}

estep() {
	msg_string=$1
	msg_string="${msg_string:-...}"
	[ "$2" != 1 ] && echo -e "${GOOD}>>${NORMAL}${BOLD} ${msg_string} ${NORMAL}"
}

# Unpack a source archive
#
# $1: the source archive to unpack
eunpack() {
	# make sure the file exists
	if [[ ! -f "$1" ]]; then
		eerror "file does not exist: $1"
		exit 1
	fi

	# bzipped tarball
	pattern='\.tar\.bz2$|\.tbz2$'
	if [[ $1 =~ $pattern ]]; then
		tar xjf "$1" ; return $?
	fi

	# gzipped tarball
	pattern='\.tar\.gz$|\.tgz$'
	if [[ $1 =~ $pattern ]]; then
		tar xzf "$1" ; return $?
	fi

	# zip archive
	pattern='\.zip$'
	if [[ $1 =~ $pattern ]]; then
		unzip "$1" ; return $?
	fi

	eerror "unknown archive format: $1"
	exit 1
}

# Automatically apply a patch
#
# $1: the patch to apply
epatch() {
	# make sure that the patch exists
	if [[ ! -f "$1" ]]; then
		eerror "file does not exist: $1"
		exit 1
	fi

	# try patch levels 0-5
	EPATCH_OPTS="-g0 -E --no-backup-if-mismatch"
	for i in {0..5}; do
		# try to apply the patch with increasing -p# options
		if ! patch -p$i $EPATCH_OPTS --dry-run -f < "$1" &>/dev/null ; then
			continue
		fi

		# ok, it worked, apply it for real
		einfo "applying patch $1 (-p$i)"
		if ! patch -p$i $EPATCH_OPTS < "$1" ; then
			eerror "failed to apply patch $1 (-p$i)"
			exit 1
		fi

		# we don't need to continue the loop anymore
		return 0
	done

	# oops, we didn't try enough patch levels, error
	eerror "patch $1 did not apply with -p0 to -p5"
	exit 1
}

is_32on64_build() {

	# CFLAGS or CXXFLAGS have the -m32 option in them
	if [[ "$CFLAGS" =~ "-m32" || "$CXXFLAGS" =~ "-m32" ]]; then
		return 0
	fi

	# any other cases is not a 32-on-64 build
	return 1
}

# check if a function is declared
#
# $1: the function name
have_function() {
	# it exists, success
	if declare -F "$1" &> /dev/null ; then
		return 0
	fi

	# it did not exist, failure
	return 1
}

# check if a variable is declared
#
# $1: the variable name
have_variable() {
	# it exists, success
	if declare -p "$1" &> /dev/null ; then
		return 0
	fi

	# it did not exist, failure
	return 1
}
