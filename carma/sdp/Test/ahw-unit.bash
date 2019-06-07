#!/bin/bash
# vim: set ts=4 sts=4 sw=4 noet:

# This test should be used to help diagnose problems with the AstroHeaderWriter
# and to make sure that it continues to work with old datasets.
#
# Unfortunately, due to the use of floating point arithmetic, this test cannot
# be run as part of the normal test suite. It will fail depending on compiler
# flags and machine architecture.
#
# Generally, you will run it from a build tree, using:
# ../carma/sdp/Test/ahw-unit.bash

# debug: print running path
echo "AHW UNIT TEST: PATH=$PWD DN=$(dirname $0)"

stderr() {
	echo -e "$@" >&2
}

for CORL_TYPE in SL WB; do
	for DATA_FILE in "$(dirname $0)/$CORL_TYPE/"*.tar.gz; do
		DATA_FILE="$(readlink -f "$DATA_FILE")"
		bash "$(dirname $0)/ahw-unit-single.bash" "$DATA_FILE" "$CORL_TYPE"
		if [[ "$?" -ne "0" ]]; then
			stderr "FAIL: DATA_FILE=$DATA_FILE CORL_TYPE=$CORL_TYPE"
			exit 1
		fi
	done
done
