#!/bin/bash
# vim: set ts=4 sts=4 sw=4 noet:

# debug: print running path
echo "AHW UNIT TEST: PATH=$PWD"

stderr() {
	echo -e "$@" >&2
}

# check arguments
if [[ "$#" -ne "2" ]]; then
	stderr "ERROR: incorrect arguments specified"
	stderr "USAGE: $0 <data-file> <corl-type>"
	exit 1
fi

TEST_DIR="$PWD/carma/sdp/Test/ahw-unit-test"
DATA_FILE="$1"
CORL_TYPE="$2"

# check that the data file exists and is readable
if [[ ! -r "$DATA_FILE" ]]; then
	stderr "ERROR: data source file not readable: $DATA_FILE"
	exit 1
fi

# remove old data and re-create the directory
echo "Removing old test data (if necessary)"
rm -rf "$TEST_DIR"
mkdir -p "$TEST_DIR"

# change to unit test directory
pushd "$TEST_DIR"

# extract mpdat and expected astroheader files
echo "Extracting test data: this will take a while!"
tar --strip-components=1 -xzf "$DATA_FILE"

# back to the original directory
popd

# run the AstroHeaderWriter unit test application
echo "Running ahwUnit on $DATA_FILE"
$PWD/bin/ahwUnit inputDir="$TEST_DIR/mpdat" \
				 outputDir="$TEST_DIR/astroheader.generated" \
				 recycleDir="$TEST_DIR/recycle" \
				 corl="$CORL_TYPE" \
				 logfile="/dev/stdout" \
				 traceFile="/dev/stdout"
RET="$?"

if [[ "$RET" -ne 0 ]]; then
	stderr "FAIL: ahwUnit program exited abnormally"
	exit 1
fi

# change to unit test directory
pushd "$TEST_DIR"

# check for any input files not marked '.done'
echo "TEST: checking that all input files are marked '.done'"
NOTDONE="$(find 'mpdat' -type f ! -iname '*.done' -print | wc -l)"
if [[ "$NOTDONE" -ne "0" ]]; then
	stderr "FAIL: some files in mpdat/ are not marked '.done'"
	exit 1
fi

# check for any output files not marked '.xml'
echo "TEST: checking that all output files are marked '.xml'"
NOTDONE="$(find 'astroheader.generated' -type f ! -iname '*.xml' -print | wc -l)"
if [[ "$NOTDONE" -ne "0" ]]; then
	stderr "FAIL: some files in astroheader.generated/ are not marked '.xml'"
	exit 1
fi

# Rename expected files to make sure they have no '.done' suffix. Data comes
# from the archive at UIUC with a '.done' suffix.
find astroheader.expected -name '*.xml.done' -exec rename '.xml.done' '.xml' '{}' \;

# check for any differences in astroheader output
echo "TEST: comparing directories astroheader.expected and astroheader.generated"
if diff -Nrua astroheader.expected astroheader.generated ; then
	echo "PASS: no differences detected"
else
	stderr "FAIL: differences detected, see above diff output"
	exit 1
fi

# back to the original directory
popd

# remove all test data
rm -rf "$TEST_DIR"
exit 0
