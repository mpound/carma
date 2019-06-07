#!/bin/bash

# A quick test for the checkDag program. It checks both files that
# are expected to pass and files that are expected to fail when
# running the program on them.
#
# If a file does not have the expected result, a failure will be
# reported so the developer can fix their mistake.

# Files expected to FAIL
#
# This is setup as an array. The first file will be passed to the
# blankFlagFile= option, and the second file will be passed to the
# alarmFile= option.
XFAIL_DAGFILES=(
	"fault/test/bf_dtdbad1.xml"		"fault/test/alarm_good.xml"
	"fault/test/bf_dtdbad2.xml"		"fault/test/alarm_good.xml"
	"fault/test/bf_monbad.xml"		"fault/test/alarm_good.xml"
	"fault/test/bf_strbad.xml"		"fault/test/alarm_good.xml"
	"fault/test/bf_xmlbad.xml"		"fault/test/alarm_good.xml"

	"fault/test/bf_good.xml"		"fault/test/alarm_monbad.xml"
	"fault/test/bf_good.xml"		"fault/test/alarm_strbad.xml"
)

# Files expected to PASS
#
# See above for how this works.
XPASS_DAGFILES=(
	"fault/blankflag.xml"			"fault/alarm.xml"
	"fault/test/bf_good.xml"		"fault/test/alarm_good.xml"
	"fault/test/bf_empty.xml"		"fault/test/alarm_empty.xml"
	"fault/test/bf_mostlyempty.xml"		"fault/test/alarm_mostlyempty.xml"
)

stderr() {
	echo -e "$@" >&2
}

# $1 == blankflag filename
# $2 == alarm filename
xfail_error() {
	local BF_FILE="$1"
	local ALARM_FILE="$2"

	stderr "ERROR: checkDag did not fail when expected!"
	stderr
	stderr "BLANK/FLAG FILE: $BF_FILE"
	stderr "ALARM FILE: $ALARM_FILE"
	stderr
	stderr "The checkDag program should fail this test file due to"
	stderr "an intentional syntax error. It did not fail the file, and"
	stderr "therefore the fault system's XML parser is now broken."
	stderr
	stderr "If you have just made changes, then you MUST go back and fix"
	stderr "your changes to the parser. Otherwise, contact the last"
	stderr "person to work on the fault system."
}

# $1 == blankflag filename
# $2 == alarm filename
xpass_error() {
	local BF_FILE="$1"
	local ALARM_FILE="$2"

	stderr "ERROR: checkDag did not pass when expected!"
	stderr
	stderr "BLANK/FLAG FILE: $BF_FILE"
	stderr "ALARM FILE: $ALARM_FILE"
	stderr
	stderr "The checkDag program should pass this file. If you have"
	stderr "just made changes, then you MUST go back and fix the file."
	stderr "The fault system will not run in this condition!"
}

# Check all of the files expected to fail verification
for (( i=0; i<${#XFAIL_DAGFILES[@]}; i+=2 )) ; do
	BF_FILE="${XFAIL_DAGFILES[$i]}"
	ALARM_FILE="${XFAIL_DAGFILES[$(expr $i + 1)]}"

	# run the checkDag program and check the return code
	echo "./bin/checkDag blankFlagFile=\"$BF_FILE\" alarmFile=\"$ALARM_FILE\""
	./bin/checkDag blankFlagFile="$BF_FILE" alarmFile="$ALARM_FILE" &> /dev/null
	if [[ $? -eq 0 ]]; then
		xfail_error "$BF_FILE" "$ALARM_FILE"
		exit 1
	fi
done

# Check all of the files expected to pass verification
for (( i=0; i<${#XPASS_DAGFILES[@]}; i+=2 )) ; do
	BF_FILE="${XPASS_DAGFILES[$i]}"
	ALARM_FILE="${XPASS_DAGFILES[$(expr $i + 1)]}"

	# run the checkDag program and check the return code
	echo "./bin/checkDag blankFlagFile=\"$BF_FILE\" alarmFile=\"$ALARM_FILE\""
	./bin/checkDag blankFlagFile="$BF_FILE" alarmFile="$ALARM_FILE" &> /dev/null
	if [[ $? -ne 0 ]]; then
		xpass_error "$BF_FILE" "$ALARM_FILE"
		exit 1
	fi
done
