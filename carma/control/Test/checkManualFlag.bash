#!/bin/bash

# A quick test for the checkControlManualFlag program. It checks all
# files named "conf/control/*Correlator.tab" to make sure that they
# can be parsed correctly.

stderr() {
	echo -e "$@" >&2
}

# Check each file
for i in $PWD/conf/control/*Correlator.tab; do
	echo "CHECK MANUAL FLAG CONFIGURATION: $i ..."
	./bin/checkControlManualFlag file="$i"
	if [[ "$?" -ne "0" ]]; then
		stderr "ERROR: checkControlManualFlag did not pass when expected!"
		stderr
		stderr "Manual Flag Configuration File: $i"
		stderr
		stderr "Please correct the error reported in the output"
		stderr "shown above!"
		exit 1
	fi
done
