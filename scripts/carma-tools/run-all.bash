#!/bin/bash
#
# A simple wrapper around run-step.bash to make it run all steps
# in sequential order, exiting if a step fails.

cd "$(dirname "$(readlink -f "$0")")"
for i in unpack prepare configure compile test install finalize; do
	bash run-step.bash "$i" "$1" || exit 1
done
