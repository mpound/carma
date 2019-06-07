#!/bin/bash
#
# Script which calls multiple instances of hammerIpq to test IPQ reads
# and writes from several processes.
# 
# Author: Andy Beard
#

# Writers only write, readers only read, readwriters do both and this is 
# all on the same IPQ at once.
NWRITERS=2
NREADERS=2
NREADWRITERS=2

DURATION=10
WRITEPERIOD=0

CMD="./carma/util/Test/hammerIpq"

# Start writer processes
for (( writer=0; writer < NWRITERS; ++writer )); do
    $CMD readers=0 writeUsec=$WRITEPERIOD duration=$DURATION &
done

# Start readers
for (( reader=0; reader < NWRITERS; ++reader )); do
    $CMD writers=0 writeUsec=$WRITEPERIOD duration=$DURATION &
done

# Start readwriters
for (( readwriter=0; readwriter < NREADWRITERS; ++readwriter )); do
    $CMD writeUsec=$WRITEPERIOD duration=$DURATION &
done

JOBS=`jobs -p`
echo $JOBS

# Wait for everybody to exit
for j in $JOBS; do
    wait $j
    if [ $? -ne 0 ]; then
        exit 1
    fi
done
