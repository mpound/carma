#!/bin/bash
#
# Script to test ovro antenna control classes.
# 
# Must be ran from build directory.
#

CMD="./scripts/rc.carma"
CMDOPTS="--imr `hostname` --file imr/andyLocal.xml --nosu --noapps --clean --dir ./"

IMRSTOP="./scripts/taoimradmin --imr `hostname` stop "

CLIENTCMD="./carma/antenna/ovro/control/Test/tOvroControlClient imr=`hostname`"

echo "********************* Ovro Control Client Test **************************"

if [[ ! -x $CMD ]]; then
    echo "$CMD does not exist."
    return 1
fi

# Start up the imr in all it's glory
$CMD $CMDOPTS restart

# Give it a few seconds to get going
sleep 2 

echo -n "Starting ovroCanHost" 

# For some reason, when an application is started by the IMR, no coverage
# results are written - as a workaround, I start ovroCanHost by hand.
./bin/ovroCanHost imr=`hostname` ant=1 emulate=true simulate=true autowrite=false traceLevel=0 &

for (( i = 0; i < 15; ++i )); do 
    echo -n "."
    sleep 1
done
echo ""
echo ""

# Run the test client
echo -n "Running test client..."
$CLIENTCMD
TESTSTAT=$?

if [[ "$TESTSTAT" -eq 0 ]]; then
    echo " success." 
else
    echo " failed"
    kill %1
fi

echo ""
echo -n "Waiting for ovroCanHost to exit... "
wait %1
if [[ $? -eq 0 ]]; then
    echo "exited successfully." 
else
    echo "exited with errors." 
    TESTSTAT=1
fi

sleep 1

echo ""
echo "Shutting everything down"

# Shut everything down
$CMD $CMDOPTS stop 

sleep 2 

echo "********************* Bye Bye **************************"

exit $TESTSTAT
