# sh functions for interferometry testing
# 

fail() {
    echo CARMA-TEST: FAIL $prog
    exit 1
}

pass() {
    echo CARMA-TEST: PASS $prog
}

setupCrossTest() {
    antparam="`dirname $0`/cross_test.antparam"
    decstart=20.18
    decstep=10.0
    decend=20.20
    harange=4.0
    timestep=30.0
}

setupFullTest() {
    antparam="`dirname $0`/uvwtest.antparam"
    decstart=-20.0
    decstep=10.0
    decend=80.0
    harange=6.0
    timestep=30.0
}

setupSourceTest() {
    source="w3oh"
    harange=6.0
    timestep=30.0
    dra=5
    ddec=-60
}

# sleep for the specified time and when awakened see if the specified process
# is running, if so, kill it.  This function is most useful if run in the
# background after the target process is started, also in the background. 
# A wait can be put on the target process. If this function kills the target,
# the wait will exit with a non-zero exit status. viz:
#
# $target_cmd &
# pid=$!
# watch $pid 60 &  # let the command run for a max of 60 seconds
# wait $pid
# if [ $? -ne 0 ] ; then
#   echo $target_cmd exited abnormally
# fi

watch() {
    pid_local=$1
    sleeptime=$2
    sleep $sleeptime
    ps $pid_local
    if [ $? -eq 0 ] ; then
        echo pid $pid_local is still running, killing it
        kill $pid_local
    fi
}

prog=`basename $0`
