# sh utility functions for sdp testing 
#
# Based closely on those used in dbms testing 
#

# Clean-up test area
cleanup() {
    cmd="rm -fr $topdir $sdpconf"
    $cmd
}

# Return fail 
fail() {
    cleanup
    echo CARMA-TEST: FAIL carma/sdp `basename $prog`
    exit 1
}

# Return pass
pass() {
    cleanup
    echo CARMA-TEST: PASS $prog
};

# Create CARMA conf files needed for tests of the sdp sub-system
createConfFiles() {
    # Template conf files
    template_sdpconf=conf/sdp/sdp.test.conf

    # Delete existing test conf files
    filelist="$sdpconf"
    for x in $filelist ; do
	if [ -f $x ] ; then
	    rm -f $x
	fi
    done

    # Create test conf files from template
    sed -e "s?/tmp/sdp_tests/sdpTest?$topdir?g" < $template_sdpconf > conf/$sdpconf
};

# Make the test directories
makeTestDirs() {
    if [ -d $topdir ] ; then
	\rm -fr $topdir
    fi

    filelist="visbrick sdp/astroheader/SLCorrelIntegrated sdp/sciencedata sdp/recycle sdp/quality"
    for x in $filelist ; do
	cmd="mkdir -p $topdir/$x"
	$cmd
	if [ $? -ne 0 ] ; then
	    echo mkdir -p $topdir/$x failed
	    fail
	fi
    done
};

# Program name
prog=`basename $0`

# Root directory for test data
topdir="/tmp/carma.sdp.tests.$prog.$USER.$$"

# SDP conf file
sdpconf="sdp/sdp.test.$prog.$$.conf"

# Override tinderbox setting of CARMA environment variables
unset CARMA



    
    
    
    
    
    
