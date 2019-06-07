//
// @version 0.3
//
// @usage use it
//
// @description
//   A test binary that unit tests the carma::util::WorkResultSet class.
//
// @key threads 3 int
//      Number of threads to use in the multithreaded testing. 0 or less means
//      no multithreaded testing.
//
// @key timeout 30.0 double
//      Timeout value in seconds to use for the waits with a timeout. Values
//      less than 0.0 will result in 0.0 being used.
//
// @logger TEST_FACILITY carma.test.util.tWorkResult
//


#include "carma/util/Program.h"

#include <sstream>
#include <vector>
#include <cmath>

#include <unistd.h>

#include "carma/util/BaseException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/StartPthread.h"
#include "carma/util/Trace.h"
#include "carma/util/WorkResult.h"
#include "carma/util/WorkResultSetPostError.h"
#include "carma/util/WorkResultSetWaitError.h"
#include "carma/util/Test/testWorkResultComplexLives.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;
using namespace carma::util::test;


namespace {


void
verifyWrsId( const WorkResultSet & wrs,
             const string &        id )
{
    if ( wrs.getId() != id )
        throw runtime_error( "verifyWrsId failed" );
}


const string kKey1 = "89898989";
const string kKey2 = "string key #2";
const string kKey3 = "string key #3";
const string kKey4 = "1";
const string kKey5 = "0";

PthreadMutex gBarrierAMutex;
PthreadMutex gBarrierBMutex;
PthreadMutex gBarrierCMutex;


struct ThreadArgs {
    const WorkResultSet wrs;
    const unsigned long timeoutMillis;
    ::size_t * const    abnormalsCount;

    ThreadArgs( const WorkResultSet & wrs,
                unsigned long         timeoutMillis,
                ::size_t *            abnormalsCount );
};


ThreadArgs::ThreadArgs( const WorkResultSet & inWrs,
                        const unsigned long   inTimeoutMillis,
                        ::size_t * const      inAbnormalsCount ) :
wrs( inWrs ),
timeoutMillis( inTimeoutMillis ),
abnormalsCount( inAbnormalsCount )
{
}


void
entryPoint( ThreadArgs & args )
{
    CARMA_CPTRACE( Trace::TRACE2, "Entered." );

    CARMA_CPTRACE( Trace::TRACE2, "Waiting for barrier A." );

    gBarrierAMutex.Lock();
    gBarrierAMutex.Unlock();

    CARMA_CPTRACE( Trace::TRACE2, "Passed barrier A." );

    CARMA_CPTRACE( Trace::TRACE2, "Waiting for barrier B." );

    gBarrierBMutex.Lock();
    gBarrierBMutex.Unlock();

    CARMA_CPTRACE( Trace::TRACE2, "Passed barrier B." );

    CARMA_CPTRACE( Trace::TRACE2, "Waiting for all results." );

    ::size_t abnormalsCount = 0;

    try {
        args.wrs.waitForAll( args.timeoutMillis,
                             true,
                             WorkResultSet::GOOD_POST_STATE );
    } catch ( const WorkResultSet::WaitError & waitError ) {
        if ( waitError.hadUnfinishedKeys() )
            throw;

        abnormalsCount = waitError.getAbnormals().size();
    }

    if ( abnormalsCount == 0 ) {
        CARMA_CPTRACE( Trace::TRACE2, "All results ready and normal." );
    } if ( abnormalsCount == 1 ) {
        CARMA_CPTRACE( Trace::TRACE2,
                       "All results ready but 1 was abnormal." );
    } else {
        CARMA_CPTRACE( Trace::TRACE2,
                       "All results ready but " << abnormalsCount <<
                       " were abnormal." );
    }

    CARMA_CPTRACE( Trace::TRACE2, "Waiting for barrier C." );

    gBarrierCMutex.Lock();
    gBarrierCMutex.Unlock();

    CARMA_CPTRACE( Trace::TRACE2, "Passed barrier C." );

    if ( args.abnormalsCount != 0 )
        *(args.abnormalsCount) = abnormalsCount;

    CARMA_CPTRACE( Trace::TRACE2, "Done." );
}


void
testMultithreaded( const ::size_t      numThreads,
                   const unsigned long timeoutMillis,
                   const bool          makeSomeAbnormal )
{
    const ScopedLogNdc ndc( "main thread" );

    programLogInfoIfPossible( "Starting MT test" );

    CARMA_CPTRACE( Trace::TRACE2, "Locking barriers A, B and C." );

    gBarrierAMutex.Lock();
    gBarrierBMutex.Lock();
    gBarrierCMutex.Lock();

    vector< ::size_t > abnormalsCounts;

    abnormalsCounts.resize( numThreads, 411 );

    vector< ::pthread_t > threads;

    {
        CARMA_CPTRACE( Trace::TRACE2, "Constructing result set object." );

        WorkResultSet wrs( "result set" );
        verifyWrsId( wrs, "result set" );

        CARMA_CPTRACE( Trace::TRACE2, "Adding keys to result set object." );

        WorkResult result1 = wrs.addKey( kKey1 );
        WorkResult result2 = wrs.addKey( kKey2 );
        WorkResult result3 = wrs.addKey( kKey3 );
        WorkResult result4 = wrs.addKey( kKey4 );
        WorkResult result5 = wrs.addKey( kKey5 );

        CARMA_CPTRACE( Trace::TRACE2,
                       "Creating " << numThreads << " threads." );

        {
            for ( ::size_t i = 0; i < numThreads; ++i ) {
                string threadNdc;

                {
                    ostringstream oss;

                    oss << "thread #" << (i + 1);

                    threadNdc = oss.str();
                }

                const ThreadArgs args( wrs,
                                       timeoutMillis,
                                       &(abnormalsCounts.at( i )) );

                threads.push_back( StartPthreadWithCopy( entryPoint,
                                                         args,
                                                         threadNdc ) );
            }
        }

        CARMA_CPTRACE( Trace::TRACE2, "Posting result 2 normal." );

        result2.postNormal();

        CARMA_CPTRACE( Trace::TRACE2, "Posting result 1 normal." );

        result1.postNormal();

        CARMA_CPTRACE( Trace::TRACE2, "Releasing barrier A." );

        gBarrierAMutex.Unlock();

        CARMA_CPTRACE( Trace::TRACE2, "Sleeping 1 second." );

        ::sleep( 1 );

        if ( makeSomeAbnormal ) {
            CARMA_CPTRACE( Trace::TRACE2, "Posting result 3 abnormal." );

            result3.postAbnormal( "Deliberate abnormal result" );
        } else {
            CARMA_CPTRACE( Trace::TRACE2, "Posting result 3 normal." );

            result3.postNormal();
        }

        CARMA_CPTRACE( Trace::TRACE2, "Sleeping 1 second." );

        ::sleep( 1 );

        CARMA_CPTRACE( Trace::TRACE2, "Releasing barrier B." );

        gBarrierBMutex.Unlock();

        CARMA_CPTRACE( Trace::TRACE2, "Sleeping 1 second." );

        ::sleep( 1 );

        CARMA_CPTRACE( Trace::TRACE2, "Posting result 4 normal." );

        result4.postNormal();

        CARMA_CPTRACE( Trace::TRACE2, "Sleeping 1 second." );

        ::sleep( 1 );

        CARMA_CPTRACE( Trace::TRACE2, "Posting result 5 normal." );

        result5.postNormal();

        CARMA_CPTRACE( Trace::TRACE2, "Destructing result set object." );
    }

    CARMA_CPTRACE( Trace::TRACE2, "Sleeping 1 second." );

    ::sleep( 1 );

    CARMA_CPTRACE( Trace::TRACE2, "Releasing barrier C." );

    gBarrierCMutex.Unlock();

    CARMA_CPTRACE( Trace::TRACE2,
                   "Joining to " << threads.size() << " threads." );

    {
        for ( ::size_t i = 0; i < threads.size(); ++i ) {
            void * threadResult = 0;

            ::pthread_join( threads[ i ], &threadResult );
        }
    }

    CARMA_CPTRACE( Trace::TRACE2, "All threads completed." );

    CARMA_CPTRACE( Trace::TRACE2, "Checking abnormals counts." );

    {
        const ::size_t expectedAbnormalsCount = (makeSomeAbnormal ? 1 : 0);

        for ( ::size_t i = 0; i < numThreads; ++i ) {
            if ( abnormalsCounts.at( i ) != expectedAbnormalsCount )
                throw runtime_error( "Bad abormals count" );
        }
    }

    programLogInfoIfPossible( "Done MT test" );
}


void
testRefCountingAndLifetimes( )
{
    WorkResultSet wrA( "wrA" );
    WorkResultSet wrB( "wrB" );

    verifyWrsId( wrA, "wrA" );
    verifyWrsId( wrB, "wrB" );

    {
        WorkResultSet wr1( "wr1" );
        WorkResultSet wr2( "wr2" );

        verifyWrsId( wr1, "wr1" );
        verifyWrsId( wr2, "wr2" );

        WorkResultSet wr3 = wr1;
        WorkResultSet wr4 = wr2;

        verifyWrsId( wr3, "wr1" );
        verifyWrsId( wr4, "wr2" );

        WorkResultSet wr6( wr3 );
        WorkResultSet wr7( wr3 );

        verifyWrsId( wr6, "wr1" );
        verifyWrsId( wr7, "wr1" );

        wrA = wr2;
    }

    wrA = wrB;
    wrA = wrA;
    wrA = wrB;
    wrB = wrB;

    {
        WorkResultSet wrC( "wrC" );

        verifyWrsId( wrC, "wrC" );

        wrC = wrC;
    }
}


string
stringSetToString( const set< string > & stringSet )
{
    string s = "{";

    bool noneYet = true;

    set< string >::const_iterator i = stringSet.begin();
    const set< string >::const_iterator iEnd = stringSet.end();

    while ( i != iEnd ) {
        if ( noneYet )
            noneYet = false;
        else
            s += ",";

        s += " \"" + (*i) + "\"";

        ++i;
    }

    s += " }";

    return s;
}


void
verifyNotKeys( const WorkResultSet & wrs,
               const set< string > & keys )
{
    bool exceptionThrown = false;

    try {
        wrs.verifyKeys( keys );
    } catch ( ... ) {
        exceptionThrown = true;
    }

    if ( exceptionThrown == false )
        throw runtime_error( "verifyNotKeys failed" );
}


void
testKeySets( )
{
    set< string > keySet1;

    keySet1.insert( kKey2 );
    keySet1.insert( kKey3 );

    set< string > keySet2;

    keySet2.insert( kKey1 );
    keySet2.insert( kKey4 );
    keySet2.insert( kKey5 );

    set< string > keySet3;

    keySet3.insert( "key #1 in key set #3" );
    keySet3.insert( "key #2 in key set #3" );
    keySet3.insert( "key #3 in key set #3" );
    keySet3.insert( "key #4 in key set #3" );
    keySet3.insert( "key #5 in key set #3" );

    set< string > keySet1And3;

    keySet1And3.insert( keySet1.begin(), keySet1.end() );
    keySet1And3.insert( keySet3.begin(), keySet3.end() );

    if ( keySet1And3.size() != (keySet1.size() + keySet3.size()) )
        throw runtime_error( "key set (1 and 3) has a bad size" );

    WorkResultSet wrs( "testKeySets work result" );
    verifyWrsId( wrs, "testKeySets work result" );

    CARMA_CPTRACE( Trace::TRACE2, "Verifying key set is empty." );

    wrs.verifyKeys( set< string >() );
    verifyNotKeys( wrs, keySet1 );
    verifyNotKeys( wrs, keySet2 );
    verifyNotKeys( wrs, keySet3 );
    verifyNotKeys( wrs, keySet1And3 );

    CARMA_CPTRACE( Trace::TRACE2, "Adding key set 1." );

    wrs.addKeys( keySet1 );

    CARMA_CPTRACE( Trace::TRACE2, "Verifying key set is key set 1." );

    verifyNotKeys( wrs, set< string >() );
    wrs.verifyKeys( keySet1 );
    verifyNotKeys( wrs, keySet2 );
    verifyNotKeys( wrs, keySet3 );
    verifyNotKeys( wrs, keySet1And3 );

    CARMA_CPTRACE( Trace::TRACE2, "Removing key set 1." );

    wrs.removeKeys( keySet1 );

    CARMA_CPTRACE( Trace::TRACE2, "Verifying key set is empty." );

    wrs.verifyKeys( set< string >() );
    verifyNotKeys( wrs, keySet1 );
    verifyNotKeys( wrs, keySet2 );
    verifyNotKeys( wrs, keySet3 );
    verifyNotKeys( wrs, keySet1And3 );

    CARMA_CPTRACE( Trace::TRACE2, "Adding key set 2." );

    wrs.addKeys( keySet2 );

    CARMA_CPTRACE( Trace::TRACE2, "Verifying key set is key set 2." );

    verifyNotKeys( wrs, set< string >() );
    verifyNotKeys( wrs, keySet1 );
    wrs.verifyKeys( keySet2 );
    verifyNotKeys( wrs, keySet3 );
    verifyNotKeys( wrs, keySet1And3 );

    CARMA_CPTRACE( Trace::TRACE2, "Removing key set 2." );

    wrs.removeKeys( keySet2 );

    CARMA_CPTRACE( Trace::TRACE2, "Verifying key set is empty." );

    wrs.verifyKeys( set< string >() );
    verifyNotKeys( wrs, keySet1 );
    verifyNotKeys( wrs, keySet2 );
    verifyNotKeys( wrs, keySet3 );
    verifyNotKeys( wrs, keySet1And3 );

    CARMA_CPTRACE( Trace::TRACE2, "Adding key set 2." );

    wrs.addKeys( keySet2 );

    CARMA_CPTRACE( Trace::TRACE2, "Adding key set 1." );

    wrs.addKeys( keySet1 );

    CARMA_CPTRACE( Trace::TRACE2,
                   "Verifying key set is not empty, 1, 2, 3, or (1 and 3)." );

    verifyNotKeys( wrs, set< string >() );
    verifyNotKeys( wrs, keySet1 );
    verifyNotKeys( wrs, keySet2 );
    verifyNotKeys( wrs, keySet3 );
    verifyNotKeys( wrs, keySet1And3 );

    CARMA_CPTRACE( Trace::TRACE2, "Removing key set 2." );

    wrs.removeKeys( keySet2 );

    CARMA_CPTRACE( Trace::TRACE2, "Verifying key set is key set 1." );

    verifyNotKeys( wrs, set< string >() );
    wrs.verifyKeys( keySet1 );
    verifyNotKeys( wrs, keySet2 );
    verifyNotKeys( wrs, keySet3 );
    verifyNotKeys( wrs, keySet1And3 );

    CARMA_CPTRACE( Trace::TRACE2, "Adding key set 3." );

    wrs.addKeys( keySet3 );

    CARMA_CPTRACE( Trace::TRACE2, "Verifying key set is key set (1 and 3)." );

    verifyNotKeys( wrs, set< string >() );
    verifyNotKeys( wrs, keySet1 );
    verifyNotKeys( wrs, keySet2 );
    verifyNotKeys( wrs, keySet3 );
    wrs.verifyKeys( keySet1And3 );

    {
        set< string > keySet3AndRandoms;

        keySet3AndRandoms.insert( keySet3.begin(), keySet3.end() );
        keySet3AndRandoms.insert( "key #0" );
        keySet3AndRandoms.insert( "key #6" );

        CARMA_CPTRACE( Trace::TRACE2,
                       "Verifying removing a key set is all or nothing and "
                       "that removing a bad key set fails." );

        bool removeFailed = false;

        try {
            wrs.removeKeys( keySet3AndRandoms );
        } catch ( ... ) {
            removeFailed = true;
        }

        if ( removeFailed == false )
            throw runtime_error( "Removing bad keys did not fail" );

        CARMA_CPTRACE( Trace::TRACE2, "Verifying key set is key set (1 and 3)." );

        verifyNotKeys( wrs, set< string >() );
        verifyNotKeys( wrs, keySet1 );
        verifyNotKeys( wrs, keySet2 );
        verifyNotKeys( wrs, keySet3 );
        wrs.verifyKeys( keySet1And3 );

        CARMA_CPTRACE( Trace::TRACE2,
                       "Verifying adding a key set is all or nothing and "
                       "that adding a bad key set fails." );

        bool addFailed = false;

        try {
            wrs.addKeys( keySet3AndRandoms );
        } catch ( ... ) {
            addFailed = true;
        }

        if ( addFailed == false )
            throw runtime_error( "Adding bad keys did not fail" );

        CARMA_CPTRACE( Trace::TRACE2, "Verifying key set is key set (1 and 3)." );

        verifyNotKeys( wrs, set< string >() );
        verifyNotKeys( wrs, keySet1 );
        verifyNotKeys( wrs, keySet2 );
        verifyNotKeys( wrs, keySet3 );
        wrs.verifyKeys( keySet1And3 );
    }
}


void
testWaitErrors( )
{
    WorkResultSet wrs( "testWaitErrors wrs" );
    verifyWrsId( wrs, "testWaitErrors wrs" );

    WorkResult result1 = wrs.addKey( "#1" );
    WorkResult result2 = wrs.addKey( "#2" );
    WorkResult result3 = wrs.addKey( "#3" );

    {
        bool bogusStateFailed = false;

        programLogInfoIfPossible( "Waiting for all keys with a bogus state" );
        try {
            wrs.waitForAll( 2,
                            false,
                            WorkResultSet::NO_WAITERS_DROPPED_POST_STATE );
        } catch ( ... ) {
            bogusStateFailed = true;
        }

        if ( bogusStateFailed == false )
            throw runtime_error( "Bogus state did not fail" );
    }

    try {
        programLogInfoIfPossible( "Waiting for all keys with a 2ms timeout" );
        wrs.waitForAll( 2, false, WorkResultSet::GOOD_POST_STATE );

        const string msg = "All are ready and they shouldn't be";

        programLogInfoIfPossible( msg );
        throw CARMA_ERROR( msg );
    } catch ( const WorkResultSet::WaitError & ) {
        programLogInfoIfPossible( "Good WaitError: " + getStringForCaught() );
    }

    programLogInfoIfPossible( "Posting result #2 as normal" );
    result2.postNormal();

    try {
        programLogInfoIfPossible( "Waiting for all keys with a 2ms timeout" );
        wrs.waitForAll( 2, false, WorkResultSet::GOOD_POST_STATE );

        const string msg = "All are ready and they shouldn't be";

        programLogInfoIfPossible( msg );
        throw CARMA_ERROR( msg );
    } catch ( const WorkResultSet::WaitError & ) {
        programLogInfoIfPossible( "Good WaitError: " + getStringForCaught() );
    }

    programLogInfoIfPossible( "Posting result #1 as abnormal" );
    result1.postAbnormal( "Deliberate abnormal result" );

    try {
        programLogInfoIfPossible( "Waiting for all keys with a 2ms timeout" );
        wrs.waitForAll( 2, false, WorkResultSet::GOOD_POST_STATE );

        const string msg = "All are ready and they shouldn't be";

        programLogInfoIfPossible( msg );
        throw CARMA_ERROR( msg );
    } catch ( const WorkResultSet::WaitError & ) {
        programLogInfoIfPossible( "Good WaitError: " + getStringForCaught() );
    }

    try {
        programLogInfoIfPossible( "Waiting for all keys normal with a 2ms timeout" );
        wrs.waitForAll( 2, true, WorkResultSet::GOOD_POST_STATE );

        const string msg = "All are ready and normal they shouldn't be";

        programLogInfoIfPossible( msg );
        throw CARMA_ERROR( msg );
    } catch ( const WorkResultSet::WaitError & ) {
        programLogInfoIfPossible( "Good WaitError: " + getStringForCaught() );
    }

    programLogInfoIfPossible( "Posting result #3 as abnormal" );
    result3.postAbnormal( "Deliberate abnormal result" );

    try {
        programLogInfoIfPossible( "Waiting for all keys with a 2ms timeout" );
        wrs.waitForAll( 2, false, WorkResultSet::GOOD_POST_STATE );

        programLogInfoIfPossible( "All are ready" );
    } catch ( const WorkResultSet::WaitError & ) {
        programLogInfoIfPossible( "Bad WaitError: " + getStringForCaught() );

        throw;
    }

    try {
        programLogInfoIfPossible( "Waiting for all keys normal with a 2ms timeout" );
        wrs.waitForAll( 2, true, WorkResultSet::GOOD_POST_STATE );

        const string msg = "All are ready and normal and they shouldn't be";

        programLogInfoIfPossible( msg );
        throw CARMA_ERROR( msg );
    } catch ( const WorkResultSet::WaitError & ) {
        programLogInfoIfPossible( "Good WaitError: " + getStringForCaught() );
    }
}


void
testLateResultPosts( )
{
    const ScopedLogNdc ndc( "testLateResultPosts" );

    WorkResultSet wrs( "testLateResultPosts wrs" );
    verifyWrsId( wrs, "testLateResultPosts wrs" );

    WorkResult result1 = wrs.addKey( "#1" );
    WorkResult result2 = wrs.addKey( "#2" );
    WorkResult result3 = wrs.addKey( "#3" );
    WorkResult result4 = wrs.addKey( "#4" );

    programLogInfoIfPossible( "Posting result #3 as abnormal" );
    result3.postAbnormal( "Deliberate abnormal result" );

    programLogInfoIfPossible( "Posting result #1 as normal" );
    result1.postNormal();

    try {
        programLogInfoIfPossible( "Waiting for all keys normal with a 2ms timeout" );
        wrs.waitForAll( 2, true, WorkResultSet::GOOD_POST_STATE );

        const string msg = "All are ready and normal and they shouldn't be";

        programLogInfoIfPossible( msg );
        throw CARMA_ERROR( msg );
    } catch ( const WorkResultSet::WaitError & ) {
        programLogInfoIfPossible( "Good WaitError: " + getStringForCaught() );
    }

    try {
        programLogInfoIfPossible( "Waiting for all keys with a 2ms timeout" );
        wrs.waitForAll( 2, false, WorkResultSet::GOOD_POST_STATE );

        const string msg = "All are ready and normal and they shouldn't be";

        programLogInfoIfPossible( msg );
        throw CARMA_ERROR( msg );
    } catch ( const WorkResultSet::WaitError & ) {
        programLogInfoIfPossible( "Good WaitError: " + getStringForCaught() );
    }

    try {
        programLogInfoIfPossible( "Waiting for all keys with a 2ms timeout "
                                  "and marking all other late after timeout" );
        wrs.waitForAll( 2, false, WorkResultSet::LATE_POST_STATE );

        const string msg = "All are ready and normal and they shouldn't be";

        programLogInfoIfPossible( msg );
        throw CARMA_ERROR( msg );
    } catch ( const WorkResultSet::WaitError & ) {
        programLogInfoIfPossible( "Good WaitError: " + getStringForCaught() );
    }

    programLogInfoIfPossible( "Posting result #2 as normal" );
    try {
        result2.postNormal();

        const string msg = "Dropped post did not throw correct exception";

        programLogErrorIfPossible( msg );
        throw CARMA_ERROR( msg );
    } catch ( const WorkResultSet::PostError & postError ) {
        if ( (postError.getPostState() != WorkResultSet::LATE_POST_STATE) ||
             (postError.getKey() != "#2") ||
             (postError.getResultWasNormal() != true) ) {
            programLogInfoIfPossible( "Bad PostError: " + getStringForCaught() );

            throw;
        }

        programLogInfoIfPossible( "Good PostError: " + getStringForCaught() );
    }

    try {
        programLogInfoIfPossible( "Waiting for all keys with a 2ms timeout "
                                  "and marking all other late after timeout" );
        wrs.waitForAll( 2, false, WorkResultSet::LATE_DROPPED_POST_STATE );

        const string msg = "All are ready and normal and they shouldn't be";

        programLogInfoIfPossible( msg );
        throw CARMA_ERROR( msg );
    } catch ( const WorkResultSet::WaitError & ) {
        programLogInfoIfPossible( "Good WaitError: " + getStringForCaught() );
    }

    programLogInfoIfPossible( "Posting result #4 as abnormal" );
    try {
        result4.postAbnormal( "Deliberate abnormal result" );

        const string msg = "Dropped post did not throw correct exception";

        programLogErrorIfPossible( msg );
        throw CARMA_ERROR( msg );
    } catch ( const WorkResultSet::PostError & postError ) {
        if ( (postError.getPostState() != WorkResultSet::LATE_DROPPED_POST_STATE) ||
             (postError.getKey() != "#4") ||
             (postError.getResultWasNormal() != false) ) {
            programLogInfoIfPossible( "Bad PostError: " + getStringForCaught() );

            throw;
        }

        programLogInfoIfPossible( "Good PostError: " + getStringForCaught() );
    }
}


void
testBogusResultPosts( )
{
    const ScopedLogNdc ndc( "testBogusResultPosts" );

    WorkResultSet wrs( "testBogusResultPosts wrs" );
    verifyWrsId( wrs, "testBogusResultPosts wrs" );

    WorkResult result1 = wrs.addKey( "#1" );
    WorkResult result2 = wrs.addKey( "#2" );
    WorkResult result3 = wrs.addKey( "#3" );
    WorkResult result4 = wrs.addKey( "#4" );

    programLogInfoIfPossible( "Posting result #1 as normal" );
    result1.postNormal();

    programLogInfoIfPossible( "Posting result #2 as normal" );
    result2.postNormal();

    programLogInfoIfPossible( "Posting result #3 as abnormal" );
    result3.postAbnormal( "Deliberate abnormal result" );

    programLogInfoIfPossible( "Posting result #4 as abnormal" );
    result4.postAbnormal( "Deliberate abnormal result" );

    {
        bool bogusPostFailed = false;

        programLogInfoIfPossible( "Bogusly posting result #1 as abnormal" );
        try {
            result1.postAbnormal( "Deliberate abnormal result" );
        } catch ( ... ) {
            bogusPostFailed = true;
        }

        if ( bogusPostFailed == false )
            throw runtime_error( "Bogus result post did not fail" );
    }

    {
        bool bogusPostFailed = false;

        programLogInfoIfPossible( "Bogusly posting result #1 as normal" );
        try {
            result1.postNormal();
        } catch ( ... ) {
            bogusPostFailed = true;
        }

        if ( bogusPostFailed == false )
            throw runtime_error( "Bogus result post did not fail" );
    }

    {
        bool bogusPostFailed = false;

        programLogInfoIfPossible( "Bogusly posting result #2 as normal" );
        try {
            result2.postNormal();
        } catch ( ... ) {
            bogusPostFailed = true;
        }

        if ( bogusPostFailed == false )
            throw runtime_error( "Bogus result post did not fail" );
    }

    {
        bool bogusPostFailed = false;

        programLogInfoIfPossible( "Bogusly posting result #2 as abnormal" );
        try {
            result2.postAbnormal( "Deliberate abnormal result" );
        } catch ( ... ) {
            bogusPostFailed = true;
        }

        if ( bogusPostFailed == false )
            throw runtime_error( "Bogus result post did not fail" );
    }

    {
        bool bogusPostFailed = false;

        programLogInfoIfPossible( "Bogusly posting result #3 as abnormal" );
        try {
            result3.postAbnormal( "Deliberate abnormal result" );
        } catch ( ... ) {
            bogusPostFailed = true;
        }

        if ( bogusPostFailed == false )
            throw runtime_error( "Bogus result post did not fail" );
    }

    {
        bool bogusPostFailed = false;

        programLogInfoIfPossible( "Bogusly posting result #3 as normal" );
        try {
            result3.postNormal();
        } catch ( ... ) {
            bogusPostFailed = true;
        }

        if ( bogusPostFailed == false )
            throw runtime_error( "Bogus result post did not fail" );
    }

    {
        bool bogusPostFailed = false;

        programLogInfoIfPossible( "Bogusly posting result #4 as normal" );
        try {
            result4.postNormal();
        } catch ( ... ) {
            bogusPostFailed = true;
        }

        if ( bogusPostFailed == false )
            throw runtime_error( "Bogus result post did not fail" );
    }

    {
        bool bogusPostFailed = false;

        programLogInfoIfPossible( "Bogusly posting result #4 as abnormal" );
        try {
            result4.postAbnormal( "Deliberate abnormal result" );
        } catch ( ... ) {
            bogusPostFailed = true;
        }

        if ( bogusPostFailed == false )
            throw runtime_error( "Bogus result post did not fail" );
    }

    programLogInfoIfPossible( "Done" );
}


void
testEmptyWrs( )
{
    const ScopedLogNdc ndc( "testEmptyWrs" );

    const WorkResultSet wrs( "testEmptyWrs wrs" );
    verifyWrsId( wrs, "testEmptyWrs wrs" );

    {
        bool bogusStateFailed = false;

        programLogInfoIfPossible( "Waiting for all keys with a bogus state" );
        try {
            wrs.waitForAll( 2,
                            false,
                            WorkResultSet::NO_WAITERS_DROPPED_POST_STATE );
        } catch ( ... ) {
            bogusStateFailed = true;
        }

        if ( bogusStateFailed == false )
            throw runtime_error( "Bogus state did not fail" );
    }

    programLogInfoIfPossible( "Waiting for all keys with a 2ms timeout" );
    wrs.waitForAll( 2, false, WorkResultSet::GOOD_POST_STATE );

    programLogInfoIfPossible( "Waiting for all keys with a 2ms timeout" );
    wrs.waitForAll( 2, false, WorkResultSet::LATE_POST_STATE );

    programLogInfoIfPossible( "Waiting for all keys with a 2ms timeout" );
    wrs.waitForAll( 2, false, WorkResultSet::LATE_DROPPED_POST_STATE );

    programLogInfoIfPossible( "Done" );
}


void
testSwaps( )
{
    const ScopedLogNdc ndc( "testSwaps" );

    {
        WorkResultSet wrsUno( "testSwaps wrs uno" );
        verifyWrsId( wrsUno, "testSwaps wrs uno" );

        WorkResult resultA = wrsUno.addKey( "#1" );
        WorkResult resultB = wrsUno.addKey( "#2" );
        WorkResult resultC = wrsUno.addKey( "#3" );
        WorkResult resultD = wrsUno.addKey( "#4" );

        programLogInfoIfPossible( "Posting result A(#1) as normal" );
        resultA.postNormal();

        programLogInfoIfPossible( "Posting result D(#4) as abnormal" );
        resultD.postAbnormal( "Deliberate abnormal result" );

        // Back where we started every 4th iteration
        for ( size_t i = 0; i < (8 * 4 + 1); ++i ) {
            if ( (i % 2) == 0 ) {
                resultA.swap( resultB );
                resultC.swap( resultD );
            } else {
                resultA.swap( resultC );
                resultB.swap( resultD );
            }
        }

        programLogInfoIfPossible( "Posting result A(#2) as normal" );
        resultA.postNormal();

        programLogInfoIfPossible( "Posting result D(#3) as normal" );
        resultD.postNormal();

        programLogInfoIfPossible( "Waiting for all uno keys" );
        wrsUno.waitForAll( 2, false, WorkResultSet::LATE_DROPPED_POST_STATE );
    }

    {
        WorkResultSet wrsDos( "testSwaps wrs dos" );
        WorkResultSet wrsTres( "testSwaps wrs tres" );

        verifyWrsId( wrsDos, "testSwaps wrs dos" );
        verifyWrsId( wrsTres, "testSwaps wrs tres" );

        WorkResult resultA = wrsDos.addKey( "#1" );
        WorkResult resultB = wrsDos.addKey( "#2" );
        WorkResult resultC = wrsTres.addKey( "#1" );
        WorkResult resultD = wrsTres.addKey( "#2" );

        programLogInfoIfPossible( "Posting result A(dos:#1) as normal" );
        resultA.postNormal();

        programLogInfoIfPossible( "Posting result D(tres:#2) as abnormal" );
        resultD.postAbnormal( "Deliberate abnormal result" );

        // Back where we started every 4th iteration
        for ( size_t i = 0; i < (8 * 4 + 1); ++i ) {
            if ( (i % 2) == 0 ) {
                resultA.swap( resultB );
                resultC.swap( resultD );
            } else {
                resultA.swap( resultC );
                resultB.swap( resultD );
            }
        }

        programLogInfoIfPossible( "Posting result A(dos:#2) as normal" );
        resultA.postNormal();

        programLogInfoIfPossible( "Waiting for all dos keys" );
        wrsDos.waitForAll( 2, false, WorkResultSet::LATE_DROPPED_POST_STATE );

        programLogInfoIfPossible( "Posting result D(tres:#1) as normal" );
        resultD.postNormal();

        programLogInfoIfPossible( "Waiting for all tres keys" );
        wrsTres.waitForAll( 2, false, WorkResultSet::LATE_DROPPED_POST_STATE );
    }

    programLogInfoIfPossible( "Done" );
}


void
testAssignments( )
{
    const ScopedLogNdc ndc( "testAssignments" );

    {
        WorkResultSet wrsUno( "testAssignments wrs uno" );
        verifyWrsId( wrsUno, "testAssignments wrs uno" );

        WorkResult resultA = wrsUno.addKey( "#1" );
        WorkResult resultB = wrsUno.addKey( "#2" );
        WorkResult resultC = wrsUno.addKey( "#3" );
        WorkResult resultD = wrsUno.addKey( "#4" );

        programLogInfoIfPossible( "Posting result A(#1) as normal" );
        resultA.postNormal();

        programLogInfoIfPossible( "Posting result D(#4) as abnormal" );
        resultD.postAbnormal( "Deliberate abnormal result" );

        // Back where we started every 4th iteration
        for ( size_t i = 0; i < (8 * 4 + 1); ++i ) {
            const WorkResult temp = resultA;

            resultA = resultB;
            resultB = resultC;
            resultC = resultD;
            resultD = temp;
        }

        programLogInfoIfPossible( "Posting result A(#2) as normal" );
        resultA.postNormal();

        programLogInfoIfPossible( "Posting result B(#3) as normal" );
        resultB.postNormal();

        programLogInfoIfPossible( "Waiting for all uno keys" );
        wrsUno.waitForAll( 2, false, WorkResultSet::LATE_DROPPED_POST_STATE );
    }

    {
        WorkResultSet wrsDos( "testAssignments wrs dos" );
        WorkResultSet wrsTres( "testAssignments wrs tres" );

        verifyWrsId( wrsDos, "testAssignments wrs dos" );
        verifyWrsId( wrsTres, "testAssignments wrs tres" );

        WorkResult resultA = wrsDos.addKey( "#1" );
        WorkResult resultB = wrsDos.addKey( "#2" );
        WorkResult resultC = wrsTres.addKey( "#1" );
        WorkResult resultD = wrsTres.addKey( "#2" );

        programLogInfoIfPossible( "Posting result A(dos:#1) as normal" );
        resultA.postNormal();

        programLogInfoIfPossible( "Posting result D(tres:#2) as abnormal" );
        resultD.postAbnormal( "Deliberate abnormal result" );

        // Back where we started every 4th iteration
        for ( size_t i = 0; i < (8 * 4 + 1); ++i ) {
            const WorkResult temp = resultA;

            resultA = resultB;
            resultB = resultC;
            resultC = resultD;
            resultD = temp;
        }

        programLogInfoIfPossible( "Posting result A(dos:#2) as normal" );
        resultA.postNormal();

        programLogInfoIfPossible( "Waiting for all dos keys" );
        wrsDos.waitForAll( 2, false, WorkResultSet::LATE_DROPPED_POST_STATE );

        programLogInfoIfPossible( "Posting result B(tres:#1) as normal" );
        resultB.postNormal();

        programLogInfoIfPossible( "Waiting for all tres keys" );
        wrsTres.waitForAll( 2, false, WorkResultSet::LATE_DROPPED_POST_STATE );
    }

    programLogInfoIfPossible( "Done" );
}


void
testSingleThreaded( )
{
    CARMA_CPTRACE( Trace::TRACE1, "Testing ref counting and lifetimes." );

    testRefCountingAndLifetimes();

    CARMA_CPTRACE( Trace::TRACE1, "Testing empty wrs." );

    testEmptyWrs();

    CARMA_CPTRACE( Trace::TRACE1, "Testing key sets." );

    testKeySets();

    CARMA_CPTRACE( Trace::TRACE1, "Testing swaps." );

    testSwaps();

    CARMA_CPTRACE( Trace::TRACE1, "Testing assignments." );

    testAssignments();

    CARMA_CPTRACE( Trace::TRACE1, "Testing wait errors." );

    testWaitErrors();

    CARMA_CPTRACE( Trace::TRACE1, "Testing late result posts." );

    testLateResultPosts();

    CARMA_CPTRACE( Trace::TRACE1, "Testing bogus result posts." );

    testBogusResultPosts();
}


}  // namespace < anonymous >


// To do list:
//  - Extend the MT tests to include testEmptyWrs, testSwaps, testAssignments
//  - Test multiple posts of the same result in ST and MT tests
//  - Test unposted results get handled okay
//  - Test the key set additions and removals w.r.t. to correctness and
//      exception atomicity
//  - Test unsatisifiable waits (i.e. all result instances are gone) either
//      can't happen and/or are handled correctly and immediately (i.e. no
//      relying on timeouts firing)
//  - Test that assignments of WR and WRS which remove the last ref to an Impl
//      instance all work correctly w.r.t. ref counts, deletes, posts and waits

int
Program::main( )
try {
    CARMA_CPTRACE( Trace::TRACE1, "Starting." );

    const int threadsParam = getIntParameter( "threads" );
    const double timeoutParam = getDoubleParameter( "timeout" );

    const double timeout = ((timeoutParam < 0.0) ? 0.0 : timeoutParam);

    const unsigned long timeoutMillis =
        static_cast< unsigned long >( floor( timeout * 1000.0 ) );

    CARMA_CPTRACE( Trace::TRACE1, "Testing single threaded." );

    testSingleThreaded();

    if ( threadsParam > 0 ) {
        CARMA_CPTRACE( Trace::TRACE1,
                       "Testing multithreaded with " <<
                       threadsParam << " threads." );

        testMultithreaded( threadsParam, timeoutMillis, true );
        testMultithreaded( threadsParam, timeoutMillis, false );
    }

    testWorkResultComplexLives();

    CARMA_CPTRACE( Trace::TRACE1, "Done." );

    programLogInfoIfPossible( "All tests done" );

    return EXIT_SUCCESS;
} catch ( const BaseException & e ) {
    CARMA_CPTRACE( Trace::TRACE1,
                   "Coming out on a BaseException (" <<
                   e.getLogString() << ")" );

    throw;
} catch ( const ::std::exception & e ) {
    CARMA_CPTRACE( Trace::TRACE1,
                   "Coming out on a ::std::exception (" << e.what() << ")" );

    throw;
} catch ( ... ) {
    CARMA_CPTRACE( Trace::TRACE1, "Coming out on an unknown exception" );

    throw;
}
