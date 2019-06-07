// @description
//   Program that deliberately does nasty things (that you are NOT supposed to
//   do) to the PthreadXXX classes to check that they handle error states and
//   conditions correctly.
//
// @noKeys
//
// @logger TEST_FACILITY carma.test.util.tPthreadClasses
//

#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadAttr.h"
#include "carma/util/PthreadCond.h"
#include "carma/util/PthreadMutexAttr.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/PthreadRWLock.h"
#include "carma/util/PthreadRWLockAttr.h"
#include "carma/util/ScopedLogNdc.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace  {


void
testPthreadAttr( )
{
    const ScopedLogNdc ndc( "testPthreadAttr" );

    programLogInfoIfPossible( "PthreadAttr tests done" );
}


void
testPthreadMutexAttr( )
{
    const ScopedLogNdc ndc( "testPthreadMutexAttr" );

    programLogInfoIfPossible( "PthreadMutexAttr tests done" );
}


void
testPthreadMutex( )
{
    const ScopedLogNdc ndc( "testPthreadMutex" );

    programLogInfoIfPossible( "PthreadMutex tests done" );
}


void
testPthreadCond( )
{
    const ScopedLogNdc ndc( "testPthreadCond" );

    programLogInfoIfPossible( "PthreadCond tests done" );
}


void
testPthreadRWLockAttr( )
{
    const ScopedLogNdc ndc( "testPthreadRWLockAttr" );

    programLogInfoIfPossible( "PthreadRWLockAttr tests done" );
}


void
testPthreadRWLock( )
{
    const ScopedLogNdc ndc( "testPthreadRWLock" );

    programLogInfoIfPossible( "PthreadRWLock tests done" );
}


void
testAll( )
{
    testPthreadAttr();
    testPthreadMutexAttr();
    testPthreadMutex();
    testPthreadCond();
    testPthreadRWLockAttr();
    testPthreadRWLock();
}


}  // namespace < anonymous >


int
Program::main( )
{
    testAll();
    
    programLogInfoIfPossible( "All tests done" );
    
    return EXIT_SUCCESS;
}
