#include "carma/util/Test/testWorkResultComplexLives.h"

#include <memory>

#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/WorkResult.h"
#include "carma/util/WorkResultSetPostError.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;
using namespace carma::util::test;


namespace {


const char kKeyA[] = "Boy George";
const char kKeyB[] = "George Michael";
const char kKeyC[] = "Curious George";
const char kKeyD[] = "G. W. Bush";


void
checkCantAddKey( WorkResultSet & wrs,
                 const string &  key )
{
    bool gotException = false;
    
    try {
        wrs.addKey( key );
        
        gotException = false;
    } catch ( ... ) {
        gotException = true;

        programLogInfoIfPossible( "Good exception: " + getStringForCaught() );
    }
    
    if ( gotException != true )
        throw CARMA_ERROR( "Shouldn't have been able to add key" );
}


void
checkPostGivesError( WorkResult &                   wr,
                     const bool                     normal,
                     const WorkResultSet::PostState expectedErr )
{
    bool gotExpectedErr = false;
    
    try {
        if ( normal )
            wr.postNormal();
        else
            wr.postAbnormal( "Deliberate abnormal result" );
        
        gotExpectedErr = false;
    } catch ( const WorkResultSet::PostError & postError ) {
        if ( postError.getPostState() != expectedErr ) {
            gotExpectedErr = false;
            
            throw;
        }
        
        gotExpectedErr = true;

        programLogInfoIfPossible( "Good PostError: " + getStringForCaught() );
    }
    
    if ( gotExpectedErr != true )
        throw CARMA_ERROR( "Didn't get the expected post error" );
}


void
doIt( )
{
    auto_ptr< WorkResultSet > wrs( new WorkResultSet( "Pop Culture" ) );

    auto_ptr< WorkResult > wrA( new WorkResult( wrs->addKey( kKeyA ) ) );
    auto_ptr< WorkResult > wrB( new WorkResult( wrs->addKey( kKeyB ) ) );
    auto_ptr< WorkResult > wrC;
    auto_ptr< WorkResult > wrD;
    
    {
        WorkResultSet x = *wrs;
    
        checkCantAddKey( x, kKeyA );
        checkCantAddKey( x, kKeyB );
    
        wrC = auto_ptr< WorkResult >( new WorkResult( x.addKey( kKeyC ) ) );
        wrD = auto_ptr< WorkResult >( new WorkResult( x.addKey( kKeyD ) ) );
    }

    checkCantAddKey( *wrs, kKeyC );
    checkCantAddKey( *wrs, kKeyD );
    
    wrB->postAbnormal( "Deliberate abnormal result" );
    wrD->postNormal();
    
    wrs.reset();
    
    checkPostGivesError( *wrC,
                         true,
                         WorkResultSet::NO_WAITERS_DROPPED_POST_STATE );
                         
    checkPostGivesError( *wrA,
                         false,
                         WorkResultSet::NO_WAITERS_DROPPED_POST_STATE );
}


} // namespace < anonymous >


void
carma::util::test::testWorkResultComplexLives( )
{
    const ScopedLogNdc ndc( "testWorkResultComplexLives" );
    
    programLogInfoIfPossible( "testing" );

    doIt();
}
