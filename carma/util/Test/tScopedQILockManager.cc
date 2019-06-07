//
// @version $Revision: 1.2 $
//
// @usage use it
//
// @description
//  Test program for testing the ScopedQILockManager class.
//
// @noKeys
//
// @logger TEST_FACILITY carma.test.util.tScopedQILockManager
//

#include "carma/util/Program.h"

#include <iostream>
#include <typeinfo>

#include "carma/util/ScopedQILockManager.h"

#include "carma/util/demangle.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/QuadraticInterpolatorNormal.h"
#include "carma/util/QuadraticInterpolatorPositiveAngle.h"
#include "carma/util/QuadraticInterpolatorSignedAngle.h"
#include "carma/util/ScopedLogNdc.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


template < typename QI >
void
testIt( QI & qI )
try {
    const ScopedLogNdc
        ndc( "testIt< " + demangleTypeName( typeid( QI ) ) + " >" );
    
    programLogInfo( "Starting" );
    
    {
        ScopedQILockManager lm1( qI, true );
    }

    {
        ScopedQILockManager lm1( qI, true );
        
        lm1.lockQI();
        lm1.unlockQI();
    }

    {
        ScopedQILockManager lm1( qI, true );
        
        lm1.lockQI();
    }

    {
        bool good1 = false;
        bool bad2 = false;
        bool good3 = false;
        
        try {
            ScopedQILockManager lm1( qI, true );
            
            lm1.lockQI();
            
            good1 = true;
            
            lm1.lockQI();
            
            bad2 = true;
        } catch ( ... ) {
            good3 = true;
            // Just stifle the exception
        }
        
        if ( (good1 != true) || (bad2 != false) || (good3 != true) ) {
            const string msg = "Double lock was not detected correctly";
            
            programLogError( msg );
            
            throw CARMA_EXCEPTION( ErrorException, msg );
        }
    }

    {
        bool good1 = false;
        bool bad2 = false;
        bool good3 = false;
        
        try {
            ScopedQILockManager lm1( qI, true );
            
            good1 = true;
            
            lm1.unlockQI();
            
            bad2 = true;
        } catch ( ... ) {
            good3 = true;
            // Just stifle the exception
        }
        
        if ( (good1 != true) || (bad2 != false) || (good3 != true) ) {
            const string msg = "Bad unlock was not detected correctly";
            
            programLogError( msg );
            
            throw CARMA_EXCEPTION( ErrorException, msg );
        }
    }

    {
        bool good1 = false;
        bool bad2 = false;
        bool good3 = false;
        
        try {
            ScopedQILockManager lm1( qI, true );
            
            lm1.lockQI();
            lm1.unlockQI();

            good1 = true;
            
            lm1.unlockQI();
            
            bad2 = true;
        } catch ( ... ) {
            good3 = true;
            // Just stifle the exception
        }
        
        if ( (good1 != true) || (bad2 != false) || (good3 != true) ) {
            const string msg = "Double unlock was not detected correctly";
            
            programLogError( msg );
            
            throw CARMA_EXCEPTION( ErrorException, msg );
        }
    }

    programLogInfo( "Done" );
} catch ( ... ) {
    programLogError(
        "Coming out of testIt< " + demangleTypeName( typeid( QI ) ) +
        " > on an exception: " + getStringForCaught() );

    throw;
}


void
testAll( )
{
    {
        QuadraticInterpolatorNormal qINormal;

        testIt( qINormal );
    }

    {
        QuadraticInterpolatorPositiveAngle qIPositiveAngle;

        testIt( qIPositiveAngle );
    }

    {
        QuadraticInterpolatorSignedAngle qISignedAngle( 0.0 );

        testIt( qISignedAngle );
    }
}


}  // namespace < anonymous >


int
Program::main( )
try {
    testAll();

    programLogInfo( "All Done" );
    
    return EXIT_SUCCESS;
} catch ( ... ) {
    return EXIT_FAILURE;
}
