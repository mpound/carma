#include "carma/util/Program.h"

#include <sstream>
#include <iomanip>
#include <cmath>

#include "carma/control/LOchain.h"
#include "carma/util/programLogging.h"
#include "carma/util/ErrorException.h"

using namespace ::std;
using namespace carma;
using namespace carma::control;
using namespace carma::util;


namespace {


struct ExpectedResult {
    double loFreqInGHz;
    double prefYigFreqInGhz;
};


const ExpectedResult kExpectedResults[] = {
    { 88.5, 9.827777777777779 }
};


const size_t kExpectedResultsCount =
    (sizeof( kExpectedResults ) / sizeof( kExpectedResults[ 0 ] ));


void
testIt( )
{
    for ( size_t i = 0; i < kExpectedResultsCount; ++i ) {
        const double loFreqInGHz = kExpectedResults[ i ].loFreqInGHz;
        
        const Freq loFreq( loFreqInGHz, Freq::GHz );
        
        const LOchain loChain( loFreq );
        
        const Harmonic prefHarm = loChain.getPreferredHarmonic();
        const double prefYigFreqInGhz = prefHarm.getYigFreq().gigahertz();
        
        if ( ::nextafter( prefYigFreqInGhz, 0.0 ) != 
             ::nextafter( kExpectedResults[ i ].prefYigFreqInGhz, 0.0 ) ) {
            ostringstream oss;
            
            oss << setiosflags( ios::fixed ) << setprecision( 15 )
                << "Bad preferred yig frequency of " << prefYigFreqInGhz
                << "GHz instead of " << kExpectedResults[ i ].prefYigFreqInGhz
                << "GHz for an LO frequency of " << loFreqInGHz << "GHz";
                
            programLogErrorIfPossible( oss.str() );

            throw CARMA_ERROR( oss.str() );
        } else {
            ostringstream oss;
            
            oss << setiosflags( ios::fixed ) << setprecision( 15 )
                << "Good preferred yig frequency of " << prefYigFreqInGhz
                << "GHz for an LO frequency of " << loFreqInGHz << "GHz";
                
            programLogInfoIfPossible( oss.str() );
        }
    }
}


}  // namespace < anonymous >


//
// @version $Revision: 1.3 $
//
// @usage use it
//
// @description
//   Unit test for the LOchain class.
//
// @noKeys
//
// @logger TEST_FACILITY carma.test.control.utLOchain
//


int
Program::main( )
try {
    testIt();
    
    return EXIT_SUCCESS;
} catch ( ... ) {
    return EXIT_FAILURE;
}
