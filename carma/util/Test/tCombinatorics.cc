//
// @version $Revision: 1.1 $
//
// @usage use it
//
// @description
//  Test program for testing the combinatoric utilities.
//
// @key minN     0 int Min value of N to test with
// @key maxN 10000 int Max value of N to test with
//
// @logger TEST_FACILITY carma.test.util.tCombinatorics
//

#include <sstream>

#include "carma/util/combinatorics.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


void
test( const int minN,
      const int maxN )
{
    int sum = 0;
    int n = 0;
    
    for ( ; n < minN; ++n )
        sum += n;
    
    int trueMinN = 0;
    int trueMaxN = 0;
    
    for ( ; n <= maxN; ++n ) {
        sum += n;
        
        const int computedSum = sumOf1ToN( n );
            
        if ( computedSum != sum ) {
            ostringstream oss;
            
            oss << "Expected to compute a sum from 1 to " << n
                << " of " << sum << " but instead got " << computedSum;
                
            programLogErrorIfPossible( oss.str() );
            
            throw CARMA_ERROR( oss.str() );
        }
        
        trueMinN = ::std::min( trueMinN, n );
        trueMaxN = ::std::max( trueMaxN, n );
    }
    
    {
        ostringstream oss;
        
        oss << "Sums for n from " << trueMinN << " to " << trueMaxN
            << " now completed";
            
        programLogInfoIfPossible( oss.str() );
    }
}


}  // namespace < anonymous >


int
Program::main( )
{
    const int minN = getIntParameter( "minN" );
    const int maxN = getIntParameter( "maxN" );
    
    test( minN, maxN );

    programLogInfoIfPossible( "All test done" );

    return 0;
}
