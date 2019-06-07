
#include "carma/util/FftwRealToRealPlan.h"
#include "carma/util/FftwRealToRealPlanManager.h"

#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/SimpleStatisticsAccumulators.h"
#include "carma/util/Time.h"


#include <cassert>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <limits>

#include <boost/foreach.hpp>

using namespace boost;
using namespace carma::util;
using namespace std;

namespace {
    
template <typename T>
void 
assertCloseEnough( const T check, const T against, const T enough ) 
{
    const T diff = ( check > against ? check - against : against - check );
    assert( diff <= enough );
}

} // namespace < unnamed >

/**
 * @version $Revision: 1.3 $
 *
 * @description
 * Test to exercise FftwRealToRealPlan and FftwRealToRealPlanManager classes.
 *
 * @usage tFftwRealToRealPlan
 *
 * @noKeys
 *
 * @logger TEST_FACILITY carma.test.tFftwRealToRealPlan
 */
int Program::main( ) 
try {
    const double epsilon = numeric_limits<double>::epsilon( );
    const FftwRealVector::size_type maxSize = 
        static_cast<FftwRealVector::size_type>( ::pow( 2, 11 ) );
    

    cout << "Creating plan manager... " << flush;
    FftwRealToRealPlanManager manager( maxSize );
    cout << "done" << endl;
    FftwRealVector & workVec = manager.getFftwRealVector( );

    // Populate manager with common power of 2 plans.
    cout << "Populating manager with plans... " << flush;
    for ( unsigned o = 3; o < 12; ++o ) {
        const int N = ( static_cast< int >( ::pow( 2, o ) ) / 2 );

        manager.createPlan( N, FftwRealToRealPlan::REAL_TO_HALFCOMPLEX ); 
        manager.createPlan( N, FftwRealToRealPlan::HALFCOMPLEX_TO_REAL ); 
    }
    cout << "done" << endl;

    { 
        cout << "Verify that DC input transforms to delta function... " 
            << flush;
        FftwRealToRealPlan & r2hcPlan = 
            manager.retrievePlan( 32, FftwRealToRealPlan::REAL_TO_HALFCOMPLEX );
        workVec.resize( 32 );

        FftwRealVector::iterator element = workVec.begin( );
        for ( ; element != workVec.end( ); ++element ) {
            *element = 0.5;
        }

        r2hcPlan.execute( ); // Perform fft on work vector

        // Verify delta function.
        assert( workVec.at( 0 ) >= 1.0 );
        element = workVec.begin( );
        ++element;
        for ( ; element != workVec.end( ); ++element ) {
            assertCloseEnough( *element, 0.0, 0.00001 );
        }

        // Verify subsequent inverse fft produces original DC signal
        FftwRealToRealPlan & hc2rPlan = 
            manager.retrievePlan( 32, FftwRealToRealPlan::HALFCOMPLEX_TO_REAL );
            
        hc2rPlan.execute( );

        element = workVec.begin( );
        for ( ; element != workVec.end( ); ++element ) {
            assertCloseEnough( *element, 0.5, 0.00001 );
        }
        cout << "they do" << endl;
    }
    
    {
        cout << "Verify on the fly creation of plans... " << flush;
        FftwRealVector::size_type size = 34;
        workVec.resize( 0 );
        workVec.resize( size, 0.0 );
        workVec[0] = 1.0;

        FftwRealToRealPlan & hc2rPlan = manager.retrievePlan( 
            size, 
            FftwRealToRealPlan::HALFCOMPLEX_TO_REAL );

        hc2rPlan.execute( );
        
        FftwRealVector::iterator element = workVec.begin( );
        for ( ; element != workVec.end( ); ++element ) {
            assertCloseEnough( *element, workVec.at( 0 ), 0.00001 );
            assert( *element != 0.0 );
        }
        
        // Verify subsequent forward fft produces original signal.
        FftwRealToRealPlan & r2hcPlan = manager.retrievePlan( 
            size, 
            FftwRealToRealPlan::REAL_TO_HALFCOMPLEX );

        r2hcPlan.execute( );
        
        assert( workVec.at( 0 ) == 1.0 );
        
        element = workVec.begin( );
        ++element;
        for ( ; element != workVec.end( ); ++element ) {
            assert( fabs( *element ) <  epsilon ); // 0.0 is not representable 
        }
        cout << "verified" << endl;
    }

    {
        // Let's assume input is zero padded.
        // a) Create zero padded input and compute forward fft.  This represents
        //  our input spectra.
        // b) Take input spectra, inverse fft it and verity that it is indeed
        //  zero padded.  
        // c) Drop the padding and place time domain data into a smaller plan.
        // d) Compute forward FFT on this data, this is our new smaller spectra.
        // e) Compare the original spectra with the new spectra.
        const FftwRealVector::size_type paddedSize = 32;
        const FftwRealVector::size_type nopadSize = 28;
        
        FftwRealToRealPlan & paddedR2HcPlan = manager.retrievePlan( 
            paddedSize, 
            FftwRealToRealPlan::REAL_TO_HALFCOMPLEX );
        FftwRealToRealPlan & paddedHc2RPlan = manager.retrievePlan( 
            paddedSize, 
            FftwRealToRealPlan::HALFCOMPLEX_TO_REAL );
        FftwRealToRealPlan & nopadPlan = manager.retrievePlan( 
            nopadSize, 
            FftwRealToRealPlan::REAL_TO_HALFCOMPLEX );

        workVec.resize( paddedSize );

        // Use an all DC input and pad it at the end
        for ( FftwRealVector::size_type i = 0; i < nopadSize; ++i ) {
            workVec.at( i ) = 0.5;
        } 
        for ( FftwRealVector::size_type i = nopadSize; i < paddedSize; ++i ) {
            workVec.at( i ) = 0.0;
        }

        paddedR2HcPlan.execute( );

        // workVec now contains our example input spectra.

        // Convert to lag space and verify that it matches our input 'exactly'.
        paddedHc2RPlan.execute( );
        
        for ( FftwRealVector::size_type i = 0; i < nopadSize; ++i ) {
            assertCloseEnough( workVec.at( i ), 0.5, 0.00001 );
        } 

        for ( FftwRealVector::size_type i = nopadSize; i < paddedSize; ++i ) {
            assert( fabs( workVec.at( i ) ) < epsilon );
        }

        // Get rid of zero padding then forward fft this guy
        workVec.resize( nopadSize ); 

        nopadPlan.execute( );

        // workVec now represents our final spectra.

        // Verify that it inverse ffts to the original signal - padding.
        FftwRealToRealPlan & nopadHc2RPlan = manager.retrievePlan( 
            nopadSize, 
            FftwRealToRealPlan::HALFCOMPLEX_TO_REAL );

        nopadHc2RPlan.execute( );
        
        FftwRealVector::iterator element = workVec.begin( );
        for ( ; element != workVec.end( ); ++element ) {
            assert( ( *element < 0.5  + epsilon ) && 
                    ( *element > 0.5 - epsilon ) );
        }
    }

    {
        cout << "Verify that work vector reallocation throws... " << flush;
        workVec.resize( workVec.capacity( ) + 1 );

        FftwRealToRealPlan & plan = manager.retrievePlan( 
            32, 
            FftwRealToRealPlan::REAL_TO_HALFCOMPLEX );

        try {
            plan.execute( );
            assert( false );
        } catch (...) {
            // assert( true );
        }
        cout << "it does" << endl;
    }

    cout << "Calculating creation time using different rigors " << endl;
    
    double baselineCreation = 0.0; 
    bool baselineSet = false;

    for ( int i = 0; i < 2; ++i ) {
        const FftwRealVector::size_type size = 320;
    
        FftwRealVector workVec( size );

        FftwRealToRealPlan::RigorEnumMap rigors = FftwRealToRealPlan::rigors();
        BOOST_FOREACH( 
            const FftwRealToRealPlan::RigorEnumMap::value_type & rigor, 
            rigors )  {
        
            if ( i == 0 ) FftwRealToRealPlan::forgetWisdom();

            double start = Time::MJD();
            FftwRealToRealPlan plan( size, 
                                     workVec, 
                                     FftwRealToRealPlan::REAL_TO_HALFCOMPLEX, 
                                     rigor.first ); 
            double end = Time::MJD();
            const double createAvg =  
                ( end - start ) * Time::MILLISECONDS_PER_DAY;

            double speedup;
            string speedupStr;
            if ( !baselineSet ) {
                baselineCreation = createAvg; 
                baselineSet = true;
                speedupStr = "baseline";
                speedup = 1.0;
            } else {
                if ( createAvg < baselineCreation ) {
                    speedupStr = "faster";
                    speedup = baselineCreation / createAvg;
                } else {
                    speedupStr = "slower";
                    speedup = createAvg / baselineCreation;
                }
            }

            cout << fixed << " rigor " << rigor.second << " created in " 
                 << setprecision(2) << createAvg << "ms (" 
                 << setprecision(0) << speedup << "x " 
                 << speedupStr << ") " << flush;
            if ( i == 0 ) cout << "with no wisdom." << endl;
            else cout << "with saved wisdom." << endl;
        }

    } // Rigor creation test
    
    cout << endl;

    double baselineExecution = 0.0;
    baselineSet = false;
    for ( int i = 0; i < 2; ++i ) {

        const FftwRealVector::size_type size = 320;

        cout << "Calculating performance of plans with different rigors " 
             << flush;

        if ( i == 0 ) {
            cout << "and no wisdom..." << endl;
        } else {
            cout << "and saved wisdom..." << endl;
        }
    
        FftwRealVector workVec( size );
        
        FftwRealVector tmpVec( size );
        BOOST_FOREACH( FftwRealVector::value_type & val, tmpVec ) 
            val = ::random( );

        FftwRealToRealPlan::RigorEnumMap rigors = FftwRealToRealPlan::rigors();
        BOOST_FOREACH( 
            const FftwRealToRealPlan::RigorEnumMap::value_type & rigor, 
            rigors ) {
        
            if ( i == 0 ) FftwRealToRealPlan::forgetWisdom();

            FftwRealToRealPlan plan( size, 
                                     workVec, 
                                     FftwRealToRealPlan::REAL_TO_HALFCOMPLEX, 
                                     rigor.first ); 

            const double NANOS_PER_DAY = Time::MILLISECONDS_PER_DAY * 1000.0;
            double start, end;
            const int trials = 10000;
            DoubleStatAccumulator avgAcc;
            for ( int i = 0; i < trials; ++i ) {
                workVec.assign( tmpVec.begin(), tmpVec.end() );
                start = Time::MJD();
                plan.execute();
                end = Time::MJD();
                avgAcc( ( end - start ) * NANOS_PER_DAY );
            }

            const double runAvg = accumulators::mean( avgAcc );

            if ( !baselineSet ) {
                baselineExecution = runAvg;
                baselineSet = true;
            }

            const double runVar = ::sqrt( ::fabs( 
                    accumulators::variance( avgAcc ) ) );
            const double runSpeedup = baselineExecution / runAvg;

            cout << "  " << rigor.second << " executed avg " 
                 << fixed << setprecision(1) << runAvg 
                 << "+/-" << setprecision(0) << runVar << "ns (" 
                 << setprecision(1) << runSpeedup << "x), "
                 << "max " << setprecision(1) << accumulators::max( avgAcc ) 
                 << "ns and min " << setprecision(3) 
                 << accumulators::min( avgAcc ) << "ns." << endl;

        }
        cout << endl;
    } // Rigor performance test.

    return 0;
} catch ( const carma::util::ErrorException & ex ) {
    cerr << ex << endl;
    return 1;
}
