// $Id: tDecimator.cc,v 1.1 2011/08/18 23:25:54 abeard Exp $

#include "carma/correlator/lib/CorrelatorDataTestSl.h"
#include "carma/pipeline/DecimatorStage.h"
#include "carma/monitor/PipelineSubsystemSL.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

#include <boost/foreach.hpp>
#include <iostream>

using namespace carma::correlator::lib;
using namespace carma::monitor;
using namespace carma::pipeline;
using namespace carma::util;

using namespace std;

namespace {

    typedef vector< carma::correlator::lib::CorrelatorBand > CorrBands;

    void
    processData( Decimator & decimator, CorrelatorDataPtr corrData )
    {
        CorrBands bands = corrData->getBands();
        decimator.preprocessCorrelatorData( corrData );

        CorrBands::iterator bBegin = bands.begin();
        CorrBands::iterator bEnd =  bands.end();
        for ( CorrBands::iterator b = bBegin; b != bEnd; ++b ) 
            decimator.processCorrelatorBand( &( *b ) );

        decimator.postprocessCorrelatorData( corrData );
    }

    void 
    processCorrelatorData( Decimator & decimator, const int numChannels ) {

        CorrelatorDataPtr corrData( new CorrelatorDataTestSl( numChannels ) );

        const double start = Time::MJD( );

        CorrBands bands = corrData->getBands();

        decimator.preprocessCorrelatorData( corrData );

        CorrBands::iterator bBegin = bands.begin();
        CorrBands::iterator bEnd =  bands.end();
        for ( CorrBands::iterator b = bBegin; b != bEnd; ++b )
        {
            decimator.processCorrelatorBand( &( *b ) );
        }

        decimator.postprocessCorrelatorData( corrData );

        const double end = Time::MJD( );

        const std::vector<carma::correlator::lib::CorrelatorBand>::size_type
        nBands = corrData->getBands().size();

        int nInputs = 0;
        if ( nBands > 0 ) {
            nInputs = corrData->getBands().at( 0 ).getNumberOfInputs( );
        }

        cout << "Processed " << nBands << " bands of " << nInputs << " station "
             << "corr data with " << numChannels << " channels in " 
             << ( ( end - start ) * Time::MILLISECONDS_PER_DAY ) << " ms." 
             << endl;
    }

} // End namespace <unnamed>

/**
 * @version $Revision: 1.1 $
 *
 * @description
 * Test program to exercise and benchmark carma::pipeline::Decimator.
 *
 * @usage tDecimator [wisdom=<filename>]
 * 
 * @key wisdom @noDefault s Filename which if specified, calculate and 
 *      save fftw wisdom using EXHAUSTIVE rigor for default decimator sizes to.
 * @key usewisdom @noDefault s Import wisdom from specified file and test.
 *
 * @logger TEST_FACILITY carma.test.tDecimator
 */

int 
Program::main( ) 
{
    PipelineSubsystemSL mon;

    if ( Program::parameterWasSpecified( "wisdom" ) ) {
        const string saveto = Program::getStringParameter( "wisdom" );
        cout << "Calculating wisdom for default decimator plans and saving to "
            << saveto << "..." << endl;
        FftwRealToRealPlan::forgetWisdom();
        set< FftwRealVector::size_type > defaults = 
            Decimator::defaultFftwPlanSizes();
        BOOST_FOREACH( const FftwRealVector::size_type & s, defaults ) {
            cout << "  Exhaustively calculating wisdom for " 
                << s << " element r2hc plan." << endl;
            FftwRealVector fftwVec( s );
            FftwRealToRealPlan r2hcPlan( 
                s, fftwVec, 
                FftwRealToRealPlan::REAL_TO_HALFCOMPLEX,
                FftwRealToRealPlan::EXHAUSTIVE );
            cout << "  Exhaustively calculating wisdom for " 
                << s << " element hc2r plan." << endl;
            FftwRealToRealPlan hc2rPlan( 
                s, fftwVec, 
                FftwRealToRealPlan::HALFCOMPLEX_TO_REAL,
                FftwRealToRealPlan::EXHAUSTIVE );
        }
        FftwRealToRealPlan::exportWisdom( saveto ); 
        return 0;
    }

    cout << "Using decimator default channel sizes and no wisdom..." << endl;
    {
        FftwRealToRealPlan::forgetWisdom();
        Decimator decimator( mon, carma::monitor::CORRELATOR_SPECTRAL_LINE );

        // Now use our default channel sizes
        set< FftwRealVector::size_type > defaults = 
            Decimator::defaultFftwPlanSizes();
        BOOST_FOREACH( const FftwRealVector::size_type & s, defaults ) {
            const int chans = ( s + 2 ) / 2;
            processCorrelatorData( decimator, chans );
        }
    }
    cout << endl;

    if ( Program::parameterWasSpecified( "usewisdom" ) ) {

        const string wisdom = getStringParameter( "usewisdom" );

        FftwRealToRealPlan::forgetWisdom();

        FftwRealToRealPlan::importWisdom( wisdom );  

        Decimator decimator( mon, carma::monitor::CORRELATOR_SPECTRAL_LINE );

        cout << "Using decimator default channel sizes and saved wisdom (" 
             << wisdom << ")." << endl;

        // Now use our default channel sizes
        set< FftwRealVector::size_type > defaults = 
            Decimator::defaultFftwPlanSizes();
        BOOST_FOREACH( const FftwRealVector::size_type & s, defaults ) {
            const int chans = ( s + 2 ) / 2;
            processCorrelatorData( decimator, chans );
        }
    }

    return 0;
}
