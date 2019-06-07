#include "carma/pipeline/pipelineUtils.h"
#include "carma/util/IllegalArgumentException.h"

#include <boost/algorithm/string.hpp>

using namespace boost;
using namespace carma::pipeline;
using namespace carma::util;
using namespace carma::monitor;
using namespace std;

string
carma::pipeline::pipelineTypeToString( const enum PipelineType pt )
{
    switch ( pt ) {
        case WB:
            return "Wb";
        case SL: 
            return "Sl";
        case C3G23:
            return "C3g23";
        case C3G8:
            return "C3g8";
        default:
            throw CARMA_EXCEPTION( IllegalArgumentException, 
                                   "Invalid PipelineType." );
    }
}

enum PipelineType
carma::pipeline::stringToPipelineType( string str )
{
    boost::trim( str );
    boost::to_lower( str );
    if ( str == "sl" )
        return SL;
    else if ( str == "wb" )
        return WB;
    else if ( str == "c3g23" )
        return C3G23;
    else if ( str == "c3g8" )
        return C3G8;
    else
        throw CARMA_EXCEPTION( IllegalArgumentException, 
                               "Invalid string for PipelineType." );
}

string
carma::pipeline::getDefaultCatchDataIpqName( const enum PipelineType pt )
{
    return "catch" + pipelineTypeToString( pt ) + "DataIPQ";
}

unsigned int
carma::pipeline::getDefaultCatchDataIpqElementBytes( 
    const enum PipelineType pt )
{
    switch ( pt ) {
        case WB:
            return 390000; 
        case SL:
            return 20000000;
        case C3G23:
            return 20000000;
        case C3G8:
            return 20000000;
        default:
            throw CARMA_EXCEPTION( IllegalArgumentException, 
                                   "Invalid string for PipelineType." );
   }
}

AstrobandRange
carma::pipeline::getAstrobandRange( const enum PipelineType pt )
{
    switch ( pt ) {
        case SL:
            return make_pair( 1, 8 );
        case WB:
            return make_pair( 9, 24 ); 
        case C3G23:
            return make_pair( 25, 32 );
        case C3G8:
            return make_pair( 33, 40 );
        default:
            throw CARMA_EXCEPTION( IllegalArgumentException, 
                                   "Invalid string for PipelineType." );
    }
}

unsigned int
carma::pipeline::getAstrobandCount( const enum PipelineType pt )
{
    const pair< unsigned int, unsigned int > astrobandRange =
        getAstrobandRange( pt );

    return ( astrobandRange.second + 1 ) - astrobandRange.first;
}

//@Todo replace with monitor::corrTypeToCorrDes()? - MWP
MonitorCorrelatorDesignation
carma::pipeline::getMonCorrDes( const enum PipelineType pt )
{
    switch( pt ) {
        case WB: return CorrDesignation::WIDEBAND;
        case SL: return CorrDesignation::SPECTRAL;
        case C3G23: return CorrDesignation::C3GMAX23;
        case C3G8: return CorrDesignation::C3GMAX8;
        default:
            throw CARMA_EXCEPTION( IllegalArgumentException, 
                                   "Invalid PipelineType." );
    }
}
