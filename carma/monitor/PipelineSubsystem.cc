// $Id: PipelineSubsystem.cc,v 1.4 2006/09/01 00:42:26 abeard Exp $

#include "carma/monitor/PipelineSubsystem.h"

#include <cmath>

using namespace carma::monitor;

namespace {

    ::std::pair<int, int> 
    computeBaselineAntennaPair( const int baselineNo )
    {
        const int j = static_cast<int>(
            1.500 + sqrt( ( 0.25 + 2.0 * ( baselineNo - 1.0 ) ) ) );
        const int i = ( baselineNo - ( ( j - 1 ) * ( j - 2 ) / 2 ) );
        return ::std::pair<int, int>( i, j );
    }

}

PipelineSubsystem::PipelineSubsystem( ) 
{

}

PipelineSubsystem::~PipelineSubsystem( )
{

}
