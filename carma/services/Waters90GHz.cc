#include "carma/services/Waters90GHz.h"

#include "carma/util/Trace.h"

using namespace carma::services;
using namespace carma::util;

namespace {

    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE5;

} // End namespace <unnamed>

Waters90GHz::Waters90GHz( ) 
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "Waters90GHz() - C'tor." );
}

Waters90GHz::~Waters90GHz( ) 
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "~Waters90GHz() - D'tor." );
}

double
Waters90GHz::calculateOpacityAtZenith( const double frequency,
                                       const double temperature,
                                       const double dewpoint,
                                       const double pressure,
                                       const double humidity ) const
{
    const double alpha0 = 0.039;
    const double alpha1 = 0.0090;
    double wvd = atmosphere_.waterVaporDensity( temperature, humidity );
    return alpha0 + ( alpha1 * wvd );
}
