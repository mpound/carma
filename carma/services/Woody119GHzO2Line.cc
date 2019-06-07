#include "carma/services/Woody119GHzO2Line.h"

#include "carma/util/Trace.h"

#include <cmath>

using namespace carma::services;
using namespace carma::util;

namespace {

    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE5;

} // End namespace <unnamed>

Woody119GHzO2Line::Woody119GHzO2Line( ) 
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "Woody119GHzO2Line() - C'tor." );
}

Woody119GHzO2Line::~Woody119GHzO2Line( ) 
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "~Woody119GHzO2Line() - D'tor." );
}

double 
Woody119GHzO2Line::calculateOpacityAtZenith( const double frequency,
                                             const double temperature,
                                             const double dewpoint,
                                             const double pressure,
                                             const double humidity ) const
{
    return Woody119GHzO2Line::calculateOpacityAtZenith( frequency,
                                                        temperature,
                                                        pressure );
}

double
Woody119GHzO2Line::calculateOpacityAtZenith( const double frequency,
                                             const double temperature,
                                             const double pressure )
{
    static const double nominalPressure = 876.0; // millibar
    static const double nominalTemperature = 300.0; // Kelvin
    double pFac = pressure / nominalPressure;
    double tFac = nominalTemperature / temperature;

    if ( ( pFac < 0.1 ) || ( pFac > 2.0 ) )
        pFac = 1.0;

    if ( ( tFac < 0.5 ) || ( tFac > 2.0 ) )
        tFac = 1.0;

    double tau = 
        ( 3.57 * pow( pFac, 2.0 ) * pow( tFac, 2.5 ) ) /
        ( ( pow( frequency - 118.75, 2 ) + 1.4 * pFac * sqrt( tFac ) ) );

    if ( tau > 100.0 ) 
        tau = 100.0;
    return tau;
}
