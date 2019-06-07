#include "carma/services/SimpleOpacityModel.h"

#include "carma/util/Trace.h"

using namespace carma::services;
using namespace carma::util;

namespace {

    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE5;

} // End namespace <unnamed>

SimpleOpacityModel::SimpleOpacityModel( )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "SimpleOpacityModel() - C'tor." );
}

SimpleOpacityModel::~SimpleOpacityModel( )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "~SimpleOpacityModel() - D'tor." );
}

double 
SimpleOpacityModel::calculateOpacityAtZenith( double frequencyInGHz, 
                                              double temperatureInK,
                                              double dewpointInK,
                                              double pressureInMbar,
                                              double humidityInPercent ) const
{
    return woodyModel_.calculateOpacityAtZenith( frequencyInGHz,
                                                 temperatureInK, 
                                                 dewpointInK,
                                                 pressureInMbar,
                                                 humidityInPercent ) 
            + freq1mm_.calculateOpacityAtZenith( frequencyInGHz,
                                                 temperatureInK,
                                                 dewpointInK,
                                                 pressureInMbar,
                                                 humidityInPercent );
}
