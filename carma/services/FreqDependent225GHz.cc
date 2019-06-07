
#include "carma/services/FreqDependent225GHz.h"

using namespace carma::services;

FreqDependent225GHz::FreqDependent225GHz( ) { }

FreqDependent225GHz::~FreqDependent225GHz( ) { }

double 
FreqDependent225GHz::calculateOpacityAtZenith( 
    const double frequencyInGHz,
    const double temperatureInK,
    const double dewpointInK,
    const double pressureInMbar,
    const double humidityInPercent ) const
{
    const double BETA_225 = 0.06;
    const double OXYGEN_OPACITY_IN_NEPERS = 0.005;
    const double NOMINAL_FREQ_IN_GHZ = 225;
    
    const double mmh2o = atmosphere_.waterColumn( temperatureInK, 
                                                  humidityInPercent );
    const double tau225 = BETA_225 * mmh2o + OXYGEN_OPACITY_IN_NEPERS;
    
    return tau225 * pow( ( frequencyInGHz / NOMINAL_FREQ_IN_GHZ ), 2.0 );
}
