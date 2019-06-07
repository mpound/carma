// $Id: Astro.cc,v 1.16 2013/07/16 13:41:30 mpound Exp $
/**
 * Astronomical Constants used across CARMA.  Units follow 
 * CARMA Standard Units where applicable, SI otherwise.
 *
 * Planetary data come from NOVAS, JPL Ephemeris and http://nineplanets.org.
 * They not meant to supplant ephemeris values calculated for a specific date, 
 * but are simply for quick calculations that do not need high accuracy.
 *
 * @see http://www.mmarray.org/workinggroups/computing/CarmaStdUnits.html
 * @see http://ssd.jpl.nasa.gov/cgi-bin/eph
 * @see http://nineplanets.org
 * @see http://astrogeology.usgs.gov/Projects/WGCCRE/
 *
 * @author Marc Pound
 * @version $Revision: 1.16 $
 * @file carma/services/Astro.cc
 */

#include "carma/services/Astro.h"
#include "carma/services/SourceNotFoundException.h"
#include "carma/util/StringUtils.h"
#include <map>
#include <iostream>
#include <sstream>

using namespace carma::services::constants;
using namespace carma::util;
using namespace std;
/**
 * Planet names
 */
const std::string Astro::PLANETNAME_[] = 
{
    "sun","mercury","venus","earth","moon", "mars","jupiter",
    "saturn","uranus","neptune","pluto"
};

const Astro::planetType Astro::PLANETDATA_[] = {
    SUN, MERCURY, VENUS, EARTH, MOON, MARS, JUPITER,
    SATURN, URANUS, NEPTUNE, PLUTO
};

std::map<std::string, Astro::planetType> * Astro::planets_ = 0;


Astro::Astro() 
{ 
}

Astro::~Astro() 
{ 
    delete planets_;
    planets_ = 0;
}

void  Astro::initPlanets()
{
    planets_ = new std::map<std::string,planetType>;
    for (unsigned int i = 0; i < NUM_PLANETS; i++ ) {
    planets_->insert(std::make_pair(Astro::PLANETNAME_[i], PLANETDATA_[i]));
    }

}


Astro::planetType 
Astro::getPlanet(const std::string& planetName)
{
    std::string indexName = 
    StringUtils::lowASCIIAlphaNumericToLower(planetName);
    if ( planets_ == 0 ) { initPlanets(); }
    if ( planets_->find(indexName) == planets_->end() ) {
    ostringstream osX;
        osX << "Planet " << planetName << " not recognized.";
    throw CARMA_EXCEPTION(SourceNotFoundException,osX.str());
    } else 
        return planets_->find(indexName)->second;
}


bool Astro::isPlanet(const std::string& planetName)
{
    std::string indexName = 
    StringUtils::lowASCIIAlphaNumericToLower(planetName);
    if ( planets_ == 0 ) { initPlanets(); }
    if ( planets_->find(indexName) == planets_->end() )
    return false;
    else
    return true;
}

//=======================  BASIC PLANETARY DATA =============================
//
/** Basic data for Sun */
const Astro::planetType Astro::SUN = {
    "sun",
    6.960E8,
    1.9891E30,
    1.0,
    0.0,  // yep, it is zero
    5800.0,
    0.0,
    286.13,
    63.87,
    0.0, // no secular variation in polar RA
    0.0, // no secular variation in polar DEC
    84.1,
    14.1844
};


/** Basic data for Mercury */
const Astro::planetType Astro::MERCURY = {
    "mercury",
    2.440E6,
    3.302E23,
    1.0,
    0.38,
    200.0, //???Low. This is from BIMA on-line software ephplanet.f
    0.0,
    281.01,
    61.45,
    -0.033,
    -0.005,
    329.548,
    6.1385025
};

/** Basic data for Venus */
// Note for Venus we have the ALMA models available
// in conf/data/venustb.tab, which are used
// in Planet.cc/PlanetTemperature.cc for online flux calibration.
// The default power-law is only used for frequencies or
// dates that our outside the range of the model.
const Astro::planetType Astro::VENUS = {
    "venus",
    6.0518E6,
    4.8685E24,
    1.0, 
    0.72,
    354.2,
    -0.207,
    272.76,
    67.16,
    0.0, // no secular variation in polar RA
    0.0, // no secular variation in polar DEC
    160.20,
    1.4813688
};

/** Basic data for Earth */
const Astro::planetType Astro::EARTH = {
    "earth",
    6.3781366E6, // from NOVAS 
    5.9742E24,
    0.997,
    1.0,
    288.0,
    0.0,
    0.0,
    90.0,
    -0.641,
    -0.557,
    190.147,
    360.9856235
};

/** Basic data for Moon */
const Astro::planetType Astro::MOON = {
    "moon",
    1.7380E6,
    7.3483E22,
    0.998, 
    1.0,
    200.0,
    0.0,
    269.9949,
    66.5392,
    0.0031, // note the moon has some higher order terms for
            // its polar angle rate of change
    0.0130, // which I choose to ignore. Since the
            // moon has essentially no oblateness its polar angle doesn't
            // matter for flux calibration. Besides, it more than
            // fills the primary beam.
    38.3213,      // again, many higher order terms here as well
    13.17635815
};

/** Basic data for Mars */
const Astro::planetType Astro::MARS = {
// Note for Mars we have the Gurwell models available
// in conf/data/marstb.tab.  These are used
// in Planet.cc/PlanetTemperature.cc for online flux calibration.
// The default power-law is only used for frequencies or
// dates that our outside the range of the model.
    "mars",
    3.38990E6,
    6.4185E23,
    0.99352369,
    1.52,
    207.0,
    0.029,
    317.68143,
    52.88650,
    -0.1061,
    -0.0609,
    176.630,
    350.89198226
};

/** Basic data for Jupiter */
// Note for Jupiter we have the ALMA models available
// in conf/data/jupitertb.tab, which are used
// in Planet.cc/PlanetTemperature.cc for online flux calibration.
// The default power-law is only used for frequencies or
// dates that our outside the range of the model.
const Astro::planetType Astro::JUPITER = {
    "jupiter",
    7.14920E7, //equatorial
    1.8986E27,
    0.93513,
    5.20,
    179.0,
    0.0,
    268.05,  
    64.49, 
    -0.009,
    0.003,
    284.95,
    870.5366420
};

/** Basic data for Saturn */
const Astro::planetType Astro::SATURN = {
    "saturn",
    6.02680E7, //equatorial
    5.68E26,
    0.90204,
    9.54,
    149.0,
    0.0,
    40.589,
    83.537,
    -0.036,
    -0.004,
    38.90,
    810.7939024
};

/** Basic data for Uranus */
// Note for Uranus we have the ALMA models available
// in conf/data/uranustb.tab, which are used
// in Planet.cc/PlanetTemperature.cc for online flux calibration.
// The default power-law is only used for frequencies or
// dates that our outside the range of the model.
const Astro::planetType Astro::URANUS = {
    "uranus",
    2.55590E7, // equatorial
    8.683E25,
    0.97707,
    19.218,
    134.7,
    -0.337,
    257.311,
    -15.175,
    0.0, // no secular variation in polar RA
    0.0, // no secular variation in polar DEC
    203.81,
    501.1600928

};

/** Basic data for Neptune */
const Astro::planetType Astro::NEPTUNE = {
    "neptune",
    2.47660E7, // equatorial
    1.0247E26,
    0.9829,
    30.06,
    129.8,
    -0.350,
    299.36,
    43.46,
    0.0, // ACK This really is 0.70*sin(N)
    0.0, // and this really is -0.51*cos(N),
     // where N = 357.85 + 52.316*T
     // So calculated polar angle can be off by
     // as much as a degree.
    253.18,
    536.3128492  // this also needs an extra -0.48 sin N term
};

/** Basic data for Pluto */
const Astro::planetType Astro::PLUTO = {
    "pluto",
    1.1370E6,
    1.27E22,
    1.0,
    39.5, 
    68, // brrrr!
    0.0,
    313.02,
    9.09,
    0.0, // no (known!) secular variation in polar RA
    0.0, // no (known!) secular variation in polar DEC
    236.77,
    -56.3623195
};
