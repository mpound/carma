// $Id: Planet.cc,v 1.6 2013/07/16 13:41:30 mpound Exp $
#include "carma/services/Angle.h"
#include "carma/services/Astro.h"
#include "carma/services/Distance.h"
#include "carma/services/Frequency.h"
#include "carma/services/FluxDensity.h"
#include "carma/services/Length.h"
#include "carma/services/Location.h"
#include "carma/services/mathFunctions.h"
#include "carma/services/Planet.h"
#include "carma/services/Physical.h"
#include "carma/services/SourceNotFoundException.h"
#include "carma/services/Temperature.h"
#include "carma/services/Velocity.h"
#include "carma/util/ErrorException.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/programLogging.h"

#include <iostream>
#include <cmath>

using namespace carma;
using namespace carma::util;
using namespace carma::services;
using namespace carma::services::constants;
using namespace std;

Planet::Planet(const string& planetName) 
    // This will throw source not found exception if planetName is unrecognized.
    : pType_(Astro::getPlanet(planetName)),
      name_(planetName)
{
    ephemeris_.setLocation(Location("carma"));
    ephemeris_.setSource(planetName);
    try {
      planetTemperature_ = auto_ptr<PlanetTemperature>( new PlanetTemperature(pType_) );
    } catch ( const out_of_range & oor ) {
        ostringstream os;
        os << " Couldn't instantiate PlanetTemperature("<<planetName<<"): "
           << " out of range error - " << oor.what()
           << " Falling back on power-law model. ";
        programLogErrorIfPossible( os.str() );
    } catch ( ... ) {
        // Stifle other exceptions. It is expected that in some cases this will
        // not get initialized (if no <planet>tb.tab file exists
        // for this planet).
    }
}

Planet::~Planet()
{
    // nada.
}

const string 
Planet::getName() const
{
    return name_;
}

void 
Planet::setMJD( double mjd ) 
{
   ephemeris_.setMJD( mjd );
} 


void 
Planet::setLocation( const Location& location )
{
    ephemeris_.setLocation( location );
}

const Distance
Planet::earthDistance(void) const
{
    const Distance d = ephemeris_.getSource().getDistance();
    return d;
}

const Distance
Planet::avgSunDistance(void) const
{
    return Distance(pType_.avgDist,"au");
}

/*
Distance
Planet::sunDistance(void) 
{
}
*/

const Angle
Planet::majorAxis() const
{
      double dmeters = earthDistance().meters();
      // use small angle theorem: tan(theta) ~ theta.
      Angle ma(2.0*pType_.radius/dmeters,"radians");
      double majorAxis = ma.arcSeconds();
      return Angle(majorAxis,"arcsec");
}

const Angle
Planet::minorAxis() const
{
      double minorAxis = majorAxis().getValue() * pType_.aspectRatio;
      return Angle(minorAxis,"arcsec");
}

const Angle
Planet::axisAngle() const
{
  return planetAngle(true);
}

const Angle
Planet::tiltAngle() const
{
  return planetAngle(false);
}


const Angle
Planet::planetAngle(bool positionAngle) const 
{
//---------------------------------------------------------------------
// Values returned by axisAngle() have been spot-checked against
// those returned by the JPL horizons ephemeris. 
// http://ssd.jpl.nasa.gov/horizons.cgi
// Return values match.
//---------------------------------------------------------------------
      const double deg2rad = M_PI/180.0;
      // The derivatives of the polar RA and DEC are stored in degrees per Julian century, so
      // get the time difference since J2000.0 in Julian centuries.
      double mjd = ephemeris_.getMJD();
      double julcen = AstroTime::elapsedJulCent(mjd);

      // compute the current position of the pole in J2000 coordinates.
      double currentPoleRa2000  = pType_.poleRa2000  + pType_.dRaDt*julcen;  // degrees
      double currentPoleDec2000 = pType_.poleDec2000 + pType_.dDecDt*julcen; // degrees
      //cout << " current Pole RA " << currentPoleRa2000 << endl;
      //cout << " RA change since J2000 " <<  pType_.dRaDt*julcen << endl;
      //cout << " current Pole DEC " << currentPoleDec2000 << endl;
      //cout << " DEC change since J2000 " <<  pType_.dDecDt*julcen << endl;
      
      // convert to radians
      currentPoleRa2000  *= deg2rad;
      currentPoleDec2000 *= deg2rad; 

      // current apparent coordinates
      double ra0  = ephemeris_.getRa();
      double dec0 = ephemeris_.getDec();

      // Next 3 statements encapsulate the planet angle formula from
      // the Astronomical Almanac, Section E3 "Definitions and Formulas"
      // Note cos(Beta_e) term described therein divides out when we take
      // the arctangent, but is in essence this is the tilt angle.
      // 
      double sinp = cos(currentPoleDec2000)*sin( currentPoleRa2000-ra0 );
      double cosp = sin(currentPoleDec2000)*cos(dec0) -
                    cos(currentPoleDec2000)*sin(dec0)*cos( currentPoleRa2000-ra0 );
      double plangle = positionAngle ?
                   atan2(sinp,cosp) :
                   acos(sinp/sin(atan2(sinp,cosp)));

      // test against wilson's mistake in hat creek code
      // This mistake causes errors of ~ 0.1 degree in polar angle
      //double x = pType_.poleRa2000*deg2rad;
      //double y = pType_.poleDec2000*deg2rad;
      //double x = 318*deg2rad;
      //double y = 53*deg2rad;
      //double xsinp = cos(y)*sin( x-ra0 );
      //double xcosp = sin(y)*cos(dec0) - cos(y)*sin(dec0)*cos( x-ra0 );
      //double wilsonangle = atan2(xsinp,xcosp);
      //cout << " Wilson mars Polar Angle: " << wilsonangle/deg2rad << endl;

      return Angle(plangle,"radians");
}

const Temperature 
Planet::powerLawTb(const Frequency& frequency) const
{
    // The planetType struct contains the brightness temperature at 100 GHz
    // and the power-law index of the variation of temperature with frequency.
    // Calculation of the brightness temperature at input frequency from these
    // is straightforward: T(f) = T(100)*(f/100)^a.
    double freqRatio = frequency.gigahertz()/100.0;
    double temperature = pType_.brightnessTemp * pow(freqRatio,pType_.tempIndex);
    /*
    cout << " FreqRatio["<<frequency.gigahertz()<<"] = " <<freqRatio << endl;
    cout << " power index  = " << pType_.tempIndex << endl;
    cout << " power factor = " << pow(freqRatio,pType_.tempIndex) << endl;
    cout << " temperature = " << temperature << endl;
    */
    return Temperature(temperature,"K");
}

const Temperature 
Planet::brightnessTemperature(const Frequency& frequency) 
{
    const double mjd = ephemeris_.getMJD();
    ostringstream osparams;
    osparams << "(mjd="<<mjd <<","<< frequency.gigahertz()<<" GHz)";
    const string params = osparams.str();

    // See if we have a table interpolation (e.g. Mars)
    if ( planetTemperature_.get() != 0 ) {
        try {
            return planetTemperature_->brightnessTemperature( mjd, frequency );
        } catch ( const ErrorException & erx ) {
            ostringstream os;
            os << "Error interpolating planetary brightness table at"
               << " for " << name_ 
               << " at " << params 
               <<  " ["
               << erx.getMessage() << "]"
               << ". Returning power-law brightness temperature instead."
               ;
            //cout << "# " << os.str() <<endl;
            programLogWarnIfPossible( os.str() );
        } catch ( ... ) {
            ostringstream os;
            os << "Unclassified error interpolating planetary brightness table" 
               << " for " << name_
               << " at " << params 
               << ". Returning power-law brightness temperature instead."
               ;
            //cout << "# " << os.str() <<endl;
            programLogWarnIfPossible( os.str() );
        }
    }

    // Otherwise return straight power-law
    return powerLawTb( frequency );
}

const FluxDensity 
Planet::flux(const Frequency& frequency) 
{
      // area of ellipse = PI/4 * major axis * minor axis
      // want omega in sterradians.
      double omega = M_PI/4 * majorAxis().radians()*minorAxis().radians();
      double temperature = brightnessTemperature(frequency).kelvin();
      double curfreq = frequency.hertz();
      double flux = omega * 2. * Physical::H * (pow(curfreq,3.0)) 
                / (Physical::C*Physical::C) ;
      flux /=  ( exp((Physical::H*curfreq)/(Physical::K*temperature)) - 1. );
      return FluxDensity( flux, "W/(m^2 Hz)" );
}

FluxDensity 
Planet::uvFlux(const Frequency& frequency, const Length& u, const Length& v)
{

//  cout << "in uvFlux ( " 
//       << frequency.gigahertz() << " , "
//       << u.meters() << " , "
//       << v.meters() << " ) "
//       << endl;
    // algorithm taken from MIRIAD's ModPlant function in $MIRSUBS/model.for.
    const FluxDensity totalFlux = flux(frequency);
    double plangle = axisAngle().radians();
    double plmaj   = majorAxis().radians();
    double plmin   = minorAxis().radians();
    double cosi    = cos(plangle);
    double sini    = sin(plangle);
    // need U and V in nanoseconds
    double uns     = u.convert("nanometer") / Physical::C;
    double vns     = v.convert("nanometer") / Physical::C;
    // rotation from planet coords to UV.
    double majuv   = plmaj * (uns*cosi - vns*sini);
    double minuv   = plmin * (uns*sini + vns*cosi);

    // Convolve the planet disk with appropriate visibility function.
    // jinc is the Fourier Transform of a uniform disk.
    // Use the mean disk radius.
    // R=beta*freq is the resolvability value.
    // Visibility of uniform disk is 2*jinc(R).
    double beta    = M_PI * sqrt(majuv*majuv + minuv*minuv);
    double newflux = 2.0 * jinc( beta * frequency.gigahertz() ) * totalFlux.jansky();
//    cout << " totflux = " << totalFlux.jansky()
//   << " uns = " << uns
//   << " vns = " << vns
//   << " beta = " << beta 
//   << " newflux = " << newflux
//   << endl;

    // no negative fluxes allowed.
    return FluxDensity(fabs(newflux), "Jy");

}

std::vector<FluxDensity*>
Planet::uvFluxes(const Frequency& frequency, 
         const vector<Length*>& u,
         const vector<Length*>& v)
{
    unsigned short length = u.size();
    if ( length != v.size() )
    throw CARMA_EXCEPTION(IllegalArgumentException,
        "Planet::uvFluxes: U and V vectors have different lengths");
    if ( length == 0 )
    throw CARMA_EXCEPTION(IllegalArgumentException,
        "Planet::uvFluxes: Vectors have length zero");

    vector<FluxDensity*> fluxes;
    fluxes.reserve(length);

    for (unsigned int i = 0; i < length; i++ ) {
    FluxDensity fd = uvFlux(frequency, *u[i], *v[i]);
    // must create a new FD here or it goes out of scope during return.
    FluxDensity *fp = new FluxDensity(fd.getValue(), fd.getUnits());
    fluxes.push_back(fp);
    }

    return fluxes;

}
