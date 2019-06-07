/**
 * CHECKEPHEM - check an ephemeris (VECTORS) file
 *
 * @author Peter Teuben
 * $Id: checkephem.cc,v 1.2 2010/02/18 22:14:08 abeard Exp $
 * $CarmaCopyright$
 */

#include "carma/util/Program.h"
#include "carma/util/StringUtils.h"
#include "carma/services/Angle.h"
#include "carma/services/AntennaCoordinates.h"
#include "carma/services/Astro.h"
#include "carma/services/AstroTime.h"
#include "carma/services/Ephemeris.h"
#include "carma/services/EphemerisException.h"
#include "carma/services/FluxCatalog.h"
#include "carma/services/FluxDensity.h"
#include "carma/services/FluxSource.h"
#include "carma/services/Frequency.h"
#include "carma/services/HourAngle.h"
#include "carma/services/Location.h"
#include "carma/services/Observatory.h"
#include "carma/services/Pad.h"
#include "carma/services/Planet.h"
#include "carma/services/SourceNotFoundException.h"
#include "carma/services/SourceChecker.h"
#include "carma/services/UnsupportedCoordSysException.h"
#include "carma/services/Vector.h"
#include "carma/util/FileNotFoundException.h"
#include "carma/util/IllegalArgumentException.h"

#include <vector>
#include <sstream>
//
// @version	$Revision: 1.2 $ $Date: 2010/02/18 22:14:08 $
//
// @usage  create ephemeris VECTORS file for rotating spots on planets
//
// @description
//	Derived from checksource, this program allows you to track the motion
//      of a spot on a planet (currently assumed to be a spherical body), and
//      it will generate a VECTORS ephemeris file for this spot.
//
//      In pratice this is the method how to generate sunspot tracking files.
//
//      If \"nsteps\" is greater than 1, then a table of source positions as a function of
//      time is listed, at time steps indicated by \"step\".
//
// @key   source @mandatory s  Source name
// @key   mjd             0 d  Modified julian day for which to calculate times 
//                             (0=now, prepend + or - to add to now)
// @key   step            0 d  Timestep, in days.
// @key   nsteps          0 i  Number of timesteps.
// @key   observatory carma s  Name of the observatory, case insensitive
//                             (see also CARMA/conf/catalogs/Observatory.cat)
// @key   spot            t b  Use planet and lat/long/spinrate to follow spot
// @key   longitude       0 d  Longitude (in arcmin) of spot on planet surface (E = positive)
// @key   latitude        0 d  Latitude (in arcmin) of spot on planet surface (N = positive)
// @key   spinrate        0 d  Spinrate (in 1/day) of spot at that latitude
//                             Sun example: 13.45 - 3.0*sin(latitude)^2 deg/day
//
// @key   sexa            t b  Output in sexagesimal notation (dd:mm:ss.ss)
// @key   verbose         f b  Verbose debug output from the Ephemeris with more info?
// @logger DEFAULT_FACILITY carma.services.checksource

using namespace std;
using namespace carma::services;
using namespace carma::services::constants;
using namespace carma::util;


double SunPeriod(double latitude=0.0)
{
  double sinl = sin(latitude/57.29577951);
  double rate = 13.45 - 3.0 * sinl * sinl;    // second order
  return 360.0/rate;
}


int carma::util::Program::main()
{
  try {
      const double rad2deg = 180.0/M_PI;
      double mjd        = getDoubleParameter("mjd");
      double mjdt       = getDoubleParameter("step");
      int nsteps        = getIntParameter("nsteps");
      string source     = getStringParameter("source");

      string mjd_string = getParameterRawValueString("mjd");
      string obs        = getStringParameter("observatory");
      bool debug        = getBoolParameter("verbose");
      bool sexa         = getBoolParameter("sexa");
      bool usePlanet    = getBoolParameter("spot");
      double latitude   = getDoubleParameter("latitude"); 
      double longitude  = getDoubleParameter("longitude");  // E = positive
      double spinrate   = getDoubleParameter("spinrate");
      double lst;

      if (mjd == 0)
        // the MJD right now 
        mjd = carma::util::Time::MJD();           
      else {
        // if the input MJD started with + or -, then we take the
        // value, which is stored in the variable mjd, and
        // add that to or subtract that from the current MJD.
        if (mjd_string[0] == '+' || mjd_string[0] == '-')
          mjd += carma::util::Time::MJD();        
      }


      Observatory observatory(obs);
      Location loc = observatory.getReference();

      AstroTime at(loc);
      Ephemeris e1;
      e1.setLocation(loc);
      e1.setMJD(mjd);
      e1.setSource(source);

      if (usePlanet) {
	try {
	  Planet planet(source);
	  planet.setMJD(mjd);
	  planet.setLocation(loc);
	  printf("  Major axis = %.2f arcseconds\n", planet.majorAxis().arcSeconds() );
	  printf("  Minor axis = %.2f arcseconds\n", planet.minorAxis().arcSeconds() );
	  printf("  Polar angle = %.2f degrees\n",   planet.axisAngle().degrees() );
	  printf("  Tilt  angle = %.2f degrees\n",   planet.tiltAngle().degrees() );
	  printf("  Current distance from Earth = %.4f AU\n",  planet.earthDistance().convert("au") );
	  printf("  Average distance from Sun = %.4f AU\n",    planet.avgSunDistance().convert("au") );
	  if (source == "sun") 
	    printf("  Rotation speed at latitude %g = %g days\n", latitude,SunPeriod(latitude));

	  e1.SetSpinningBodySpot(
				 planet.majorAxis().arcMinutes(),
				 planet.minorAxis().arcMinutes(),
				 planet.axisAngle().degrees(),
				 planet.tiltAngle().degrees(),
				 mjd,
				 longitude,
				 latitude,
				 spinrate);
	} catch (...) {
	  e1.SetSpinningBodySpot(0.0,0.0,0.0,0.0,mjd,0.0,0.0,0.0);
	}
      }	else {
	  e1.SetSpinningBodySpot(0.0,0.0,0.0,0.0,mjd,0.0,0.0,0.0);
      }

      if (nsteps <= 0) {
          if (debug) e1.Debug();
          return EXIT_SUCCESS;
      }
        
      cout << endl;
      cout << endl;
      if ( sexa )
	  cout << "      Time              "
	       << "App. Ra      "
	       << "App. Dec      "
	       << "Azimuth    "
	       << "Elevation        "
	       << "LST         "
	       << "Doppler       "
	       ;
      else 
	  cout << "  Time        "
	       << "App. Ra (deg)    "
	       << "App. Dec (deg)   "
	       << "Azimuth (deg)  "
	       << "Elevation (deg)      "
	       << "LST (hr)       "
	       << "Doppler    "
	       ;

      cout << endl;

      while (nsteps > 0) {
        nsteps--;
        e1.setMJD(mjd);
        lst = at.localSiderealTime(mjd);      // will be in hours
	double velo = Velocity(e1.getDoppler(),"m/s").kms();
        if (sexa) {
          //printf("%21s %11s %12s %12s %12s %.7f %12s %8s",
          printf("%21s %11s %12s %-13s %12s %12s %10.2f",
                 carma::util::Time::getFITSdateTimeString(mjd,1).c_str(),
		 HourAngle::getAngleString(e1.getRa(),2).c_str(),
		 Angle::getString(e1.getDec(),2).c_str(),
                 Angle::getString(e1.getAz(),2).c_str(),
                 Angle::getString(e1.getEl(),2).c_str(),
		 //mjd,
                 HourAngle::getAngleString(lst * M_PI / 12.0, 2).c_str(),
		 velo
		 );
        } else {
	    printf("%10.4f %16.12f %16.12f %16.12f %16.12f %16.12f %10.4f",
	    mjd, 
	    e1.getRa()*rad2deg, 
	    e1.getDec()*rad2deg,
	    e1.getAz()*rad2deg,
	    e1.getEl()*rad2deg,
	    lst, 
	    velo
	    );
        }
        printf("\n");
	e1.ShowVector();
        if (debug) e1.Debug();
        mjd += mjdt;
      }
      
  } catch (const UnsupportedCoordSysException & ucse) {
	cerr << "### UnsupportedCoordSys exception: "
	     << ucse.getMessage() << endl;
	return EXIT_FAILURE;
  } catch (const SourceNotFoundException& snfe) {
	cerr << "### SourceNotFound exception: "
	     << snfe.getMessage() << endl;
	return EXIT_FAILURE;
  } catch (const FileNotFoundException& fnfe) {
	cerr << "### FileNotFound exception: "
	     << fnfe.getMessage() << endl;
	return EXIT_FAILURE;
  } catch (const NotFoundException& nfe) {
	cerr << "### NotFound exception: "
	     << nfe.getMessage() << endl;
	return EXIT_FAILURE;
  } catch (const EphemerisException& ee) {
	cerr << "### Ephemeris exception: "
	     << ee.getMessage() << endl;
	return EXIT_FAILURE;
  } catch (const IllegalArgumentException & ille) {
	cerr << "### IllegalArgument exception: "
	     << ille.getMessage() << endl;
	return EXIT_FAILURE;
  } catch (const BaseException & be) {
	cerr << "### Program exception: "
	     << be.getMessage() << endl;
	return EXIT_FAILURE;
  } catch (...) {
	cerr << "### Program exception: an unspecified error." << endl;
	return EXIT_FAILURE;
  }
  
  return EXIT_SUCCESS;
}
