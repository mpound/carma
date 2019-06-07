#include "carma/util/Program.h"

#include "carma/services/AstroTime.h"
#include "carma/services/Ephemeris.h"
#include "carma/services/EphemerisException.h"
#include "carma/services/Observatory.h"
#include "carma/services/Physical.h"
#include "carma/services/SourceNotFoundException.h"
#include "carma/services/Vector.h"
#include "carma/util/FileNotFoundException.h"

//
// @version	$Revision: 1.42 $ $Date: 2012/02/28 21:41:20 $
//
// @usage  test program for Ephemeris
//
// @description
//	Test program for Ephemeris calculations. It also has 2 benchmarks,
//      one for accuracy, one for speed.
//      speed:
//      .    time carma/services/Test/tEphemeris nsteps=10000 silent=t
//      accuracy:
//      .    carma/services/Test/tEphemeris mjd=53278.75 ra=187.2089167 dec=-3.115194
//      The accuracy benchmark is fairly useless, since the IERS table is updated 'weekly'
//
// @logger DEFAULT_FACILITY carma.services.Test.tEphemeris
//

// @key   ra            180 d  RA2000 of source (degrees)
// @key   dec            30 d  DEC2000 of source (degrees)
// @key   doppler         0 d  Doppler velocity of source (km/s)
// @key   distance        0 d  Distance of source (AU)
// @key   mjd             0 d  Modified julian day for which to calculate times (0=now, prepend + or - to add to now)
// @key   observatory carma s  Name of the observatory
// @key   freq          100 d  Observing frequency for refraction (0 to skip) in GHz. (optical: 3000 GHz = 100 micron)
// @key   nsteps          1 i  Number of timesteps to sample ephemeris
// @key   deltatime     0.1 d  Incremental step in time (fractional days) 
// @key   deltapos      0.0 d  Incremental step in ra, dec (degrees) 
// @key   silent      false b  Run a silent benchmark?
// @key   source         "" s  Source name; if used, it  will override ra/dec/doppler
// @key   catalog        "" s  Catalog name; if user, it will check before the system catalog
// @key   deltat @noDefault d  Cheat dUT1=TT-UT1  (should be about 65 sec these days in 2004)
// @key   ntrial          0 i  Number of trials to Create/Destruct an Ephemeris before other work
// @key   topo            t b  Topocentric ephemeris (vs. Geocentric)
// @key   badweather      f b  input bad weather data to test setWeather
// @key   pressure        0 d  alternative pressure from default (mb)
// @key   temperature     0 d  alternative temperature from default (C)
// @key   humidity        0 d  alternative relative humidity from default (%)

//
// first: w/ solsys2)        2.6   (only novas, no TT, no atmosphere)
// w/solsys4                16.5

using namespace std;
using namespace carma::services;
using namespace carma::util;

int carma::util::Program::main()
{
    double deg2rad = M_PI/180.0;
    double rad2deg = 180.0/M_PI;
    Units units;
    double ra = getDoubleParameter("ra") * deg2rad;
    double dec = getDoubleParameter("dec") * deg2rad;
    double doppler = getDoubleParameter("doppler");
    double distance = getDoubleParameter("distance");
    double mjd = getDoubleParameter("mjd");
    int nsteps = getIntParameter("nsteps");
    double deltatime = getDoubleParameter("deltatime");
    double deltapos = getDoubleParameter("deltapos") * deg2rad;
    double freq = getDoubleParameter("freq") * 1e9;
    Vector<Angle> azel;
    bool silent = getBoolParameter("silent");
    string source = getStringParameter("source");
    string catalog = getStringParameter("catalog");
    string mjd_string = getParameterRawValueString("mjd");  // i really object to this name
    bool topo = getBoolParameter("topo");
    bool use_source = parameterWasSpecified("source");
    int ntrial = getIntParameter("ntrial");
    bool badWeather = getBoolParameter("badweather");
    string obs = getStringParameter("observatory");
    bool setWeather;
    double pressure = 0.0, temperature = 0.0 , humidity = 0.0;
    bool use2 = false; // really only used for debugging 

    if (ntrial > 0) {
      cout << "Creating " << ntrial << " Ephemeris' and destructing them" << endl;
      while (ntrial--) {
        Ephemeris e;
      }
    }
    
    Ephemeris e1, e2;            // a default ephemeris
    e1.setLocation(obs,topo);    // e1 is the one we work with
    if (use2) e2.setLocation(obs,topo);    // e2 is another one

    // test a bug in setting up for location_
    Observatory observatory(obs);
    Location loc = observatory.getReference();
    e1.setLocation(loc);

    setWeather = (parameterWasSpecified("pressure") && 
		  parameterWasSpecified("temperature") &&
		  parameterWasSpecified("humidity"));
    if (setWeather) {
      pressure    = getDoubleParameter("pressure");
      temperature = getDoubleParameter("temperature") - constants::Physical::ABS_ZERO;
      humidity    = getDoubleParameter("humidity");
    }


    try {

      if (mjd == 0) {
	mjd = carma::util::Time::MJD();           // the MJD right now 
      } else {
	if (mjd_string[0] == '+' || mjd_string[0] == '-')
	  mjd += carma::util::Time::MJD();        // add (or subtract) to current MJD
      }
      if (mjd > 2400000) mjd -= 2400000.5;        // cheat to accept JD where MJD is meant

      if (use_source) {
	e1.setSource(source,catalog);
	if (use2) e2.setSource(source,catalog);
      } else {
        Source testSource("EPHTEST",
			Angle(ra,"radian"),
			Angle(dec,"radian"),
			Velocity(doppler,"km/s"),
			Distance(distance,"AU")
	      );
	e1.setSource( testSource );
	if (use2) e2.setSource( testSource );
      }

      // especially for EphemerisTables (via source) setMJD must be called after setSource
      e1.setMJD(mjd);
      if (use2) e2.setMJD(mjd);

      if (parameterWasSpecified("deltat")) {
	e1.setDeltaT(getDoubleParameter("deltat"));
	if (use2) e2.setDeltaT(getDoubleParameter("deltat"));
      }
      e1.setFreq(freq);
      if (use2) e2.setFreq(freq);
      if (badWeather) {
	  e1.setWeather(-1.,0,100000);
	  if (use2) e2.setWeather(-1.,0,100000);
      }
      if (setWeather) {
	e1.setWeather(pressure, temperature, humidity);
	if (use2) e2.setWeather(pressure, temperature, humidity);
      }

      for (int i=0; i<nsteps; i++) {
	if (i>0) {
	  mjd += deltatime;
	  e1.setMJD(mjd);
	  if (use_source) {
	    e1.setSource(source,catalog);
	  } else {
	    dec+=deltapos;
	    ra+=deltapos;
	    Source testSource("EPHTEST",
			    Angle(ra,"radian"),
			    Angle(dec,"radian"),
			    Velocity(doppler,"km/s"),
			    Distance(distance,"AU")
		  );
	    e1.setSource( testSource );
	  }
	}
	if (!silent) {
	  // warning: for a true benchmark, this kind of I/O will add
	  // significantly, so comment it out and do something else
	  cout << mjd << " " << e1.getRa()*rad2deg << " " << e1.getDec()*rad2deg  << " "
	       << e1.getAz()*rad2deg << " " << e1.getEl()*rad2deg
	       << endl;
	}

      }
      if (!silent) {
	e1.Debug();
	if(1) {    // this one reports the correct numbers and units
	  Vector<double> azel = e1.getAzEl(mjd,e1.getRa(),e1.getDec());
	  cout << "getAzEl-1: " << azel[0]*rad2deg << " degrees " << 
	                           azel[1]*rad2deg << " degrees " << endl;
	}	
	if(use2) {    // this one is from the 2nd instance e2 and should be the same as e1 */
	  Vector<double> azel = e2.getAzEl(mjd,e2.getRa(),e2.getDec());
	  cout << "getAzEl-2: " << azel[0]*rad2deg << " degrees " << 
	                           azel[1]*rad2deg << " degrees " << endl;
	}	
	if (1) {    // this one reports the correct numbers and right units now
	  Angle currentRa(e1.getRa()*rad2deg, "degrees");
	  Angle currentDec(e1.getDec()*rad2deg, "degrees");
	  Vector<Angle> azel = e1.getAzEl(mjd,currentRa, currentDec);
	  cout << "getAzEl-3: " << azel[0] << " " << azel[1] << endl;
	}
	if (1) {    // this one reports the correct numbers and right units now
	  Angle currentRa(e1.getRa(), "radians");
	  Angle currentDec(e1.getDec(), "radians");
	  Vector<Angle> azel = e1.getAzEl(mjd,currentRa, currentDec);
	  cout << "getAzEl-4: " << azel[0] << " " << azel[1] << endl;
	}
	if (1) {
	  Vector<double> azel = e1.getAzEl(mjd,e1.getRa(),e1.getDec());
	  Vector<double> radec = e1.getRaDec(mjd,azel[0],azel[1]);
	  double rad2deg = 180.0/M_PI;
	  cout << "getRaDec-1: " << radec[0]*rad2deg << " " << radec[1]*rad2deg << endl;
	}
	cout << "BENCHMARKS: time carma/services/Test/tEphemeris nsteps=10000 silent=t" << endl;
	cout << "  carma/services/Test/tEphemeris mjd=53278.75 ra=187.2089167 dec=-3.115194" << endl;
	cout << "  Which is the sun on 30-sep-2004 18:00 UTC:  ra=12:28:50.14 dec=-3:06:54.7" << endl;
	cout << "  Xephem reports:  az=143:47:31 el=43:07:46" << endl;
      }

      // This will cause a copy construction compile time error
      // if we have done something to make Ephemeris noncopyable.
      // e.g. like when I put an auto_ptr into AstroTime. - MWP
      cout << " COPY CONSTRUCTOR " << endl;
      Ephemeris e3 = e1;

    } catch (const SourceNotFoundException& snfe) {
      cerr << "Program::main : " << snfe.getMessage() << endl;
      return EXIT_FAILURE;
    } catch (const FileNotFoundException& fnfe) {
      cerr << "Program::main : " << fnfe.getMessage() << endl;
      return EXIT_FAILURE;
    } catch (const NotFoundException& nfe) {
      cerr << "Program::main : " << nfe.getMessage() << endl;
    } catch (const EphemerisException& ee) {
      cerr << "Program::main : novas error " << ee.getMessage() << endl;
    } catch (const ErrorException& ee) {
      cerr << "Program::main : exception " << ee.getMessage() << endl;
    } catch (...) {
      cerr << "Program::main : an uncaught error" << endl;
      return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
