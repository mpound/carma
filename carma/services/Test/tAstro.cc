#include "carma/util/BaseException.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/services/Astro.h"
#include "carma/services/Ephemeris.h"
#include "carma/services/EphemerisException.h"
#include "carma/services/Observatory.h"
#include "carma/services/SourceNotFoundException.h"
#include "carma/services/stringConstants.h"

//
// @version	$Revision: 1.5 $ $Date: 2006/11/09 16:43:40 $
//
// @usage	test astronomical constants data.
//
// @description
//	Test program that prints out astronomical constants and
//	looks up planetary data
//
// @key   source sun s  Source name to test planet lookup
// @logger DEFAULT_FACILITY carma.services.Test.tAstro
//

using namespace std;
using namespace carma::util;
using namespace carma::services;
using namespace carma::services::constants;

int Program::main()
{

    try {
	string sourceName = getStringParameter("source");
	cout << "EARTH: " 
	     << " radius: "   << Astro::EARTH.radius 
	     << " mass: "     << Astro::EARTH.mass
	     << " distance: " << Astro::EARTH.avgDist
	     << endl;
	cout << "earth is planet? " << boolalpha << Astro::isPlanet("eArTH")
	    << endl;
	cout << "foo is planet? " << boolalpha << Astro::isPlanet("foo")
	    << endl;
	Astro::planetType jupiter = Astro::getPlanet("jupiter");
	cout << "JUPITER: "
	     << " radius: "   << jupiter.radius 
	     << " mass: "     << jupiter.mass
	     << " distance from sun (AU): " << jupiter.avgDist
	     << " aspect ratio " << jupiter.aspectRatio
	     << " brightness temperature: " << jupiter.brightnessTemp
	     << " tempIndex " << jupiter.tempIndex
	     << endl;
	if ( Astro::isPlanet(sourceName) ) {
	    // Get current distance from ephmeris as opposed to average distance
	    // stored in Astro::planetType
	    Observatory obs("carma");
	    Ephemeris ephem( obs.getReference() );
	    ephem.setMJD( Time::MJD() );
	    ephem.setSource(sourceName);
	    double distance  = ephem.getSource().getDistance().meters();
	    Astro::planetType thePlanet = Astro::getPlanet(sourceName);
	    // use small angle theorem: tan(theta) ~ theta.
	    Angle ma(2.0*thePlanet.radius/distance,"radians");
	    double majorAxis = ma.arcSeconds();
	    double minorAxis = majorAxis * thePlanet.aspectRatio;
	    // @TODO This is Tb @ 100 GHz, future Planet class will
	    // have frequency dependence. (See also thePlanet.tempIndex).
	    double temperature = thePlanet.brightnessTemp; 
	    cout << sourceName 
		 << " distance = " << distance << "(m) "
		 << " major axis = " << majorAxis << "(arcsec) "
		 << " minor axis = " << minorAxis << "(arcsec) "
		 << " Tb = " << temperature << "(K) "
		 << endl;
	} else {
	    cout << sourceName << " is not a planet." << endl;
	}

	cout << "carma: " << CARMA_OBSERVATORY << endl;
	cout << "reference " << REFERENCE << endl;
	cout << "no source " << NO_SOURCE << endl;
      return EXIT_SUCCESS;
    } catch (const SourceNotFoundException& snfe) {
      cerr << "Program::main : " << snfe.getMessage() << endl;
      return EXIT_FAILURE;
    } catch (const NotFoundException& nfe) {
      cerr << "Program::main : " << nfe.getMessage() << endl;
      return EXIT_FAILURE;
    } catch (const EphemerisException& ee) {
      cerr << "Program::main : novas error - " << ee.getMessage() << endl;
    } catch (const BaseException& be) {
      cerr << "Program::main : CARMA error - " << be.getMessage() << endl;
    } catch ( std::exception& stdex ) {
      cerr << "Program::main : std error - " << stdex.what() << endl;
    } catch ( ... ) {
      cerr << "Program::main : an uncaught error" << endl;
      return EXIT_FAILURE;
    }

   return EXIT_SUCCESS;
}
