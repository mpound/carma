#include "carma/util/Program.h"
#include "carma/util/StringUtils.h"

#include "carma/services/AntennaCoordinates.h"
#include "carma/services/Astro.h"
#include "carma/services/AstroTime.h"
#include "carma/services/Angle.h"
#include "carma/services/DecAngle.h"
#include "carma/services/Ephemeris.h"
#include "carma/services/EphemerisException.h"
#include "carma/services/HourAngle.h"
#include "carma/services/Pad.h"
#include "carma/services/SourceNotFoundException.h"
#include "carma/services/Vector.h"

//
// @version	$Revision: 1.3 $ $Date: 2010/02/18 22:14:09 $
// @author Marc Pound
//
// @usage  Find the limbs of Sun or Moon for fringe test
//
// @description
// This program calculates the RA and DEC offsets for the
// solar or lunar limb which is perpendicular to a given
// baseline.  The default antenna pair locations, Pads #33 and #45
// form a more-or-less North-South baseline.
//
// @key   source      "sun" s  Source name 
// @key   refpad         33 i  The pad number containing of the reference antenna
// @key   pad            45 i  The pad number of the other antenna
// @key   mjd             0 d  Modified julian day for which to calculate times (0=now, prepend + or - in days to add to now)
//
// @key   freq          100 d  Observing frequency for refraction (0 to skip) in GHz. (optical: 3000 GHz = 100 micron)
// @logger DEFAULT_FACILITY carma.services.findLimb
//
// @see Marc's CARMA IV notebook.

using namespace std;
using namespace carma::services;
using namespace carma::services::constants;
using namespace carma::util;

int carma::util::Program::main()
{
    double mjd = getDoubleParameter("mjd");
    string source = getStringParameter("source");
    string mjd_string = getParameterRawValueString("mjd");
    int refpadNo = getIntParameter("refpad");
    int padNo = getIntParameter("pad");
    double freq = getDoubleParameter("freq") * 1.0E9;

    string srcname = StringUtils::lowASCIIAlphaNumericToLower(source);

    double srcSize; // source radius in meters
    if ( srcname == "sun"  ) {
	  srcSize = Astro::SUN.radius;
    } else if (srcname =="moon")  {
	  srcSize = Astro::MOON.radius;
	   } else {
	    cerr << "This program is only useful for the Sun and Moon." <<endl;
	    return EXIT_FAILURE;
	   }

    const std::string CARMA = "carma";
    ostringstream os;
    os << "pad#" <<refpadNo;
    const string refStr = os.str();
    Pad refpad(CARMA,refStr);

    ostringstream pos;
    pos << "pad#" << padNo;
    const string posStr = pos.str();
    Pad pad(CARMA,posStr);

    Ephemeris ephem(refpad.getLocation());
    ephem.setSource(srcname);

    try {

      if (mjd == 0)
	mjd = carma::util::Time::MJD();           // the MJD right now 
      else {
	if (mjd_string[0] == '+' || mjd_string[0] == '-')
      // add (or subtract) to current MJD
	  mjd += carma::util::Time::MJD();        
      }

      ephem.setMJD(mjd);
      ephem.setFreq(freq);
      double ra = ephem.getRa();
      double dec = ephem.getDec();
      double az = ephem.getAz();
      double el = ephem.getEl();
      if ( el <= 0.0 ) {
	  cerr << "The " << srcname 
	       << " is below the horizon at your requested MJD!"
	       << endl;
	  return EXIT_FAILURE;
      }
      Angle raAngle(ra,"radians");
      DecAngle decAngle(dec,"radians");

      AstroTime at( refpad.getLocation() );
      HourAngle HA1 = at.hourAngle( mjd, raAngle );
      at.setSite( pad.getLocation() );
      HourAngle HA2 = at.hourAngle( mjd, raAngle );

      AntennaCoordinates reference( refpad.getLocation() );
      AntennaCoordinates antenna( pad.getLocation() );
      Vector<double> b1 = reference.getUvw( HA1, decAngle );
      Vector<double> b2 =   antenna.getUvw( HA2, decAngle );
      Vector<double> baseline = b1 - b2;
      cout << "baseline " << refpadNo <<"-"<<padNo<< " is " 
	   << baseline << endl;
      Vector<double> vhat(0.0,1.0,0.0);
      double dotProduct = baseline * vhat;

      Source src      = ephem.getSource();
      double distance = src.getDistance().convert("m");
      Angle radius(srcSize/distance,"radians");
      double arcmin = radius.arcMinutes();
      double cosAlpha = dotProduct/baseline.magnitude();
      double ddec = arcmin*cosAlpha;
      double sineAlpha = sqrt( 1.0 - cosAlpha*cosAlpha );
      double dra  = arcmin*sineAlpha;

      double rad2deg = 180.0/M_PI;
      cout << setiosflags(ios::scientific)
	  << setprecision(4)
	  << "distance(m) to " << src.getName() << " is " << distance
	  << endl;
      cout << setiosflags(ios::scientific)
          << "radius (m) of " << src.getName() << " is " << srcSize 
	  << endl;
      cout << "Angular diameter (arcmin) of " 
	   << setprecision(4) << setiosflags(ios::fixed)
	   << srcname << " is " << 2*arcmin  << " arcminutes."
	   << endl;
      cout << setprecision(6) 
	  << "At mjd = " << mjd 
	  << ", LST = " << at.localSiderealTime(mjd)
	  << " the " << srcname << " is at "
	  << setprecision(4) << setiosflags(ios::fixed)
	  << "(AZ,EL) = " << "(" << az*rad2deg
	  << " , " << el*rad2deg
	  << ")"
	  <<endl;
        
      cout << "Limb locations (dra,ddec) in arcmin = (" 
	   << setprecision(4)
	   << setiosflags(ios::fixed)
	   << dra << "," << ddec << ") and ("
	   << -dra << "," << -ddec << ")."
	   <<endl;

      /*
      cout << "dotproduct = " << dotProduct 
	   << " mag = " << baseline.magnitude() 
	   << " sin(Alpha) =" << sineAlpha 
	   << " cos(Alpha) =" << cosAlpha 
	   << endl;
      cout << " HA1 " << HA1 << " " << HA1.getValue() << endl;
      cout << " HA2 " << HA2 << " " << HA2.getValue() << endl;
      */

    } catch (const SourceNotFoundException& snfe) {
      cerr << "Program::main : " << snfe.getMessage() << endl;
      return EXIT_FAILURE;
    } catch (const NotFoundException& nfe) {
      cerr << "Program::main : " << nfe.getMessage() << endl;
      return EXIT_FAILURE;
    } catch (const EphemerisException& ee) {
      cerr << "Program::main : novas error " << ee.getMessage() << endl;
      return EXIT_FAILURE;
    } catch (...) {
      cerr << "Program::main : an uncaught error" << endl;
      return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
