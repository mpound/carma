#include "carma/util/Program.h"

#include "carma/services/Angle.h"
#include "carma/services/AstroTime.h"
#include "carma/services/Ephemeris.h"
#include "carma/services/EphemerisException.h"
#include "carma/services/SourceNotFoundException.h"
#include "carma/services/Vector.h"
#include "carma/util/FileNotFoundException.h"

//
// @version	$Revision: 1.10 $ $Date: 2015/02/24 01:56:44 $
//
// @usage  angular distance of two sources, e.g. phases of moon or planet
//
// @description
//	The ephemeris is used to compute the phase of a planet or moon.
//      Returns 0 if aligned (conjunction or new moon) and 180 if opposite
//      (opposition or full moon). It cannot distinguish between just before
//      and just after opposition, since the angular distance is always
//      returned between 0 and 180. 
//      Can of course be used to compute any angular distance between two
//      sources
//
// @key   source @mandatory s  Planet source name
// @key   ref           sun s  Name of reference object
// @key   mjd             0 d  Modified julian day for which to calculate times (0=now, prepend + or - to add to now)
// @key   step            0 d  Timestep, in days.
// @key   nsteps          1 i  Number of timestamp samples to simulate the ephemeris
// @key   catalog        ""  s  Search extra user catalog before system catalog
// @key   observatory  carma s  Name of the observatory (see also CARMA/conf/catalogs/Observatory.cat)
// @key   freq          100 d  Observing frequency for refraction (0 to skip) in GHz. (optical: 3000 GHz = 100 micron)
//
// @logger DEFAULT_FACILITY carma.services.distance
//
// note added (23 feb 2015) as observed by Dick Plambeck:
// When elevation for one source is positive and the elevation for the other is negative
// distances as function of time may display glitches. Common with planet-moon system.

using namespace std;
using namespace carma::services;
using namespace carma::util;

int carma::util::Program::main()
{
    //double deg2rad = M_PI/180.0;
    double rad2deg = 180.0/M_PI;
    double mjd = getDoubleParameter("mjd");
    double freq = getDoubleParameter("freq") * 1e9;
    double mjdt = getDoubleParameter("step");
    int nsteps =getIntParameter("nsteps");
    double az0, el0, az1, el1, cosd, d;
    string source = getStringParameter("source");
    string catalog = getStringParameter("catalog");
    string ref = getStringParameter("ref");
    string mjd_string = getParameterRawValueString("mjd");     // i really object to this name
    Time t;                   // helper for printing time

    try {
      Ephemeris e1;
      e1.setLocation(getStringParameter("observatory"));
      if (mjd == 0)
        mjd = carma::util::Time::MJD();           // the MJD right now 
      else
        if (mjd_string[0] == '+' || mjd_string[0] == '-')
          mjd += carma::util::Time::MJD();        // add (or subtract) to current MJD
      e1.setFreq(freq);
      cout << setprecision(10);

      cout << "# Time                  "
           << ref << " : "
           << " az/el           " 
           << source << " : "
           << " az/el         " 
           << " angular distance "
           << endl;


      while (nsteps--) {
        e1.setMJD(mjd);
        e1.setSource(ref,catalog);
        az0 = e1.getAz();
        el0 = e1.getEl();

        e1.setSource(source,catalog);
        az1 = e1.getAz();
        el1 = e1.getEl();
        cosd = sin(el0)*sin(el1) + cos(el0)*cos(el1)*cos(az0-az1);
        d = acos(cosd);
        cout << t.getFITSdateTimeString(mjd,1) << " " 
             <<  az0*rad2deg << " "
             <<  el0*rad2deg << " "
             <<  az1*rad2deg << " "
             <<  el1*rad2deg << " "
             <<  d*rad2deg   << " "
             << endl;
          
        mjd += mjdt;
      }

    } catch (const FileNotFoundException& fnfe) {
      cerr << "Program::main error: " << fnfe.getMessage() << endl;
    } catch (const SourceNotFoundException& snfe) {
      cerr << "Program::main error: " << snfe.getMessage() << endl;
      return EXIT_FAILURE;
    } catch (const NotFoundException& nfe) {
      cerr << "Program::main error: " << nfe.getMessage() << endl;
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
