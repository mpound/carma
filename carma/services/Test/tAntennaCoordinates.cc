#include "carma/util/Program.h"

#include "carma/services/AntennaCoordinates.h"
#include "carma/services/AstroTime.h"
#include "carma/services/Delay.h"
#include "carma/services/HourAngle.h"
#include "carma/services/Vector.h"

//
// @version	$Revision: 1.7 $ $Date: 2008/10/22 12:30:41 $
//
// @usage  test program for AntennaCoordinates
//
// @description
//	Test program for AntennaCoordinates
//
// @key       lon       -118.14222222   d    Longitude (degrees)
// @key       lat         37.27861111   d    Latitude (degrees)
// @key       alt       2200.0          d    Altitude (meters)
// @key       az        0.0             d    azimuth (degrees);
// @key       ha        0.0             d    hour angle (hours);
// @key       x         0.0             d    equatorial XA 
// @key       y         0.0             d    equatorial YA
// @key       z         0.0             d    equatorial ZA 
//
// @logger DEFAULT_FACILITY carma.services.Test.tAntennaCoordinates
//
//

using namespace std;
using namespace carma::services;
using namespace carma::util;

int carma::util::Program::main()
{

    const double lon = getDoubleParameter("lon");
    const double lat = getDoubleParameter("lat");
    const double alt = getDoubleParameter("alt");
    const double az  = getDoubleParameter("az");
    const double ha  = getDoubleParameter("ha");
    const double x = getDoubleParameter("x");
    const double y = getDoubleParameter("y");
    const double z = getDoubleParameter("z");
    AntennaCoordinates a;
    Angle Alat(lat,"degrees");
    Angle Alon(lon,"degrees");
    Length Aalt(alt,"meters");
    Vector<double> duen;

    if (x!=0 || y!= 0 || z!= 0) {
      Vector<double> xyz = a.topoXYZToUen(x,y,z,Alat);
      cout << "xyz: " << xyz[0] << " " << xyz[1] << " " << xyz[2] << endl;
      return 0;
    }

    try {
      const double rad2deg = 180.0/M_PI;
      a.setLla(Alon,Alat,Aalt);
      duen = a.getLla(0.0,0.0,0.0);
      cout << duen << endl;
      cout << "deg: " << duen[0]*rad2deg << " " << duen[1]*rad2deg << endl;
      cout << "cmd: " << lon << " " << lat << endl;
      const HourAngle hourAngle (ha ,"hours");
      ostringstream os;
      os << setiosflags(ios::fixed) << setprecision(4);
      for (double el = 0.0; el < 91.0; el += 15.0) {

	  double dec = el - lat;

	  Delay delayHA = a.getGeometricDelay(
			  hourAngle,
			  Angle(dec,"degrees"),
			  -10.0,30.0,10.0,0.0,0.0,0.0,true);

	  Delay delay0 = a.getGeometricDelay(
			  Angle(lat,"degrees"),
			  Length(alt,"m"),
			  Angle(az,"degrees"),
			  Angle(el,"degrees"),
			  0.0,0.0,0.0);

	  Delay delay1 = a.getGeometricDelay(
			  Angle(lat,"degrees"),
			  Length(alt,"m"),
			  Angle(az,"degrees"),
			  Angle(el,"degrees"),
			  -10.0,30.0,10.0);

	  os << endl;
	  os << "Ref delay [AZ= " << az << ", EL= " << el <<"] = " 
	     << delay0.nanoSeconds();
	  os << endl;
	  os << "Station delay [AZ= " << az << ", EL= " << el <<"] = " 
	     << delay1.nanoSeconds() << " ns , "
	     << delay1.meters() << " m ";
	  os << endl;
	  os << "DIFF = " << delay1.nanoSeconds() - delay0.nanoSeconds();
	  os << endl;
	  os << "HOURANGLE delay [HA= " << hourAngle.hours() 
	     << ", DEC = " << dec <<"] = " 
	     << delayHA.nanoSeconds() << " ns , "
	     << delayHA.meters() << " m ";
	  os << endl;
      }
      cout << os.str() << endl;
      
    } catch (...) {
      cerr << "Program::main : some error" << endl;
      return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
