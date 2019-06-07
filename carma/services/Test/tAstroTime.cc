#include "carma/util/Program.h"
#include "carma/services/AstroTime.h"
#include "carma/services/Angle.h"
#include "carma/services/Length.h"
#include "carma/services/Location.h"

// debugging
#include "carma/services/Ephemeris.h"
#include "carma/services/Test/slamac.h"
#include <math.h>

//
// @version	$Revision: 1.8 $ $Date: 2006/04/29 16:43:34 $
//
// @usage  tAstro longitude=<longitude in degrees> mjd=<modified julian day>
//
// @description
//  to test this code out, set the mjd keyword to equal values in
//  carma/service/Test/timeData ... the location here has been
//  defaulted to hat creek.  as cross checks, we've added the GMT that
//  slalib returns
//
// @key longitude -121.4718027778 d  longitude in degrees at which to calculate times
// @key  latitude   40.8173611111 d  longitude in degrees at which to calculate times
// @key  altitude   1043.0 d  longitude in degrees at which to calculate times
// @key       mjd      0.0 d  Modified julian day for which to calculate times
// @key       dup        f b  Use an additional (location-less) instantiation for testing
//
// @logger DEFAULT_FACILITY carma.services.Test.tAstroTime

using namespace carma::services;

// slalib method needed by slaGmst
double slaDranrm ( double angle )
{
   double w;

   w = dmod ( angle, D2PI );
   return ( w >= 0.0 ) ? w : w + D2PI;
}

// slalib method for calculating GST
double slaGmst ( double ut1 )
{
   double tu;

/* Julian centuries from fundamental epoch J2000 to this UT */
   tu = ( ut1 - 51544.5 ) / 36525.0;

/* GMST at this UT */
   return slaDranrm ( dmod ( ut1, 1.0 ) * D2PI +
                       ( 24110.54841 +
                       ( 8640184.812866 +
                       ( 0.093104 - 6.2e-6 * tu ) * tu ) * tu ) * DS2R );
}

int carma::util::Program::main()
{

    Units units;
    double mjd = getDoubleParameter("mjd");
    bool dup = getBoolParameter("dup");
    if (mjd==0)
     mjd = Time::MJD(); 

    // define location from position coordinates
    Location myLocation;
    myLocation.setLatitude(getDoubleParameter("latitude"), "degrees");
    myLocation.setLongitude(getDoubleParameter("longitude"), "degrees");
    myLocation.setAltitude(getDoubleParameter("altitude"), "meters");

    // create a new AstroTime object for the given location
    AstroTime astroTime(myLocation);
    // and another with no location
    AstroTime astroTime2;
    
    // compare following line with timeData file
    // - UT - fractional part of MJD
    std::cout << "  UT              MJD             LST" << std::endl;
    std::cout << "(hours)         (days)          (hours)" << std::endl;
    std::cout << std::setprecision(10)
	      << 24.0*(mjd-floor(mjd)) << "\t" 
	      << mjd                   << "\t" 
	      << astroTime.localSiderealTime(mjd) << std::endl;
    if (dup) std::cout << std::setprecision(10)
		       << 24.0*(mjd-floor(mjd)) << "\t" 
		       << mjd                   << "\t" 
		       << astroTime2.localSiderealTime(mjd) << std::endl;


    /* other information */
    std::cout << "\n\nOther Debug Information:" << std::endl;
    // MST
    std::cout << "-- MST: " << astroTime.meanSiderealTime(mjd) << std::endl;

    // UT1 - UTC
    std::cout << "-- UT1UTC: " << astroTime.ut1Utc(mjd) << std::endl;
    if (dup)
      std::cout << "-- UT1UTC: " << astroTime2.ut1Utc(mjd) << std::endl;

    std::cout << "-- SLAGST: " << slaGmst(mjd+astroTime.ut1Utc(mjd)/86400.0)*12/M_PI << std::endl;
    return 0;

}
