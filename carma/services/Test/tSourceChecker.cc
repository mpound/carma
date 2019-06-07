#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"
#include "carma/util/StringUtils.h"

#include "carma/services/stringConstants.h"
#include "carma/services/SourceChecker.h"
#include "carma/services/SourceCatalog.h"
#include "carma/services/Types.h"
#include "carma/services/AstroTime.h"
#include "carma/services/Location.h"

//
// @author Marc Pound
// @version $Revision: 1.11 $ $Date: 2007/10/24 21:08:45 $
//
// @usage  test program for SourceChecker
//
// @description
//      Test program for SourceChecker calculations.  
//      This program goes through the system source catalog
//      and lists the current sky info about each source as
//      viewed from CARMA. It also does the planets.
// @key   elevlim     10 d  The elevation limit for rise/set time calculation.
// @key   j2000       true b  Print RA/DEC in J2000 format. If false use apparent RA/DEC
//
// @see checksource
// @logger DEFAULT_FACILITY carma.services.Test.tSourceChecker

using namespace std;
using namespace carma::services;
using namespace carma::util;

int carma::util::Program::main()
{
    try {
        const string planet[] = {
            "Sun", "Mercury", "Venus", "Moon", "Mars",
            "Jupiter", "Saturn", "Neptune", "Uranus", "Pluto"
        };
        double elevLimit  = getDoubleParameter("elevlim") ; // degrees
        bool   j2000      = getBoolParameter("j2000") ;
        SourceChecker checker;
        checker.setLocation( Location( CARMA_OBSERVATORY ) );
        checker.setElevLimit(elevLimit);
        checker.setFrequency(0.0);  // no refraction correction
        checker.setMJD();
	cout << "Local time: " << checker.localTime() << endl;
        checker.useJ2000(j2000);
        SourceCatalog sc;
        sc.open(SourceCatalog::defaultCatalog());
        SourceIterator si = sc.catalogBegin();
        checker.showHeader(true);

        while ( si != sc.catalogEnd() ) {
            checker.setSource(si->second);
            cout << checker.info() << endl;
            checker.showHeader(false);
            si++;
        }

        cout << endl;
        double mjd = Time::MJD();
        checker.setMJD(mjd);
        checker.showHeader(true);
        for ( unsigned int i=0; i < 10; i++)
        {
            checker.setSource(planet[i]);
            cout << checker.info() << endl;
            checker.showHeader(false);
	    double mut = checker.minutesUntilTransit();
	    double fmut = fabs(mut);
	    const string word = ( mut < 0 ? "since" : "until" );

	    string l = StringUtils::hms(checker.lst(), 1 );
            cout << " Current LST: " << l 
		 << " Minutes until set: "  << checker.minutesUntilSet() 
                 << " Minutes until rise: " << checker.minutesUntilRise() << endl
                 << " Minutes since set: "  << checker.minutesSinceSet() 
                 << " Minutes since rise: " << checker.minutesSinceRise() << endl
                 << " Minutes " << word << " transit: " << fmut
                 << endl;
            cout << " Azimuth (deg): " << checker.getAzimuth().degrees()
                 << " Elevation (deg): " << checker.getElevation().degrees()
                 << endl;
	    if ( fmut > 720.0 ) {
		ostringstream os;
		os << "Delta transit > 12 hrs for source " << planet[i];
		throw CARMA_EXCEPTION( ErrorException, os.str().c_str() );
	    }
        }

	// test of azimuth wrap code
	//cout << Time::MJD() << endl;
        checker.setLocation(Location("wht"));
	checker.setMJD( 54378.80000000 );
	checker.setElevUpperLimit( Angle(90,"degrees"));
	checker.setElevLimit( 10 );
	checker.setAzPositiveWrapLimit( Angle(355.0,"degrees") );
	checker.setAzNegativeWrapLimit( Angle(-175.0,"degrees") );
	// this source hits WHT azimuth limit at 54377.830963
	const string sname("0841+708");
	checker.setSource( sname );
	cout << endl << sname
	     << " AZ,EL @ WHT = (" 
	     << checker.getAzimuth().degrees()
	     << ","
	     << checker.getElevation().degrees()
	     << ")"
	     << endl;
	cout << "AZLIMIT: ";
	switch( checker.getTelescopeStatus().getLimits() ) {
	    case LIMIT_NEVER_RISES:
		cout << " NEVER RISES " << endl;
		break;

	    case LIMIT_HORIZON_STOP:
		cout << " HORIZON LIMIT " << endl;
		break;

	    case LIMIT_AZ_HORIZON_STOP:
		cout << " POSITIVE AZ LIMIT, HORIZON " << endl;
		break;

	    case LIMIT_AZ_STOP:
		cout << " POSITIVE AZ LIMIT " << endl;
		break;

	    case NO_LIMIT:
		cout << " NO LIMIT" << endl;
		break;
	    }
	    cout << "Minutes until WHT azimuth limit (=355) " 
		 << checker.minutesUntilAzWrapLimit() 
		 << " SHOULD BE " << 0.03*AstroTime::MINUTES_PER_DAY
		 << endl;

	// this source hits WHT azimuth limit at 54378.9193900  
	const string s2name("1153+809");
	checker.setSource( s2name );
	checker.setMJD(54378.80000);
	cout << endl << s2name
	     << " AZ,EL @ WHT = (" 
	     << checker.getAzimuth().degrees()
	     << ","
	     << checker.getElevation().degrees()
	     << ")"
	     << endl;
	cout << "AZLIMIT: ";
	switch( checker.getTelescopeStatus().getLimits() ) {
	    case LIMIT_NEVER_RISES:
		cout << " NEVER RISES " << endl;
		break;

	    case LIMIT_HORIZON_STOP:
		cout << " HORIZON LIMIT " << endl;
		break;

	    case LIMIT_AZ_HORIZON_STOP:
		cout << " POSITIVE AZ LIMIT, HORIZON " << endl;
		break;

	    case LIMIT_AZ_STOP:
		cout << " POSITIVE AZ LIMIT " << endl;
		break;

	    case NO_LIMIT:
		cout << " NO LIMIT" << endl;
		break;
	    }
	cout << "Minutes until WHT azimuth limit (=355) " 
	     << checker.minutesUntilAzWrapLimit() 
	     << " SHOULD BE " << 0.11939*AstroTime::MINUTES_PER_DAY
	     << endl;

	// this source never hist azlimit
	const string s3name("HP005372");
	checker.setSource( s3name );
	checker.setMJD(54378.80000);
	cout << endl << s3name
	     << " AZ,EL @ WHT = (" 
	     << checker.getAzimuth().degrees()
	     << ","
	     << checker.getElevation().degrees()
	     << ")"
	     << endl;
	cout << "AZLIMIT: ";
	switch( checker.getTelescopeStatus().getLimits() ) {
	    case LIMIT_NEVER_RISES:
		cout << " NEVER RISES " << endl;
		break;

	    case LIMIT_HORIZON_STOP:
		cout << " HORIZON LIMIT " << endl;
		break;

	    case LIMIT_AZ_HORIZON_STOP:
		cout << " POSITIVE AZ LIMIT, HORIZON " << endl;
		break;

	    case LIMIT_AZ_STOP:
		cout << " POSITIVE AZ LIMIT " << endl;
		break;

	    case NO_LIMIT:
		cout << " NO LIMIT" << endl;
		break;
	    }
	// no limits. can be tracked always on the negative wrap
	cout << "Minutes until WHT azimuth limit (=355) " 
	     << checker.minutesUntilAzWrapLimit() 
	     << " SHOULD BE " << AstroTime::MINUTES_PER_DAY
	     << endl;


    } catch (const BaseException& ee) {
      cerr << "Program::main caught error " << ee.getMessage() << endl;
      return EXIT_FAILURE;
    } catch (...) {
      cerr << "Program::main : an uncaught error" << endl;
      return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
