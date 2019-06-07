/** 
 * $Id: SourceCheckerTest.cc,v 1.16 2011/09/12 16:27:46 mpound Exp $
 * @file 
 * CppUnit test fixture for carma::services::SourceChecker
 *
 * @author: Marc Pound
 */
#include <cmath>
#include <map>
#include <string>
#include <ostream>
#include <vector>
#include "SourceCheckerTest.h"
#include "carma/services/stringConstants.h"
#include "carma/services/Angle.h"
#include "carma/services/AstroTime.h"
#include "carma/services/Frequency.h"
#include "carma/services/Location.h"
#include "carma/services/Neighbor.h"
#include "carma/services/Source.h"
#include "carma/services/SourceChecker.h"
#include "carma/services/SourceChecker.h"
#include "carma/services/Velocity.h"
#include "carma/services/Types.h"
#include "carma/util/ErrorException.h"


using namespace CppUnit;
using namespace std;
using namespace carma;
using namespace carma::services;
using namespace carma::util;

void SourceCheckerTest::setUp() 
{   
}

void SourceCheckerTest::tearDown() 
{   
}

namespace { // anonymous
    class TypeAzPair {
	public:
	TypeAzPair( carma::services::TelescopeLimitsType t, double m );
	virtual ~TypeAzPair();
	carma::services::TelescopeLimitsType type;
	double  minutesUntilAzLimit;

    };

    TypeAzPair::TypeAzPair( TelescopeLimitsType t, double m ) : 
	type(t), minutesUntilAzLimit(m) { }
    TypeAzPair::~TypeAzPair() { }

    class WrapCheck {
	public:
	WrapCheck( const string & name, 
		   double mjd,
		   vector<double> antAz,
		   vector<carma::services::AzWrapType>
		);
	virtual ~WrapCheck();

	string name_;
	double mjd_;
	vector<double> az_;
	vector<carma::services::AzWrapType> mode_;
    };

    WrapCheck::WrapCheck(const string & name, double mjd, vector<double> antAz, 
	                 vector<carma::services::AzWrapType> mode ) 
	: name_(name), mjd_(mjd), az_(antAz), mode_(mode) 
    {
    }

    WrapCheck::~WrapCheck() { }

}

string 
SourceCheckerTest::pMode(const AzWrapType mode) const
{
    switch (mode) {
    case AZWRAP_ZERO:
	return "AZWRAP_ZERO";
    case AZWRAP_ADD:
	return "AZWRAP_ADD";
    case AZWRAP_SUB:
	return "AZWRAP_SUB";
    default:
	return "UNKNOWN";
    }
}
    

void SourceCheckerTest::testAzWrapLimits()
{
    // Use known answers from WHT observatory
    // @see LIMITS TO OBSERVING TIMES FOR ALTAZIMUTH TELESCOPES
    // by R.A. Laing.  RGO Technical Note 71
    
    map< string, TypeAzPair > m;
    m.insert( make_pair("WHYA",     TypeAzPair(LIMIT_HORIZON_STOP, 0)) );
    m.insert( make_pair("0841+708", TypeAzPair(LIMIT_AZ_HORIZON_STOP, 44.6)) );
    m.insert( make_pair("1153+809", TypeAzPair(LIMIT_AZ_STOP, 172.0)) );
    m.insert( make_pair("HP005372", TypeAzPair(NO_LIMIT, 1440)) );
    SourceChecker checker;
    checker.setLocation( Location( "wht" ));
    checker.setElevLimit( 10.0 );
    checker.setElevUpperLimit( 90.0 );
    checker.setAzPositiveWrapLimit( Angle(355.0, DEGREES ) );
    checker.setAzNegativeWrapLimit( Angle(-175.0, DEGREES ) );
    checker.setFrequency(0.0);  // no refraction correction
    // hand-picked MJD to match the required answers
    checker.setMJD(54378.80000);

    // required accuracy in minutes. should be stricter than this 
    // but i don't quite understand where the slop in the calculation is 
    // coming from.
    const double epsilon = 0.5; 

    map< string, TypeAzPair>::const_iterator mi = m.begin();
    while ( mi != m.end() ) {

	cout << " Source " << mi->first << endl;
	checker.setSource( mi->first );

	// first check the type is as expected
	TelescopeLimitsType t = checker.getTelescopeStatus().getLimits();
	CPPUNIT_ASSERT( t == mi->second.type );

	// second check the minutes until limit is as expected
	double v = fabs( checker.minutesUntilAzWrapLimit() 
		        - mi->second.minutesUntilAzLimit  );
	cout << " Source " << mi->first 
	     << " diff : " << setprecision(6) << v << " VALUE "
	     << checker.minutesUntilAzWrapLimit() << endl;
	CPPUNIT_ASSERT( v < epsilon );
	mi++;
    }

    checker.setLocation( Location(CARMA_OBSERVATORY) );
    // 10m antenna limits
    checker.setAzPositiveWrapLimit( Angle(353.0, DEGREES) );
    checker.setAzNegativeWrapLimit( Angle(-88.0, DEGREES) );
    checker.setSource("1153+809");
    // source is at AZ=348 at this time, so can be tracked
    // on negative wrap.
    checker.setMJD(54378.0000);

    CPPUNIT_ASSERT( checker.canBeTrackedOnNegativeWrap() );
    // az=221, cannot be tracked on negative wrap
    checker.setSource("WHYA");
    CPPUNIT_ASSERT( checker.canBeTrackedOnNegativeWrap() == false );

    // see what past limit looks like for normal az wrap limit < 360
    checker.setSource("1153+809");
    checker.setMJD(54378.2100);
    cout << checker.info() << endl;
    cout << "LIM (10m)" << checker.getTelescopeStatus().getLimitsString() << endl;
    cout << "WRAP " << checker.getAzPositiveWrapLimit().degrees() << endl;
    cout << "AZ " << checker.getAzimuth().degrees() << endl;
    cout << "TIME " << checker.minutesUntilAzWrapLimit() << " PAST LIMIT" << endl;
    checker.setSource("0841+708");
    cout << checker.info() << endl;
    cout << "LIM (10m)" << checker.getTelescopeStatus().getLimitsString() << endl;
    checker.setSource("0431+206");
    cout << checker.info() << endl;
    cout << "LIM (10m)" << checker.getTelescopeStatus().getLimitsString() << endl;
    checker.setSource("0921+622");
    cout << checker.info() << endl;
    cout << "LIM (10m)" << checker.getTelescopeStatus().getLimitsString() << endl;
    checker.setSource("HP005372");
    cout << checker.info() << endl;
    cout << "LIM (10m)" << checker.getTelescopeStatus().getLimitsString() << endl;

    // 6m antenna limits
    checker.setAzPositiveWrapLimit( Angle(460.0, DEGREES) );
    checker.setAzNegativeWrapLimit( Angle(-90.0, DEGREES) );
    checker.setSource("0431+206");
    checker.setMJD(54378.35);
    cout << checker.info() << endl;
    cout << "LIM (6m)" << checker.getTelescopeStatus().getLimitsString() << endl;
    cout << "WRAP " << checker.getAzPositiveWrapLimit().degrees() << endl;
    cout << "AZ " << checker.getAzimuth().degrees() << endl;
    double lim = checker.minutesUntilAzWrapLimit();
    cout << "TIME " << lim << " should be about 5" << endl;

    CPPUNIT_ASSERT( fabs(5.0-lim) < epsilon );

    checker.setMJD(54377.5);
    cout << checker.info() << endl;
    cout << "LIM (6m)" << checker.getTelescopeStatus().getLimitsString() << endl;
    cout << "WRAP " << checker.getAzPositiveWrapLimit().degrees() << endl;
    cout << "AZ " << checker.getAzimuth().degrees() << endl;
    lim = checker.minutesUntilAzWrapLimit();
    //cout << "TIME " << lim <<" PAST LIMIT" << endl;
    CPPUNIT_ASSERT( lim > 0 );

    checker.setSource("0921+622");
    cout << checker.info() << endl;
    cout << "LIM (6m)" << checker.getTelescopeStatus().getLimitsString() << endl;

    checker.setSource("0753+538");
    cout << checker.info() << endl;
    cout << "LIM (6m)" << checker.getTelescopeStatus().getLimitsString() << endl;
    checker.setSource("0834+555");
    cout << checker.info() << endl;
    cout << "LIM (6m)" << checker.getTelescopeStatus().getLimitsString() << endl;
    checker.setSource("0837+584");
    cout << checker.info() << endl;
    cout << "LIM (6m)" << checker.getTelescopeStatus().getLimitsString() << endl;
}

void SourceCheckerTest::testZenithBlindSpot()
{
    SourceChecker checker;
    checker.setLocation( Location( CARMA_OBSERVATORY ));
    checker.setElevLimit( 10.0 );
    checker.setElevUpperLimit( 87.5 );
    checker.setAzPositiveWrapLimit( Angle(353.0, DEGREES) );
    checker.setAzNegativeWrapLimit( Angle(-88.0, DEGREES) );

    // First try a source that will get above 87.5 elevation
    checker.setSource("2015+372");
    bool zb = checker.getTelescopeStatus().canEnterZenithBlindSpot();
    CPPUNIT_ASSERT( zb == true );

    checker.setMJD( 54379.1278963 ); // elevation = 88.6833
    zb = checker.getTelescopeStatus().isInZenithBlindSpot();
    CPPUNIT_ASSERT( zb == true );
    CPPUNIT_ASSERT( checker.isUp() == true );

    checker.setMJD( 54379.3736647 ); // elevation = 23.5203
    zb = checker.getTelescopeStatus().isInZenithBlindSpot();
    CPPUNIT_ASSERT( zb == false );
    CPPUNIT_ASSERT( checker.isUp() == true );

    checker.setMJD( 54379.5878963 ); // elevation = -14:1281
    zb = checker.getTelescopeStatus().isInZenithBlindSpot();
    CPPUNIT_ASSERT( zb == false );
    CPPUNIT_ASSERT( checker.isUp() == false );

    // now check a source that cannot get to 87.5 elevation
    checker.setSource("VXSGR");
    checker.setMJD( 54379.0 ); // elevation = 28.6
    zb = checker.getTelescopeStatus().canEnterZenithBlindSpot();
    cout << "VXSGR zb = " << boolalpha << zb << endl;
    CPPUNIT_ASSERT( zb == false );
    zb = checker.getTelescopeStatus().isInZenithBlindSpot();
    CPPUNIT_ASSERT( zb == false );

}

void 
SourceCheckerTest::testOptimumWrapValues()
{
    testOvroOptimumWrapValues();
    testBimaOptimumWrapValues();
}

void 
SourceCheckerTest::testOvroOptimumWrapValues()
{
    SourceChecker checker;
    checker.setLocation( Location( CARMA_OBSERVATORY ));
    checker.setElevLimit( 10.0 );
    checker.setElevUpperLimit( 87.5 );

    // first test 10m antennas
    checker.setAzPositiveWrapLimit( Angle(353.0, DEGREES) );
    checker.setAzNegativeWrapLimit( Angle(-88.0, DEGREES) );
    // one az in each quadrant including the negative wrap
    vector<double> antAz ;
    antAz.push_back(-60.0); 
    antAz.push_back(44.0); 
    antAz.push_back(138.2);
    antAz.push_back(200.0);
    antAz.push_back(300.0);
    map< string, WrapCheck > m;
    vector<AzWrapType> modevec;
    unsigned int size = antAz.size();
    for (unsigned int i = 0; i< size; i++ )
	modevec.push_back(AZWRAP_ZERO);

    //LIMIT_HORIZON_STOP, 
    //rises at az=128 mjd=54379.69, elev=0
    //sets at az 232, mjd=54380.0400
    //should always be on AZWRAP_ZERO wrap
    m.insert( make_pair("WHYA 10m Test", 
		         WrapCheck("WHYA",54737.75,antAz,modevec) ) ); 

    // LIMIT_AZ_HORIZON_STOP
    // rises at az=6.5 el=10 mjd=54380.21
    // sets at az=355 el=10 mjd 54381.1450
    // Choose it when at az=355, which then will always be AZWRAP_SUB
    // since it is in the forbidden zone of the 10m antennas
    modevec.clear();
    for (unsigned int i = 0; i< size; i++ )
	modevec.push_back(AZWRAP_SUB);

    m.insert( make_pair("0921+622 10m Test", 
		         WrapCheck("0921+622",54381.145,antAz,modevec) 
		       )
	    ); 
    // LIMIT_AZ_STOP
    // Circumpolar source
    // rises/sets at az=355.5 el=29.02 mjd=54380.215/54381.215
    // az = 349.5 at mjd=54381.0
    // So returned modes should be
    // antaz= -60 :sub
    // antaz= 44  :sub
    // antaz= 138 :sub
    // antaz= 200,300 :zero
    modevec.clear();
    modevec.push_back(AZWRAP_SUB);
    modevec.push_back(AZWRAP_SUB);
    modevec.push_back(AZWRAP_SUB);
    modevec.push_back(AZWRAP_ZERO);
    modevec.push_back(AZWRAP_ZERO);
    m.insert( make_pair("1153+809 10m Test", 
		         WrapCheck("1153+809",54381.0,antAz,modevec)
		       )
	    ); 
    // NO_LIMIT
    // Circumpolar source
    // rise/sets at az=0.344742228087  el=33.611076018606 at 54378.850/54338.95
    // returned modes should be all zero
    //
    modevec.clear();
    for (unsigned int i = 0; i< size; i++ )
	modevec.push_back(AZWRAP_ZERO);
    m.insert( make_pair("HP005372 10m Test 1", 
		         WrapCheck("HP005372",54378.850,antAz,modevec) 
		       )
	    ); 
    // az=355.5, el=38
    // modes should be all AZWRAP_SUB
    modevec.clear();
    for (unsigned int i = 0; i< size; i++ )
	modevec.push_back(AZWRAP_SUB);
    m.insert( make_pair("HP005372 10m Test 2", 
		         WrapCheck("HP005372",54379.550,antAz,modevec) 
		       )
	    ); 

    //source az=166, el=28.8
    modevec.clear();
    for (unsigned int i = 0; i< size; i++ )
	modevec.push_back(AZWRAP_ZERO);
    m.insert( make_pair("Jupiter 10m Test 1", 
		         WrapCheck("jupiter",54404.9,antAz,modevec) 
		       )
	    ); 

    //source az=222 el=17
    modevec.clear();
    for (unsigned int i = 0; i< size; i++ )
	modevec.push_back(AZWRAP_ZERO);
    m.insert( make_pair("Jupiter 10m Test 2", 
		         WrapCheck("jupiter",54404.06,antAz,modevec) 
		       )
	    ); 
    
    map< string, WrapCheck >::const_iterator mi = m.begin();
    while ( mi != m.end() ) {

	const string testName = mi->first;
	const WrapCheck w = mi->second;
	cout << "Wrap Test " << testName << endl;
	unsigned short al = w.az_.size();
	unsigned short ml = w.mode_.size();
	if (  al != ml ) {
	    ostringstream os ;
	    os << " Vector sizes don't match! "
	       << al << " != " << ml;

	    throw CARMA_EXCEPTION(ErrorException, os.str() );
	}

	checker.setSource( w.name_ );
	checker.setMJD( w.mjd_ );
	AzWrapType mode;
	for ( int i = 0 ; i< al ; i++ ) 
	{
	    mode = computeOptimumWrapValue( checker, ANT_TYPE_OVRO, 
		                            w.az_.at(i), 25.0);
	    cout << " Source " << testName
		 << " antAz " << w.az_.at(i) 
		 << " Source Az " << checker.getAzimuth().degrees() 
		 << " minutes " << checker.minutesUntilAzWrapLimit()
	         << " mode = " << pMode(mode) 
		 << " expected = " << pMode(w.mode_.at(i)) 
		 << endl;
            CPPUNIT_ASSERT( mode == w.mode_.at(i) );
	}

	mi++;
    }


}


void 
SourceCheckerTest::testBimaOptimumWrapValues()
{
    CPPUNIT_ASSERT( true );
    SourceChecker checker;
    checker.setLocation( Location( CARMA_OBSERVATORY ));
    checker.setElevLimit( 10.0 );
    checker.setElevUpperLimit( 87.5 );

    checker.setAzPositiveWrapLimit( Angle(460.0, DEGREES) );
    checker.setAzNegativeWrapLimit( Angle(-90.0, DEGREES) );
    // one az in each quadrant including the negative wrap
    vector<double> antAz ;
    antAz.push_back(-60.0); 
    antAz.push_back(44.0); 
    antAz.push_back(138.2);
    antAz.push_back(200.0);
    antAz.push_back(300.0);
    antAz.push_back(400.0);
    antAz.push_back(459.0);
    map< string, WrapCheck > m;
    vector<AzWrapType> modevec;
    modevec.clear();
    unsigned int size = antAz.size();
    for (unsigned int i = 0; i< size; i++ )
	modevec.push_back(AZWRAP_ZERO);

    // source az 99 [1 degree away from Az limit modulo(2PI)], el 46
    m.insert( make_pair("0431+206 6m Test 1", 
		         WrapCheck("0431+206",54378.350,antAz,modevec) 
		       )
	    ); 

    modevec.clear();
    modevec.push_back(AZWRAP_ZERO); // -60
    modevec.push_back(AZWRAP_ZERO); // 44
    modevec.push_back(AZWRAP_ZERO); // 138.2
    modevec.push_back(AZWRAP_ZERO); // 200
    modevec.push_back(AZWRAP_ADD);  // 300
    modevec.push_back(AZWRAP_ADD);  // 400
    modevec.push_back(AZWRAP_ADD); // 459
    // source az=78, el=18
    m.insert( make_pair("0431+206 6m Test 2", 
		         WrapCheck("0431+206",54379.250,antAz,modevec) 
		       )
	    ); 

    //source az=166, el=28.8
    modevec.clear();
    for (unsigned int i = 0; i< size; i++ )
	modevec.push_back(AZWRAP_ZERO);
    m.insert( make_pair("Jupiter 6m Test 1", 
		         WrapCheck("jupiter",54404.9,antAz,modevec) 
		       )
	    ); 

    //source az=222 el=17
    modevec.clear();
    for (unsigned int i = 0; i< size; i++ )
	modevec.push_back(AZWRAP_ZERO);
    m.insert( make_pair("Jupiter 6m Test 2", 
		         WrapCheck("jupiter",54404.06,antAz,modevec) 
		       )
	    ); 

    // source az=326 el=24.54
    modevec.clear();
    modevec.push_back(AZWRAP_SUB); // -60
    modevec.push_back(AZWRAP_SUB); // 44
    modevec.push_back(AZWRAP_SUB); // 138.2
    modevec.push_back(AZWRAP_ZERO); // 200
    modevec.push_back(AZWRAP_ZERO);  // 300
    modevec.push_back(AZWRAP_ZERO);  // 400
    modevec.push_back(AZWRAP_ZERO); // 459
    checker.setSource("0837+584");
    m.insert( make_pair("0837+584 6m Test 1", 
		         WrapCheck("0837+584",54377.940,antAz,modevec) 
		       )
	    ); 


    // check a source right at transit
    // az=3.59, el =85.73
    modevec.clear();
    modevec.push_back(AZWRAP_ZERO); // -60
    modevec.push_back(AZWRAP_ZERO); // 44
    modevec.push_back(AZWRAP_ZERO); // 138.2
    modevec.push_back(AZWRAP_ADD);  // 200
    modevec.push_back(AZWRAP_ADD);  // 300
    modevec.push_back(AZWRAP_ADD);  // 400
    modevec.push_back(AZWRAP_ADD);  // 459
    checker.setSource("3c84");
    m.insert( make_pair("3c84 6m Test 1", 
		         WrapCheck("3c84",54418.319,antAz,modevec) 
		       )
	    ); 

    // check a source right at transit
    // az=359.96 el =85.73
    modevec.clear();
    modevec.push_back(AZWRAP_SUB); // -60
    modevec.push_back(AZWRAP_SUB); // 44
    modevec.push_back(AZWRAP_SUB); // 138.2
    modevec.push_back(AZWRAP_ZERO); // 200
    modevec.push_back(AZWRAP_ZERO); // 300
    modevec.push_back(AZWRAP_ZERO); // 400
    modevec.push_back(AZWRAP_ZERO); // 459
    checker.setSource("3c84");
    m.insert( make_pair("3c84 6m Test 1", 
		         WrapCheck("3c84",54418.320,antAz,modevec) 
		       )
	    ); 

    map< string, WrapCheck >::const_iterator mi = m.begin();
    while ( mi != m.end() ) {

	const string testName = mi->first;
	const WrapCheck w = mi->second;
	cout << "Wrap Test " << testName << endl;
	unsigned short al = w.az_.size();
	unsigned short ml = w.mode_.size();
	if (  al != ml ) {
	    ostringstream os ;
	    os << " Vector sizes don't match! "
	       << al << " != " << ml;

	    throw CARMA_EXCEPTION(ErrorException, os.str() );
	}

	checker.setSource( w.name_ );
	checker.setMJD( w.mjd_ );
	AzWrapType mode;
	for ( int i = 0 ; i< al ; i++ ) 
	{
	    mode = computeOptimumWrapValue( checker, ANT_TYPE_BIMA, 
		                            w.az_.at(i), 25.0);
	    cout << " Source " << testName
		 << " antAz " << w.az_.at(i) 
		 << " Source Az " << checker.getAzimuth().degrees() 
		 << " Source El " << checker.getElevation().degrees() 
		 << " minutes " << checker.minutesUntilAzWrapLimit()
	         << " mode = " << pMode(mode) 
		 << " expected = " << pMode(w.mode_.at(i)) 
		 << endl;
            CPPUNIT_ASSERT( mode == w.mode_.at(i) );
	}

	mi++;
    }

}


// @todo move this to SourceChecker
// and remove parallel code from DriveHandle
// requires consolidation of azwrap and antennatype enumerations
// to services.
services::AzWrapType 
SourceCheckerTest::computeOptimumWrapValue( 
	SourceChecker & checker,
	services::AntennaType antType,
	double antAzDegrees,
        double timeToTrack )
{
    return checker.computeOptimumWrapValue(antType, antAzDegrees, timeToTrack);
}


void 
SourceCheckerTest::bug571Test( void )
{
    SourceChecker checker;
    checker.setLocation( Location( CARMA_OBSERVATORY ));
    checker.setElevLimit( 10.0 );
    checker.setElevUpperLimit( 87.5 );

    // 10m antennas M51 at -7 degrees
    checker.setAzPositiveWrapLimit( Angle(353.0, DEGREES) );
    checker.setAzNegativeWrapLimit( Angle(-88.0, DEGREES) );
    vector<double> antAz ;
    antAz.push_back(-6.999); 
    antAz.push_back(352); 
    vector<AzWrapType> modevec;
    unsigned int size = antAz.size();
    for (unsigned int i = 0; i< size; i++ )
	modevec.push_back(AZWRAP_SUB);

    map< string, WrapCheck > m;
    // Source is at az 354 so it must be SUB in  both cases
    //timeConvert tomjd="2007 Dec 13 16:01:09" : 54447.667361
    m.insert( make_pair("M51 10m Test a", 
		         WrapCheck("M51MOS",54447.667361,antAz,modevec) ) ); 

    modevec.clear();
    modevec.push_back(AZWRAP_SUB);
    modevec.push_back(AZWRAP_ZERO);
    // Source is at az 352.9 so it is SUB in first case and ZERO in second.
    //timeConvert tomjd="2007 Dec 13 16:02:09" : 54447.668160
    //timeConvert tomjd="2007 Dec 13 16:03:09" : 54447.668750
    m.insert( make_pair("M51 10m Test b", 
		         WrapCheck("M51MOS",54447.669160,antAz,modevec) ) ); 
    m.insert( make_pair("M51 10m Test c", 
		         WrapCheck("M51MOS",54447.668750,antAz,modevec) ) ); 

    map< string, WrapCheck >::const_iterator mi = m.begin();
    const Source M51("M51MOS",
	        Angle(13.4982*M_PI/12.0,"radians"),
		Angle(47.1918,"degrees"),
		Velocity(-30.4605,"km/s"),
		Distance(0.0,"pc")
	    );

    const Source NGC7538("NGC7538M",
		Angle(23.2292*M_PI/12.0,"radians"),
		Angle(61.4583,"degrees"),
		Velocity(-56.0,"km/s"),
		Distance(0.0,"pc")
	    );

    while ( mi != m.end() ) {

	const string testName = mi->first;
	const WrapCheck w = mi->second;
	cout << "Wrap Test " << testName << endl;
	unsigned short al = w.az_.size();
	unsigned short ml = w.mode_.size();
	if (  al != ml ) {
	    ostringstream os ;
	    os << " Vector sizes don't match! "
	       << al << " != " << ml;

	    throw CARMA_EXCEPTION(ErrorException, os.str() );
	}

	//const string mycat("/home/mpound/src/carma/conf/catalogs/observer/mpound.cat");
	//checker.setSource( w.name_, mycat );
	checker.setSource( M51 );
	checker.setMJD( w.mjd_ );
	AzWrapType mode;
	for ( int i = 0 ; i< al ; i++ ) 
	{
	    mode = computeOptimumWrapValue( checker, ANT_TYPE_OVRO, 
		                            w.az_.at(i), 25.0);
	    cout << " Source " << testName
		 << " antAz " << w.az_.at(i) 
		 << " Source Az " << checker.getAzimuth().degrees() 
		 << " Source El " << checker.getElevation().degrees() 
		 << " minutes " << checker.minutesUntilAzWrapLimit()
	         << " mode = " << pMode(mode) 
		 << " expected = " << pMode(w.mode_.at(i)) 
		 << endl;
            CPPUNIT_ASSERT( mode == w.mode_.at(i) );
	}

	mi++;
    }

}

void 
SourceCheckerTest::testGetNearest( void )
{

    set<string> inputExcludeSet;
    // must be upper case here for comparison.
    string refSource("3C273");
    inputExcludeSet.insert( refSource );
    inputExcludeSet.insert("FOO");
    SourceChecker sc;
    sc.setSource( refSource );
    Frequency freq(115.271,services::GHZ);
    sc.setFrequency( freq.hertz() );
    sc.setMJD();

    const bool include = false;
    unsigned short numReturn = 10;
    const bool ignoreNorthSouth = true;
    const float magnitudeLimit = 3;
    

    NeighborSet neighborSet = sc.getNearest( inputExcludeSet, include,
	                             numReturn, ignoreNorthSouth,
				     services::COORDSYS_RADEC, 
				     services::PNT_OPTICAL,
				     magnitudeLimit );
    NeighborSet::iterator i = neighborSet.begin();
    cout  << "optical getNearest query return names:" << endl;
    for (; i != neighborSet.end(); ++i ) {
	Neighbor n = *i;
	      cout << n.getName() << " " << n.getDistance() << endl;
    }

    // must be upper case here for comparison.
    refSource = "3C345";
    inputExcludeSet.clear();
    inputExcludeSet.insert( refSource );
    //cout << "RefSOURCE is " << refSource << endl;
    sc.setSource( refSource );
    freq.reset( 95.0, services::GHZ );
    sc.setFrequency( freq.hertz() );
    // March 25, 2009
    sc.setMJD(54915.4552955);
    numReturn = 3;
    const float fluxLimit = 0.01;
    // we know the correct answer to this query
    set<string> validReturnSet;
    validReturnSet.insert( "1640+397" );
    validReturnSet.insert( "1653+397" );
    validReturnSet.insert( "1635+381" );
    neighborSet = sc.getNearest( inputExcludeSet, include,
	                         numReturn, ignoreNorthSouth,
				 services::COORDSYS_RADEC, 
				 services::PNT_RADIO,
				 fluxLimit );
    i = neighborSet.begin();
    set<string> queryReturnNames;
    cout  << "radio getNearest query return names:" << endl;
    for (; i != neighborSet.end(); ++i ) {
        Neighbor n = *i;
        cout << n.getName() << " " << n.getDistance() << endl;
        queryReturnNames.insert( n.getName() );
    }
    cout  << "expecting to match:" << endl;
    set<string>::iterator j = validReturnSet.begin();
    for (; j != validReturnSet.end(); ++j ) {
        string n = *j;
        cout << n << endl;
    }

    // Note:  == works with std::set!
    CPPUNIT_ASSERT( queryReturnNames == validReturnSet );

}


