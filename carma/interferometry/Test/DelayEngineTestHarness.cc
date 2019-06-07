/* $Id: DelayEngineTestHarness.cc,v 1.42 2011/01/03 18:48:29 iws Exp $ */

#include <iomanip>
#include <iostream>
#include <fstream>
#include <sstream>


#include "carma/services/Atmosphere.h"
#include "carma/interferometry/DelayEngine.h"
#include "carma/interferometry/DelayInfo.h"
#include "carma/monitor/DelayEngineSubsystem.h"
#include "carma/services/Angle.h"
#include "carma/services/AstroTime.h"
#include "carma/services/Global.h"
#include "carma/services/Length.h"
#include "carma/services/Location.h"
#include "carma/services/Ephemeris.h"
#include "carma/services/Observatory.h"
#include "carma/services/Pad.h"
#include "carma/services/padUtils.h"
#include "carma/services/Source.h"
#include "carma/services/SourceCatalog.h"
#include "carma/services/Types.h"
#include "carma/services/Units.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

using namespace carma::interferometry;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace std;

//
// @version     $Revision: 1.42 $ $Date: 2011/01/03 18:48:29 $
//
// @usage  Run a delay engine test.
//
// @description
//  This program instantiates a Delay Engine for testing.  It loops over 
//  several declinations (as indicated by the decstart and decend keywords)
//  and writes the Delay Engine calculations to the specified trace file.
//  Alternatively, it will take a specific source rather than a declination
//  range.
//  The default time range is +/- 3 hrs in hour angle, with default
//  steps of half an hour (timestep keyword).
//  There are a number of different tests that can be run, specified
//  by the xxxtest keywords.
//
//  There a few ways to run this program: 
//
//  1. Using a declination range and antparam file
//
//       DelayEngineTestHarness antparam=<filename> 
//           decstart=<declination1> decend=<declination2> decstep=<dstep> 
//           timestep=<tstep> harange=<harange>
//
//  2. Using a source and antparam file
//
//       DelayEngineTestHarness antparam=<filename> source=<sourcename>
//           timestep=<tstep> harange=<harange> 
//
//  2. Using a source and observatory configuration.
//
//       DelayEngineTestHarness observatory=<obs> config=<config> 
//           source=<sourcename> timestep=<tstep> harange=<harange>
//           padtest=<t|f>
//
//           where obs is e.g., carma, and config is e.g., FL,
//           (same meaning as in checksource).
//
//  This program is invoked by the integration test
//  $CARMA/interferometry/Test/tDelayEngineIntegrationTest.rt
//       
//
// @key  antparam bima_c.antparam s Name of file with antenna parameters
// @key  nanosec    t           b True if input XYZ coordinates are nanoseconds
// @key  observatory carma      s Observatory name, used to look up 
//                                reference position
// @key  config     ""          s Array config from Observatory.cat.  This keyword is
// exclusive with antparam.  If neither is specified, the default antparam is used.
// @key  maxants   15           i Maximum number of antennas to use, will be truncated to this value if input is greater.
// @key  decstart   0           d Beginning declination (degrees), ignored if source is given
// @key  decend     0           d Ending declination (degrees), ignored if source is given
// @key  decstep   20           d Size of steps to take in declination (degrees), ignored if source is given
// @key  harange    6           d Total hour angle range (hours). Starting hour angle 
//                                will be -harange/2, ending will be +harange/2
// @key  timestep  30           d Size of steps to take in time (minutes)
// @key  source     3c273       s Use a source instead of declination steps.
// @key  dra        0           d RA sky offset arcmin to add to source position
// @key  ddec       0           d DEC sky offset arcmin to add to source position
// @key  freq     115.271254    d Frequency (GHz) to use for atmospheric delay correction
// @key  padtest    f           b Do a test adding fixed pad ENU offsets. Ignored if config keyword is unset.
// @key  selftest   f           b Whether or not to perform the delay engine internal selftest.
// @key  bimatest   f           b Whether or not to test against old bima code
// @key  uvwtest    f           b Whether or not to test UVW calculation
// @key  triplettest    f       b Whether or not to test triplet continuity
// @key  addHeight  0           d Additional height in millimeters to add to first 6 antennas (OVRO-BIMA difference is 223 mm)
// @key  print      f           b print out series of delays as a table
//
// @logger INTERFEROMETRY_FACILITY carma.interferometry.Test.DelayEngineTestHarness

/** 
 * @author Marc Pound
 */

int Program::main()
{
    const double MAX_UVW_DIFF   = 1.0E-9;
    const double NSEC_PER_METER = 3.33564;  //conversion factor: 1/C in nanosec/meter
    const string fmt("%3d %9.6g %9.6f %9.6f %9.6f %11.5f %11.5f %11.5f %13.7f %13.7f %13.7f %13.7f %13.7f    %6.3f %13.7f %13.7f %13.7f  %9.2E  %9.2E  %9.2E\n");// %12.9f %12.9f\n");

    int status  = EXIT_SUCCESS;
try {

    const string fileName = getStringParameter("antparam");
    bool   convert        = getBoolParameter("nanosec");
    bool   doPadTest      = getBoolParameter("padtest");
    bool   doBimaTest     = getBoolParameter("bimatest");
    bool   doUvwTest      = getBoolParameter("uvwtest");
    bool   doTripTest     = getBoolParameter("triplettest");
    bool   doPrint        = getBoolParameter("print");
    const string observatory = getStringParameter("observatory");
    const string config      = getStringParameter("config");
    bool   useConfig = parameterWasSpecified("config");
    bool   doHeight = parameterWasSpecified("addHeight");
    double additionalHeight = getDoubleParameter("addHeight");
    // either antparam OR config, please
    if ( parameterWasSpecified("antparam") && useConfig ) {
	throw CARMA_ERROR("You can't specify BOTH antparam and config!");
    }

    unsigned short maxants  = getIntParameter("maxants");
    double decStart = getDoubleParameter("decstart");
    double decEnd   = getDoubleParameter("decend");
    double decStep  = getDoubleParameter("decstep");
    double timeStep = getDoubleParameter("timestep");
    double haRange  = getDoubleParameter("harange");
    bool   doSelfTest = getBoolParameter("selftest");
    const string sourceName = getStringParameter("source");
    bool   useSource = parameterWasSpecified("source");
    double dra       = getDoubleParameter("dra");
    double ddec      = getDoubleParameter("ddec");
    double frequency = getDoubleParameter("freq");
    const Length OVRO_BIMA_DIFF_HEIGHT(223.0, "mm");
    const Length ZERO_LENGTH(0, "mm");
    if (frequency < 0) 
	throw CARMA_EXCEPTION(IllegalArgumentException,
	"Frequency parameter must be non-negative.");

    Units units;
    // convert to radians
    decStart = units.convert(decStart,"degrees","radians");
    decStep  = units.convert(decStep,"degrees","radians");
    decEnd   = units.convert(decEnd,"degrees","radians");

    // convert to days
    timeStep  = units.convert(timeStep,"minutes", "days");

    double RA; // radian
    const unsigned short gmax = Global::maxAntennas() ;
    unsigned short maxAnts = min(gmax,maxants);
    double X[maxAnts];      // x position
    double Y[maxAnts];      // y position
    double Z[maxAnts];      // z position
    double axis[maxAnts];   // axis misalignment
    double delay0[maxAnts]; // delay offset
    bool discontinuity;

    DelayEngine delayEngine;
    Observatory obs(observatory);
    const Location refLoc = obs.getReference(); 
    cout << "Reference " << refLoc << endl;
    AntennaCoordinates arrayRefPt(refLoc);
    vector<Pad> padv;
    if (useConfig) {
	padv = obs.getPadsInConfig(config);
	unsigned short numc = obs.numPadsInConfig(config);
	maxAnts = min( numc, maxants );
    } else {
    // loop over lines to get X,Y,Z antenna positions
	ifstream inFile(fileName.c_str());

	// if file was not found, throw a carma exception
	if (!inFile) {
	    ostringstream os;
	    os << "Can't open file: " << fileName;
	    throw CARMA_ERROR(os.str());
	};
	string s;
	unsigned short i = 0;
	padv.reserve(maxAnts);
	while (getline(inFile, s)) {
	    if (i >= maxAnts) {
		CPTRACE(carma::util::Trace::TRACE7,
		     " Max number of antennas reached -- skipping rest ");
		break;
	    }
	    // # is comment char if at beginning of line
	    if ( s[0] == '#' ) continue;
	    sscanf(s.c_str(),"%lf %lf %lf %lf %lf",
			       &X[i],   &Y[i],   &Z[i],
			       &axis[i], &delay0[i]
			       );

	    // convert POSITIONS ONLY from nanoseconds to meters.
	    if ( convert ) {
		X[i]    /= NSEC_PER_METER;
		Y[i]    /= NSEC_PER_METER;
		Z[i]    /= NSEC_PER_METER;
		//axis[i] /= NSEC_PER_METER;
		//delay0[i] /= NSEC_PER_METER;
	    }

	    // create Pad objects from the input topo XYZ 
	    // coordinates and the array reference point.
	    Vector<double> uen = 
		arrayRefPt.topoXYZToUen( 
			X[i],Y[i],Z[i], refLoc.getLatitude()
		    ) ;
	    Length up(uen[0],"meter");
	    Length east(uen[1],"meter");
	    Length north(uen[2],"meter");
	    Vector<double> lonLatAlt = arrayRefPt.getLla(
		    up.meters(), east.meters(), north.meters()
		    );

	    const Location padLoc( Angle( lonLatAlt[ 0 ], "radians" ),
		                  Angle( lonLatAlt[ 1 ], "radians" ), 
				  Length( lonLatAlt[ 2 ], "meters" ) );

	    ostringstream padnameOs;
	    padnameOs << "unnamed#" << i;
	    Pad thePad(observatory,padnameOs.str(),padLoc, refLoc, "X");
	    cout << " Ant "<<i+1<<" pad " << padLoc <<endl;
	    padv.push_back(thePad);

	    i++;
	}
	// fill out the rest of the array in case there weren't
	// enough lines in the file.
	while(i<maxAnts) {
	    X[i] = Y[i] = Z[i] = axis[i] = delay0[i] = 0.0;
	    i++;
	}
    }

    double now = carma::util::Time::MJD();
    double dhours ;
    double startMjd;
    double stopMjd ;
    AstroTime astroTime;
    Ephemeris eph;
    eph.setSource(sourceName);
    eph.setLocation(refLoc);
    eph.setMJD(now);
    astroTime.setSite(refLoc);
    Source src = eph.getSource();
    if ( useSource ) {
	decStart=0;
	decEnd=0;
	decStep=100;
	RA = src.getXCoordinate().radians();
	double xRA = astroTime.localSiderealTime(now) * M_PI/12.; // radians
	// Find transit time by looping over mjd until
	// xRA = RA.  Get within two minutes or so of transit.
	while (  fabs(xRA - RA) > 0.01 ) { 
	    now += 0.0005; // 0.72 minutes
	    xRA = astroTime.localSiderealTime(now) * M_PI/12.; // radians
	}
	cout << "Using " << src.getName() 
	     << ". Transit MJD = " 
	     << setprecision(9)
	     << now 
	     << endl;
    } else {
	// put the source on the meridian 
	// (haRange takes care of tracking it).
	RA = astroTime.localSiderealTime(now) * M_PI/12.; // radians
    }
    // We will run for +/- harange hours from the transit
    dhours   = units.convert( haRange/2.0, "hours","days");
    startMjd = now - dhours;
    stopMjd  = now + dhours;

    // First try the selftest if requested. 
    // If that fails, return EXIT_FAILURE
    // and don't bother doing the rest of the test.
    if (doSelfTest) {
	bool selfOK = delayEngine.selfTest(true);
	ostringstream selfos;
	selfos << "SELFTEST RESULT: " << (selfOK ? "true" : "false");
	CPTRACE(carma::util::Trace::TRACE7, selfos.str());

	if ( !selfOK ) return EXIT_FAILURE;
    // Self-test succeeded, continue on.
    }

    // set the weather parameters once.
    delayEngine.setWeather(
	Atmosphere::DEFAULT_AIR_TEMP,
	Atmosphere::DEFAULT_ATM_PRESSURE,
	Atmosphere::DEFAULT_RH
    );

    // set array center reference coordinates
    delayEngine.setArrayReferencePoint(refLoc);

    // frequency to use for all 
    const Frequency freq(frequency,"GHz");

    // vector of adjusted locations, if doing pad offset test
    vector<Location> locv;
    locv.reserve(maxAnts);

    // this initialization needs to be done only once.
    // antenna numbers count from 1
    for(unsigned short antennaNo=1; antennaNo < maxAnts+1; antennaNo++) {
	unsigned short ind = antennaNo-1;
	CPTRACE(carma::util::Trace::TRACE7, 
	    "DelayEngineTestHarness: setting up antenna "<<antennaNo);
	delayEngine.setAntennaLOFreq(antennaNo,freq);
	delayEngine.setDelayOffset(antennaNo,delay0[ind]);
	const Length axisMis(axis[ind],"mm");
	if ( useConfig ) {
	    if ( doPadTest ) {
		// if doing pad offset test, add an
		// arbitrary fixed ENU offsets to the pad location.
		float offset = static_cast<float>(ind);
		const Location adjLoc = adjustedLocation(
					    padv[ind],
					    Length(offset,"m"),
					    Length(12.34*offset,"m"),
					    Length(5.0*offset,"m")
					);
		// save this location
		locv.push_back(adjLoc);
		delayEngine.setAntennaCoordinates(
			     antennaNo,
			     adjLoc,
			     axisMis);
	    } else {
		if ( doHeight && antennaNo < 6 ) {
		    const Location ovroLocation =adjustedLocation(
					    padv[ind],
					    ZERO_LENGTH,
					    ZERO_LENGTH,
					    Length(additionalHeight,"mm"));
		    delayEngine.setAntennaCoordinates(
				 antennaNo,
				 ovroLocation,
				 axisMis);
		} else {
		    delayEngine.setAntennaCoordinates(
				 antennaNo,
				 padv[ind].getLocation(),
				 axisMis);
		}
	    }
	} else {
	    delayEngine.setAntennaCoordinates(
			 antennaNo,
			 X[ind],Y[ind],Z[ind],
			 ANTCOORD_TOPO_XYZ,
			 axisMis.meters() );
	}
    }

    // timing variables.
    double start;
    double end;
    double diff;
    ostringstream output;
    if ( useConfig ) 
	output<< "#Pad " ;
    else 
	output<< "#Ant " ;

    output<< "APPRA(hr) "    
      << "APPDEC(deg) "   
      << "HA(hr)  " 
      << " LST(hr)     "   
      << "  X(ns)     "  
      << "  Y(ns)     "  
      << "  Z(ns)  "  
      << "   U(mjd,ns) " 
      << "   V(mjd,ns)  "  
      << "   U(2000,ns) " 
      << "   V(2000,ns)   "  
      << "   W(ns)"  
      << "   theta(deg)"  
      << "  MelU(2000,ns) " 
      << "  MelV(2000,ns) "
      << "  MelW(ns) "
      << "   diffU   "
      << "   diffV   "
      << "   diffW   "
      << "\n";
    

    // Loop over declination range requested by tester.
    for(double DEC = decStart; DEC <= decEnd; DEC += decStep ) {
      // loop over time range requested by tester.
      int count=0;
      for( double mjd = startMjd; mjd < stopMjd; mjd += timeStep) {
	// Loop over antennas
	// antenna numbers count from 1
	for(unsigned short antennaNo=1; antennaNo < maxAnts+1; antennaNo++) 
	{
	    // make sure the ephemeris has the location
	    // of the specific antenna, not just the
	    // array reference point. otherwise hour angle
	    // errors occur and increase as a function
	    // of antenna Y position.
	    if ( doPadTest ) {
		eph.setLocation(locv[antennaNo-1]);
		astroTime.setSite(locv[antennaNo-1]);
	    }
	    else {
		eph.setLocation(padv[antennaNo-1].getLocation());
		astroTime.setSite(padv[antennaNo-1].getLocation());
	    }

	    //******************************************
	    // Note the following INCORRECT assumption
	    // that LST and Hour angle may be calcuated w.r.t
	    // array ref pt will cause an errors in UVW that scale
	    // with the antenna Y coordinate.
	    // At Y=1m error ~ 1E-7 nanosec
	    // At Y=10m error ~ 1E-5 nanosec
	    // At Y=100m error ~ 1E-3 nanosec
	    // At Y=1000m error ~ 1E-1 nanosec
	    //******************************************
	    // eph.setLocation(refLoc);
            // astroTime.setSite(refLoc);
	    //******************************************
	    
	    discontinuity = ( count  == 0 ? true : false );
	    // Do the setup of the time-tagged triplet;
	    // need to write three times in order to interpolate.
	    // Use -1 to 2 ensure we hit HA=0, which is critical
	    // for checking the test "cross" array.
	    double timestamp=mjd;
	    eph.setMJD(timestamp); 
	    if (useSource) {
		double raOffrad = units.convert(dra,"arcminutes","radians");
		double decOffrad = units.convert(ddec,"arcminutes","radians");
		eph.setRaDecOffsets(raOffrad,decOffrad);
	    }
	    double currRA = eph.getRa();
	    double currDec = eph.getDec();
	    //for(int haStep=-1; haStep<2; haStep++) {
		// add i * 20 seconds to generate next
		// timestamp. (1 Day)/2880 = 30 seconds.
		//timestamp += 
		//    ((float)haStep * 0.333/AstroTime::MINUTES_PER_DAY );
		if (useSource) {
		    delayEngine.setAntennaRaDec(antennaNo,
			    timestamp,
			    currRA,
			    currDec,
			    currRA,
			    currDec,
			    true,"TEST");

		} else  {
		    delayEngine.setAntennaRaDec(antennaNo,
			       timestamp,RA,DEC,RA,DEC,true,"TEST");
		}
		// after first hastep, set to false.
		discontinuity = false;
	     //}
	// tell the engine to compute the delays for the triplet.
	// This will do it for all initialized antennas.
	 start = Time::MJD();
	 CPTRACE(carma::util::Trace::TRACE7, 
	    "DelayEngineTestHarness: Calling computedelays()...");
	 DelayFrameVec vd = delayEngine.computeDelays();
	 end = Time::MJD();
	 diff = ( end - start ) * AstroTime::SECONDS_PER_DAY;
	 CPTRACE(carma::util::Trace::TRACE7, 
	     " delayEngine->computeDelays() took " 
		  << diff << " seconds. count is " << count);

	 if ( vd.size() != 3 )  {
	     ostringstream os;
	     os << "Returned DelayFrameVec has wrong size: " << vd.size();
	     throw CARMA_ERROR ( os.str() );
	 }

	 const DelayEngineSubsystem* df;

	 if ( doTripTest ) 
	 {
	     ostringstream ttout;
	     ttout << "      |   ANT    MJD  GEODELAY     "
		   << "TROPODELAY    TOTALDELAY"
		   << endl;
	     if ( antennaNo == 1 ) 
	     {
		 for ( unsigned int i = 0; i < 3; i++ ) 
		 {
		     df = vd.at(i);
		     const DelayEngineSubsystem::DelayData& dd 
			 = df->delayData(antennaNo-1);
		     switch ( i ) {
			 case 0:
			  	ttout << "ANTE  |   ";
				break;
			 case 1:
			  	ttout << "PEN   |   ";
				break;
			 case 2:
			  	ttout << "CURR  |   ";
				break;
		     }
		     ttout 
			   << antennaNo
		           //<< setw(15) 
		           << setprecision(8) 
		           << " "
		           << dd.calculatedFor().getValue()
		           << "   "
			   << dd.geometricDelay().getValue()
		           << "   "
		           << dd.troposphericDelay().getValue()
		           << "   "
		           << dd.totalDelay().getValue()
			   << endl;
		 }
	     }
	     cout << ttout.str() << endl;
	 }

	 df = vd.at(2);

	 if ( df == 0 ) {
	     throw CARMA_ERROR ( "delay frame pointer is null" );
	 }

	 if ( doPrint )
	 {
	     const DelayEngineSubsystem::DelayData& dd 
		 = df->delayData(antennaNo-1);
	     if ( antennaNo == 1 )
		 cout << endl 
		      << "ANT      MJD          TIME          SRC    "
		      << "HA      AXISDELAY     GEODELAY     "
		      << "TROPODELAY    TOTALDELAY" 
		      <<endl;

	     const double cfMjd = dd.calculatedFor().getValue();
	     const string cftimeStr = Time::getTimeString( cfMjd, 1);
	     const double cfHA = (cfMjd - now ) * 24.;
	     cout << antennaNo
		   //<< setw(15) 
		   << "     "
		   << setprecision(12) 
		   << cfMjd
		   << "   "
		   << cftimeStr
		   << "   "
		   << src.getName() 
		   << "   "
		   << setprecision(3) 
		   << cfHA
		   << "   "
		   << setprecision(8) 
		   << dd.axisDelay().getValue()
		   << "   "
		   << dd.geometricDelay().getValue()
		   << "   "
		   << dd.troposphericDelay().getValue()
		   << "   "
		   << dd.totalDelay().getValue()
		   << endl;
	 }

	 const DelayEngineSubsystem::DelayData& dd 
	     = df->delayData(antennaNo-1);
	 double LST = astroTime.localSiderealTime(timestamp);
	 double HA;
	 double sind;
	 double cosd;
	 double theta;
	 double ora, odec;
	 if ( useSource ) {
	     sind  = sin(currDec);
	     cosd  = cos(currDec);
	     theta = eph.angle2000();
	     ora = currRA*12.0/M_PI;
	     odec = currDec*180/M_PI;
	 } else {
	     theta = 0.0;
	     sind  = sin(DEC);
	     cosd  = cos(DEC);
	     ora = RA*12/M_PI;
	     odec = DEC*180/M_PI;
	 }
	 HA = LST - ora;
	 double uu = dd.u().getValue()*NSEC_PER_METER ;
	 double vv = dd.v().getValue()*NSEC_PER_METER ;
	 double ww = dd.w().getValue()*NSEC_PER_METER ;
	 double costh = cos(theta);
	 double sinth = sin(theta);
	 double u2000 =  (uu * costh) + (vv * sinth);
	 double v2000 =  (vv * costh) - (uu * sinth);
	 double bxx = dd.x().getValue()*NSEC_PER_METER;
	 double byy = dd.y().getValue()*NSEC_PER_METER;
	 double bzz = dd.z().getValue()*NSEC_PER_METER;
	 // compare with "Mel method"
	 double sinha = sin(HA*M_PI/12.0);
	 double cosha = cos(HA*M_PI/12.0);
	 double melu =   bxx * sinha + byy * cosha;
	 double melv = -(bxx * cosha - byy * sinha)*sind + bzz*cosd;
	 double melw =  (bxx * cosha - byy * sinha)*cosd + bzz*sind;
	 /*
	 cout << " #############  MEL ROTATION MATRIX ################" <<endl;
	 cout << setw(15) << setprecision(8) << "FOR ["<<HA<< " , "<<odec<<" , "
	 << dd.X().getValue()
	      <<"," 
	<<  dd.Y().getValue()
	      <<"," 
	 << dd.Z().getValue()
	      <<"]" << endl;

	 cout << "| "
	     <<setw(15) << setprecision(8) 
	     << sinha       
	     <<setw(15) << setprecision(8) 
	     << cosha      
	     <<setw(15) << setprecision(8) 
	     << 0
	     << "| "<<endl;
	 cout << "| "
	     <<setw(15) << setprecision(8) 
	     << -sind*cosha 
	     <<setw(15) << setprecision(8) 
	     << sind*sinha 
	     <<setw(15) << setprecision(8) 
	     <<  cosd <<"| "<<endl;
	 cout << "| "
	     <<setw(15) << setprecision(8) 
	     << cosd*cosha 
	     <<setw(15) << setprecision(8) 
	     << -cosd*sinha
	     <<setw(15) << setprecision(8) 
	     <<  sind <<"| "<<endl;
	 cout << " ##################################################" <<endl;
	 */

	 double melu2000 =  (melu * costh) + (melv * sinth);
	 double melv2000 =  (melv * costh) - (melu * sinth);
	 double diffu = u2000 - melu2000;
	 double diffv = v2000 - melv2000;
	 double diffw = ww - melw;
	 diffu = uu - melu;
	 diffv = vv - melv;


	 char foo[256];
	 int num;
	 if ( useConfig ) {
	     string padName = padv[antennaNo - 1].getName() ;
	     string padNo = padName.substr(padName.find('#')+1);
	     if ( padNo.empty() ) {
		 num = antennaNo;
	     } else {
		 num = atoi( padNo.c_str() );
	     }
	 } else {
	     num = antennaNo;
	 }
	     sprintf(foo,fmt.c_str(),
		     num,
		     ora,
		     odec,
		     HA, LST,
		     bxx, byy, bzz,
		     uu, vv, u2000, v2000, ww,
		     theta*180.0/M_PI,
		     melu2000, melv2000, melw ,
		     diffu, diffv, diffw
	    //	 ,timestamp,
	    //	 dd.calculatedFor().getValue()
		     );
	     output << foo;

	 // add this check so that the integration test
	 // can notify of FAILURE.
	 // @todo make this a normalizedDiff like services tests.
	 if ( ( fabs(diffu) > MAX_UVW_DIFF ) ||
	      ( fabs(diffv) > MAX_UVW_DIFF ) ||
	      ( fabs(diffw) > MAX_UVW_DIFF ) 
	    ) 
	 {
	     // always spew output on error.
             cout << output.str() << endl;;
	     cout << "DIFFERENCE IN UVW IS TOO LARGE" << endl;
	     return EXIT_FAILURE;
	 }

	 count++;
      } // for antenna
      output << "#\n";
      // this does all antennas so keep it out of the antenna loop
      if ( doBimaTest ) 
          delayEngine.testAgainstBima();

     }
    }

    if ( doUvwTest ) { 
	// only write to stdout if uvwtest=t.
        cout << output.str();
    }

    CPTRACE(carma::util::Trace::TRACE7, "DelayEngineTestHarness: finished.");
    }  catch(ErrorException& err)  {
        cerr << "### EXCEPTION - " << err.getErrorMessage() << endl;
        status = EXIT_FAILURE;
    } catch ( out_of_range& oorex) {
	cerr << "### EXCEPTION - " << oorex.what() << endl;
        status = EXIT_FAILURE;
    } catch ( std::exception& stdex) {
	cerr << "### EXCEPTION - " << stdex.what() << endl;
        status = EXIT_FAILURE;
    } catch (... ) {
	cerr << "### EXCEPTION - unidentified exception" << endl;
        status = EXIT_FAILURE;
    }
    CPTRACE(carma::util::Trace::TRACE7, 
	    "DelayEngineTestHarness: returning "<<status);

    return status;
}


