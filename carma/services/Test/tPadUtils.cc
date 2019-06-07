#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"
#include "carma/util/IllegalArgumentException.h"

#include "carma/services/AntennaCoordinates.h"
#include "carma/services/Length.h"
#include "carma/services/Location.h"
#include "carma/services/Pad.h"
#include "carma/services/padUtils.h"
#include "carma/services/Physical.h"
#include "carma/services/stringConstants.h"
#include "carma/services/Table.h"
#include "carma/services/Vector.h"


#include <sstream>
#include <iomanip>

//
// @version	$Revision: 1.3 $ $Date: 2008/01/22 17:34:48 $
//
// @usage  test program for Pad utility methods
//
// @description
//      Test program for Pad utility methods
//
// @key  pad  7 i Pad number
//
// @logger DEFAULT_FACILITY carma.services.Test.tPadUtils
//
//

using namespace std;
using namespace carma::services;
using namespace carma::services::constants;
using namespace carma::util;

int carma::util::Program::main()
{
    try {
	const int padNo = getIntParameter("pad");
	if ( padNo <= 0 ) {
	    throw CARMA_EXCEPTION( IllegalArgumentException, "Pad number must be greather than zero." );
	}
	// Jin's B array baseline solution for antenna 1 on pad 7.
	
	const double X = 383.5856;
        const double Y = -1580.0563;
	const double Z = -329.8614;
        const Length XL(X/Physical::NANOSEC_PER_METER,"meter");
	const Length YL(Y/Physical::NANOSEC_PER_METER,"meter");
	const Length ZL(Z/Physical::NANOSEC_PER_METER,"meter");

	ostringstream os ;
	os << "pad#"<<padNo;
	const string padStr = os.str();
	Pad pad( CARMA_OBSERVATORY, padStr );
	const vector< Length * > enu = convertBaseline(pad,XL,YL,ZL);
	// subtract off antenna offsets ( see subarrayinit.py )
	double east  = enu[0]->millimeters()  - 0.0 ;
	double north = enu[1]->millimeters()  - 0.0 ;
	double up    = enu[2]->millimeters()  - 5417.0 ;
	cout.setf(ios::fixed) ;
	cout << setprecision(4)
	    << "Pad   " << padNo << endl
	    << "East  " << east  << endl
	    << "North " << north << endl
	    << "Up    " << up    << endl
	    ;

        Vector<double> xyz1;
        Vector<double> xyz2;
        Vector<double> xyzref;
        Vector<double> uen1;
        Vector<double> uen2;
	AntennaCoordinates arrayRefPt( pad.getReference() );
	Location adjLoc = adjustedLocation(pad, *enu[0], *enu[1], *enu[2]);
	uen1 = arrayRefPt.getUen(
		adjLoc.getLongitude(),
		adjLoc.getLatitude(),
		adjLoc.getAltitude()
		);
	uen2 = arrayRefPt.getUen(
		pad.getLocation().getLongitude(),
		pad.getLocation().getLatitude(),
		pad.getLocation().getAltitude()
		);
	uen2[0] += (5.417+up/1000.);
	uen2[1] += (east/1000.);
	uen2[2] += (north/1000.);
	xyz1 = arrayRefPt.getXyz(uen1[0], uen1[1], uen1[2], false);
	xyz2 = arrayRefPt.getXyz(uen2[0], uen2[1], uen2[2], false);
	xyzref = arrayRefPt.getXyz(0.0, 0.0, 0.0, false);
	const double x1 = ( xyz1[0] - xyzref[0] ) * Physical::NANOSEC_PER_METER;
	const double y1 = ( xyz1[1] - xyzref[1] ) * Physical::NANOSEC_PER_METER;
	const double z1 = ( xyz1[2] - xyzref[2] ) * Physical::NANOSEC_PER_METER;
	const double x2 = ( xyz2[0] - xyzref[0] ) * Physical::NANOSEC_PER_METER;
	const double y2 = ( xyz2[1] - xyzref[1] ) * Physical::NANOSEC_PER_METER;
	const double z2 = ( xyz2[2] - xyzref[2] ) * Physical::NANOSEC_PER_METER;
	cout << setprecision(4)
	    << "X " << X << " , " << x1 << " , " << x2 << endl
	    << "Y " << Y << " , " << y1 << " , " << y2 << endl
	    << "Z " << Z << " , " << z1 << " , " << z2 << endl
	    ;

	const string padTable("carma/services/Test/padUtils.tab");
	Table t;
	t.open( padTable );
	std::vector<double> goodX = t.getDoubleColumn("X");
	std::vector<double> goodY = t.getDoubleColumn("Y");
	std::vector<double> goodZ = t.getDoubleColumn("Z");
	std::vector<double> goodE = t.getDoubleColumn("E");
	std::vector<double> goodN = t.getDoubleColumn("N");
	std::vector<double> goodU = t.getDoubleColumn("U");
	std::vector<double> aoffE = t.getDoubleColumn("AoffE");
	std::vector<double> aoffN = t.getDoubleColumn("AoffN");
	std::vector<double> aoffU = t.getDoubleColumn("AoffU");
	std::vector<int> padNumber = t.getIntColumn("padNo");
	unsigned int len = goodX.size();
	cout << "#  X    Y   Z    Xc   Yc   Zc   E   N   U   Ec  Nc  Uc " <<endl;
	for ( uint i = 0 ; i < len; i++ ) {
	    const Length Xi(goodX[i]/Physical::NANOSEC_PER_METER,"meter");
	    const Length Yi(goodY[i]/Physical::NANOSEC_PER_METER,"meter");
	    const Length Zi(goodZ[i]/Physical::NANOSEC_PER_METER,"meter");
	    ostringstream os ;
	    os << "pad#"<< padNumber[i];
	    const string padStr = os.str();
	    Pad padi( CARMA_OBSERVATORY, padStr );
	    const vector< Length * > enui = convertBaseline(padi,Xi,Yi,Zi);
	    Vector<double> xyz;
	    Vector<double> ueni;
	    Vector<double> uenref;
	    Location adjLoc = adjustedLocation(padi, *enui[0], *enui[1], *enui[2]);
	    ueni = arrayRefPt.getUen(
		    adjLoc.getLongitude(),
		    adjLoc.getLatitude(),
		    adjLoc.getAltitude()
		    );
	    uenref = arrayRefPt.getUen(
		    arrayRefPt.longitude(),
		    arrayRefPt.latitude(),
		    arrayRefPt.altitude()
		    );
	    xyz = arrayRefPt.getXyz(ueni[0], ueni[1], ueni[2], false);
	    Vector<double> newxyz = (xyz - xyzref);
	    newxyz = newxyz * Physical::NANOSEC_PER_METER;
	    ueni = ueni - uenref;
	    cout << setprecision(4)
		<< goodX[i] << "   " << newxyz[0] << "   "
		<< goodY[i] << "   " << newxyz[1] << "   "
		<< goodZ[i] << "   " << newxyz[2] << "   "
		<< setprecision(3) 
		<< goodE[i] << "   " << ueni[1] << "   "
		<< "   " << goodN[i] << "   " << ueni[2] << "   "
		<< "   " << goodU[i] << "   " << ueni[0] << endl
		;
	}


    } catch ( const ErrorException & ee ) {
      cerr << "Program::main : caught exception " << ee.getMessage() << endl;
      return EXIT_FAILURE;
    } catch (...) {
      cerr << "Program::main : caught unclassified exception " << endl;
      return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
