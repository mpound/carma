#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"
#include "carma/services/AntennaCoordinates.h"
#include "carma/services/Length.h"
#include "carma/services/Location.h"
#include "carma/services/Observatory.h"
#include "carma/services/Pad.h"
#include "carma/services/padUtils.h"
#include "carma/services/Physical.h"
#include "carma/services/stringConstants.h"
#include "carma/services/Table.h"
#include "carma/services/Vector.h"
#include <sstream>
#include <iomanip>
//
// @version	$Revision: 1.1 $ $Date: 2012/03/05 22:34:06 $
//
// @usage  Program for computing full antenna positions
//
// @description
//      Program to compute antenna positions using Observatory.cat
//      and an user-supplied file with pad and antenna offsets as
//      a function of array configuration.
//
// @key  antfile  @mandatory  s  Antenna/pad offset file
// @key  config   @mandatory  s  Configuration to use in antfile , e.g. C12A 
//
// @logger DEFAULT_FACILITY carma.services.checklocation
//
//

using namespace std;
using namespace carma::services;
using namespace carma::services::constants;
using namespace carma::util;

int carma::util::Program::main()
{
  try {
    const string antfile = getStringParameter("antfile");
    const string config  = getStringParameter("config");
    // default is CARMA which reads Observatory.cat
    Table t;
    t.open( antfile );
    vector<double> padOffE = t.getDoubleColumn("padOffsetEast");
    vector<double> padOffN = t.getDoubleColumn("padOffsetNorth");
    vector<double> padOffU= t.getDoubleColumn("padOffsetUp");
    //vector<double> axisMis = t.getDoubleColumn("axisMis");
    vector<double> height = t.getDoubleColumn("heightOffset");
    vector<string> confstr = t.getColumn("config");
    vector<int> padNo = t.getIntColumn("padNo");
    vector<int> antNo = t.getIntColumn("antNo");

    Observatory carma;
    AntennaCoordinates arrayRefPt = carma.getReferenceCoordinates();
    const string mm("mm");
    Vector<double> xyzref;
    Vector<double> uen1;
    Vector<double> xyz1;
    xyzref = arrayRefPt.getXyz(0.0, 0.0, 0.0, false);

    size_t len = antNo.size();
    cout << "ANT    Pad#       X(m)         Y(m)          Z(m)         X(ns)       Y(ns)       Z(ns)         E(m)         N(m)           U(m)        E(ns)       N(ns)        U(ns) " << endl;
    //        C1     38    -31.112939     87.490861     50.973867    -103.782     291.838     170.031     87.490861     59.424014      5.927613     291.838     198.217      19.772
    for ( size_t i=0; i < len; ++i) {
        // skip entries not in requested configuration
        if ( confstr.at(i).compare(config) != 0  ) continue;
        ostringstream p;
        p << "pad#"<<padNo.at(i);
        const string padStr = p.str();
        const Pad pad = carma.getPad(padStr);
        double totalUp = padOffU.at(i)+height.at(i);
        Location adjLoc = adjustedLocation( pad, 
                            Length(padOffE.at(i), mm),
                            Length(padOffN.at(i), mm),
                            Length(totalUp, mm)
                        );

        // UEN in meters
        AntennaCoordinates locCoord( adjLoc ) ;
        uen1 = arrayRefPt.getUen( locCoord );
        // XYZ in meters
        xyz1 = arrayRefPt.getXyz(uen1[0], uen1[1], uen1[2], false);

        const double x1 = ( xyz1[0] - xyzref[0] ) ;
        const double y1 = ( xyz1[1] - xyzref[1] ) ;
        const double z1 = ( xyz1[2] - xyzref[2] ) ;
        const double NS=Physical::NANOSEC_PER_METER;
        printf("C%0d  %5d  %12.6f  %12.6f  %12.6f  %10.3f  %10.3f  %10.3f  %12.6f  %12.6f  %12.6f  %10.3f  %10.3f  %10.3f\n",  antNo.at(i), padNo.at(i), x1,y1,z1, x1*NS,y1*NS,z1*NS, uen1[1], uen1[2], uen1[0], uen1[1]*NS, uen1[2]*NS,uen1[0]*NS );
                /*
        cout << "C"<<antNo.at(i) << "    "<<padNo.at(i) << "    "
             << setprecision(7) 
             << x1 << "   " << y1 << "   " << z1 
             << "   " x1*NS<< "   " << y1*NS<< "   " << z1*NS
             << "   " << uen1[1] << "   " << uen1[2] << "   " << uen1[0] 
             << "   " << uen1[1]*NS << "   " << uen1[2]*NS << "   " << uen1[0]*NS
             << endl;
             */
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
