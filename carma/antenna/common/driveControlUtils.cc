#include "carma/antenna/common/driveControlUtils.h"

#include <iostream>
#include <iomanip>
#include <sstream>

using namespace std;
using namespace carma::antenna::common;

string
carma::antenna::common::raDecEpochToString( 
    const DriveControl::RaDecEpoch & epoch )
{
    ostringstream oss;
    oss << "( mjd=" << fixed << setprecision( 5 ) << epoch.mjd 
        << ", ra=" << setprecision( 7 ) << epoch.ra 
        << ", dec=" << setprecision( 7 ) << epoch.dec << " )";
    return oss.str();
}

string
carma::antenna::common::raDecTripletToString(
    const DriveControl::RaDecTriplet & triplet )
{
    const CORBA::Long count = triplet.length( );
    string asString = "[ ";
    for ( CORBA::Long  i = 0; i < count; ++i ) {
        asString += raDecEpochToString( triplet[i] ) + " ";
    }
    asString += "]";
    return asString;
}

