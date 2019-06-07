#include "carma/services/padUtils.h"
#include "carma/services/AntennaCoordinates.h"
#include "carma/services/Length.h"
#include "carma/services/Location.h"
#include "carma/services/Pad.h"
#include "carma/services/Vector.h"

using namespace carma::services;
using namespace std;

Location 
carma::services::adjustedLocation(
	Pad pad, Length east, Length north, Length up)
{
    /*
     * I think this does not work because it
     * is a rotation about the pad location rather
     * than about the array reference point.
     *
    return carma::services::adjustedLocation( 
	    pad.getLocation(), east, north, up );
    */
    vector<Length*> padEnu = pad.getEnu();
    double em = east.meters() + padEnu[0]->meters();
    double nm = north.meters() + padEnu[1]->meters();
    double um = up.meters() + padEnu[2]->meters();
    return carma::services::adjustedLocation( 
	    pad.getReference(), Length(em,"m"), Length(nm,"m"),Length(um,"m"));

}

Location 
carma::services::adjustedLocation(
	Location loc, Length east, Length north, Length up)
{
     AntennaCoordinates coord( loc );

     coord.add( up.meters(), east.meters(), north.meters() );
     return Location( coord.longitude(),
	              coord.latitude(),
		      coord.altitude()
	     );
}

std::vector<Length*> 
carma::services::convertBaseline(Pad pad, Length X, Length Y, Length Z)
{
    const Location refLoc = pad.getReference();
    const Location padLoc = pad.getLocation();
    AntennaCoordinates arrayRefPt( refLoc );
    // This is updated antenna UEN from the array reference position.
    Vector<double> antUen = arrayRefPt.topoXYZToUen(
				   X.meters(),Y.meters(),Z.meters(), 
				   refLoc.getLatitude()
			    );
    // This is the pad UEN from the array reference position.
    Vector<double> padUen = arrayRefPt.getUen(
				 padLoc.getLongitude(),
				 padLoc.getLatitude(),
				 padLoc.getAltitude()
	                    );
    // The difference is the new pad offsets.
    Vector<double> uenDiff = antUen - padUen;
    Length* east  = new Length(uenDiff[1],"meter");
    Length* north = new Length(uenDiff[2],"meter");
    Length* up    = new Length(uenDiff[0],"meter");
    vector<Length*> enu;
    enu.reserve(3);
    enu.push_back(east);
    enu.push_back(north);
    enu.push_back(up);
    return enu;
}

std::string 
carma::services::defaultPadName(const unsigned short padNo) 
{
    std::ostringstream padStr;
    padStr << "pad#" << padNo;
    string padName = padStr.str();
    return padName;
}
