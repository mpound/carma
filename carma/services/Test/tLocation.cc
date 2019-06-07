/**
 * $Id: tLocation.cc,v 1.5 2006/04/29 16:43:34 mpound Exp $
 *
 * @usage exercise the Location and Pad classes, including Observatory.cat 
 * catalog
 * @description
 *     Test the Observatory.cat entries lookup. Returns values in radians
 *     though, and the altitude in meters
 *
 * @key  observatory      carma    s       Name of the observatory to match
// @logger DEFAULT_FACILITY carma.services.Test.tLocation
 *
 */

#include "carma/services/Location.h"
#include "carma/services/Observatory.h"
#include "carma/services/Pad.h"
#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"
#include "carma/util/NotFoundException.h"

using namespace carma::services;
using namespace std;

int carma::util::Program::main() {
  const std::string obsName = getStringParameter("observatory");

  if ( obsName.empty( ) ) {
    cout << "no defined observatory name!" << endl;
    return EXIT_FAILURE;
  }
  try {

    Location loc(obsName);
    cout << 
      "Matched " << 
      obsName << " : " << 
      " lon=" << loc.getLongitude() <<
      " lat=" << loc.getLatitude() <<
      " alt=" << loc.getAltitude() << endl;
    Pad pad(obsName,"pad#39");
    cout << pad.toString() << endl;

    Observatory obs(obsName);
    cout << obs.toString() << endl;
  } catch ( carma::util::NotFoundException & notf) {
    cout << "Caught NotFoundException:"
	 << notf.what()
         << endl;
    return EXIT_FAILURE;
  } catch ( carma::util::ErrorException & err) {
    cout << "Caught CARMA Exception:"
	 << err.what()
         << endl;
    return EXIT_FAILURE;
  } catch ( std::exception& ex) {
    cout << "Some error, possibly a bad observatory name or catalog:" 
	 << ex.what()
         << endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
