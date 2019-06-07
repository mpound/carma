/**
 * $Id: tOpticalCatalog.cc,v 1.1 2008/11/24 14:22:06 mpound Exp $
 *
 * @usage tOpticalcatalog=conf/catalogs/BrightStars.cat source=sirius
 * @description
 * Test OpticalCatalog functionality, look up sources by name
 *
 * @key catalog conf/catalogs/BrightStars.cat s File containing stellar info
 * @key source      sirius  s name of star
 *
 * @logger DEFAULT_FACILITY carma.services.Test.tOpticalCatalog
 *
 */

#include "carma/services/OpticalCatalog.h"
#include "carma/services/Star.h"
#include "carma/services/SourceNotFoundException.h"
#include "carma/util/FileNotFoundException.h"
#include "carma/util/Program.h"

using namespace std;
using namespace carma;
using namespace carma::util;
using namespace carma::services;

int Program::main() {

  const string fileName   = getStringParameter("catalog");
  const string sourceName = getStringParameter("source");

  try {

    OpticalCatalog fc;
    fc.open(fileName);

    Star fs = fc.lookup( sourceName );
    cout << "Star "
	 << fs.getName() 
	 << " , Vmag =  "
	 << fs.getMagnitude()
	 << endl;


  } catch (const SourceNotFoundException &ex) {
    cout << ex.getMessage() << endl;
    return EXIT_FAILURE;
  } catch (const util::FileNotFoundException &ex) {
    cout << "Problems opening " << fileName << endl;
    cout << ex.getMessage() << endl;
    return EXIT_FAILURE;
  } catch (const util::ErrorException &ex) {
    cout << ex.getMessage() << endl;
    return EXIT_FAILURE;
  } catch (const util::BaseException &ex) {
    cout << ex.getMessage() << endl;
    return EXIT_FAILURE;
  } catch ( ... ) {
    cout << " Unclassified exception" << endl;
    return EXIT_FAILURE;
  }

  try {
      // bogus lookup, should throw SourceNotFound
    OpticalCatalog fc;
    fc.open(fileName);

    Star fs = fc.lookup( "bogus" );
  } catch (const SourceNotFoundException &ex) {
    cout << "Correctly caught exception on bogus source." << endl;
  } catch ( ... ) {
    cout << " Unclassified exception on bogus lookup" << endl;
    return EXIT_FAILURE;
  }

    
  return EXIT_SUCCESS;
}
