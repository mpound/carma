/**
 * CheckCatalog - check a source catalog for bad values.
 *
 * @author Marc Pound
 * @todo takes 30 mins on a 10,000 source catalog (e.g. opticalGuideStars.cat)
 *       this appears to be linear in the size of the catalog
 *       (around 650 sources/sec to do the first 100)
 *       needs an option to be silent on OK catalogs, 
 *       needs rewrite logic to continue after one failed catalog
 * $Id: checkcat.cc,v 1.8 2013/02/04 17:35:27 teuben Exp $
 * $CarmaCopyright$
 */

#include "carma/util/Program.h"
#include "carma/util/StringUtils.h"
#include "carma/services/Ephemeris.h"
#include "carma/services/EphemerisException.h"
#include "carma/services/SourceCatalog.h"
#include "carma/services/SourceChecker.h"
#include "carma/services/stringConstants.h"
#include "carma/services/UnsupportedCoordSysException.h"
#include "carma/services/Velocity.h"
#include "carma/util/FileNotFoundException.h"
#include "carma/util/NotFoundException.h"
//#include "carma/util/NumberFormatException.h"
#include "carma/util/IllegalArgumentException.h"

#include <vector>
#include <sstream>
//
// @version	$Revision: 1.8 $ $Date: 2013/02/04 17:35:27 $
//
// @usage  catalog verifier
//
// @description
//    Check a catalog for bad values.
// @key   catalog   @mandatory s  Catalog name to check.
//                                Multiple catalogs, separated by commas or spaces, 
//                                are allowed.  Note that the program will exit
//                                when the first bad catalog is encountered.
// @key   verbose   f          b  Show source names and RADIO+LSR velocities (km/s) 
//                                as processing the catalog(s). This is a good way
//                                to double check your conversions, as CARMA is using
//                                RADIO+LSR as the internally used doppler velocities
//                                as of February 2013.
// @key   maxsource 0          i  Maximum number of sources to process. By
//                                default all will be processed.
//
// @logger DEFAULT_FACILITY carma.services.checkcat

using namespace std;
using namespace carma::services;
using namespace carma::util;

// this routine was stolen from Table.cc so some refactoring would be in place
void
Tokenize(const std::string& line, 
	 std::vector<std::string>& tokens, 
	 const string& delimiters)
{
  string::size_type lastPos = line.find_first_not_of(delimiters, 0);   // skip first del's
  string::size_type pos     = line.find_first_of(delimiters, lastPos); // find first non-del

  while (string::npos != pos || string::npos != lastPos) {  // loop while more token found
    tokens.push_back(carma::util::StringUtils::trimWhiteSpace(line.substr(lastPos, pos - lastPos)));  // found one, add it
    lastPos = line.find_first_not_of(delimiters, pos);      // skip del's
    pos = line.find_first_of(delimiters, lastPos);          // find next non-del
  }
}

void badCatalog() {
    cerr << endl 
         << "*************************************************"
	 << endl
	 << " Your catalog has a problem. Below is the error"
	 << endl
	 << " message from the catalog reader.  Look carefully"
	 << endl
	 << " at the message to figure out the problem."
	 << endl
         << "*************************************************"
	 << endl;
}

int carma::util::Program::main()
{
  string srcName;
  string catalogName;
  bool verbose  = getBoolParameter("verbose");
  int  maxsource = getIntParameter("maxsource");
  int ns;
  try {
      const string catalog    = getStringParameter("catalog");
      std::vector<std::string> catalogs; 

      if (catalog.size() > 0) {
        Tokenize(catalog,catalogs,", ");
      }

      Location loc(CARMA_OBSERVATORY);
      double mjd = carma::util::Time::MJD();           

      SourceChecker checker;
      checker.setLocation(loc);
      checker.setElevLimit(10);
      checker.setFrequency(100e9);
      checker.setMJD(mjd);

      // if catalog= contained multiple files, check them all
      vector<string>::iterator ci = catalogs.begin();
      vector<string>::iterator ce = catalogs.end();
      while ( ci != ce ) {
        // this will do basic parsing of the catalog,
        // e.g. missing : or missing columns, bad velframe, etc.
        catalogName = *ci;
        SourceCatalog sc;

        cout << catalogName << " ";
        sc.open( catalogName );

        // loop over all sources in the catalog
        // to verify each one.
        SourceIterator si = sc.catalogBegin();
        SourceIterator se = sc.catalogEnd();
	ns = 0;
        while ( si != se ) {
            srcName = si->second.getName();

            checker.setSource( srcName, *ci );
            // this will catch errors in ra/dec values.
            const string foo = checker.info();
            // this will catch errors in VelFrame/Veldef
            const float kms = si->second.getVelocity().kms();
	    if (verbose) cout << "  " << ns << " " << srcName << " " << kms << endl;
            ++si;
	    ++ns;
	    if (maxsource>0 && maxsource==ns) break;
        }
        cout << ns << " sources [OK] " <<  endl;
        ++ci;
      }
  } catch (const UnsupportedCoordSysException & ucse) {
      badCatalog();
    cerr << "### Unsupported coordinate system on source "
         << srcName << " in catalog " << catalogName << " : "
         << ucse.getMessage() << endl;
    return EXIT_FAILURE;
  } catch (const EphemerisException& ee) {
      badCatalog();
    cerr << "### Ephemeris exception on source " 
         << srcName << " in catalog " << catalogName << " : "
         << ee.getMessage() << endl;
    return EXIT_FAILURE;
  } catch (const FileNotFoundException& fnfe) {
      badCatalog();
    cerr << "### FileNotFound exception: "
         << fnfe.getMessage() << endl;
    return EXIT_FAILURE;
  } catch (const NotFoundException & nfe) {
      badCatalog();
    cerr << "### NotFound exception";
    if (! srcName.empty() ) 
        cerr << " on source " << srcName;
    if (! catalogName.empty() ) 
        cerr << " in catalog " << catalogName ;
    cerr  << " : " << nfe.getMessage() << endl;
    return EXIT_FAILURE;
  } catch (const IllegalArgumentException & ille) {
      badCatalog();
    cerr << "### IllegalArgument exception";
    if (! srcName.empty() ) 
        cerr << " on source " << srcName;
    if (! catalogName.empty() ) 
        cerr << " in catalog " << catalogName ;
    cerr  << " : " << ille.getMessage() << endl;
    return EXIT_FAILURE;
  } catch (const BaseException & be) {
      badCatalog();
    cerr << "### Program exception";
    if (! srcName.empty() ) 
        cerr << " on source " << srcName;
    if (! catalogName.empty() )
        cerr << " in catalog " << catalogName ;
    cerr  << " : " << be.getMessage() << endl;
    return EXIT_FAILURE;
  } catch (...) {
      badCatalog();
    cerr << "### Program exception: an unspecified error." << endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
