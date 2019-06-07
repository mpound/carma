/**
 * @file
 * $Id: OpticalCatalog.cc,v 1.1 2008/11/24 14:22:06 mpound Exp $
 *
 * @author Marc Pound
 */

#include "carma/services/OpticalCatalog.h"
#include "carma/services/SourceNotFoundException.h"
#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"
#include "carma/util/StringUtils.h"
#include <iostream>

using namespace std;
using namespace carma::services;
using namespace carma::util;

const string OpticalCatalog::DEFAULT_CATALOG = "catalogs/BrightStars.cat";

OpticalCatalog::OpticalCatalog() { }

OpticalCatalog::~OpticalCatalog() { }

void
OpticalCatalog::open(const std::string& fileName) {
  Catalog::open(fileName);

  vector<string> sourceName;
  vector<double> vMagnitude;

  try {
    sourceName = catalogTable_.getColumn("Source");
    vMagnitude = catalogTable_.getDoubleColumn("V");
  } catch (const carma::util::ErrorException &ex) {
      programLogError( ex.getErrorMessage() );
      throw;
  }

  int nEntries = sourceName.size();

  Star star;
  for (int i = 0; i < nEntries; i++) {
    star.setName( sourceName[i] );
    star.setMagnitude( vMagnitude[i] );
    stars_.insert( make_pair( sourceName[i], star ) );
  }

}
    
const Star &
OpticalCatalog::lookup(const string & sourceName) 
{
    string sourceUC = StringUtils::lowASCIIAlphaNumericToUpper( sourceName );
    map< string, Star >::const_iterator p = stars_.find( sourceUC );
    if ( p == stars_.end() ) {
	ostringstream os;
	os << " Source " << sourceUC 
	   << " not found in catalog " << fileName_;
	throw CARMA_EXCEPTION(SourceNotFoundException, os.str().c_str());

    }
	
    return p->second;
}

