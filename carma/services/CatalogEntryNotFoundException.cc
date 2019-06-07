/**
 * Implementation for the CatalogEntryNotFoundException class.
 *
 * @author: Marc Pound
 *
 * $Id: CatalogEntryNotFoundException.cc,v 1.3 2005/02/15 23:32:33 tcosta Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/services/CatalogEntryNotFoundException.h"

using namespace std;
using namespace carma::services;

CatalogEntryNotFoundException::CatalogEntryNotFoundException(
     const CatalogEntryNotFoundException & ex) 
   : carma::util::NotFoundException(ex)
{ 
    // nothing else to do here 
}

CatalogEntryNotFoundException::CatalogEntryNotFoundException(
      const ostringstream & str,
      const char *          f,
      int                   l) 
    : carma::util::NotFoundException(str, f, l) 
{ 
    // nothing else to do here 
}

CatalogEntryNotFoundException::CatalogEntryNotFoundException(
      const string & str,
      const char *   f,
      int            l) 
    : carma::util::NotFoundException(str, f, l) 
{ 
    // nothing else to do here 
}

