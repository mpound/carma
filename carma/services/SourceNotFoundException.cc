/**
 * Implementation for the SourceNotFoundException class.
 *
 * @author: Marc Pound
 *
 * $Id: SourceNotFoundException.cc,v 1.2 2005/02/15 23:32:33 tcosta Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/services/CatalogEntryNotFoundException.h"
#include "carma/services/SourceNotFoundException.h"

using namespace std;
using namespace carma::services;

SourceNotFoundException::SourceNotFoundException(
    const SourceNotFoundException & ex) 
    : carma::services::CatalogEntryNotFoundException(ex)
{ 
    // nothing else to do here 
}

SourceNotFoundException::SourceNotFoundException(const ostringstream & str, 
                                                 const char *          f,
                                                 int                   l) :
    carma::services::CatalogEntryNotFoundException(str, f, l)
{ 
    // nothing else to do here 
}

SourceNotFoundException::SourceNotFoundException(const string & str, 
                                                 const char *   f,
                                                 int            l) :
    carma::services::CatalogEntryNotFoundException(str, f, l)
{ 
    // nothing else to do here 
}

