/**
 * Implementation for the SpectralLineNotFoundException class.
 *
 * @author: Marc Pound
 *
 * $Id: SpectralLineNotFoundException.cc,v 1.2 2005/02/15 23:32:33 tcosta Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/services/CatalogEntryNotFoundException.h"
#include "carma/services/SpectralLineNotFoundException.h"

using namespace std;
using namespace carma::services;

SpectralLineNotFoundException::SpectralLineNotFoundException(
    const SpectralLineNotFoundException & ex) 
    : carma::services::CatalogEntryNotFoundException(ex)
{ 
    // nothing else to do here 
}

SpectralLineNotFoundException::SpectralLineNotFoundException(
    const ostringstream & str,
    const char *          f,
    int                   l) :
    carma::services::CatalogEntryNotFoundException(str, f, l)
{ 
    // nothing else to do here 
}

SpectralLineNotFoundException::SpectralLineNotFoundException(
    const string & str,
    const char *   f,
    int            l) :
    carma::services::CatalogEntryNotFoundException(str, f, l)
{ 
    // nothing else to do here 
}

