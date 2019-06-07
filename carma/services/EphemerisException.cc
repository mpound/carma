
/**
 * Implementation for the EphemerisException class.
 *
 * @author: Marc Pound
 *
 * $Id: EphemerisException.cc,v 1.1 2005/07/17 02:10:10 mpound Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/services/EphemerisException.h"

using namespace std;
using namespace carma::services;

EphemerisException::EphemerisException(
    const EphemerisException & ex) : carma::util::ErrorException(ex)
{ 
    // nothing else to do here 
}

EphemerisException::EphemerisException(const ostringstream & str, 
                                       const char *          f,
                                       int                   l) :
    carma::util::ErrorException(str, f, l) 
{ 
    // nothing else to do here 
}

EphemerisException::EphemerisException(const string & str, 
                                       const char *   f,
                                       int            l) :
    carma::util::ErrorException(str, f, l) 
{ 
    // nothing else to do here 
}

