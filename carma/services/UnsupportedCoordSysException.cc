
/**
 * Implementation for the UnsupportedCoordSysException class.
 *
 * @author: Marc Pound
 *
 * $Id: UnsupportedCoordSysException.cc,v 1.1 2005/07/22 05:17:29 mpound Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/services/UnsupportedCoordSysException.h"

using namespace std;
using namespace carma::services;

UnsupportedCoordSysException::UnsupportedCoordSysException
(const UnsupportedCoordSysException & ex) 
    : carma::util::ErrorException(ex)
{ 
    // nothing else to do here 
}

UnsupportedCoordSysException::UnsupportedCoordSysException
(const ostringstream & str, const char *f, int l) 
    : carma::util::ErrorException(str, f, l) 
{ 
    // nothing else to do here 
}

UnsupportedCoordSysException::UnsupportedCoordSysException
(const string & str, const char *f, int l) 
    : carma::util::ErrorException(str, f, l) 
{ 
    // nothing else to do here 
}

