/**
 * Implementation for the ConformabilityException class.
 *
 * @author: Marc Pound
 *
 * $Id: ConformabilityException.cc,v 1.2 2005/02/16 01:01:33 tcosta Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/services/ConformabilityException.h"

using namespace std;
using namespace carma::services;

ConformabilityException::ConformabilityException(
    const ConformabilityException & ex) : carma::util::ErrorException(ex)
{ 
    // nothing else to do here 
}

ConformabilityException::ConformabilityException(const ostringstream & str, 
                                                 const char *          f,
                                                 int                   l) :
    carma::util::ErrorException(str, f, l) 
{ 
    // nothing else to do here 
}

ConformabilityException::ConformabilityException(const string & str, 
                                                 const char *   f,
                                                 int            l) :
    carma::util::ErrorException(str, f, l) 
{ 
    // nothing else to do here 
}

