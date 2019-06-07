/**
 * Implementation for the NotFoundException class.
 *
 * @author: Ray Plante
 *
 * $Id: NotFoundException.cc,v 1.5 2005/02/15 22:57:07 tcosta Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/util/NotFoundException.h"

using namespace std;
using namespace carma::util;

NotFoundException::NotFoundException(const NotFoundException & ex) : 
    ErrorException(ex)
{}

NotFoundException::NotFoundException(const ostringstream & str,
                                     const char *          f,
                                     int                   l) :
    ErrorException(str, f, l) 
{}

NotFoundException::NotFoundException(const string & str,
                                     const char *   f,
                                     int            l) :
    ErrorException(str, f, l) 
{}

