/**
 * Implementation for the EOFException class.
 *
 * @author: Marc Pound
 *
 * $Id: EOFException.cc,v 1.2 2005/02/15 22:57:07 tcosta Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/util/EOFException.h"

using namespace std;
using namespace carma::util;

EOFException::EOFException(const EOFException & ex) : 
    ErrorException(ex)
{}

EOFException::EOFException(const ostringstream & str,
                           const char *          f,
                           int                   l) :
    ErrorException(str, f, l) 
{}

EOFException::EOFException(const string & str,
                           const char *   f,
                           int            l) :
    ErrorException(str, f, l) 
{}

