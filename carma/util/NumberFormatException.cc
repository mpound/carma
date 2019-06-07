/**
 * Implementation for the NumberFormatException class.
 *
 * @author: Marc Pound
 *
 * $Id: NumberFormatException.cc,v 1.1 2009/05/27 00:19:01 mpound Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/util/NumberFormatException.h"

using namespace std;
using namespace carma::util;

NumberFormatException::NumberFormatException(const NumberFormatException & ex) : 
    ErrorException(ex)
{}

NumberFormatException::NumberFormatException(const ostringstream & str,
                                         const char *          f,
                                         int                   l) :
    ErrorException(str, f, l) 
{}

NumberFormatException::NumberFormatException(const string & str,
                                         const char *   f,
                                         int            l) :
    ErrorException(str, f, l) 
{}

