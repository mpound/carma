/**
 * Implementation for the SemaphoreException class.
 *
 * @author: Colby Gutierrez-Kraybill
 *
 * $Id: SemaphoreException.cc,v 1.1 2007/11/28 07:00:59 colby Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/util/SemaphoreException.h"

using namespace std;
using namespace carma::util;

SemaphoreException::SemaphoreException(const SemaphoreException & ex) : 
    ErrorException(ex)
{}

SemaphoreException::SemaphoreException(const ostringstream & str,
                                         const char *          f,
                                         int                   l) :
    ErrorException(str, f, l) 
{}

SemaphoreException::SemaphoreException(const string & str,
                                         const char *   f,
                                         int            l) :
    ErrorException(str, f, l) 
{}

