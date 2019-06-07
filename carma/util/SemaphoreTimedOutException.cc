/**
 * Implementation for the SemaphoreTimedOutException class.
 *
 * @author: Colby Gutierrez-Kraybill
 *
 * $Id: SemaphoreTimedOutException.cc,v 1.1 2007/11/28 07:00:59 colby Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/util/SemaphoreTimedOutException.h"

using namespace std;
using namespace carma::util;

SemaphoreTimedOutException::SemaphoreTimedOutException(const SemaphoreTimedOutException & ex) : 
    ErrorException(ex)
{}

SemaphoreTimedOutException::SemaphoreTimedOutException(const ostringstream & str,
                                         const char *          f,
                                         int                   l) :
    ErrorException(str, f, l) 
{}

SemaphoreTimedOutException::SemaphoreTimedOutException(const string & str,
                                         const char *   f,
                                         int            l) :
    ErrorException(str, f, l) 
{}

