/**
 * Implementation for the MalformedSyslogLineException class.
 *
 * @author: Colby Gutierrez-Kraybill
 *
 * $Id: MalformedSyslogLineException.cc,v 1.1 2007/11/21 12:09:10 colby Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/dbms/MalformedSyslogLineException.h"

using namespace std;
using namespace carma::util;
using namespace carma::dbms;

MalformedSyslogLineException::MalformedSyslogLineException(const MalformedSyslogLineException & ex) : 
    ErrorException(ex)
{}

MalformedSyslogLineException::MalformedSyslogLineException(const ostringstream & str,
                                         const char *          f,
                                         int                   l) :
    ErrorException(str, f, l) 
{}

MalformedSyslogLineException::MalformedSyslogLineException(const string & str,
                                         const char *   f,
                                         int            l) :
    ErrorException(str, f, l) 
{}

