/**
 * Implementation for the MPMLException class.
 *
 * @author Original: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */
#include "carma/dbms/MPMLException.h"

using namespace std;
using namespace carma::dbms;

MPMLException::MPMLException(const MPMLException & ex) : 
    ErrorException(ex)
{}

MPMLException::MPMLException(const ostringstream & str,
                             const char *          f,
                             int                   l) :
    ErrorException(str, f, l) 
{}

MPMLException::MPMLException(const string & str,
                             const char *   f,
                             int            l) :
    ErrorException(str, f, l) 
{}

