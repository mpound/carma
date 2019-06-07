/**
 * Implementation for the IllegalArgumentException class.
 *
 * @author: Ray Plante
 *
 * $Id: IllegalArgumentException.cc,v 1.4 2006/10/09 17:54:06 tcosta Exp $
 * $CarmaCopyright$
 *
 */
 
#include "carma/util/IllegalArgumentException.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


IllegalArgumentException::IllegalArgumentException(
    const IllegalArgumentException & rhs ) :
ErrorException( rhs )
{
}


IllegalArgumentException::IllegalArgumentException(
    const ostringstream & oss, 
    const char * const    filename,
    const int             lineNo ) :
ErrorException( oss, filename, lineNo )
{
}


IllegalArgumentException::IllegalArgumentException(
    const string &     msg,
    const char * const filename,
    int                lineNo ) :
ErrorException( msg, filename, lineNo )
{
}
