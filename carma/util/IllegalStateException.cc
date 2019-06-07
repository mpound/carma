/**
 * Implementation for the IllegalStateException class.
 *
 * @author: Ray Plante
 *
 * $Id: IllegalStateException.cc,v 1.4 2006/10/09 17:54:06 tcosta Exp $
 * $CarmaCopyright$
 *
 */

#include "carma/util/IllegalStateException.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


IllegalStateException::IllegalStateException(
    const IllegalStateException & rhs ) :
ErrorException( rhs )
{
}


IllegalStateException::IllegalStateException(
    const ostringstream & oss, 
    const char * const    filename,
    const int             lineNo ) :
ErrorException( oss, filename, lineNo ) 
{
}


IllegalStateException::IllegalStateException(
    const string &     msg,
    const char * const filename,
    const int          lineNo ) :
ErrorException( msg, filename, lineNo )
{
}
