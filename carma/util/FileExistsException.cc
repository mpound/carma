/**
 * Implementation for the FileExistsException class.
 *
 * @author: Marc Pound
 *
 * $Id: FileExistsException.cc,v 1.2 2005/02/15 22:57:07 tcosta Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/util/FileExistsException.h"

using namespace std;
using namespace carma::util;

FileExistsException::FileExistsException(const FileExistsException & ex) : 
    ErrorException(ex)
{}

FileExistsException::FileExistsException(const ostringstream & str,
                                         const char *          f,
                                         int                   l) :
    ErrorException(str, f, l) 
{}

FileExistsException::FileExistsException(const string & str,
                                         const char *   f,
                                         int            l) :
    ErrorException(str, f, l) 
{}

