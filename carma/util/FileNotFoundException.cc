/**
 * Implementation for the FileNotFoundException class.
 *
 * @author: Marc Pound
 *
 * $Id: FileNotFoundException.cc,v 1.2 2005/11/01 18:45:54 teuben Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/util/FileNotFoundException.h"

using namespace std;
using namespace carma::util;

FileNotFoundException::FileNotFoundException(const FileNotFoundException & ex) : 
    NotFoundException(ex)
{}

FileNotFoundException::FileNotFoundException(const ostringstream & str,
                                         const char *          f,
                                         int                   l) :
    NotFoundException(str, f, l) 
{}

FileNotFoundException::FileNotFoundException(const string & str,
                                         const char *   f,
                                         int            l) :
    NotFoundException(str, f, l) 
{}

