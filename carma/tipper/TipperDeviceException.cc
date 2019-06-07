/**
 * Implementation for the TipperDeviceException class.
 *
 * @author: Colby Gutierrez-Kraybill
 *
 * $Id: TipperDeviceException.cc,v 1.1 2006/11/28 23:00:58 colby Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/tipper/TipperDeviceException.h"

using namespace std;
using namespace carma::util;
using namespace carma::tipper;

TipperDeviceException::TipperDeviceException(
    const TipperDeviceException & ex) : 
  ErrorException(ex)
{}

TipperDeviceException::TipperDeviceException(
    const ostringstream & str,
    const char *          f,
    int                   l) :
  ErrorException(str, f, l) 
{}

TipperDeviceException::TipperDeviceException(const string & str,
    const char *   f,
    int            l) :
  ErrorException(str, f, l) 
{}

