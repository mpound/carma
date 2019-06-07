/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Kim Drongesen, ported by: Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.4 $
 * $Date: 2011/09/01 18:19:13 $
 * $Id: TipperControlImpl.cc,v 1.4 2011/09/01 18:19:13 abeard Exp $
 */

#include <string>

#include <errno.h>
// for strerror
#include <string.h>

#include <math.h>

// CARMA includes
#include "carma/tipper/TipperControlImpl.h"
#include "carma/tipper/TipperControlThread.h"
#include "carma/tipper/TipperDeviceException.h"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::tipper;


TipperControlImpl::TipperControlImpl(
    TipperControlThread &tipperControlThread ) :
  _logger( Program::getLogger() ),
  _tipperControlThread( tipperControlThread )
{ 
  CPTRACE( Trace::TRACE3, "   TipperControlImpl()" );

}

TipperControlImpl::~TipperControlImpl()
{
    CPTRACE( Trace::TRACE4, "    TipperControlImpl : d'tor" );
}

void TipperControlImpl::doTip()
{
  string logmsg = "doTip() called DO call invoked";
  CPTRACE( Trace::TRACE1, logmsg );
  _logger << Priority::INFO << logmsg;

  // by design, no exception should be thrown here
  try
  {
    _tipperControlThread.doTip();
  }
  catch ( ... )
  {
    logCaughtAsErrorAndRethrowAsUser( _logger );
  }
}
