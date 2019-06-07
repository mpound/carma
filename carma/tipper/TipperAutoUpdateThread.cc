
#include "carma/util/programLogging.h"
#include "carma/util/Trace.h"
#include "carma/tipper/TipperAutoUpdateThread.h"

using namespace carma::util;
using namespace carma::tipper;
using namespace log4cpp;

TipperAutoUpdateThread::TipperAutoUpdateThread( TipperControlThread &tipperControl, int update ) :
  _log( Program::getLogger() ),
  _tipperControl( tipperControl ),
  _update( update )
{
}

void TipperAutoUpdateThread::thread( TipperAutoUpdateThread &This )
{
  while (true)
  {
    // Try to keep running!  But throttle so that if there's a
    // recurring problem, it doesn't drag down the whole system
    sleep(20);

    try
    {
      programLogInfoIfPossible( "Calling TipperAutoUpdateThread::run" );

      This.run();

      programLogInfoIfPossible( "TipperAutoUpdateThread::run completed" );

    } catch ( ... ) {

      programLogErrorIfPossible(
	  "Coming out of TipperAutoUpdateThread::thread on an exception: " +
	  getStringForCaught() );
      // Just stifle the exception
    }

  }

}

void TipperAutoUpdateThread::run()
{
  while (true )
  {
    CPTRACE( Trace::TRACE1, "Auto Update calling doTip()" );
    _log << Priority::INFO << "Auto Update calling doTip()";
    _tipperControl.doTip();
    sleep(_update);
  }
}

