#include <boost/thread.hpp>
#include <iomanip>
#include <ios>
#include <sys/time.h>
#include "carma/control/HalfSecUpdater.h"
#include "carma/control/SubarrayControlImpl.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/loggingUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/Trace.h"

using namespace ::std;
using namespace carma;
using namespace carma::control;
using namespace carma::util;
using namespace log4cpp;

namespace { // anonymous

  const Trace::TraceLevel kTraceLevel = Trace::TRACE1;

  const string halfSecUpdaterName= "HalfSecUpdater";

  string
  getInitialNdcString( const SubarrayControlImpl & saCI ) {
      return saCI.getAlphanumericName() + " half second updater thread";
  }
}

HalfSecUpdater::HalfSecUpdater( SubarrayControlImpl & saCI ) :
saCI_( saCI ),
timer_( 100, 1, true )
{
    timer_.ResetNextFireTime(0);
    CARMA_CPTRACE(kTraceLevel, "Constructed HalfSecUpdater");
}

HalfSecUpdater::~HalfSecUpdater()
try {
    CARMA_CPTRACE(kTraceLevel, "Destructed HalfSecUpdater");
} catch ( ... ) {
    // Just stifle any exception

    return;
}

void
HalfSecUpdater::operator( ) ( )
try {

    while ( true ) {
        try {
            // will pass current Time::MJD() for UVW monitor points interp,
            // which should be close enough.
            saCI_.updateHalfSecMonitorPoints();
        } catch ( ... ) {
            try {
                programLogErrorIfPossible(
                        "HalfSecUpdater - Caught exception syncing and/or calling saCI_.updateHalfSecMonitorPoints(): " +
                        getStringForCaught() );
            } catch ( ... ) {
                // Just stifle any exception
            }
        }

        // wait one frame.
        boost::this_thread::interruption_point();
        timer_.ResetNextFireTimeAndWait( 0 );
        boost::this_thread::interruption_point();
    } // while ( true )
} catch ( ... ) {
    programLogErrorIfPossible(
        "Exiting HalfSecUpdater::action on an exception: " +
        getStringForCaught() );
        
    throw;
}

