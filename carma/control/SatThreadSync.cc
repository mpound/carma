//!
//! $CarmaCopyright$
//!


#include "carma/control/SatThreadSync.h"
#include "carma/control/SubarrayTrackerThread.h"

using namespace ::std;
using namespace carma;
using namespace carma::control;


SubarrayControlImpl::TrackerThreadSync::TrackerThreadSync(
    SubarrayControlImpl & saCI ) :
scopedLock_( saCI.trackerThreadSyncMutex_ )
{

}

SubarrayControlImpl::TrackerThreadSync::~TrackerThreadSync( )
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}
