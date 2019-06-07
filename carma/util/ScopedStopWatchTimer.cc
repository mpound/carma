#include "carma/util/ScopedStopWatchTimer.h"
#include "carma/util/StopWatch.h"

using namespace carma::util;

ScopedStopWatchTimer::ScopedStopWatchTimer( StopWatch & stopwatch ) :
    stopWatch_( stopwatch ) 
{
    stopWatch_.start();
}

ScopedStopWatchTimer::~ScopedStopWatchTimer( )
{
    stopWatch_.stop();
}

