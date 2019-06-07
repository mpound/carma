/**
 * @file StopWatch implementation
 * @author Dave Mehringer
 * @version $Id: StopWatch.cc,v 1.6 2006/10/04 04:49:31 tcosta Exp $
 * $CarmaCopyright$
 */

#include <string>
#include <ctime>
#include "carma/util/StopWatch.h"
#include "carma/util/IllegalStateException.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


const StopWatch::ClockType kDefaultType = StopWatch::WALL_CLOCK;


} // namespace < anonymous >


StopWatch::StopWatch( ) :
type_( kDefaultType ),
name_(),
startTime_( 0 ),
elapsedTime_( 0 ),
cumulativeElapsedTime_( 0 ),
running_( false )
{
}


StopWatch::StopWatch( const ClockType type ) :
type_( type ),
name_(),
startTime_( 0 ),
elapsedTime_( 0 ),
cumulativeElapsedTime_( 0 ),
running_( false )
{
}


StopWatch::StopWatch( const string & name ) :
type_( kDefaultType ),
name_( name ),
startTime_( 0 ),
elapsedTime_( 0 ),
cumulativeElapsedTime_( 0 ),
running_( false )
{
}


StopWatch::StopWatch( const ClockType type,
                      const string &  name ) :
type_( type ),
name_( name ),
startTime_( 0 ),
elapsedTime_( 0 ),
cumulativeElapsedTime_( 0 ),
running_( false )
{
}


StopWatch::~StopWatch()
{
}


void
StopWatch::start( )
{
    if ( running_ ) {
        string emsg;
        
        if ( name_.empty() == false ) {
            emsg += name_;
            emsg += ": ";
        }
        
        emsg += "Stop watch is already running, stop it first.";
            
        throw CARMA_EXCEPTION(IllegalStateException, emsg);
    }
    
    elapsedTime_ = 0;
    running_ = true;
    switch (type_) {
    case WALL_CLOCK:
      clock_gettime(CLOCK_REALTIME, &scratchTimeSpec_);
      startTime_ = scratchTimeSpec_.tv_sec + 1e-9*scratchTimeSpec_.tv_nsec;
      break;
    case CPU_TIME:
      startTime_ = static_cast<double>(clock());
      break;
    };
}


void
StopWatch::stop( )
{
    if ( !running_ ) {
        string emsg;
        
        if ( name_.empty() == false ) {
            emsg += name_;
            emsg += ": ";
        }

        emsg += "Stop watch is not running, start it first.";

        throw CARMA_EXCEPTION(IllegalStateException, emsg);
    }
    switch (type_) {
    case WALL_CLOCK:
      clock_gettime(CLOCK_REALTIME, &scratchTimeSpec_);
      elapsedTime_ = scratchTimeSpec_.tv_sec + 1e-9*scratchTimeSpec_.tv_nsec 
        - startTime_;
      break;
    case CPU_TIME:
      elapsedTime_ = (static_cast<double>(clock()) - startTime_) / 
	CLOCKS_PER_SEC;
      break;
    };
    cumulativeElapsedTime_ += elapsedTime_;
    running_ = false;
}


bool
StopWatch::isRunning( ) const
{
    return running_;
}


double
StopWatch::getElapsedTime( ) const
{
    return elapsedTime_;
}


double
StopWatch::getCumulativeElapsedTime( const bool reset )
{
    const double retval = cumulativeElapsedTime_;

    if ( reset )
      cumulativeElapsedTime_ = 0;

    return retval;
}

