/**
 * $Id: FrameIPQwriter.cc,v 1.19 2012/01/18 16:18:29 abeard Exp $ 
 * 
 * Frame timer that incorporates a timer thread.  
 * 
 * Author: N. S. Amarnath
 * 
 * Version: $Revision: 1.19 $ * $Date: 2012/01/18 16:18:29 $ 
 */


#include "carma/monitor/FrameIPQwriter.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <boost/thread.hpp>

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


namespace {

const size_t kWriteLogPeriod = 0;
    
double calculateSecondsAfterFrame( const double seconds ) 
{
    const int frames = static_cast< int >( seconds * 2.0 );
    return seconds - ( frames * 0.5 ); 
}


}  // namespace < anonymous >


// base level frame delay of 200 microseconds
const double nanosecondsPerSecond =
    static_cast< double >(1000.0 * 1000.0 * 1000.0);
    
const long carma::monitor::HALF_SECOND_IN_NANOSECONDS = 500000000;


// public

FrameIPQwriter::FrameIPQwriter( const string &     threadName, 
                                const double       delayInS ) :
timer_( static_cast< long >( 
        nanosecondsPerSecond * calculateSecondsAfterFrame( delayInS ) ), 
        1, true ),
writeCount_( 0 ),
nextWriteLogCount_( kWriteLogPeriod )
{
    // Quick and dirty initialization.  Actual fire time depends on when
    // start() gets called.
    nextTime_ = timer_.ResetNextFireTime( );
}


FrameIPQwriter::~FrameIPQwriter( )
try {
    // Nothing
} catch ( ... ) {
    // Just stifle any exception
}


// protected

double
FrameIPQwriter::getNextFireTime( ) const
{
    return Time::timespec2MJD( nextTime_ );
}

double
FrameIPQwriter::resetNextFireTime( const long delayFrames ) 
{
    nextTime_ = timer_.ResetNextFireTime( delayFrames );
    return Time::timespec2MJD( nextTime_ );
}

void
FrameIPQwriter::operator( ) ( )
try {
    
    const ScopedLogNdc ndc( "FrameIPQwriter::operator( ) ( ) - Functor" );
    while ( true ) {

        boost::this_thread::interruption_point();

        timer_.WaitForNextFireTime( );
        
        boost::this_thread::interruption_point();

        writeBuffer();

        ++writeCount_;

        if ( (kWriteLogPeriod != 0) && (writeCount_ >= nextWriteLogCount_) ) {
            nextWriteLogCount_ = writeCount_ + kWriteLogPeriod;

            ostringstream oss;

            oss << writeCount_ << " buffer writes so far";

            programLogInfoIfPossible( oss.str() );
        }

        nextTime_ = timer_.getNextFireTime( );

    } // while ( true )
    
} catch ( ... ) {
    programLogErrorIfPossible(
        "FrameIPQwriter::action exiting on an exception: " +
        getStringForCaught() );

    throw;
}
