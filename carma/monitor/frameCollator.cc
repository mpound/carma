/**
 * Collects monitor data from monitor subsystems and writes collated
 * monitor data for the entire CARMA system into shared memory.
 * Sets validity flags for use by the fault system.
 *
 * @author: Amar Amarnath
 *
 * $Id: frameCollator.cc,v 1.42 2014/04/23 23:39:43 scott Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <sys/times.h>
#include <unistd.h>

#include "carma/corba/corba.h"
#include "carma/corba/Server.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/Trace.h"
#include "carma/monitor/FrameCollatorThread.h"
#include "carma/monitor/MonitorSystemIPQwriter.h"

#include <boost/ref.hpp>
#include <boost/thread.hpp>

using namespace ::std;
using namespace CORBA;
using namespace carma;
using namespace carma::util;
using namespace carma::monitor;


//
// @version        $Revision: 1.42 $ $Date: 2014/04/23 23:39:43 $
//
// @description        catch subsystem frames as notifications, write them to
//              monitor system frames in an IPQ.
//
// @usage       frameCollator [--keywords] [--help] [--usage] [delay=[450]] [collatorOn=[true]] [IPQwriterOn=[true]] [imr=[imr]] [-- [-ORBDefaultInitRef corbaloc::<imrhost>]]
//              If using ORBDefaultInitRef, then explictly set imr=-
//
// @key mode raw string
//      May be \"raw\" or \"final\". Default is \"raw\".
//
// @key collatorOn true bool
//      If true, the monitor data is collected and collated
//
// @key IPQwriterOn true bool
//      If true, the monitor data is written to the IPQ
//
// @key delay 450 int
//      Offset from the half-second, in milliseconds.
//      must be between 0 and 450 milliseconds to take effect.
//      Default value is 450 msec.
//
// @key clearDelay 0 int
//      The number of seconds to run before clearing all error counts,
//      such as late, early, and missed frames. The counts are cleared for
//      all subsystems and for the totals. The delayed clearing eliminates 
//      flow irregularities that arise when the systems are overloaded 
//      during startup.
//
// @key duration @noDefault int
//      If given then it is the number of seconds to run for before quitting.
//      Otherwise run forever.
//      Only usefeul for debugging, profiling, and coverage.
//      Value gets pinned to a minimum of 5 seconds to hopefully avoid
//      potentially ugly race conditions at startup that I don't feel like
//      fixing right in the base NotificationCOnsumer class.
//
// @logger MONITOR_FACILITY carma.monitor.frameCollator


namespace {

const string kReceiverName = "MonitorSystemCollator";

const double COLLATOR_WRITE_DELAY_IN_S  = 0.450; 

} // namespace < anonymous >

int
Program::main( )
{
    const bool rawMode = (getStringParameter( "mode" ) == "raw");
    const bool turnIPQwriterOn = getBoolParameter( "IPQwriterOn" );
    const bool turnCollatorOn = getBoolParameter( "collatorOn" );
    const bool haveDuration = parameterWasSpecified( "duration" );
    const int duration =
        (haveDuration ?
            (::std::max( 5, getIntParameter( "duration" ) )) :
            0);
    int writeDelayInMS = getIntParameter( "delay" );
    int clearDelayInFrames = 2*getIntParameter( "clearDelay" );
    {
        const unsigned int defaultDelay =
            static_cast< unsigned int >( COLLATOR_WRITE_DELAY_IN_S * 1000 );

        if ( (writeDelayInMS < 0) ) {
            ostringstream oss;
            
            oss << "frameCollator: Invalid delay value of "
                << writeDelayInMS << " msec. Using a write delay of "
                << defaultDelay << " msec instead.";
            
            programLogWarnIfPossible( oss.str() );
            
            writeDelayInMS = defaultDelay;
        }
    }

    const double delayInS = ( writeDelayInMS ) / 1000.0;
    
    {
        ostringstream msg;
        msg << " Creating FrameCollatorThread with delay " << delayInS << "s.";
        programLogNotice( msg.str( ) );
    }

    corba::Server & server = getCorbaServer();

    FrameCollatorThread collatorWorker(kReceiverName,
                                       delayInS,
	                                   rawMode,
                                       server);
    boost::thread collatorThread; // Not-A-Thread (yet)

    programLogNotice(" Creating MonitorSystemIPQwriter " );
    MonitorSystemIPQwriter writerThread(delayInS, collatorWorker,
                        clearDelayInFrames);
    boost::thread writerThreadThread;

    try {

        // start collator thread to collect subsystem frames and write
        // them to the local buffer for the SystemFrame
        programLogNotice(" Starting Collator thread " );
        if ( turnCollatorOn ) {
            boost::thread newCollatorThread( boost::ref( collatorWorker ) );
            collatorThread.swap( newCollatorThread );
        }

        // start timer thread to write the system frame to shared memory
        programLogNotice(" Starting IPQwriter thread " );
        if ( turnIPQwriterOn ) {
            boost::thread newWriterThread( boost::ref( writerThread ) );
            writerThreadThread.swap( newWriterThread );
        }

        if ( haveDuration ) {
            string durationText;
            {
                ostringstream oss;                
                oss << duration;                
                durationText = oss.str();
            }

            programLogInfoIfPossible(
                "Going to sleep for " + durationText + " seconds..." );

            ::sleep( duration );
        
            programLogInfoIfPossible(
                durationText + 
                " second sleep is done and now attempting to quit" );
            
            {
                programLogInfoIfPossible( "Stopping collator thread" );
                
                server.stop();
                collatorThread.join();

                programLogInfoIfPossible( "Collator thread is dead" );
            }
            
        } else {
            programLogInfoIfPossible( "Waiting on collator thread exit..." );
    
            collatorThread.join();

            programLogInfoIfPossible( "Collator thread is dead" );
        }

        {
            programLogInfoIfPossible( "Stopping writer thread" );

            writerThreadThread.interrupt();
            writerThreadThread.join(); 
            programLogInfoIfPossible( "Writer thread is dead" );
        }
    } catch ( ... ) {
        programLogCriticalIfPossible( 
            "frameCollator: Program::main caught exception - " +
            getStringForCaught() );
        throw;
    }

    return EXIT_SUCCESS;
}
