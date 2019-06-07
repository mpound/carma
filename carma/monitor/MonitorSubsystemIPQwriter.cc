/**
 * $Id: MonitorSubsystemIPQwriter.cc,v 1.16 2011/08/01 20:24:40 abeard Exp $ 
 * 
 * Frame timer that incorporates a timer thread.  
 * 
 * Author: N. S. Amarnath
 * 
 * Version: $Revision: 1.16 $ * $Date: 2011/08/01 20:24:40 $ 
 */

#include "carma/monitor/MonitorSubsystemIPQwriter.h"

#include <iomanip>
#include <ios>
#include <ctime>

#include <pthread.h>

#include <log4cpp/Category.hh>

#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/monitor/MonitorPointSet.h"
#include "carma/monitor/MonitorSubsystem.h"
#include "carma/monitor/SubsystemFrameBuffer.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


namespace {

const string kMonitorSubsystemIPQwriterThreadName =
    "carma.monitor.MonitorSubsystemIPQwriterThread";


const string kMonitorSubsystemIPQwriterThreadInitialNdc =
    "MonSubsysIpqWriterThread";

const double kMaxWriteScriberDelayDiffInS = 0.025; // 25ms;

}  // namespace < anonymous >


MonitorSubsystemIPQwriter::MonitorSubsystemIPQwriter(
    MonitorSubsystem & subsystem,
    const double       delayInS,
    boost::mutex &     frameBufferMutex ) : 
FrameIPQwriter( kMonitorSubsystemIPQwriterThreadName,
                delayInS ),
monSubsystem_( &subsystem ),
delayInS_( delayInS ),
notifiedOfAutowriteScriberDelayError_( false ),
frameBufferMutex_( frameBufferMutex )
{
    double now = util::Time::MJD();
    monSubsystem_->monitorPointSet().getBuffer().setFrameCount (
                             util::Time::computeFrame(now) - 1);
}


MonitorSubsystemIPQwriter::~MonitorSubsystemIPQwriter()
{
}


void
MonitorSubsystemIPQwriter::writeBuffer()
{
    writeSubsystem();
}


// protected


// private


void
MonitorSubsystemIPQwriter::writeSubsystem( ) 
try {

    boost::mutex::scoped_lock lock( frameBufferMutex_ );

    if ( monSubsystem_ == 0 ) {
        ostringstream os;
        log4cpp::Category& log = Program::getLogger();
        os << "MonitorSubsystemIPQwriter::writeSubsystem(); monSubsystem_ is NULL"; 
        log << log4cpp::Priority::CRIT << os;
        throw CARMA_ERROR(os);
    }
    
    CARMA_CPTRACE ( Trace::TRACEALL, "MonitorSubsystemIPQwriter::writeSubsystem - Entered " );
    SubsystemFrameBuffer & buffer =
        monSubsystem_->monitorPointSet().getBuffer();
    // buffer.getSubsystemFrame().consolidateSamples();
    long currentTimestamp = buffer.getFrameCount();
    double timestampBefore = util::Time::MJD();
    monSubsystem_->updateFrameAverage();
    double timestampAfter = util::Time::MJD();
    CARMA_CPTRACE ( Trace::TRACEALL,
            "MonitorSubsystemIPQwriter::writeSubsystem: - "
            << "Time to update frame averages "
            << static_cast<int>(1000*util::Time::SECONDS_PER_DAY*(timestampAfter - timestampBefore))
            << endl );

    // Verify that autowrite delay isn't too close to scriber delay
    const double delayDiff = delayInS_ - buffer.getLastWriterDelay( );
    if ( !notifiedOfAutowriteScriberDelayError_ && 
         delayDiff < kMaxWriteScriberDelayDiffInS ) {
        ostringstream err;
        err << "MonitorSubsystemIPQwriter::writeSubsystem() - Autowriter "
            << "delay of " << buffer.getLastWriterDelay() << " is too close ("
            << delayDiff << ") to scriber write delay of "
            << delayInS_ << " seconds.";

        programLogErrorIfPossible( err.str( ) );

        notifiedOfAutowriteScriberDelayError_ = true;

        // Don't throw - throwing will just cause the IMR to continually try
        // and restart the FSP leading to a logging storm.
    }

    buffer.setScriberWriteDelay(delayInS_);
    buffer.setScriberWriteTime();
    buffer.write();
    timestampBefore = util::Time::MJD();
    monSubsystem_->resetValidity();
    timestampAfter = util::Time::MJD();
    CARMA_CPTRACE ( Trace::TRACEALL,
            "MonitorSubsystemIPQwriter::writeSubsystem: - "
            << "Time to reset validity flags "
            << static_cast<int>(1000*util::Time::SECONDS_PER_DAY*(timestampAfter - timestampBefore))
            << endl );

    monSubsystem_->resetTimes();

    const long prevTimestamp = currentTimestamp;

    currentTimestamp = util::Time::computeFrame (getNextFireTime());

    if ( (prevTimestamp != 0) && ((currentTimestamp - prevTimestamp) > 2) ) {
        CARMA_CPTRACE ( Trace::TRACEALL,
                        "timestamp jumped from " << prevTimestamp <<
                        " to " << currentTimestamp );
    }

    buffer.setFrameCount (currentTimestamp);
    CARMA_CPTRACE ( Trace::TRACEALL, "MonitorSubsystemIPQwriter::writeSubsystem - About to leave... " );
} catch ( ... ) {
  Program::getLogger() << log4cpp::Priority::ERROR
		       << "MonitorSubsystemIPQwriter::writeSubsystem exiting with unknown error";
  throw;
}


