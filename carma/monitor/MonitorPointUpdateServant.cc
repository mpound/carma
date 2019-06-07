/**
 * Servant that collects data from clients and pumps said data
 * into monitor stream.
 *
 * @author: Amar Amarnath
 *
 * $Id: MonitorPointUpdateServant.cc,v 1.56 2011/08/01 20:24:40 abeard Exp $
 *
 * $CarmaCopyright$
 *
 */


#include "carma/monitor/MonitorPointUpdateServant.h"

#include <ostream>
#include <iomanip>
#include <sstream>

#include "carma/monitor/SubsystemFrameBuffer.h"
#include "carma/util/Time.h"
#include "carma/util/programLogging.h"
#include "carma/util/Trace.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/ScopedLock.h"

using namespace ::std;
using namespace ::log4cpp;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;

static const string MONITOR_SCRIBER_NAME ("FrameScriber_");

MonitorPointUpdateServant::MonitorPointUpdateServant(
    SubsystemFrameBuffer & buffer,
    boost::mutex & bufferWriteMutex ) :
frameBuffer_( buffer ),
frameBufferMutex_( bufferWriteMutex )
{
    // Nothing
}

MonitorPointUpdateServant::~MonitorPointUpdateServant()
{
    // Nothing
}

string
MonitorPointUpdateServant::makeName( const ushort subsystemID )
{
    ostringstream oss;

    oss << MONITOR_SCRIBER_NAME << subsystemID;

    return oss.str();
}

CORBA::Double
MonitorPointUpdateServant::monitorPointSampleUpdate(
    const MonitorSampleValues & samples,
    const CORBA::Long             frameTime,
    const CORBA::Double           writeDelay )
try
{
    CARMA_CPTRACE( Trace::TRACE1, "MonitorPointUpdateServant::monitorPointSeqUpdate." );
    const CORBA::Double startTime = Time::MJD();

    const bool traceStats = false;

    const int delayStart =
        (traceStats ? static_cast< int >( 1000 * Time::SECONDS_PER_DAY *
                        (Time::MJD() - Time::MJD(Time::computeCurrentFrame())))
                    : 0);

    const long currentFrameTime = SubsystemFrame::computeCurrentFrameTime();

    if ( frameTime != currentFrameTime )  {
        ostringstream oss;

        oss << "MonitorPointUpdateServant::monitorPointSeqUpdate:"
            << " Discarding client data at timestamp " << currentFrameTime
            << " as data frame time is " << frameTime << " (";

        if ( frameTime < currentFrameTime )
            oss << (currentFrameTime - frameTime) << " frames in the past";
        else
            oss << (frameTime - currentFrameTime) << " frames in the future";

        oss << ")";

        programLogErrorIfPossible( oss.str() );
    } else {
        {
            const boost::mutex::scoped_lock lock( frameBufferMutex_ );

            frameBuffer_.writeFromPointSet( samples );
            frameBuffer_.setLastWriterDelay( writeDelay );
            frameBuffer_.setLastWriteTime();
        }

        if ( traceStats ) {
            const int delayEnd =
                static_cast<int>(1000*Time::SECONDS_PER_DAY*
                    (Time::MJD() - Time::MJD(Time::computeCurrentFrame())));

            CPTRACE ( Trace::TRACEALL,
                     "MPupdateServant: numMP's="
                     << samples.dataType.length()
                     << "  delayStart=" << delayStart
                     << "  delayEnd=" << delayEnd
                     << "  delta=" << delayEnd-delayStart );
        }
    }

    return startTime;
} catch ( ... ) {
    {
        ostringstream oss;

        oss << "MonitorPointUpdateServant::monitorPointSeqUpdate: Coming out "
            << " on an exception - " << getStringForCaught();

        programLogErrorIfPossible( oss.str() );
    }

    try {
        throw;
    } catch ( const CORBA::SystemException & ) {
        throw;
    } catch ( const util::UserException & ) {
        throw;
    } catch ( ... ) {
        rethrowCaughtAsUser();
    }

    throw CARMA_EXCEPTION( util::UserException,
                           "Very bad things in MonitorPointUpdateServant::monitorPointSeqUpdate" );

}

