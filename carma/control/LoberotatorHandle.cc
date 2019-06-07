/**
 *
 * Carma control lobe rotator interface implementation.
 *
 * @author: Amar Amarnath
 *
 * $Id: LoberotatorHandle.cc,v 1.31 2010/09/27 18:40:25 scott Exp $
 *
 * $CarmaCopyright$
 *
 */
 

#include "carma/control/LoberotatorHandle.h"

#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/LoberotatorSubsystem.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/util/Logger.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/util/corbaSequenceUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/rangeFormatting.h"

using namespace ::std;
using namespace carma;
using namespace carma::control;
using namespace carma::loberotator;
using namespace carma::monitor;
using namespace carma::util;
using namespace log4cpp;


LoberotatorHandle::LoberotatorHandle(
    MonitorSystem&                   carmaMonitor,
    ControlSubsystemBase::Reachable& reachable ) :
        LoberotatorControlRemoteObjHandle( LOBEROTATOR_NAME,
                                   &(reachable.loberotator( )),
                                   &(carmaMonitor.loberotator( )),
                                   &(carmaMonitor),
                                   true,
                                   false )
{

}

LoberotatorHandle::~LoberotatorHandle()
try {
} catch ( ... ) {
    // Just stifle any exception    
    return;
}

void
LoberotatorHandle::updateDelayAndFreq(
   loberotator::LoberotatorControl::DelayFreqPacket delayFreq )
{

    if ( isObjReachable( ) ) {
        string remoteCallString;
        {
            multiset< CORBA::Long > channelIds;
            size_t numChans = delayFreq.delaySeq.length();
            for ( size_t i = 0; i < numChans; ++i ) {
                channelIds.insert(delayFreq.delaySeq[i].channelID);
            }
            
            ostringstream oss;            
            oss << "LoberotatorControl::updateDelayAndFreq( "
                << numChans << " channel ids: "
                << formatAsRanges( channelIds ) << ", freq:"
                << delayFreq.frequency << " )";                
            remoteCallString = oss.str();
        }
        try {
            const double sendTime = Time::MJD( );
            remoteObj( )->updateDelayAndFreq( delayFreq );
            logSentCommandIfNeeded( remoteCallString, sendTime );
        }  catch ( const CORBA::Exception & ex )  {
            processException( remoteCallString, ex );
        }
    }
}



