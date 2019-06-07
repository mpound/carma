/**
 *
 * Carma control interface server implementation, for procedures.
 * These are either more complicated methods, or those which take
 * a while to complete. This helps breaks the idl implementation into
 * managable sized pieces.
 *
 * @author: Steve Scott
 *
 * $Id: SubarrayControlProcs.cc,v 1.123 2014/06/04 17:09:16 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */


#include <map>

#include "carma/corba/corba.h"
#include "carma/antenna/common/RxTypeInfo.h"
#include "carma/control/CalibratorHandle.h"
#include "carma/control/CorrelatorHandle.h"
#include "carma/control/DriveHandle.h"
#include "carma/control/FocusHandle.h"
#include "carma/control/FunctorWorkRequest.h"
#include "carma/control/OpticalTelHandle.h"
#include "carma/control/PipelineHandle.h"
#include "carma/control/HandleMethodFunctorGroup.h"
#include "carma/control/RemoteObjMethodFunctorGroup.h"
#include "carma/control/RxSelectorHandle.h"
#include "carma/control/SubarrayControlImpl.h"
#include "carma/monitor/SlPipelineSubsystemExt.h"
#include "carma/monitor/BimaSubsystem.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/util/corbaSequenceUtils.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/rangeFormatting.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/WorkResult.h"


using namespace ::std;
using namespace ::log4cpp;
using namespace carma;
using namespace carma::antenna::common;
using namespace carma::control;
using namespace carma::monitor;
using namespace carma::pipeline;
using namespace carma::util;


void
SubarrayControlImpl::tilt( const SeqShort & carmaAntNoSeq )
try {
    return;
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::doIVcurve(
    const carma::antenna::common::RxControl::Type rxType,
    const carma::antenna::common::RxControl::Pol_Type pol,
    const CORBA::Float startVjInMv,
    const CORBA::Float stopVjInMv,
    const CORBA::Float stepVjInMv,
    const CORBA::UShort deltaInMs,
    const CORBA::Boolean doTotalPower,
    const SeqShort & carmaAntNoSeq )
try {

    string paramString;
    {
        ostringstream os;
        os << " rxType=" << RxTypeInfo::stringFromRxType( rxType ) << ","
           << " startVjInMv=" << startVjInMv << ","
           << " stopVjInMv=" << stopVjInMv << ","
           << " stepVjInMv=" << stepVjInMv << ","
           << " deltaInMs=" << deltaInMs << ","
           << " doTotalPower=" << doTotalPower << ","
           << getStringForCarmaAntNoSeq(carmaAntNoSeq);

        paramString = os.str( );
    }

    cmdlog() << "SubarrayControlImpl::doIVcurve( " << paramString << " ).";

    nextTuneSeqNo_++;  // Preferred seq no

    string requestIdCallString;
    {
        ostringstream os;
        os << "RxSelect::doIVcurve( seqNo=" << nextTuneSeqNo_ << " ).";
        requestIdCallString = os.str( );
    }

    const RxSelectorGroup rxSelectorGroup =
        getRxSelectorGroup("doIVcurve", carmaAntNoSeq);
    WorkResultSet doIVcurveWrs("RxSelect::doIVcurve cmd");

    RxSelectorHandle::IVcurveArgs args;
    args.startVjInMv = startVjInMv;
    args.stopVjInMv = stopVjInMv;
    args.stepVjInMv = stepVjInMv;
    args.deltaInMs = deltaInMs;
    args.doTotalPower = doTotalPower;

    queueFunctorWorkRequestGroup(
        requestIdCallString,
        makeHandleMethodFunctorGroup(
            rxSelectorGroup,
            &RxSelectorHandle::doIVcurve,
            rxType,
            pol,
            args,
            static_cast<MonitorSystem*>(&carmaMonitor_),
            nextTuneSeqNo_ ),
        doIVcurveWrs,
        *workerPool_);

    waitForAllNormal( doIVcurveWrs );

} catch ( ... ) {
    rethrowCaughtAsUser();
}

carma::antenna::common::IVCurve *
SubarrayControlImpl::getIVcurve( CORBA::Short carmaAntNo )
try {
    cmdlog() << "getIVcurve( carmaAntNo=" << carmaAntNo << " ).";

    RxSelectorGroup rxGroup = getRxSelectorGroupForCarmaAntNo( "getIVcurve",
                                                               carmaAntNo );

    if ( rxGroup.size( ) == 1 ) {
        RxSelectorGroup::iterator i = rxGroup.begin( );

        const bool logIfNotReachable = true;
        if ( (*i)->isObjReachable( logIfNotReachable ) )
            return (*i)->getIVcurve( );
    }

    return 0;
} catch ( ... ) {
    rethrowCaughtAsUser();
    return 0;
}

// deprecated (used for Vax)
unsigned long SubarrayControlImpl::getRxCommandLateAfterMillis(
    const RxSelectorGroup& rxGroup )
{
    return getDefaultLateAfterMillis();
}

namespace {

string
getStringForWaitItem( const WaitItem item ) {
    switch ( item ) {
        case WAIT_INTERVAL:    return "WAIT_INTERVAL";
        case WAIT_INTEG:       return "WAIT_INTEG";
        case WAIT_ONSOURCE:    return "WAIT_ONSOURCE";
        case WAIT_CALIBRATOR:  return "WAIT_CALIBRATOR";
        case WAIT_TUNED:       return "WAIT_TUNED";
        case WAIT_TILT:        return "WAIT_TILT";
        case WAIT_CENTROID:    return "WAIT_CENTROID";
        case WAIT_OPTICS:      return "WAIT_OPTICS";
        case WAIT_CORRELATOR:  return "WAIT_CORRELATOR";
    }

    return "< unknown >";
}


string
getStringForWaitCondition( const WaitCondition condition ) {
    switch ( condition ) {
        case WAIT_SINGLE: return "WAIT_SINGLE";
        case WAIT_ALL:    return "WAIT_ALL";
        case WAIT_COUNT:  return "WAIT_COUNT";
    }

    return "< unknown >";
}

}  // End namespace < anonymous >

//========================================================================
bool
SubarrayControlImpl::containsComponent( const CarmaComponentNoVec & v,
                                      const unsigned short  component )
{
    // A 'component' is either a band or an antenna number
    // This would be more efficient as a std::set.
    for (unsigned short a=0; a<v.size(); a++) {
        if (v.at(a) == component) return true;
    }
    return false;
}


class SubarrayControlImpl::CachedCarmaMonSys::ScopedUse {
    public:
        explicit ScopedUse( CachedCarmaMonSys & cache,
                            const string &      clientId,
                            bool                logPerfectCaching );

        /* virtual */ ~ScopedUse( );

        CarmaMonitorSystem & getInstance( );

    private:
        // No copying
        ScopedUse( const ScopedUse & rhs );
        ScopedUse & operator=( const ScopedUse & rhs );

        const bool                     logPerfectCaching_;
        CachedCarmaMonSys &            cache_;
        const string                   clientId_;
        auto_ptr< CarmaMonitorSystem > instance_;

        static unsigned int instanceCount_;
        static ::pthread_mutex_t instanceCountMutex_;
};

unsigned int
SubarrayControlImpl::CachedCarmaMonSys::ScopedUse::instanceCount_ = 0;

::pthread_mutex_t
SubarrayControlImpl::CachedCarmaMonSys::ScopedUse::instanceCountMutex_
    = PTHREAD_MUTEX_INITIALIZER;

SubarrayControlImpl::CachedCarmaMonSys::ScopedUse::ScopedUse(
    CachedCarmaMonSys & cache,
    const string &      clientId,
    const bool          logPerfectCaching ) :
logPerfectCaching_( logPerfectCaching ),
cache_( cache ),
clientId_( clientId.empty() ? string( "unknown client" ) : clientId ),
instance_()
{
}


SubarrayControlImpl::CachedCarmaMonSys::ScopedUse::~ScopedUse( )
try {
    if ( instance_.get() != 0 ) {
        // Try to put the one we have back into the cache unless the cache
        // already has one
        {
            string newLastCachedInUseBy;

            const ScopedLock< PthreadMutex > lock( cache_.guard_ );

            if ( cache_.cachedInstance_.get() == 0 ) {
                cache_.cachedInstance_ = instance_;
                cache_.lastCachedInUseBy_.swap( newLastCachedInUseBy );
            }
        }

        if ( instance_.get() == 0 ) {
            if ( logPerfectCaching_ ) {
                programLogInfoIfPossible(
                    "Cached cachable cms from " + clientId_ );
            }
        } else {
            // Failing that we clean it up ourselves
            instance_.reset();

            string warnMsg;
            {
                ScopedLock< ::pthread_mutex_t > scopelck( instanceCountMutex_ );

                --instanceCount_;

                ostringstream warnOss;
                warnOss << "Deleted cachable cms from " << clientId_
                        << ", instance count is now " << instanceCount_ << ".";
                warnMsg = warnOss.str();
            }

            // Cache capacity exceeded by overlapping clients
            programLogWarnIfPossible( warnMsg );

        }
    }
} catch ( ... ) {
    try {
        programLogErrorIfPossible(
            "Stifling exception in"
            " SubarrayControlImpl::CachedCarmaMonSys::ScopedUse::~ScopedUse"
            " - " + getStringForCaught() );
    } catch ( ... ) {
        // Just stifle any exceptions
    }

    // Just stifle any exceptions
    return;
}


CarmaMonitorSystem &
SubarrayControlImpl::CachedCarmaMonSys::ScopedUse::getInstance( )
try {
    if ( instance_.get() != 0 )
        return *instance_;

    // First try to use one the cache is already holding
    string lastCachedInUseBy;
    {
        string newLastCachedInUseBy = clientId_;

        const ScopedLock< PthreadMutex > lock( cache_.guard_ );

        if ( cache_.cachedInstance_.get() != 0 ) {
            instance_ = cache_.cachedInstance_;
            cache_.lastCachedInUseBy_.swap( newLastCachedInUseBy );
        } else
            lastCachedInUseBy = cache_.lastCachedInUseBy_;
    }

    if ( instance_.get() != 0 ) {
        if ( logPerfectCaching_ ) {
            programLogInfoIfPossible(
                "Recycled cachable cms for " + clientId_ );
        }

        return *instance_;
    }

    string instanceCountString;
    {
        ScopedLock< ::pthread_mutex_t > scopelck( instanceCountMutex_ );
        ++instanceCount_;
        ostringstream oss;
        oss << instanceCount_;
        instanceCountString = oss.str();
    }

    if ( lastCachedInUseBy.empty() == false ) {
        // Cache capacity exceeded by overlapping clients
        programLogWarnIfPossible(
            "Allocating new cachable cms for " + clientId_ +
            " because last cached one was in use by " +
            lastCachedInUseBy + ". Instance count will be "  +
            instanceCountString );
    } else {
        // First attempted use of an inactive cache
        programLogInfoIfPossible(
            "Allocating new cachable cms for " + clientId_ +
            ". Instance count will be "  + instanceCountString );
    }

    // We can't recycle one so we allocate a new one
    instance_ = auto_ptr< CarmaMonitorSystem >( new CarmaMonitorSystem );

    return *instance_;
} catch ( ... ) {
    programLogErrorIfPossible(
        "Coming out of"
        " SubarrayControlImpl::CachedCarmaMonSys::ScopedUse::getInstance"
        " on an exception - " + getStringForCaught() );

    throw;
}


// This method is thread safe only if not waiting on the same components;
// this is a sensible restriction, it is just not enforced by the code.
ComponentReady *
SubarrayControlImpl::wait( const WaitItem      item,
                           const SeqShort&     carmaComponentNoSeq,
                           const float         tmo,
                           const WaitCondition condition,
                           const short         count )
try {
    ScopedLogNdc ndc("SubarrayControlImpl::wait");
    const vector<CORBA::Short> vec =
             convertSequenceToVector<CORBA::Short>(carmaComponentNoSeq);
    WaitCondition condition_ = condition;

    string cmdText;
    ostringstream cmdSS;
    cmdSS << "wait("
             << "item=" << getStringForWaitItem(item)
             << ", componentNoSeq=[" << formatAsRanges(vec) << "]"
             << ", tmo=" << tmo
             << ", condition=" << getStringForWaitCondition(condition)
             << ", count=" << count
             << ")";
    cmdText = cmdSS.str();
    cmdlog() << cmdText;

    cancelFlag_ = false;
    CompletionStatusType completionStatus;
    CarmaComponentNoVec initialCarmaComponentNoVec;
    ComponentReady_var componentReady(new ComponentReady);
    componentReady->ready.length(0);

#if 0
    bool timeOutSpecified = ( tmo > 0 );
#endif
    // Convert the seq to a vector, handling case of ant 0 or band 0
    if ((item != WAIT_INTEG) && (item != WAIT_INTERVAL)) {
        if ( (carmaComponentNoSeq.length() != 0) &&
             (carmaComponentNoSeq[0] == 0) ) {

            if ( item == WAIT_CORRELATOR ) {
                initialCarmaComponentNoVec =
                    getCarmaBandNoVecForAllOnlineAndReachableBands( );
            } else {
                initialCarmaComponentNoVec =
                    getCarmaAntNoVecForAllReachableAntennas();
            }

        } else {
            for (unsigned int i=0; i < carmaComponentNoSeq.length(); ++i) {
                initialCarmaComponentNoVec.push_back(carmaComponentNoSeq[i]);
            }
        }
    }

    // Consecutive invalid monitor data samples
    const int invalidDataLimit = 24;

    double startTime = Time::MJD();
    bool timedOut = false;

    CarmaMonitorSystem& mon = carmaMonitor_;
    // Read from the top of the queue
    mon.readNewestConditionalCopy();
    int monFrameCount = mon.getFrameCount();

    // Setup completion status vector for various modes
    switch (item) {
        case WAIT_INTERVAL:
            // This should never do anything besides check tmo value
            // as it just uses the tmo
            if (tmo <= 0) {
                throw CARMA_EXCEPTION(UserException,
                    "INTERVAL wait must have tmo value > 0");
            }
            break;
        case WAIT_INTEG:
            break;
       case WAIT_ONSOURCE:
       case WAIT_CALIBRATOR:
       case WAIT_TUNED:
       case WAIT_CENTROID:
       case WAIT_OPTICS:
       case WAIT_CORRELATOR:
           {
            CarmaComponentNoVec::iterator cmpIt =
                initialCarmaComponentNoVec.begin();
            const CarmaComponentNoVec::iterator cmpItEnd =
                initialCarmaComponentNoVec.end();
            for ( ; cmpIt != cmpItEnd; ++cmpIt ) {
                completionStatus.insert( make_pair( *cmpIt, false ) );
            }
           }
           break;
       case WAIT_TILT:
            break;
    }

    // ---------------------------------------------------------
    // The loop that does the waiting...
    bool componentsAreReady = false;
    int passCount = 0;
    double lastLoopTime = 0; // MJD of finish of last loop
    while (!componentsAreReady && !cancelFlag_) {
        passCount++;
        // Check for timeout
        if (tmo > 0.0) {
            double elapsedSecs =
                    Time::SECONDS_PER_DAY*(Time::MJD() - startTime);
            if (elapsedSecs > tmo) {
                timedOut = true;
                break;
            }
        }

        // Sleep for half a second since last time through loop.
        // This thread will sleep an extra amount if it is scheduled out
        // in between the sleeptime calc and the actual sleep.
        // The logic here will cut down on extra sleeping if it is
        // scheduled out after the sleep.
        double loopTime = Time::MJD();
        double sleepTime =
                0.5 - Time::SECONDS_PER_DAY*(loopTime - lastLoopTime);
        if (sleepTime > 0.050) {
            usleep((int)(1e6*sleepTime));
        }
        if (item != WAIT_INTERVAL) {
            mon.readNewestConditionalCopy();
            int fc = mon.getFrameCount();
            bool sameData = (fc == monFrameCount) ;
            // Poll until a new frame is available; this is a shared
            // copy of the mon system so others may be reading it.
            // Don't poll longer than timeout or fixed interval of
            // 10 seconds if timeout was not given.  Otherwise an
            // infinite loop is possible if the monitor system is
            // not being updated.
            //double maxWait = tmo > 0.0 ? tmo : 10.0;
            while (sameData) {
#if 0
                double elapsedSecs =
                        Time::SECONDS_PER_DAY*(Time::MJD() - startTime);
                if (elapsedSecs > maxWait ) {
                    if ( timeOutSpecified ) {
                        timedOut = true;
                        break; 
                    } else {
                        throw CARMA_ERROR("Unable to read the monitor system for the last 10s.  Is the FaultSystem running?"); 
                    }
                }
#endif
                // Sleep for 50 milliseconds
                usleep(50000);
                mon.readNewestConditionalCopy();
                fc = mon.getFrameCount();
                sameData = (fc == monFrameCount) ;
            }
            monFrameCount = fc;
        }
        // we may have timed out waiting for the monitor system.
        // if so, break out of loop here.
        if ( timedOut ) break;
        lastLoopTime = Time::MJD();

        // Update antennas or bands in the completion status vector in
        // the event that someone removed ants or a correlator or band
        // became unreachable or offline.  If we don't do this we can get
        // exceptions later on when the handle is used to get the status.
        if ((item != WAIT_INTEG) && (item != WAIT_INTERVAL)) {

            CarmaComponentNoVec allOnlineAndReachableComponents;
            if ( item == WAIT_CORRELATOR ) {
                allOnlineAndReachableComponents =
                    getCarmaBandNoVecForAllOnlineAndReachableBands( );
            } else {
                allOnlineAndReachableComponents =
                    getCarmaAntNoVecForAllReachableAntennas();
            }

            CarmaComponentNoVec carmaComponentNoVec;

            // Get either initial req set of ants or bands or current set of
            // all ants or bands since either can go offline at anytime.
            if ( (carmaComponentNoSeq.length() != 0) &&
                 (carmaComponentNoSeq[0] == 0) ) {
                carmaComponentNoVec = allOnlineAndReachableComponents;
            } else {
                carmaComponentNoVec = initialCarmaComponentNoVec;
            }

            // Now remove those that should no longer be in completion stat vec.
            CompletionStatusType::iterator i = completionStatus.begin();
            const CompletionStatusType::const_iterator iEnd =
                        completionStatus.end();
            for ( ; i != iEnd;  ) {
                const unsigned short carmaComponentNo = i->first;
                // Remove(erase) if not in our req set or not in reachable set
                if (!containsComponent( carmaComponentNoVec,
                                        carmaComponentNo) ||
                    !containsComponent( allOnlineAndReachableComponents,
                                        carmaComponentNo)) {
                    CompletionStatusType::iterator j = i;
                    ++i;
                    completionStatus.erase(j);
                }
                else {
                    ++i;
                }
            }

        } // End if (notIntegrating and notInterval)

        // Now use monitor data to see if wait is complete
        switch (item) {
            case WAIT_INTERVAL:
                // This should never do anything - just uses the tmo value
                break;
            case WAIT_INTEG:
                {
                    const bool integDone = waitIntegration(mon,
                                                           passCount,
                                                           invalidDataLimit);
                    if ( integDone )
                        return componentReady._retn();
                }
                break;
           case WAIT_ONSOURCE:
                if (noiseSource_) {
                    // If the noise source is on then we don't wait on antennas
                    CompletionStatusType::iterator i = completionStatus.begin();
                    const CompletionStatusType::const_iterator iEnd =
                                completionStatus.end();
                    for ( ; i != iEnd;  i++) i->second = true;
                    componentsAreReady = true;
                }
                else {
                    componentsAreReady = waitTracking(mon, passCount,
                        invalidDataLimit, completionStatus, condition_, count);
                }
                break;
           case WAIT_CALIBRATOR:
                componentsAreReady = waitCalibrator(mon, passCount,
                        invalidDataLimit, completionStatus, condition_);
                break;
           case WAIT_TUNED:
                componentsAreReady = waitTuned(mon, passCount,
                        invalidDataLimit, completionStatus, condition_);
                break;
           case WAIT_TILT:
                break;
           case WAIT_CENTROID:
                componentsAreReady = waitCentroid(mon, passCount,
                        invalidDataLimit, completionStatus, condition_, count);
               break;
           case WAIT_OPTICS:
                componentsAreReady = waitOptics(mon, passCount,
                        invalidDataLimit, completionStatus, condition_ );
                break;
           case WAIT_CORRELATOR:
                componentsAreReady = waitCorrelator( mon, passCount,
                        invalidDataLimit, completionStatus, condition_ );
                break;
        }
    } // End while wait loop
    // ---------------------------------------------------------

    // If we are done or timed out, prepare and return the results
    if (componentsAreReady || timedOut) {
        CompletionStatusType::iterator i = completionStatus.begin();
        const CompletionStatusType::const_iterator iEnd =
                    completionStatus.end();
        int done = 0;
        int notdone = 0;
        bool isInterval = (item == WAIT_INTERVAL);
        for ( ; i != iEnd;  i++) {
            unsigned short carmaAntNo = i->first;
            if (i->second || isInterval) {
                done++;
                componentReady->ready.length(done);
                componentReady->ready[done-1] = carmaAntNo;
            }
            else {
                notdone++;
                componentReady->notready.length(notdone);
                componentReady->notready[notdone-1] = carmaAntNo;
            }
        }
        return componentReady._retn();
    }

    // Throw if we are canceled
    throw CARMA_EXCEPTION(CancelException, "Wait cancelled");

} catch ( const TimeoutException & ) {
    throw;  // rethrow
} catch ( const InvalidMonitorDataException & ) {
    // This is an invalid monitor data timeout
    throw;  // rethrow
} catch ( const CancelException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // just to shut up the compiler warning
    throw CARMA_EXCEPTION(UserException,"Very bad things in SaCI::wait()");
}

// Helper method; returns true if done waiting, throws on errors
bool
SubarrayControlImpl::waitIntegration(
    const MonitorSystem&  mon,
    const int             passCount,
    const int             invalidDataLimit )
{
    // if not correlator assigned to this subarray, there is nothing to wait for.
    if ( getCorrelatorDesignation() == CORR_NONE ) return true;

    PipelineGroup pg = getPipelineGroup();
    if ( pg.empty() )  {
        ostringstream os;
        os << "There appears to be no integration pipeline associated with this subarray "
           << "even though there the " << getStringForCorrType(getCorrelatorDesignation()) << " correlator is assigned to it. "
           << "You probably need to restart the system to recover from this, but you might try "
           << "addCorrelator first.";
           throw CARMA_ERROR( os.str() );
    }

    PipelineHandle & ph = (*(*(pg.begin( ))));

    // Trigger reset of data counts on first pass
    int lim = 0;
    if ( passCount == 1 )
        lim = invalidDataLimit;

    return ph.isIntegrationComplete( mon, lim );
}

// Helper method; returns true if done waiting, throws on errors
bool
SubarrayControlImpl::waitTracking(
    const MonitorSystem&  mon,
    const int             passCount,
    const int             invalidDataLimit,
    CompletionStatusType& completionStatus,
    const WaitCondition   condition,
    const short           count)
{
    CompletionStatusType::iterator i = completionStatus.begin();
    const CompletionStatusType::const_iterator iEnd = completionStatus.end();
    int numDone    = 0;
    int numNotDone = 0;
    const string cmd = "waitTracking";
    for ( ; i != iEnd;  i++) {
        unsigned short carmaAntNo = i->first;
        bool done = i->second;
        if (done) {
            numDone++;
        }
        else { // Not previously on source, let's check
            bool newDone = false;
            // If an antenna is removed from the subarray, then we get
            // an exception when trying to get the drive group.
            // We interpret that as an action complete.
            DriveGroup dg;
            try {
                dg = getDriveGroupForCarmaAntNo(cmd, carmaAntNo);
            }
            catch (const BaseException& e) {
                newDone = true;
            }
            if (!newDone) {
                if (dg.empty()) {
                    ostringstream o;
                    o << "No driveHandle for carma antenna #" << carmaAntNo;
                    throw CARMA_EXCEPTION(UserException, o.str().c_str());
                }
                DriveHandle* d = *(dg.begin());
                if (d == 0) {
                    ostringstream o;
                    o << "Invalid drive handle for carma antennna #" << carmaAntNo;
                    throw CARMA_EXCEPTION(UserException, o.str().c_str());
                }
                try {
                    // Use limit on first pass, 0 on subsequent
                    int lim = 0;
                    if (passCount == 1) lim = invalidDataLimit;
                    newDone = d->isActionComplete(mon, lim);
                } catch ( const BaseException & e ) {
                    {
                    ostringstream oss2;
                    oss2 << "Caught exception coming out of "
                         << "DriveHandle::isActionComplete() and "
                         << "rethrowing as an InvalidMonitorDataException: "
                         << getStringForCaught();

                    programLogErrorIfPossible( oss2.str() );
                    }

                    ostringstream oss;
                    oss << "waitTracking: Monitor data invalid for carma antenna #"
                        << carmaAntNo
                        << " for " << invalidDataLimit
                        << " consecutive frames";

                    throw CARMA_EXCEPTION( InvalidMonitorDataException,
                                       oss.str().c_str() );
                } // end catch
            }     // end if !newDone
            // Update based on new result
            if (newDone) {
                numDone++;
                i->second = true;
            }
            else {
                numNotDone++;
            }
        } // End if(done)
    } // End for loop over antennas

    //waitDebugger(completionStatus, "Track", passCount, numDone, numNotDone);

    // Completion status...
    if ((condition == WAIT_SINGLE) && (numDone >= 1)) return true;
    if (condition == WAIT_COUNT) {
        if (count >= 0) {
            // We are finished if all ants are done, even if we don't make
            // it to the count
            if (numNotDone == 0)  return true;
            // Check count...
            if (numDone >= count) return true;
            else                  return false;
        }
        else {
            // Negative counts mean check the number not done
            if (numNotDone <= abs(count)) return true;
            else                          return false;
        }
    }
    // For the WAIT_ALL case
    return (numNotDone == 0);
}


// Helper method; returns true if done waiting, throws on errors
bool
SubarrayControlImpl::waitCentroid(
    const MonitorSystem&  mon,
    const int             passCount,
    const int             invalidDataLimit,
    CompletionStatusType& completionStatus,
    const WaitCondition   condition,
    const short           count)
{
    CompletionStatusType::iterator i = completionStatus.begin();
    const CompletionStatusType::const_iterator iEnd = completionStatus.end();
    int numDone    = 0;
    int numNotDone = 0;
    const string cmd = "waitCentroid";

    for ( ; i != iEnd;  i++) {
        const unsigned short carmaAntNo = i->first;
        const bool done = i->second;
        if (done) {
            numDone++;
        }
        else {
            bool newDone = false;
            // If an antenna is removed from the subarray, then we get
            // an exception when trying to get the drive group.
            // We interpret that as an action complete.
            OpticalTelGroup otg;
            try {
                otg = getOpticalTelGroupForCarmaAntNo( cmd, carmaAntNo );
            }
            catch (const BaseException& e) {
                newDone = true;
            }
            if (!newDone) {
                if (otg.empty()) {
                    ostringstream o;
                    o << "No OpticalTelescopeHandle for carma antenna #"
                      << carmaAntNo;
                    throw CARMA_EXCEPTION(UserException, o.str().c_str());
                }
                OpticalTelHandle * oth = *(otg.begin());
                if (oth == 0) {
                    ostringstream o;
                    o << "Invalid optical telescope handle for carma "
                      << "antennna #" << carmaAntNo;
                    throw CARMA_EXCEPTION(UserException, o.str().c_str());
                }
                try {
                    // Use limit on first pass, 0 on subsequent
                    int lim = 0;
                    if (passCount == 1) lim = invalidDataLimit;
                    newDone = oth->isActionComplete(mon, lim);
                } catch ( const BaseException & e ) {
                    {
                    ostringstream oss2;
                    oss2 << "Caught exception coming out of "
                         << "OpticalTelHandle::isActionComplete() and "
                         << "rethrowing as an InvalidMonitorDataException: "
                         << getStringForCaught();

                    programLogErrorIfPossible( oss2.str() );
                    }

                    ostringstream oss;
                    oss << "waitCentroid: Monitor data invalid for carma antenna #"
                        << carmaAntNo
                        << " for " << invalidDataLimit
                        << " consecutive frames";

                    throw CARMA_EXCEPTION( InvalidMonitorDataException,
                                       oss.str().c_str() );
                } // end catch
            }     // end if !newDone
            // Update based on new result
            if (newDone) {
                numDone++;
                i->second = true;
            }
            else {
                numNotDone++;
            }
        } // End if(done)
    } // End for loop over antennas

    //waitDebugger(completionStatus, "Centroid", passCount, numDone, numNotDone);

    // Completion status...
    if ((condition == WAIT_SINGLE) && (numDone >= 1)) return true;
    if (condition == WAIT_COUNT) {
        if (count >= 0) {
            // We are finished if all ants are done, even if we don't make
            // it to the count
            if (numNotDone == 0)  return true;
            // Check count...
            if (numDone >= count) return true;
            else                  return false;
        }
        else {
            // Negative counts mean check the number not done
            if (numNotDone <= abs(count)) return true;
            else                          return false;
        }
    }
    // For the WAIT_ALL case
    return (numNotDone == 0);
}



// Helper method; returns true if done waiting, throws on errors
bool
SubarrayControlImpl::waitOptics(
    const MonitorSystem &  mon,
    const int              passCount,
    const int              invalidDataLimit,
    CompletionStatusType & completionStatus,
    WaitCondition          condition )
{
    CompletionStatusType::iterator i = completionStatus.begin();
    const CompletionStatusType::const_iterator iEnd = completionStatus.end();
    int numDone    = 0;
    int numNotDone = 0;
    for ( ; i != iEnd;  i++) {
        const unsigned short carmaAntNo = i->first;
        const bool done = i->second;
        if (done) {
            numDone++;
        } else { // Not previously ready, let's check
            bool newDone = false;
            // If an antenna is removed from the subarray, then we get
            // an exception when trying to get the calibrator group.
            // We interpret that as an action complete.
            FocusGroup fg;
            try {
                fg = getFocusGroupForCarmaAntNo( "wait", carmaAntNo );
            } catch (const BaseException& e) {
                newDone = true;
            }
            if (!newDone) {
                if ( fg.empty() ) {
                    ostringstream o;
                    o << "No focusHandle for carma ant#" << carmaAntNo;
                    throw CARMA_EXCEPTION(UserException, o.str().c_str());
                }
                FocusHandle * f = *(fg.begin());
                if ( f == 0 ) {
                    ostringstream o;
                    o << "Invalid focus handle for carma antennna #"
                    << carmaAntNo;
                    throw CARMA_EXCEPTION(UserException, o.str().c_str());
                }
                try {
                    newDone = f->isActionComplete(mon, invalidDataLimit);
                } catch ( const BaseException & e ) {
                    {
                        ostringstream oss2;
                        oss2 << "Caught exception coming out of "
                             << "FocusHandle::isActionComplete() and "
                             << "rethrowing as an InvalidMonitorDataException: "
                             << getStringForCaught();
                        programLogErrorIfPossible( oss2.str() );
                    }

                    ostringstream oss;
                    oss << "waitOptics: Monitor data invalid for carma antenna #"
                        << carmaAntNo << " for "
                        << invalidDataLimit << " consecutive frames";
                    throw CARMA_EXCEPTION( InvalidMonitorDataException,
                                       oss.str().c_str() );
                } // End catch
            }     // End if !newDone
            // Update based on new result
            if (newDone) {
                numDone++;
                i->second = true;
            }
            else {
                numNotDone++;
            }
        } // End if(done)
    } // End for loop over antennas

    if ((condition == WAIT_SINGLE) && (numDone >= 1)) return true;
    return (numNotDone == 0);
}

// Helper method; returns true if done waiting, throws on errors
bool
SubarrayControlImpl::waitCalibrator(
    const MonitorSystem &  mon,
    const int              passCount,
    const int              invalidDataLimit,
    CompletionStatusType & completionStatus,
    WaitCondition          condition )
{
    CompletionStatusType::iterator i = completionStatus.begin();
    const CompletionStatusType::const_iterator iEnd = completionStatus.end();
    int numDone    = 0;
    int numNotDone = 0;
    for ( ; i != iEnd;  i++) {
        unsigned short carmaAntNo = i->first;
        bool done = i->second;
        if (done) {
            numDone++;
        } else { // Not previously ready, let's check
            bool newDone = false;
            // If an antenna is removed from the subarray, then we get
            // an exception when trying to get the calibrator group.
            // We interpret that as an action complete.
            CalibratorGroup cg;
            try {
                cg = getCalibratorGroupForCarmaAntNo("wait", carmaAntNo );
            }
            catch (const BaseException& e) {
                newDone = true;
            }
            if (!newDone) {
                if (cg.empty()) {
                    ostringstream o;
                    o << "No calibratorHandle for carma ant#" << carmaAntNo;
                    throw CARMA_EXCEPTION(UserException, o.str().c_str());
                }
                CalibratorHandle* c = *(cg.begin());
                if (c == 0) {
                    ostringstream o;
                    o << "Invalid calibrator handle for carma antennna #"
                    << carmaAntNo;
                    throw CARMA_EXCEPTION(UserException, o.str().c_str());
                }
                try {
                    newDone = c->isActionComplete(mon, invalidDataLimit);
                } catch ( const BaseException & e ) {
                    {
                        ostringstream oss2;
                        oss2 << "Caught exception coming out of "
                             << "CalibratorHandle::isActionComplete() and "
                             << "rethrowing as an InvalidMonitorDataException: "
                             << getStringForCaught();
                        programLogErrorIfPossible( oss2.str() );
                    }

                    ostringstream oss;
                    oss << "waitCalibrator: Monitor data invalid for carma antenna #"
                        << carmaAntNo << " for "
                        << invalidDataLimit << " consecutive frames";
                    throw CARMA_EXCEPTION( InvalidMonitorDataException,
                                       oss.str().c_str() );
                } // End catch
            }     // End if !newDone
            // Update based on new result
            if (newDone) {
                numDone++;
                i->second = true;
            }
            else {
                numNotDone++;
            }
        } // End if(done)
    } // End for loop over antennas

    //waitDebugger(completionStatus, "Cal", passCount, numDone, numNotDone);

    if ((condition == WAIT_SINGLE) && (numDone >= 1)) return true;
    return (numNotDone == 0);
}

// Helper method; returns true if done waiting, throws on errors
bool
SubarrayControlImpl::waitTuned(
    const MonitorSystem &  mon,
    const int              passCount,
    const int              invalidDataLimit,
    CompletionStatusType & completionStatus,
    WaitCondition          condition )
{
    CompletionStatusType::iterator i = completionStatus.begin();
    const CompletionStatusType::const_iterator iEnd = completionStatus.end();
    int numDone    = 0;
    int numNotDone = 0;
    for ( ; i != iEnd;  i++) {
        unsigned short carmaAntNo = i->first;
        bool done = i->second;
        if (done) {
            numDone++;
        }
        else { // Not previously ready, let's check
            bool newDone = false;
            // If an antenna is removed from the subarray, then we get
            // an exception when trying to get the rx selector group.
            // We interpret that as an action complete.
            RxSelectorGroup rg;
            try {
                CarmaAntNoSeq antSeq;
                antSeq.length(1);
                antSeq[0] = carmaAntNo;
                rg = getRxSelectorGroup("wait", antSeq);
            }
            catch (const BaseException& e) {
                newDone = true;
            }
            if (!newDone) {
                if (rg.empty()) {
                    ostringstream o;
                    o << "No rxSelectorHandle for carma ant#" << carmaAntNo;
                    throw CARMA_ERROR(o);
                }
            }
            RxSelectorHandle* r;
            if (!newDone) {
                r = *(rg.begin());
                if (r == 0) {
                    ostringstream o;
                    o << "Invalid receiver handle for carma antennna #"
                      << carmaAntNo;
                    throw CARMA_ERROR(o);
                }
            }
            if (!newDone) {
                try {
                    newDone = r->isActionComplete(mon, invalidDataLimit);
                } catch (const BaseException& e) {
                    {
                        ostringstream oss2;
                        oss2 << "Caught exception coming out of "
                             << "RxSelectorHandle::isActionComplete() and "
                             << "rethrowing as an InvalidMonitorDataException: "
                             << getStringForCaught();
                        programLogErrorIfPossible(oss2.str());
                    }

                    ostringstream oss;
                    oss << "waitTuned: Monitor data invalid for carma antenna #"
                        << carmaAntNo << " for "
                        << invalidDataLimit << " consecutive frames";
                    throw CARMA_EXCEPTION( InvalidMonitorDataException,
                                       oss.str().c_str() );
                } // End catch
            }     // End if !newDone
            // Update based on new result
            if (newDone) {
                numDone++;
                i->second = true;
            }
            else {
                numNotDone++;
            }
        } // End if(done)
    } // End for loop over antennas

    //waitDebugger(completionStatus, "Tune", passCount, numDone, numNotDone);

    if ((condition == WAIT_SINGLE) && (numDone >= 1)) return true;
    return (numNotDone == 0);
}

// Helper method; returns true if done waiting, throws on errors
bool SubarrayControlImpl::waitCorrelator(
    const monitor::MonitorSystem & mon,
    const int passCount,
    const int invalidDataLimit,
    CompletionStatusType & completionStatus,
    const WaitCondition condition )
{
    ScopedLogNdc ndc("SubarrayControlImpl::waitCorrelator");
    const bool debug = false;

    if ( debug ) {
        ostringstream oss;
        oss << "( "
            << "passCount=" << passCount << ", "
            << "invalidDataLimit=" << invalidDataLimit << ", "
            << "completionStatus.size()=" << completionStatus.size() << ", "
            << "waitCondition=" << condition << " ).";
        programLogInfoIfPossible( oss.str() );
    }

    CompletionStatusType::iterator i = completionStatus.begin();
    const CompletionStatusType::const_iterator iEnd = completionStatus.end();
    int numDone    = 0;
    int numNotDone = 0;
    for ( ; i != iEnd;  i++) { // Loop over bands
        unsigned short carmaBandNo = i->first;
        if ( carmaBandNo == 0 ) 
            throw CARMA_ERROR("Correlator band number = 0 not allowed in wait command");
        bool done = i->second;
        if (done) {
            numDone++;
        } else { // Not previously ready, let's check
            bool newDone = false;

            // if SPM is not available, skip the loop.
            if ( signalPathMapper_.get() != 0 ) continue;

            // use the signalpathmapper to get the correlator band
            // numbers for the input astroband number.
            // Note that getCorrelatorGroup() requires a *correlator* band number
            // NOT an astroband number.
            CarmaBandNoSeq * correlatorBandNoSeq
                  = signalPathMapper_->getCorrelatorBandNoSeq( carmaBandNo );
            const CarmaBandNoVec bvec =
                  convertSequenceToVector< CarmaBandNoVec::value_type >( *correlatorBandNoSeq );

            // Interpret an exception as an action complete.
            CorrelatorGroup cg;
            try {
                const bool allowZero = false;
                cg = getCorrelatorGroup( "wait",
                     bvec,
		     getCorrelatorDesignation(),
                     allowZero );
            } catch (const BaseException& e) {
                newDone = true;
            }

            if ( !newDone ) {
                if ( cg.empty( ) ) {
                    ostringstream o;
                    o << "No correlator handle for carma band #" << carmaBandNo;
                    throw CARMA_ERROR(o);
                }
            }
            CorrelatorHandle* c;
            if (!newDone) {
                c = *(cg.begin());
                if (c == 0) {
                    ostringstream o;
                    o << "Invalid correlator handle for carma band #"
                      << carmaBandNo;
                    throw CARMA_ERROR(o);
                }
            }
            if (!newDone) {
                try {
                    newDone = c->isActionComplete( mon, invalidDataLimit );
                } catch (const BaseException& e) {
                    {
                        ostringstream oss2;
                        oss2 << "Caught exception coming out of "
                             << "CorrelatorHandle::isActionComplete() and "
                             << "rethrowing as an InvalidMonitorDataException: "
                             << getStringForCaught();
                        programLogErrorIfPossible(oss2.str());
                    }

                    ostringstream oss;
                    oss << "waitCorrelator: Monitor data invalid for carma "
                        << "band #" << carmaBandNo << " for "
                        << invalidDataLimit << " consecutive frames";
                    throw CARMA_EXCEPTION( InvalidMonitorDataException,
                                       oss.str().c_str() );
                } // End catch
            }     // End if !newDone
            // Update based on new result
            if (newDone) {
                numDone++;
                i->second = true;
            }
            else {
                numNotDone++;
            }
        } // End if(done)
    } // End for loop over bands

    // waitDebugger( completionStatus, "Correlator",
    //              passCount, numDone, numNotDone );

    if ((condition == WAIT_SINGLE) && (numDone >= 1)) return true;
    return (numNotDone == 0);

}

// Adds a message about which antennas are done/not done to the log...
void SubarrayControlImpl::waitDebugger(
        const CompletionStatusType& completionStatus,
        const string& name,
        const int passCount, const int numDone, const int numNotDone)
{
    ostringstream doneAnts, notDoneAnts;
    doneAnts << "[";
    notDoneAnts << "[";
    bool noDoneAnts = true;
    bool noNotDoneAnts = true;
    CompletionStatusType::const_iterator i = completionStatus.begin();
    const CompletionStatusType::const_iterator iEnd =
            completionStatus.end();
    for ( ; i != iEnd;  i++) {
        // Add to the strings with done/notDone antennas
        if (i->second) {
            if (!noDoneAnts) doneAnts << ",";
            noDoneAnts = false;
            doneAnts << i->first;
        }
        else {
            if (!noNotDoneAnts) notDoneAnts << ",";
            noNotDoneAnts = false;
            notDoneAnts << i->first;
        }
    }
    doneAnts << "]";
    notDoneAnts << "]";

    Category& log = Program::getLogger();
    // Using INFO priority becuz DEBUG won't work on pgm:logger
    log << Priority::INFO
        << name << " passes=" << passCount << " "
        << "Done/Total=" << numDone
        << "/" << (numNotDone+numDone)
        << " antsDone/NotDone=" << doneAnts.str()
        << "/" << notDoneAnts.str();
}


AntennaReady *
SubarrayControlImpl::tsys( )
try {
    cmdlog() << "tsys()" ;
    Category& log = Program::getLogger();
    cancelFlag_ = false;
    const float CAL_TMO = 13.0; // 13 secs seems very long to me...

    AntennaReady_var antFailure( new AntennaReady );
    CarmaAntNoVec carmaAntNoVec = getCarmaAntNoVecForAllReachableAntennas();
    antFailure->ready.length(0);
    unsigned int numAnts = carmaAntNoVec.size();
    antFailure->notready.length(numAnts);
    for (unsigned int i=0; i < numAnts; i++) {
        antFailure->notready[i] = carmaAntNoVec.at(i);
    }

    CarmaAntNoSeq allAnts;
    allAnts.length(1);
    allAnts[0] = 0;
    log << Priority::INFO << "allAnts setup";

    AntennaReady_var calReady;
    AntennaReady_var skyReady;

    cal(CalibratorControl::AMBIENT, allAnts);
    calReady =
        wait(control::WAIT_CALIBRATOR, allAnts, CAL_TMO, control::WAIT_ALL);
    if (cancelFlag_) return antFailure;
    sleep(1);
    cal(CalibratorControl::SKY, allAnts);
    skyReady =
        wait(control::WAIT_CALIBRATOR, allAnts, CAL_TMO, control::WAIT_ALL);
    if (cancelFlag_) return antFailure;

    sleep(1);

    AntennaReady_var antReady(new AntennaReady);
    antReady->ready.length(0);
    antReady->notready.length(0);
    for (unsigned int c = 0; c < calReady->ready.length(); c++) {
        for (unsigned int s = 0; s < skyReady->ready.length(); s++) {
            if (calReady->ready[c] == skyReady->ready[s]) {
                int l = antReady->ready.length();
                antReady->ready.length(l+1);
                antReady->ready[l] = calReady->ready[c];
            }
        }
    }
    for (unsigned int a = 0; a < numAnts; a++) {
        bool thisOneNotReady = true;
        unsigned int numReady = antReady->ready.length();
        if(false)log << Priority::INFO << "a=" << a << "/" << numAnts
                     << "   numReady=" << numReady;
        for (unsigned int r = 0; r < numReady; r++) {
            if(false)log << Priority::INFO << "r=" << r
                         << "   rant=" << antReady->ready[r]
                         << " aant=" << carmaAntNoVec[a];
            if (antReady->ready[r] == carmaAntNoVec[a]) {
                thisOneNotReady = false;
            }
        }
        if (thisOneNotReady) {
            unsigned int l = antReady->notready.length();
            antReady->notready.length(l+1);
            antReady->notready[l] = carmaAntNoVec.at(a);
        }
    }
    return antReady._retn();
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}

void
SubarrayControlImpl::resetTsys( const carma::control::SeqShort & carmaAntNoSeq )
try {

    if ( carmaAntNoSeq.length( ) == 0 ) return;
    
    SeqShort localCarmaAntNoSeq;
    if ( carmaAntNoSeq.length() == 1 && carmaAntNoSeq[ 0 ] == 0 ) {
        const CarmaAntNoVec allAntsVec = getCarmaAntNoVecForAllAntennas( );
        assignVectorToSequence( allAntsVec, localCarmaAntNoSeq ); 
    } else {
        localCarmaAntNoSeq = carmaAntNoSeq;
    }

    WorkResultSet wrs( "PipelineControl::resetTsys cmd" );

    const string paramString = "< None >";
    const string commandString = "resetTsys";

    queueFunctorWorkRequestGroup(
        "PipelineControl::resetTsys()",
        makeHandleMethodFunctorGroup(
            getPipelineGroup( ),
            &PipelineHandle::resetTsys,
            localCarmaAntNoSeq ),
        wrs,
        *workerPool_ );

    waitForAllNormal( wrs );

} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}

void
SubarrayControlImpl::cancel( )
try
{
    cmdlog() << "cancel()";

    const string paramString = "< None >";

    cancelFlag_ = true;

    WorkResultSet wrs( "PipelineControl::stopIntegration result set" );

    queueFunctorWorkRequestGroup(
        "PipelineControl::stopIntegration()",
        makeRemoteObjMethodFunctorGroup(
            getPipelineGroup(),
            "stopIntegration",
            paramString,
            &PipelineControl::stopIntegration ),
        wrs,
        *workerPool_ );

    waitForAllNormal( wrs );
} catch ( ... ) {
    rethrowCaughtAsUser();
}
