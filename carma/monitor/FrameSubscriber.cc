/**
 * A class which defines how to process TransportSubsystemFrames received
 * by from the notification server.
 *
 *  @author N. S. Amarnath
 *  @version $Revision: 1.59 $, $Date: 2014/07/09 16:31:46 $
 */


#include "carma/monitor/FrameSubscriber.h"

#include <iomanip>
#include <map>
#include <set>
#include <sstream>
#include <vector>
#include <limits>

#include "carma/corba/corba.h"
#include "carma/dbms/TagIDAuthority.h"
#include "carma/util/demangle.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/IllegalStateException.h"
#include "carma/util/programLogging.h"
#include "carma/util/rangeFormatting.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/stlContainerUtils.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/monitorPointSpecializations.h"
#include "carma/monitor/MonitorPointSet.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/SystemFrameBuffer.h"
#include "carma/monitor/SystemThresholdFrameBuffer.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


namespace {


struct DiscardedSubsyInfo {
    ushort subsyId;
    long   subsysFrameTime;
    long   subsysPublishTime;
};

typedef vector< DiscardedSubsyInfo > DiscardedSubsyInfoVec;
typedef multiset<ushort> SubsysIdMultiset;
typedef dbms::TagIDAuthority TAGID_AUTH;

/* Utility that produces a string with subsystem names separated by commas.
 * Don't know why a multiset is needed (multisets allow multiple
 * elements to have the same value) - this means we would have
 * repeat subsystem ID's. 
 */
string
stringForSubsysIdMultiset(const TAGID_AUTH&       authority,
                          const SubsysIdMultiset& subsysIdMultiset )
{
    multiset< string > nonindexed;
    map< string, multiset< size_t > > indexed;
    {
        SubsysIdMultiset::const_iterator i = subsysIdMultiset.begin();
        const SubsysIdMultiset::const_iterator iEnd = subsysIdMultiset.end();
        
        for ( ; i != iEnd; ++i ) {
            string name;
            try {
                name = authority.lookupName( *i );
            } catch ( ... ) {
                // Just stifle any exception
            }

            if ( name.empty() ) {
                nonindexed.insert( name );                
                continue;
            }
    
            const string::size_type nameSize = name.size();
            
            const string::size_type lastNondigit =
                name.find_last_not_of( "0123456789" );
    
            if ( lastNondigit == (nameSize - 1) ) {
                nonindexed.insert( name );                
                continue;
            }
            
            string::size_type firstRealDigit =
                name.find_first_not_of( '0', (lastNondigit + 1) );
            
            if ( firstRealDigit == string::npos ) {
                // They are all zeros so just use the last one
                firstRealDigit = nameSize - 1;
            }
            
            size_t indexValue = 0;
            
            for ( string::size_type j = firstRealDigit; j < nameSize; ++j )
                indexValue = (indexValue * 10) + (name[ j ] - '0');
            
            indexed[ name.substr( 0, firstRealDigit ) ].insert( indexValue );
        }
    }
    
    string result;

    bool firstOne = true;
    
    {
        map< string, multiset<size_t> >::const_iterator i = indexed.begin();
            
        const map< string, multiset< size_t > >::const_iterator iEnd =
            indexed.end();
            
        for ( ; i != iEnd; ++i ) {
            if ( firstOne )
                firstOne = false;
            else
                result += ",";
                
            result += formatAsRanges( i->second, i->first, string() );
        }
    }
    
    {
        multiset< string >::const_iterator i = nonindexed.begin();
        const multiset< string >::const_iterator iEnd = nonindexed.end();
            
        for ( ; i != iEnd; ++i ) {
            if ( firstOne )
                firstOne = false;
            else
                result += ",";
                
            result += *i;
        }
    }
    
    return result;
}

// Both early and late frames are logged here
void
logDiscardedSubsysFrameInfos(const long                   currFrameTimeToLog,
                             const DiscardedSubsyInfoVec& infos )
{
    if (infos.empty()) return;

    ostringstream oss;
    oss << "Discarded " << infos.size() << " subsys frames"
        << " at system timestamp " << currFrameTimeToLog << " because: ";

    typedef map<long, SubsysIdMultiset>     SubsysPublishTimeMap;
    typedef map<long, SubsysPublishTimeMap> SubsysFrameTimeMap;

    SubsysFrameTimeMap subsysFrameTimeMap;
    {
        DiscardedSubsyInfoVec::const_iterator       i    = infos.begin();
        const DiscardedSubsyInfoVec::const_iterator iEnd = infos.end();

        for ( ; i != iEnd; ++i ) {
            const ushort subsyId           = i->subsyId;
            const long   subsysFrameTime   = i->subsysFrameTime;
            const long   subsysPublishTime = i->subsysPublishTime;

            subsysFrameTimeMap[subsysFrameTime][subsysPublishTime].insert(subsyId);
        }
    }

    const TAGID_AUTH& authority = TAGID_AUTH::getAuthority();

    bool firstSubsysMultiset = true;

    SubsysFrameTimeMap::const_iterator       i    = subsysFrameTimeMap.begin();
    const SubsysFrameTimeMap::const_iterator iEnd = subsysFrameTimeMap.end();

    for ( ; i != iEnd; ++i ) {
        const long subsysFrameTime = i->first;

        SubsysPublishTimeMap::const_iterator j = i->second.begin();
        const SubsysPublishTimeMap::const_iterator jEnd = i->second.end();

        for ( ; j != jEnd; ++j ) {
            const long subsysPublishTime = j->first;
            const bool singleton = sizeIsExactlyOne( j->second );

            if ( firstSubsysMultiset ) oss << "; ";            
            if ( singleton ) oss << "Subsystem ";
            else             oss << "Subsystems ";            
            oss << stringForSubsysIdMultiset( authority, j->second );
            if ( singleton ) oss << " was";
            else             oss << " were";

            if ( subsysFrameTime < currFrameTimeToLog ) {
                oss << " late with a received data frame time of "
                    << subsysFrameTime
                    << " (" << (currFrameTimeToLog - subsysFrameTime)
                    << " frames in the past)"
                    << " published at " << subsysPublishTime;
            } 
            else {
                oss << " early with a received data frame time of "
                    << subsysFrameTime
                    << " (" << (subsysFrameTime - currFrameTimeToLog)
                    << " frames in the future)"
                    << " published at " << subsysPublishTime;
            }

            if ( subsysPublishTime < currFrameTimeToLog ) {
                oss << " (" << (currFrameTimeToLog - subsysPublishTime)
                    << " frames in the past)";
            } else if ( subsysPublishTime == currFrameTimeToLog ) {
                oss << " (system timestamp)";
            } else {
                oss << " (" << (subsysPublishTime - currFrameTimeToLog)
                    << " frames in the future)";
            }
            firstSubsysMultiset = false;
        }
    }
    oss << ".";
    programLogWarnIfPossible( oss.str() );
}


struct Synopsis {
    long                  beginTime;
    long                  endTime;
    size_t                totalDiscards;
    map< size_t, size_t > discardCountMap; // key=#subsystems, value=# frames
    
    Synopsis( );
};


Synopsis::Synopsis( ) :
beginTime( 0 ),
endTime( 0 ),
totalDiscards( 0 ),
discardCountMap( )
{
}


// Initial size of InfosToLog vector
const size_t kAccumReserve = 80;
// Interval for producing a synopsis to the log (frames)
const long kSynopsisFrameCount = 60; // 1200

typedef ScopedLock< pthread_mutex_t > AccumGuardLock;

pthread_mutex_t gAccumGuard = PTHREAD_MUTEX_INITIALIZER;
long                   gAccumCurrFrameTime = 0;
DiscardedSubsyInfoVec* gAccumInfos         = 0;
Synopsis*              gAccumSynopsis      = 0;


void
updateCurrFrameTimeHoldingLock( const long              currFrameTime,
                                DiscardedSubsyInfoVec & infosToLog,
                                long &                  currFrameTimeToLog,
                                Synopsis &              synopsis )
{
    ostringstream o;
    o << "updateCurrFrame: " 
      << "currFrameTime=" << currFrameTime  
      << ", gAccumCurrFrameTime=" << gAccumCurrFrameTime 
      << ",  gAccumSynopsis="; 
    if (gAccumSynopsis != 0) o  << gAccumSynopsis->endTime;
    else                     o << "null";
    o << ", gAccumInfos=";
    if (gAccumInfos != 0) o  << gAccumInfos->empty();
    else                  o << "null";
    //programLogInfoIfPossible(o.str());

    if ( gAccumCurrFrameTime != currFrameTime ) {
        if ((gAccumInfos != 0) && (gAccumInfos->empty() == false) ) {
            // There are some gAccumInfos to process; move them into infosToLog
            infosToLog.reserve( kAccumReserve );

            infosToLog.swap( *gAccumInfos );
            currFrameTimeToLog = gAccumCurrFrameTime;
            
            if ( gAccumSynopsis != 0 ) {
                const size_t discardCount = infosToLog.size();
            
                gAccumSynopsis->totalDiscards += discardCount;
                
                // discardCountMap is keyed by number of discarded frames.
                const map< size_t, size_t >::iterator i =
                    gAccumSynopsis->discardCountMap.find(discardCount);
                    
                if ( i == gAccumSynopsis->discardCountMap.end() )
                    gAccumSynopsis->discardCountMap.
                            insert( make_pair(discardCount, 1));
                else
                    ++(i->second);
            }
        }

        if ( (gAccumSynopsis != 0) &&
             (currFrameTime >= gAccumSynopsis->endTime) ) {
            // Close out the accumulating synopsis by moving it to synopsis
            synopsis.beginTime     = gAccumSynopsis->beginTime;
            synopsis.endTime       = currFrameTime;
            synopsis.totalDiscards = gAccumSynopsis->totalDiscards;
            synopsis.discardCountMap.swap(gAccumSynopsis->discardCountMap);
            
            // Set up the accumulating synopsis for another round
            gAccumSynopsis->beginTime     = currFrameTime;
            gAccumSynopsis->endTime       = currFrameTime + kSynopsisFrameCount;
            gAccumSynopsis->totalDiscards = 0;
            
            if (true) {
                ostringstream o;
                o << "New synopsis: " << currFrameTime <<",  " 
                                      << gAccumSynopsis->endTime;
                programLogInfoIfPossible(o.str());
            }
        }
        
        gAccumCurrFrameTime = currFrameTime;
    }
}

void
logSynopsis(const Synopsis& synopsis)
{
    const bool exitOnNoDiscards = false;
    
    if (exitOnNoDiscards && (synopsis.totalDiscards == 0)) return;
        
    ostringstream oss;    
    if (synopsis.totalDiscards == 0) {
        oss << "No";
    }
    else {
        oss << synopsis.totalDiscards;
    }
    
    if ( synopsis.totalDiscards == 1 )
        oss << " subsys frame";
    else
        oss << " subsys frames";
        
    oss << " discarded between system timestamps "
        << synopsis.beginTime << " and " << (synopsis.endTime - 1) << " ("
        << (synopsis.endTime - synopsis.beginTime) << " frames): ";

    if (synopsis.totalDiscards == 0) {
        programLogInfoIfPossible( oss.str() );
        return;
    }
        
    map< size_t, size_t >::const_iterator i =
        synopsis.discardCountMap.begin();        
    const map< size_t, size_t >::const_iterator iEnd =
        synopsis.discardCountMap.end();

    bool firstOne = true;
    for ( ; i != iEnd; ++i ) {
        if ( firstOne ) oss << ", ";                   
        oss << i->second << " frame period";        
        if ( i->second != 1 ) oss << "s";            
        oss << " discarded " << i->first;
        firstOne = false;
    }
    programLogErrorIfPossible( oss.str() );
}


void
flushDiscardedSubsysFrameInfo( const long currFrameTime )
try {
    DiscardedSubsyInfoVec infosToLog;
    long currFrameTimeToLog = 0;
    Synopsis synopsis;
    
    {
        const AccumGuardLock lock( gAccumGuard );

        updateCurrFrameTimeHoldingLock( currFrameTime,
                                        infosToLog,
                                        currFrameTimeToLog,
                                        synopsis );
    }

    logDiscardedSubsysFrameInfos( currFrameTimeToLog,
                                  infosToLog );

    logSynopsis( synopsis );
} catch ( ... ) {
    try {
        programLogErrorIfPossible(
            "Stifling exception in flushDiscardedSubsysFrameInfo: " +
            getStringForCaught() );
    } catch ( ... ) {
        // Just stifle any exception
    }
    
    // Just stifle any exception
}


void
accumDiscardedSubsysFrameInfo( const ushort subsyId,
                               const long   currFrameTime,
                               const long   subsysFrameTime,
                               const long   subsysPublishTime )
try {
    DiscardedSubsyInfoVec infosToLog;
    long currFrameTimeToLog = 0;
    Synopsis synopsis;

    {
        const AccumGuardLock lock( gAccumGuard );

        updateCurrFrameTimeHoldingLock( currFrameTime,
                                        infosToLog,
                                        currFrameTimeToLog,
                                        synopsis );

        if ( gAccumInfos == 0 ) {
            gAccumInfos = new DiscardedSubsyInfoVec;
            gAccumInfos->reserve( kAccumReserve );
        }

        if ( gAccumSynopsis == 0 ) {
            gAccumSynopsis = new Synopsis;
            
            gAccumSynopsis->beginTime = currFrameTime;
            gAccumSynopsis->endTime = currFrameTime + kSynopsisFrameCount;
        }
        
        DiscardedSubsyInfo info;

        info.subsyId = subsyId;
        info.subsysFrameTime = subsysFrameTime;
        info.subsysPublishTime = subsysPublishTime;

        gAccumInfos->push_back( info );
    }

    logDiscardedSubsysFrameInfos(currFrameTimeToLog, infosToLog);
    logSynopsis( synopsis );
    
} catch ( ... ) {
    try {
        programLogErrorIfPossible(
            "Stifling exception in accumDiscardedSubsysFrameInfo: " +
            getStringForCaught() );
    } catch ( ... ) {
        // Just stifle any exception
    }

    // Just stifle any exception
}


}  // namespace < anonymous >

/**
 * Class to hold the one entry in 
 * the queue of monitor systems 
 * which might be published.
 */
namespace carma {
namespace monitor {

    class MonitorSystemQueueEntry {
	public:

        MonitorSystemQueueEntry( bool raw );
        virtual ~ MonitorSystemQueueEntry();

        void resetMonitorData( );

        std::auto_ptr< MonitorSystem > monitorSystem;
        std::auto_ptr< SystemThresholdFrameBuffer > thresholdBuffer;
        ::std::vector< MonitorPoint * > mpList;
    };

}
}

MonitorSystemQueueEntry::MonitorSystemQueueEntry ( const bool raw ) 
{
    if ( raw ) {
        monitorSystem = auto_ptr<MonitorSystem>( new RawCarmaMonitorSystem() );
    } else {
        monitorSystem = auto_ptr<MonitorSystem>( new CarmaMonitorSystem() );
    }

    thresholdBuffer = auto_ptr<SystemThresholdFrameBuffer>( 
            new SystemThresholdFrameBuffer( *monitorSystem ) );

    MonitorPointIterator mpi( *monitorSystem );
    while ( ++mpi ) {
        MonitorPoint & mp = mpi.getMonitorPoint();

        // mark the point as INVALID_NO_DATA
        mp.setNoData();
        mpList.push_back( &mp );
    }
}

carma::monitor::MonitorSystemQueueEntry::~MonitorSystemQueueEntry () { }

void 
carma::monitor::MonitorSystemQueueEntry::resetMonitorData( ) 
{
    // Mark the monitor point as INVALID_NO_DATA in a faster way than 
    // just calling MonitorSystem::setNoData(). The latter involves 
    // hierarchical calls and is somewhat slow.
    vector< MonitorPoint * >::const_iterator i = mpList.begin( ); 
    const vector< MonitorPoint * >::const_iterator iEnd  = mpList.end( ); 
    for ( ; i != iEnd; ++i ) (*i)->setNoData( );

    // sets publish, collate, read times in SystemFrame to zero.
    monitorSystem->resetTimes( );
    
}


// ----------------------------------------------------------------------
// Based on the FrameSubscriber implementation
// ----------------------------------------------------------------------


FrameSubscriber::FrameSubscriber( const double    delayInS,
                                  const bool      rawMode) :
rawMode_( rawMode ),
initialized_( false ),
lastWriteFrame_( 0 ),
totalFrames_(0),
totalMissedDataFrames_(0),
totalEarlyFrames_(0),
totalLateFrames_(0),
totalMissedFrames_(0),
outOfOrderSubsystemFrames_( 0 ),
duplicateSubsystemFrames_( 0 ),
erroneousNotifications_( 0 )
{
    this->allocateMonitorSystems( delayInS );
}


FrameSubscriber::~FrameSubscriber( )
try {
    const ScopedLogNdc ndc( "FrameSubscriber::~FrameSubscriber" );
    
    programLogInfoIfPossible( "Begin" );    
    deallocateMonitorSystems();
    programLogInfoIfPossible( "End" );
} catch ( ... ) {
    try {
        programLogErrorIfPossible(
            "Stifling exception in FrameSubscriber::~FrameSubscriber - " +
            getStringForCaught() );
    } catch ( ... ) {
        // Just stifle any exceptions
    }

    // Just stifle any exceptions
    return;
}

void FrameSubscriber::clearErrorCounts()
{
    const int v            = 0;  // For flexibility in debugging...
    totalMissedDataFrames_ = v;
    totalEarlyFrames_      = v;
    totalLateFrames_       = v;
    totalMissedFrames_     = v;
    typedef SubsystemCountMap::iterator CountIterator;
    CountIterator i    = receivedSubsystemFrameCount_.begin();
    CountIterator iEnd = receivedSubsystemFrameCount_.end();
    for( ; i != iEnd; ++i) i->second = v;
    i    = missedDataFrameCount_.begin();
    iEnd = missedDataFrameCount_.end();
    for( ; i != iEnd; ++i) i->second = v;
    i    = lateSubsystemFrameCount_.begin();
    iEnd = lateSubsystemFrameCount_.end();
    for( ; i != iEnd; ++i) i->second = v;
    i    = earlySubsystemFrameCount_.begin();
    iEnd = earlySubsystemFrameCount_.end();
    for( ; i != iEnd; ++i) i->second = v;
    i    = missedSubsystemFrameCount_.begin();
    iEnd = missedSubsystemFrameCount_.end();
    for( ; i != iEnd; ++i) i->second = v;
}

bool FrameSubscriber::isMissedDataCountingEnabled(unsigned int ssID)
{
    // from ../dbms/subsystemnames.cc
    const unsigned int delaySSid        = 25;
    const unsigned int dataflowSSid     = 67;
    const unsigned int systemstatusSSid = 76;
    const unsigned int opacitySSid      = 30;
    
    if ((ssID == delaySSid) || (ssID == dataflowSSid)) return false;
    if ((ssID == systemstatusSSid) || (ssID == opacitySSid)) return false;
    // Subsystem FSP has to have received a little bit of data before we count
    if (receivedSubsystemDataCount_[ssID] < 10) return false;
    return true;
}


bool FrameSubscriber::subsystemHasTimestamp(unsigned int ssID)
{
    if (ssID < 24) return true; // All antenna subsystems
    return false;
}

void 
FrameSubscriber::writeMonitorStatsHoldingMsMapLock(MonitorSystem& ms)
{
    const ScopedLogNdc ndc("FrameSubscriber::writeMonitorStats");
    // First write subsystem stats.
    for ( long ss = 0; ss < ms.getActualNumSubsystems(); ++ss ) {
        bool isValid = true;
        try {
            MonitorSubsystem& mss = ms.getChildSubsystem( ss );
            const subsystemIDType ssID = 
                    mss.getMonitorPointSet().getSubsystemID();

            // Use RTTI to set these values after the fact.  
            MonitorPointInt& dataWriteTime = 
                dynamic_cast< MonitorPointInt & >( mss.getMonitorPoint(
                    "MonitorSubsystemStats.dataWriteTime", false ) );
            MonitorPointInt & fspWriteTime = 
                dynamic_cast< MonitorPointInt & >( mss.getMonitorPoint( 
                    "MonitorSubsystemStats.fspWriteTime", false ) );
            MonitorPointInt& frameCollatorRxTime =
                dynamic_cast< MonitorPointInt & >( mss.getMonitorPoint( 
                    "MonitorSubsystemStats.frameCollatorRxTime", false ) );
            MonitorPointInt& lateSsFrameCount = 
                dynamic_cast< MonitorPointInt & >( mss.getMonitorPoint(
                    "MonitorSubsystemStats.lateSubsystemFrames", false ));
            MonitorPointInt& earlySsFrameCount = 
                dynamic_cast< MonitorPointInt & >( mss.getMonitorPoint(
                    "MonitorSubsystemStats.earlySubsystemFrames", false ));
            MonitorPointInt& missedSsFrameCount = 
                dynamic_cast< MonitorPointInt& >( mss.getMonitorPoint(
                    "MonitorSubsystemStats.missedSubsystemFrames", false ));
            MonitorPointInt& missedDataFrameCount = 
                dynamic_cast< MonitorPointInt& >( mss.getMonitorPoint(
                    "MonitorSubsystemStats.missedDataFrames", false ));
            MonitorPointBool& validSubsystem = 
                dynamic_cast<MonitorPointBool&>( mss.getMonitorPoint(
                    "MonitorSubsystemStats.validSubsystem", false ));
                
            double frameTime = Time::MJD( mss.getFrameCount() + 1 );

            if ( ms.getFrameCount() == mss.getFrameCount()) {
                // Only write times for subsystems that reported for this frame
                const double lastWriteTime = mss.getLastWriteTime();
                // And only set the subsystem write time if it was actually set
                if (lastWriteTime > 0) {
                    dataWriteTime.setValue( 
                        static_cast< int >( Time::MILLISECONDS_PER_DAY * 
                            (lastWriteTime - frameTime)), 0);
                    (receivedSubsystemDataCount_[ssID])++;
                }
                if ((lastWriteTime <= 0) || (lastWriteTime < frameTime)) {
                    // We don't count some of the aberrant subsystems
                    if (isMissedDataCountingEnabled(ssID)) {
                        // But we count these
                        (missedDataFrameCount_[ssID])++;
                        totalMissedDataFrames_++;
                        isValid = false;
                    }
                }
                // Subsystems will timestamps must have a valid timestamp
                // or it is counted as missedData. Do not check subsystems
                // that are already invalid or they would be counted twice.
                if (isValid && subsystemHasTimestamp(ssID)) {
                    MonitorPointAbstime& ts = 
                        dynamic_cast<MonitorPointAbstime&>( 
                            mss.getMonitorPoint("timestamp", false));
                    if (!ts.isValid()) { // Check timestamp for validity
                        isValid = false;                    
                        (missedDataFrameCount_[ssID])++;
                        totalMissedDataFrames_++;
                    }
                }
                fspWriteTime.setValue( 
                    static_cast< int >( Time::MILLISECONDS_PER_DAY * 
                            (mss.getPublishTime() - frameTime)), 0);
                frameCollatorRxTime.setValue( 
                    static_cast< int >( Time::MILLISECONDS_PER_DAY * 
                            (mss.getReceiveTime() - frameTime)), 0);
            }
            else {
                // No data is interpreted as a missing frame!
                // Start counting after we have gotten at least one frame.
                if (receivedSubsystemFrameCount_[ssID] > 0) {
                    (missedSubsystemFrameCount_[ssID])++;
                    totalMissedFrames_++;
                    isValid = false;
                }
            }
                
            
            // Always write out subsystem valid and the late/early/missed 
            // counts for all subsystems
            // Warning: Lock order dependency (msMap lock -> ssCountsMutex)
            ScopedPthreadMutexLock scopelock(subsystemDataMutex_);
            lateSsFrameCount.setValue(  lateSubsystemFrameCount_[ssID]);
            earlySsFrameCount.setValue( earlySubsystemFrameCount_[ssID]);
            missedSsFrameCount.setValue(missedSubsystemFrameCount_[ssID]);
            missedDataFrameCount.setValue(missedDataFrameCount_[ssID]);
            validSubsystem.setValue(isValid);

        } catch (...) {
            // this routine is not critical, just informational so stifle
            // any exceptions.
        }
    }

    // Now write collator/system stats.
    try { 
        string mstats = "SystemStatus.MonitorSystemStats.";
        MonitorPointInt & rawWriteTime =
            dynamic_cast< MonitorPointInt & >( ms.getMonitorPoint(
                mstats + "rawWriteTime", false));
        MonitorPointInt & oooSSFrames =
            dynamic_cast< MonitorPointInt & >( ms.getMonitorPoint(
                mstats + "outOfOrderSubsystemFrames", false));
        MonitorPointInt & dupSSFrames =
            dynamic_cast< MonitorPointInt & >( ms.getMonitorPoint(
                mstats + "duplicateSubsystemFrames", false));
        MonitorPointInt & errNotifications =
            dynamic_cast< MonitorPointInt & >( ms.getMonitorPoint(
                mstats + "erroneousNotifications", false));
        MonitorPointInt& totFrames =
            dynamic_cast<MonitorPointInt&>( ms.getMonitorPoint(
                mstats + "totalFrames", false));
        MonitorPointInt& totLateFrames =
            dynamic_cast<MonitorPointInt&>( ms.getMonitorPoint(
                mstats + "totalLateFrames", false));
        MonitorPointInt& totEarlyFrames =
            dynamic_cast<MonitorPointInt&>( ms.getMonitorPoint(
                mstats + "totalEarlyFrames", false));
        MonitorPointInt& totMissedFrames =
            dynamic_cast<MonitorPointInt&>( ms.getMonitorPoint(
                mstats + "totalMissedFrames", false));
        MonitorPointInt& totMissedDataFrames =
            dynamic_cast<MonitorPointInt&>( ms.getMonitorPoint(
                mstats + "totalMissedDataFrames", false));

        const double frameMjd = Time::MJD( ms.getFrameCount() + 1 );
        const double writeMjd = ms.getCollatorWriteTime( );
        rawWriteTime.setValue( static_cast< int >( 
            ( writeMjd - frameMjd ) * Time::MILLISECONDS_PER_DAY ) );
                
        ScopedPthreadMutexLock scopelock( subsystemDataMutex_ );
        oooSSFrames.setValue( outOfOrderSubsystemFrames_ );
        dupSSFrames.setValue( duplicateSubsystemFrames_ );
        errNotifications.setValue( erroneousNotifications_ );
        totFrames.setValue(totalFrames_);
        totLateFrames.setValue(totalLateFrames_);
        totEarlyFrames.setValue(totalEarlyFrames_);
        totMissedFrames.setValue(totalMissedFrames_);
        totMissedDataFrames.setValue(totalMissedDataFrames_);
    } catch (...) {
        // stifle
    }

}


void
FrameSubscriber::writeMonitorSystemToIPQ(const double currentFireTimeMJD,
                                         const double delayInSeconds,
                                         const int clearDelayInFrames) 
{
    ScopedPthreadMutexLock scopelock( msMapMutex_ );

    CARMA_CPTRACE(Trace::TRACE1, "Writing frame " << Time::computeFrame( currentFireTimeMJD ) );
    
    /* 
     * Increment the frame count;
     * conditionally clear all counters when we have given the system
     * long enough to start up and get running smoothly.
     */
    totalFrames_++;
    if (totalFrames_ == clearDelayInFrames) clearErrorCounts();
    
    checkForWriteDiscontinuityHoldingLock( currentFireTimeMJD );

    MonitorSystemMap::iterator oldestElement = msMap_.begin( );

    MonitorSystemQueueEntry* msQueueEntry = oldestElement->second; 
    MonitorSystem & ms = *(msQueueEntry->monitorSystem);
    SystemFrameBuffer & buffer = ms.systemFrameBuffer( );
   
    // set the time markers of what delay the writer used 
    // and at what time it wrote.
    buffer.setCollatorWriteDelay( delayInSeconds );
    buffer.setCollatorWriteTime( Time::MJD( ) );
    writeMonitorStatsHoldingMsMapLock( ms );
    
    // set threshold-related validity flags
    msQueueEntry->thresholdBuffer->calibrateMonitorSystem( );

    // finally, write the system frame to the IPQ
    buffer.write( );

    lastWriteFrame_ = oldestElement->first;

    // Clean up the monitor system data prior to recycling the queue entry
    msQueueEntry->resetMonitorData();

    // Create new map element for current frame time and recycle ms queue entry.
    // Note that this is the **current sample time**, so there is no risk of
    // an FSP sending data for this frame before this routine fires.
    const frameType nextFireFrame = Time::computeFrame( currentFireTimeMJD );

    pair< MonitorSystemMap::iterator, bool > insertResult; 
    
    insertResult = msMap_.insert(
        MonitorSystemMap::value_type( nextFireFrame, msQueueEntry ) );  

    if ( insertResult.second ) { // map insertion succeeded.
        MonitorSystemMap::iterator newElement = insertResult.first;
    
        CARMA_CPTRACE(Trace::TRACE1, "Inserted new frame " <<  newElement->first);

        // Set frame count for recycled monitor system instance we just wrote 
        // to the IPQ to the nextFireFrame.
        MonitorSystem * recycledMs = newElement->second->monitorSystem.get( );
        recycledMs->systemFrameBuffer().setFrameCount( nextFireFrame );

        CARMA_CPTRACE(Trace::TRACE1, "Erasing old frame " <<  oldestElement->first);
        // Erase the old element
        msMap_.erase( oldestElement );

        CARMA_CPTRACE(Trace::TRACE1, "Map size is " <<  msMap_.size() );        
    } 
    else {
        // Insert failed - the only way this can happen is if we get an 
        // invalid currentFireTimeMJD.  Rather than guess what the MJD
        // should be, log it, and continue.  The map will get properly 
        // rebuilt on the next go around but frames will be lost.
        ostringstream err;
        err << "writeMonitorSystemToIPQ - Error inserting new element for "
            << "frame " << nextFireFrame << ".";
        programLogErrorIfPossible( err.str( ) );
    }

    // log what frames have been discarded   
    const MonitorSystemMap::const_iterator newOldestElement = msMap_.begin();
ostringstream err;
err << "newOldestElement->first:" << newOldestElement->first;
//cout << err.str() << endl;
//programLogErrorIfPossible( err.str( ) );
    
    //flushDiscardedSubsysFrameInfo(newOldestElement->first);  

   
}

void
FrameSubscriber::setFirstFireTime( const double firstFireTimeMJD )
{
    if ( initialized_ ) {
        throw CARMA_EXCEPTION( IllegalStateException, 
                               "First fire time has already been set." );
    }

    const frameType firstFireFrame = Time::computeFrame( firstFireTimeMJD );

    {
        ScopedPthreadMutexLock scopelock( msMapMutex_ );

        rebuildMapHoldingLock( firstFireFrame - 1 );

        lastWriteFrame_ = ( msMap_.begin( ) )->first - 1; 

        initialized_ = true;
    }

    ostringstream msg;    
    msg << "First fire time set for frame " << firstFireFrame << ".";
    //programLogInfoIfPossible( msg.str( ) );
}
    
void
FrameSubscriber::checkForWriteDiscontinuityHoldingLock( 
    const double currentFireTimeMJD )
{
    if ( msMap_.empty( ) )
        throw CARMA_EXCEPTION( ErrorException, "Monitor system map is empty!" );
    
    const MonitorSystemMap::iterator currentWriteElement = msMap_.begin( );

    if ( currentWriteElement->first == lastWriteFrame_ + 1 ) {
        return;
    } else {
        ostringstream err;
        err << "Discontinuity detected, current write frame "
            << currentWriteElement->first << " is ";

        if ( currentWriteElement->first < lastWriteFrame_ ) {
            err << "older than last written frame " << lastWriteFrame_ << "."; 
        } else if ( currentWriteElement->first == lastWriteFrame_ ) {
            err << "identical to last written frame.";
        } else {
            err << ( currentWriteElement->first - lastWriteFrame_ )
                << " frames newer than last written.";
        }
    
        err << "  Rebuilding map." << endl;

        programLogErrorIfPossible( err.str( ) );

        const frameType newMapHead = 
            Time::computeFrame( currentFireTimeMJD ) - 1;

        rebuildMapHoldingLock( newMapHead );

        programLogErrorIfPossible( "Map rebuilt." );
    } 

}

// What does this do???
void
FrameSubscriber::rebuildMapHoldingLock( const frameType newHeadFrame ) 
{
    // Our oldest frame (first map element) should be frames queued -1 behind 
    // the currentFireTime frame.
    const frameType firstElementFrame = newHeadFrame - (msMap_.size() - 1);

    // Create list of obsolete frames which no longer belong in the map.
    vector< frameType > obsoleteFrames;

    const MonitorSystemMap::const_iterator iBegin = msMap_.begin( );
    const MonitorSystemMap::const_iterator iEnd = msMap_.end( );
    for ( MonitorSystemMap::const_iterator i = iBegin; i != iEnd; ++i ) {

        if ( i->first < firstElementFrame || i->first > newHeadFrame ) {

            obsoleteFrames.push_back( i->first );
        }
    }

    // Now go through and add the proper contiguous frames using the 
    // pre existing monitor systems from the obsolete frames list.
    for (frameType frame = firstElementFrame; frame <= newHeadFrame; ++frame)
    {
        MonitorSystemMap::iterator pos = msMap_.find( frame );
        
        if ( pos != msMap_.end( ) ) { 
            continue;
        }

        // Add new frame while recycling monitor system from obsolete one. 
        if ( obsoleteFrames.size( ) > 0 ) {
            frameType obsoleteFrame = obsoleteFrames.back( );
            obsoleteFrames.pop_back( );

            MonitorSystemMap::iterator obsoleteElement = 
                msMap_.find( obsoleteFrame );

            if ( obsoleteElement == msMap_.end( ) ) {
                throw CARMA_EXCEPTION( ErrorException, "Element not found." );
            }
            
            obsoleteElement->second->resetMonitorData( );

            pair< MonitorSystemMap::iterator, bool > insertResult;
            
            insertResult = msMap_.insert( 
                MonitorSystemMap::value_type(frame, obsoleteElement->second));

            if ( !insertResult.second ) {
                throw CARMA_EXCEPTION(ErrorException, "Error inserting frame.");
            }

            // Set new frameCount on the buffer
            MonitorSystemMap::iterator newElement = insertResult.first;
            MonitorSystem * ms = newElement->second->monitorSystem.get();
            ms->systemFrameBuffer( ).setFrameCount( frame );

            msMap_.erase( obsoleteElement );
        } else {
            throw CARMA_EXCEPTION( ErrorException, "Expected to find obsolete "
                "frames but obsolete frames list is empty." );
        }
    } 
}

// Called when a frame is received from the notification service
// Does the following:
//  - Increments the # of received frames for the subsystem
//  - Checks queue of monitor systems to find one that matches SS frame
//  - If a match is found, copies in the SS frame data
//  - If no match:
//    - checks for early or late and bumps counters
//    - checks if out of order or duplicate and bumps counters
//  - Compare time of this SS frame with lastReceiveTime for this SS
//      and conditionally bump out-of-order and duplicate-frame counters
//  - Store new lastReceivedTime for this subsystem
void
FrameSubscriber::operator()( const TransportSubsystemFrame & frame )
try {
    const ScopedLogNdc ndc( 
        "FrameSubscriber::operator()( TransportSubsystemFrame )" );

    CPTRACE ( Trace::TRACEALL, "Event data received!!" );

    // Just ignore incoming data until we're initialized
    if ( !initialized_ )  
        return;

    const TransportSubsystemFrame* framePtr = &frame;
    const frameType incomingSubsysFrameTime = framePtr->frameCount;
    const int       ssID                    = framePtr->subsystemID;

    const CORBA::Double receiveTime = Time::MJD();

    // Insert current time as the SS receive time
    memcpy( const_cast< CORBA::Double * >( &(framePtr->receiveTime) ),
            &receiveTime, sizeof( receiveTime ) );

    CPTRACE ( Trace::TRACE4, "Frame id : " << ssID
                << " Frame count: " << incomingSubsysFrameTime);

    ScopedPthreadMutexLock scopelock( msMapMutex_ );
    // Bump the received frame counter
    (receivedSubsystemFrameCount_[ssID])++;

    // find monitor system in queue that matches time of subsystem data rx'ed
    const MonitorSystemMap::iterator matchingMsQueueEntry = 
        msMap_.find(incomingSubsysFrameTime);

    if ( matchingMsQueueEntry != msMap_.end() ) {
        // Found a monitor system that corresponds to time of this subsystem;
        // write subsystem frame to matching monitor system queue entry 
        MonitorSystem* ms = matchingMsQueueEntry->second->monitorSystem.get();
        ms->systemFrameBuffer().writeSubsystemFrame( *framePtr );
    } 
    else {
        // Incoming frame is early or late and should thus be discarded
        
        // First check for aberrant case of an empty monitor system queue
        const MonitorSystemMap::iterator firstElement = msMap_.begin( );
        if ( firstElement == msMap_.end( ) ) 
            throw CARMA_EXCEPTION( ErrorException, "MS Map is empty!" );

        // Get range of times covered by monitor systems in queue
        // The queue is ordered from oldest (begin) to newest (end)
        const MonitorSystemMap::reverse_iterator lastElement = msMap_.rbegin(); 
        // Oldest (smallest time)
        const frameType currFrameTime = firstElement->first;
        // Newest (biggest time)
        const frameType lastFrameTime = lastElement->first;  

        if (incomingSubsysFrameTime > lastFrameTime) { 
            // Frame is early
            ScopedPthreadMutexLock countScopelock( subsystemDataMutex_ );
            ++( earlySubsystemFrameCount_[ssID] );
            ++totalEarlyFrames_;
        } 
        else if (incomingSubsysFrameTime < currFrameTime) { 
            // Late
            ScopedPthreadMutexLock countScopelock(subsystemDataMutex_);
            ++(lateSubsystemFrameCount_[ssID]);
            ++totalLateFrames_;
            // A late frame has already been previously counted as a missed 
            // frame, so we need to correct the missed count.
            // Ignoring the case of dup SS frames arriving in different
            // collator frames.
            (missedSubsystemFrameCount_[ssID])--;
            totalMissedFrames_--;
        }
        
    }

    // Compare lastReceivedTime with time for this SS frame to see if there
    // is a duplicate or out-of-order condition. Dups are only detected if they
    // are contiguous.
    ScopedPthreadMutexLock countScopelock( subsystemDataMutex_ );
    SubsystemFrameMap::iterator lastRxFrameEntry = 
        lastReceivedSubsystemFrames_.find(ssID);

    if (lastRxFrameEntry != lastReceivedSubsystemFrames_.end()) {
        // A last rx'ed frame exists; process missed/other frame
        const frameType lastRxFrame = lastRxFrameEntry->second; 

        if (incomingSubsysFrameTime < lastRxFrame) {
            // Out of order
            ++outOfOrderSubsystemFrames_;
        } 
        else if (incomingSubsysFrameTime == lastRxFrame) {
            // Duplicate
            ++duplicateSubsystemFrames_;
        } 
    }

    // Update the lastReceivedTime for this subsystem
    lastReceivedSubsystemFrames_[ssID] = incomingSubsysFrameTime;

} catch ( ... ) {
    try {
        const string caught( getStringForCaught() );
        programLogErrorIfPossible( "FrameSubscriber::operator()(Transport) -- "
                                   "stifling caught exception: " + caught );
    } catch ( ... ) {
        // just stifle any exception
    }

    // just stifle the exception
}

void
FrameSubscriber::printTimes( double timestamp[ ],
                             int    numTimes ) const
{
    CPTRACE ( Trace::TRACEALL,
            "All times are in milliseconds."
            << endl
            << "Time to get frame from notserv "
            << static_cast<int>(1000*Time::SECONDS_PER_DAY*(timestamp[1] - timestamp[0]))
            << endl
            << "Time to write subsystem frame to local system cache "
            << static_cast<int>(1000*Time::SECONDS_PER_DAY*(timestamp[3] - timestamp[2]))
            << endl
            << "Total time for one loop in collator - "
            << static_cast<int>(1000*Time::SECONDS_PER_DAY*(timestamp[3] - timestamp[0]))
            << endl );
}

// Fill msMap_ with (2*delay+1) empty monitor systems.
void 
FrameSubscriber::allocateMonitorSystems( const double delayInS )
{
    const int queueDepth = 1 + static_cast< unsigned int >( delayInS / 0.5 );

    const frameType currentFrame = Time::computeCurrentFrame( );

    for ( int i = 0; i < queueDepth; i++ ) {
	    // Create a monitor system and get a pointer to it
        MonitorSystemQueueEntry* msqP = 
            new MonitorSystemQueueEntry( rawMode_ );

        if ( msqP == 0 ) {
            ostringstream oss;
            oss << "MonitorSystemQueueEntry queue allocation failed ["
                << i+1 
                << " of " 
                << queueDepth
                << "].";

            throw CARMA_EXCEPTION( ErrorException, oss.str() );
        }

        msMap_[ currentFrame + i ] = msqP;
    }

    ostringstream oss;
    oss << "Successfully created MonitorSystemQueueEntry queue of depth " 
	<< queueDepth;
    programLogNoticeIfPossible( oss.str() );
}

void 
FrameSubscriber::deallocateMonitorSystems()
{
    const MonitorSystemMap::iterator iBegin = msMap_.begin( );
    const MonitorSystemMap::iterator iEnd = msMap_.end( );
    for ( MonitorSystemMap::iterator i = iBegin; i != iEnd; ++i ) {
        delete i->second;
    }
}
