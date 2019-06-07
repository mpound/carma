/**
 *
 * Carma subarray control interface servant implementation.
 * Contains primarily methods having to do with
 * initialization, subarray management, DO handle management,
 * monitor point updating (maybe move to own file).
 *
 * @author: Steve Scott
 *
 * $Id: SubarrayControlImpl.cc,v 1.585 2014/06/18 12:56:39 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */


#include "carma/control/SubarrayControlImpl.h"

#include <iomanip>
#include <sstream>
#include <stdexcept>
#include <cctype>
#include <cstdlib>
#include <cmath>

#include "carma/corba/corba.h"
#include "carma/control/antennaHandleUtils.h"
#include "carma/control/ClockHandle.h"
#include "carma/control/CorrelatorHandle.h"

#include "carma/control/CorrDataRemapperHandle.h"

#include "carma/control/DriveHandle.h"
#include "carma/control/DownconverterHandle.h"
#include "carma/control/errorMsgs.h"
#include "carma/control/FaultHandle.h"
#include "carma/control/HalfSecUpdater.h"
#include "carma/control/AlarmHandle.h"
#include "carma/control/LineLengthHandle.h"
#include "carma/control/LoberotatorHandle.h"
#include "carma/control/LOrefHandle.h"
#include "carma/control/PipelineHandle.h"
#include "carma/control/ProjectDatabaseManagerHandle.h"
#include "carma/control/RxSelectorHandle.h"
#include "carma/control/SignalPathMapperHandle.h"
#include "carma/control/stringUtils.h"
#include "carma/control/SubarrayTrackerThread.h"
#include "carma/control/SatThreadSync.h"
#include "carma/control/VlbiHandle.h"
#include "carma/control/WorkerPool.h"

#include "carma/monitor/Runnable.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/MonitorContainerFileIO.h"
#include "carma/monitor/MonitorPointSet.h"
#include "carma/services/Astro.h"
#include "carma/services/Frequency.h"
#include "carma/services/Length.h"
#include "carma/services/Location.h"
#include "carma/services/padUtils.h"
#include "carma/services/Physical.h"
#include "carma/services/Planet.h"
#include "carma/services/Source.h"
#include "carma/services/SourceCatalog.h"
#include "carma/services/SourceNotFoundException.h"
#include "carma/services/ephemFunctions.h"
#include "carma/services/stringConstants.h"
#include "carma/signalpath/SignalPathMapperControl.h"
#include "carma/util/corbaSequenceUtils.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/FileUtils.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Logger.h"
#include "carma/util/loggingUtils.h"
#include "carma/util/programExtras.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadRWLock.h"
#include "carma/util/rangeFormatting.h"
#include "carma/util/ScopedExclusiveLock.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/ScopedSharedLock.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include "carma/util/ScopedQILockManager.h"
#include "carma/util/WorkResult.h"
#include "carma/util/WorkResultSetWaitError.h"

using namespace ::std;
using namespace log4cpp;
using namespace CORBA;
using namespace carma;
using namespace carma::control;
using namespace carma::interferometry;
using namespace carma::loberotator;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::signalpath;
using namespace carma::services::constants;
using namespace carma::util;

namespace carma  {
namespace control  {

const double CONTROL_AUTO_WRITE_DELAY = 0.15; // 150 ms offset

}  // namespace carma::control
}  // namespace carma


// namespace < anonymous > ==================================
namespace {


const short kMaxValidCarmaAntNo = 23;
const unsigned short kMaintSaNo = 5;


const Trace::TraceLevel kTraceLevel = Trace::TRACE4;

string
generateSubarrayWorkerPoolId( const int saNo ) {
    return SubarrayControlImpl::getAlphanumericSubarrayName( saNo );
}

double
hackFixDec2000( double dec2000 ) {
    while ( dec2000  >  M_PI_2 ) { dec2000 -= 2*M_PI; }
    while ( dec2000  < -M_PI_2 ) { dec2000 += 2*M_PI; }
    return dec2000;
}


void
logSaCIError( const string & commandName,
              const string & message ) {
    const string msg =
        "SaCI: Error detected. " + commandName + " : " + message;

    programLogError( msg );
}


bool
parsePrefixAndNumber( const string & s,
                      const string & prefix,
                      short        & number ) {
    const ::size_t prefixSize = prefix.size();

    if ( s.substr( 0, prefixSize ) != prefix )
        return false;

    const string suffix = s.substr( prefixSize );

    const ::size_t suffixSize = suffix.size();

    if ( suffixSize <= 0 )
        return false;

    const char * suffixBegin = suffix.c_str();
    char * numEnd = 0;

    errno = 0;

    const long value = ::std::strtol( suffixBegin, &numEnd, 10 );

    if ( errno != 0 )
        return false;

    if ( numEnd != (suffixBegin + suffixSize) )
        return false;

    if ( (value < -32768) || (value > 32767) )
        throw runtime_error( "value too large for a short" );

    number = static_cast< short >( value );

    return true;
}


}  // namespace < anonymous > ==================================


SubarrayControlImpl::AntTypeCounts::AntTypeCounts( ) :
ovro( 0 ),
bima( 0 ),
sza( 0 )
{
}




class SubarrayControlImpl::AntManager {
    public:

        AntManager( SubarrayControlImpl* parent,
                    ControlSubsystem &               outgoingControlSubsys,
                    ControlSubsystemBase::Subarray & subarrayContainer );

        AntManager( unsigned short                   saNo,
                    ControlSubsystem &               outgoingControlSubsys,
                    ControlSubsystemBase::Subarray & subarrayContainer );

        virtual ~AntManager( );

        void deleteAllAntennas( );

        struct AddResult {
            CarmaAntNoSet actuallyAdded;
            CarmaAntNoSet alreadyInSubarray;
            CarmaAntNoSet ownedByOtherSubarrays;
        };

        AddResult
        addAntennas( const string &        commandName,
                     const CarmaAntNoVec & carmaAntNoVec,
                     bool                  ignoreDupes,
                     MonitorSystem &       incomingCms,
                     Observatory &         obs,
                     bool                  skipAntsOwnedByOthers,
                     bool                  saInitialized );

        AddResult
        addAntennas( const string &        commandName,
                     const CarmaAntNoSeq & carmaAntNoSeq,
                     bool                  ignoreDupes,
                     MonitorSystem &       incomingCms,
                     Observatory &         obs,
                     bool                  skipAntsOwnedByOthers,
                     bool                  saInitialized );

        struct RemoveResult {
            CarmaAntNoSet actuallyRemoved;
            CarmaAntNoSet alreadyInMaint;
            CarmaAntNoSet ownedByOtherSubarrays;
        };

        RemoveResult
        removeAntennas( const string &        commandName,
                        const CarmaAntNoVec & carmaAntNoVec,
                        bool                  allowZero,
                        bool                  ignoreDupes,
                        MonitorSystem &       incomingCms,
                        bool                  skipAntsNotOwnedByMe );

        RemoveResult
        removeAntennas( const string &        commandName,
                        const CarmaAntNoSeq & carmaAntNoSeq,
                        bool                  allowZero,
                        bool                  ignoreDupes,
                        MonitorSystem &       incomingCms,
                        bool                  skipAntsNotOwnedByMe );

        AntControlsGroup getGroup( ) const;

        AntControlsGroup
        getGroupForCarmaAntNoVec(
            const string &        commandName,
            const CarmaAntNoVec & carmaAntNoVec,
            bool                  allowZero,
            bool                  ignoreDupes,
            bool                  skipAntsNotOwnedByMe ) const;

        AntControlsGroup
        getGroupForCarmaAntNoSeq(
            const string &        commandName,
            const CarmaAntNoSeq & carmaAntNoSeq,
            bool                  allowZero,
            bool                  ignoreDupes,
            bool                  skipAntsNotOwnedByMe ) const;

        AntControlsGroup
        getGroupForCarmaAntNo( const string & commandName,
                               CORBA::Short   carmaAntNo,
                               bool           allowEmptyResult ) const;

        CarmaAntNoVec getCarmaAntNoVecForAllAntennas( ) const;
        // Reachable in the drive control sense
        CarmaAntNoVec getCarmaAntNoVecForAllReachableAntennas( ) const;

        struct Assignment {
            short  carmaAntNo;
            string carmaAntName;
            string typedAntName;
        };

        typedef vector< Assignment > AssignmentVec;

        AssignmentVec getAssignments( ) const;

    private:
        // No copying
        AntManager( const AntManager & rhs );
        AntManager & operator=( const AntManager & rhs );

        typedef ScopedSharedLock< PthreadRWLock > ScopedReadLock;
        typedef ScopedExclusiveLock< PthreadRWLock > ScopedWriteLock;

    public:

        static AntennaControls::PersistentInfo
        retrieveAntPersistentInfo( const short           carmaAntNo,
                                   const MonitorSystem & incomingCms,
                                   Observatory &         obs );

    private:

        static CarmaAntNoSet getCarmaAntNoSetForCarmaAntNoVec(
            const string &        commandName,
            const CarmaAntNoVec & carmaAntNoVec,
            const bool            ignoreDupes );

        SubarrayControlImpl* parent_;
        const unsigned short             saNo_;
        ControlSubsystem &               outgoingControlSubsys_;
        ControlSubsystemBase::Subarray & subarrayContainer_;

        mutable PthreadRWLock guard_;

        AntControlsGroup saAnts_;
};

SubarrayControlImpl::AntManager::AntManager(
    SubarrayControlImpl* parent,
    ControlSubsystem &               outgoingControlSubsys,
    ControlSubsystemBase::Subarray & subarrayContainer ) :
parent_(parent),
saNo_( parent->subarrayNo_ ),
outgoingControlSubsys_( outgoingControlSubsys ),
subarrayContainer_( subarrayContainer ),
saAnts_()
{
    subarrayContainer_.numberOfAntennas().setValue( saAnts_.size() );
}

SubarrayControlImpl::AntManager::AntManager(
    const unsigned short             saNo,
    ControlSubsystem &               outgoingControlSubsys,
    ControlSubsystemBase::Subarray & subarrayContainer ) :
saNo_( saNo ),
outgoingControlSubsys_( outgoingControlSubsys ),
subarrayContainer_( subarrayContainer ),
saAnts_()
{
    subarrayContainer_.numberOfAntennas().setValue( saAnts_.size() );
}


SubarrayControlImpl::AntManager::~AntManager( )
try {
    deleteAllAntennas();
} catch ( ... ) {
    // Just stifle any exception

    return;
}


SubarrayControlImpl::CarmaAntNoSet
SubarrayControlImpl::AntManager::getCarmaAntNoSetForCarmaAntNoVec(
    const string &        commandName,
    const CarmaAntNoVec & carmaAntNoVec,
    const bool            ignoreDupes )
{
    CarmaAntNoSet result;
    {
        bool invalidZero = false;
        CarmaAntNoSet negatives;
        CarmaAntNoSet nonexistents;
        CarmaAntNoSet badDupes;

        {
            CarmaAntNoVec::const_iterator i = carmaAntNoVec.begin();
            const CarmaAntNoVec::const_iterator iEnd = carmaAntNoVec.end();

            for ( ; i != iEnd; ++i ) {
                const short carmaAntNo = *i;

                if ( carmaAntNo == 0 ) {
                    invalidZero = true;

                    continue;
                }

                if ( carmaAntNo < 0 ) {
                    negatives.insert( carmaAntNo );

                    continue;
                }

                if ( carmaAntNo > kMaxValidCarmaAntNo ) {
                    nonexistents.insert( carmaAntNo );

                    continue;
                }

                const bool wasInserted = result.insert( carmaAntNo ).second;

                if ( (wasInserted == false) && (ignoreDupes == false) )
                    badDupes.insert( carmaAntNo );
            }
        }

        if ( invalidZero ||
             (negatives.empty() == false) ||
             (nonexistents.empty() == false) ||
             (badDupes.empty() == false) ) {
            string message;
            {
                ostringstream oss;

                oss << "Error -";

                if ( invalidZero ) {
                    oss << " Carma antenna number 0 is invalid because it is"
                        << " not the only entry in the list (if it was the"
                        << " only entry it would indicate that the "
                        << commandName << " command should should be applied"
                        << " to all antennas that are assigned to this"
                        << " subarray).";
                }

                if ( negatives.empty() == false ) {
                    if ( negatives.size() == 1 ) {
                        oss << " Negative carma antenna number "
                            << formatAsRanges( negatives )
                            << " is invalid.";
                    } else {
                        oss << " Negative carma antenna numbers "
                            << formatAsRanges( negatives )
                            << " are invalid.";
                    }
                }

                if ( nonexistents.empty() == false ) {
                    if ( nonexistents.size() == 1 ) {
                        oss << " Carma antenna number "
                            << formatAsRanges( nonexistents )
                            << " is invalid.";
                    } else {
                        oss << " Carma antenna numbers "
                            << formatAsRanges( nonexistents )
                            << " are invalid.";
                    }
                }

                if ( badDupes.empty() == false ) {
                    if ( badDupes.size() == 1 ) {
                        oss << " Carma antenna number "
                            << formatAsRanges( badDupes )
                            << " is duplicated.";
                    } else {
                        oss << " Carma antenna numbers "
                            << formatAsRanges( badDupes )
                            << " are duplicated.";
                    }
                }

                message = oss.str();
            }

            logSaCIError( commandName, message );

            throw CARMA_ERROR( message );
        }
    }

    return result;
}


SubarrayControlImpl::AntManager::AddResult
SubarrayControlImpl::AntManager::addAntennas(
    const string &        commandName,
    const CarmaAntNoVec & carmaAntNoVec,
    const bool            ignoreDupes,
    MonitorSystem &       incomingCms,
    Observatory &         obs,
    const bool            skipAntsOwnedByOthers,
    const bool            saInitialized )
{
  const ScopedLogNdc ndc( "SaCI::AntManager::addAntennas()" );
  
  const CarmaAntNoSet carmaAntNoSet =
    getCarmaAntNoSetForCarmaAntNoVec( commandName,
                      carmaAntNoVec,
                      ignoreDupes );
  
  AddResult result;
  
  {
    const ScopedWriteLock writeLock( guard_ );
    
    CarmaAntNoSet actuallyAdded;
    CarmaAntNoSet alreadyInSaAccordingToMemory;
    CarmaAntNoSet ownedByOthers;
    
    // Filter out things that are already in this subarray
    {
      CarmaAntNoSet::const_iterator i = carmaAntNoSet.begin();
      const CarmaAntNoSet::const_iterator iEnd = carmaAntNoSet.end();
      
      for ( ; i != iEnd; ++i ) {
        const short carmaAntNo = *i;
        
        AntControlsGroup::iterator j = saAnts_.begin();
        const AntControlsGroup::iterator jEnd = saAnts_.end();
        
        for ( ; j != jEnd; ++j ) {
          const AntennaControls * const antControls = *j;
          
          if ( antControls == 0 )
            continue;
          
          if ( antControls->getCarmaAntennaNo() == carmaAntNo )
            break;
        }
        
        if ( j != jEnd )
          alreadyInSaAccordingToMemory.insert( carmaAntNo );
        else
          actuallyAdded.insert( carmaAntNo );
      }
      
      if ( alreadyInSaAccordingToMemory.empty() == false ) {
    string msg;
    
    msg += formatAsRanges( alreadyInSaAccordingToMemory, "C", "" );
    msg += " ";
    
    if ( alreadyInSaAccordingToMemory.size() == 1 )
      msg += "is";
    else
      msg += "are";
    
    msg += " already in this subarray";
    msg += " according to in-memory data structures";
    
    programLogWarnIfPossible( msg );
      }
    }
    
    const string saName = getSubarrayName( saNo_ );
    
    incomingCms.readNewestConditionalCopy();
    
    // Check that all ones we need to add are coming from the maintenance
    // subarray or are still listed as being in this subarray (maybe we
    // crashed and left them marked as in this subarray?)
    {
      typedef map< string, CarmaAntNoSet > OwnedByOthersMap;
      
      OwnedByOthersMap ownedByOthersAccordingToIncomingCms;
      CarmaAntNoSet ownedByMeAccordingToIncomingCms;
      {
        const string maintSaName = getSubarrayName( kMaintSaNo );
        
        ControlSubsystem & incomingControlSubsys = incomingCms.control();
        
        CarmaAntNoSet::const_iterator i = actuallyAdded.begin();
        const CarmaAntNoSet::const_iterator iEnd = actuallyAdded.end();
        
        for ( ; i != iEnd; ++i ) {
          const short carmaAntNo = *i;
          
          const ControlSubsystemBase::Antenna & incomingControlSubsysAnt =
            incomingControlSubsys.antenna( carmaAntNo -  1 );
          
          const int antSaNo =
            incomingControlSubsysAnt.subarrayNumber().getValue();
          
          const string antSaName =
            incomingControlSubsysAnt.subarrayName().getValue();
          
          if ( ((antSaNo == 0) || (antSaNo == kMaintSaNo)) &&
               (antSaName.empty() || (antSaName == maintSaName)) ) {
            // okay to add as it is in the maintenance subarray
          } else if ( ((antSaNo != 0) && (antSaNo != saNo_)) ||
                  ((antSaName.empty() == false) && (antSaName != saName)) ) {
            // Problem. Someone else is listed as the owner
            
            ownedByOthers.insert( carmaAntNo );
            
            string other;
            {
              ostringstream oss;
              
              oss << antSaName << " (" << antSaNo << ")";
              
              other = oss.str();
            }
            
            OwnedByOthersMap::iterator j =
              ownedByOthersAccordingToIncomingCms.find( other );
            
            if ( j == ownedByOthersAccordingToIncomingCms.end() ) {
              CarmaAntNoSet s;
              
              s.insert( carmaAntNo );
              
              ownedByOthersAccordingToIncomingCms.insert(
                                 make_pair( other, s ) );
            } else
              j->second.insert( carmaAntNo );
          } else {
            // okay to add as it still listed as being in this subarray
            
            ownedByMeAccordingToIncomingCms.insert( carmaAntNo );
          }
        }
      }
      
      if ( ownedByMeAccordingToIncomingCms.empty() == false ) {
        string msg;
        
        msg += formatAsRanges( ownedByMeAccordingToIncomingCms, "C", "" );
        msg += " ";
        
        if ( ownedByMeAccordingToIncomingCms.size() == 1 )
          msg += "is";
        else
          msg += "are";
        
        msg += " already listed as owned by this subarray";
        msg += " according to the incoming monitor system";
        
        programLogWarnIfPossible( msg );
      }
      
      if ( ownedByOthers.empty() == false ) {
        string msg = "According to the incoming monitor system ";
        {
          bool firstOne = true;
          
          OwnedByOthersMap::const_iterator j =
            ownedByOthersAccordingToIncomingCms.begin();
          
          const OwnedByOthersMap::const_iterator jEnd =
            ownedByOthersAccordingToIncomingCms.end();
          
          for ( ; j != jEnd; ++j ) {
            if ( firstOne )
              firstOne = false;
            else
              msg += " and ";
            
            msg += formatAsRanges( j->second, "C", "" );
            msg += " ";
            
            if ( j->second.size() == 1 )
              msg += "is";
            else
              msg += "are";
            
            msg += " already owned by subarray ";
            msg += j->first;
          }
        }
        
        if ( skipAntsOwnedByOthers == false ) {
          programLogErrorIfPossible( msg );
          
          throw CARMA_ERROR( msg );
        }
        
        programLogInfoIfPossible( msg );
        
        // Remove them from the ones to actually add
        {
          CarmaAntNoSet::const_iterator j =
            ownedByOthers.begin();
          
          const CarmaAntNoSet::const_iterator jEnd =
            ownedByOthers.end();
          
          for ( ; j != jEnd; ++j )
            actuallyAdded.erase( *j );
        }
      }
    }
    
    // Okay go ahead and add them

    {
      for(CarmaAntNoSet::const_iterator i=actuallyAdded.begin(); i != actuallyAdded.end(); ++i) 
      {
        const short carmaAntNo = *i;
        
        ControlSubsystemBase::Antenna & outgoingControlSubsysAnt =
          outgoingControlSubsys_.antenna(carmaAntNo - 1);
        
        const AntennaControls::PersistentInfo persistentInfo =
          retrieveAntPersistentInfo(carmaAntNo, incomingCms, obs);
        
        auto_ptr<AntennaControls> ap(new AntennaControls(carmaAntNo,
                                 persistentInfo,
                                 incomingCms,
                                 outgoingControlSubsysAnt)
                         );
        
        saAnts_.insert( ap.get() );
        
        //------------------------------------------------------------
        // assert subarray ownership in the
        // signalpath mapper, and set the LO and LL switches
        // accordingly
        //------------------------------------------------------------
        
        parent_->signalPathMapper_->addAntenna( carmaAntNo, saNo_);

        // We don't need to update the walsh functions, because those are
        // pulled from the SPM by the lobe rotator process every time it
        // updates, so this will happen automatically.
        //
        // Therefore we just set the LO and LL switches according to the
        // subarray number.  Note that SignalPathMapperControl::addAntenna()
        // will throw if the action is not permitted, so getting to this
        // point means the call was successful

        std::vector<carma::switchyard::SwitchPosition> swVec(1);
        swVec[0].switchNo  = carmaAntNo;
        swVec[0].switchPos = saNo_;

        parent_->loSwitchyard_->setSwitches(swVec);
        parent_->llSwitchyard_->setSwitches(swVec);
        
        ap.release();
        
        subarrayContainer_.numberOfAntennas().setValue( saAnts_.size() );
        
        outgoingControlSubsysAnt.subarrayNumber().setValue(saNo_);
        outgoingControlSubsysAnt.subarrayName().setValue(saName);
      }
    }
    
    result.actuallyAdded = actuallyAdded;
    result.alreadyInSubarray = alreadyInSaAccordingToMemory;
    result.ownedByOtherSubarrays = ownedByOthers;
  }

  //------------------------------------------------------------
  // EML: Finally, adding an antenna into the array can cause the
  // Walsh columns to be recalculated.  So we lastly have to update
  // the WC assignments for each correlator band in the calling
  // subarray
  //
  // Note that we can only call updateWalshColumns() once the the
  // writelock has gone out of scope, since updateWalshColumns()
  // eventually calls getAntControlsGroup(), which locks the same
  // mutex
  //------------------------------------------------------------

  if(result.actuallyAdded.size() > 0)
    parent_->updateWalshColumns();

  return result;
}

SubarrayControlImpl::AntManager::AddResult
SubarrayControlImpl::AntManager::addAntennas(
    const string &        commandName,
    const CarmaAntNoSeq & carmaAntNoSeq,
    const bool            ignoreDupes,
    MonitorSystem &       incomingCms,
    Observatory &         obs,
    const bool            skipAntsOwnedByOthers,
    const bool            saInitialized )
{
    const CarmaAntNoVec carmaAntNoVec =
        convertSequenceToVector< CarmaAntNoVec::value_type >( carmaAntNoSeq );

    return addAntennas( commandName,
                        carmaAntNoVec,
                        ignoreDupes,
                        incomingCms,
                        obs,
                        skipAntsOwnedByOthers,
                        saInitialized );
}


SubarrayControlImpl::AntManager::RemoveResult
SubarrayControlImpl::AntManager::removeAntennas(
    const string &        commandName,
    const CarmaAntNoVec & carmaAntNoVec,
    const bool            allowZero,
    const bool            ignoreDupes,
    MonitorSystem &       incomingCms,
    const bool            skipAntsNotOwnedByMe )
{
    const ScopedLogNdc ndc( "SaCI::AntManager::removeAntennas()" );

    const bool doAll =
        allowZero &&                         // constant time check
        (carmaAntNoVec.empty() == false) &&  // constant time check
        (carmaAntNoVec.at( 0 ) == 0) &&      // constant time check
        (carmaAntNoVec.size() == 1);         // linear time check if false

    CarmaAntNoSet actuallyRemoved;
    CarmaAntNoSet alreadyInMaint;
    CarmaAntNoSet ownedByOthers;

    if ( doAll == false ) {
        actuallyRemoved = getCarmaAntNoSetForCarmaAntNoVec( commandName,
                                                            carmaAntNoVec,
                                                            ignoreDupes );
    }

    {
        CarmaAntNoSet notOwnedByMe;

        const ScopedWriteLock writeLock( guard_ );

        // First remove either all of them or none of them from saAnts_
        {
            vector< AntControlsGroup::iterator > itersToErase;
            itersToErase.reserve( kMaxValidCarmaAntNo );

            if ( doAll ) {
                // Get all of them from saAnts_
                AntControlsGroup::iterator i = saAnts_.begin();
                const AntControlsGroup::iterator iEnd = saAnts_.end();

                for ( ; i != iEnd; ++i ) {
                    itersToErase.push_back( i );

                    const AntennaControls * const antControls = *i;

                    if ( antControls == 0 )
                        continue;

                    actuallyRemoved.insert( antControls->getCarmaAntennaNo() );
                }

                string msg;

                if ( actuallyRemoved.empty() && itersToErase.empty() )
                    msg = "Carma antenna number set is empty so removing"
                          " nothing.";
                else {
                    msg = "Removing ";

                    if ( actuallyRemoved.size() == 1 ) {
                        msg += "antenna ";
                        msg += formatAsRanges( actuallyRemoved, "C", "" );
                    } else {
                        msg += "antennas ";
                        msg += formatAsRanges( actuallyRemoved, "C", "" );
                    }

                    msg += ".";
                }

                programLogInfoIfPossible( msg );
            } else {
                // Find all of them in saAnts_
                CarmaAntNoSet::const_iterator i =
                    actuallyRemoved.begin();

                const CarmaAntNoSet::const_iterator iEnd =
                    actuallyRemoved.end();

                for ( ; i != iEnd; ++i ) {
                    const short carmaAntNo = *i;

                    AntControlsGroup::iterator j = saAnts_.begin();
                    const AntControlsGroup::iterator jEnd = saAnts_.end();

                    for ( ; j != jEnd; ++j ) {
                        const AntennaControls * const antControls = *j;

                        if ( antControls == 0 )
                            continue;

                        if ( antControls->getCarmaAntennaNo() != carmaAntNo )
                            continue;

                        break;
                    }

                    if ( j != jEnd )
                        itersToErase.push_back( j );
                    else
                        notOwnedByMe.insert( carmaAntNo );
                }

                if ( notOwnedByMe.empty() == false ) {
                    string msg;
                    {
                        ostringstream oss;

                        if ( notOwnedByMe.size() == 1 ) {
                            oss << "Antenna "
                                << formatAsRanges( notOwnedByMe, "C", "" )
                                << " is not owned by this subarray";
                        } else {
                            oss << "Antennas "
                                << formatAsRanges( notOwnedByMe, "C", "" )
                                << " are not owned by this subarray";
                        }

                        msg = oss.str();
                    }

                    if ( skipAntsNotOwnedByMe == false ) {
                        programLogErrorIfPossible( msg );

                        throw CARMA_ERROR( msg );
                    }

                    programLogInfoIfPossible( msg );

                    // Remove them from the ones to actually remove
                    {
                        CarmaAntNoSet::const_iterator j =
                            notOwnedByMe.begin();

                        const CarmaAntNoSet::const_iterator jEnd =
                            notOwnedByMe.end();

                        for ( ; j != jEnd; ++j )
                            actuallyRemoved.erase( *j );
                    }
                }
            }

            // Erase them from saAnts_ and delete the AntennaControls objects
            if ( itersToErase.empty() == false ) {
                vector< AntControlsGroup::iterator >::const_iterator k =
                    itersToErase.begin();

                const vector< AntControlsGroup::iterator >::const_iterator kEnd =
                    itersToErase.end();

                for ( ; k != kEnd; ++k ) {
                    AntControlsGroup::iterator iter = *k;

                    const AntennaControls * const antControls = *iter;

                    saAnts_.erase( iter );



                // assert subarray ownership in the
                // signalpath mapper, and set the LO and LL switches
                // accordingly
                
                    parent_->signalPathMapper_->removeAntenna(
                             antControls->getCarmaAntennaNo(), 
                             saNo_);

                    try {
                        delete antControls;
                    } catch ( ... ) {
                        const string msg =
                            "Problem deleting AntennaControls object";

                        programLogErrorIfPossible( msg );
                    }
                }
            }

            subarrayContainer_.numberOfAntennas().setValue( saAnts_.size() );
        }

        // Now modify all the control subsystem info about them
        if ( actuallyRemoved.empty() == false ) {
            const string saName = getSubarrayName( saNo_ );

            const string maintSaName = getSubarrayName( kMaintSaNo );

            CarmaAntNoSet::const_iterator i = actuallyRemoved.begin();
            const CarmaAntNoSet::const_iterator iEnd = actuallyRemoved.end();

            for ( ; i != iEnd; ++i ) {
                const short carmaAntNo = *i;

                try {
                    ControlSubsystemBase::Antenna& outgoingControlSubsysAnt =
                        outgoingControlSubsys_.antenna( carmaAntNo - 1 );

                    MonitorPointString& antSaNameMp =
                        outgoingControlSubsysAnt.subarrayName();

                    const bool antSaNameValid = antSaNameMp.isValid();
                    const string antSaName = antSaNameMp.getValue();

                    MonitorPointInt& antSaNoMp =
                        outgoingControlSubsysAnt.subarrayNumber();

                    const bool antSaNoValid = antSaNoMp.isValid();
                    const int antSaNo = antSaNoMp.getValue();

                    if ( ((antSaNameValid == false) || (antSaName == saName)) &&
                         ((antSaNoValid == false) || (antSaNo == saNo_)) ) {
                        antSaNameMp.setValue( maintSaName );
                        antSaNoMp.setValue( kMaintSaNo );
                    } else {
                        string msg;
                        {
                            ostringstream oss;

                            oss << "C" << carmaAntNo << " is marked as not"
                                << " being owned by this subarray"
                                << " (" << saName << "/" << saNo_ << ")"
                                << " but instead marked with";

                            if ( antSaNameValid ) oss << " a valid";
                            else oss << " an invalid";
                            oss << " subarray name of " << antSaName
                                << " and";
                            if ( antSaNoValid ) oss << " a valid";
                            else oss << " an invalid";
                            oss << " subarray number of " << antSaNo << ".";
                            msg = oss.str();
                        }
                        programLogErrorIfPossible( msg );
                    }
                } catch ( ... ) {
                    string msg;
                    {
                        ostringstream oss;
                        oss << "Problem trying to update control subsystem"
                            << " info for C" << carmaAntNo << ": "
                            << getStringForCaught();
                        msg = oss.str();
                    }
                    programLogErrorIfPossible( msg );
                }
            }
        }

        // sort out the "not owned by me" cases
        if ( notOwnedByMe.empty() == false ) {
            incomingCms.readNewestConditionalCopy();

            const string maintSaName = getSubarrayName( kMaintSaNo );

            ControlSubsystem & incomingControlSubsys = incomingCms.control();

            CarmaAntNoSet::const_iterator i = notOwnedByMe.begin();
            const CarmaAntNoSet::const_iterator iEnd = notOwnedByMe.end();

            for ( ; i != iEnd; ++i ) {
                const short carmaAntNo = *i;

                const ControlSubsystemBase::Antenna & incomingControlSubsysAnt =
                    incomingControlSubsys.antenna( carmaAntNo -  1 );

                const int antSaNo =
                    incomingControlSubsysAnt.subarrayNumber().getValue();

                const string antSaName =
                    incomingControlSubsysAnt.subarrayName().getValue();

                if ( ((antSaNo == 0) || (antSaNo == kMaintSaNo)) &&
                     (antSaName.empty() || (antSaName == maintSaName)) )
                    alreadyInMaint.insert( carmaAntNo );
                else
                    ownedByOthers.insert( carmaAntNo );
            }
        }
    }

    RemoveResult result;

    result.actuallyRemoved = actuallyRemoved;
    result.alreadyInMaint = alreadyInMaint;
    result.ownedByOtherSubarrays = ownedByOthers;

    return result;
}


SubarrayControlImpl::AntManager::RemoveResult
SubarrayControlImpl::AntManager::removeAntennas(
    const string &        commandName,
    const CarmaAntNoSeq & carmaAntNoSeq,
    const bool            allowZero,
    const bool            ignoreDupes,
    MonitorSystem &       incomingCms,
    const bool            skipAntsNotOwnedByMe )
{
    const CarmaAntNoVec carmaAntNoVec =
        convertSequenceToVector< CarmaAntNoVec::value_type >( carmaAntNoSeq );

    return removeAntennas( commandName,
                           carmaAntNoVec,
                           allowZero,
                           ignoreDupes,
                           incomingCms,
                           skipAntsNotOwnedByMe );
}


void
SubarrayControlImpl::AntManager::deleteAllAntennas( )
{
    const ScopedWriteLock writeLock( guard_ );

    while ( saAnts_.empty() == false ) {
        const AntennaControls * const antControls = *(saAnts_.begin());

        saAnts_.erase( saAnts_.begin() );

        delete antControls;
    }
}


AntennaControls::PersistentInfo
SubarrayControlImpl::AntManager::retrieveAntPersistentInfo(
    const short           carmaAntNo,
    const MonitorSystem & incomingCms,
    Observatory &         obs )
{
    AntennaControls::PersistentInfo result;

    const ControlSubsystemBase::Antenna & ant =
        incomingCms.control().antenna( carmaAntNo - 1 );

    ostringstream oss;

    oss << "Retrieved ";

    {
        const MonitorPointInt & padNoMp = ant.padNumber();

        if ( padNoMp.isValid() == false )
            oss << "no valid pad";
        else {
            const int padNo = padNoMp.getValue();

            if ( padNo == 0 )
                oss << "zero pad";
            else {
                result.pad = obs.getPad( defaultPadName( padNo ) );

                oss << "pad #" << padNo;
            }
        }
    }

    oss << "; ";

    {
        const ControlSubsystemBase::PadOffset & padOffset = ant.padOffset();

        const MonitorPointFloat & eastMp  = padOffset.east();
        const MonitorPointFloat & northMp = padOffset.north();
        const MonitorPointFloat & upMp    = padOffset.up();

        const bool eastMpValid  = eastMp.isValid();
        const bool northMpValid = northMp.isValid();
        const bool upMpValid    = upMp.isValid();

        if ( (eastMpValid == false) &&
             (northMpValid ==false ) &&
             (upMpValid == false) ) {
            oss << "no valid pad offset";
        } else {
            oss << "pad offset of ";

            if ( eastMpValid == false )
                oss << "no valid east";
            else {
                const float east = eastMp.getValue();

                result.eastPadOffset = Length( east, "mm" );

                oss << "east=" << east << "mm";
            }

            oss << ", ";

            if ( northMpValid == false )
                oss << "no valid north";
            else {
                const float north = northMp.getValue();

                result.northPadOffset = Length( north, "mm" );

                oss << "north=" << north << "mm";
            }

            oss << ", ";

            if ( upMpValid == false )
                oss << "no valid up";
            else {
                const float up = upMp.getValue();

                result.upPadOffset = Length( up, "mm" );

                oss << "up=" << up << "mm";
            }
        }
    }

    oss << "; ";

    {
        const ControlSubsystemBase::AntennaOffset & antOffset =
            ant.antennaOffset();

        const MonitorPointFloat & eastMp = antOffset.east();
        const MonitorPointFloat & northMp = antOffset.north();
        const MonitorPointFloat & upMp = antOffset.up();

        const bool eastMpValid = eastMp.isValid();
        const bool northMpValid = northMp.isValid();
        const bool upMpValid = upMp.isValid();

        if ( (eastMpValid == false) &&
             (northMpValid ==false ) &&
             (upMpValid == false) ) {
            oss << "no valid antenna offset";
        } else {
            oss << "antenna offset of ";

            if ( eastMpValid == false )
                oss << "no valid east";
            else {
                const float east = eastMp.getValue();

                result.eastAntennaOffset = Length( east, "mm" );

                oss << "east=" << east << "mm";
            }

            oss << ", ";

            if ( northMpValid == false ) {
                oss << "no valid north";
            } else {
                const float north = northMp.getValue();

                result.northAntennaOffset = Length( north, "mm" );

                oss << "north=" << north << "mm";
            }

            oss << ", ";

            if ( upMpValid == false ) {
                oss << "no valid up";
            } else {
                const float up = upMp.getValue();

                result.upAntennaOffset = Length( up, "mm" );

                oss << "up=" << up << "mm";
            }
        }
    }
    oss << "; ";
    {
        const ControlSubsystemBase::TotalENU & tenu = ant.totalENU();

        const MonitorPointFloat & eastMp = tenu.east();
        const MonitorPointFloat & northMp = tenu.north();
        const MonitorPointFloat & upMp = tenu.up();

        const bool eastMpValid  = eastMp.isValid();
        const bool northMpValid = northMp.isValid();
        const bool upMpValid    = upMp.isValid();

        if ( (eastMpValid == false) &&
             (northMpValid ==false ) &&
             (upMpValid == false) ) {
            oss << "no valid Antenna Total ENU ";
        } else {
            oss << "Antenna Total ENU of ";

            if ( eastMpValid == false )
                oss << "no valid east";
            else {
                const float east = eastMp.getValue();
                result.totalEast = Length( east, "meter" );
                oss << "east=" << east << "m";
            }

            oss << ", ";

            if ( northMpValid == false ) {
                oss << "no valid north";
            } else {
                const float north = northMp.getValue();
                result.totalNorth = Length( north, "meter" );
                oss << "north=" << north << "m";
            }

            oss << ", ";

            if ( upMpValid == false ) {
                oss << "no valid up";
            } else {
                const float up = upMp.getValue();
                result.totalUp = Length( up, "meter" );
                oss << "up=" << up << "m";
            }
        }
        oss << "; ";

        const MonitorPointFloat& axisNonXMP = ant.axisNonintersection();
        if (axisNonXMP.isValid()) {
            float axisNonX = axisNonXMP.getValue();
            result.axisMisalignment = Length(axisNonX, "mm");
            oss << "axisNonX=" << axisNonX << "mm";
        }
        else {
            oss << "no value for axisNonX";
        }
    }


    oss << " for C" << carmaAntNo;

    programLogInfoIfPossible( oss.str() );

    return result;
}


SubarrayControlImpl::AntManager::AssignmentVec
SubarrayControlImpl::AntManager::getAssignments( ) const
{
    typedef map< short, Assignment > AssignmentMap;

    AssignmentMap assignmentMap;
    {
        const ScopedReadLock readLock( guard_ );

        AntControlsGroup::const_iterator i = saAnts_.begin();
        const AntControlsGroup::const_iterator iEnd = saAnts_.end();

        for ( ; i != iEnd; ++i ) {
            const AntennaControls * const antControls = *i;

            if ( antControls == 0 )
                continue;

            const short carmaAntNo = antControls->getCarmaAntennaNo();

            Assignment assignment;

            assignment.carmaAntNo = carmaAntNo;
            assignment.carmaAntName = antControls->getCarmaAntennaName();
            assignment.typedAntName = antControls->getTypedAntennaName();

            assignmentMap.insert( make_pair( carmaAntNo, assignment ) );
        }
    }

    AssignmentVec result;
    {
        result.reserve( assignmentMap.size() );

        AssignmentMap::const_iterator i = assignmentMap.begin();
        const AssignmentMap::const_iterator iEnd = assignmentMap.end();

        for ( ; i != iEnd; ++i )
            result.push_back( i->second );
    }

    return result;
}


SubarrayControlImpl::CarmaAntNoVec
SubarrayControlImpl::AntManager::getCarmaAntNoVecForAllAntennas( ) const
{
    CarmaAntNoVec result;
    result.reserve( kMaxValidCarmaAntNo );

    {
        const ScopedReadLock readLock( guard_ );

        AntControlsGroup::const_iterator i = saAnts_.begin();
        const AntControlsGroup::const_iterator iEnd = saAnts_.end();

        for ( ; i != iEnd; ++i ) {
            const AntennaControls * const antControls = *i;

            if ( antControls == 0 )
                continue;

            result.push_back( antControls->getCarmaAntennaNo() );
        }
    }

    return result;
}


SubarrayControlImpl::CarmaAntNoVec
SubarrayControlImpl::AntManager::getCarmaAntNoVecForAllReachableAntennas() const
{
    CarmaAntNoVec result;
    result.reserve( kMaxValidCarmaAntNo );

    {
        const ScopedReadLock readLock( guard_ );

        AntControlsGroup::const_iterator i = saAnts_.begin();
        const AntControlsGroup::const_iterator iEnd = saAnts_.end();

        for ( ; i != iEnd; ++i ) {
            const AntennaControls * const antControls = *i;

            if ( antControls == 0 )
                continue;

            DriveHandle * const drive = antControls->driveHandle();

            if ( drive == 0 )
                continue;

            if ( drive->isObjReachable() )
                result.push_back( antControls->getCarmaAntennaNo() );
        }
    }

    return result;
}


SubarrayControlImpl::CarmaAntNoVec
SubarrayControlImpl::getCarmaAntNoVecForAllAntennas( ) const
{
    return antManager_->getCarmaAntNoVecForAllAntennas();
}

SubarrayControlImpl::CarmaAntNoVec
SubarrayControlImpl::getCarmaAntNoVec(const CarmaAntNoSeq& seq) const
{
    CarmaAntNoVec v;
    for (unsigned int i=0; i < seq.length(); i++) {
         v.push_back(seq[i]);
    }
    return v;
}


SubarrayControlImpl::CarmaAntNoVec
SubarrayControlImpl::getCarmaAntNoVecForAllReachableAntennas() const
{
    return antManager_->getCarmaAntNoVecForAllReachableAntennas();
}

SubarrayControlImpl::CarmaBandNoVec
SubarrayControlImpl::getCarmaBandNoVecForAllBands( )
{
    //programLogInfoIfPossible("SubarrayControlImpl::getCarmaBandNoVecForAllBands - Entering. SPM->gACB will be called.");
  CarmaBandNoVec result;

  std::vector<carma::signalpath::SignalPathMapperControl::CorrelatorBand> bandVec =
    signalPathMapper_->getActiveCorrelatorBands(csCorrType());

  for(unsigned iCorrBand=0; iCorrBand < bandVec.size(); iCorrBand++) {
    result.push_back( bandVec[iCorrBand].bandNo );
  }

  return result;
}

SubarrayControlImpl::CarmaBandNoVec
SubarrayControlImpl::getCarmaBandNoVecForAllOnlineAndReachableBands( )
{
    CarmaBandNoVec result;
    unsigned short bandIdx;

    std::vector<carma::signalpath::SignalPathMapperControl::CorrelatorBand> bandVec =
            signalPathMapper_->getActiveCorrelatorBands(csCorrType());

  for(unsigned iCorrBand = 0; iCorrBand < bandVec.size(); iCorrBand++) {

    bandIdx                           = bandVec[iCorrBand].bandNo - 1;

    ControlCorrelatorDesignation corrType = bandVec[iCorrBand].crate.type;

    // Get the correct correlator handle for this correlator band

    CorrelatorHandle* const correlatorHandle = (corrType == util::CORR_SPECTRAL) ? 
      slCorrelatorVec_->at( bandIdx ) :
      wbCorrelatorVec_->at( bandIdx );

    if ( correlatorHandle == 0 ) {
      ostringstream msg;
      msg << "correlatorHandle for " << (corrType == util::CORR_SPECTRAL ? "SL" : "WB") 
      << " band #" << bandIdx + 1 << " is NULL.";
      throw CARMA_ERROR( msg.str() );
    }

    if ( !correlatorHandle->isOffline() &&
     correlatorHandle->isObjReachable() )
      result.push_back( bandIdx + 1 );
  }

  return result;
}

SubarrayControlImpl::CarmaBandNoVec
SubarrayControlImpl::getCarmaBandNoVec( const CarmaBandNoSeq & seq ) const
{
    CarmaBandNoVec v;
    for (unsigned int i=0; i < seq.length(); i++) {
         v.push_back(seq[i]);
    }
    return v;
}

SubarrayControlImpl::AntControlsGroup
SubarrayControlImpl::AntManager::getGroup( ) const
{
  AntControlsGroup result;

  {
    const ScopedReadLock readLock( guard_ );
    
    AntControlsGroup::const_iterator i = saAnts_.begin();
    const AntControlsGroup::const_iterator iEnd = saAnts_.end();
    
    for ( ; i != iEnd; ++i ) {
      AntennaControls * const antControls = *i;
      
      if ( antControls == 0 )
    continue;
      
      result.insert( antControls );
    }
  }
  
  return result;
}


SubarrayControlImpl::AntControlsGroup
SubarrayControlImpl::AntManager::getGroupForCarmaAntNoVec(
    const string &        commandName,
    const CarmaAntNoVec & carmaAntNoVec,
    const bool            allowZero,
    const bool            ignoreDupes,
    const bool            skipAntsNotOwnedByMe ) const
{
    if ( allowZero &&                         // constant time check
         (carmaAntNoVec.empty() == false) &&  // constant time check
         (carmaAntNoVec.at( 0 ) == 0) &&      // constant time check
         (carmaAntNoVec.size() == 1) )        // linear time check if false
        return getGroup();

    const CarmaAntNoSet carmaAntNoSet =
        getCarmaAntNoSetForCarmaAntNoVec( commandName,
                                          carmaAntNoVec,
                                          ignoreDupes );

    AntControlsGroup result;
    {
        CarmaAntNoSet unowned;
        {
            const ScopedReadLock readLock( guard_ );

            CarmaAntNoSet::const_iterator i = carmaAntNoSet.begin();
            const CarmaAntNoSet::const_iterator iEnd = carmaAntNoSet.end();

            for ( ; i != iEnd; ++i ) {
                const short carmaAntNo = *i;

                AntennaControls * antControls = 0;
                {
                    AntControlsGroup::const_iterator j =
                        saAnts_.begin();

                    const AntControlsGroup::const_iterator jEnd =
                        saAnts_.end();

                    for ( ; j != jEnd; ++j ) {
                        AntennaControls * const jAntControls = *j;

                        if ( jAntControls == 0 )
                            continue;

                        if ( jAntControls->getCarmaAntennaNo() != carmaAntNo )
                            continue;

                        antControls = jAntControls;
                        break;
                    }
                }

                if ( antControls == 0 )
                    unowned.insert( carmaAntNo );
                else
                    result.insert( antControls );
            }
        }

        if ( unowned.empty() == false ) {
            string message;
            {
                message += "Error - ";
                message += formatAsRanges( unowned, "C", "" );
                message += " ";

                if ( unowned.size() == 1 )
                    message += "is";
                else
                    message += "are";

                message += " not in this subarray";
            }

            logSaCIError( commandName, message );

            if ( skipAntsNotOwnedByMe == false )
                throw CARMA_ERROR( message );
        }
    }

    if ( result.empty() && (carmaAntNoVec.empty() == false) ) {
        string message;

        {
            ostringstream oss;

            oss << "WARNING: carma antenna number vector parameter "
                << "mapped to an empty set of antenna controls pointers.";

            message = oss.str();
        }

        logSaCIError( commandName, message );
    }

    return result;
}


SubarrayControlImpl::AntControlsGroup
SubarrayControlImpl::AntManager::getGroupForCarmaAntNoSeq(
    const string &        commandName,
    const CarmaAntNoSeq & carmaAntNoSeq,
    const bool            allowZero,
    const bool            ignoreDupes,
    const bool            skipAntsNotOwnedByMe ) const
{
    const CarmaAntNoVec carmaAntNoVec =
        convertSequenceToVector< CarmaAntNoVec::value_type >( carmaAntNoSeq );

    return getGroupForCarmaAntNoVec( commandName,
                                     carmaAntNoVec,
                                     allowZero,
                                     ignoreDupes,
                                     skipAntsNotOwnedByMe );
}


SubarrayControlImpl::AntControlsGroup
SubarrayControlImpl::AntManager::getGroupForCarmaAntNo(
    const string &     commandName,
    const CORBA::Short carmaAntNo,
    const bool         allowEmptyResult ) const
{
    if ( carmaAntNo == 0 ) {
        string msg;

        msg += "The ";
        msg += commandName;
        msg += " command does not allow a carma antenna number parameter";
        msg += " value of 0. It can only be applied to a single antenna.";

        logSaCIError( commandName, msg );

        throw CARMA_ERROR( msg );
    }

    if ( carmaAntNo < 0 ) {
        string msg;
        {
            ostringstream oss;

            oss << "Negative carma antenna number (" << carmaAntNo
                << ") requested for command " << commandName << ".";

            msg = oss.str();
        }

        logSaCIError( commandName, msg );

        throw CARMA_ERROR( msg );
    }

    if ( carmaAntNo > kMaxValidCarmaAntNo ) {
        string msg;
        {
            ostringstream oss;

            oss << "Carma antenna number " << carmaAntNo
                << " requested for command " << commandName
                << " and as far as this code knows the only carma antennas"
                << " in existence are C1-C" << kMaxValidCarmaAntNo << ".";

            msg = oss.str();
        }

        logSaCIError( commandName, msg );

        throw CARMA_ERROR( msg );
    }

    AntControlsGroup result;

    {
        const ScopedReadLock readLock( guard_ );

        AntControlsGroup::const_iterator i = saAnts_.begin();
        const AntControlsGroup::const_iterator iEnd = saAnts_.end();

        for ( ; i != iEnd; ++i ) {
            AntennaControls * const antControls = *i;

            if ( antControls == 0 )
                continue;

            if ( antControls->getCarmaAntennaNo() != carmaAntNo )
                continue;

            result.insert( antControls );

            break;
        }
    }

    if ( result.empty() && (allowEmptyResult == false) ) {
        string msg;
        {
            ostringstream oss;

            oss << "Antenna C" << carmaAntNo
                << " is not assigned to this subarray";

            msg = oss.str();
        }

        logSaCIError( commandName, msg );

        throw CARMA_ERROR( msg );
    }

    return result;
}


class SubarrayControlImpl::Prewriter : public Runnable {
    public:
        explicit Prewriter( const SubarrayControlImpl&  saCI,
                            CarmaMonitorSystem& cms,
                            ControlSubsystem& controlSubsystem,
                            ControlSubsystem::Subarray & subarray );

        virtual ~Prewriter( );

        virtual int execute( ) const;

        /**
         * activate/deactivate validity modification for certain
         * critical MPs when we are doing OTF mosaics.
         */
        void activateInvalidationForMosaics(const bool invalidate);

    private:
        const SubarrayControlImpl& saCI_;
        MonitorPointBool&          runningMp_;
        MonitorPointBool&          initializedMp_;
        MonitorPointBool&          stateRestored_;
        MonitorPointDouble&        trackRuntimeMP_;
        MonitorPointFloat&         phaseCenterOffsetRa_;
        MonitorPointFloat&         phaseCenterOffsetDec_;
        MonitorPointDouble&        phaseCenterRa_;
        MonitorPointDouble&        phaseCenterDec_;
        MonitorPointDouble&        phaseCenterAppRa_;
        MonitorPointDouble&        phaseCenterAppDec_;
        MonitorPointDouble&        velObservatory_;
        CarmaMonitorSystem&        cms_;
        ControlSubsystem&          controlSubsys_;

        /** 
         * controls if we need to invalidate certain MPs during
         * continuous integration mosaic
         */
        bool invalidationActive_;
};


SubarrayControlImpl::Prewriter::Prewriter(
    const SubarrayControlImpl&  saCI,
    CarmaMonitorSystem& cms,
    ControlSubsystem& cs,
    ControlSubsystem::Subarray& subarray ) :
saCI_( saCI ),
runningMp_( subarray.controllerRunning() ),
initializedMp_( subarray.controllerInitialized() ),
stateRestored_( subarray.stateRestored() ),
trackRuntimeMP_(subarray.trackRuntime()),
phaseCenterOffsetRa_(subarray.commands().phaseCenterOffset().ra() ),
phaseCenterOffsetDec_(subarray.commands().phaseCenterOffset().dec() ),
phaseCenterRa_( subarray.phaseCenterRa() ),
phaseCenterDec_( subarray.phaseCenterDec() ),
phaseCenterAppRa_( subarray.phaseCenterAppRa() ),
phaseCenterAppDec_( subarray.phaseCenterAppDec() ),
velObservatory_(subarray.velObservatory() ),
cms_(cms),
controlSubsys_(cs),
invalidationActive_(false)
{
    runningMp_.setValue( true );
    initializedMp_.setValue( saCI_.initializationFlag_ );
    stateRestored_.setValue( saCI_.controlSubsystemRestored_ );
    trackRuntimeMP_.setValue(-1.0);
    trackRuntimeMP_.setValidity(MonitorPoint::INVALID_NO_DATA);
    saCI_.updateRepTasks(cms_.control(), controlSubsys_);
}


SubarrayControlImpl::Prewriter::~Prewriter( )
try {
    runningMp_.setValue( false );
    initializedMp_.setValue( false );
    stateRestored_.setValue( false ); // As if this will ever be called.
} catch ( ... ) {
    // Just stifle any exception

    return;
}


int
SubarrayControlImpl::Prewriter::execute( ) const
{
    ScopedLogNdc ndc("SubarrayControlImpl::Prewriter::execute");
    runningMp_.setValue( true );
    initializedMp_.setValue( saCI_.initializationFlag_ );
    stateRestored_.setValue( saCI_.controlSubsystemRestored_ );
    if (saCI_.lastStartTrackMJD_ < 0.5) {
        trackRuntimeMP_.setValidity(MonitorPoint::INVALID_NO_DATA);
    }
    else {
        // In hours
        double deltaT = 24*(util::Time::MJD() - saCI_.lastStartTrackMJD_);
        trackRuntimeMP_.setValue(deltaT);
    }


    if ( invalidationActive_ ) {
        const SubarrayControlImpl::CarmaAntNoVec v 
            = saCI_.antManager_->getCarmaAntNoVecForAllAntennas();
        const size_t sz = v.size();
        {
            ostringstream os;
            os << "Found antnovec size: " << sz;
            //programLogNoticeIfPossible( os.str() );
        }
        for( size_t i = 0 ; i < sz; ++i ) {
            {
                ostringstream os;
                os << "Setting invalid UVW for antenna " << v[i];
                //programLogNoticeIfPossible( os.str() );
            }
            // mon pt index is offset by -1
            const unsigned index = v[i] - 1;
            saCI_.controlSubsystem_.antenna(index).interpU().setValidity(MonitorPoint::INVALID_NO_DATA );
            saCI_.controlSubsystem_.antenna(index).interpV().setValidity(MonitorPoint::INVALID_NO_DATA );
            saCI_.controlSubsystem_.antenna(index).interpW().setValidity(MonitorPoint::INVALID_NO_DATA );
        }
        phaseCenterOffsetRa_.setValidity( MonitorPoint::INVALID_NO_DATA );
        phaseCenterOffsetDec_.setValidity( MonitorPoint::INVALID_NO_DATA );
        phaseCenterRa_.setValidity( MonitorPoint::INVALID_NO_DATA );
        phaseCenterDec_.setValidity( MonitorPoint::INVALID_NO_DATA );
        phaseCenterAppRa_.setValidity( MonitorPoint::INVALID_NO_DATA );
        phaseCenterAppDec_.setValidity( MonitorPoint::INVALID_NO_DATA );
        velObservatory_.setValidity( MonitorPoint::INVALID_NO_DATA );

    } 
    else {
        // re-mark the persistent points as VALID.
        phaseCenterOffsetRa_.setValidity( MonitorPoint::VALID );
        phaseCenterOffsetDec_.setValidity( MonitorPoint::VALID );
    }
    
    // Set time since last completed and ready flags
    cms_.readNewestConditionalCopy();
    saCI_.updateRepTasks(cms_.control(), controlSubsys_);

    return 0;
}

void
SubarrayControlImpl::Prewriter::activateInvalidationForMosaics( const bool invalidate )

{ 
    if ( invalidate ) 
        programLogNotice("Activating mp invalidation in prewriter");
    else 
        programLogNotice("Deactivating mp invalidation in prewriter");

    invalidationActive_ = invalidate;
}

SubarrayControlImpl::CachedCarmaMonSys::CachedCarmaMonSys( ) :
guard_(),
lastCachedInUseBy_(),
cachedInstance_()
{
}


SubarrayControlImpl::CachedCarmaMonSys::~CachedCarmaMonSys( )
try {
    const ScopedLock< PthreadMutex > lock( guard_ );

    cachedInstance_.reset();
} catch ( ... ) {
    // Just stifle any exceptions

    return;
}


SubarrayControlImpl::SubarrayControlImpl(
    const int                     saNo,
    const bool                    verbose,
    const std::string             scriptStateDir)
try :
cancelFlag_( false ),
prewriter_( 0 ),
subarrayNo_(saNo),
//controlSubsystem_(),
subarrayContainer_(controlSubsystem_.subarray (saNo-1)),
carmaMonitor_(),
rawCarmaMonitor_(),
signalPath_(),
cachedWaitCarmaMonSys_(),
nextDriveSeqNo_(1),
nextTuneSeqNo_(100),
nextCentroidSeqNo_(200),
nextOpticsSeqNo_(300),
nextIntegSeqNo_(400),
numAstroBands_(carma::util::AstroBand::nBandMax_),
loRestFreq_(100.0),
loFreq_(100.0),
lo1Sideband_( 0 ),
ifFreq_( 0.0 ),
ifRestFreq_( 0.0 ),
restFreq_( 0.0 ),
skyFreq_( 0.0 ),
lo2DopplerTrackingEnabled_( false ),
LOchain_(LOchain(1)),
delayEngine_( 0 ),
dcLoSwitchyard_( 0 ),
slDownconverter_( 0 ),
wbDownconverter_( 0 ),
alarm_( 0 ),
corrDataRemapper_( 0 ),
ifSwitchyard_( 0 ),
lineLength_( 0 ),
llSwitchyard_( 0 ),
loberotator_( 0 ),
loRef_( 0 ),
loSwitchyard_( 0 ),
masterClock_( 0 ),
faultSys_( 0 ),
slPipeline_( 0 ),
wbPipeline_( 0 ),
c3gMax8Pipeline_(0),
c3gMax23Pipeline_(0),
projectDatabaseManager_( 0 ),
signalPathMapper_( 0 ),
antManager_( 0 ),
slCorrelatorVec_( 0 ),
wbCorrelatorVec_( 0 ),
vlbiVec_( 0 ),
trackerThread_( 0 ),
halfSecUpdater_( 0 ),
workerPool_( new WorkerPool( generateSubarrayWorkerPoolId( saNo ),
                             23,
                             false ) ),
obs_( 0 ),
elevLimit_(0.0),
arrayRefEphem_( 0 ),
dopplerEphem_( 0 ),
initializationFlag_(false),
verbose_( verbose ),
isDopplerTracking_(false),
sourceNameGuard_(),
sourceName_( carma::services::NO_SOURCE ),
dopplerSource_( carma::services::NO_SOURCE ),
userCatalog_(""),
pdmRequestId_(0),
pdmEditId_(0),
runId_(0),
scriptHistoryIsFull_(false),
scriptStateDir_(scriptStateDir),
nScriptStrings_(32),
globalDelay_(0),
restorationInProgress_( false ),
controlSubsystemRestored_( false ),
lastStartTrackMJD_(0.0),
astroTime_(),
prewriterIsInvalidating_( false ),
shadowingCalculator_(&carmaMonitor_)
{
  // Mutex initialization

  if(pthread_mutex_init(&gCorrNextSeqNoGuard_, NULL)) {
    throw CARMA_ERROR( strerror(errno) );
  };

  gCorrNextSeqNo_ = 50;

  if(pthread_mutex_init(&gCabMapGuard_, NULL)) {
    throw CARMA_ERROR( strerror(errno) );
  };

  this->sourceCatalog_.open(SourceCatalog::defaultCatalog());

  startAutoWriters();
  // Perform any internal initialization
  internalInitialization();

} catch ( ... ) {
  programLogErrorIfPossible( "Coming out of SaCI::SaCI() on an exception: " +
                 getStringForCaught() );

  logCaughtBacktraceAsErrorIfPossible( "  exception bt " );
}


SubarrayControlImpl::~SubarrayControlImpl( )
try {
    try {
        const auto_ptr< Prewriter > prewriter = prewriter_;

        if ( prewriter.get() != 0 )
            controlSubsystem_.monitorPointSet().removePrewriteMethod( *(prewriter.get()) );
    } catch ( ... ) {
        // Just stifle any exception
    }

    try {
       if ( trackerThread_.get() != 0 )
            trackerThreadThread_.interrupt();
    } catch ( ... ) {
        // Just stifle any exception
    }

    try {
        if ( slCorrelatorVec_.get() != 0 ) {
            CorrelatorHandleVector::iterator i = slCorrelatorVec_->begin();
            const CorrelatorHandleVector::iterator iEnd = slCorrelatorVec_->end();

            for ( ; i != iEnd; ++i ) {
                const CorrelatorHandle * const correlatorHandle = *i;

                *i = 0;

                delete correlatorHandle;
            }
        }
        if ( wbCorrelatorVec_.get() != 0 ) {
            CorrelatorHandleVector::iterator i = wbCorrelatorVec_->begin();
            const CorrelatorHandleVector::iterator iEnd = wbCorrelatorVec_->end();

            for ( ; i != iEnd; ++i ) {
                const CorrelatorHandle * const correlatorHandle = *i;

                *i = 0;

                delete correlatorHandle;
            }
        }
    } catch ( ... ) {
        // Just stifle any exception
    }

    try {
        if ( antManager_.get() != 0 )
            antManager_->deleteAllAntennas();
    } catch ( ... ) {
        // Just stifle any exception
    }

} catch ( ... ) {
    // Just stifle any exception

    return;
}


void
SubarrayControlImpl::instantiateHandles() 
{
    const ScopedLogNdc ndc( "SaCI::instantiateHandles()" );
    programLogInfoIfPossible( "Constructing remote object handles" );

    ControlSubsystemBase::Reachable & reachable =
        subarrayContainer_.reachable();

    delayEngine_ = auto_ptr< DelayEngine >( new DelayEngine );

    // Give all subarrays handles to all the downconverters and pipelines.
    // A science subarray can be initialized with no correlators
    // (hence no downconverters or pipelines). The dc's and pipeline's
    // will be added along with correlators, following the same model
    // as antennas.
    slDownconverter_ = auto_ptr< DownconverterHandle >(
        new DownconverterHandle( true, carmaMonitor_, reachable ) );
            
    wbDownconverter_ = auto_ptr< DownconverterHandle >(
        new DownconverterHandle( false, carmaMonitor_, reachable ) );

    slPipeline_ = auto_ptr< PipelineHandle >(
        new SLPipelineHandle(controlSubsystem_, carmaMonitor_, reachable));

    wbPipeline_ = auto_ptr< PipelineHandle >(
          new WBPipelineHandle(controlSubsystem_, carmaMonitor_, reachable));

    c3gMax8Pipeline_ = auto_ptr< PipelineHandle >(
        new C3gMax8PipelineHandle(controlSubsystem_, carmaMonitor_, reachable));

    c3gMax23Pipeline_ = auto_ptr< PipelineHandle >(
        new C3gMax23PipelineHandle(controlSubsystem_, carmaMonitor_,reachable));

    CPTRACE( Trace::TRACE7, "       AlarmHandle instantiation" );
    alarm_ = auto_ptr< AlarmHandle >(
        new AlarmHandle( carmaMonitor_, reachable ) );

    dcLoSwitchyard_ = auto_ptr< DCLOSwitchyardHandle >(
        new DCLOSwitchyardHandle( carmaMonitor_, reachable ) );

    ifSwitchyard_ = auto_ptr< IFSwitchyardHandle >(
        new IFSwitchyardHandle( carmaMonitor_, reachable ) );

    if ( subarrayNo_ < 4 ) {
        lineLength_ = auto_ptr< LineLengthHandle >(
            new LineLengthHandle( carmaMonitor_, reachable ) );
    }

    llSwitchyard_ = auto_ptr< LLSwitchyardHandle >(
        new LLSwitchyardHandle( carmaMonitor_, reachable ) );

    if ((subarrayNo_ == 1) || (subarrayNo_ == 2)) {
        loberotator_ = auto_ptr< LoberotatorHandle >(
            new LoberotatorHandle( carmaMonitor_, reachable ));
    }

    loSwitchyard_ = auto_ptr< LOSwitchyardHandle >(
        new LOSwitchyardHandle( carmaMonitor_, reachable ) );

    if ((subarrayNo_ >= 1) && (subarrayNo_ <= 3)) {
        loRef_ = auto_ptr< LOrefHandle >(
            new LOrefHandle(carmaMonitor_, reachable));
    }

    if ((subarrayNo_ == 1) || (subarrayNo_ == 2)) {
        masterClock_ = auto_ptr< ClockHandle >(
            new ClockHandle(carmaMonitor_, reachable) );
    }

    if (subarrayNo_ <= 2) {
        faultSys_ = auto_ptr< FaultHandle >(
            new FaultHandle(carmaMonitor_, reachable));
    }

    // any reason why this handle should not be accessible to
    // all subarrays?
    programLogInfoIfPossible( "Instantiating SignalPathMapperHandle" );
    signalPathMapper_ = auto_ptr< SignalPathMapperHandle >(
            new SignalPathMapperHandle(carmaMonitor_, reachable));

    programLogInfoIfPossible( "Instantiating CorrDataRemapperHandle" );
    corrDataRemapper_ = auto_ptr< CorrDataRemapperHandle >(
            new CorrDataRemapperHandle(carmaMonitor_, reachable));
    programLogInfoIfPossible( "Constructing and configuringing antManager_" );

    antManager_ =
        auto_ptr< AntManager >( new AntManager( this,
                                                controlSubsystem_,
                                                subarrayContainer_ ) );
    programLogInfoIfPossible( "antManager_ constructed and configged" );

    setupCorrelatorVec();// constructor to CorrelatorHandle can throw here
    setupVlbiVec();

    programLogInfoIfPossible( "Constructing subarray tracker thread" );
    trackerThread_ =
        auto_ptr< TrackerThread >( new TrackerThread( *this, carmaMonitor_ ) );
    programLogInfoIfPossible( "Subarray tracker thread constructed" );

    halfSecUpdater_ =
        auto_ptr< HalfSecUpdater >( new HalfSecUpdater( *this ) );
    programLogInfoIfPossible( "Half second update thread constructed" );

    projectDatabaseManager_ =
    auto_ptr< ProjectDatabaseManagerHandle >(
        new ProjectDatabaseManagerHandle( carmaMonitor_, reachable ) );
    programLogInfoIfPossible( "ProjectDatabaseManagerHandle constructed" );

    delayEngine_->setArrayReferencePoint( obs_->getReference() );

    try {
        CARMA_CPTRACE(Trace::TRACE7, "Set delay eng LO freq");

        // set LO frequencies for all antennas - by default it is 88.5 GHz
        const Frequency lo1freq( loFreq_, "GHz" );

        delayEngine_->setAllAntennaLOFreqs( lo1freq );
    } catch ( ... ) {
        Category& log = Program::getLogger();
        log << Priority::ERROR
            << "SubarrayControlImpl::setAllAntennaLOFreqs - "
            << "caught and stifled unknown exception.";
    }

}

void
SubarrayControlImpl::startAutoWriters( )
try {
    const ScopedLogNdc ndc( "SaCI::startAutoWriters()" );

    {
        if ( prewriter_.get() != 0 )
            throw CARMA_ERROR( "prewriter_ was not NULL" );

        auto_ptr< Prewriter > prewriter( new Prewriter( 
                *this, carmaMonitor_, controlSubsystem_, subarrayContainer_));

        controlSubsystem_.monitorPointSet().installPrewriteMethod( *(prewriter.get()) );

        prewriter_ = prewriter;
    }

    programLogInfoIfPossible( "Starting the autowriters" );

    delaySubsystem_.startAutoWriter( CONTROL_AUTO_WRITE_DELAY + 0.05 );

    controlSubsystem_.startAutoWriter (CONTROL_AUTO_WRITE_DELAY);

    // Because we are using the control system to populate some
    // of the signalpath subsystem monitor points, this monitor
    // system autowriter must before the SPM's autowriter, otherwise
    // we lose our values. The SPM has a delay of 0.1, so set this
    // one to 0.05
    signalPath_.startAutoWriter (0.05);

} catch ( ... ) {
    programLogErrorIfPossible(
        "Coming out of SaCI::startAutoWriters() on an exception: " +
        getStringForCaught());

    logCaughtBacktraceAsErrorIfPossible( "  exception bt " );

    throw;
}

void
SubarrayControlImpl::setupVlbiVec( ) {
    vlbiVec_ = auto_ptr< VlbiHandleVector >( new VlbiHandleVector );
    const unsigned NUM_VLBI_BANDS = 8;
    vlbiVec_->resize( NUM_VLBI_BANDS, 0 );

    // Only populate vlbiVec_ for sci1 subarray!
    if(subarrayNo_ == 1) {
        ControlSubsystemBase::Reachable & reachable =
            subarrayContainer_.reachable();

        const bool defaultLogIfNotReachable = false;

        for ( unsigned int band = 1; band <= NUM_VLBI_BANDS ; ++band ) {
            vlbiVec_->at( band - 1 ) =
                new VlbiHandle( band,
                                carmaMonitor_,
                                reachable,
                                defaultLogIfNotReachable );
        }
    }
}

/**.......................................................................
 * Set up vectors of handles used for controlling correlator bands
 *
 * Now creates and populates vectors for both correlators, regardless
 * of the subarray number.
 */
void
SubarrayControlImpl::setupCorrelatorVec( ) {

    const unsigned maxSlBands = getMaxNumBands(util::CORR_SPECTRAL);
    const unsigned maxWbBands = getMaxNumBands(util::CORR_WIDEBAND);
    slCorrelatorVec_ =
        auto_ptr< CorrelatorHandleVector >( new CorrelatorHandleVector );
    slCorrelatorVec_->resize( maxSlBands, 0 );

    wbCorrelatorVec_ =
        auto_ptr< CorrelatorHandleVector >( new CorrelatorHandleVector );
    wbCorrelatorVec_->resize( maxWbBands, 0 );

    ControlSubsystemBase::Reachable & reachable =
        subarrayContainer_.reachable();

    const bool defaultLogIfNotReachable = false;
    for ( unsigned int band = 1; band <= maxSlBands; ++band ) {
      
      slCorrelatorVec_->at( band - 1 ) =
        // This constructor will now throw ErrorException if
        // the Band hardware type comes back as
        // HARDWARE_TYPE_UNKNOWN.
        // see util/corrUtils.h
        new CorrelatorHandle( band,
                      util::CORR_SPECTRAL,
                      carmaMonitor_,
                      reachable,
                      defaultLogIfNotReachable );
    }

    for ( unsigned int band = 1; band <= maxWbBands; ++band ) {
      
      wbCorrelatorVec_->at( band - 1 ) =
        // This constructor will now throw ErrorException if
        // the Band hardware type comes back as
        // HARDWARE_TYPE_UNKNOWN.
        // see util/corrUtils.h
        new CorrelatorHandle( band+8,
                      util::CORR_WIDEBAND,
                      carmaMonitor_,
                      reachable,
                      defaultLogIfNotReachable );
    }


    {
      ostringstream oss;
      oss << "slCorrelatorVec_ populated with "
          << slCorrelatorVec_->size() << " bands.";
      oss << " wbCorrelatorVec_ populated with "
          << wbCorrelatorVec_->size() << " bands.";
      programLogInfoIfPossible( oss.str() );
    }

    // Note: I am passing in astrobandNo = 25 and 33 here to 
    // workaround the band-based monitor system requirements in
    // CorrelatorHandle.cc.  This is described in that
    // code.  Once the C3G monitor system is finalized
    // (late Jan 2014 according to Hawkins), I will revisit
    // the monitor system dependencies in CorrelatorHandle.
    // For now, this allows testing to proceed.
    programLogInfoIfPossible("C3gmax8 CorrelatorHandle ....");
    c3gMax8Correlator_ =
        auto_ptr< CorrelatorHandle >( 
                new CorrelatorHandle( 33,
                      util::CORR_C3GMAX8,
                      carmaMonitor_,
                      reachable,
                      defaultLogIfNotReachable ) 
                );
    programLogInfoIfPossible("C3gmax23 CorrelatorHandle ....");
    c3gMax23Correlator_ =
        auto_ptr< CorrelatorHandle >( 
                new CorrelatorHandle( 25,
                      util::CORR_C3GMAX23,
                      carmaMonitor_,
                      reachable,
                      defaultLogIfNotReachable ) 
                );
    programLogInfoIfPossible("C3gmax8 and C3gMax23 CorrelatorHandles created");
}


void
SubarrayControlImpl::startSAT( )
{
    programLogInfoIfPossible( "Starting subarray tracker thread" );

    if ( trackerThread_.get() == 0 ) {
        const string msg = "NULL subarray tracker thread";

        programLogErrorIfPossible( msg );

        throw CARMA_ERROR( msg );
    }

    if ( trackerThreadThread_.get_id() != boost::thread::id() ) {
        const string msg = "Subarray tracker thread already started!";

        programLogErrorIfPossible( msg );

        throw CARMA_ERROR( msg );
    }

    boost::thread newTrackerThreadThread( boost::ref(*(trackerThread_.get())) );
                                          
    trackerThreadThread_.swap( newTrackerThreadThread );

    programLogInfoIfPossible( "Subarray tracker thread started" );
}

void
SubarrayControlImpl::startHSU( )
{
    programLogInfoIfPossible( "Starting half second updater thread" );

    if ( halfSecUpdater_.get() == 0 ) {
        const string msg = "NULL subarray half second updater thread";

        programLogErrorIfPossible( msg );

        throw CARMA_ERROR( msg );
    }

    if ( halfSecUpdaterThread_.get_id() != boost::thread::id() ) {
        const string msg = "Half sec updater thread already started!";

        programLogErrorIfPossible( msg );

        throw CARMA_ERROR( msg );
    }

    boost::thread newHalfSecUpdaterThread( boost::ref( *halfSecUpdater_ ) );
    halfSecUpdaterThread_.swap( newHalfSecUpdaterThread ); 

    programLogInfoIfPossible( "Subarray half second updater thread started" );
}



CORBA::Long
SubarrayControlImpl::getSubarrayNo( )
try {
    return subarrayNo_;
} catch ( ... ) {
    rethrowCaughtAsUser();
}


char *
SubarrayControlImpl::getSubarrayName()
try {
    return string_dup( getName().c_str() );
} catch ( ... ) {
    rethrowCaughtAsUser();

    // just to shut the compiler warning up
    throw CARMA_EXCEPTION( util::UserException,
                           "Very bad things in SaCI::getSubarrayName()" );
}


string
SubarrayControlImpl::getName( ) const
{
    return getSubarrayName( subarrayNo_ );
}


string
SubarrayControlImpl::getAlphanumericName( ) const
{
    return getAlphanumericSubarrayName( subarrayNo_ );
}


string
SubarrayControlImpl::getSubarrayName( const int subarrayNo )
{
    return ControlSubsystem::getSubarrayName( subarrayNo );
}


string
SubarrayControlImpl::getAlphanumericSubarrayName( const int subarrayNo )
{
    return ControlSubsystem::getSubarrayAlphanumericName( subarrayNo );
}


void
SubarrayControlImpl::checkBandNo( const short bandNo,
                                  const bool  allowZero ) {
    unsigned lowLimit = 0;
    unsigned nbands = getMaxNumBands( getCorrelatorDesignation() );
    if(nbands == 0)
      return;
    if (!allowZero) lowLimit = 1;
    if (((unsigned)bandNo >= lowLimit) && ((unsigned)bandNo <= nbands )) return;
    ostringstream o;
    o << "Band number (" << bandNo << ") is out of range ["
      << lowLimit << "-" << nbands << "] for the " 
      << getStringForCorrType( getCorrelatorDesignation() ) << " correlator.";
    Program::getLogger() << Priority::ERROR << o.str();
    throw CARMA_ERROR(o);
}


string
SubarrayControlImpl::getStringForCarmaAntNoSeq(
    const CarmaAntNoSeq & carmaAntNoSeq )
{
    const vector< CORBA::Short > shortVec =
        convertSequenceToVector< CORBA::Short >( carmaAntNoSeq );

    if ( shortVec.size() == 1 ) {
        if ( (*(shortVec.begin())) == 0 )
            return "[0]";

        return formatAsRanges( shortVec, "C", "" );
    }

    return "[" + formatAsRanges( shortVec, "C", "" ) + "]";
}


string
SubarrayControlImpl::getStringForCarmaAntNo( const CORBA::Short carmaAntNo )
{
    ostringstream oss;
    oss << "C" << carmaAntNo;
    return oss.str();
}


//====================================================================


void
SubarrayControlImpl::reconnect( const bool force )
try {
    cmdlog() << "reconnect(force=" << force << ")";

    TrackerThreadSync sync( *this );

    {
        const AntControlsGroup antControlsGroup = getAntControlsGroup();

        AntControlsGroup::const_iterator i = antControlsGroup.begin();
        const AntControlsGroup::const_iterator iEnd = antControlsGroup.end();

        for ( ; i != iEnd; ++i ) {
            AntennaControls * const antControls = *i;

            if ( antControls == 0 )
                continue;

            if ( force )
                antControls->forceFullReconnect();
            else
                antControls->attemptToReconnectIfNeeded();
        }
    }

    if ( wbCorrelatorVec_.get() != 0 ) {
        CorrelatorHandleVector::iterator i = wbCorrelatorVec_->begin();
        const CorrelatorHandleVector::iterator iEnd = wbCorrelatorVec_->end();

        for ( ; i != iEnd; ++i ) {
            CorrelatorHandle * const correlatorHandle = *i;

            if ( correlatorHandle == 0 )
                continue;

            if ( force )
                correlatorHandle->forceFullReconnect();
            else
                correlatorHandle->attemptToReconnectIfNeeded();
        }
    }

    if ( slCorrelatorVec_.get() != 0 ) {
        CorrelatorHandleVector::iterator i = slCorrelatorVec_->begin();
        const CorrelatorHandleVector::iterator iEnd = slCorrelatorVec_->end();

        for ( ; i != iEnd; ++i ) {
            CorrelatorHandle * const correlatorHandle = *i;

            if ( correlatorHandle == 0 )
                continue;

            if ( force )
                correlatorHandle->forceFullReconnect();
            else
                correlatorHandle->attemptToReconnectIfNeeded();
        }
    }

    if ( force ) {
        if ( slDownconverter_.get() != 0 )
            slDownconverter_->forceFullReconnect();

        if ( wbDownconverter_.get() != 0 )
            wbDownconverter_->forceFullReconnect();

        if ( alarm_.get() != 0 )
            alarm_->forceFullReconnect();

        if ( dcLoSwitchyard_.get() != 0 )
            dcLoSwitchyard_->forceFullReconnect();

        if ( ifSwitchyard_.get() != 0 )
            ifSwitchyard_->forceFullReconnect();

        if ( lineLength_.get() != 0 )
            lineLength_->forceFullReconnect();

        if ( llSwitchyard_.get() != 0 )
            llSwitchyard_->forceFullReconnect();

        if ( loberotator_.get() != 0 )
            loberotator_->forceFullReconnect();

        if ( loRef_.get() != 0 )
            loRef_->forceFullReconnect();

        if ( loSwitchyard_.get() != 0 )
            loSwitchyard_->forceFullReconnect();

        if ( masterClock_.get() != 0 )
            masterClock_->forceFullReconnect();

        if ( faultSys_.get() != 0 )
            faultSys_->forceFullReconnect();

        if ( slPipeline_.get() != 0 )
            slPipeline_->forceFullReconnect();

        if ( wbPipeline_.get() != 0 )
            wbPipeline_->forceFullReconnect();

        if ( projectDatabaseManager_.get() != 0 )
            projectDatabaseManager_->forceFullReconnect();

        if ( signalPathMapper_.get() != 0 )
            signalPathMapper_->forceFullReconnect();

        if ( corrDataRemapper_.get() != 0 )
          corrDataRemapper_->forceFullReconnect();

    } else {
        if ( slDownconverter_.get() != 0 )
            slDownconverter_->attemptToReconnectIfNeeded();

        if ( wbDownconverter_.get() != 0 )
            wbDownconverter_->attemptToReconnectIfNeeded();

        if ( alarm_.get() != 0 )
            alarm_->attemptToReconnectIfNeeded();
        
        if ( dcLoSwitchyard_.get() != 0 )
            dcLoSwitchyard_->attemptToReconnectIfNeeded();

        if ( ifSwitchyard_.get() != 0 )
            ifSwitchyard_->attemptToReconnectIfNeeded();

        if ( lineLength_.get() != 0 )
            lineLength_->attemptToReconnectIfNeeded();

        if ( llSwitchyard_.get() != 0 )
            llSwitchyard_->attemptToReconnectIfNeeded();

        if ( loberotator_.get() != 0 )
            loberotator_->attemptToReconnectIfNeeded();

        if ( loRef_.get() != 0 )
            loRef_->attemptToReconnectIfNeeded();

        if ( loSwitchyard_.get() != 0 )
            loSwitchyard_->attemptToReconnectIfNeeded();

        if ( masterClock_.get() != 0 )
            masterClock_->attemptToReconnectIfNeeded();

        if ( faultSys_.get() != 0 )
            faultSys_->attemptToReconnectIfNeeded();

        if ( slPipeline_.get() != 0 )
            slPipeline_->attemptToReconnectIfNeeded();

        if ( wbPipeline_.get() != 0 )
            wbPipeline_->attemptToReconnectIfNeeded();

        if ( projectDatabaseManager_.get() != 0 )
            projectDatabaseManager_->attemptToReconnectIfNeeded();

        if ( signalPathMapper_.get() != 0 )
            signalPathMapper_->attemptToReconnectIfNeeded();

        if ( corrDataRemapper_.get() != 0 )
            corrDataRemapper_->attemptToReconnectIfNeeded();

    }
} catch ( ... ) {
    rethrowCaughtAsUser();
}


DelayEngine *
SubarrayControlImpl::getDelayEngine( ) const {
    return delayEngine_.get();
}

void
SubarrayControlImpl::setAntPosFilename( const char * filename )
try {
    controlSubsystem_.antPosFilename().setValue( filename );
    markStateChange();
} catch (...) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::setInitializationFlag( const bool state )
try {
    if ( initializationFlag_ != state ) {
        initializationFlag_ = state;

        ostringstream oss;
        oss << "SaCI::setInitializationFlag() is changing the state from ";
        if ( state ) {
            oss << "false to true";
            programLogInfoIfPossible( oss.str() );
            if (subarrayNo_ == 1) {
                controlSubsystem_.repTaskErrors().setValue(0);
            }
        } 
        else {
            oss << "true to false";
            programLogErrorIfPossible( oss.str() );
        }        
    }
} catch ( ... ) {
    rethrowCaughtAsUser();
}

bool
SubarrayControlImpl::getInitializationFlag( )
try {
    const bool result = initializationFlag_;

    if ( result == false ) {
        programLogInfoIfPossible(
            "SaCI::getInitializationFlag() is returning false" );
    }

    return result;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


void
SubarrayControlImpl::internalInitialization( )
{
    ScopedLogNdc ndc("internalInitialization");

    if (subarrayNo_ == 2) loFreq_ = 35.938;

    try {
    // if we can't initialize the array & antenna positions
    // don't even bother proceeding. See bug 417.
        obs_ = auto_ptr< Observatory >( new Observatory );
    } catch ( const ErrorException & e ) {
        ostringstream os;
        os << " Could not initialize Observatory CARMA. "
           << " It is likely the file Observatory.cat is corrupted. "
           << " Error message was: "
           << e.getMessage();
        programLogCriticalIfPossible( os.str() );
        throw CARMA_ERROR( os );
    }

  
    // initialize the Ephemeris and AstroTime objects here, since we
    // know now that obs_ is good.
    arrayRefEphem_ = auto_ptr< Ephemeris >( new Ephemeris );
    dopplerEphem_  = auto_ptr< Ephemeris >( new Ephemeris );

    // AstroTime will self-update its IERSTable every 12 hours.
    astroTime_.setSite( obs_->getReference() );

    // instantiate the handle auto pointers. This call must come
    // after obs_ and Ephem_ members are created.
    instantiateHandles();

    // Carma antenna numbers are fixed to specific indices in mon sys
    const unsigned int maxAnts = controlSubsystem_.antennaCount();

    // set antenna names, Jy/K values, ifatten
    for (unsigned short carmaAntNo = 1; carmaAntNo <= maxAnts; ++carmaAntNo ) {
        // The phys antenna names are inserted here, hardwired to carma ant#'s
        ostringstream antName;

        if (carmaAntNo <= 6) {
            antName << "ovro" << carmaAntNo;
        } else if (carmaAntNo <= 15) {
            antName << "bima" << carmaAntNo - 6;
        } else  {
            antName << "sza"  << carmaAntNo - 15;
        }

        ControlSubsystemBase::Antenna & antByNo =
            controlSubsystem_.antenna( carmaAntNo - 1 );

        antByNo.carmaAntennaNumber().setValue( carmaAntNo );
        antByNo.name().setValue( antName.str() );
        antByNo.diameter().setValue( diameter( carmaAntNo ) );
        antByNo.jyperk().setValue( computeJyPerK( carmaAntNo, loFreq_ ) );
        // Need to make sure that we have valid atten values for queries
        // in python. But just setting these values will screw up antennas 
        // in another subarray. So only set invalid attens.
        for (int i=0; i<2; i++) {
            if (!antByNo.ifAttenAmb(i).isValid()) {
                antByNo.ifAttenAmb(i).setValue(20);
            }
        }    
    }

    subarrayContainer_.elevLimit().setValue(elevLimit_);

    subarrayContainer_.loFreq().setValue(loFreq_);
    Freq loFreq(loFreq_, Freq::GHz);
    Freq oscFreq(loFreq);
    int LOchainMode = 1; // New normal
    try {
        LOchain_ = LOchain(oscFreq, LOchainMode);
    }
    catch (std::exception& e) {
        ostringstream o;
        o << "Trouble in internalInitialization() with creation of LOchain: " << e;
        throw CARMA_ERROR(o);
    }

    const unsigned maxSlBands = getMaxNumBands(util::CORR_SPECTRAL);
    const unsigned maxWbBands = getMaxNumBands(util::CORR_WIDEBAND);
    const unsigned maxC3gMax8Bands = getMaxNumBands(util::CORR_C3GMAX8);
    const unsigned maxC3gMax23Bands = getMaxNumBands(util::CORR_C3GMAX23);


    //bool stateRestoredSa1 = false;
    //bool stateRestoredSa2 = false;
    //bool initializedSa1 = false;
    //bool initializedSa2 = false;
    //carma::control::CorrType typeSa1 = carma::util::CORR_NONE;
    //carma::control::CorrType typeSa2 = carma::util::CORR_NONE;

    slBdcEnabled_.resize( maxSlBands );
    slBdcEnabled_.assign( maxSlBands, false);

    wbBdcEnabled_.resize( maxWbBands );
    wbBdcEnabled_.assign( maxWbBands, false);


    // Default bandwidth to 500 MHz for all bands
    slcmodeVec_.resize( maxSlBands );
    //slcmodeVec_.assign( maxSlBands, "500");
    wbcmodeVec_.resize( maxWbBands );
    //wbcmodeVec_.assign( maxWbBands , "500");

    c3gMax8modeVec_.resize(maxC3gMax8Bands);
    c3gMax23modeVec_.resize(maxC3gMax23Bands);
    /*try{
      if(carmaMonitor_.control().subarray(0).stateRestored().isValid()){
    stateRestoredSa1 = carmaMonitor_.control().subarray(0).stateRestored().getValue();
    typeSa1 = static_cast<carma::control::CorrType>(carmaMonitor_.signalPath().mapping().subarray(0).CORRELATOR_DESIGNATION_MP().getValue());
    initializedSa1 = carmaMonitor_.control().subarray(0).controllerInitialized().getValue();
      }
    }
    catch(...){
      
    }
  
    try{
      if(carmaMonitor_.control().subarray(1).stateRestored().isValid()){
    stateRestoredSa2 = carmaMonitor_.control().subarray(1).stateRestored().getValue();
    typeSa2 = static_cast<carma::control::CorrType>(carmaMonitor_.signalPath().mapping().subarray(1).CORRELATOR_DESIGNATION_MP().getValue());
    initializedSa2 = carmaMonitor_.control().subarray(1).controllerInitialized().getValue();
      }
    }
    catch(...){
      
    }
        switch (subarrayNo_) {
        case 1:
            addCorrelator(util::CORR_SPECTRAL);
            for (unsigned b=0; b< maxSlBands ; b++) {
        ControlBandPoints& cbp =
                    controlSubsystem_.spectralLineCorrelator().
            slcBand(b).controlBandPoints();
                cbp.bandwidth().setValue(500.0); // Default to 500 MHz
                //slcmodeVec_.at(b) = "500";
        }
        break;
        case 2:
            addCorrelator(util::CORR_WIDEBAND);
            for (unsigned b=0; b< maxWbBands ; b++) {
        ControlBandPoints& cbp =
                    controlSubsystem_.widebandCorrelator().
            wbcBand(b).controlBandPoints();
            cbp.bandwidth().setValue(500.0); // Default to 500 MHz
                //wbcmodeVec_.at(b) = "500";
        }
        break;
        default:
            break;
        }*/
    
    
    // initialize correlator mode description strings
    //string slDesc = makeCorrModeDescString(CorrDesignation::SPECTRAL);
    //string wbDesc = makeCorrModeDescString(CorrDesignation::WIDEBAND);
    //controlSubsystem_.spectralLineCorrelator().modeDesc().setValue(slDesc);
    //controlSubsystem_.widebandCorrelator().modeDesc().setValue(wbDesc);

    // Initialize array reference data & monitor points
    setArrayReference( obs_->getReference() );

    // Set the command monitor point values for phase center offsets.
    ControlSubsystemBase::PhaseCenterOffset& command
        = subarrayContainer_.commands().phaseCenterOffset();
    command.timestamp().setValue(Time::MJD());
    command.ra().setValue(0.0);
    command.dec().setValue(0.0);

    // initialize the frequency interpolation container
    {
        ScopedQILockManager lockManager( dopplerInterp_, true );

        lockManager.lockQI();
        dopplerInterp_.empty();
        lockManager.unlockQI();
    }

    // initialize to no doppler tracking.
    doppler( services::NO_SOURCE.c_str() );

    // this is also done in subarrayInit.py
    //resetProjectAndObsblock();

    subarrayContainer_.scriptState().setValue(
        ControlSubsystemBase::ScriptStateMonitorPointEnum::COMPLETED);

    if (subarrayNo_ == 1) {
        controlSubsystem_.repTaskErrors().setValue(0);
    }

    initializeDefaultProjectMap();

    typedef ControlSubsystemBase::AlarmMonitorPointEnum ALARM;
    subarrayContainer_.alarm().setValue(ALARM::OFF);
    noiseSource_ = false;

    // load the default pad and antenna delay values from file
    loadDelayTables();
    
    // Create directory for scriptState
    bool dirExists = FileUtils::exists(scriptStateDir_);
    if (!dirExists) FileUtils::makeDirectory(scriptStateDir_);
    scriptStateDir_ += "/ScriptState";
    dirExists = FileUtils::exists(scriptStateDir_);
    if (!dirExists) FileUtils::makeDirectory(scriptStateDir_);
    scriptStateDir_ += "/" + getName();
    dirExists = FileUtils::exists(scriptStateDir_);
    if (!dirExists) FileUtils::makeDirectory(scriptStateDir_);

    /*
    mode_t mode = S_IRWXU | S_IRWXG | S_IROTH;     
        errno = 0;
        const int result = ::mkdir(scriptStateDir_.c_str(), mode); 
        const int localErrno = errno;
        if ( result == -1 )
            throwPosixError( localErrno );
    }
    */
    // Initialize decimation MPs (these must be valid all the time)
    for (unsigned int b=0; b<maxSlBands; b++) {
        MonitorPointBool& d = controlSubsystem_.
                                  spectralLineCorrelator().slcBand(b).
                                  controlBandPoints().decimation();
        if (!(d.isValid())) d.setValue(false);        
    }
    for (unsigned int b=0; b<maxWbBands; b++) {
        MonitorPointBool& d = controlSubsystem_.
                                  widebandCorrelator().wbcBand(b).
                                  controlBandPoints().decimation();
        if (!(d.isValid())) d.setValue(false);        
    }
    for (unsigned int b=0; b<maxC3gMax8Bands; b++) {
        MonitorPointBool& d = controlSubsystem_.
                                  c3gMax23Correlator().c3gMax23Band(b).
                                  controlBandPoints().decimation();
        if (!(d.isValid())) d.setValue(false);        
    }
    for (unsigned int b=0; b<maxC3gMax23Bands; b++) {
        MonitorPointBool& d = controlSubsystem_.
                                  c3gMax8Correlator().c3gMax8Band(b).
                                  controlBandPoints().decimation();
        if (!(d.isValid())) d.setValue(false);        
    }

//  Default to no owned correlators.  State restoration will update these
//  as necessary.
/*
    controlSubsystem_.spectralLineCorrelator().controllingSubarray().setValue("NONE");
    controlSubsystem_.widebandCorrelator().controllingSubarray().setValue("NONE");
    controlSubsystem_.c3gMax8Correlator().controllingSubarray().setValue("NONE");
    controlSubsystem_.c3gMax23Correlator().controllingSubarray().setValue("NONE");
    */
    
}

void
SubarrayControlImpl::setArrayReference( const Location & arrayRef ) {
    ScopedLogNdc ndc("SaCI::setArrayReference");
    arrayRefEphem_->setLocation( arrayRef );
    dopplerEphem_->setLocation( arrayRef );

    if ( getDelayEngine() != NULL )
        delayEngine_->setArrayReferencePoint( arrayRef );
    ControlSubsystemBase::ArrayReference& arrayCenter =
            subarrayContainer_.arrayReference();
    arrayCenter.latitude().setValue( arrayRef.getLatitude().radians() );
    arrayCenter.longitude().setValue( arrayRef.getLongitude().radians() );
    arrayCenter.altitude().setValue( arrayRef.getAltitude().meters() );
    ostringstream os;
    os << std::fixed << "Called with Location = " << arrayRef ;
    programLogInfoIfPossible( os.str() );
}


CategoryStream SubarrayControlImpl::loginfo()
{
    static Category& programLogger = getProgramLogger();
    CategoryStream infologger = programLogger << Priority::INFO ;
    infologger << std::fixed << boolalpha;
    return infologger;
}

CategoryStream
SubarrayControlImpl::cmdlog()
{
    static Category& programLogger = getProgramLogger();
    CategoryStream cmdlogger = programLogger << CMD_LOG_PRIORITY;
    cmdlogger << std::fixed << boolalpha << "COMMAND: ";
    return cmdlogger;
}

void SubarrayControlImpl::logSentCommand(string cmd, string dest)
{
    loginfo() << "Sent " << cmd << " to " << dest << " at "
              << Time::getTimeString(3);
}

bool SubarrayControlImpl::isActionCompleteHelper(
    const MonitorPointInt& seqNoMP,
    const int  consecutiveErrorLimit,
          int& consecutiveErrorCounter,
    const int nextSequenceNo,
    const int carmaAntNo,
    const std::string& name,
    const bool debug)
{
    if (!seqNoMP.isValid()) {
        consecutiveErrorCounter++;
        if (consecutiveErrorCounter >= consecutiveErrorLimit) {
            consecutiveErrorCounter = 0;
            ostringstream o;
            o << "isActionComplete(" << name << ") :"
              << "number of consecutive invalid monitor frames "
              << "equals limit of "
              << consecutiveErrorLimit;
            throw CARMA_ERROR(o);
        }
        return false;
    }

    // Valid monitor point
    if (debug && (consecutiveErrorCounter > 0)) {
        ostringstream o;
        o << "isActionComplete(" << name << "): ant#" << carmaAntNo
          << " had "<< consecutiveErrorCounter
          << " consecutive invalid monitor frames";
        Program::getLogger() << Priority::INFO << o.str();
    }
    consecutiveErrorCounter = 0;

    if (false) {
        // For desperate debugging...
        ostringstream o;
        o << "isActionComplete(" << name << "): ant#" << carmaAntNo
          << " currentSeqNo=" << seqNoMP.getValue()
          << "   nextSeqNo=" << nextSequenceNo ;
        Category& log = Program::getLogger();
        log << Priority::INFO << o.str();
     }

    return (seqNoMP.getValue() == nextSequenceNo);
}




SubarrayControlImpl::AntControlsGroup
SubarrayControlImpl::getAntControlsGroup( ) {
    if ( antManager_.get() == 0 )
        return AntControlsGroup();

    return antManager_->getGroup();
}


SubarrayControlImpl::AntControlsGroup
SubarrayControlImpl::getAntControlsGroupForCarmaAntNoVec(
    const string &        commandName,
    const CarmaAntNoVec & carmaAntNoVec,
    const bool            allowZero,
    const bool            ignoreDupes,
    const bool            skipAntsNotOwnedByMe )
{
    if ( antManager_.get() == 0 ) {
        const string msg =
            "Bad carma antenna number vector before ant manager is up";

        throw CARMA_ERROR( msg );
    }

    return antManager_->getGroupForCarmaAntNoVec( commandName,
                                                  carmaAntNoVec,
                                                  allowZero,
                                                  ignoreDupes,
                                                  skipAntsNotOwnedByMe );
}


SubarrayControlImpl::AntControlsGroup
SubarrayControlImpl::getAntControlsGroupForCarmaAntNoSeq(
    const string &        commandName,
    const CarmaAntNoSeq & carmaAntNoSeq,
    const bool            allowZero,
    const bool            ignoreDupes,
    const bool            skipAntsNotOwnedByMe )
{
    if ( antManager_.get() == 0 ) {
        const string msg =
            "Bad carma antenna number sequence before ant manager is up";

        throw CARMA_ERROR( msg );
    }

    return antManager_->getGroupForCarmaAntNoSeq( commandName,
                                                  carmaAntNoSeq,
                                                  allowZero,
                                                  ignoreDupes,
                                                  skipAntsNotOwnedByMe );
}


SubarrayControlImpl::AntControlsGroup
SubarrayControlImpl::getAntControlsGroupForCarmaAntNo(
    const string &     commandName,
    const CORBA::Short carmaAntNo ) {
    if ( antManager_.get() == 0 )
        throw CARMA_ERROR( "Bad carma antenna number before ant manager is up" );

    const bool allowEmptyResult = (initializationFlag_ == false);

    return antManager_->getGroupForCarmaAntNo( commandName,
                                               carmaAntNo,
                                               allowEmptyResult );
}


ControlSubsystemBase::Antenna &
SubarrayControlImpl::getAntMonPtForCarmaAntNo(
    const unsigned short carmaAntNo )
{
    return controlSubsystem_.antenna( carmaAntNo - 1 );
}

ControlSubsystemBase::Ovro &
SubarrayControlImpl::getOvroMonPtForCarmaAntNo(
    const unsigned short carmaAntNo )
{
    return controlSubsystem_.ovro( carmaAntNo - 1 );
}

ControlSubsystemBase::Bima &
SubarrayControlImpl::getBimaMonPtForCarmaAntNo(
    const unsigned short carmaAntNo )
{
    return controlSubsystem_.bima( carmaAntNo - 7 );
}

ControlSubsystemBase::Sza &
SubarrayControlImpl::getSzaMonPtForCarmaAntNo(
    const unsigned short carmaAntNo )
{
    return controlSubsystem_.sza( carmaAntNo - 16 );
}


ControlSubsystemBase::Antenna &
SubarrayControlImpl::getAntMonPtForAntControls(
    const AntennaControls & antControls )
{
    const unsigned short carmaAntNo = antControls.getCarmaAntennaNo();

    return getAntMonPtForCarmaAntNo( carmaAntNo );
}


SubarrayControlImpl::AntMonPtGroup
SubarrayControlImpl::getAntMonPtGroupForAntControlsGroup(
    const string &           commandName,
    const AntControlsGroup & aGroup)
{
    AntMonPtGroup ampGroup;

    AntControlsGroup::const_iterator i = aGroup.begin();
    const AntControlsGroup::const_iterator iEnd = aGroup.end();

    for ( ; i != iEnd; ++i ) {
        const AntennaControls * const antControls = *i;

        if ( antControls == 0 )
            continue;

        ampGroup.insert( &(getAntMonPtForAntControls( *antControls )) );
    }

    return ampGroup;
}


SubarrayControlImpl::AntMonPtGroup
SubarrayControlImpl::getAntMonPtGroupForCarmaAntNoSeq(
    const string &        commandName,
    const CarmaAntNoSeq & carmaAntNoSeq,
    const bool            allowZero,
    const bool            ignoreDupes,
    const bool            skipAntsNotOwnedByMe )
{
    AntControlsGroup aGroup = getAntControlsGroupForCarmaAntNoSeq(
                    commandName, carmaAntNoSeq,
                    allowZero, ignoreDupes, skipAntsNotOwnedByMe );

    return getAntMonPtGroupForAntControlsGroup(commandName,aGroup);
}


AntennaAssignmentSeq *
SubarrayControlImpl::getAntennaAssignments( )
try {
    if ( antManager_.get() == 0 )
        throw CARMA_ERROR( "getAntennaAssignments before ant manager is up" );

    const AntManager::AssignmentVec assignments =
        antManager_->getAssignments();

    const size_t count = assignments.size();

    AntennaAssignmentSeq_var result( new AntennaAssignmentSeq( count ) );
    {
        result->length( count );

        AntManager::AssignmentVec::const_iterator i =
            assignments.begin();

        const AntManager::AssignmentVec::const_iterator iEnd =
            assignments.end();

        for ( size_t j = 0; i != iEnd; ++i, ++j ) {
            (*result)[ j ].carmaAntennaNo = i->carmaAntNo;

            (*result)[ j ].carmaAntennaName =
                string_dup( i->carmaAntName.c_str() );

            (*result)[ j ].typedAntennaName =
                string_dup( i->typedAntName.c_str() );
        }
    }

    return result._retn();
} catch ( ... ) {
    rethrowCaughtAsUser();

    // just to shut the compiler warning up
    throw CARMA_EXCEPTION( util::UserException,
                           "Very bad things in SaCI::getAntennaAssignments()" );
}


namespace {

    void
    checkCarmaAntNo( const CORBA::Short carmaAntNo )
    {
        if ( (carmaAntNo < 1) || (carmaAntNo > kMaxValidCarmaAntNo) ) {
            string msg;
            {
                ostringstream oss;
                oss << "Bad carma antenna number " << carmaAntNo;
                msg = oss.str();
            }

            programLogErrorIfPossible( msg );

            throw CARMA_ERROR( msg );
        }

    } // checkCarmaAntNo

  void checkCorrInput( const ControlCorrelatorDesignation corr, const CORBA::Short corrInputNo )
    {
        switch ( corr ) {
            case util::CORR_NONE:
                if ( corrInputNo != 0 )
                    throw CARMA_ERROR( "Input number must be 0 for CORR_NONE" );
                break;

            case util::CORR_SPECTRAL:
                if ( (corrInputNo < 1) || (corrInputNo > 16) ) {
                    ostringstream oss;

                    oss << "Invalid spectral line correlator input number ("
                        << corrInputNo << ")";

                    throw CARMA_ERROR( oss.str() );
                }
                break;

            case util::CORR_WIDEBAND:
                if ( (corrInputNo < 1) || (corrInputNo > 8) ) {
                    ostringstream oss;

                    oss << "Invalid wideband correlator input number ("
                        << corrInputNo << ")";

                    throw CARMA_ERROR( oss.str() );
                }
                break;
        default:
          {
        ostringstream oss;
        oss << "Unknown correlator designation " << corr;
        throw CARMA_ERROR( oss.str() );
          }
          break;

        }

    } // checkCorrInput

    int getAntSubarrayNo( const short                       carmaAntNo,
                          const monitor::ControlSubsystem & control )
    {
        ControlSubsystemBase::Antenna & controlSubsysAnt =
            control.antenna( carmaAntNo - 1 );

        const MonitorPointInt & antSaNoMp = controlSubsysAnt.subarrayNumber();
        if ( !antSaNoMp.isValid() ) {
            ostringstream err;
            err << "getAntSubarrayNo - Control.Antenna" << carmaAntNo
                << ".subarrayNumber is invalid.";
            throw CARMA_ERROR( err.str() );
        }

        return antSaNoMp.getValue();
    }

} // namespace < unnamed >

AddAntennaResult *
SubarrayControlImpl::addAntenna( const SeqShort & carmaAntNoSeq,
                                 const bool       skipAntsOwnedByOthers )
try {
  programLogNoticeIfPossible("SS addAntenna ACI");
    cmdlog() << "addAntenna("
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ", "
             << "skipAntsOwnedByOthers=" << skipAntsOwnedByOthers << ")";

    const ScopedLogNdc ndc( "SaCI::addAntenna()" );

    if ( antManager_.get() == 0 )
        throw CARMA_ERROR( "addAntenna before ant manager is up" );

    AntManager::AddResult addResult;
    {
        const TrackerThreadSync sync( *this );

        addResult =
            antManager_->addAntennas( "addAntenna",
                                      carmaAntNoSeq,
                                      false,
                                      carmaMonitor_,
                                      *obs_,
                                      skipAntsOwnedByOthers,
                                      initializationFlag_ );

        CarmaAntNoSet::const_iterator i    = addResult.actuallyAdded.begin();
        CarmaAntNoSet::const_iterator iEnd = addResult.actuallyAdded.end();
        for ( ; i != iEnd; ++i ) {
            const short carmaAntNo = *i;
            const short antIndex   = carmaAntNo - 1;

            ControlSubsystemBase::Antenna & outgoingControlSubsysAnt =
                controlSubsystem_.antenna( antIndex );

            outgoingControlSubsysAnt.dcInput().setValue(carmaAntNo);
            outgoingControlSubsysAnt.loChannel().setValue(carmaAntNo);
            outgoingControlSubsysAnt.loberotatorChannel().setValue(carmaAntNo);
            outgoingControlSubsysAnt.iFchannel().setValue(carmaAntNo);

            // Set the command monitor point values for offsets.
            carma::monitor::EquatOffset & outgoingEquatOffset
                = outgoingControlSubsysAnt.antCommands().equatOffset();

            outgoingEquatOffset.timestamp().setValue(Time::MJD());
            outgoingEquatOffset.ra().setValue(0);
            outgoingEquatOffset.dec().setValue(0);

            // Update the antenna location
            AntControlsGroup g =
                getAntControlsGroupForCarmaAntNo("addAntenna", carmaAntNo);
            AntennaControls* ac = *(g.begin());
            updateLocation(*ac);

            // update the antenna and global delay values
            delayEngine_->setAntennaDelays(carmaAntNo, 
                antDelay_.at(antIndex),
                opticsDelayMM_.at(antIndex),
                opticsDelayCM_.at(antIndex),
                extraLOcableDelayMM_.at(antIndex),
                extraLOcableDelayCM_.at(antIndex)
                );
            delayEngine_->setAdjustableDelay(carmaAntNo , globalDelay_ );

            // Inform the LineLength system that the antenna has changed
            // to a new LO Reference synthesizer (hardwired to each subarray)
            if ( lineLength_.get() != 0 )
                lineLength_->setAntennaLORef(carmaAntNo, subarrayNo_);
        }
    }

    AddAntennaResult_var result( new AddAntennaResult );

    assignSetToSequence( addResult.actuallyAdded,
                         result->actuallyAdded );

    assignSetToSequence( addResult.alreadyInSubarray,
                         result->alreadyInSubarray );

    assignSetToSequence( addResult.ownedByOtherSubarrays,
                         result->ownedByOtherSubarrays );


    // Recompute and broadcast updated delay data for new antenna
    // only if the subarray is already initialized.
    if ( initializationFlag_ ) {
        const DelayFrameVec dfv = delayEngine_->computeDelays();
        broadcastDelayData( dfv );
    }

    markStateChange();

    return result._retn();
} catch ( ... ) {
    rethrowCaughtAsUser();

    // just to shut the compiler warning up
    throw CARMA_EXCEPTION( util::UserException,
                           "Very bad things in SaCI::addAntenna()" );
}


RemoveAntennaResult *
SubarrayControlImpl::removeAntenna( const SeqShort & carmaAntNoSeq,
                                    const bool       skipAntsNotOwnedByMe )
try {
    cmdlog() << "removeAntenna("
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ", "
             << "skipAntsNotOwnedByMe=" << skipAntsNotOwnedByMe << ")";

    if ( antManager_.get() == 0 ) {
        const string msg =
            "SaCI::removeAntenna() before ant manager is up";

        throw CARMA_ERROR( msg );
    }

    AntManager::RemoveResult removeResult;
    {
        const TrackerThreadSync sync( *this );

        removeResult =
            antManager_->removeAntennas( "removeAntenna",
                                         carmaAntNoSeq,
                                         true,
                                         false,
                                         carmaMonitor_,
                                         skipAntsNotOwnedByMe );
    }

    RemoveAntennaResult_var result( new RemoveAntennaResult );

    assignSetToSequence( removeResult.actuallyRemoved,
                         result->actuallyRemoved );

    assignSetToSequence( removeResult.alreadyInMaint,
                         result->alreadyInMaint );

    assignSetToSequence( removeResult.ownedByOtherSubarrays,
                         result->ownedByOtherSubarrays );

    markStateChange();

    return result._retn();
} catch ( ... ) {
    rethrowCaughtAsUser();

    // just to shut the compiler warning up
    throw CARMA_EXCEPTION( util::UserException,
                           "Very bad things in SaCI::removeAntenna()" );
}


namespace {


struct ::timeval
getFutureTime( const struct ::timeval & absBase,
               const unsigned long      millis ) {
    const long long absMicros =
        static_cast< long long >( absBase.tv_sec ) * 1000LL * 1000LL +
        static_cast< long long >( absBase.tv_usec ) +
        static_cast< long long >( millis ) * 1000LL;

    const long long absSecs = (absMicros / (1000LL * 1000LL));

    struct ::timeval absResult;

    absResult.tv_sec = absSecs;
    absResult.tv_usec = absMicros - (absSecs * 1000LL * 1000LL);

    return absResult;
}


}  // namespace < anonymous >


void
SubarrayControlImpl::waitForAllNormal(
    WorkResultSet &     wrs,
    const unsigned long lateAfterMillis,
    const bool          logStatus )
try {
    struct ::timeval absBase;

    ::gettimeofday( &absBase, 0 );

    string wrsId = "< WRS ??? >";

    try {
        const string id = wrs.getId();

        if ( id.empty() == false )
            wrsId = escapeAndQuoteStringAsNeeded( id );
    } catch ( ... ) {
        // Just stifle any exceptions
    }

    try {
        const string msg = "waiting for " + wrsId;

        if ( logStatus )
            programLogInfoIfPossible( msg );

        CARMA_CPTRACE( Trace::TRACE4, msg );
    } catch ( ... ) {
        // Just stifle any exceptions
    }

    const unsigned long giveUpMillis =
        ::std::max( 10000UL,  // 10 seconds
                    (2 * lateAfterMillis) );

    bool allDoneAndNormal = false;
    unsigned long waitMillis = lateAfterMillis;

    while ( allDoneAndNormal == false ) {
        const bool lastTry = (waitMillis >= giveUpMillis);

        const WorkResultSet::PostState postStateAfterTimeout =
            (lastTry ? WorkResultSet::LATE_DROPPED_POST_STATE :
                       WorkResultSet::LATE_POST_STATE);

        try {
            wrs.waitForAll( getFutureTime( absBase, waitMillis ),
                            true,
                            postStateAfterTimeout );

            allDoneAndNormal = true;
        } catch ( const WorkResultSet::WaitError & waitError ) {
            if ( waitError.hadUnfinishedKeys() == false ) {
                const string msg =
                    wrsId + " all done but " +
                    waitError.getStringForAbnormals();

                programLogErrorIfPossible( msg );
                CARMA_CPTRACE( Trace::TRACE4, msg );

                throw;
            }

            if ( lastTry ) {
                string msg;
                {
                    ostringstream oss;

                    oss << "Giving up on waiting for " << wrsId
                        << " at " << Time::getTimeString( 3 )
                        << " after " << waitMillis << "ms with "
                        << (waitError.singleUnfinishedKey() ? "key " : "keys ")
                        << waitError.getStringForUnfinishedKeys()
                        << " still not ready";

                    if ( waitError.hadAbnormals() ) {
                        oss << " and "
                            << waitError.getStringForAbnormals();
                    }

                    msg = oss.str();
                }

                programLogErrorIfPossible( msg );
                CARMA_CPTRACE( Trace::TRACE4, msg );

                throw;
            }

            {
                string msg;
                {
                    const bool singleUnfinishedKey =
                        waitError.singleUnfinishedKey();

                    ostringstream oss;

                    oss << "Trying again to wait for " << wrsId
                        << " because after " << waitMillis << "ms "
                        << (singleUnfinishedKey ? "key " : "keys ")
                        << waitError.getStringForUnfinishedKeys()
                        << (singleUnfinishedKey ? " is" : " are")
                        << " still not ready";

                    msg = oss.str();
                }

                if ( false )
                    programLogErrorIfPossible( msg );

                CARMA_CPTRACE( Trace::TRACE2, msg );
            }

            waitMillis = giveUpMillis;
        }
    }

    try {
        const string msg = wrsId + " all done and good";

        if ( logStatus )
            programLogInfoIfPossible( msg );

        CARMA_CPTRACE( Trace::TRACE4, msg );
    } catch ( ... ) {
        // Just stifle any exceptions
    }
} catch ( ... ) {
    const string msg =
        "Coming out of SaCI::waitForAllNormal on an exception: " +
        getStringForCaught();

    programLogErrorIfPossible( msg );
    CARMA_CPTRACE( Trace::TRACE4, msg );

    throw;
}


void
SubarrayControlImpl::waitForAllNormal( WorkResultSet &     wrs,
                                       const unsigned long lateAfterMillis )
{
    waitForAllNormal( wrs, lateAfterMillis, false );
}


void
SubarrayControlImpl::waitForAllNormal( WorkResultSet & wrs,
                                       const bool      logStatus )
{
    waitForAllNormal( wrs, getDefaultLateAfterMillis(), logStatus );
}


void
SubarrayControlImpl::waitForAllNormal( WorkResultSet & wrs )
{
    waitForAllNormal( wrs, getDefaultLateAfterMillis() );
}


unsigned long
SubarrayControlImpl::getDefaultLateAfterMillis( )
{
    return 1000;
}


SubarrayControlImpl::AntTypeCounts
SubarrayControlImpl::getAntTypeCounts( const CarmaAntNoVec & carmaAntNoVec )
{
    AntTypeCounts result;

    CarmaAntNoVec::const_iterator i = carmaAntNoVec.begin();
    const CarmaAntNoVec::const_iterator iEnd = carmaAntNoVec.end();

    for ( ; i != iEnd; ++i ) {
        const short carmaAntNo = *i;
        const AntennaType antType = computeAntennaType( carmaAntNo );

        bool okay = false;

        switch ( antType ) {
            case ANTENNA_TYPE_OVRO:  ++(result.ovro);  okay = true;  break;
            case ANTENNA_TYPE_BIMA:  ++(result.bima);  okay = true;  break;
            case ANTENNA_TYPE_SZA:   ++(result.sza);   okay = true;  break;
        }

        if ( okay == false ) {
            ostringstream oss;

            oss << "Invalid antenna type " << antType
                << " for C" << carmaAntNo;

            throw CARMA_ERROR( oss.str() );
        }
    }

    return result;
}


SubarrayControlImpl::AntTypeCounts
SubarrayControlImpl::getAntTypeCounts( const CarmaAntNoSeq & carmaAntNoSeq )
{
    const CarmaAntNoVec carmaAntNoVec =
        convertSequenceToVector< CarmaAntNoVec::value_type >( carmaAntNoSeq );

    return getAntTypeCounts( carmaAntNoVec );
}


SubarrayControlImpl::AntTypeCounts
SubarrayControlImpl::getAntTypeCounts( const AntControlsGroup & antControls )
{
    CarmaAntNoVec carmaAntNoVec;
    {
        AntControlsGroup::const_iterator i = antControls.begin();
        const AntControlsGroup::const_iterator iEnd = antControls.end();

        for ( ; i != iEnd; ++i ) {
            const AntennaControls * const antennaControls = *i;

            if ( antennaControls == 0 )
                continue;

            carmaAntNoVec.push_back( antennaControls->getCarmaAntennaNo() );
        }
    }

    return getAntTypeCounts( carmaAntNoVec );
}


SubarrayControlImpl::AntTypeCounts
SubarrayControlImpl::getAntTypeCounts( const DriveGroup & driveGroup )
{
    CarmaAntNoVec carmaAntNoVec;
    {
        DriveGroup::const_iterator i = driveGroup.begin();
        const DriveGroup::const_iterator iEnd = driveGroup.end();

        for ( ; i != iEnd; ++i ) {
            const DriveHandle * const driveHandle = *i;

            if ( driveHandle == 0 )
                continue;

            carmaAntNoVec.push_back( driveHandle->getCarmaAntennaNo() );
        }
    }

    return getAntTypeCounts( carmaAntNoVec );
}


SubarrayControlImpl::AntTypeCounts
SubarrayControlImpl::getAntTypeCounts(const RxSelectorGroup& rxGroup )
{
    CarmaAntNoVec carmaAntNoVec;
    {
        RxSelectorGroup::const_iterator i = rxGroup.begin();
        const RxSelectorGroup::const_iterator iEnd = rxGroup.end();

        for ( ; i != iEnd; ++i ) {
            const RxSelectorHandle* const rxHandle = *i;
            if (rxHandle == 0)continue;
            carmaAntNoVec.push_back(rxHandle->getCarmaAntennaNo());
        }
    }

    return getAntTypeCounts(carmaAntNoVec);
}


// should be called every 20 seconds.
void
SubarrayControlImpl::updateShortTermMonitorPoints( double mjd )
{
    const string prefix = "SubarrayControlImpl::updateShortTermMonitorPoints";
    CARMA_CPTRACE(kTraceLevel, prefix << "setting control system MPs");
    /**
     * Set the control monitor points for the equinox
     * source position and the apparent source position
     * with respect to the array reference point.
     * Note Ephemeris::setSource will clear any Ra/Dec/Az/El source
     * offsets in the arrayRefEphem_, do NOT call it here!
     */

    arrayRefEphem_->setMJD( mjd );
    const Source source = arrayRefEphem_->getSource();

    const double ra2000 = source.getXCoordinate().radians();
    // MWP: Fix to make sure dec is within range.  Should
    // change getYCoordinate() to return DecAngle but
    // needs to be coordinated since it affects drive code.
    const double dec2000 = hackFixDec2000( source.getYCoordinate().radians() );

    const string localSourceName = sourceName();

    subarrayContainer_.phaseCenterRa().setValue(ra2000);
    subarrayContainer_.phaseCenterDec().setValue(dec2000);
    subarrayContainer_.meanEquinox().setValue(2000.0);
    subarrayContainer_.phaseCenterAppRa().setValue(arrayRefEphem_->getRa());
    subarrayContainer_.phaseCenterAppDec().setValue(arrayRefEphem_->getDec());

    subarrayContainer_.timestamp().setValue(mjd);
    subarrayContainer_.source().setValue(localSourceName);

    // set the planetary monitor points
    double majorAxis   = 0.0;
    double minorAxis   = 0.0;
    double temperature = 0.0;
    double angle       = 0.0;

    if ( Astro::isPlanet(localSourceName) ) {
    // Yeah, the Planet constructor does the isPlanet() check
    // as well, so I could be clever here and try-catch around
    // the constructor instead of the if statement. But
    // that would just be clever, not necessarily smart.

        Planet planet(localSourceName);
        planet.setMJD(mjd);
        majorAxis   = planet.majorAxis().arcSeconds();
        minorAxis   = planet.minorAxis().arcSeconds();
        temperature = planet.brightnessTemperature(
                       Frequency(loFreq_,"GHz")
                    ).kelvin();
        angle       = planet.axisAngle().degrees();
    }

    subarrayContainer_.planetMajorAxis().setValue(majorAxis);
    subarrayContainer_.planetMinorAxis().setValue(minorAxis);
    subarrayContainer_.planetTemperature().setValue(temperature);
    subarrayContainer_.planetAngle().setValue(angle);

    updateDopplerMonitorPoints ( mjd );

    //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // Mon Feb  5 11:35:00 EST 2007 - MWP
    // Kludge for Bug 367/323
    // http://www.mmarray.org/bugzilla/show_bug.cgi?id=323
    // http://www.mmarray.org/bugzilla/show_bug.cgi?id=367
    // Remove code between ++++ when 323 is properly fixed
    const Location arrayRef = obs_->getReference();
    ControlSubsystemBase::ArrayReference& arrayCenter =
            subarrayContainer_.arrayReference();
    arrayCenter.latitude().setValue( arrayRef.getLatitude().radians() );
    arrayCenter.longitude().setValue( arrayRef.getLongitude().radians() );
    arrayCenter.altitude().setValue( arrayRef.getAltitude().meters() );
    //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    // UT1-UTC and age of the IERS table.
    // Setting these every 20 seconds is overkill, but its
    // a convenient pre-existing loop.
    const double dut1 = astroTime_.ut1Utc( mjd );
    controlSubsystem_.ut1utc().setValue( dut1 );
    controlSubsystem_.iersAge().setValue( astroTime_.iersTableAge() );

    CARMA_CPTRACE(kTraceLevel, prefix << "done setting control system MPs");
}

void
SubarrayControlImpl::updateHalfSecMonitorPoints()
{
    updateUVWMonitorPoints();

    {
        ControlSubsystemBase::ThreadStats &ts = subarrayContainer_.threadStats();
        struct WorkerPoolStats stats;

        workerPool_->getStatistics( stats );

        ts.frameExceptions().setValue(stats.frameExceptions);
        ts.totalExceptions().setValue(stats.totalExceptions);

        ts.instExecuteCount().setValue(stats.instExecuteCount);
        ts.instQueueCount().setValue(stats.instQueueCount);

        ts.frameQueueTimeMaxId().setValue(stats.frameQueueTimeMaxId);
        ts.frameQueueCount().setValue(stats.frameQueueCount);
        ts.frameQueueTimeMax().setValue(stats.frameQueueTimeMax);
        ts.frameQueueTimeMin().setValue(stats.frameQueueTimeMin);
        ts.frameQueueTimeAvg().setValue(stats.frameQueueTimeAvg);

        ts.totalQueueTimeMaxId().setValue(stats.totalQueueTimeMaxId);
        ts.totalQueueCount().setValue(stats.totalQueueCount);
        ts.totalQueueTimeMax().setValue(stats.totalQueueTimeMax);
        ts.totalQueueTimeMin().setValue(stats.totalQueueTimeMin);
        ts.totalQueueTimeAvg().setValue(stats.totalQueueTimeAvg);

        ts.frameExecuteTimeMaxId().setValue(stats.frameExecuteTimeMaxId);
        ts.frameExecuteCount().setValue(stats.frameExecuteCount);
        ts.frameExecuteTimeMax().setValue(stats.frameExecuteTimeMax);
        ts.frameExecuteTimeMin().setValue(stats.frameExecuteTimeMin);
        ts.frameExecuteTimeAvg().setValue(stats.frameExecuteTimeAvg);

        ts.totalExecuteTimeMaxId().setValue(stats.totalExecuteTimeMaxId);
        ts.totalExecuteCount().setValue(stats.totalExecuteCount);
        ts.totalExecuteTimeMax().setValue(stats.totalExecuteTimeMax);
        ts.totalExecuteTimeMin().setValue(stats.totalExecuteTimeMin);
        ts.totalExecuteTimeAvg().setValue(stats.totalExecuteTimeAvg);
    }
}

//@TODO
//   Deal correctly with alternate velocity reference frames and definitions
//   especially with planets.
void
SubarrayControlImpl::updateDopplerMonitorPoints( double mjd ) {
    CARMA_CPTRACE(kTraceLevel, "Entering updateDopplerMonitorPoints()" );
    if ( !isDopplerTracking_ ) {
        // NB: Isn't this already done in computeDopplerFreq(string)
        // when sourcename = none?
        subarrayContainer_.velocity().setValue(0.0);
        subarrayContainer_.velObservatory().setValue(0.0);
        subarrayContainer_.dopplerSource().setValue( services::NO_SOURCE );
        return;
    }

    const string prefix = "SubarrayControlImpl::updateDopplerMonitorPoints";
    ControlSubsystemBase::VelFrameMonitorPointEnum::VELFRAME velframe;
    ControlSubsystemBase::VelDefMonitorPointEnum::VELDEF     veldef;

    // first update the MJD for the doppler ephemeris
    dopplerEphem_->setMJD( mjd );

    // velocity wrt to earth in meters/sec.
    double velobs = dopplerEphem_->getDoppler( FRAME_TOPOGRAPHIC );
    // monitor point units are km/s
    velobs /= 1000.0;
    const Source dopsrc = dopplerEphem_->getSource();
    double sourceVel    = dopsrc.getVelocity().kms();
    // ??? IS MIRIAD ALWAYS RADIO DEFINITION? IF SO, MUST CONVERT
    // HERE.
    subarrayContainer_.velocity().setValue( sourceVel );
    subarrayContainer_.velObservatory().setValue( velobs );
    subarrayContainer_.dopplerSource().setValue( dopplerSource_ );

    ostringstream traceOs;
    traceOs << prefix << "set doppler MPs v="
            << subarrayContainer_.velObservatory().getValue()
            << " src = "
            <<  subarrayContainer_.dopplerSource().getValue()
    ;
    CARMA_CPTRACE(kTraceLevel, traceOs.str());

    if ( Astro::isPlanet( dopplerSource_ ) || services::isEphem( dopplerSource_ )) {
        velframe = ControlSubsystemBase::VelFrameMonitorPointEnum::OBSERV;
    } else {
        velframe = ControlSubsystemBase::VelFrameMonitorPointEnum::LSR;
    }

    //  OPTICAL NOT SUPPORTED
    //    veldef = ControlSubsystemBase::VelDefMonitorPointEnum::OPTICAL;

    //** Doppler tracking is currently using the RADIO definition
    veldef = ControlSubsystemBase::VelDefMonitorPointEnum::RADIO;

    subarrayContainer_.velDef().setValue( veldef );
    subarrayContainer_.velFrame().setValue( velframe );
}


void
SubarrayControlImpl::updateUVWMonitorPoints( const double mjd )
{
    // If prewriter is in invalidation mode, force these to be invalid
    // by letting them expire.
    if ( prewriterIsInvalidating_ ) return;

    CARMA_CPTRACE(Trace::TRACE7, "Entering SaCI::updateUVWMonitorPoints");

    unsigned short antennaCount = 0;
    unsigned short invalidCount = 0;

    const AntControlsGroup antGroup = getAntControlsGroup();
    CARMA_CPTRACE(Trace::TRACE6, "AntControlsGroup size is " << antGroup.size());

    bool checkedSourceName = false;
    bool sourceNameWasNone = false;

    AntControlsGroup::const_iterator j          = antGroup.begin();
    const AntControlsGroup::const_iterator jEnd = antGroup.end();

    for ( ; j != jEnd; ++j ) {
        AntennaControls * const antControls = *j;

        if ( antControls == 0 )
            continue;

        const unsigned short carmaAntNo = antControls->getCarmaAntennaNo();

        ControlSubsystemBase::Antenna & ant =
            getAntMonPtForCarmaAntNo( carmaAntNo );

        try {
            const AntennaControls::Uvw uvw =
                antControls->interpolateUVWfor( mjd );

            ant.interpU().setValue( uvw.u );
            ant.interpV().setValue( uvw.v );
            ant.interpW().setValue( uvw.w );

            ostringstream tmpStream;
            tmpStream << "SacI::updateUVWMonitorPoints - "
                      << "interped C" << carmaAntNo << " [U,V,W] = ["
                      << uvw.u << ","
                      << uvw.v << ","
                      << uvw.w << "]" ;
            CARMA_CPTRACE(Trace::TRACE6, tmpStream.str());
            antennaCount++;
        } catch ( const util::IllegalArgumentException & iae ) {
            // Only warn if we have tracked at some point.
            // This will eliminate false warnings on start up.
            if ( checkedSourceName == false ) {
                checkedSourceName = true;

                sourceNameWasNone =
                    StringUtils::equalsIgnoreCase( sourceName(), "none" );
            }

            if ( sourceNameWasNone != true ) {
                ostringstream warning;

                warning << "Couldn't interpolate UVW for antenna C"
                       << carmaAntNo
                       << " at time " << setprecision(9) << mjd
                       << " because " << iae.getMessage();

                //programLogWarnIfPossible( warning.str() );
            }

            ant.interpU().setValidity( MonitorPoint::INVALID_NO_DATA );
            ant.interpV().setValidity( MonitorPoint::INVALID_NO_DATA );
            ant.interpW().setValidity( MonitorPoint::INVALID_NO_DATA );
            invalidCount++;
        }
    }

    CARMA_CPTRACE(Trace::TRACE6,
            "Exiting SaCI::updateUVWMonitorPoints -- updated "
            << antennaCount << " antennas; found "
            << invalidCount << " invalid antennas");
}


void
SubarrayControlImpl::setTraceLevel( const CORBA::Short traceLevel )
try {
    cmdlog() << "setTraceLevel(" << traceLevel << ")";

    {
        ostringstream oss;

        oss << "trace level is being set to " << traceLevel;

        programLogErrorIfPossible( oss.str() );
    }

    Program::getProgram().adjustTraceLevel( traceLevel );
} catch ( ... ) {
    rethrowCaughtAsUser();
}


string
SubarrayControlImpl::sourceName( ) const
{
    const ScopedSharedLock< PthreadRWLock > readLock( sourceNameGuard_ );

    return sourceName_;
}


void
SubarrayControlImpl::setSourceName( const string & sourceName )
{
    const ScopedExclusiveLock< PthreadRWLock > writeLock( sourceNameGuard_ );

    sourceName_ = sourceName;
}

void
SubarrayControlImpl::saveControlSubsystemState( const char * filename )
try {

    cmdlog() << "saveControlSubsystemState( filename=" << filename << ").";

    carma::monitor::writeContainerToFile( carmaMonitor_.control(),
                                          filename );
} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::restoreControlSubsystemFromFile( const char * filename )
try {
    cmdlog() << "restoreControlSubsystemFromFile( filename=" << filename << ").";

    carma::monitor::setContainerFromFile( controlSubsystem_,
                                          filename );
} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::restorationInProgress( const bool restoring )
try {
    cmdlog() << "restorationInProgress( restoring=" << restoring << ").";

    restorationInProgress_ = restoring;
} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::signalControlSubsystemRestored( )
try {
    cmdlog() << "signalControlSubsystemRestored( ).";

    controlSubsystemRestored_ = true;
} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::markStateChange( )
{
    if ( !restorationInProgress_ )
        controlSubsystem_.stateChangeTimestamp( ).setValue( Time::MJD() );
}

carma::control::PadOffsets
SubarrayControlImpl::convertBaseline( const double       X,
                                      const double       Y,
                                      const double       Z,
                                      const CORBA::Short carmaAntNo )
try {
    const Length XL(X/Physical::NANOSEC_PER_METER,"meter");
    const Length YL(Y/Physical::NANOSEC_PER_METER,"meter");
    const Length ZL(Z/Physical::NANOSEC_PER_METER,"meter");

    carmaMonitor_.readNewestConditionalCopy();

    AntennaControls::PersistentInfo antPersistentInfo =
        AntManager::retrieveAntPersistentInfo(carmaAntNo, carmaMonitor_, *obs_);

    const Pad pad = antPersistentInfo.pad;

    const vector< Length * > enu = services::convertBaseline(pad,XL,YL,ZL);

    // we must subtract off any antenna offsets which have been
    // put in via the antennaOffsets() command.
    vector< Length > aOffsets;
    aOffsets.reserve( 3 );
    aOffsets.push_back( antPersistentInfo.eastAntennaOffset );
    aOffsets.push_back( antPersistentInfo.northAntennaOffset );
    aOffsets.push_back( antPersistentInfo.upAntennaOffset );

    PadOffsets p;
    {
        p.east  = enu[0]->millimeters() - aOffsets[0].millimeters();
        p.north = enu[1]->millimeters() - aOffsets[1].millimeters();
        p.up    = enu[2]->millimeters() - aOffsets[2].millimeters();
        p.antNo = carmaAntNo;
        p.padNo = pad.getPadNo();
    }

    return p;
} catch ( ... ) {
    rethrowCaughtAsUser( );

    // just to shut the compiler warning up
    throw CARMA_EXCEPTION( util::UserException,
                           "Very bad things in SaCI::convertBaseline()" );
}

SubarrayControlImpl::CarmaBandNoSet
SubarrayControlImpl::
getCarmaBandNoSetForCarmaBandNoVec(
        const string & commandName,
        const CarmaBandNoVec & carmaBandNoVec )
{
    CarmaBandNoSet result;
    {
        bool invalidZero = false;
        CarmaBandNoSet negatives;
        CarmaBandNoSet nonexistents;
        CarmaBandNoSet badDupes;

        {
            CarmaBandNoVec::const_iterator i = carmaBandNoVec.begin();
            const CarmaBandNoVec::const_iterator iEnd = carmaBandNoVec.end();

            for ( ; i != iEnd; ++i ) {
                const short carmaBandNo = *i;

                if ( carmaBandNo == 0 ) {
                    invalidZero = true;

                    continue;
                }

                if ( carmaBandNo < 0 ) {
                    negatives.insert( carmaBandNo );

                    continue;
                }

                if ( carmaBandNo > (short)getMaxNumBands(getCorrelatorDesignation())) {
                    nonexistents.insert( carmaBandNo );

                    continue;
                }

                const bool wasInserted = result.insert( carmaBandNo ).second;

                // already inserted
                if (wasInserted == false) badDupes.insert( carmaBandNo );
            }
        }

        if ( invalidZero ||
             (negatives.empty() == false) ||
             (nonexistents.empty() == false) ||
             (badDupes.empty() == false) ) {
            string message;
            {
                ostringstream oss;

                oss << "Error -";

                if ( invalidZero ) {
                    oss << " Band number 0 is invalid because it is"
                        << " not the only entry in the list (if it was the"
                        << " only entry it would indicate that the "
                        << commandName << " command should should be applied"
                        << " to all bands.";
                }

                if ( negatives.empty() == false ) {
                    if ( negatives.size() == 1 ) {
                        oss << " Negative band number "
                            << formatAsRanges( negatives )
                            << " is invalid.";
                    } else {
                        oss << " Negative band numbers "
                            << formatAsRanges( negatives )
                            << " are invalid.";
                    }
                }

                if ( nonexistents.empty() == false ) {
                    if ( nonexistents.size() == 1 ) {
                        oss << " Band number "
                            << formatAsRanges( nonexistents )
                            << " is invalid.";
                    } else {
                        oss << " Band numbers "
                            << formatAsRanges( nonexistents )
                            << " are invalid.";
                    }
                }

                if ( badDupes.empty() == false ) {
                    if ( badDupes.size() == 1 ) {
                        oss << " Band number "
                            << formatAsRanges( badDupes )
                            << " is duplicated.";
                    } else {
                        oss << " Band numbers "
                            << formatAsRanges( badDupes )
                            << " are duplicated.";
                    }
                }

                message = oss.str();
            }

            logSaCIError( commandName, message );

            throw CARMA_ERROR( message );
        }
    }

    return result;
}

bool SubarrayControlImpl::antIsOffline(unsigned antNo)
{
#if 0
  controlSubsystem_.readNewestConditionalCopy();
  int antSaNo = getAntSubarrayNo( antNo,  controlSubsystem_ );
  return antSaNo == kMaintSaNo;
#else
  return false;
#endif
}

bool SubarrayControlImpl::antIsInSubarray(unsigned antNo)
{
#if 0
  controlSubsystem_.readNewestConditionalCopy();
  int antSaNo = getAntSubarrayNo( antNo,  controlSubsystem_ );
  return antSaNo == subarrayNo_;
#else
  return true;
#endif
}

bool SubarrayControlImpl::antIsInAnotherSubarray(unsigned antNo)
{
  return !(antIsOffline(antNo) || antIsInSubarray(antNo));
}

AntennaControls::PersistentInfo
SubarrayControlImpl::retrieveAntPersistentInfo( 
            const short carmaAntNo, 
            const monitor::MonitorSystem & monSys, 
            services::Observatory & obs)
{
    return antManager_->retrieveAntPersistentInfo(carmaAntNo, monSys, obs) ;
}

void 
SubarrayControlImpl::setInvalidationForMosaics( bool invalidate ) 
try {
    prewriterIsInvalidating_ = invalidate;
    prewriter_->activateInvalidationForMosaics( invalidate );
    {
    // force persistent mon pts to be restored.
        const TrackerThreadSync sync( *this );
        trackerThread_->updateTracking();
    }
} catch ( ... ) {
    rethrowCaughtAsUser();
}
