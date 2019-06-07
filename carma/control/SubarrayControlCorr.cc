/**
 *
 * Carma control interface server implementation for commands related to
 * the correlator, downconverter and LO.
 *
 * @author: Steve Scott
 *
 * $Id: SubarrayControlCorr.cc,v 1.405 2014/08/26 21:46:36 scott Exp $
 *
 * $CarmaCopyright$
 *
 */


#include <sstream>

#include "carma/control/LOchain.h"
#include "carma/control/SubarrayControlImpl.h"

#include "carma/control/AntennaControls.h"
#include "carma/control/DriveHandle.h"
#include "carma/monitor/ControlCorrelEnum.h"
#include "carma/monitor/CorrDesignation.h"
#include "carma/control/CorrelatorHandle.h"
#include "carma/control/CorrelatorInfo.h"
#include "carma/control/FaultHandle.h"
#include "carma/control/LineLengthHandle.h"
#include "carma/control/LOrefHandle.h"
#include "carma/control/DownconverterHandle.h"
#include "carma/control/RxSelectorHandle.h"
#include "carma/control/SignalPathMapperHandle.h"
#include "carma/control/FunctorWorkRequest.h"
#include "carma/control/HandleMethodFunctorGroup.h"
#include "carma/control/RemoteObjMethodFunctorGroup.h"
#include "carma/control/SatThreadSync.h"
#include "carma/control/ManualFlag.h"
#include "carma/control/VlbiHandle.h"
#include "carma/control/WorkerPool.h"
#include "carma/control/errorMsgs.h"
#include "carma/correlator/obsRecord2/obsRecordUtils.h"

#include "carma/antenna/common/RxTypeInfo.h"
#include "carma/downconverter/common/DownconverterControl.h"
#include "carma/downconverter/spectral/BlockDownconverter.h"
#include "carma/fault/FaultControl.h"
#include "carma/interferometry/DelayEngine.h"
#include "carma/monitor/BimaSubsystem.h"
#include "carma/monitor/C3gMax8PipelineSubsystemExt.h"
#include "carma/monitor/C3gMax23PipelineSubsystemExt.h"
#include "carma/monitor/ControlBandCommon.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/monitor/SignalPathSubsystem.h"
#include "carma/monitor/SlPipelineSubsystemExt.h"
#include "carma/monitor/SzaSubsystem.h"
#include "carma/monitor/WbPipelineSubsystemExt.h"
#include "carma/services/Frequency.h"
#include "carma/services/stringConstants.h"
#include "carma/signalpath/SignalPathMapperControl.h"

#include "carma/util/AstroBand.h"
#include "carma/util/corbaSequenceUtils.h"
#include "carma/util/corrUtils.h"
#include "carma/util/CorrelatorSet.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/rangeFormatting.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/StringUtils.h"
#include "carma/util/WorkResult.h"

#include <boost/foreach.hpp>

using namespace ::std;
using namespace carma;
using namespace carma::antenna;
using namespace carma::antenna::common;
using namespace carma::control;
using namespace carma::correlator;
using namespace carma::correlator::obsRecord2;
using namespace carma::downconverter;
using namespace carma::fault;
using namespace carma::interferometry;
using namespace log4cpp;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::signalpath;
using namespace carma::util;

typedef carma::antenna::common::RxControl    RX;
typedef ControlBandPoints::BlockDCpolarizationMonitorPointEnum BLOCKDCPOL;

static void
clearManualFlagMonitorPoints(ControlSubsystemBase::ManualFlagCorrelator &mfc)
{
    typedef carma::monitor::ManualFlagPreferenceMonitorPointEnum MFPrefMPE;

    const unsigned int count = static_cast<unsigned int>(mfc.slotCount());
    for (unsigned int i = 0; i < count; i++) {
        ControlSubsystemBase::ManualFlagSlot &slot = mfc.slot(i);

        slot.band().setValue(0);
        slot.input1().setValue(0);
        slot.input2().setValue(0);
        slot.preference().setValue(MFPrefMPE::NONE);
    }
}


static void
initializeManualFlagMonitorPoints(ControlSubsystemBase::ManualFlagCorrelator &mfc, const std::string &name)
{
    typedef carma::monitor::ManualFlagPreferenceMonitorPointEnum MFPrefMPE;

    const std::string filename = "control/" + name + ".tab";
    const std::string confname = carma::util::ProgramBase::getConfFile(filename);
    const std::vector<ManualFlagPtr> vec = parseManualFlagTable(confname, true);

    const unsigned int count = static_cast<unsigned int>(mfc.slotCount());
    if (vec.size() > count) {
        std::ostringstream oss;
        oss << "too many entries in the ManualFlag configuration file: " << confname;
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    clearManualFlagMonitorPoints(mfc);

    // now fill in the real values from the configuration file
    for (unsigned int i = 0; i < vec.size(); i++) {
        const ManualFlagPtr mfptr = vec.at(i);
        ControlSubsystemBase::ManualFlagSlot &slot = mfc.slot(i);

        slot.band().setValue(mfptr->band);
        slot.input1().setValue(mfptr->input1);
        slot.input2().setValue(mfptr->input2);
        slot.preference().setValue(mfptr->preference);
    }
}


static void
clearBirdiesHelper(ControlSubsystemBase::ManualFlagCorrelator &mfc)
{
    typedef carma::monitor::ManualFlagPreferenceMonitorPointEnum MFPrefMPE;

    const unsigned int count = static_cast<unsigned int>(mfc.slotCount());
    for (unsigned int i = 0; i < count; i++) {
        ControlSubsystemBase::ManualFlagSlot &slot = mfc.slot(i);

        // clear anything marked as BIRDIE
        if (slot.preference().getValue() == MFPrefMPE::BIRDIE) {
            slot.band().setValue(0);
            slot.input1().setValue(0);
            slot.input2().setValue(0);
            slot.preference().setValue(MFPrefMPE::NONE);
        }
    }
}


static void
removeBirdieHelper(ControlSubsystemBase::ManualFlagCorrelator &mfc,
        unsigned short band, unsigned short input1, unsigned short input2)
{
    typedef carma::monitor::ManualFlagPreferenceMonitorPointEnum MFPrefMPE;

    const unsigned int count = static_cast<unsigned int>(mfc.slotCount());
    for (unsigned int i = 0; i < count; i++) {
        ControlSubsystemBase::ManualFlagSlot &slot = mfc.slot(i);

        if (slot.band().getValue() != band)
            continue;

        if (slot.input1().getValue() != input1)
            continue;

        if (slot.input2().getValue() != input2)
            continue;

        if (slot.preference().getValue() != MFPrefMPE::BIRDIE)
            continue;

        // found the specified monitor point, clear it
        slot.band().setValue(0);
        slot.input1().setValue(0);
        slot.input2().setValue(0);
        slot.preference().setValue(MFPrefMPE::NONE);

        // continue searching for more slots with identical values
    }
}


static void
addBirdieHelper(ControlSubsystemBase::ManualFlagCorrelator &mfc,
        unsigned short band, unsigned short input1, unsigned short input2)
{
    typedef carma::monitor::ManualFlagPreferenceMonitorPointEnum MFPrefMPE;

    // clear any slots which already have this exact birdie flag set
    removeBirdieHelper(mfc, band, input1, input2);

    const unsigned int count = static_cast<unsigned int>(mfc.slotCount());
    for (unsigned int i = 0; i < count; i++) {
        ControlSubsystemBase::ManualFlagSlot &slot = mfc.slot(i);

        // skip monitor points with any set values
        if (slot.band().getValue() != 0)
            continue;

        if (slot.input1().getValue() != 0)
            continue;

        if (slot.input2().getValue() != 0)
            continue;

        if (slot.preference().getValue() != MFPrefMPE::NONE)
            continue;

        // we have an empty monitor point, fill it up
        slot.band().setValue(band);
        slot.input1().setValue(input1);
        slot.input2().setValue(input2);
        slot.preference().setValue(MFPrefMPE::BIRDIE);

        // we have set a value, return
        return;
    }

    // if we get here, no value was set, and we should warn the user
    throw CARMA_ERROR("no more empty slots: cannot add BIRDIE flag");
}

static ControlSubsystemBase::ManualFlagCorrelator&
getManualFlagContainer(ControlSubsystem &subsystem, const CorrelatorSet &corrSet)
{
    if (corrSet.isEmpty()) {
        throw CARMA_ERROR("getManualFlagContainer called with empty correlator designation");
    }

    if (!corrSet.isSingleCorrelator()) {
        throw CARMA_ERROR("getManualFlagContainer called with multiple correlator designation");
    }

    if (corrSet.isSpectral()) {
        return subsystem.spectralLineCorrelator().manualFlag();
    }

    if (corrSet.isWideband()) {
        return subsystem.widebandCorrelator().manualFlag();
    }

    if (corrSet.isC3gMax8()) {
        return subsystem.c3gMax8Correlator().manualFlag();
    }

    if (corrSet.isC3gMax23()) {
        return subsystem.c3gMax23Correlator().manualFlag();
    }

    // error: unknown correlator type
    {
        std::ostringstream oss;
        oss << "getManualFlagContainer called with unknown corrrelator designation: " << corrSet;
        throw CARMA_ERROR(oss.str());
    }
}


namespace carma {
  namespace control {

    const double ANTENNA_IF_POWER_LEVEL = 0.1;

  } // namespace control
} // namespace anonymous

namespace {

  // mutex for making certain calls to delay engine.
  ::pthread_mutex_t gDelayGuard = PTHREAD_MUTEX_INITIALIZER;
  typedef ScopedLock< ::pthread_mutex_t > PThreadMutexLock;

  // Give the correlator control DO 1 second + a little extra since
  // at present empirically it appears to have an effective internal
  // timeout of about 1 second + ~(1-45)ms and I'm tired of seeing
  // these log messages about barely late results in the WWWW logs
  const unsigned long kPaddedCorrLateAfterMillis = 1060UL;

  const string kNoiseSourceName = "NOISE";
  const string kNoiseRefName = "NOISEREF";

  ::pthread_mutex_t gLastSourceNameGuard = PTHREAD_MUTEX_INITIALIZER;

} // namespace < anonymous >


/**.......................................................................
 * bandCenter and bandwidth are in GHz
 */
bool
SubarrayControlImpl::isBandCompletelyInsideBlock(double bandCenter,
                              const BlockDownconverterControl::Block block,
                              double actualBW)
{
    /*
     * Hi Marc,
     *
     * Sorry for the delay in getting back; I have had to look
     * through the various filter and mixer specs to get an
     * answer.
     *
     * On the direct path the filter does not really define an
     * edge at 5.00 GHz. There are a couple of dips in transmission
     * just above 5 GHz of <10 dB. Therefore you can put the direct
     * path edge right at 5 GHz.
     *
     * On the downconverted path there is a 5-9 GHz filter. The
     * LO is at 10 GHz, so the spectrum is flipped around 5 GHz.
     * The mixer does not have much IF/RF isolation, which means
     * that the signal leaking through directly is not much weaker
     * than the downconverted signal. An IF of 5 GHz + delta
     * is converted to a baseband signal of 5 GHz - delta, and the
     * amplitude of this needs to be compared with the direct signal
     * at 5 GHz - delta. If we require the direct signal to be 10 dB
     * less than the downconverted one, the IF should be less than
     * about 4.950 GHz.
     *
     * Bottom line is that the forbidden zone is about 5 GHz plus
     * 50 MHz, minus 0 MHz.
     *
     * >...-James
     */               
  /* Verify that the band is completely in the specified block.
   * 1.0 GHz <= fcen - 0.5BW, fcen + 0.5BW <= 5.0 GHz or
   * 5.0 GHz <= fcen - 0.5BW, fcen + 0.5BW <= 9.0 GHz
   */

    ScopedLogNdc ndc("SubarrayControlImpl::isBandCompletelyInsideBlock");
    const double bandEdgeLo = bandCenter - 0.5*actualBW;
    const double bandEdgeHi = bandCenter + 0.5*actualBW;
  if ( block == BlockDownconverterControl::LOWER ) {
    // direct path can be right at the edge. Let's define "edge" as 1MHz
    if (( bandEdgeLo < 0.999 ) || ( bandEdgeHi > 5.001 ) ) {
      ostringstream os;
      os << " LOWER BLOCK: bandEdgeLo = "<<bandEdgeLo
         << " bandEdgeHi = " << bandEdgeHi
         << " [bandCenter, bw] = "<<bandCenter << "," <<actualBW;
      programLogNoticeIfPossible( os.str() );
      return false;
    }
  } else  {
    // downconverted path has 50MHz zone
    // But I interpret JWL as 50MHz only on low end (S. Scott, 10/15/2011)
    if (( bandEdgeLo  < 5.050 ) || ( bandEdgeHi > 9.0001) ) {
      ostringstream os;
      os << " UPPER BLOCK: bandEdgeLo = "<<bandEdgeLo
         << " bandEdgeHi = " << bandEdgeHi
         << " [bandCenter, bw] = "<<bandCenter << "," <<actualBW;
      programLogNoticeIfPossible( os.str() );
      return false;
    }
  }

  return true;

}

SubarrayControlImpl::RxSelectorGroup
SubarrayControlImpl::getRxSelectorGroupForAntControlsGroup(
                               const string &           commandName,
                               const AntControlsGroup & antControlsGroup )
{
  RxSelectorGroup result;

  AntControlsGroup::const_iterator i = antControlsGroup.begin();
  const AntControlsGroup::const_iterator iEnd = antControlsGroup.end();

  while ( i != iEnd ) {
    RxSelectorHandle * const rshp = (*i)->rxSelectorHandle();
    if ( rshp != 0 )result.insert( rshp );
    ++i;
  }

  return result;
}


SubarrayControlImpl::RxSelectorGroup
SubarrayControlImpl::getRxSelectorGroupForCarmaAntNo(
                             const string & commandName,
                             const CORBA::Short carmaAntNo )
{
  return getRxSelectorGroupForAntControlsGroup(
                           commandName,
                           getAntControlsGroupForCarmaAntNo(commandName, carmaAntNo) );
}

SubarrayControlImpl::RxSelectorGroup
SubarrayControlImpl::getRxSelectorGroup( const string & commandName )
{
  return getRxSelectorGroupForAntControlsGroup( commandName,
                        getAntControlsGroup() );
}

SubarrayControlImpl::RxSelectorGroup
SubarrayControlImpl::getRxSelectorGroup(const string& commandName,
                                        const CarmaAntNoSeq& carmaAntNoSeq)
{
  const bool   allowZero             = true;
  const bool   ignoreDupes           = true;
  const bool   skipAntsNotOwnedByMe  = true;
  return getRxSelectorGroupForAntControlsGroup(commandName,
                           getAntControlsGroupForCarmaAntNoSeq(
                                           commandName, carmaAntNoSeq,
                                           allowZero, ignoreDupes, skipAntsNotOwnedByMe) );
}

SubarrayControlImpl::DownconverterGroup
SubarrayControlImpl::getDownconverterGroup( const string & commandName )
{
  DownconverterGroup result;

  DownconverterHandle * dcp = 0;

  carma::util::CorrelatorType corr_ = getCorrelatorDesignation();

  const CorrelatorSet corrSet(corr_);

  if(corrSet.includesSpectral()) {
    dcp = slDownconverter_.get();
    if ( dcp != 0 ) 
      result.insert( dcp );
  }

  if(corrSet.includesWideband()) {
    dcp = wbDownconverter_.get();
    if ( dcp != 0 ) 
      result.insert( dcp );
  }

  if(corrSet.includesC3gMax8() || corrSet.includesC3gMax23()) {
    CARMALOGINFO("Correlator set includes 3G correlator -- ignoring call to get downconverted handles for C3G");
  }

  return result;
}

DownconverterHandle *
SubarrayControlImpl::getDownconverterHandleForCorrType( const carma::util::CorrelatorType type )
{
  const CorrelatorSet corrSet(type);

  if(!corrSet.isSingleCorrelator()) {
    ThrowCarmaError("Invalid calling context (multiple correlators specified).  If you conditionally need N handles, use "
            "SubarrayControlImpl::getDownconverterGroup.");
  }

  if(corrSet.isSpectral()) {
    return slDownconverter_.get();
  } else if(corrSet.isWideband()) {
    return wbDownconverter_.get();
  } else {
    CARMALOGINFO("Warning: Ignoring call to getDownconverterHandleForCorrType for C3G correlator");
    return 0;
  }

}

//-----------------------------------------------------------------------
// getCorrelatorGroup() family of accessors, and related functions
//-----------------------------------------------------------------------

/**.......................................................................
 * Add the requested correlator handle to the result set.
 */
SubarrayControlImpl::CorrelatorGroup
SubarrayControlImpl::addCorrelatorHandle(SubarrayControlImpl::CorrelatorGroup& result,
                     const short                           bandNo,
                     carma::util::CorrelatorType          passedCorrType)
{
  const CorrelatorSet passedCorrSet(passedCorrType);
  CorrelatorSet corrSet;

  // If the passed correlator type is a default value, use the current value
  if ( passedCorrSet.isEmpty() ) {
      corrSet.initialize( getCorrelatorDesignation() );
  } else  {
      corrSet = passedCorrSet;
  }

  if(corrSet.includesSpectral()) {
    if(slCorrelatorVec_.get() != 0) {
      CorrelatorHandle* const correlatorHandle = slCorrelatorVec_->at(bandNo - 1);
      
      if(correlatorHandle != 0) {
        result.insert(correlatorHandle);
      } 
    }
  }

  if(corrSet.includesWideband()) {
    if(wbCorrelatorVec_.get() != 0) {
      CorrelatorHandle* const correlatorHandle = wbCorrelatorVec_->at(bandNo - 1);
      
      if(correlatorHandle != 0) {
        result.insert(correlatorHandle);
      } 
    }
  }

  if ( corrSet.includesC3gMax8() )  {
      CorrelatorHandle * const correlatorHandle = c3gMax8Correlator_.get();
      if ( correlatorHandle != 0) {
          result.insert(correlatorHandle);
      } 
      // Don't allow both c3gmax8 and c3gmax23.
      // Signalpath mapper needs rules to prevents this.
  } else if ( corrSet.includesC3gMax23() )  {
      CorrelatorHandle * const correlatorHandle = c3gMax23Correlator_.get();
      if ( correlatorHandle != 0) {
          result.insert(correlatorHandle);
      } 
  }

  return result;
}

/**.......................................................................
 * Return all correlator handles managed by this SAC, optionally
 * switching on correlator type
 */
SubarrayControlImpl::CorrelatorGroup
SubarrayControlImpl::getCorrelatorGroup(const string&                commandName, 
                    carma::util::CorrelatorType passedCorrType)
{
  CorrelatorGroup result;

  // If the passed correlator type is a default value, use the current value
  const CorrelatorSet passedCorrSet(passedCorrType);
  CorrelatorSet corrSet;

  if ( passedCorrSet.isEmpty() ) {
      corrSet.initialize( getCorrelatorDesignation() );
  } else  {
      corrSet = passedCorrSet;
  }


  if(corrSet.includesSpectral()) {
    if(slCorrelatorVec_.get() != 0) {
      for(CorrelatorHandleVector::const_iterator i = slCorrelatorVec_->begin(); i != slCorrelatorVec_->end(); i++) {
    CorrelatorHandle* const correlatorHandle = *i;
    
    if(correlatorHandle != 0)
      result.insert(correlatorHandle);
      }
    }
  }

  if(corrSet.includesWideband()) {
    if(wbCorrelatorVec_.get() != 0) {
      for(CorrelatorHandleVector::const_iterator i = wbCorrelatorVec_->begin(); i != wbCorrelatorVec_->end(); i++) {
    CorrelatorHandle* const correlatorHandle = *i;
    
    if(correlatorHandle != 0)
      result.insert(correlatorHandle);
      }
    }
  } 

  if ( corrSet.includesC3gMax8() )  
  {
      CorrelatorHandle * const correlatorHandle = c3gMax8Correlator_.get();
      if ( correlatorHandle != 0)
          result.insert(correlatorHandle);
  }

  if ( corrSet.includesC3gMax23() ) 
  {
      CorrelatorHandle * const correlatorHandle = c3gMax23Correlator_.get();
      if ( correlatorHandle != 0)
          result.insert(correlatorHandle);
  }

  checkCorrelatorGroupReturn(result.empty(), false);

  return result;
}

/**.......................................................................
 * Get correlator handles for the requested bandNo and correlator type
 */
SubarrayControlImpl::CorrelatorGroup
SubarrayControlImpl::getCorrelatorGroup(const string&            commandName,
                    const short              bandNo,
                    carma::util::CorrelatorType           corrType,
                    bool                     allowZero)
{
  checkBandNo(bandNo, allowZero);

  CorrelatorGroup result;

  // Return all bands, if bandNo == 0

  if(bandNo == 0)
    result = getCorrelatorGroup(commandName, corrType);
  else
    addCorrelatorHandle(result, bandNo, corrType);

  return result;
}

/**.......................................................................
 * Get correlator handles for the passed vector of bandNos and
 * correlator type
 */
SubarrayControlImpl::CorrelatorGroup
SubarrayControlImpl::getCorrelatorGroup(const string&            commandName,
                    const CarmaBandNoVec &   carmaBandNoVec,
                    carma::util::CorrelatorType           corrType,
                    bool                     allowZero)
{
  // If band 0 was requested

  if(allowZero &&                         // constant time check
     (carmaBandNoVec.empty() == false) && // constant time check
     (carmaBandNoVec.at(0)   == 0)     && // constant time check
     (carmaBandNoVec.size()  == 1))       // linear time check if false
    return getCorrelatorGroup(commandName, corrType);

  // Else convert to a set

  const CarmaBandNoSet carmaBandNoSet =
    getCarmaBandNoSetForCarmaBandNoVec(commandName,
                       carmaBandNoVec);

  CorrelatorGroup result;

  for(CarmaBandNoSet::const_iterator i = carmaBandNoSet.begin(); i != carmaBandNoSet.end(); i++)
    addCorrelatorHandle(result, *i, corrType);
  
  checkCorrelatorGroupReturn(result.empty(), carmaBandNoVec.empty());
  
  return result;
}

/**.......................................................................
 * Get correlator handles for the requested bands
 */
SubarrayControlImpl::CorrelatorGroup
SubarrayControlImpl::getCorrelatorGroup(const string&    commandName,
                    const vector<SignalPathMapperControl::CorrelatorBand>& carmaBandNoVec)
{
  
  CorrelatorGroup result;
  for(unsigned iCorrBand=0; iCorrBand < carmaBandNoVec.size(); iCorrBand++) 
    addCorrelatorHandle(result, carmaBandNoVec[iCorrBand].bandNo, carmaBandNoVec[iCorrBand].crate.type);
  
  checkCorrelatorGroupReturn(result.empty(), carmaBandNoVec.empty());

  return result;
}

/**.......................................................................
 * Get correlator handles for the passed sequence of bandNos
 */
SubarrayControlImpl::CorrelatorGroup
SubarrayControlImpl::getCorrelatorGroup(const string&            commandName,
                    const CarmaBandNoSeq&    bandNoSeq,
                    carma::util::CorrelatorType           corrType,
                    const bool               allowZero)
{

  const CarmaBandNoVec bvec =
    convertSequenceToVector< CarmaBandNoVec::value_type >(bandNoSeq);

  return getCorrelatorGroup(commandName, bvec, corrType, allowZero);
}

/**.......................................................................
 * Error handling function for getCorrelatorGroup() family of functions
 */
void SubarrayControlImpl::checkCorrelatorGroupReturn(bool outputIsEmpty, bool inputIsEmpty)
{
  if(outputIsEmpty && !inputIsEmpty) {

    ostringstream oss;
    
    oss << "WARNING: band number vector parameter "
    << "mapped to an empty set of CorrelatorHandle pointers.";
    
    programLogWarnIfPossible( oss.str() );
  }
}


SubarrayControlImpl::VlbiGroup
SubarrayControlImpl::getVlbiGroup( const string & commandName )
{
  VlbiGroup result;

  if ( vlbiVec_.get() != 0 ) {
    VlbiHandleVector::const_iterator i = vlbiVec_->begin();
    const VlbiHandleVector::const_iterator iEnd = vlbiVec_->end();

    for ( ; i != iEnd; ++i ) {
      VlbiHandle * const vlbiHandle= *i;

      if ( vlbiHandle!= 0 && vlbiHandle->isObjReachable(false))
        result.insert( vlbiHandle);
    }
  }

  return result;
}

SubarrayControlImpl::VlbiGroup
SubarrayControlImpl::getVlbiGroupForBandNoVec(
                            const string &        commandName,
                            const CarmaBandNoVec & carmaBandNoVec,
                            bool                  allowZero)
{
  if ( allowZero &&                         // constant time check
       (carmaBandNoVec.empty() == false) &&  // constant time check
       (carmaBandNoVec.at( 0 ) == 0) &&      // constant time check
       (carmaBandNoVec.size() == 1) )        // linear time check if false
    return getVlbiGroup( commandName );

  const CarmaBandNoSet carmaBandNoSet =
    getCarmaBandNoSetForCarmaBandNoVec( commandName,
                    carmaBandNoVec);
  VlbiGroup result;
  {
    CarmaBandNoSet::const_iterator i = carmaBandNoSet.begin();
    const CarmaBandNoSet::const_iterator iEnd = carmaBandNoSet.end();
    for ( ; i != iEnd; ++i ) {
      const short bandNo = *i;
      if(1 <= bandNo && (unsigned short)bandNo <= vlbiVec_->size()) {
        VlbiHandle * const vlbiHandle = vlbiVec_->at( bandNo - 1 );

        if ( vlbiHandle != 0  && vlbiHandle->isObjReachable(false))
          result.insert( vlbiHandle );
      }
    }
  }

  // Empty VlbiGroup is OK, no need to warn

  return result;
}

SubarrayControlImpl::VlbiGroup
SubarrayControlImpl::getVlbiGroupForBandNoSeq(
                            const string & commandName,
                            const CarmaBandNoSeq & bandNoSeq,
                            const bool     allowZero )
{

  const CarmaBandNoVec bvec =
    convertSequenceToVector< CarmaBandNoVec::value_type >( bandNoSeq );

  return getVlbiGroupForBandNoVec( commandName, bvec, allowZero );
}

SubarrayControlImpl::VlbiGroup
SubarrayControlImpl::getVlbiGroupForBandNoParam(
                        const string & commandName,
                        const short    bandNo,
                        const bool     allowZero )
{
  checkBandNo( bandNo, allowZero );

  VlbiGroup result;

  if ( bandNo == 0 )
    result = getVlbiGroup( commandName );
  else {
    if(1 <= bandNo && (unsigned short)bandNo <= vlbiVec_->size()) {
      VlbiHandle * const vlbiHandle = vlbiVec_->at( bandNo - 1 );

      if ( vlbiHandle != 0 && vlbiHandle->isObjReachable(false))
        result.insert( vlbiHandle );
    }
  }

  return result;
}

void
SubarrayControlImpl::updateCorrelator( const DelayFrameVec & delayFrameVec )
try {

    const ScopedLogNdc ndc( "SaCI::updateCorrelator" );
    const string currentSourceName = sourceName();

    typedef signalpath::SignalPathMapperControl::PolarizationType  SPPolType;

    const bool kVerboseLogging = false; //debug

    CARMA_CPTRACE( Trace::TRACE4, "Entering" );

    const size_t numDelayFrames = delayFrameVec.size();

    if ( numDelayFrames != 3 ) {
        ostringstream oss;
        oss << "Missing delay frames [nframes ="
            << numDelayFrames << "], expected 3";
        programLogCriticalIfPossible( oss.str() );
        throw CARMA_ERROR( oss.str() );
    }

    const DelayEngineSubsystem * const df0 = delayFrameVec.at( 0 );
    const DelayEngineSubsystem * const df1 = delayFrameVec.at( 1 );
    const DelayEngineSubsystem * const df2 = delayFrameVec.at( 2 );

    if ( (df0 == 0) || (df1 == 0) || (df2 == 0) ) {
        programLogErrorIfPossible( "NULL delay frame pointer for index" );

        return;
    }
    {//debug
        ostringstream os;
        os << "DEBUG P1 DELAY(ant=1) : "
           << df0->delayData(0).totalDelayPol1().getValue()
           << ", "
           << df1->delayData(0).totalDelayPol1().getValue()
           << ", "
           << df2->delayData(0).totalDelayPol1().getValue()
           ;
        os << "DEBUG P2 DELAY(ant=1) : "
           << df0->delayData(0).totalDelayPol2().getValue()
           << ", "
           << df1->delayData(0).totalDelayPol2().getValue()
           << ", "
           << df2->delayData(0).totalDelayPol2().getValue()
           ;
        //programLogNoticeIfPossible( os.str() );
    }


    // We get each active Astroband and then loop over the
    // CorrelatorBands in that Astroband to aggregate delays
    // for those CorrelatorBands, then send down
    // that full set of delay triplets for the Astroband
    // to each of its CorrelatorBands.  The reason the CorrelatorBand
    // must know the delay for its mate is because the bias
    // delay subtracted off at the PowerPCs must be determined by
    // the *total range of delay* across the entire AstroBand.

    if ( signalPathMapper_.get() == 0 ) {
        programLogWarnIfPossible("Handle to SignalPathMapper is null!");
        return;
    }

    vector<SignalPathMapperControl::AstroBand> astroBands
      = signalPathMapper_->getActiveAstroBands( getCorrelatorDesignation() );
    vector<SignalPathMapperControl::AstroBand>::const_iterator
        abi = astroBands.begin();
    vector<SignalPathMapperControl::AstroBand>::const_iterator
        abiEnd = astroBands.end();

    for ( ; abi != abiEnd; ++abi ) {

        const unsigned short astroBandNo = abi->astroBandNo;
        if ( astroBandNo == 0 )  continue;  // zero signals not active??

        vector<SignalPathMapperControl::CorrelatorBand> cbands
            = signalPathMapper_->getCorrelatorBands( astroBandNo );

        vector<SignalPathMapperControl::CorrelatorBand>::const_iterator   cbi = cbands.begin();
        vector<SignalPathMapperControl::CorrelatorBand>::const_iterator cbiEnd = cbands.end();

        // Needed to test for subarray membership and offline antennas.
        // See comment about "end-run" below.
        vector<SignalPathMapperControl::Antenna> antsInAB
                = signalPathMapper_->getAntennas( astroBandNo );
        set<unsigned short> antsUsedByThisAstroBand;
        for (unsigned idx =0; idx < antsInAB.size(); idx++)
            antsUsedByThisAstroBand.insert( antsInAB[idx].antNo );

        // Create this vector outside the loop over CorrelatorBands
        // so that it contains all relevant inputs and delays
        // for the AstroBand
        vector< DelayTriplet > delayTriplets;


        for ( ; cbi != cbiEnd; ++cbi ) {
            const SignalPathMapperControl::CorrelatorBand corrBand = *cbi;

            vector<SignalPathMapperControl::CorrelatorBandInput> cbiSeq =
                signalPathMapper_->getCorrelatorBandInputMap( corrBand );

            vector<SignalPathMapperControl::CorrelatorBandInput>::const_iterator cbii = cbiSeq.begin();
            vector<SignalPathMapperControl::CorrelatorBandInput>::const_iterator cbiiEnd = cbiSeq.end();


            for ( ; cbii != cbiiEnd; ++cbii ) {
                const SignalPathMapperControl::CorrelatorBandInput corrBandInput = *cbii;
                const unsigned short carmaAntNo = corrBandInput.antIF.antNo;

                if ( corrBandInput.aBandInput.inputNo == 0 ) continue; // zero signals not active

                if ( carmaAntNo == 0 ) continue; // zero signals not active

                 /********************************************************
                 * THIS CALL IS NOT FUNCTIONING BECAUSE OF THE AUTOWRITER BUG
                 * THEREFORE WE DO AN END-RUN AROUND IT BY USING THE
                 * SIGNALPATHMAPPER TO CONSTRUCT A SET OF ONLINE ANTENNAS
                 * IN THIS SUBARRAY.
                 *********************************************************
                // weed out antennas not in this subarray
                if ( !antIsInSubarray(antNo) ) continue;
                 */

                // weed out antennas not in this subarray.
                if ( antsUsedByThisAstroBand.find( carmaAntNo ) == antsUsedByThisAstroBand.end() ) continue;

                SPPolType polType = corrBandInput.antIF.polType;

                DelayEngineSubsystem::DelayData & antDelayData0 =
                        df0->delayData( carmaAntNo - 1 );

                DelayEngineSubsystem::DelayData & antDelayData1 =
                        df1->delayData( carmaAntNo - 1 );

                DelayEngineSubsystem::DelayData & antDelayData2 =
                        df2->delayData( carmaAntNo - 1 );

                DelayTriplet tripletp1;

                // EML:  Correlator requires the astroband input number,
                // not the correlator band input number

                tripletp1.inputNumber = corrBandInput.aBandInput.inputNo;
                tripletp1.timestamp0  = antDelayData0.calculatedFor().getValue();

                // If POL_NONE, assign the left polarization.
                // We assume Pol1 is always left (??!)

                tripletp1.delay0 =
                    ( polType ==
                      signalpath::SignalPathMapperControl::POL_RIGHT ?
                        antDelayData0.totalDelayPol2().getValue() :
                        antDelayData0.totalDelayPol1().getValue()
                    );

                tripletp1.timestamp1 = antDelayData1.calculatedFor().getValue();
                tripletp1.delay1 =
                    ( polType ==
                      signalpath::SignalPathMapperControl::POL_RIGHT ?
                        antDelayData1.totalDelayPol2().getValue() :
                        antDelayData1.totalDelayPol1().getValue()
                    );


                tripletp1.timestamp2 = antDelayData2.calculatedFor().getValue();
                tripletp1.delay2 =
                    ( polType ==
                      signalpath::SignalPathMapperControl::POL_RIGHT ?
                        antDelayData2.totalDelayPol2().getValue() :
                        antDelayData2.totalDelayPol1().getValue()
                    );

                delayTriplets.push_back( tripletp1 );
                { //debug
                    ostringstream os;
                    os  << "DelayTriplets for band "
                        << corrBand.bandNo
                        << "  Antenna " << carmaAntNo
                        << "  Input Number " << tripletp1.inputNumber
                        << "  TIMES ["
                        << tripletp1.timestamp0
                        << ","
                        << tripletp1.timestamp1
                        << ","
                        << tripletp1.timestamp2
                        << "]  "
                        << "DELAYS ["
                        << tripletp1.delay0
                        << ","
                        << tripletp1.delay1
                        << ","
                        << tripletp1.delay2
                        << "]"
                        ;
                    const string delaymsg = os.str();
                    if ( kVerboseLogging )
                        programLogInfoIfPossible( delaymsg );
                    CARMA_CPTRACE(Trace::TRACE4, delaymsg );
                }

            }
        } // for CorrelatorBands, collecting delays

        // now loop again over the CorrelatorBands to
        // actually send the delays out.
        cbi = cbands.begin();
        for ( ; cbi != cbiEnd; ++cbi ) {
            const SignalPathMapperControl::CorrelatorBand corrBand = *cbi;

            if ( delayTriplets.empty() ) {
                ostringstream os;
                os << "delayTriplets for band "
                   << corrBand.bandNo
                   << " are empty";
                const string msg = os.str();

                CARMA_CPTRACE( Trace::TRACE4, msg );

                if ( kVerboseLogging )
                    programLogInfoIfPossible( msg );
            } else {
                if ( kVerboseLogging ) {
                    ostringstream os;
                    os << "Total delay vector size sent to correlators is "
                       << delayTriplets.size();
                    programLogInfoIfPossible( os.str() );
                }


                const CorrelatorGroup correlatorGroup =
                    getCorrelatorGroup( "< updateCorrelator >",
                            corrBand.bandNo, 
                            corrBand.crate.type, 
                            false );

                if ( correlatorGroup.empty() ) {
                    ostringstream os;
                    os << "Skipping setInputDelayTriplets call for correlator band "
                       << corrBand.bandNo
                       << " because its correlator group is empty.";

                    const string msg = os.str() ;
                    if ( kVerboseLogging )
                        programLogInfoIfPossible( msg );
                    CARMA_CPTRACE( Trace::TRACE4, msg );
                } else {
                  CARMA_CPTRACE( Trace::TRACE4,
                                 "calling setInputDelayTriplet via WRS" );

                  WorkResultSet
                      wrs( "CorrelatorHandle::setInputDelayTriplets result set" );
                  //ostringstream os;
                  //os << "CorrelatorHandle["<<corrBand.bandNo<<"]::setInputDelayTriplets result set";

                  // Since we are looping over CorrelatorBand and
                  // thus correlatorGroup is unity length, we lose
                  // the benefit of parallelization in this call
                  // that we had before SignalPathMapper.
                  // Such is life.
                  queueFunctorWorkRequestGroup(
                      "CorrelatorHandle::setInputDelayTriplet()",
                      makeHandleMethodFunctorGroup(
                          correlatorGroup,
                          &CorrelatorHandle::setInputDelayTriplets,
                          delayTriplets ),
                      wrs,
                      *workerPool_,
                      false );

                  // Give the correlator control DO 1 second + a little extra since
                  // at present empirically it appears to have an effective internal
                  // timeout of about 1 second + ~(1-45)ms and I'm tired of seeing
                  // these log messages about barely late results in the WWWW logs
                  const unsigned long lateAfterMillis =
                      ::std::max( kPaddedCorrLateAfterMillis,
                                  getDefaultLateAfterMillis() );

                  waitForAllNormal( wrs, lateAfterMillis, false );
                }

                const VlbiGroup vlbiGroup =
                    getVlbiGroupForBandNoParam( "< updateVlbi >",
                                       corrBand.bandNo, false );
                if ( !vlbiGroup.empty() ) {
                  CARMA_CPTRACE( Trace::TRACE4,
                                 "calling setInputDelayTriplet for vlbi via WRS" );

                  WorkResultSet
                      vlbi_wrs( "VlbiHandle::setInputDelayTriplets result set" );

                  queueFunctorWorkRequestGroup(
                      "VlbiHandle::setInputDelayTriplet()",
                      makeHandleMethodFunctorGroup(
                          vlbiGroup,
                          &VlbiHandle::setInputDelayTriplets,
                          delayTriplets ),
                      vlbi_wrs,
                      *workerPool_,
                      false );

                  // Give the vlbi control DO 1 second + a little extra since
                  // that's how much the correaltor gets.
                  const unsigned long lateAfterMillis =
                      ::std::max( kPaddedCorrLateAfterMillis,
                                  getDefaultLateAfterMillis() );

                  try {
                    waitForAllNormal( vlbi_wrs, lateAfterMillis, false );
                  } catch(const ErrorException & e) {
                    // Log error and move on
                    e.log(log4cpp::Priority::ERROR);
                  }
                }
            }
        } // for CorrelatorBands, sending delays
    } // for AstroBands

    CARMA_CPTRACE( Trace::TRACE4, "Exiting" );
} catch ( ... ) {
    const string msg =
        "Coming out of SaCI::updateCorrelator on an exception: " +
        getStringForCaught();

    programLogErrorIfPossible( msg );

    throw;
}

/**.......................................................................
 *  Update walsh functions for all correlator handles associated with
 *  astrobands managed by this subarray
 */
void SubarrayControlImpl::updateWalshColumns()
{
  //------------------------------------------------------------
  // If we are managing the wideband correlator, do nothing, since the
  // WB corr does not have the ability to set Walsh columns
  //------------------------------------------------------------

  carma::util::CorrelatorType corr_ = getCorrelatorDesignation();

  const CorrelatorSet corrSet(corr_);
  std::vector<carma::util::CorrelatorType> corrs = corrSet.getControlCorrelatorDesignations();

  for(unsigned iCorr=0; iCorr < corrs.size(); iCorr++) {
    updateWalshColumns(corrs[iCorr]);
  }

}

/**.......................................................................
 * Update walsh columns for a specified correlator
 */
void SubarrayControlImpl::updateWalshColumns(carma::util::CorrelatorType corr)
{
  const CorrelatorSet corrSet( corr );

  // Do nothing for the wideband correlator
  if ( corrSet.isWideband() ) return;

  if ( corrSet.isC3gMax8() || corrSet.isC3gMax23() ) {
    programLogError("MWP Not yet determined what to do with walsh updating for new correlator");
    return;
  }

  //------------------------------------------------------------
  // Get the vector of SL astrobands managed by this subarray
  //------------------------------------------------------------
  
  std::vector<carma::signalpath::SignalPathMapperControl::AstroBand> 
    astroBands = signalPathMapper_->getActiveAstroBands(corr);
  
  //------------------------------------------------------------
  // Now iterate over astrobands, getting the correlator group
  // associated with each one
  //------------------------------------------------------------
  
  for(unsigned iAstroBand=0; iAstroBand < astroBands.size(); iAstroBand++) {
    
    carma::util::AstroBand astroBand(astroBands[iAstroBand].astroBandNo);
    vector<carma::signalpath::SignalPathMapperControl::CorrelatorBand> 
        bandVec = signalPathMapper_->getCorrelatorBands(astroBand.bandNo_);

    CorrelatorGroup corrGroup  = getCorrelatorGroup("updateWalshColumns", bandVec);
    
    //------------------------------------------------------------
    // Finally update Walsh columns for these correlator handles
    //------------------------------------------------------------
    
    updateWalshColumns(corrGroup, astroBand.bandNo_);
  }
}

/**.......................................................................
 * This will be called with the correlator group corresponding to a
 * single astroband
 */
void SubarrayControlImpl::updateWalshColumns(const CorrelatorGroup & correlatorGroup, unsigned astroBandNo)
{

  ScopedLogNdc ndc("SubarrayControlImpl::updateWalshColumns(CorrelatorGroup,astroBandNo)");

  if(correlatorGroup.empty()) {
    const string msg("Skipping setWalshColumns because correlator group is empty.");
    CARMA_CPTRACE( Trace::TRACE4, msg );
    return;
  }
  

  if(signalPathMapper_.get() == 0) {
    programLogWarnIfPossible("Signal path mapper handle is NULL!");
    return;
  }

  //timeout in seconds.  setbandwidth can take up to 30 seconds.
  const float timeout = 30.0; 
  const int count = 0; // ignored for WAIT_CORRELATOR

  // We have to wait for the sequence number of any previous
  // correlator command before sending down new Walsh columns.
  // Construct the sequence of band numbers here.
  SeqShort corrSeq;
  const size_t cgsize = correlatorGroup.size();
  corrSeq.length( cgsize );
  size_t i = 0;
  BOOST_FOREACH( const CorrelatorHandle * c, correlatorGroup) {
      corrSeq[i] = c->correlatorBandNo();
      ++i;
  }

  wait(WAIT_CORRELATOR, corrSeq, timeout, WAIT_ALL, count);
  
  const AntControlsGroup antControlsGroup = getAntControlsGroup();

  // Get assignments for all antennas from signalpath mapper
  // These are not guaranteed to be in antenna number order, so 
  // we make a map an insert them. std::map will sort upon insertion
  // by key, so we can afterwards pull out the values in antenna order.
  // The correlator method call wants the values in input order.
  // First construct the map of antenna number versus Walsh column number
  
  WalshColVec walshColVec = signalPathMapper_->getWalshColumnAssignment(0);  

  //------------------------------------------------------------
  // Iterate over antennas controlled by this subarray to setup the
  // antNo --> walshColNo association
  //------------------------------------------------------------

  std::map<short, int> antWalsh;

  for(AntControlsGroup::const_iterator iAnt = antControlsGroup.begin(); iAnt != antControlsGroup.end(); iAnt++) {

    const AntennaControls* const antControls = *iAnt;
    
    if(antControls == 0) {
      throw CARMA_ERROR( NULL_ANTENNA );
    }

    const short carmaAntNo = antControls->getCarmaAntennaNo() ;

    int walshColNo = getWalshCol(walshColVec, carmaAntNo);
    antWalsh.insert(make_pair(carmaAntNo, walshColNo));
  }

  //------------------------------------------------------------
  // Create the astroband input <--> walsh column map by iterating
  // over all astroband inputs of all corr bands associated with this
  // astroband
  //------------------------------------------------------------

  std::map<unsigned short, int> inputWalsh;

  // Start by getting the correlator bands associated with this astroband
  
  ::std::vector<carma::signalpath::SignalPathMapperControl::CorrelatorBand> corrBands = 
      signalPathMapper_->getCorrelatorBands(astroBandNo);

  // Now iterate over all correlator bands, getting the input map for
  // each one

  for(unsigned iCorrBand=0; iCorrBand < corrBands.size(); iCorrBand++) {
    carma::signalpath::SignalPathMapperControl::CorrelatorBand corrBand = corrBands[iCorrBand];

    ::std::vector<carma::signalpath::SignalPathMapperControl::CorrelatorBandInput> corrBandInputs = 
        signalPathMapper_->getCorrelatorBandInputMap(corrBand);

    // Iterate over all inputs for this correlator band

    for(unsigned iInput=0; iInput < corrBandInputs.size(); iInput++) {

      carma::signalpath::SignalPathMapperControl::CorrelatorBandInput& corrBandInput = corrBandInputs[iInput];
      const unsigned short carmaAntNo = corrBandInput.antIF.antNo;

      if(corrBandInput.aBandInput.inputNo == 0) 
        continue; // zero signals not active

      if(carmaAntNo == 0) 
        continue; // zero signals not active

      inputWalsh[corrBandInput.aBandInput.inputNo] = antWalsh[carmaAntNo];
    }
  }

  if(inputWalsh.size() == 0) {
//    programLogWarnIfPossible("Input<->Walsh column map is empty!");
    return;
  }

  
  // Initialize the walsh column vector to standard size expected by
  // the correlator
  
  unsigned walshColVecSize = 30;
  vector<int> walshCol90(walshColVecSize);

  for(unsigned iWalsh=0; iWalsh < walshColVecSize; iWalsh++) 
    walshCol90[iWalsh] = 0;
  
  // Now fill with real values for inputs which were configured.
  // Convert from inputNo (first) to index for indexin, and from
  // walshColNo (second) to index for the correlator, which expects
  // walsh column indices starting at 0.
  

  for(map<unsigned short,int>::const_iterator walshIter = inputWalsh.begin(); walshIter != inputWalsh.end(); walshIter++) {
    walshCol90[walshIter->first-1] = (walshIter->second > 0) ? walshIter->second-1 : 0;
  }
  
  //------------------------------------------------------------    
  // Now iterate over all correlator handles for this astroband,
  // sending the vector to each one
  //------------------------------------------------------------    


  // WHY IS THIS LOOP HERE?  Why not just send to correlatorGroup?
  /*
  for(CorrelatorGroup::iterator corrGroupIter=correlatorGroup.begin(); corrGroupIter != correlatorGroup.end(); corrGroupIter++) {
    
    CorrelatorGroup corrHandle;
    corrHandle.insert(*corrGroupIter);
  */
    
    // By default we use the same walsh sequence for 
    // 90 and 180 degree phase states,
    // so reuse the walshCol90 array in the CorrelatorHandle call.
    
    CARMA_CPTRACE( Trace::TRACE4, "calling setWalshColumns via WRS" );
    WorkResultSet wrs( "CorrelatorHandle::WalshColumns result set" );
    
    const bool noiseEnabled = false; // noise source off 
    
    // leave nstates = 0 to indciate we should retain current value
    // of active demodulation states
    
    const int nStates90  = 0;
    const int nStates180 = 0;


    queueFunctorWorkRequestGroup(
                 "CorrelatorHandle::setWalshColumns()",
                 makeHandleMethodFunctorGroup(
                                  correlatorGroup,
                                  &CorrelatorHandle::setWalshColumns,
                                  walshCol90,
                                  walshCol90, /*see comment above*/
                                  nStates90,
                                  nStates180,
                                  noiseEnabled),
                 wrs,
                 *workerPool_,
                 false );
    
    
    // Give the correlator control DO 1 second + a little extra since
    // at present empirically it appears to have an effective internal
    // timeout of about 1 second + ~(1-45)ms and I'm tired of seeing
    // these log messages about barely late results in the WWWW logs
    
    const unsigned long lateAfterMillis =
      ::std::max( kPaddedCorrLateAfterMillis,
                  getDefaultLateAfterMillis() );
    
    waitForAllNormal( wrs, lateAfterMillis, false );
  //} FOR 
}

// ---------------------- Noisesource ---------------------------------
void
SubarrayControlImpl::noiseSource( const bool stateIsOn, bool isReference )
  try {
    const TrackerThreadSync sync( *this );
    // why is this static?
    static string lastSourceName = carma::services::NO_SOURCE;

    const string stateParamString = (stateIsOn ? "state=ON" : "state=OFF");
    cmdlog() << "noiseSource(" << stateParamString << ")";

    {
      // Get source name straightened away
      const PThreadMutexLock
        lastSourceLock( gLastSourceNameGuard );

      const string currentSourceName = sourceName();

      if ( stateIsOn ) { // Turn it on
        noiseSource_     = true;
        // If current source name is not already NOISE or NOISEREF,
        // then stash away the current source name for
        // restoration later.
        if (   !StringUtils::equalsIgnoreCase(currentSourceName,
                                              kNoiseSourceName )
            && !StringUtils::equalsIgnoreCase(currentSourceName,
                                              kNoiseRefName )
               )
        {
          // Save away current source so that it can be restored later
          lastSourceName = currentSourceName;

          // Set sourcename to noise/noiseref & update monitor system
          if ( isReference )
            setSourceName(kNoiseRefName);
          else
            setSourceName(kNoiseSourceName);

          updateShortTermMonitorPoints(Time::MJD());
        }
      } else {
        noiseSource_     = false;
        // Restore source name if we are leaving noise/noiseref
        if (   StringUtils::equalsIgnoreCase(currentSourceName,
                               kNoiseSourceName )
            || StringUtils::equalsIgnoreCase(currentSourceName,
                               kNoiseRefName )
           ) 
        {
          // Set sourcename to noise & update monitor system
          setSourceName(lastSourceName);
          updateShortTermMonitorPoints(Time::MJD());
        }
      }
    } // ScopedLock<pthread_mutex_t>

    // Tell the fault system
    {
      const FaultSysGroup faultSysGroup = getFaultSysGroup();

      // Silently ignore for subarrays without fault system access
      if ( faultSysGroup.empty() == false ) {
        const string faultSysReqIdCallString =
          "FaultControl::setNoiseState(" + stateParamString + ")";

        WorkResultSet faultSysWrs( faultSysReqIdCallString );

        queueFunctorWorkRequestGroup(
                         faultSysReqIdCallString,
                         makeRemoteObjMethodFunctorGroup(
                                   faultSysGroup,
                                   "setNoiseState",
                                   stateParamString,
                                   &FaultControl::setNoiseState,
                                   static_cast<CORBA::UShort>(subarrayNo_),
                                   stateIsOn ),
                         faultSysWrs,
                         *workerPool_ );

        const unsigned long faultSysLateAfterMillis = 1000;

        waitForAllNormal( faultSysWrs, faultSysLateAfterMillis );
      }
    }

    // Tell the noise source and quad mods
    {
      const DownconverterGroup dcGroup = getDownconverterGroup( "noisesource" );
      const string dcRequestIdCallString( 
        "Downconverter::noiseSource( " + stateParamString + " )" );
      WorkResultSet dcWrs( "downconverter::noisesource result set" );

      queueFunctorWorkRequestGroup(
        dcRequestIdCallString,
        makeHandleMethodFunctorGroup( dcGroup, 
                                      &DownconverterHandle::noiseSource,
                                      stateIsOn ),
                                      dcWrs,
                                      *workerPool_ );

      waitForAllNormal( dcWrs );
      
      logSentCommand( dcRequestIdCallString, "downconverter" );

    } 

    // Tell the correlators
    {
      // get set of correlators to talk to
      const CorrelatorGroup corrGroup = getCorrelatorGroup( "noiseSource" );

      const string corrReqIdCallString =
    "setNoiseSourceState(" + stateParamString + ")";

      WorkResultSet corrWrs( "correlators::noisesource result set" );

      queueFunctorWorkRequestGroup(
                   corrReqIdCallString,
                   makeRemoteObjMethodFunctorGroup(
                                   corrGroup,
                                   corrReqIdCallString,
                                   stateParamString,
                                   &Correlator_I::setNoiseSourceState,
                                   stateIsOn ),
                   corrWrs,
                   *workerPool_ );

      logSentCommand(corrReqIdCallString, "correlators");

      // Give the correlator control DO 1 second + a little extra since
      // at present empirically it appears to have an effective internal
      // timeout of about 1 second + ~(1-45)ms and I'm tired of seeing
      // these log messages about barely late results in the WWWW logs
      const unsigned long corrLateAfterMillis =
    ::std::max( kPaddedCorrLateAfterMillis,
            getDefaultLateAfterMillis() );

      waitForAllNormal( corrWrs, corrLateAfterMillis );
    }

    // Tell the VLBI backends
    {
      // get set of vlbi backends to talk to
      const VlbiGroup vlbiGroup = getVlbiGroup( "noiseSource" );
      if(!vlbiGroup.empty()) {

        const string vlbiReqIdCallString =
    "setNoiseSourceState(" + stateParamString + ")";

        WorkResultSet vlbiWrs( "vlbis::noisesource result set" );

        queueFunctorWorkRequestGroup(
             vlbiReqIdCallString,
             makeRemoteObjMethodFunctorGroup(
                     vlbiGroup,
                     vlbiReqIdCallString,
                     stateParamString,
                     &Correlator_I::setNoiseSourceState,
                     stateIsOn ),
             vlbiWrs,
             *workerPool_ );

        logSentCommand(vlbiReqIdCallString, "vlbi backends");

        // Give the vlbi control DO 1 second + a little extra since
        // that's how long the correlators get.
        const unsigned long vlbiLateAfterMillis =
    ::std::max( kPaddedCorrLateAfterMillis,
          getDefaultLateAfterMillis() );

        try {
          waitForAllNormal( vlbiWrs, vlbiLateAfterMillis );
        } catch(const ErrorException & e) {
          // Log error and move on
          e.log(log4cpp::Priority::ERROR);
        }
      }
    }

    /* Send out delay triplets immediately iff noise source was
     * turned off because we stopped sending them when the noise
     * source was on, so they are likely out of date.
     * While a call to updateCorrelator(delayEngine_->computeDelays())
     * would probably suffice, to be safe make the top-level call 
     * so all subsystems are updated. Note the computation is
     * locked to the 20-second interval so a calculation in between
     * the interval will change the DelayEngine.calculatedAt monitor
     * point but not change the calculatedFor or validUntil.  Those
     * will still be for the active 20 second interval.
     */
    // only send to correlator and loberotater if the subarray is initialized
    if ( initializationFlag_ == true && noiseSource_ == false ) {
        const DelayFrameVec dfv = delayEngine_->computeDelays();
        broadcastDelayData( dfv );
    }
  } catch (...) {
    rethrowCaughtAsUser();
  }

void
SubarrayControlImpl::noisePreset()
  try {

    cmdlog() << "noisePreset()";
      
    const DownconverterGroup dcGroup = getDownconverterGroup( "noisepreset" );

    const string dcRequestIdCallString( "Downconverter::noisePreset" );

    WorkResultSet dcWrs( "downconverter::noisepreset result set" );

    queueFunctorWorkRequestGroup(
        dcRequestIdCallString,
        makeHandleMethodFunctorGroup( dcGroup, 
                                      &DownconverterHandle::noisePreset ),
        dcWrs,
                *workerPool_ );

      waitForAllNormal( dcWrs );

  } catch (...) {
    rethrowCaughtAsUser();
  }

void
SubarrayControlImpl::noiseAtten(const CORBA::UShort atten)
  try {
    cmdlog() << "noiseAtten(" << atten << ")";

    const DownconverterGroup dcGroup = getDownconverterGroup( "noiseatten" );

    const string dcRequestIdCallString( "Downconverter::noiseAtten" );

    WorkResultSet dcWrs( "downconverter::noiseatten result set" );

    queueFunctorWorkRequestGroup(
        dcRequestIdCallString,
        makeHandleMethodFunctorGroup( dcGroup, 
                                      &DownconverterHandle::noiseAtten,
                                      static_cast<short>( atten ) ),
        dcWrs,
                *workerPool_ );

    waitForAllNormal( dcWrs );

  } catch (...) {
    rethrowCaughtAsUser();
  }


void
SubarrayControlImpl::quadmodAtten(const CORBA::UShort atten)
  try {
    cmdlog() << "quadmodAtten(" << atten << ")";
    
    const DownconverterGroup dcGroup = getDownconverterGroup( "quadmodatten" );

    const string dcRequestIdCallString( "Downconverter::quadmodAtten" );

    WorkResultSet dcWrs( "downconverter::quadmodAtten result set" );

    queueFunctorWorkRequestGroup(
        dcRequestIdCallString,
        makeHandleMethodFunctorGroup( dcGroup, 
                                      &DownconverterHandle::quadmodAtten,
                                      static_cast<short>( atten ) ),
        dcWrs,
                *workerPool_ );

    waitForAllNormal( dcWrs );

  } catch (...) {
    rethrowCaughtAsUser();
  }


/**.......................................................................
 * Add a correlator to the set controlled by this subarray
 */
carma::util::SeqShort* SubarrayControlImpl::getAstroBandsForConfiguration(const char* confName)
  try {

    ScopedLogNdc ndc("getAstrobandsForConfiguration");

    std::vector<carma::signalpath::SignalPathMapperControl::AstroBand> bandVec;

    if (signalPathMapper_.get() != 0)
      bandVec = signalPathMapper_->getAstroBandsForConfiguration(confName, subarrayNo_, getCorrelatorDesignation());


    // Assign the resulting vector to a return sequence of shorts

    SeqShort_var result(new SeqShort(bandVec.size()));
    result->length(bandVec.size());

    for(unsigned iAstroBand=0; iAstroBand < bandVec.size(); iAstroBand++) {
      result[iAstroBand] = bandVec[iAstroBand].astroBandNo;
    }

    return result._retn();

  } catch (...) {

    std::ostringstream os;
    os << "EML: Caught an error";
    programLogInfo(os.str());

    rethrowCaughtAsUser();

    return 0;
  }

void SubarrayControlImpl::addBirdie(
    carma::util::CorrelatorType type,
    unsigned short band,
    unsigned short input1,
    unsigned short input2)
try {
    const CorrelatorSet corrSetParam(type);

    cmdlog() << "addBirdie(" << corrSetParam << ", " << band << ", "
        << input1 << ", " << input2 << ")";

    // this command only takes a single correlator type, by definition
    if (!corrSetParam.isSingleCorrelator()) {
        throw CARMA_ERROR("addBirdie() takes exactly one correlator type only");
    }

    // check that this subarray owns the correlator already
    const CorrelatorSet corrSetOwned(getCorrelatorDesignation());
    if (!corrSetOwned.includes(corrSetParam)) {
        std::ostringstream oss;
        oss << "currently owned correlators (" << corrSetOwned << ") does not"
            << " contain the correlator specified (" << corrSetParam << ")";
        throw CARMA_ERROR(oss.str());
    }

    // we are ok to proceed, clear all birdies from the correct monitor system
    ControlSubsystemBase::ManualFlagCorrelator &mfc = getManualFlagContainer(controlSubsystem_, corrSetParam);
    addBirdieHelper(mfc, band, input1, input2);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

void SubarrayControlImpl::removeBirdie(
    carma::util::CorrelatorType type,
    unsigned short band,
    unsigned short input1,
    unsigned short input2)
try {
    const CorrelatorSet corrSetParam(type);

    cmdlog() << "removeBirdie(" << corrSetParam << ", " << band << ", "
        << input1 << ", " << input2 << ")";

    // this command only takes a single correlator type, by definition
    if (!corrSetParam.isSingleCorrelator()) {
        throw CARMA_ERROR("removeBirdie() takes exactly one correlator type only");
    }

    // check that this subarray owns the correlator already
    const CorrelatorSet corrSetOwned(getCorrelatorDesignation());
    if (!corrSetOwned.includes(corrSetParam)) {
        std::ostringstream oss;
        oss << "currently owned correlators (" << corrSetOwned << ") does not"
            << " contain the correlator specified (" << corrSetParam << ")";
        throw CARMA_ERROR(oss.str());
    }

    // we are ok to proceed, clear all birdies from the correct monitor system
    ControlSubsystemBase::ManualFlagCorrelator &mfc = getManualFlagContainer(controlSubsystem_, corrSetParam);
    removeBirdieHelper(mfc, band, input1, input2);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

void SubarrayControlImpl::clearBirdies(carma::util::CorrelatorType type)
try {
    const CorrelatorSet corrSetParam(type);
    cmdlog() << "clearBirdies(" << corrSetParam << ")";

    // this command only takes a single correlator type, by definition
    if (!corrSetParam.isSingleCorrelator()) {
        throw CARMA_ERROR("clearBirdies() takes exactly one correlator type only");
    }

    // check that this subarray owns the correlator already
    const CorrelatorSet corrSetOwned(getCorrelatorDesignation());
    if (!corrSetOwned.includes(corrSetParam)) {
        std::ostringstream oss;
        oss << "currently owned correlators (" << corrSetOwned << ") does not"
            << " contain the correlator specified (" << corrSetParam << ")";
        throw CARMA_ERROR(oss.str());
    }

    // we are ok to proceed, clear all birdies from the correct monitor system
    ControlSubsystemBase::ManualFlagCorrelator &mfc = getManualFlagContainer(controlSubsystem_, corrSetParam);
    clearBirdiesHelper(mfc);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

/**.......................................................................
 * Add a correlator to the set controlled by this subarray
 */
void SubarrayControlImpl::addCorrelator(carma::util::CorrelatorType type)
  try {

      // Initialize the set with the current value of the control
      // monitor point
      CorrelatorSet currentCorrSet(getCorrelatorDesignation());
      CorrelatorSet requestedCorrSet(type);

      // Disallow both types of C3G correlator in same subarray.
      if (   (currentCorrSet.includesC3gMax8() && requestedCorrSet.includesC3gMax23() )
          || (currentCorrSet.includesC3gMax23() && requestedCorrSet.includesC3gMax8() )
         )
          ThrowCarmaError("You can't have both C3GMAX8 and C3GMAX23 correlators in the same subarray.")

      if (signalPathMapper_.get() == 0 ) return;//throw?

      signalPathMapper_->addCorrelator(type, subarrayNo_);

    // update the corresponding monitor point needed by the pipelines

      std::vector<carma::util::CorrelatorType> corrs = requestedCorrSet.getControlCorrelatorDesignations();

      for(unsigned iCorr=0; iCorr < corrs.size(); iCorr++) {
        updateCorrelatorMonitorPoints(corrs[iCorr]);
      }

       markStateChange();

  } catch ( ... ) {

    std::ostringstream os;
    os << "EML: Caught an error";
    programLogInfo(os.str());

    rethrowCaughtAsUser();
  }

/**.......................................................................
 * Remove a correlator to the set controlled by this subarray
 */
void SubarrayControlImpl::removeCorrelator(carma::util::CorrelatorType type)
  try {
    ScopedLogNdc ndc("removeCorrelator");

    // EML: get the current correlator designation first, so we're not
    // in a race with the SPM monitor system!
    
    carma::util::CorrelatorType corr = getCorrelatorDesignation();

    // Remove the entries from the configastroband map related
    // to the correlator being removed.  Avoid concurrent modiication
    // by iterating over a copy.  This must be done before the
    // band is removed by the SPM, otherwise 
    //     signalPathMapper_->getCorrTypeForAstroBand( c.first );
    // returns CORR_NONE.
    const ConfigAstroBandMap cabmapcopy = cabmap_;
    BOOST_FOREACH( ConfigAstroBandPair c, cabmapcopy ) {
       const carma::util::CorrelatorType cType = 
           signalPathMapper_->getCorrTypeForAstroBand( c.first );
       if ( cType == type ) {
           cabmap_.erase( c.first );
       } 
    }

    if (signalPathMapper_.get() != 0 ){
      signalPathMapper_->removeCorrelator(type, subarrayNo_);
    }
    else{
      return;
    }


      CorrelatorSet prevCorrSet(corr);
      CorrelatorSet currCorrSet = prevCorrSet;

      currCorrSet.removeCorrelator(type);

      // update the corresponding monitor point needed by the pipelines

     if(prevCorrSet.includesSpectral() && !currCorrSet.includesSpectral()) {
       ControlSubsystemBase::SpectralLineCorrelator &slc = controlSubsystem_.spectralLineCorrelator();
       slc.controllingSubarray().setValue("NONE");
       clearManualFlagMonitorPoints(slc.manualFlag());
     }

     if(prevCorrSet.includesWideband() && !currCorrSet.includesWideband()) {
       ControlSubsystemBase::WidebandCorrelator &wbc = controlSubsystem_.widebandCorrelator();
       wbc.controllingSubarray().setValue("NONE");
       clearManualFlagMonitorPoints(wbc.manualFlag());
     }

     if(prevCorrSet.includesC3gMax8() && !currCorrSet.includesC3gMax8()) {
       ControlSubsystemBase::C3gMax8Correlator &max8 = controlSubsystem_.c3gMax8Correlator();
       max8.controllingSubarray().setValue("NONE");
       clearManualFlagMonitorPoints(max8.manualFlag());
     }

     if(prevCorrSet.includesC3gMax23() && !currCorrSet.includesC3gMax23()) {
       ControlSubsystemBase::C3gMax23Correlator &max23 = controlSubsystem_.c3gMax23Correlator();
       max23.controllingSubarray().setValue("NONE");
       clearManualFlagMonitorPoints(max23.manualFlag());
     }

     markStateChange();

  } catch ( ... ) {

    std::ostringstream os;
    os << "EML: Caught an error";
    programLogInfo(os.str());

    rethrowCaughtAsUser();
  }

/**.......................................................................
 * Return true if the subarray owns the specified correlator
 */
bool SubarrayControlImpl::subarrayOwnsCorrelator(CORBA::Long subarrayNo, carma::util::CorrelatorType type)
  try {
    ScopedLogNdc ndc("subarrayOwnsCorrelator");
    signalPath_.readNewestConditionalCopy();
    
    CorrelatorSet corrSet(signalPath_.mapping().subarray(subarrayNo - 1).CORRELATOR_DESIGNATION_MP().getValue());
    return corrSet.includes(type);
    
  } catch ( ... ) {
    rethrowCaughtAsUser();
    return false;
  }

/**.......................................................................
 * Return true if the subarray owns a single correlator
 */
bool SubarrayControlImpl::subarrayOwnsSingleCorrelator(CORBA::Long subarrayNo)
  try {
    ScopedLogNdc ndc("subarrayOwnsSingleCorrelator");
    signalPath_.readNewestConditionalCopy();
    
    CorrelatorSet corrSet(signalPath_.mapping().subarray(subarrayNo - 1).CORRELATOR_DESIGNATION_MP().getValue());
    return corrSet.isSingleCorrelator();
    
  } catch ( ... ) {
    rethrowCaughtAsUser();
    return false;
  }

/**.......................................................................
 * Return true if the subarray owns no correlator
 */
bool SubarrayControlImpl::subarrayOwnsNoCorrelator(CORBA::Long subarrayNo)
  try {
    ScopedLogNdc ndc("subarrayOwnsNoCorrelator");
    signalPath_.readNewestConditionalCopy();
    
    CorrelatorSet corrSet(signalPath_.mapping().subarray(subarrayNo - 1).CORRELATOR_DESIGNATION_MP().getValue());
    return corrSet.isEmpty();
    
  } catch ( ... ) {
    rethrowCaughtAsUser();
    return false;
  }

/**.......................................................................
 * Return a string version of the correlator this astroband belongs to
 */
char* SubarrayControlImpl::astrobandCorrelator(CORBA::Long astrobandNo)
  try {
    ScopedLogNdc ndc("astrobandCorrelator");
    signalPath_.readNewestConditionalCopy();
    
    CorrelatorSet corrSet(signalPath_.mapping().astroband(astrobandNo - 1).input(0).CORRELATOR_DESIGNATION_MP().getValue());
    return CORBA::string_dup(corrSet.mpString().c_str());
    
  } catch ( ... ) {
    rethrowCaughtAsUser();
    return false;
  }


/**.......................................................................
 * Return a string version of the correlators currently owned by the specified subarray
 */
char* SubarrayControlImpl::ownedCorrelator(CORBA::Long subarrayNo)
  try {
    ScopedLogNdc ndc("ownedCorrelator");
    signalPath_.readNewestConditionalCopy();
    
    CorrelatorSet corrSet(signalPath_.mapping().subarray(subarrayNo - 1).CORRELATOR_DESIGNATION_MP().getValue());
    return CORBA::string_dup(corrSet.mpString().c_str());
    
  } catch ( ... ) {
    rethrowCaughtAsUser();
    return false;
  }

// ---------------------- Downconverter ---------------------------------

void
SubarrayControlImpl::psysPreset(const SeqShort& carmaAntNoSeq, const short astroBandNo)
  try {

    resetTsys( carmaAntNoSeq );

    cmdlog() << "psysPreset(carmaAnts="
      <<  getStringForCarmaAntNoSeq(carmaAntNoSeq)
      << ", astroBandNo=" << astroBandNo << ")";

    //------------------------------------------------------------
    // Check if the signalPathMapper is reachable
    //------------------------------------------------------------

    if(signalPathMapper_.get() == 0) {
      programLogWarnIfPossible("Handle to SignalPathMapper is null!");
      return;
    }

    //------------------------------------------------------------
    // Construct a vector of antennas from the input sequence
    //------------------------------------------------------------

    std::vector<short> carmaAntNoVec = getCarmaAntNoVec(carmaAntNoSeq);

    //------------------------------------------------------------
    // Construct a map of antennas from the vector
    //------------------------------------------------------------

    std::map<short, short> carmaAntNoMap;
    bool allInputs=false;

    // ants = 0 means all antennas

    if(carmaAntNoVec.size()==1 && carmaAntNoVec[0] == 0) {
      allInputs = true;

      // Else add an entry in the map for any requested antenna

    } else {
      for(unsigned iAnt=0; iAnt < carmaAntNoVec.size(); iAnt++) {
        carmaAntNoMap[carmaAntNoVec[iAnt]] = carmaAntNoVec[iAnt];
      }
    }

    //------------------------------------------------------------
    // If astroBandNo = 0, get the astrobands corresponding to this
    // subarray's correlator assignment.  Else use the passed
    // astroBandNo only.
    //------------------------------------------------------------

    ::std::vector<short> astroBandNos;

    if(astroBandNo == 0) {

      std::vector<carma::signalpath::SignalPathMapperControl::AstroBand> 
        astroBands = signalPathMapper_->getActiveAstroBands(getCorrelatorDesignation());

      for(unsigned iAstroBand=0; iAstroBand < astroBands.size(); iAstroBand++) {
        astroBandNos.push_back(astroBands[iAstroBand].astroBandNo);
      }

    } else {
      astroBandNos.push_back(astroBandNo);
    }

    //------------------------------------------------------------
    // Now we have the astroBandNos for which we want inputs.  Iterate
    // over astroBandNos to get the correlator bands corresponding to
    // each one
    //------------------------------------------------------------

    for(unsigned iAstroBandNo=0; iAstroBandNo < astroBandNos.size(); iAstroBandNo++) {

      std::vector<carma::signalpath::SignalPathMapperControl::CorrelatorBand> corrBands = 
        signalPathMapper_->getCorrelatorBands(astroBandNos[iAstroBandNo]);

      //------------------------------------------------------------
      // For this astroBand, iterate over all correlator bands that
      // comprise it
      //------------------------------------------------------------

      for(unsigned iCorrBand=0; iCorrBand < corrBands.size(); iCorrBand++) {

        carma::signalpath::SignalPathMapperControl::CorrelatorBand& corrBand = corrBands[iCorrBand];

        // Get the input map for this correlator band

        std::vector<carma::signalpath::SignalPathMapperControl::CorrelatorBandInput> corrInputs = 
          signalPathMapper_->getCorrelatorBandInputMap(corrBand);

        // Get the appropriate downconverter handle for this correlator band.

        DownconverterHandle * dcHandle = 
          getDownconverterHandleForCorrType( corrBand.crate.type );

        //------------------------------------------------------------
        // Now iterate over all inputs of this correlator band.  If
        // any input corresponds to one of the requested antennas, we
        // call psysPreset() on the corresponding downconverter input
        // and band.
        //------------------------------------------------------------

        for(unsigned iCorrInput=0; iCorrInput < corrInputs.size(); iCorrInput++) {

          // Get the input number, band and antenna number
          // corresponding to this correlator input

          carma::signalpath::SignalPathMapperControl::CorrelatorBandInput& corrInput = corrInputs[iCorrInput];

          std::vector<short> inputVec(1);

          inputVec[0]  = corrInput.inputNo;
          short bandNo = corrInput.band.bandNo;
          unsigned short antNo  = corrInput.antIF.antNo;

          // If all inputs were requested, don't bother checking which
          // antenna this correlator input traces back to

          if(allInputs) {

            if(dcHandle != 0) 
                dcHandle->psysPreset(inputVec, bandNo);

            // Else see if the map contains an entry for the antenna
            // corresponding to this input

          } else {

            // If so, call psysPreset() on the appropriate
            // downconverter and band

            if(carmaAntNoMap.find(antNo) != carmaAntNoMap.end()) {

              if(dcHandle != 0) 
                  dcHandle->psysPreset(inputVec, bandNo);

            }

          }

        }
      }
    }
  } catch ( ... ) {

    std::ostringstream os;
    os << "EML: Caught an error";
    programLogInfo(os.str());

    rethrowCaughtAsUser();
  }


void
SubarrayControlImpl::psysLevel(const double level)
  try {

    CarmaAntNoSeq allAnts;
    allAnts.length(1);
    allAnts[0] = 0;

    resetTsys( allAnts );

    cmdlog() << "psysLevel(level=" << setprecision(3) << level << ")";

    const DownconverterGroup dcGroup = getDownconverterGroup("psyslevel");

    const string dcRequestIdCallString( "Downconverter::psysLevel" );
    WorkResultSet dcWrs( "downconverter::psyslevel result set" );

    queueFunctorWorkRequestGroup( 
      dcRequestIdCallString,
      makeHandleMethodFunctorGroup( dcGroup, 
                                    &DownconverterHandle::psysLevel,
                                    level ),
      dcWrs,
      *workerPool_ );
    
    waitForAllNormal( dcWrs );

  } catch ( ... ) {
    rethrowCaughtAsUser();
  }


void
SubarrayControlImpl::ifoutPreset()
  try {
    cmdlog() << "ifoutPreset()";

    const DownconverterGroup dcGroup = getDownconverterGroup("ifoutpreset");

    const string dcRequestIdCallString( "Downconverter::ifoutpreset" );
    WorkResultSet dcWrs( "downconverter::ifoutpreset result set" );

    queueFunctorWorkRequestGroup( 
      dcRequestIdCallString,
      makeHandleMethodFunctorGroup( dcGroup, 
                                    &DownconverterHandle::ifoutPreset ),
      dcWrs,
      *workerPool_ );
    
    waitForAllNormal( dcWrs );

  } catch ( ... ) {
    rethrowCaughtAsUser();
  }


void
SubarrayControlImpl::ifoutLevel( const double level )
  try {
    cmdlog() << "ifoutLevel(level=" << setprecision(3) << level << ")";

    const DownconverterGroup dcGroup = getDownconverterGroup("ifoutlevel");

    const string dcRequestIdCallString( "Downconverter::ifoutlevel" );
    WorkResultSet dcWrs( "downconverter::ifoutlevel result set" );

    queueFunctorWorkRequestGroup( 
      dcRequestIdCallString,
      makeHandleMethodFunctorGroup( dcGroup, 
                                    &DownconverterHandle::ifoutLevel,
                                    level ),
      dcWrs,
      *workerPool_ );
    
    waitForAllNormal( dcWrs );

  } catch ( ... ) {
    rethrowCaughtAsUser();
  }


void
SubarrayControlImpl::rfPower( const bool state )
  try {
    cmdlog() << "rfPower(state=" << boolalpha << state << ")";

    const DownconverterGroup dcGroup = getDownconverterGroup("rfpower");

    const string dcRequestIdCallString( "Downconverter::rfpower" );
    WorkResultSet dcWrs( "downconverter::rfpower result set" );

    queueFunctorWorkRequestGroup( 
      dcRequestIdCallString,
      makeHandleMethodFunctorGroup( dcGroup, 
                                    &DownconverterHandle::rfPower,
                                    state ),
      dcWrs,
      *workerPool_ );
    
    waitForAllNormal( dcWrs );

  } catch ( ... ) {
    rethrowCaughtAsUser();
  }


/*
void
SubarrayControlImpl::psysLevelSingle( const double level,
                                      const short  inputNo,
                                      const short  bandNo )
  try {
    const int maxInputNo = 15;
    const int maxBandNo  = 8;
    if ((inputNo < 1) || (inputNo > maxInputNo)) {
      ostringstream os;
      os << "Input(" << inputNo << ") is out of range [1-"
     << maxInputNo << "]";
      throw CARMA_EXCEPTION(UserException, os.str().c_str());
    }
    if ((bandNo < 1) || (bandNo > maxBandNo)) {
      ostringstream os;
      os << "Band(" << bandNo << ") is out of range [1-"
     << maxBandNo << "]";
      throw CARMA_EXCEPTION(UserException, os.str().c_str());
    }

    cmdlog() << "psysLevel("
             << "level=" << setprecision(3) << level << ", "
             << "inputNo=" << inputNo << ", "
             << "bandNo=" << bandNo << ")";

    if ( downconverter_.get() != 0 )
      downconverter_->psysLevel(level, inputNo, bandNo);
  } catch ( ... ) {
    rethrowCaughtAsUser();
  }
*/


// ---------------------------- Delay ----------------------------
// This goes to the delay engine
// Sets total delay for all antennas
void
SubarrayControlImpl::setDelay()
{
  {
    // mutex protection against overlapping call from
    // SubarrayTrackerThread
    const TrackerThreadSync sync(*this);

    // note antennaNo == zero allowed by delay engine interface.
    // But don't pass antNo=zero to delay engine unless
    // you really mean "ALL antennas" not "all antennas in this
    // subarray".

    const AntControlsGroup antControlsGroup = getAntControlsGroup();

    AntControlsGroup::const_iterator i = antControlsGroup.begin();
    const AntControlsGroup::const_iterator iEnd = antControlsGroup.end();

    for ( ; i != iEnd; ++i ) {
      const AntennaControls * const antControls = *i;

      if ( antControls == 0 )
    continue;

      const unsigned short carmaAntNo = antControls->getCarmaAntennaNo();
      ControlSubsystem::Antenna& a =
    controlSubsystem_.antenna(carmaAntNo-1);
      double delay3mmPol1 = a.delayOffset3mmRx().getValue();
      double offsetPol1 = 0;
      double offsetPol2 = 0;
      // Compute the receiver offsets
      // with respect to the the 3mm polarization 1 value
      //double f = subarrayContainer_.loFreq().getValue();
      RX::Type rxType = getCurrentRxType();
      switch ( rxType ) {
      case RX::RX3MM:
    offsetPol2 = a.rxDelay3mmPol2().getValue();
    break;
      case RX::RX1MM:
    offsetPol1 = a.rxDelay1mmPol1().getValue();
    offsetPol2 = a.rxDelay1mmPol2().getValue();
    break;
      case RX::RX1CM:
    offsetPol1 = a.rxDelay1cmPol1().getValue();
    offsetPol2 = a.rxDelay1cmPol2().getValue();
    break;
      case RX::RXANY:
    return;
      }

      // "The" delay offset is the 3mm Pol1 value
      delayEngine_->setDelayOffset( carmaAntNo, delay3mmPol1 );
      // Pass along the pol1 & pol2 offsets from 3mmPol1 to
      // the delay engine.
      delayEngine_->setRxDelayPol1( carmaAntNo, offsetPol1 );
      delayEngine_->setRxDelayPol2( carmaAntNo, offsetPol2 );
    }

    // only send to correlator and loberotater if the subarray is initialized
    if ( initializationFlag_ ) {
        const DelayFrameVec dfv = delayEngine_->computeDelays();
        broadcastDelayData( dfv );
    }
  }
}

void
SubarrayControlImpl::delay(const float        offset,
                           const CORBA::Short carmaAntNo)
  try {
    ControlSubsystem::Antenna& a = controlSubsystem_.antenna(carmaAntNo-1);
    a.delayOffset3mmRx().setValue(offset);
    cmdlog() << "delay("
             << "offset=" << setprecision(3)  << offset << ", "
             << carmaAntNo << ")";
    markStateChange();
    setDelay();

  } catch ( ... ) {
    rethrowCaughtAsUser();
  }

void
SubarrayControlImpl::adjustableDelay(
                     const float      offset,
                     const SeqShort & carmaAntNoSeq
                     )
  try {

    //TODO: add state restoration monitor points.

    cmdlog() << "adjustableDelay(" << offset << ", "
             << getStringForCarmaAntNoSeq( carmaAntNoSeq ) << ")";

    // can't use carmaAntNoVec = getCarmaAntNoVec(carmaAntNoSeq)
    // here because [0] translates to 0 rather than full subarray list.
    const AntControlsGroup antControlsGroup =
      getAntControlsGroupForCarmaAntNoSeq(
                      "adjustableDelay", carmaAntNoSeq, true, true, true );
    AntControlsGroup::const_iterator i = antControlsGroup.begin();
    const AntControlsGroup::const_iterator iEnd = antControlsGroup.end();

    for ( ; i != iEnd; ++i ) {
      const AntennaControls* const antControls = *i;
      if (antControls == 0) continue;
      const unsigned short carmaAntNo = antControls->getCarmaAntennaNo();
      delayEngine_->setAdjustableDelay(carmaAntNo, offset);
    }

    // If carmaAntNoSeq was zero meaning all antennas in the subarray,
    // then set the globalDelay value.
    //
    // @todo add a compare of carmaAntNoSeq to e.g.
    // [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15], which we want to
    // behave the same as [0].
    // @see Marc's email Fri, 31 Mar 2006 17:26:48
    // @see deviant observer behavior

    const bool useAllAntennas = (carmaAntNoSeq.length() == 1)
      && (carmaAntNoSeq[ 0 ] == 0);
    if ( useAllAntennas ) globalDelay_ = offset;

    // no mutex needed. broadcastDelayData is mutex protected
    // only send to correlator and loberotater if the subarray is initialized
    if ( initializationFlag_ ) {
        const DelayFrameVec dfv = delayEngine_->computeDelays();
        broadcastDelayData( dfv );
    }

  } catch ( ... ) {
    rethrowCaughtAsUser();
  }

void
SubarrayControlImpl::delayDifference(const float diff,
                     const CORBA::Short carmaAntNo,
                     const RX::Type rx,
                     const BlockDCPolarizationType pol)
  try {
    const AntControlsGroup antControlsGroup =
      getAntControlsGroupForCarmaAntNo( "delayDifference", carmaAntNo );
    AntControlsGroup::const_iterator i = antControlsGroup.begin();
    const AntControlsGroup::const_iterator iEnd = antControlsGroup.end();

    for ( ; i != iEnd; ++i ) {
      const AntennaControls* const antControls = *i;
      if (antControls == 0) continue;
      const unsigned short carmaAntNo = antControls->getCarmaAntennaNo();

      ControlSubsystem::Antenna& a = controlSubsystem_.antenna(carmaAntNo-1);
      switch( rx ) {
      case RX::RX3MM :
    if ( pol == BLOCK_DC_POL1 )
      // you can't set an difference value for 3MM POL1 here,
      // since that is the reference delay
      return;
    if ( pol == BLOCK_DC_POL2 )
      a.rxDelay3mmPol2().setValue(diff);
    break;
      case RX::RX1MM :
    if ( pol == BLOCK_DC_POL1 )
      a.rxDelay1mmPol1().setValue(diff);
    else
      if ( pol == BLOCK_DC_POL2 )
        a.rxDelay1mmPol2().setValue(diff);
    break;
      case RX::RX1CM :
    if ( pol == BLOCK_DC_POL1 )
      a.rxDelay1cmPol1().setValue(diff);
    else
      if ( pol == BLOCK_DC_POL2 )
        a.rxDelay1cmPol2().setValue(diff);
    break;
      case RX::RXANY :
      default:
    return;
      }
      markStateChange();
      cmdlog() << "delayDiff("
           << "diff=" << setprecision(3) << diff
           << ", "
           << carmaAntNo
           << ", "
           << RxTypeInfo::stringFromRxType( rx )
           << ", "
           << ( pol == BLOCK_DC_POL1 ? "POL1" : "POL2" )
           << ")";
    }
    setDelay();

  } catch ( ... ) {
    rethrowCaughtAsUser();
  }

// ---------------------------- Freq -----------------------------
void SubarrayControlImpl::freq(double frest, SidebandType sb, double fif,
                   const char* doppler, const char* transition)
  try
  {
    const bool endSetFreqWithAbsorberInBeam = true;
    const bool optimizeReceiver             = true;
    const bool setPowerLevels               = true;
    freqCore(frest, sb, fif, doppler, transition,
             endSetFreqWithAbsorberInBeam, optimizeReceiver, setPowerLevels);
  } catch ( ... ) {
    rethrowCaughtAsUser();
  }

// ---------------------------- Qfreq -----------------------------
void SubarrayControlImpl::qfreq(double frest, SidebandType sb, double fif,
                const char* doppler, const char* transition)
  try
  {
    const bool endSetFreqWithAbsorberInBeam = false;
    const bool optimizeReceiver             = false;
    const bool setPowerLevels               = false;
    freqCore(frest, sb, fif, doppler, transition,
             endSetFreqWithAbsorberInBeam, optimizeReceiver, setPowerLevels);
  } catch ( ... ) {
    rethrowCaughtAsUser();
  }

// ---------------------------- FreqCore -----------------------------
// The quick version does not set up any power levels or move the absorber
void SubarrayControlImpl::freqCore(double frest, SidebandType sb, double fif,
                                   const string& dopplerSource,
                                   const string& transition,
                                   const bool endSetFreqWithAbsorberInBeam,
                                   const bool optimizeReceiver,
                                   const bool setPowerLevels)
  try
  {
    ScopedLogNdc ndc("SubarrayControlImpl::freqCore");
    CarmaAntNoSeq allAnts;
    allAnts.length(1);
    allAnts[0] = 0;

    // Reset tsys but try to minimize calls (psysPreset and antennaIF call too)
    if ( !setPowerLevels ) resetTsys( allAnts ); 

    string              cmdname  = "qfreq";
    if (setPowerLevels) cmdname  = "freq";
    string     sbstring = "USB";
    if (sb == SB_LOWER) sbstring = "LSB";
    cmdlog() << cmdname << "(frest="
             << setprecision(6) << frest << "GHz, sideband="
             << sbstring << ", IFfreq="
             << setprecision(6) << fif << "GHz, dopplerSource=" << dopplerSource
             << " transition=" << transition
             << " endSetFreqWithAbsorberInBeam="
             << endSetFreqWithAbsorberInBeam
             << " optimizeReceiver=" << optimizeReceiver
             << ")";

    int isb = 1;
    if (sb == SB_LOWER) isb = -1;
    string saveDoppler = dopplerSource_;
    // this also stores the dopplerSource_
    const double skyFreq =
      computeDopplerFrequencyWithInterpReset(dopplerSource, frest);
    double lofreq      = skyFreq - isb*fif;

    // Range check LO frequency
    const double LO1CM       = 35.938;
    const double freqLow1cm  = LO1CM - 9.0;
    const double freqHigh1cm = LO1CM + 1.0;
    const double freqLow3mm  =  70.0;
    const double freqHigh3mm = 116.0;
    const double freqLow1mm  = 210.0;
    const double freqHigh1mm = 270.0;
    bool  is1cm = ((lofreq >= freqLow1cm) && (lofreq <= freqHigh1cm));
    bool  is3mm = ((lofreq >= freqLow3mm) && (lofreq <= freqHigh3mm));
    bool  is1mm = ((lofreq >= freqLow1mm) && (lofreq <= freqHigh1mm));
    if (!(is1cm || is3mm || is1mm)) {
      // revert back to old settings (this also restores the doppler source)
      loFreq_ =
        computeDopplerFrequencyWithInterpReset(saveDoppler, loRestFreq_);
      ostringstream o;
      o << "LO frequency("
        << setiosflags(ios::fixed) << setprecision(1) << lofreq
        << ") is out of range [" << freqLow1cm << "-" << freqHigh1cm << "], "
        << "[" << freqLow3mm << "-" << freqHigh3mm << "], "
        << "[" << freqLow1mm << "-" << freqHigh1mm << "]";
      throw CARMA_EXCEPTION(UserException, o.str().c_str());
    }
    if (is1cm && (sb != SB_LOWER)) {
      // revert back to old settings
      loFreq_ =
        computeDopplerFrequencyWithInterpReset(saveDoppler, loRestFreq_);
      ostringstream o;
      o << "Only LSB allowed in the 1cm band; ";
      throw CARMA_EXCEPTION(UserException, o.str().c_str());
    }

    // Does not apply to WB corr, which has fixed LO2's
    lo2DopplerTrackingEnabled_ = true; 

    double dopfac;
    if (frest < 1)      dopfac = 1.0;
    else                dopfac = skyFreq/frest;
    if (dopfac < 0.001) dopfac = 1.0;
    restFreq_      = frest;
    skyFreq_       = skyFreq;
    if (is1cm) {
        lofreq = LO1CM;  // 1cm LO is fixed frequency
    }
    loFreq_        = lofreq;
    ifFreq_        = isb*(skyFreq_ - loFreq_);
    if (is1cm) {
        loRestFreq_    = loFreq_;
        ifRestFreq_    = ifFreq_;
    }
    else {
        loRestFreq_    = loFreq_/dopfac;
        ifRestFreq_    = ifFreq_/dopfac;
    }
    // Store the command monitor points
    ControlSubsystemBase::Freq& freqcmd = subarrayContainer_.commands().freq();
    freqcmd.timestamp().setValue(Time::MJD());
    freqcmd.restFreq().setValue(restFreq_);
    freqcmd.ifFreq().setValue(ifFreq_);
    typedef ControlSubsystemBase::SidebandMonitorPointEnum FREQ_SB;
    if (isb == 1) {
      freqcmd.sideband().setValue(FREQ_SB::USB);
    }
    else {
      freqcmd.sideband().setValue(FREQ_SB::LSB);
    }
    markStateChange();

    updateDopplerMonitorPoints(Time::MJD());
    Freq loFreq(loFreq_, Freq::GHz);

    // We need to distinguish between the LO freq (maybe tripled) and
    // the oscillator frequency, and then pick the rx band and aperture.
    // This is done based on the input LO frequency
    if (getCurrentRxType() == RX::RX1MM) {
      // The 1mm oscillator is tripled to create the LO
      LOchain_.setOscillatorMultiplier(3);
    }
    else {
      LOchain_.setOscillatorMultiplier(1);
    }
    Freq oscFreq(loFreq/LOchain_.getOscillatorMultiplier());

    Harmonic harm;
    try {
      LOchain_.setOscillatorFreq(oscFreq);
      harm = LOchain_.getPreferredHarmonic();
    }
    catch (std::exception& e) {
      throw CARMA_EXCEPTION(UserException, e.what());
    }

    // Update ref LO synthesizer and mon system values;
    // do this before we talk to the antennas
    updateFrequency(loFreq_);
    updateJyperk(loFreq_);
    updateSbrs(loFreq_);

    // This must be called BEFORE updateLo2Frequency
    updateBlockDownconverters();
    updateLo2DopplerFrequency();

    // Set the delay (frequency dependent) for all antennas
    setDelay();

    // issue the set cmds out to the group of antennas; Rx and Drives
    const RxSelectorGroup rxSelectorGroup = getRxSelectorGroup("freq");
    const DriveGroup driveGroup = getDriveGroup("freq");

    ostringstream oss;
    oss << fixed << "RxSelect::setFrequency(yig="
        << setprecision(3) << harm.getYigFreq().gigahertz()
        << ", " << setprecision(3) << loFreq.gigahertz()
        << " )";
    string requestIdCallString = oss.str();

    ostringstream os;
    os << "yig=" << harm.getYigFreq().gigahertz();
    string paramString = os.str();

    WorkResultSet setFrequencyWrs( "RxSelect::setFrequency result set" );

    const bool forceRelock = false;
    
    // Send rx and frequencies to Rx's
    nextTuneSeqNo_++;  // Preferred seq no
    queueFunctorWorkRequestGroup(
                 requestIdCallString,
                 makeHandleMethodFunctorGroup(
                                  rxSelectorGroup,
                                  &RxSelectorHandle::setFrequency,
                                  getCurrentRxType(),
                                  harm.getYigFreq().gigahertz(),
                                  loFreq.gigahertz(),
                                  harm.getRefFreq().gigahertz(),
                                  harm.getYigHarmonicNumber(),
                                  endSetFreqWithAbsorberInBeam,
                                  optimizeReceiver,
                                  forceRelock,
                                  static_cast<ControlSubsystem*>(&controlSubsystem_),
                                  static_cast<MonitorSystem*>(&carmaMonitor_),
                                  nextTuneSeqNo_
                                  ),
                 setFrequencyWrs,
                 *workerPool_ );
    waitForAllNormal( setFrequencyWrs );

    // Select correct aperture (optical=false, antNo=0)
    selectAperture(false, allAnts);

    // Now update the frequency used in the tracking threshold
    const string cmdName = "conditionallyUpdateTrackThreshold";
    ostringstream params;
    params << setiosflags(ios::fixed) << "freq="
           << setprecision(2) << loFreq_ ;
    paramString = params.str();

    const DriveGroup driveGroup2 = getDriveGroup(cmdName);

    // Send the tracking threshold update cmd to the group of antennas
    ostringstream o;
    o << "SubarrayControl::" << cmdName << "(" << paramString << ")";
    requestIdCallString = o.str();

    ostringstream wrsText;
    wrsText << "SubarrayControl::" << cmdName << " cmd";

    WorkResultSet wrs(wrsText.str());

    queueFunctorWorkRequestGroup(
                 requestIdCallString,
                 makeHandleMethodFunctorGroup(driveGroup2,
                                  &DriveHandle::conditionallyUpdateTrackTolerance, loFreq_),
                 wrs,
                 *workerPool_ );

    waitForAllNormal(wrs);

    // Leave NOW if not requested to set up power levels (quick)
    if (!setPowerLevels) return;

    // Wait for receiver tuning to complete
    const float TWO_MINUTES = 120.0;
    wait(control::WAIT_TUNED, allAnts, TWO_MINUTES, control::WAIT_ALL);

    // Set the IF level in the antennas (PAM) to 0.3 dBm
    antennaIFpower(0.3, allAnts);

    // Set all downconverter psys levels
    psysPreset(allAnts, 0);

    // Wait for the psysPreset to complete and then get the ambient load
    // measurement into the system. We need 4 seconds, although it is not
    // understood why. Three seconds is too short!!
    sleep(4);

    // And back to sky to finish off Tsys
    const float CAL_TMO = 13.0;
    cal(CalibratorControl::SKY, allAnts);
    wait(control::WAIT_CALIBRATOR, allAnts, CAL_TMO, control::WAIT_ALL);

  } catch ( ... ) {
    rethrowCaughtAsUser();
  }



string
SubarrayControlImpl::makeCorrModeDescString( carma::util::CorrelatorSet c )
{
  ScopedLogNdc ndc("SubarrayControlImpl::makeCorrModeDescString");

  if ( ! c.isSingleCorrelator() ) {
      ThrowCarmaError("SubarrayControlImpl::makeCorrModeDescString - cannot handle multiple correlators. This suggests problem in the calling class SubarrayControlConfigAstroBand");
  }

  ostringstream os;
  unsigned i;

  if ( c.isSpectral() ) {
    for(i = 0; i < getMaxNumBands(CORR_SPECTRAL); i++ ) {
      try {
        os << slcmodeVec_.at(i);
      } catch ( const out_of_range & ex ) {
        os << "X";
      }
      if ( i != getMaxNumBands(CORR_SPECTRAL) - 1 )
        os << "-";
    }
  } else if ( c.isWideband() ) {
    for(i = 0; i < getMaxNumBands(CORR_WIDEBAND) ; i++ ) {
      try {
        os << wbcmodeVec_.at(i);
      } catch ( const out_of_range  & ex ) {
        os << "X";
      }
      if ( i != getMaxNumBands(CORR_WIDEBAND) - 1 )
        os << "-";
    }
  } else if ( c.isC3gMax23() ) {
    for(i = 0; i < getMaxNumBands(CORR_C3GMAX23) ; i++ ) {
      try {
        os << c3gMax23modeVec_.at(i);
      } catch ( const out_of_range  & ex ) {
        os << "X";
      }
      if ( i != getMaxNumBands(CORR_C3GMAX23) - 1 )
        os << "-";
    }
  } else if ( c.isC3gMax8() ) {
    for(i = 0; i < getMaxNumBands(CORR_C3GMAX8) ; i++ ) {
      try {
        os << c3gMax8modeVec_.at(i);
      } catch ( const out_of_range  & ex ) {
        os << "X";
      }
      if ( i != getMaxNumBands(CORR_C3GMAX8) - 1 )
        os << "-";
    }
  } else if ( c.isEmpty() ) {
    // will this break sdpFiller?
    os << "NO CORRELATORS IN THIS SUBARRAY";
  }

  return os.str();
}

// ---------------------------- checkConfig -----------------------------
// @TODO modify to handle CORR_ANY for ownership of two correlators
bool
SubarrayControlImpl::checkConfig(const bool quiet)
  try {
      ScopedLogNdc ndc("SubarrayControlImpl::checkConfig(bool)");
      signalPath_.readNewestConditionalCopy();


      // Under the new paradigm, the correlatorDesignation is a
      // bitmask of correlators -- we iterate over any corrs included
      // in this subarray's mask

      const CorrelatorSet corrSet(signalPath_.mapping().subarray(subarrayNo_ - 1).CORRELATOR_DESIGNATION_MP().getValue());
      std::vector<carma::util::CorrelatorType> corrs = corrSet.getControlCorrelatorDesignations();
      bool status = true;

      for(unsigned iCorr=0; iCorr < corrs.size(); iCorr++) {
        status = status && checkConfig(quiet, corrs[iCorr]);
      }

      return status;

  } catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
  }

bool
SubarrayControlImpl::checkConfigC3G(const bool quiet, const carma::util::CorrelatorType cType)
{
    programLogNotice("checkConfigC3G:  implement me!");

    return true;
}

bool
SubarrayControlImpl::checkConfigWB(const bool quiet)
{
    typedef ControlBandPoints::ConfigurationMonitorPointEnum CONFIG;
    vector<int> badBands;
    ostringstream errmsg;
    errmsg << fixed << setprecision(10);
    int nbands = SubarrayControlImpl::getMaxNumBands(CORR_WIDEBAND);
    for (int b = 0; b < nbands; b++) {
        int corrBandNo = b+9;
        ControlBandPoints& bandMS = controlSubsystem_.widebandCorrelator().
            wbcBand(b).controlBandPoints();
        double ifFreqExpected = 1.250 + b*0.500;
        double ifFreq = bandMS.ifFreq().getValue();
        // within a kHz is close enough
        bool v = (fabs(ifFreqExpected-ifFreq) < 1.0e-6);
        if (v) {
            bandMS.configuration().setValue(CONFIG::VALID);
        }
        else {
            badBands.push_back(b);
            bandMS.configuration().setValue(CONFIG::INVALID);
            ostringstream os;
            os << "Setting wideband astroband" << corrBandNo << " to INVALID "
               << "because the IF frequency is incorrect (expected=" 
               << fixed << setprecision(10) << ifFreqExpected
               << ", got " << ifFreq << ")";
            programLogNotice(os.str());
            errmsg.str("");
            errmsg << "Astroband" << corrBandNo
                << " (wideband) has incorrect IF frequency ("
                << ifFreq
                << "); should be " << ifFreqExpected;
        }
    }
    int numBad = badBands.size();
    bool status = (numBad == 0);
    if (status || quiet) return status;
    // At least one band is invalid and we are !quiet, so throw
    if (numBad == 1) {
        throw CARMA_EXCEPTION(UserException, errmsg.str().c_str());
    }
    errmsg.str("");
    errmsg << "Incorrect frequency settings for  wideband astrobands:"; 
    for (int i = 0; i < numBad; i++) {
        int b = badBands.at(i);
        int corrBandNo = b+9;
        double ifFreq = controlSubsystem_.widebandCorrelator().
                wbcBand(b).controlBandPoints().ifFreq().getValue();
        double ifFreqExpected = 1.250 + b*0.500;
        errmsg << "\nAstroband" << corrBandNo
            << " IFfrequency=" << ifFreq
            << " expected=" << ifFreqExpected;
    }
    throw CARMA_EXCEPTION(UserException, errmsg.str().c_str());
    return status;
}

bool SubarrayControlImpl::checkConfig(const bool quiet , carma::util::CorrelatorType cType )
  try {
    if ( cType == CORR_ALL || cType == CORR_NONE ) return true;
    ScopedLogNdc ndc("SubarrayControlImpl::checkConfig(bool,CorrType)");

    if (cType == CORR_WIDEBAND) {
        return checkConfigWB(quiet);
    }   
    if (cType == CORR_C3GMAX8 || cType == CORR_C3GMAX23 ) {
        return checkConfigC3G(quiet, cType);
    }

    typedef ControlBandPoints::ConfigurationMonitorPointEnum CONFIG;
    vector<int> badBands;
    vector<int> badBlockDC;

    // Loop over all bands...
    int nbands = getMaxNumBands( cType );
    for (int b = 0; b < nbands; b++) {
      ControlBandPoints* cbp = 0;
      cbp = &controlSubsystem_.spectralLineCorrelator()
                              .slcBand(b).controlBandPoints();
      const unsigned corrBandNo = b + 1;
      if (cbp == 0) {
          ostringstream os;
          os << "Couldn't get pointer to correlator part of monitor system "
             << "for " << getStringForCorrType( cType ) << " band " << corrBandNo ;
        throw CARMA_ERROR( os.str() );
      }

      BlockDownconverterControl::Block block =
                  getBlockDownconverterEnabled(cType, corrBandNo) 
                  ? BlockDownconverterControl::UPPER
                  : BlockDownconverterControl::LOWER;

      // bandCenterIF is center of the band in IF space. 
      double bandCenterIF = cbp->ifFreq().getValue();

      // bandwidth in GHz
      float bw = cbp->bandwidth().getValue() * 1E-3;

      //    The inBand check has to be done here rather than in configband
      //    because the computation can't happen
      //    until after freq() has been called.  Otherwise the answer
      //    may be wrong.
      //    Normally checkConfig is called in scripts after freq.
      //    Don't check for CORR_WIDEBAND because we don't doppler track the
      //    2nd LO of that system.
      bool inBand = true;
      if ( cType == CORR_SPECTRAL ) {
          inBand = isBandCompletelyInsideBlock(bandCenterIF, block, bw);

          if ( !inBand ) {
                cbp->configuration().setValue(CONFIG::INVALID);
                ostringstream os;
                os << "Setting Band " << corrBandNo << " to INVALID "
                   << "because it is not fully within a blockDC block" ;
                programLogInfoIfPossible( os.str() );
          }
      }

      if (cbp->online().getValue()) {
            if (cbp->configuration().getValue() == CONFIG::INVALID )
            {
              if (quiet)return false;
              badBands.push_back( corrBandNo );
              if (!inBand) badBlockDC.push_back( corrBandNo );
            }
      }
    }

    const string longCType(cType == CORR_SPECTRAL ? 
            "Spectral correlator " : "Wideband correlator ");
    int numBad = badBands.size();
    if (numBad > 0) {
      ostringstream errMsg;
      for (int i=0; i<numBad; i++) {
            if (numBad == 1) {
              errMsg << longCType << "Band " << badBands.at(i)
               << " has an invalid configuration. ";
            }
            else if (i == 0) {
              errMsg << longCType << "Bands " << badBands.at(i) ;
            }
            else if ((i+1) < numBad) {
              errMsg << ", " << badBands.at(i) ;
            }
            else {
              errMsg << " and " << badBands.at(i)
               << " have invalid configurations. ";
            }
      }
      // this could potentially be a very long error message!
      unsigned short numBadBlockDC = badBlockDC.size();
      for (unsigned short i=0; i < numBadBlockDC; i++ ) {
          errMsg << longCType << "Band " << badBlockDC.at(i)
                 << " is not completely inside the ";
          const string bstr = getBlockDownconverterEnabled(cType, i+1)  ? "upper " : "lower ";
          errMsg << bstr
                 << " block of the downconverter.  Change the band center frequency"
                 << " in your configband command."
                 << endl;
      }
      throw CARMA_EXCEPTION(UserException, errMsg.str().c_str());
    }
    return true;
  } catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
  }

// --------------------- updateBlockDownConverters----------------------
// For use by the subarray tracker thread; just logs, doesn't throw
void SubarrayControlImpl::updateBlockDownconverters()
{
  ScopedLogNdc ndc("SacI::BlockDownConverters()");
  try {
    // If this subarray owns no correlators return immediately
    // This prevents a subarray tracker thread from another subarray from
    // stomping on this subarray's Lo2/correlator.
    if( getCorrelatorDesignation() == CORR_NONE ) return;

    // Loop over ConfigAstroBandMap and update each in turn.
    // Mutex protection is minimally over map access with a retry
    // to ensure freshness in case the map changed while we were
    // updating. [Thanks, Ira]
    const size_t mapsize = cabmap_.size();
    for( size_t i = 0 ; i < mapsize; ++i ) {
        size_t abNo = i+1; // astroband number is the map key
        size_t j = 0;
        while ( j < 10 ) {
             ++j;
             boost::shared_ptr<ConfigAstroBand> c;
             {
                 const PThreadMutexLock lock(gCabMapGuard_);
                 // If the item does not exist, break 
                 // to the next iteration of the for loop.
                 if ( cabmap_.find(abNo) == cabmap_.end() ) 
                     break;
                 c = cabmap_[abNo];
             }

             // Not under lock! can take arbitrarily long
             if ( c.get() != 0 ) 
                 c->updateBlockDownconverter();

             // Now we double check that we operated with fresh data, and retry
             // if we didn't make it
             {
                 const PThreadMutexLock lock(gCabMapGuard_);
                 // It was removed. Break to the next iteration of 
                 // the for loop.
                 if ( cabmap_.find(abNo) == cabmap_.end() ) 
                     break;
                 // It has not changed. Break to the next iteration of 
                 // the for loop.
                 if ( c == cabmap_[abNo] ) {
                     break;
                 }
             }
        } // while
        if ( j == 10 ) {
            ostringstream os;
            os << "MWP Tried 10 times to update block downconverter on cabmap_[" 
               << i << "].  Skipping.";
            programLogError(os.str());
        }
    } // for
  } catch ( const BaseException & ex ) {
    // Log Error, don't rethrow
    programLogErrorIfPossible( ex.getMessage() );
  }
}


// --------------------- updateLo2DopplerFrequency ----------------------
// For use by the subarray tracker thread; just logs, doesn't throw
void SubarrayControlImpl::updateLo2DopplerFrequency()
{
  ScopedLogNdc ndc("SacI::updateLo2DopplerFrequency()");
  try {
    // If this subarray owns no correlators return immediately
    // This prevents a subarray tracker thread from another subarray from
    // stomping on this subarray's Lo2/correlator.
    if( getCorrelatorDesignation() == CORR_NONE ) return;

    // Loop over ConfigAstroBandMap and update each in turn.
    // Mutex protection is minimally over map access with a retry
    // to ensure freshness in case the map changed while we were
    // updating. [Thanks, Ira]
    const size_t retries = 10;
    const size_t mapsize = cabmap_.size();
    for (size_t i=0; i < mapsize; i++) {
        size_t abNo = i+1; // astroband number is the map key
        size_t r = 0;
        while ( r < retries ) {
             ++r;
             boost::shared_ptr<ConfigAstroBand> c;
             {
                 const PThreadMutexLock lock(gCabMapGuard_);
                 // If the item does not exist, break 
                 // to the next iteration of the for loop.
                 if ( cabmap_.find(abNo) == cabmap_.end() ) 
                     break;

                 c = cabmap_[abNo];
             }

             // Not under lock! can take arbitrarily long
             if ( c.get() != 0 )  {
                c->updateLo2Frequency();
             }

             // Now we double check that we operated with fresh data, and retry
             // if we didn't make it
             {
                 const PThreadMutexLock lock(gCabMapGuard_);
                 // It was removed. Break to the next iteration of 
                 // the for loop.
                 if ( cabmap_.find(abNo) == cabmap_.end() ) 
                     break;
                 
                 // It has not changed. Break to the next iteration of 
                 // the for loop.
                 if ( c == cabmap_[abNo] ) 
                     break;
                     
                 // The map has changed for this abNo; retry to get the new
                 // configuration from the map for this abNo    

             }
        } // while r
        if ( r == retries ) {
            ostringstream os;
            os << "Tried " << retries 
               << " times to update LO2 freq on cabmap_[" 
               << i << "];  skipping...";
            programLogError(os.str());
        }
    } // for i
  } catch ( const BaseException & ex ) {
    // Log Error, don't rethrow
    programLogErrorIfPossible( ex.getMessage() );
  }
}


//======================================================================

// ---------------------------- Refreq -----------------------------
// The input is final LO freq in GHz
void
SubarrayControlImpl::refreq(const SeqShort& carmaAntNoSeq, const bool retune)
  try
  {
    if ( retune ) resetTsys( carmaAntNoSeq );

    cmdlog() << "refreq(carmaAntNo="
             << getStringForCarmaAntNoSeq(carmaAntNoSeq)
             << " retune=" << boolalpha << retune
             << ")";

    setDelay();

    const bool endSetFreqWithAbsorberInBeam = false;
    const bool optimizeReceiver             = retune;

    Freq loFreq(loFreq_, Freq::GHz);

    Harmonic harm = LOchain_.getPreferredHarmonic();
    ostringstream oss;
    oss << fixed << "RxSelect::setFrequency(yig="
        << setprecision(3) << harm.getYigFreq().gigahertz()
        << ", " << setprecision(3) << loFreq.gigahertz()
        << " )";
    string requestIdCallString = oss.str();

    nextTuneSeqNo_++; // Preferred seq no

    const RxSelectorGroup rxSelectorGroup =
      getRxSelectorGroup("refreq", carmaAntNoSeq);
    WorkResultSet setFrequencyWrs("RxSelect::setFrequency cmd");
    
    const bool forceRelock = true;

    // Send frequencies to Rx's
    queueFunctorWorkRequestGroup(
                 requestIdCallString,
                 makeHandleMethodFunctorGroup(
                                  rxSelectorGroup,
                                  &RxSelectorHandle::setFrequency,
                                  getCurrentRxType(),
                                  harm.getYigFreq().gigahertz(),
                                  loFreq.gigahertz(),
                                  harm.getRefFreq().gigahertz(),
                                  harm.getYigHarmonicNumber(),
                                  endSetFreqWithAbsorberInBeam,
                                  optimizeReceiver,
                                  forceRelock,
                                  static_cast<ControlSubsystem*>(&controlSubsystem_),
                                  static_cast<MonitorSystem*>(&carmaMonitor_),
                                  nextTuneSeqNo_
                                  ),
                 setFrequencyWrs,
                 *workerPool_ );

    waitForAllNormal( setFrequencyWrs );

    selectAperture(false, carmaAntNoSeq);

  } catch ( ... ) {
    rethrowCaughtAsUser();

  }

// --------------------------- Harmonics ---------------------------
// The input is in GHz
// This implementation should use the LOchain and Harmonics classes
// that are used to compute frequencies in control rather than this
// ad hoc computation.
carma::control::SeqDouble *
SubarrayControlImpl::harmonics( const double oscfreq )
  try {
    int mhmin = static_cast< int >((oscfreq - .05) / 12.5 + 1);
    int mhmax = static_cast< int >((oscfreq - .05) / 8.0);
    int nharm;
    double xGHz, refMHz;

    ostringstream o;

    const int count = 2 * (mhmax - mhmin + 1);

    SeqDouble_var seq( new SeqDouble( count ) );
    seq->length( count );

    int i = 0;
    for ( int m = mhmin; m <= mhmax; m++ ) {
      ostringstream o;
      if (m != mhmin) o << "\n";
      o << "oscfreq: " << oscfreq ;
      xGHz = (oscfreq - .05) / m;
      nharm = static_cast< int >((xGHz - .01) / 1.105);
      refMHz = 1000. * (xGHz - .01) / nharm;
      o << " m=" << m << " xGHz=" << xGHz << " refMHz=" << refMHz;
      seq[i++] =  refMHz/1000;  // GHz!
      seq[i++] =  xGHz;

    }
    CARMA_CPTRACE(Trace::TRACE7, o.str());
    //cout << o.str() << endl;

    return seq._retn();
  } catch ( ... ) {
    rethrowCaughtAsUser();

    // Just to shut up a compiler warning about "no return value"
    throw CARMA_EXCEPTION( UserException, "SaCI::harmonics() failure" );
  }


/***************
 * @TODO - Refactor flattenPhases, optimizeThresholds, calibrateSpectra,
 * as there is much repeated code and structurally the methods are
 * identical.   calibrateSpectra has extra method parameters.
 ***************/
void
SubarrayControlImpl::flattenPhases( const SeqShort & astroBandNoSeq )
  try {
    const string cid("flattenPhases");
    ScopedLogNdc ndc(cid);

    const carma::util::CorrelatorType csType = csCorrType();

    if ( subarrayIsIntegrating() )
      throw CARMA_ERROR("You may not issue flattenPhases while integrating");

    //------------------------------------------------------------
    // Check if the signalPathMapper is reachable
    //------------------------------------------------------------

    if(signalPathMapper_.get() == 0) {
      programLogWarnIfPossible("Handle to SignalPathMapper is null! Unable to execute this command");
      return;
    }

    // Precondition: noise source has been turned ON.
    // CorrelatorHandle method will throw if this condition is not met.
    CORBA::Long seqNo = 0;
    {
      const PThreadMutexLock lock( gCorrNextSeqNoGuard_ );
      seqNo = gCorrNextSeqNo_;
      gCorrNextSeqNo_ += 10;
    }

    // Since CorrelatorBands work together to flatten the phases
    // in an Astrobands in CARMA23 or FULLPOL mode, we must ensure that
    // the call to each CorrelatorBand in an AstroBand is coordinated.
    // We get each active Astroband and then loop over the
    // CorrelatorBands in that Astroband and make the remote call.

    vector<short> astroBands = convertSequenceToVector<short>( astroBandNoSeq );
    unsigned short absize = astroBands.size();

    // Note  astrobandNoSeq could be [0], meaning all astrobands,
    // so check for that here and repopulate the astroband vector
    // with the actual astroband numbers in that case.
    // Otherwise assume the input list is valid.
    if ( absize == 1 && astroBands[0] == 0 ) {
        astroBands = signalPathMapper_->getActiveAstroBandNoVec( csType );
        absize = astroBands.size();
    }

    if ( absize == 0 ) return;

    // Aggregate all individual group members into one group
    // for dispatch via queue/WorkResult method.
    CorrelatorGroup aggregatedCorrHandles;
    VlbiGroup aggregatedVlbiHandles;
    ostringstream bos;
    ostringstream vos;
    for ( unsigned short i = 0 ; i < absize ; ++i) {

        // Zero not allowed as part of a sequence that
        // otherwise includes non-zero astroband numbers.
        if ( astroBands[i] == 0 )  continue;

        // Weed out astrobands not assigned to this correlator.
        // Why is this needed, you ask? Because
        // signalpathMapper_->getCorrelatorBandNoSeq() and
        // its underlying signalpath:: calls are not correlator-type
        // aware.  So we eliminate the problem astrobands before that
        // call is made.  The issue is that getCorrelatorBandNoSeq()
        // could return a correlator band number which is valid in
        // this subarray but in reality refers to a different correlator,
        // and thus a flattenphases call can be dispatched to a
        // single correlator band instead of a pair, possibly
        // deadlocking that band.  For instance, in Sci1 with
        // the SL correlator in CARMA23 mode, "flattenPhases(9)" would
        // dispatch a call to slcor1 because the correlator band
        // number associated with astroband9 is 1.  Then bad things
        // happen because no call was also dispatched to corrband 2.

        if ( !signalPathMapper_->isValidAstroBand( astroBands[i], csType ) )
            continue;

        std::vector<carma::signalpath::SignalPathMapperControl::CorrelatorBand> bandVec =
          signalPathMapper_->getCorrelatorBands(astroBands[i]);

        const CorrelatorGroup correlatorGroup =
          getCorrelatorGroup(cid, bandVec);

        // Just needed for logging purposes
        
        CarmaBandNoSeq * correlatorBandNoSeq
          = signalPathMapper_->getCorrelatorBandNoSeq( astroBands[i] );
        
        const CarmaBandNoVec bvec =
          convertSequenceToVector< CarmaBandNoVec::value_type >( *correlatorBandNoSeq );

    
        if ( !correlatorGroup.empty() ) {

          CorrelatorGroup::iterator ci = correlatorGroup.begin();
          CorrelatorGroup::iterator ce = correlatorGroup.end();
          while ( ci != ce ) {
            //  only dispatch this command to spectral bands
            //  These cannot be weeded out by checking csCorrType
            //  because CORR_ANY is valid
            if ( ( *ci )->isSpectral() ) {
              aggregatedCorrHandles.insert( *ci );
            }
            ci++;
          }

          const size_t sz = correlatorGroup.size();
          const bool plural = ( sz > 1 );
          bos << " AstroBand " << astroBands[i];
          bos << ( plural ? " Correlator Bands [" : "Correlator Band ");
          bos << formatAsRanges(bvec,"","") ;
          bos << ( plural ? "]," : ",");
        } else {
            ostringstream xxx;
            xxx << " NO CORRELATOR BANDS FOR ASTROBAND " << astroBands[i];
            programLogWarnIfPossible( xxx.str() );
        }

        const VlbiGroup vlbiGroup =
          getVlbiGroupForBandNoSeq(cid, *correlatorBandNoSeq, true);
        VlbiGroup::iterator vi = vlbiGroup.begin();
        VlbiGroup::iterator ve = vlbiGroup.end();
        if ( !vlbiGroup.empty() ) {
          while( vi != ve ) {
            //  VLBI handles implement this function, so
            //  add them to the aggregate
            aggregatedVlbiHandles.insert( *vi );
            vi++;
          }

          const size_t sz = vlbiGroup.size();
          const bool plural = ( sz > 1 );
          vos << " AstroBand " << astroBands[i];
          vos << ( plural ? " Vlbi Bands [" : " Vlbi Band ");
          vos << formatAsRanges(bvec,"","");
          vos << ( plural ? "]," : ",");
        }
    }/// end aggregation

    // Now dispatch the commands to the aggregated handles.
    if ( ! aggregatedCorrHandles.empty() ) {
        const string b = bos.str();
        // Log the command.
        cmdlog() << "SubarrayControlImpl::flattenPhases( seqNo = "
                 << seqNo  << " ) on " << b;

        ostringstream reqOs;
        reqOs << "CorrelatorHandle::flattenPhases( seqNo="
              << seqNo  << " )";
        const string requestIdCallString = reqOs.str();

        WorkResultSet wrs( requestIdCallString );

        queueFunctorWorkRequestGroup(
             requestIdCallString,
             makeHandleMethodFunctorGroup(
                        aggregatedCorrHandles,
                        &CorrelatorHandle::flattenPhases,
                        static_cast<int>( seqNo ) ),
             wrs,
             *workerPool_ );
          // Kevin says flattenPhases takes ~ 10s so,
          // give a 20 second timeout here.
             waitForAllNormal( wrs, 20000UL, true );
    } else {
        // Log that we did not dispatch a command.
          cmdlog() << "SubarrayControlImpl::flattenPhases( seqNo = "
                   << seqNo  << " ). No correlator bands are active.";
    }

    // Also send to vlbi backends, if necessary.
    // It was not possible to aggregate these into a single
    // WorkResultSet along with the CorrelatorHandles.
    // I tried using WorkResultSet::addKey to put them all
    // together, but that resulted in the calls just hanging
    // forever.  I guess I don't understand the WRS code.
    // However, flattenPhases is a no-op for VlbiHandles,
    // so this method returns quickly (thus one wonders if
    // it should be called at all).

    if(!aggregatedVlbiHandles.empty()) {
      const string b = vos.str();

      ostringstream vlbi_reqOs;
      vlbi_reqOs << "VlbiHandle::flattenPhases( seqNo="
                 << seqNo  << " )";
      const string vlbi_requestIdCallString = vlbi_reqOs.str();
      WorkResultSet wrs( vlbi_requestIdCallString );
      queueFunctorWorkRequestGroup(
           vlbi_requestIdCallString,
           makeHandleMethodFunctorGroup(
                      aggregatedVlbiHandles,
                      &VlbiHandle::flattenPhases,
                      static_cast<int>( seqNo ) ),
           wrs,
           *workerPool_ );

      try {
        waitForAllNormal( wrs, 20000UL, true );
      } catch(const ErrorException & e) {
        // Log error and move on
        e.log(log4cpp::Priority::ERROR);
      }

    } // since attached VLBI bands are not normal, don't log if there are not
      // active VLBI bands.

  } catch ( ... ) {
    rethrowCaughtAsUser();
  }

void
SubarrayControlImpl::optimizeThresholds( const SeqShort & astroBandNoSeq )
try {

    const string cid("optimizeThresholds");
    ScopedLogNdc ndc(cid);

    const carma::util::CorrelatorType csType = csCorrType();

    if ( subarrayIsIntegrating() )
      throw CARMA_ERROR("You may not issue optimizeThresholds while integrating");

    //------------------------------------------------------------
    // Check if the signalPathMapper is reachable
    //------------------------------------------------------------

    if(signalPathMapper_.get() == 0) {
      programLogWarnIfPossible("Handle to SignalPathMapper is null! Unable to execute this command");
      return;
    }
    // Precondition: noise source has been turned ON.
    // CorrelatorHandle method will throw if this condition is not met.
    CORBA::Long seqNo = 0;
    {
      const PThreadMutexLock lock( gCorrNextSeqNoGuard_ );
      seqNo = gCorrNextSeqNo_;
      gCorrNextSeqNo_ += 10;
    }


    // Make a coordinated call to each CorrelatorBand in an astroband.
    // See comment in flattenPhases above.  Note such coordination
    // is not required for optimizeThresholds, but keep the code
    // the same for consistency.

    vector<short> astroBands = convertSequenceToVector<short>( astroBandNoSeq );
    unsigned short absize = astroBands.size();

    // Note  astrobandNoSeq could be [0], meaning all astrobands,
    // so check for that here and repopulate the astroband vector
    // with the actual astroband numbers in that case.
    // Otherwise assume the input list is valid.
    if ( absize == 1 && astroBands[0] == 0 ) {
        astroBands = signalPathMapper_->getActiveAstroBandNoVec( csType );
        absize = astroBands.size();
    }

    if ( absize == 0 ) return;

    // Aggregate all individual group members into one group
    // for dispatch via queue/WorkResult method.
    CorrelatorGroup aggregatedCorrHandles;
    VlbiGroup aggregatedVlbiHandles;
    ostringstream bos;
    ostringstream vos;
    for ( unsigned short i = 0 ; i < absize ; ++i) {

        // Zero not allowed as part of a sequence that
        // otherwise includes non-zero astroband numbers.
        if ( astroBands[i] == 0 )  continue;

        // Weed out astrobands not assigned to this correlator.
        // See note in flattenPhases
        if ( !signalPathMapper_->isValidAstroBand( astroBands[i], csType ) ) 
            continue;

        std::vector<carma::signalpath::SignalPathMapperControl::CorrelatorBand> bandVec =
          signalPathMapper_->getCorrelatorBands(astroBands[i]);

        const CorrelatorGroup correlatorGroup =
          getCorrelatorGroup(cid, bandVec);

        // Just needed for logging purposes
        
        CarmaBandNoSeq * correlatorBandNoSeq
          = signalPathMapper_->getCorrelatorBandNoSeq( astroBands[i] );
        
        const CarmaBandNoVec bvec =
          convertSequenceToVector< CarmaBandNoVec::value_type >( *correlatorBandNoSeq );


        if ( !correlatorGroup.empty() ) {

          CorrelatorGroup::iterator ci = correlatorGroup.begin();
          CorrelatorGroup::iterator ce = correlatorGroup.end();
          while ( ci != ce ) {
            //  only dispatch this command to spectral bands
            //  These cannot be weeded out by checking csCorrType
            //  because CORR_ANY is valid
            if ( ( *ci )->isSpectral() ) {
              aggregatedCorrHandles.insert( *ci );
            } else {
            }
            ci++;
          }

          const size_t sz = correlatorGroup.size();
          const bool plural = ( sz > 1 );
          bos << " AstroBand " << astroBands[i];
          bos << ( plural ? " Correlator Bands [" : "Correlator Band ");
          bos << formatAsRanges(bvec,"","") ;
          bos << ( plural ? "]," : ",");
        }

        const VlbiGroup vlbiGroup =
          getVlbiGroupForBandNoSeq(cid, *correlatorBandNoSeq, true);
        VlbiGroup::iterator vi = vlbiGroup.begin();
        VlbiGroup::iterator ve = vlbiGroup.end();
        if ( !vlbiGroup.empty() ) {
          while( vi != ve ) {
            //  VLBI handles implement this function, so
            //  add them to the aggregate
            aggregatedVlbiHandles.insert( *vi );
            vi++;
          }

          const size_t sz = vlbiGroup.size();
          const bool plural = ( sz > 1 );
          vos << " AstroBand " << astroBands[i];
          vos << ( plural ? " Vlbi Bands [" : " Vlbi Band ");
          vos << formatAsRanges(bvec,"","");
          vos << ( plural ? "]," : ",");
        }
    }/// end aggregation

    // Now dispatch the commands to the aggregated handles.
    if ( ! aggregatedCorrHandles.empty() ) {
        const string b = bos.str();
        // Log the command.
        cmdlog() << "SubarrayControlImpl::optimizeThresholds( seqNo = "
                 << seqNo  << " ) on " << b;

        ostringstream reqOs;
        reqOs << "CorrelatorHandle::optimizeThresholds( seqNo="
              << seqNo  << " )";
        const string requestIdCallString = reqOs.str();

        WorkResultSet wrs( requestIdCallString );

        queueFunctorWorkRequestGroup(
             requestIdCallString,
             makeHandleMethodFunctorGroup(
                        aggregatedCorrHandles,
                        &CorrelatorHandle::optimizeThresholds,
                        static_cast<int>( seqNo ) ),
             wrs,
             *workerPool_ );
          // Kevin says optimizeThresholds takes ~ 5s so,
          // give a 10 second timeout here.
             waitForAllNormal( wrs, 20000UL, true );
    } else {
        // Log that we did not dispatch a command.
          cmdlog() << "SubarrayControlImpl::optimizeThresholds( seqNo = "
                   << seqNo  << " ). No correlator bands are active.";
    }

    // Also send to vlbi backends, if necessary.
    // It was not possible to aggregate these into a single
    // WorkResultSet along with the CorrelatorHandles.
    // I tried using WorkResultSet::addKey to put them all
    // together, but that resulted in the calls just hanging
    // forever.  I guess I don't understand the WRS code.
    // However, optimizeThresholds is a no-op for VlbiHandles,
    // so this method returns quickly (thus one wonders if
    // it should be called at all).

    if(!aggregatedVlbiHandles.empty()) {
      const string b = vos.str();

      ostringstream vlbi_reqOs;
      vlbi_reqOs << "VlbiHandle::optimizeThresholds( seqNo="
                 << seqNo  << " )";
      const string vlbi_requestIdCallString = vlbi_reqOs.str();
      WorkResultSet wrs( vlbi_requestIdCallString );
      queueFunctorWorkRequestGroup(
           vlbi_requestIdCallString,
           makeHandleMethodFunctorGroup(
                      aggregatedVlbiHandles,
                      &VlbiHandle::optimizeThresholds,
                      static_cast<int>( seqNo ) ),
           wrs,
           *workerPool_ );

      try {
        waitForAllNormal( wrs, 10000UL, true );
      } catch(const ErrorException & e) {
        // Log error and move on
        e.log(log4cpp::Priority::ERROR);
      }

    } // since attached VLBI bands are not normal, don't log if there are not
      // active VLBI bands.

  } catch ( ... ) {
    rethrowCaughtAsUser();
  }

void
SubarrayControlImpl::calibrateSpectra(
                      const SeqShort & astroBandNoSeq,
                      const bool noiseEnabled,
                      const float intTime,
                      const bool cache,
                      const bool enable )
  try {

    const string cid("calibrateSpectra");
    ScopedLogNdc ndc(cid);

    const carma::util::CorrelatorType csType = csCorrType();

    if ( subarrayIsIntegrating() )
      throw CARMA_ERROR("You may not issue calibrateSpectra while integrating");

    //------------------------------------------------------------
    // Check if the signalPathMapper is reachable
    //------------------------------------------------------------

    if(signalPathMapper_.get() == 0) {
      programLogWarnIfPossible("Handle to SignalPathMapper is null! Unable to execute this command");
      return;
    }
    // Precondition: noise source is in desired state.
    // CorrelatorHandle method will throw if this condition is not met.
    CORBA::Long seqNo = 0;
    {
      const PThreadMutexLock lock( gCorrNextSeqNoGuard_ );
      seqNo = gCorrNextSeqNo_;
      gCorrNextSeqNo_ += 10;
    }


    // Make a coordinated call to each CorrelatorBand in an astroband.
    // See comment in flattenPhases above.  Note such coordination
    // is not required for calibrateSpectra , but keep the code
    // the same for consistency.

    vector<short> astroBands = convertSequenceToVector<short>( astroBandNoSeq );
    unsigned short absize = astroBands.size();

    // Note  astrobandNoSeq could be [0], meaning all astrobands,
    // so check for that here and repopulate the astroband vector
    // with the actual astroband numbers in that case.
    // Otherwise assume the input list is valid.
    if ( absize == 1 && astroBands[0] == 0 ) {
        astroBands = signalPathMapper_->getActiveAstroBandNoVec( csType );
        absize = astroBands.size();
    }

    if ( absize == 0 ) return;
    // Aggregate all individual group members into one group
    // for dispatch via queue/WorkResult method.
    CorrelatorGroup aggregatedCorrHandles;
    //VlbiGroup aggregatedVlbiHandles;
    ostringstream bos;
    //ostringstream vos;
    for ( unsigned short i = 0 ; i < absize ; ++i) {

        // Zero not allowed as part of a sequence that
        // otherwise includes non-zero astroband numbers.
        if ( astroBands[i] == 0 )  continue;

        // Weed out astrobands not assigned to this correlator.
        // Why is this needed, you ask? Because
        // signalpathMapper_->getCorrelatorBandNoSeq() and
        // its underlying signalpath:: calls are not correlator-type
        // aware.  So we eliminate the problem astrobands before that
        // call is made.  The issue is that getCorrelatorBandNoSeq()
        // could return a correlator band number which is valid in
        // this subarray but in reality refers to a different correlator,
        // and thus a flattenphases call can be dispatched to a
        // single correlator band instead of a pair, possibly
        // deadlocking that band.  For instance, in Sci1 with
        // the SL correlator in CARMA23 mode, "flattenPhases(9)" would
        // dispatch a call to slcor1 because the correlator band
        // number associated with astroband9 is 1.  Then bad things
        // happen because no call was also dispatched to corrband 2.
        if ( !signalPathMapper_->isValidAstroBand( astroBands[i], csType ) )
            continue;

        std::vector<carma::signalpath::SignalPathMapperControl::CorrelatorBand> bandVec =
          signalPathMapper_->getCorrelatorBands(astroBands[i]);

        const CorrelatorGroup correlatorGroup =
          getCorrelatorGroup(cid, bandVec);

        // Just needed for logging purposes
        
        CarmaBandNoSeq * correlatorBandNoSeq
          = signalPathMapper_->getCorrelatorBandNoSeq( astroBands[i] );
        
        const CarmaBandNoVec bvec =
          convertSequenceToVector< CarmaBandNoVec::value_type >( *correlatorBandNoSeq );

        if ( !correlatorGroup.empty() ) {

          CorrelatorGroup::iterator ci = correlatorGroup.begin();
          CorrelatorGroup::iterator ce = correlatorGroup.end();
          while ( ci != ce ) {
            //  only dispatch this command to spectral bands
            //  These cannot be weeded out by checking csCorrType
            //  because CORR_ANY is valid
            if ( ( *ci )->isSpectral() ) {
              aggregatedCorrHandles.insert( *ci );
            }
            ci++;
          }

          const size_t sz = correlatorGroup.size();
          const bool plural = ( sz > 1 );
          bos << " AstroBand " << astroBands[i];
          bos << ( plural ? " Correlator Bands [" : "Correlator Band ");
          bos << formatAsRanges(bvec,"","") ;
          bos << ( plural ? "]," : ",");
        }

        /* no VLBI method for calibrateSpectra
        const VlbiGroup vlbiGroup =
          getVlbiGroupForBandNoSeq(cid, *correlatorBandNoSeq, true);
        VlbiGroup::iterator vi = vlbiGroup.begin();
        VlbiGroup::iterator ve = vlbiGroup.end();
        if ( !vlbiGroup.empty() ) {
          while( vi != ve ) {
            aggregatedVlbiHandles.insert( *vi );
            vi++;
          }

          const size_t sz = vlbiGroup.size();
          const bool plural = ( sz > 1 );
          vos << " AstroBand " << astroBands[i];
          vos << ( plural ? " Vlbi Bands [" : " Vlbi Band ");
          vos << formatAsRanges(bvec,"","");
          vos << ( plural ? "]," : ",");
        }
        */
    }/// end aggregation

    // Now dispatch the commands to the aggregated handles.
    if ( ! aggregatedCorrHandles.empty() ) {
        const string b = bos.str();
        // Log the command.
        cmdlog() << "SubarrayControlImpl::calibrateSpectra( seqNo = "
                 << seqNo  << " ) on " << b;

        ostringstream reqOs;
        reqOs << "CorrelatorHandle::calibrateSpectra( seqNo="
              << "enable = " << boolalpha << enable
              << ", noiseEnabled = " << boolalpha << noiseEnabled
              << ", cache = " << boolalpha << cache
              << ", intTime = " << intTime
              << ", seqNo = " << seqNo
              <<" ) ";
        const string requestIdCallString = reqOs.str();

        WorkResultSet wrs( requestIdCallString );

        queueFunctorWorkRequestGroup(
             requestIdCallString,
             makeHandleMethodFunctorGroup(
                        aggregatedCorrHandles,
                        &CorrelatorHandle::calibrateSpectra,
                        noiseEnabled,
                        intTime,
                        cache,
                        enable,
                        static_cast<int>( seqNo ) ),
             wrs,
             *workerPool_ );

         waitForAllNormal( wrs, 10000UL, true );
    } else {
        // Log that we did not dispatch a command.
          cmdlog() << "SubarrayControlImpl::calibrateSpectra( seqNo = "
                   << seqNo  << " ). No correlator bands are active.";
    }

    // no VLBIHandle::calibrateSpectra method

  } catch ( ... ) {
    rethrowCaughtAsUser();
  }

util::SeqShort * 
SubarrayControlImpl::getCorrelatorBandNoSeq( const SeqShort & astrobandNoSeq,
                                             const bool includeOfflineBands )
try {

    ScopedLogNdc ndc("SacI::getCorrelatorBandNoSeq");
    // Need to create a default return value because 
    // just declaring an _var does no allocation.
    // So you will get a CORBA marshalling error if
    // you try to _retn() on the unallocated _var.
    // This default value will get overwritten if the
    // input astroBandNoSeq is has active astrobands.
    SeqShort * defaultReturnValue = new SeqShort(1);
    (*defaultReturnValue).length(1);
    (*defaultReturnValue)[0] = -1;
    SeqShort_var finalReturnSeq( defaultReturnValue ); 
    //------------------------------------------------------------
    // Check if the signalPathMapper is reachable
    //------------------------------------------------------------

    if(signalPathMapper_.get() == 0) {
      programLogWarnIfPossible("Handle to SignalPathMapper is null! Unable to execute this method");
      return finalReturnSeq._retn();
    }

    const carma::util::CorrelatorType csType = csCorrType();

    vector<short> astroBands = convertSequenceToVector<short>( astrobandNoSeq );
    unsigned short absize = astroBands.size();

    // Note  astrobandNoSeq could be [0], meaning all astrobands,
    // so check for that here and repopulate the astroband vector
    // with the actual astroband numbers in that case. 
    // Otherwise assume the input list is valid.
    if ( absize == 1 && astroBands[0] == 0 ) {
        astroBands = signalPathMapper_->getActiveAstroBandNoVec( csType );
        absize = astroBands.size();
    }

    if ( absize == 0 ) {
        return finalReturnSeq._retn() ;
    }

    const string cid("getCorrelatorBandNoSeq");
    vector<short> finalReturnVec;
    for ( unsigned short i = 0 ; i < absize ; ++i) {

        // Zero not allowed as part of a sequence that
        // otherwise includes non-zero astroband numbers.
        if ( astroBands[i] == 0 )  continue;  

        // Weed out astrobands not assigned to this correlator.
        // See note in flattenPhases
        if ( !signalPathMapper_->isValidAstroBand( astroBands[i], csType ) ) 
            continue;
 
        vector<SignalPathMapperControl::CorrelatorBand> cbands
            = signalPathMapper_->getCorrelatorBands( astroBands[i] );

        const CorrelatorGroup correlatorGroup =
          getCorrelatorGroup(cid, cbands);

        if ( !correlatorGroup.empty() ) {
          
          CorrelatorGroup::iterator ci = correlatorGroup.begin();
          CorrelatorGroup::iterator ce = correlatorGroup.end();
          while ( ci != ce ) {
            // skip offline bands if requested
            if ( !includeOfflineBands && (*ci)->isOffline() ) {
              ci++;
              continue;
            }
            finalReturnVec.push_back( (*ci)->correlatorBandNo() );
            ci++;
          }
        } 
    }

    finalReturnSeq = convertVectorToSequence< SeqShort >( finalReturnVec );

    return finalReturnSeq._retn();

  } catch ( ... ) {
    rethrowCaughtAsUser();
    // Just to shut up a compiler warning about "no return value"
    throw CARMA_EXCEPTION( UserException, "SaCI::getCorrelatorBandNoSeq() failure" );
  }

// ------------------------ Frequency helpers -----------------------------
// Update all monsys frequencies; call when any input freq changes
// @TODO modify to handle CORR_ANY for ownership of two correlators
void
SubarrayControlImpl::updateFrequencyMPs()
{
  try {
      ScopedLogNdc ndc("SubarrayControlImpl::updateFrequencyMps(void)");

      const util::CorrelatorSet corrset(getCorrelatorDesignation());

      if ( corrset.includesSpectral() ) {
        updateFrequencyMPs( CORR_SPECTRAL );
      }

      if ( corrset.includesWideband() ) {
        updateFrequencyMPs( CORR_WIDEBAND );
      }

      if ( corrset.includesC3gMax8() ) {
        updateFrequencyMPs( CORR_C3GMAX8 );
      }

      if ( corrset.includesC3gMax23() ) {
        updateFrequencyMPs( CORR_C3GMAX23 );
      }

  } catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
  }
}

void SubarrayControlImpl::updateFrequencyMPs( const carma::util::CorrelatorType cType )
{
  ScopedLogNdc ndc("SubarrayControlImpl::updateFrequencyMps(CorrType)");
  if  ( cType == util::CORR_NONE ) return;

  // Fixed Block downconverter LO freq, GHz. In Hawkin's correlator document,
  // this is called fLO2.
  const double BLOCK_DC_LO     = 10.0;

  CARMA_CPTRACE(Trace::TRACE4, "Entering updateFrequencyMPs()" );

  subarrayContainer_.skyFreq().setValue(skyFreq_);

  // Loop over all bands...
  const int numBands = getMaxNumBands( cType );

  for (int b=0; b < numBands; b++) {
    ControlBandPoints* cbp = 0;
    // Get pointer to the correct correlator band
    switch( cType ) {
    case util::CORR_SPECTRAL:
      cbp = &controlSubsystem_.spectralLineCorrelator().
                slcBand(b).controlBandPoints();
      break;
    case util::CORR_WIDEBAND:
      cbp = &controlSubsystem_.widebandCorrelator().
                wbcBand(b).controlBandPoints();
      break;
    case util::CORR_C3GMAX8:
      cbp = &controlSubsystem_.c3gMax8Correlator().
            c3gMax8Band(b).controlBandPoints();
      break;
    case util::CORR_C3GMAX23:
      cbp = &controlSubsystem_.c3gMax23Correlator().
            c3gMax23Band(b).controlBandPoints();
      break;
    case util::CORR_NONE: // shouldn't happen
    default:
      break;
    }
    if (cbp != 0) {
      double bwghz = 0.001 * cbp->bandwidth().getValue();
      // Now load the frequencies
      typedef ControlBandPoints::Lo2SidebandMonitorPointEnum LO2SB;
      // All downconversions conform to:
      // Frf = lo + sb*Fif, where sb=+1 for usb, -1 for lsb
      double lo1 = subarrayContainer_.loFreq().getValue();
      double lo2 = cbp->lo2Freq().getValue();
      int lo1sb ;
      int lo2sb ;
      if (cbp->lo2Sideband().getValue() == LO2SB::UPPER) {
        lo2sb =  1;
      }
      else {
        lo2sb = -1;
      }

      // The 1GHz digitizer acts like a lower sideband downconverter
      // with lo freq of 1.0GHz, aliasing the 0.5-1.0GHz band down
      // to 0.0-0.5GHz.
      double lo3   =  1.0; // 1 GHz
      int    lo3sb = -1;   // Digitizer is always an lsb conversion

      // The FIR filter moves the edge of the filtered band,
      // which is centered on 0.25GHz, down to baseband,
      // without any channel reversal.
      // This makes an effective 4th LO of 0.25GHz-bw/2, usb conversion
      double lo4   = 0.25 - bwghz/2;  // GHz
      int    lo4sb = 1;  // FIR filter is always a usb conversion

      // Loop over sidebands
      for (int s = 0; s < 2; s++) {
        // Sized for 2 baseband frequencies (lowest and highest)
        double fir[2];      // FIR filter input band frequency
        double fsb[2];      // Sampler band frequency
        double fif[2];      // IF band frequency
        double fsky[2];     // Sky frequency

        lo1sb = 1 - (2 * s);  // Gives +1 first time through, then -1

        // Loop over both ends of baseband
        for (int bb = 0; bb < 2; bb++) {
          double fbb = bb*bwghz;          // Baseband freq for calcs
          fir[bb]  = lo4 + lo4sb*fbb;     // FIR band
          fsb[bb]  = lo3 + lo3sb*fir[bb]; // Sampler band
          fif[bb]  = lo2 + lo2sb*fsb[bb]; // IF band

          // If the block downconverter was enabled,
          // modify the IF frequency accordingly
          if ( getBlockDownconverterEnabled( cType, b+1 ) )
            fif[bb] = BLOCK_DC_LO - fif[bb];

          fsky[bb] = lo1 + lo1sb*fif[bb];
        }
        // Choose top or bottom of baseband based on IF frequencies.
        // The correlator data are always in order of increasing
        // IF frequency, so pick the edge of the baseband
        // that is consistent with this
        int bb = 0;
        if (fif[0] > fif[1]) bb = 1;
        cbp->sideband(s).skyFreq().setValue(fsky[bb]);
        // These MPs are for the center of the band
        double fskyCenter  = (fsky[0]+fsky[1])/2;
        cbp->sideband(s).bandCenter().skyFreq().setValue(fskyCenter);
      }  // Sideband loop
    } // cpb if statement
  } // Band loop

  //CARMA_CPTRACE(Trace::TRACE4, "Exiting updateFrequencyMPs()" );
}


// Update all frequencies in system except antennas;
//   call when any input freq changes
void
SubarrayControlImpl::updateFrequency(double loFreqGHz)
{
  CARMA_CPTRACE(Trace::TRACE4, "Entering updateFrequency("
        << loFreqGHz << ")");
  loFreq_ = loFreqGHz;
  Freq freq(loFreqGHz, Freq::GHz);
  LOchain_.updateFreq(freq);
  Harmonic harm;
  try {
    harm = LOchain_.getPreferredHarmonic();
  }
  catch (std::exception& e) {
    ostringstream o;
    o << "trouble in updateFrequency(): " << e;
    throw CARMA_ERROR(o);
  }

  // Update monitor system command values
  subarrayContainer_.loFreq().setValue(loFreqGHz);
  subarrayContainer_.refLoFreq().setValue(harm.getRefFreq().gigahertz());
  subarrayContainer_.oscFreq().setValue(LOchain_.getOscillatorFreq().gigahertz());
  subarrayContainer_.yigFreq().setValue(harm.getYigFreq().gigahertz());
  subarrayContainer_.harmonic().setValue(harm.getYigHarmonicNumber());
  updateFrequencyMPs();

  // Set the lo ref frequency, and set power to the maxmimum of 16 dBm
  if ( loRef_.get() != 0 ) {
    const double loRefHertz = harm.getRefFreq().hertz();

    loRef_->setFrequencyPower( loRefHertz, 16 );
  }

  // Inform the linelength system that the LO Reference frequency has changed
  if (lineLength_.get()) {
    const double loRefHertz = harm.getRefFreq().hertz();

    // synthesizer is hardwired to the subarray
    lineLength_->setLORefFreq(subarrayNo_, loRefHertz);
  }

  // update the freq used in the delay engine.
  const Frequency lo1freq( loFreqGHz, "GHz" );
  if(delayEngine_.get()) {
    const PThreadMutexLock dlock( gDelayGuard );
    delayEngine_->setAllAntennaLOFreqs( lo1freq );
  }
}

double
SubarrayControlImpl::apEff(unsigned short carmaAntNo, double loFreqGHz) const
{
  // default 3mm/1mm aperture efficiencies
  double ovroEff     = 0.50;
  double bimaEff     = 0.65;
  const double szaEff = 0.60; // constant with frequency. 5/2011

  // @todo get better numbers for 1mm all ants.
  if ( loFreqGHz <= 50.0 ) {
    ovroEff     = 0.70;
    bimaEff     = 0.75;
  }

  if (carmaAntNo <= 6) {
      return ovroEff;
  } else if (carmaAntNo <= 15) {
      return bimaEff;
  } else  {
      return szaEff;
  }
}

double
SubarrayControlImpl::diameter(unsigned short carmaAntNo) const
{
  if (carmaAntNo <= 6) {
      return 10.4; // meters
  } else if (carmaAntNo <= 15) {
      return 6.1; // meters
  } else  {
      return 3.5; // meters
  }
}

double
SubarrayControlImpl::computeJyPerK(unsigned short carmaAntNo, double loFreqGHz) const
{
  const double eff = apEff(carmaAntNo, loFreqGHz );
  // Diameters, jyPerK
  const double refJyperk   = 32.5; // A perfect 10.4m dish is 32.5 Jy/K
  const double refDiameter = 10.4;
  const double refArea     = refDiameter * refDiameter;
  double area    = diameter(carmaAntNo) * diameter(carmaAntNo);
  double jyperk  = refJyperk * (refArea/area) /eff;
  return jyperk;
}

// Update Jy per Kelvin for all antenna in subarray
void
SubarrayControlImpl::updateJyperk(double loFreqGHz)
{
  // Get the vector of antennas in this subarray
  CarmaAntNoVec antvec = getCarmaAntNoVecForAllAntennas( ) ;
  const unsigned asize = antvec.size();
  for (unsigned int i=0; i < asize; i++) {
    const short a = antvec[i];
    ControlSubsystemBase::Antenna& ant = controlSubsystem_.antenna(a-1);
    const int antNo = ant.carmaAntennaNumber().getValue();
    ant.jyperk().setValue( computeJyPerK(antNo,loFreqGHz) );
  }
}

// Update sideband ratios for all ants in subarray based on loFreq.
void
SubarrayControlImpl::updateSbrs(double loFreqGHz)
{
  // Default SBRs - all are SSB (LSB) @ 1cm, SZA (SSB) in USB @ 3mm
  const double ovroBimaSbr = ( loFreqGHz <= 50.0 ? 0.0 : 1.0 );
  const double szaSbr      = ( loFreqGHz <= 50.0 ? 0.0 : 1000.0 );

  SeqShort ovroBimaAntSeq;
  SeqShort szaAntSeq;

  const CarmaAntNoVec antvec = getCarmaAntNoVecForAllAntennas( ) ;
  BOOST_FOREACH( const short & antNo, antvec ) {

    const AntennaType antType = computeAntennaType( antNo );

    switch ( antType ) {
      case ANTENNA_TYPE_BIMA:
      case ANTENNA_TYPE_OVRO:
        {
          const CORBA::ULong idx = ovroBimaAntSeq.length( );
          ovroBimaAntSeq.length( idx + 1 );
          ovroBimaAntSeq[idx] = antNo;
        }
        break;
      case ANTENNA_TYPE_SZA:
        {
          const CORBA::ULong idx = szaAntSeq.length( );
          szaAntSeq.length( idx + 1 );
          szaAntSeq[idx] = antNo;
        }
        break;
      default:
        break;
    }
  }

  sbratio( ovroBimaSbr, ovroBimaAntSeq );
  sbratio( szaSbr, szaAntSeq );
}

void
SubarrayControlImpl::refAtten( const unsigned short atten,
                               const CORBA::Short   carmaAntNo )
  try
  {
    cmdlog() << "refAtten(atten=" << atten << ", "
             << carmaAntNo << ")";

    const string cmd = "RxSelect::setRefAtten";
    AntControlsGroup acg =
      getAntControlsGroupForCarmaAntNo(cmd, carmaAntNo);
    const RxSelectorGroup rsg =
      getRxSelectorGroupForAntControlsGroup(cmd, acg);
    WorkResultSet wrs( cmd + " command" );
    ostringstream oss;
    oss << cmd << "(atten=" << atten << " )";
    string requestIdCallString = oss.str();

    queueFunctorWorkRequestGroup(
                 requestIdCallString,
                 makeHandleMethodFunctorGroup(
                                  rsg, &RxSelectorHandle::setRefAtten, atten),
                 wrs,
                 *workerPool_ );

    waitForAllNormal(wrs );
    ControlSubsystemBase::Antenna& ant =
      controlSubsystem_.antenna( carmaAntNo - 1 );
    ant.refAtten().setValue( atten );
    markStateChange();
  } catch ( ... ) {
    rethrowCaughtAsUser();
  }

// Helper to get the current rx type
RX::Type SubarrayControlImpl::getCurrentRxType()
{
  if (loFreq_ < 50) {
    return RX::RX1CM;
  }
  else if (loFreq_ > 200) {
    return  RX::RX1MM;
  }
  else {
    return RX::RX3MM;
  }
}

// ------------------------ AntennaIFpower -------------------------
void
SubarrayControlImpl::antennaIFpower( const double power,
                                     const SeqShort& carmaAntNoSeq )
  try
  {
    resetTsys( carmaAntNoSeq );

    cmdlog() << "antennaIFpower(power=" << setprecision(3) << power
             << ", carmaAntNo="
             << getStringForCarmaAntNoSeq(carmaAntNoSeq) << ")";


    RX::Type  rxType = getCurrentRxType();

    ostringstream oss;
    oss << fixed << "RxSelect::antennaIFpower(power="
        << setprecision(3) << power << " )";
    string requestIdCallString = oss.str();

    const RxSelectorGroup rxSelectorGroup =
      getRxSelectorGroup("IFpow", carmaAntNoSeq);
    WorkResultSet setPowerWrs( "RxSelect::antennaIFpower cmd" );

    // Send IF power level to rx's
    queueFunctorWorkRequestGroup(
                 requestIdCallString,
                 makeHandleMethodFunctorGroup(
                                  rxSelectorGroup,
                                  &RxSelectorHandle::antennaIFpower,
                                  rxType,
                                  power ),
                 setPowerWrs,
                 *workerPool_);
    waitForAllNormal(setPowerWrs);

  } catch ( ... ) {
    rethrowCaughtAsUser();
  }

// ------------------------ AntennaIFatten-------------------------
void
SubarrayControlImpl::antennaIFatten( const double atten,
                                     const short ifNum,
                                     const SeqShort& carmaAntNoSeq,
                                     const bool invalidateTsys )
  try
  {
    if ( invalidateTsys ) resetTsys( carmaAntNoSeq );

    cmdlog() << "antennaIFatten(atten=" << setprecision(1) << atten
             << ", ifNum=" << ifNum
             << ", carmaAntNo="
             << getStringForCarmaAntNoSeq(carmaAntNoSeq) << ")";

    antenna::common::RxControl::IF_Type iftype;
    switch(ifNum) {
    case 0:
      iftype = RxControl::BOTH;
      break;
    case 1:
      iftype = RxControl::IF1;
      break;
    case 2:
      iftype = RxControl::IF2;
      break;
    default:
      iftype = RxControl::IF1;
      break;
    }

    RX::Type  rxType = getCurrentRxType();

    ostringstream oss;
    oss << fixed << "RxSelect::antennaIFatten(atten="
        << setprecision(1) << atten << " )";
    string requestIdCallString = oss.str();

    const RxSelectorGroup rxSelectorGroup =
      getRxSelectorGroup("IFatten", carmaAntNoSeq);
    WorkResultSet setPowerWrs( "RxSelect::antennaIFatten cmd" );

    // Send IF atten to rx's
    queueFunctorWorkRequestGroup(
                 requestIdCallString,
                 makeHandleMethodFunctorGroup(
                                  rxSelectorGroup,
                                  &RxSelectorHandle::antennaIFatten,
                                  rxType,
                                  iftype,
                                  atten),
                 setPowerWrs,
                 *workerPool_);
    waitForAllNormal(setPowerWrs);

  } catch ( ... ) {
    rethrowCaughtAsUser();
  }

// ------------------------ AntennaIFpresetPower -------------------------
void
SubarrayControlImpl::antennaIFpresetPower(const SeqShort& carmaAntNoSeq)
  try
  {
    resetTsys( carmaAntNoSeq );

    cmdlog() << "antennaIFpresetPower(carmaAntNo="
             << getStringForCarmaAntNoSeq(carmaAntNoSeq) << ")";

    RX::Type  rxType = getCurrentRxType();

    ostringstream oss;
    oss << fixed << "RxSelect::antennaIFpresetPower()";
    string requestIdCallString = oss.str();

    const RxSelectorGroup rxSelectorGroup =
      getRxSelectorGroup("IFpow", carmaAntNoSeq);
    WorkResultSet setPowerWrs( "RxSelect::antennaIFpresetPower cmd" );

    // Send IF preset power command to Rx's
    queueFunctorWorkRequestGroup(
                 requestIdCallString,
                 makeHandleMethodFunctorGroup(
                                  rxSelectorGroup,
                                  &RxSelectorHandle::antennaIFpresetPower,
                                  rxType
                                  ),
                 setPowerWrs,
                 *workerPool_);
    waitForAllNormal(setPowerWrs);

  } catch ( ... ) {
    rethrowCaughtAsUser();
  }

// ------------------------ storeAntennaIFattenAmbient -------------------------
void
SubarrayControlImpl::storeAntennaIFattenAmbient(const SeqShort& carmaAntNoSeq)
  try
  {
    cmdlog() << "storeAntennaIFattenAmbient(carmaAntNo="
             << getStringForCarmaAntNoSeq(carmaAntNoSeq) << ")";
             
    carmaMonitor_.readNewestConditionalCopy();
    
    static MonitorPointFloat* fromMP[2][23];
    static MonitorPointFloat* toMP[2][23];
    static bool initMPs = true;
    if (initMPs) {
        ostringstream o;
        for (int i=0; i<2; i++) {
            int cIdx = 0;
            for (int a=0; a<6; a++) {
              fromMP[i][cIdx++] = &carmaMonitor_.ovro(a).antennaIfContainer(i).
                                       antennaIF().attenSet();
            }
            for (int a=0; a<9; a++) {
              fromMP[i][cIdx++] = &carmaMonitor_.bima(a).antennaIfContainer(i).
                                       antennaIF().attenSet();
            }
            for (int a=0; a<8; a++) {
              fromMP[i][cIdx++] = &carmaMonitor_.sza(a).ifmod().totalAtten();
            }
            for (int a=0; a<23; a++) {
              toMP[i][a] = &controlSubsystem_.antenna(a).ifAttenAmb(i);
            }
        }
        initMPs = false;
    }
    
    // Currently this construct is only used at 1cm and all centimeter rx
    // only have one IF. Using two causes many erroneous error msgs in logs.
    const int maxIFs = 1;
    
    CarmaAntNoVec v = getCarmaAntNoVec(carmaAntNoSeq);
    // Check for magic '0' value and interpret as all ants in subarray
    if ((v.size() == 1) && (v[0] == 0)) v = getCarmaAntNoVecForAllAntennas();
    // Loop on all requested ants, setting atten values
    for (unsigned int c=0; c<v.size(); c++) {
        for (int i=0; i<maxIFs; i++) {
            short a = v[c] - 1;  // Antenna index
            float f = fromMP[i][a]->getValue();
            cmdlog() << "store AntennaIFattenAmb => AntIdx:" << a 
                     << ",  IFidx:"<< i<< ",  Value: " << f;
            if (f < 8.0) {
                ostringstream e;
                e << "Low attenuation value (" 
                  << fixed << setprecision(1) << f 
                  << ") on C" << (a+1)
                  << ", IF" << i+1;
                  if (a > 14) {
                      // The SZA IF switch is sometimes incorrect and the root
                      // cause of these problems, so its position is logged.
                      // A value of 1 is correct, 2 is incorrect.
                      MonitorPointShort& mp = 
                            carmaMonitor_.sza(a-15).ifmod().ifSwitchState();
                      short pos = mp.getValue();
                      e << ", IFswitch=" << pos;
                      if (pos != 1) e << "(wrong!!)";            
                  }
                  e << " in storeAntennaIFattenAmbient();"
                    << " setting to 8.0.";
                programLogErrorIfPossible(e.str());
                f = 8.0;
            }        
            toMP[i][a]->setValue(f);
        }
    }

  } catch ( ... ) {
    rethrowCaughtAsUser();
  }
// ------------------------ AntennaRxTuning-------------------------
// Backdoor commands needed by the ovro antennas
void
SubarrayControlImpl::vj(
            const carma::antenna::common::RxControl::Pol_Type pol,
            const float vj, const short carmaAntNo)
  try
  {
    cmdlog() << "vj(vj=" << vj << ", carmaAntNo=" << carmaAntNo << ")";

    RX::Type  rxType = getCurrentRxType();

    ostringstream oss;
    oss << setiosflags(ios::fixed) << "RxSelect::vj(vj="
        << setprecision(3) << vj << " )";
    string requestIdCallString = oss.str();

    CarmaAntNoSeq ant;
    ant.length(1);
    ant[0] = carmaAntNo;
    const RxSelectorGroup rxSelectorGroup = getRxSelectorGroup("vj", ant);
    WorkResultSet tuneWrs( "RxSelect::vj cmd" );

    // Send vj to Rx
    queueFunctorWorkRequestGroup(
                 requestIdCallString,
                 makeHandleMethodFunctorGroup(
                                  rxSelectorGroup,
                                  &RxSelectorHandle::vj,
                                  rxType,
                                  pol,
                                  vj),
                 tuneWrs,
                 *workerPool_ );
    waitForAllNormal(tuneWrs);

  } catch ( ... ) {
    rethrowCaughtAsUser();
  }

void
SubarrayControlImpl::ij(
            const carma::antenna::common::RxControl::Pol_Type pol,
            const float ij, const short carmaAntNo)
  try
  {
    cmdlog() << "ij(ij=" << ij << ", carmaAntNo=" << carmaAntNo << ")";

    RX::Type  rxType = getCurrentRxType();

    ostringstream oss;
    oss << setiosflags(ios::fixed) << "RxSelect::ij(ij="
        << setprecision(3) << ij << " )";
    string requestIdCallString = oss.str();

    CarmaAntNoSeq ant;
    ant.length(1);
    ant[0] = carmaAntNo;
    const RxSelectorGroup rxSelectorGroup = getRxSelectorGroup("ij", ant);
    WorkResultSet tuneWrs( "RxSelect::ij cmd" );

    // Send vj to Rx
    queueFunctorWorkRequestGroup(
                 requestIdCallString,
                 makeHandleMethodFunctorGroup(
                                  rxSelectorGroup,
                                  &RxSelectorHandle::ij,
                                  rxType,
                                  pol,
                                  ij),
                 tuneWrs,
                 *workerPool_ );
    waitForAllNormal(tuneWrs);

  } catch ( ... ) {
    rethrowCaughtAsUser();
  }

// -------------------------------------------------------------
bool
SubarrayControlImpl::subarrayIsIntegrating( )
{

  MonitorPointBool& busySL = carmaMonitor_.slPipeline().
          integratorStageContainer().integratorStage().integrating();  
  MonitorPointBool& busyWB = carmaMonitor_.wbPipeline().
          integratorStageContainer().integratorStage().integrating();  
  MonitorPointBool& busyC3gMax8 = carmaMonitor_.c3gMax8Pipeline().
          integratorStageContainer().integratorStage().integrating();  
  MonitorPointBool& busyC3gMax23 = carmaMonitor_.c3gMax23Pipeline().
          integratorStageContainer().integratorStage().integrating();  

  carmaMonitor_.readNewestConditionalCopy();

  MonitorCorrelatorDesignation cor = msCorrType();
  CorrelatorSet corrset( corrDesToCorrType(cor) );

  bool busy = false;
  if ( corrset.includesSpectral() )
      busy = ( busy || busySL.getValue() );
  if ( corrset.includesWideband() )
      busy = ( busy || busyWB.getValue() );
  if ( corrset.includesC3gMax8() )
      busy = ( busy || busyC3gMax8.getValue() );
  if ( corrset.includesC3gMax23() )
      busy = ( busy || busyC3gMax23.getValue() );
  return busy;

}

bool  
SubarrayControlImpl::anyScienceSubarrayIsIntegrating( )
{
  MonitorPointBool& busySL = carmaMonitor_.slPipeline().
    integratorStageContainer().integratorStage().integrating();

  MonitorPointBool& busyWB = carmaMonitor_.wbPipeline().
    integratorStageContainer().integratorStage().integrating();

  MonitorPointBool& busyC3gMax8 = carmaMonitor_.c3gMax8Pipeline().
          integratorStageContainer().integratorStage().integrating();  

  MonitorPointBool& busyC3gMax23 = carmaMonitor_.c3gMax23Pipeline().
          integratorStageContainer().integratorStage().integrating();  

  carmaMonitor_.readNewestConditionalCopy();

  MonitorCorrelatorDesignation cor = msCorrType();
  CorrelatorSet corrset( corrDesToCorrType(cor) );

  if ( corrset.isEmpty() ) return false;  //WHY?????  This is only this subarray not ANY subarray as the method name indicated.  MWP 2014-Mar-12

  return busySL.getValue() || busyWB.getValue() || busyC3gMax8.getValue() || busyC3gMax23.getValue();
}

/**.......................................................................
 * Helper function to return the correlator designation of this subarray
 */
MonitorCorrelatorDesignation
SubarrayControlImpl::msCorrType()
{
  signalPath_.readNewestConditionalCopy();
  return signalPath_.mapping().subarray(subarrayNo_ - 1).CORRELATOR_DESIGNATION_MP().getValue();
}


/**.......................................................................
 * Helper function to return the correlator designation of this subarray
 */
carma::util::CorrelatorType 
SubarrayControlImpl::csCorrType()
{
  signalPath_.readNewestConditionalCopy();
  return static_cast<carma::util::CorrelatorType> (signalPath_.mapping().subarray(subarrayNo_ - 1).CORRELATOR_DESIGNATION_MP().getValue());
}

/**.......................................................................
 * Helper function to return the monitor stream equivalent of a
 * util::CorrelatorFpgaModeType
 */
ControlBandPoints::FpgaModeMonitorPointEnum::FPGAMODE
SubarrayControlImpl::msFpgaMode(util::CorrelatorFpgaModeType fpgaMode)
{
  switch (fpgaMode) {
  case CORR_SINGLEPOL:
    return ControlBandPoints::FpgaModeMonitorPointEnum::SINGLEPOL;
    break;
  case CORR_DUALPOL:
    return ControlBandPoints::FpgaModeMonitorPointEnum::DUALPOL;
    break;
  case CORR_FULLPOL:
    return ControlBandPoints::FpgaModeMonitorPointEnum::FULLPOL;
    break;
  case CORR_CARMA23:
    return ControlBandPoints::FpgaModeMonitorPointEnum::CARMA23;
    break;
  default:
    return ControlBandPoints::FpgaModeMonitorPointEnum::SINGLEPOL;
    break;
  }
}

const string
SubarrayControlImpl::getStringForAstroBandMode(util::CorrelatorFpgaModeType fpgaMode)
{
    switch ( fpgaMode ) {
      case CORR_SINGLEPOL:
        return "SINGLEPOL";
      case CORR_DUALPOL:
        return "DUALPOL";
      case CORR_FULLPOL:
        return "FULLPOL";
      case CORR_CARMA23:
        return  "CARMA23";
      default:
        return "UNKNOWN";
    }
}

/**.......................................................................
 * Return the appropriate obsBlock() monitor point for this correlator
 * designation
 */
void SubarrayControlImpl::updateCorrelatorMonitorPoints(const carma::util::CorrelatorType ctype)
{
    ScopedLogNdc ndc("SubarrayControlImpl::updateCorrelatorMonitorPoints");

    std::ostringstream oss;
    if (subarrayNo_ == 1 || subarrayNo_ == 2) {
        oss << "Sci#" << subarrayNo_;
    } else if(subarrayNo_ == 3 || subarrayNo_ == 4) {
        oss << "Eng#" << subarrayNo_ - 2;
    } else if(subarrayNo_ == 5) {
        oss << "Maint";
    } else {
        oss << "NONE";
    }

    const CorrelatorSet corrSet(ctype);

    if (corrSet.isEmpty()) {
        ThrowCarmaError("updateCorrelatorMonitorPoints called with CORR_NONE");
    }

    if (!corrSet.isSingleCorrelator()) {
        ThrowCarmaError("updateCorrelatorMonitorPoints called with multiple correlator designation");
    }

    if (corrSet.isSpectral()) {
        ControlSubsystemBase::SpectralLineCorrelator &slc = controlSubsystem_.spectralLineCorrelator();
        slc.controllingSubarray().setValue(oss.str());
        initializeManualFlagMonitorPoints(slc.manualFlag(), slc.getName());
        return;
    }

    if (corrSet.isWideband()) {
        ControlSubsystemBase::WidebandCorrelator &wbc = controlSubsystem_.widebandCorrelator();
        wbc.controllingSubarray().setValue(oss.str());
        initializeManualFlagMonitorPoints(wbc.manualFlag(), wbc.getName());
        return;
    }

    if (corrSet.isC3gMax8()) {
        ControlSubsystemBase::C3gMax8Correlator &max8 = controlSubsystem_.c3gMax8Correlator();
        max8.controllingSubarray().setValue(oss.str());
        initializeManualFlagMonitorPoints(max8.manualFlag(), max8.getName());
        return;
    }

    if (corrSet.isC3gMax23()) {
        ControlSubsystemBase::C3gMax23Correlator &max23 = controlSubsystem_.c3gMax23Correlator();
        max23.controllingSubarray().setValue(oss.str());
        initializeManualFlagMonitorPoints(max23.manualFlag(), max23.getName());
        return;
    }

    // We'll never get here -- just to shut up compiler warnings
    ThrowCarmaError("updateCorrelatorMonitorPoints called with invalid correlator designation");
}

void
SubarrayControlImpl::enableCorrelation( 
         const SeqShort & astroBandNoSeq,
         const bool correlationsEnabled )
try {
    const string cid("SubarrayControImpl::enableCorrelation");
    ScopedLogNdc ndc(cid);

    const carma::util::CorrelatorType csType = csCorrType();
    if (   ( csType == carma::util::CORR_SPECTRAL ) 
        || ( csType == carma::util::CORR_NONE )  ) {
      throw CARMA_ERROR("This subarray does not own the Wideband correlator");
    }

    if ( subarrayIsIntegrating() )
      throw CARMA_ERROR("You may not issue enableCorrelation while integrating");

    //------------------------------------------------------------
    // Check if the signalPathMapper is reachable
    //------------------------------------------------------------

    if(signalPathMapper_.get() == 0) {
        throw CARMA_ERROR("Handle to SignalPathMapper is null! Unable to execute this command");
    }

    vector<short> astroBands = convertSequenceToVector<short>( astroBandNoSeq );
    unsigned short absize = astroBands.size();

    // Note  astrobandNoSeq could be [0], meaning all astrobands,
    // so check for that here and repopulate the astroband vector
    // with the actual astroband numbers in that case.
    // Otherwise assume the input list is valid.
    if ( absize == 1 && astroBands[0] == 0 ) {
        astroBands = signalPathMapper_->getActiveAstroBandNoVec( csType );
        absize = astroBands.size();
    }
    if ( absize == 0 ) return;

    // Aggregate all individual group members into one group
    // for dispatch via queue/WorkResult method.
    CorrelatorGroup aggregatedCorrHandles;
    ostringstream bos;
    for ( unsigned short i = 0 ; i < absize ; ++i) {

        // Zero not allowed as part of a sequence that
        // otherwise includes non-zero astroband numbers.
        // Also, this command is relevant only for wideband, ABNO>=9.
        if ( astroBands[i] == 0 || astroBands[i] < 9)  continue;

        if ( !signalPathMapper_->isValidAstroBand( astroBands[i], csType ) )
        {
            ostringstream os;
            os << "SPM SAYS BAND " << astroBands[i] << " IS NOT VALID IN "
                << getStringForCorrType( csType );
            programLogInfo( os.str() );
            continue;
        }

        std::vector<carma::signalpath::SignalPathMapperControl::CorrelatorBand> bandVec =
          signalPathMapper_->getCorrelatorBands(astroBands[i]);

        const CorrelatorGroup correlatorGroup =
          getCorrelatorGroup(cid, bandVec);

        if ( !correlatorGroup.empty() ) {

          CorrelatorGroup::iterator ci = correlatorGroup.begin();
          CorrelatorGroup::iterator ce = correlatorGroup.end();
          while ( ci != ce ) {
            //  only dispatch this command to wideband bands
            //  These cannot be weeded out by checking csCorrType
            //  because CORR_ANY is valid
            if ( ! ( *ci )->isSpectral() ) {
              aggregatedCorrHandles.insert( *ci );
            } 
            ci++;
          }
        }
    }

    // Now dispatch the commands to the aggregated handles.
    if ( ! aggregatedCorrHandles.empty() ) {
        const string b = bos.str();
        ostringstream reqOs;
        reqOs << "CorrelatorHandle::enableCorrelation( enable="
              << boolalpha << correlationsEnabled << " )";
        const string requestIdCallString = reqOs.str();

        WorkResultSet wrs( requestIdCallString );

        queueFunctorWorkRequestGroup(
             requestIdCallString,
             makeHandleMethodFunctorGroup(
                        aggregatedCorrHandles,
                        &CorrelatorHandle::enableCorrelation,
                        correlationsEnabled),
             wrs,
             *workerPool_ );

        const unsigned long lateAfterMillis =
              ::std::max( kPaddedCorrLateAfterMillis,
                          getDefaultLateAfterMillis() );
        waitForAllNormal( wrs, lateAfterMillis, false );

    } else {
       programLogNotice("Aggregated correlator group was empty"); 
    }

} catch ( ... ) {
    rethrowCaughtAsUser();
}

void 
SubarrayControlImpl::assertCorrelatorConfiguration()
try {

  const string cid("SubarrayControImpl::assertCorrelatorConfiguration");
  ScopedLogNdc ndc(cid);
  const CorrelatorSet corrset( csCorrType() );
  if (!corrset.isSingleCorrelator()) {
        ThrowCarmaError("assertCorrelatorConfiguration must be called with single correlator designation");
  }

  int seqNo = 0;
  const ScopedLock< ::pthread_mutex_t > lock( gCorrNextSeqNoGuard_ );
  seqNo = gCorrNextSeqNo_;
  gCorrNextSeqNo_ += 10;

  // instantiate and populate the arguments for the
  // vectorized version of setBandwidth
  vector<carma::correlator::obsRecord2::BandWidthType> bw;
  vector<carma::correlator::obsRecord2::FpgaModeType>  fm;
  vector<unsigned int> astroBandNo;
  BOOST_FOREACH( SubarrayControlImpl::ConfigAstroBandPair c, cabmap_) {
     // ensure the correct astrobands  go into C3GMAX8 or C3GMAX25 
     if ( corrset.isC3gMax8() 
         && ( c.second->astroBand_.bandNo_ > 24 && c.second->astroBand_.bandNo_<= 32)
        ) {
         bw.push_back( getBandWidthType(c.second->bandwidth_,c.second->bits_));
         fm.push_back( utilFpgaModeToObsrecordFpgaMode(
                     c.second->getFpgaMode()) );
         astroBandNo.push_back(c.second->astroBand_.bandNo_);
     } else if ( corrset.isC3gMax23() 
         && ( c.second->astroBand_.bandNo_ > 32 && c.second->astroBand_.bandNo_ <= 40 )
        ) {
         bw.push_back( getBandWidthType(c.second->bandwidth_,c.second->bits_));
         fm.push_back( utilFpgaModeToObsrecordFpgaMode(
                     c.second->getFpgaMode()) );
         astroBandNo.push_back(c.second->astroBand_.bandNo_);
         }
  }

  const CorrelatorGroup correlatorGroup = getCorrelatorGroup(cid, csCorrType() );
  if ( !correlatorGroup.empty() ) {
    const string methodName ("CorrelatorHandle::setBandwidth(<vector>)");
    WorkResultSet wrs(methodName + " result set");
      queueFunctorWorkRequestGroup(
           methodName,
           makeHandleMethodFunctorGroup(
                        correlatorGroup,
                        &CorrelatorHandle::setBandwidth,
                        bw, fm, seqNo, astroBandNo
                       ),
           wrs,
           *workerPool_);

        const unsigned long lateAfterMillis =
              ::std::max( kPaddedCorrLateAfterMillis,
                          getDefaultLateAfterMillis() );
        waitForAllNormal( wrs, lateAfterMillis, false );
  }
     
} catch ( ... ) {
    rethrowCaughtAsUser();
}

