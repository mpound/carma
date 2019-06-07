/**
 *
 * Carma control interface server implementation for commands related to
 * the correlator, downconverter and LO.
 *
 * @author: Erik Leitch
 *
 * $Id: SubarrayControlConfigAstroBand.cc,v 1.69 2014/08/26 21:46:36 scott Exp $
 *
 * $CarmaCopyright$
 *
 */
#include <sstream>

#include "carma/control/LOchain.h"
#include "carma/control/SubarrayControlImpl.h"

#include "carma/control/AntennaControls.h"
#include "carma/control/DriveHandle.h"
#include "carma/control/CorrelatorHandle.h"
#include "carma/control/CorrelatorInfo.h"
#include "carma/control/FaultHandle.h"
#include "carma/control/LOrefHandle.h"
#include "carma/control/DownconverterHandle.h"
#include "carma/control/RxSelectorHandle.h"
#include "carma/control/VlbiHandle.h"
#include "carma/control/FunctorWorkRequest.h"
#include "carma/control/HandleMethodFunctorGroup.h"
#include "carma/control/RemoteObjMethodFunctorGroup.h"
#include "carma/control/SatThreadSync.h"
#include "carma/control/WorkerPool.h"

#include "carma/antenna/common/RxTypeInfo.h"
#include "carma/correlator/obsRecord2/obsRecordUtils.h"
#include "carma/downconverter/common/DownconverterControl.h"
#include "carma/downconverter/spectral/BlockDownconverter.h"
#include "carma/fault/FaultControl.h"
#include "carma/interferometry/DelayEngine.h"
#include "carma/services/Frequency.h"
#include "carma/services/stringConstants.h"
#include "carma/monitor/ControlCorrelEnum.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/CorrDesignation.h"
#include "carma/monitor/ControlBandCommon.h"
#include "carma/monitor/SlPipelineSubsystemExt.h"
#include "carma/monitor/WbPipelineSubsystemExt.h"

#include "carma/util/AstroBand.h"
#include "carma/util/corbaSequenceUtils.h"
#include "carma/util/CorrelatorSet.h"

#include "carma/util/ExceptionUtils.h"

#include "carma/util/programLogging.h"
#include "carma/util/rangeFormatting.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/StringUtils.h"
#include "carma/util/WorkResult.h"

#include "carma/szautil/Exception.h"
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
using namespace sza::util;

//-----------------------------------------------------------------------
// Methods of ConfigAstroBand
//-----------------------------------------------------------------------

typedef boost::shared_ptr<SubarrayControlImpl::ConfigAstroBand> configab_ptr;
typedef ControlBandPoints::Lo2SidebandMonitorPointEnum    LO2SB;
typedef ControlBandPoints::ReqLO2sidebandMonitorPointEnum REQLO2SB;
typedef ControlBandPoints::ConfigurationMonitorPointEnum  CONFIG;

SubarrayControlImpl::ConfigAstroBand::ConfigAstroBand(
                              SubarrayControlImpl* parent,
                              unsigned astroBandNo,
                              const string & astroBandConf,
                              util::CorrelatorBandWidthType bandwidth,
                              double fcenter,
                              SidebandType sb,
                              double frest,
                              double imagefrest,
                              const bool online,
                              const string & transition,
                              const string & imageTransition,
                              util::CorrelatorBitType bits) :
  astroBand_(astroBandNo)
{
  parent_          = parent;
  astroBandConf_   = astroBandConf;
  bandwidth_       = bandwidth;
  fcenter_         = fcenter;
  sb_              = sb;
  frest_           = frest;
  imagefrest_      = imagefrest;
  online_          = online;
  transition_      = transition;
  imageTransition_ = imageTransition;
  bits_            = bits;
  lo2sb_           = LO2SB::LOWER;

}

/**.......................................................................
 * Assert an astroband configuration
 */
void SubarrayControlImpl::ConfigAstroBand::assertAstroBandConfiguration()
{
  //------------------------------------------------------------
  // Check the validity of a configuration before asserting it.  We
  // call remoteObj() method directly, so that exceptions propagate up
  // to the user
  //------------------------------------------------------------
  parent_->signalPathMapper_->checkConfigurationValidity(astroBand_.bandNo_, astroBandConf_,
                         parent_->subarrayNo_, parent_->csCorrType());

  //------------------------------------------------------------
  // Now assert the configuration
  //------------------------------------------------------------

  parent_->signalPathMapper_->configureAstroBand(astroBand_.bandNo_, astroBandConf_,
                         parent_->subarrayNo_, parent_->csCorrType());

  //------------------------------------------------------------
  // Now check that all required antennas are either in our subarray
  // or offline
  //------------------------------------------------------------

  checkSubarrayMembership();

  //------------------------------------------------------------
  // Now set the IF switch positions
  //------------------------------------------------------------

  setIFSwitchPositions();

  //------------------------------------------------------------
  // Set the second (DC) LO switch positions
  //------------------------------------------------------------

  setDCLOSwitchPositions();

  // Note that we no longer set the LO and LL switch positions on
  // configuration of an astroband; these are instead set by
  // addAntenna()
}

/**.......................................................................
 * Assert an astroband configuration
 */
void SubarrayControlImpl::ConfigAstroBand::updateCorrDataRemapper()
{

  ScopedLogNdc ndc("SacI:ConfigAstroBand::updateCorrDataRemapper");

  // Get the vector of correlator bands asociated with this astroband

  ::std::vector<carma::signalpath::SignalPathMapperControl::CorrelatorBand>
    cbVec = parent_->signalPathMapper_->getCorrelatorBands(astroBand_.bandNo_);

  // Now iterate over the input map for each correlator band,
  // accumulating astroband input assignments into abVec

  std::vector<CorrDataRemapperControl::AstroBandInput> abVec;
  for(unsigned iCorrBand=0; iCorrBand < cbVec.size(); iCorrBand++) {

    ::std::vector<carma::signalpath::SignalPathMapperControl::CorrelatorBandInput>
      cbiVec = parent_->signalPathMapper_->getCorrelatorBandInputMap(cbVec[iCorrBand]);

    // Loop over each input

    for(unsigned iInput = 0; iInput < cbiVec.size(); iInput++) {

      carma::signalpath::SignalPathMapperControl::CorrelatorBandInput& input = cbiVec[iInput];

      CorrDataRemapperControl::AstroBandInput abInput;
      abInput.inputNo   = input.aBandInput.inputNo;
      abInput.antennaIF = input.antIF;
      abVec.push_back(abInput);
    }
  }

  // Finally, pass the mapping down to the corrdataremapper
  if ( parent_->corrDataRemapper_.get() != 0 ) {
    parent_->corrDataRemapper_->updateAstroBandInputMap(astroBand_.bandNo_, abVec);
  } else {
    programLogWarnIfPossible("CorrDataRemapperHandle is uninitialized.");
  }
}

/**.......................................................................
 * Check that the antnenas involved in an astroband configuration are
 * part of this subarray
 */
void SubarrayControlImpl::
ConfigAstroBand::checkSubarrayMembership()
{
  std::vector<SignalPathMapperControl::Antenna> antVec =
    parent_->signalPathMapper_->getAntennas(astroBand_.bandNo_);

  for(unsigned iAnt=0; iAnt < antVec.size(); iAnt++) {
    unsigned antNo = antVec[iAnt].antNo;

    if(parent_->antIsInAnotherSubarray(antNo)) {
      ThrowCarmaError("Configuration for astro band " << astroBand_.bandNo_
              << " involves antenna C" << antNo << ", which is currently in another subarray");
    }
  }
}

/**.......................................................................
 * Set IF switch positions for a given astroband configuration
 */
void SubarrayControlImpl::
ConfigAstroBand::setIFSwitchPositions()
{
  std::vector<carma::switchyard::SwitchPosition> swVec =
    parent_->signalPathMapper_->getIFSwitchSettings(astroBand_.bandNo_);

  parent_->ifSwitchyard_->setSwitches(swVec);
}

/**.......................................................................
 * Set LO switch positions for a given astroband configuration
 */
void SubarrayControlImpl::
ConfigAstroBand::setLOSwitchPositions()
{
  std::vector<carma::switchyard::SwitchPosition> swVec =
    parent_->signalPathMapper_->getLOSwitchSettings(astroBand_.bandNo_);

  parent_->loSwitchyard_->setSwitches(swVec);
}

/**.......................................................................
 * Set DCLO switch positions for a given astroband configuration
 */
void SubarrayControlImpl::
ConfigAstroBand::setDCLOSwitchPositions()
{
  std::vector<carma::switchyard::SwitchPosition> swVec =
    parent_->signalPathMapper_->getDCLOSwitchSettings(astroBand_.bandNo_);

  if(swVec.size() > 0) {
    parent_->dcLoSwitchyard_->setSwitches(swVec);
  }
}

/**.......................................................................
 * Set LL switch positions for a given astroband configuration
 */
void SubarrayControlImpl::
ConfigAstroBand::setLLSwitchPositions()
{
  std::vector<carma::switchyard::SwitchPosition> swVec =
    parent_->signalPathMapper_->getLLSwitchSettings(astroBand_.bandNo_);

  parent_->llSwitchyard_->setSwitches(swVec);
}

/**.......................................................................
 * Return the hardware type of this astroband
 */
util::hardwareType SubarrayControlImpl::
ConfigAstroBand::bType()
{
  return util::hwType(astroBand_);
}

/**.......................................................................
 * Check for internal  consistency in input arguments
 */
void SubarrayControlImpl::ConfigAstroBand::checkInputArguments()
{
  if(parent_->subarrayIsIntegrating())
    throw CARMA_ERROR("You may not issue configastroband while integrating");
  
  if ( bType() == util::HARDWARE_TYPE_COBRA && 
       bits_ != util::CORR_2BIT ) 
    throw CARMA_ERROR("COBRA band hardware supports only 2-bit operation. Use bits=CORR_2BIT in configastroband().");
}

/**.......................................................................
 * Parse a bandwidth specification for configAstroBand()
 */
void SubarrayControlImpl::ConfigAstroBand::
parseBandwidth()
{
  bType_ = bType();
  const double bwmax = 500.0;

  switch (bandwidth_) {
  case util::CORR_BW_500MHZ:
    bwmhz_ = bwmax/1;
    break;
  case util::CORR_BW_250MHZ:
    if ( bType_ != HARDWARE_TYPE_CARMA && bType_ != HARDWARE_TYPE_C3G) 
    {
      ThrowCarmaError(" AstroBand " << astroBand_.bandNo_
              << " hardware type is " << getStringForHardwareType(bType_)
              << " . This hardware type does not support 250MHz bw");
    }
    bwmhz_ = bwmax/2;
    break;
  case util::CORR_BW_125MHZ:
    if ( bType_ != HARDWARE_TYPE_CARMA && bType_ != HARDWARE_TYPE_C3G) 
    {
      ThrowCarmaError(" AstroBand " << astroBand_.bandNo_
              << " hardware type is " << getStringForHardwareType(bType_)
              << " . This hardware type does not support 125MHz bw");
    }
    bwmhz_ = bwmax/4;
    break;
  case util::CORR_BW_62MHZ:
    bwmhz_ = bwmax/8;
    break;
  case util::CORR_BW_31MHZ:
    bwmhz_ = bwmax/16;
    break;
  case util::CORR_BW_8MHZ:
    bwmhz_ = bwmax/64;
    break;
  case util::CORR_BW_2MHZ:
    bwmhz_ = bwmax/256;
    break;
  default:
    bwmhz_ = 0;
    break;
  }

  if(bwmhz_ > 0.0) {
    corrmodeStream_ << static_cast<int>(floor(bwmhz_+0.4));
  }
}

/**.......................................................................
 * Format the bandwidth as a string
 */
std::string SubarrayControlImpl::ConfigAstroBand::
bwString()
{
  std::stringstream os;

  if(bwmhz_ > 0.0) {
    os << (unsigned)bwmhz_ << "MHz";
  } else {
    os << "BW?";
  }

  return os.str();
}

/**.......................................................................
 * Return the DC filter type for the current hardware
 */
DownconverterControl::FilterType SubarrayControlImpl::ConfigAstroBand::
dcFilter()
{
  switch(bandwidth_) {
  case util::CORR_BW_500MHZ:
    return DownconverterControl::FILTER_490MHZ;
    break;
  default:
    if(bType_ == HARDWARE_TYPE_COBRA) {
      return DownconverterControl::FILTER_58MHZ;
    } else {
      return DownconverterControl::FILTER_280MHZ;
    }
    break;
  }
}

void SubarrayControlImpl::ConfigAstroBand::
parseBits()
{
  // Set correlator quantization bits enumerations value
  // and add bits to correlator mode string.

  typedef ControlBandPoints::CorrBitsMonitorPointEnum       CBITS;

  switch ( bits_ ) {
  case CORR_2BIT:
    nbit_ = 2;
    cbits_ = CBITS::CORR_2BIT;
    break;
  case CORR_3BIT:
    nbit_ = 3;
    cbits_ = CBITS::CORR_3BIT;
    corrmodeStream_ << "b3";
    break;
  case CORR_4BIT:
    nbit_ = 4;
    cbits_ = CBITS::CORR_4BIT;
    corrmodeStream_ << "b4";
    break;
  default:
    {
      // this should never happen

      ostringstream errOs;
      errOs << "Impossible correlator bit selection: " << bits_;
      throw CARMA_ERROR( errOs.str() );
    }
    break;
  }
}

/**.......................................................................
 * Set up correlator band control monitor points
 */
void SubarrayControlImpl::ConfigAstroBand::
setupCorrMps()
{
  // ScopedLogNdc ndc("SubarrayControlImpl::ConfigAstroBand::setupCorrMps(void)");
  // Use signalpathmapper handle class to determine which bands to control and what
  // their correlator type is (WB or SPECTRAL), then call:
  // setupCorrMps(bandNo,corrType) for each one

  std::vector<carma::signalpath::SignalPathMapperControl::CorrelatorBand> bandVec =
    parent_->signalPathMapper_->getCorrelatorBands(astroBand_.bandNo_);

  const ControlCorrelatorDesignation cType = 
      parent_->signalPathMapper_->getCorrTypeForAstroBand(astroBand_.bandNo_);

  for(unsigned iCorrBand=0; iCorrBand < bandVec.size(); iCorrBand++) {
   setupCorrMps(bandVec[iCorrBand].bandNo, cType);
  }

  parent_->markStateChange();
}

/**.......................................................................
 * Set up correlator band control monitor points for a single band
 */
void SubarrayControlImpl::ConfigAstroBand::
setupCorrMps(unsigned bandNo, const ControlCorrelatorDesignation cType)
{
  // Get a pointer to the correct part of the monitor system

  const short bandIndex   = bandNo - 1;
  ControlBandPoints* cbp  = 0;

  const CorrelatorSet corrset( cType );

  if ( corrset.isEmpty() ) { 
    throw CARMA_EXCEPTION(UserException,
        "setupCorrMps: Correlator set contains no correlators");
  }

  if ( corrset.isSpectral() ) {
    cbp = &parent_->controlSubsystem_.spectralLineCorrelator().
      slcBand(bandIndex).controlBandPoints();
    // offline bands get an X
    if(online_)
      parent_->slcmodeVec_.at(bandIndex) = corrmodeStream_.str();
    else 
    {
      parent_->slcmodeVec_.at(bandIndex) = "X";
    }
  }

  if ( corrset.isWideband() ) {
    cbp = &parent_->controlSubsystem_.widebandCorrelator().
      wbcBand(bandIndex).controlBandPoints();
    // offline bands get an X
    if(online_)
      parent_->wbcmodeVec_.at(bandIndex) = corrmodeStream_.str();
    else 
    {
      parent_->wbcmodeVec_.at(bandIndex) = "X";
    }
   }

  if ( corrset.isC3gMax8() ) {
    cbp = &parent_->controlSubsystem_.c3gMax8Correlator().
      c3gMax8Band(bandIndex).controlBandPoints();
    // offline bands get an X
    if(online_)
      parent_->c3gMax8modeVec_.at(bandIndex) = corrmodeStream_.str();
    else 
    {
      parent_->c3gMax8modeVec_.at(bandIndex) = "X";
    }
  }

  if ( corrset.isC3gMax23() ) {
    cbp = &parent_->controlSubsystem_.c3gMax23Correlator().
      c3gMax23Band(bandIndex).controlBandPoints();
    // offline bands get an X
    if(online_)
      parent_->c3gMax23modeVec_.at(bandIndex) = corrmodeStream_.str();
    else 
    {
      parent_->c3gMax23modeVec_.at(bandIndex) = "X";
    }
  }

  if(cbp == 0) {
    throw CARMA_EXCEPTION(UserException,
        "setupCorrMps: Couldn't get pointer to control band points monitor system");
  }

  // Update monitor points

  cbp->centerFreq().setValue(fcenter_);
  cbp->restFreq().setValue(frest_);
  cbp->imageRestFreq().setValue(imagefrest_);

  if (sb_ == SB_UPPER) {
    cbp->reqLO2sideband().setValue(REQLO2SB::UPPER);
  } else if(sb_ == SB_LOWER) {
    cbp->reqLO2sideband().setValue(REQLO2SB::LOWER);
  } else if(sb_ == SB_AUTO) {
    cbp->reqLO2sideband().setValue(REQLO2SB::AUTO);
  }

  cbp->bandwidth().setValue( bwmhz_ );
  cbp->online().setValue( online_ );
  cbp->astrobandConf().setValue( astroBandConf_ );
  cbp->transition().setValue( transition_ );
  cbp->imageTransition().setValue( imageTransition_ );
  cbp->corrBits().setValue( cbits_ );


  //------------------------------------------------------------
  // Set the fpga mode based on the astro band configuration
  //------------------------------------------------------------

  carma::util::CorrelatorFpgaModeType fpgaMode =
    parent_->signalPathMapper_->getFpgaMode(astroBand_.bandNo_);

  cbp->fpgaMode().setValue(parent_->msFpgaMode(fpgaMode));

  const string desc = parent_->makeCorrModeDescString( corrset );

  if(corrset.isSpectral()) {
    parent_->controlSubsystem_.spectralLineCorrelator().modeDesc().setValue(desc);
  }

  if(corrset.isWideband()) {
    parent_->controlSubsystem_.widebandCorrelator().modeDesc().setValue(desc);
  }

  if(corrset.isC3gMax8()) {
    parent_->controlSubsystem_.c3gMax8Correlator().modeDesc().setValue(desc);
  }

  if(corrset.isC3gMax23()) {
    parent_->controlSubsystem_.c3gMax23Correlator().modeDesc().setValue(desc);
  }

  // set the expected number of channels,
  // number of correlatorbands monitor points
  // and correlator efficiency
  unsigned nchan      = numExpectedAstroChans( cType, bandwidth_, 
                                               bits_, fpgaMode );
  unsigned nCorrBands = numExpectedCorrBands( astroBandConf_, cType );

  parent_->signalPath_.mapping().astroband( astroBand_.bandNo_ - 1 )
                      .expectedChannels().setValue( nchan );
  parent_->signalPath_.mapping().astroband( astroBand_.bandNo_ - 1 )
                      .expectedCorrBands().setValue( nCorrBands );
  parent_->signalPath_.mapping().astroband( astroBand_.bandNo_ - 1 )
                      .correlatorEfficiency()
                      .setValue( correlatorEfficiency( bits_ ) );

}

/**.......................................................................
 * Update downconverter filter settings for spectral-line
 * downconverters involved in this astroband
 */
void SubarrayControlImpl::ConfigAstroBand::
selectFilter()
{
  if(parent_->slDownconverter_.get() != 0) {

    // Get all correlator bands associated with this astro band

    std::vector<SignalPathMapperControl::CorrelatorBand> bandVec =
      parent_->signalPathMapper_->getCorrelatorBands(astroBand_.bandNo_);

    // Iterate over bands, setting the filter for each one

    for(unsigned iCorrBand=0; iCorrBand < bandVec.size(); iCorrBand++) {

      SignalPathMapperControl::CorrelatorBand band = bandVec[iCorrBand];

      // Only try to set filters for bands of the spectral-line
      // correlator, regardless of which subarray is currently controlling

      if(band.crate.type == util::CORR_SPECTRAL) {
        parent_->slDownconverter_->selectFilter(dcFilter(), band.bandNo);
      }
    }
  }
}

/**.......................................................................
 * Return the group of correlators associated with this astroband
 */
std::set<CorrelatorHandle *> SubarrayControlImpl::ConfigAstroBand::
getCorrelatorGroup()
{
  return getCorrelatorGroup(parent_, astroBand_);
}

/**.......................................................................
 * Return the group of correlator associated with this correlator band
 */
std::set<CorrelatorHandle *> SubarrayControlImpl::ConfigAstroBand::
getCorrelatorGroup(SubarrayControlImpl* parent, unsigned short corrBandNo, ControlCorrelatorDesignation corrType)
{

  return parent->getCorrelatorGroup("configAstroband", corrBandNo, corrType, false);
}

/**.......................................................................
 * Return the group of correlator associated with this astroband
 */
std::set<CorrelatorHandle *> SubarrayControlImpl::ConfigAstroBand::
getCorrelatorGroup(SubarrayControlImpl* parent, carma::util::AstroBand& astroBand)
{
  std::vector<carma::signalpath::SignalPathMapperControl::CorrelatorBand> bandVec =
    parent->signalPathMapper_->getCorrelatorBands(astroBand.bandNo_);

  return parent->getCorrelatorGroup("configAstroband", bandVec);
}

/**.......................................................................
 * Return the group of vlbi backends associated with this astroband
 */
std::set<VlbiHandle *> SubarrayControlImpl::ConfigAstroBand::
getVlbiGroup()
{
  // Get the vector of correlator bands associated with this astro
  // band

  std::vector<carma::signalpath::SignalPathMapperControl::CorrelatorBand> bandVec =
    parent_->signalPathMapper_->getCorrelatorBands(astroBand_.bandNo_);

  // Now populate the result with the corresponding correlator handles

  VlbiGroup result;

  for(unsigned iCorrBand=0; iCorrBand < bandVec.size(); iCorrBand++) {
    unsigned corrBandNo = bandVec[iCorrBand].bandNo;

    if(1 <= corrBandNo && corrBandNo <= parent_->vlbiVec_->size()) {
      VlbiHandle * const vlbiHandle =   parent_->vlbiVec_->at(corrBandNo - 1);

      if(vlbiHandle != 0 && vlbiHandle->isObjReachable(false))
        result.insert(vlbiHandle);
    }
  }

  // Empty VlbiGroup is OK, no need to warn

  return result;
}

/**.......................................................................
 * Return the group of vlbi backends associated with this correlator band
 */
std::set<VlbiHandle *> SubarrayControlImpl::ConfigAstroBand::
getVlbiGroup(SubarrayControlImpl* parent, unsigned short corrBandNo)
{
  // Now populate the result with the corresponding correlator handles

  VlbiGroup result;

  if(1 <= corrBandNo && corrBandNo <= parent->vlbiVec_->size()) {
    VlbiHandle * const vlbiHandle = parent->vlbiVec_->at(corrBandNo - 1);

    if(vlbiHandle != 0 && vlbiHandle->isObjReachable(false))
      result.insert(vlbiHandle);
  }

  // Empty VlbiGroup is OK, no need to warn

  return result;
}

/**.......................................................................
 * Set up correlator bandwidth
 */
void SubarrayControlImpl::ConfigAstroBand::
setupCorrelatorBandwidth()
{
  const obsRecord2::BandWidthType bw = 
      obsRecord2::getBandWidthType(bandwidth_, bits_);

  CORBA::Long seqNo = 0;

  const ScopedLock< ::pthread_mutex_t > lock( parent_->gCorrNextSeqNoGuard_ );
  seqNo = parent_->gCorrNextSeqNo_;
  parent_->gCorrNextSeqNo_ += 10;

  // Send to the correlators associated with this astroband (ie, use
  // ConfigAstroBand::getCorrelatorGroup() instead of
  // SubarrayControlImpl::getCorrelatorGroup())

  const CorrelatorGroup correlatorGroup = getCorrelatorGroup();

  if ( !correlatorGroup.empty() ) {
    WorkResultSet wrs("CorrelatorHandle::setBandwidth() result set");

      queueFunctorWorkRequestGroup(
                       "CorrelatorHandle::setBandwidth()",
                       makeHandleMethodFunctorGroup(
                                    correlatorGroup,
                                    &CorrelatorHandle::setBandwidth,
                                    bw,
                                    static_cast<int>( seqNo ),
                                    astroBand_.bandNo_  ),
                       wrs,
                       *(parent_->workerPool_) );

      // Kevin says setBandwidth takes ~ 20-30s so,
      // give a 60 second timeout here.
      parent_->waitForAllNormal( wrs, (10UL * 6000UL), true );  
      
      // Note Walsh column updating must happen after setBandwidth
      // but before optimizeThresholds.  So put it immediately after
      // the dispatch/return for setBandwidth.

      // EML commenting this back in as of 14 Nov 2011
      //      parent_->updateWalshColumns( correlatorGroup, astroBand_.bandNo_);

      // But replacing with rconfiguration of ALL SL bands managed by
      // this sac, since configuring a band might necessitate changing
      // the walsh functions for other bands

      parent_->updateWalshColumns();
  }

  // Send to VLBI backends
  const VlbiGroup vlbiGroup = getVlbiGroup();
  if(!vlbiGroup.empty()) {

    WorkResultSet vlbi_wrs("VlbiHandle::setBandwidth() result set");

    queueFunctorWorkRequestGroup(
               "VlbiHandle::setBandwidth()",
               makeHandleMethodFunctorGroup(
                    vlbiGroup,
                    &VlbiHandle::setBandwidth,
                    bw,
                    static_cast<int>( seqNo ),
                                  astroBand_.bandNo_  ),
               vlbi_wrs,
               *(parent_->workerPool_) );

    // give a 5 second timeout here.

    try {
      parent_->waitForAllNormal( vlbi_wrs, (5UL * 1000UL), true );  // 5 second timeout
    } catch(const ErrorException & e) {
      // Log error and move on
      e.log(log4cpp::Priority::ERROR);
    }

    // Is this correct?  What is the walshing for the VLBI correlator?
    // parent_->updateWalshColumns( vlbiGroup );
  }
}

/**.......................................................................
 * Log the command
 */
void SubarrayControlImpl::ConfigAstroBand::
logCommand()
{
  string sbstring = "USB";

  if(sb_ == SB_LOWER)
    sbstring = "LSB";

  if(sb_ == SB_AUTO)
    sbstring = "AUTO";

  // Set correlator quantization bits enumerations value
  // and add bits to correlator mode string.

  parseBits();

  cmdlog() << "configAstroBand("
       << "astroBandNo="           << astroBand_.bandNo_
       << ", astroBandConf="       << astroBandConf_
       << ", bandwidth="           << bwString()
       << ", fcenter="             << fcenter_
       << ", sb="                  << sbstring
       << ", frest="               << frest_
       << ", imagefrest="          << imagefrest_
       << boolalpha << ", online=" << online_
       << ", transition="          << transition_
       << ", imageTransition="     << imageTransition_
       << ", bits="                << nbit_
       << ")";
}


/**.......................................................................
 * Send relevant information about this astro band configuration to the correlator
 *
 * EML: significant changes are needed here.  There is no longer a
 * single correlator handle associated with an astro band.  There
 * are in general N correlator crates (or bands, or whatever
 * 'handle' refers to), each of which can receive inputs from
 * arbitrary antenna polarizations.  This means that:
 *
 * 1) We need to query the SignalPathMapper DO to determine which
 *    correlator handles we are controlling.
 *
 * 2) we need to query the SignalPathMapper DO determine the
 *    mapping of correlator input to antenna IF and polarization
 *
 * 3) CorrelatorHandle now needs to have a method to take a vector
 *    of antenna numbers and polarizations specifying this mapping
 *
 * 4) CorelatorHandle needs to implement sending a vector of delay
 *    triplets that is constructed out of correctly ordered
 *    selections from CorrelatorHandle::tripletsPol1 and
 *    CorrelatorHandle::tripletsPol2
 */
void SubarrayControlImpl::ConfigAstroBand::
updateCorrelator(bool updateFpgaMode)
{
  // Query the signalpath mapper to get the correlator group associated with this astroband

  CorrelatorGroup correlatorGroup = getCorrelatorGroup();
  VlbiGroup vlbiGroup = getVlbiGroup();

  // Get the FPGA mode from the signalpath mapper

  util::CorrelatorFpgaModeType fpgaMode = CORR_SINGLEPOL; //shut the compiler up 
  if ( updateFpgaMode )
      fpgaMode = parent_->signalPathMapper_->getFpgaMode(astroBand_.bandNo_);

  // Now iterate over all correlator handles associated with this
  // astro band

  for(CorrelatorGroup::iterator iter=correlatorGroup.begin(); iter != correlatorGroup.end(); iter++) {

    (*iter)->setOnline(online_);
    if (updateFpgaMode) (*iter)->setAstroBandMode(fpgaMode);

  }

  // Now iterate over all VLBI handles associated with this
  // astro band

  for(VlbiGroup::iterator iter=vlbiGroup.begin(); iter != vlbiGroup.end(); iter++) {

#if 0
    // Only allow bands 5, 6, 7, and 8 to be marked online
    if (5 <= astroBand_.bandNo_ && astroBand_.bandNo_ <= 8) {
      (*iter)->setOnline(online_);
      if (updateFpgaMode) (*iter)->setAstroBandMode(fpgaMode);
    }
#else
    // Set all VLBI handle offline to prevent excessive logging when
    // VLBI backends are not running.  Ideally, the control system will
    // automatically and correctly manage the online/offline status of the VLBI
    // handles, but that is not currently working as desired.  This is a
    // temporary work around in the meantime.
    (*iter)->setOnline(false);
#endif

  }
}


/**.......................................................................
 * Update block downconverter settings
 */
void SubarrayControlImpl::ConfigAstroBand::
updateBlockDownconverter()
{
  // If this band is offline, skip it completely.
  // We don't want to touch the hardware.
  if ( online_ == false) return;
  std::vector<carma::signalpath::SignalPathMapperControl::CorrelatorBand> bandVec =
    parent_->signalPathMapper_->getCorrelatorBands(astroBand_.bandNo_);

  for(unsigned iCorrBand=0; iCorrBand < bandVec.size(); iCorrBand++) {
    updateBlockDownconverter(bandVec[iCorrBand].crate.type, bandVec[iCorrBand].bandNo);
  }
}

/**.......................................................................
 * Update block downconverter settings for a single correlator band
 */
void SubarrayControlImpl::ConfigAstroBand::
updateBlockDownconverter(const ControlCorrelatorDesignation type, const short bandNo)
{

  // If input astroband is offline, skip it completely.
  // If this band is offline, skip it completely.
  // We don't want to touch the hardware.
  if ( online_ == false) return;

  const CorrelatorSet corrset( type );

  const short b = bandNo - 1;
  ControlBandPoints* cbp = 0;

  if (corrset.isSpectral() ) {
    cbp = &(parent_->controlSubsystem_.spectralLineCorrelator().
        slcBand(b).controlBandPoints());
  }
  if (corrset.isWideband() ) {
    // Do not reset the BlockDC for wideband correlator, but set MPs!

    cbp = &(parent_->controlSubsystem_.widebandCorrelator().
        wbcBand(b).controlBandPoints());
  }

  // no BDC for C3g correlator(?)
  if ( corrset.isC3gMax8() || corrset.isC3gMax23() ) {
          return;
  }

  if (cbp == 0) {
    ThrowCarmaError("updateBDC: Couldn't get pointer to control band points monitor system: type = " << type << " bandNo = " << bandNo);
  }

  // Decide what block (L, U, for lower and upper) to use
  // depending on whether the IF band center is less than or
  // greater than 5.000 GHz

  double bandCenter = s1_*(dopfac_*fcenter_ - parent_->loFreq_);

  BlockDownconverterControl::Block block =
    bandCenter > 5.0 ? BlockDownconverterControl::UPPER
    : BlockDownconverterControl::LOWER;

  parent_->setBlockDownconverterEnabled(type, b+1, 
                (block == BlockDownconverterControl::UPPER) );

  // If wideband correlator system, we do not get to choose the value
  // of the block downconverter state. So force it here.
  // NB: can probably remove this statement, as the real error
  // was assigning the WB bands in incorrect frequency order at the
  // python level.

  if ( corrset.isWideband() ) {
    parent_->setBlockDownconverterEnabled(type, b+1, 
            (bandNo < 9 ? false : true) );
  }

  // The downconverter filter selection must be done
  // before the call to setBandwidth, since for the CARMA boards,
  // setBandwidth may also call flattenPhases().
  // Do the blockDC setting here as well, noting that it depends on
  // the IF frequency, which can only be determined from the doppler shift.
  
  if ( parent_->slDownconverter_.get() != 0  && corrset.isSpectral() ) {

    // Iterate over block downconverter settings for this astro band
    std::vector<SignalPathMapperControl::BlockDownconverterSetting> bdcSettings =
      parent_->signalPathMapper_->getBdcSettings(astroBand_.bandNo_);

    for(unsigned iBdcSetting=0; iBdcSetting < bdcSettings.size(); iBdcSetting++) {
      SignalPathMapperControl::BlockDownconverterSetting setting = 
            bdcSettings[iBdcSetting];
      parent_->slDownconverter_->setBlockAndPolarization(block,
                               setting.bdcInputType,
                               setting.bdcNo,
                               setting.bandNo);
    }
  }

  // set the control band point regardless of whether or not the
  // call to the downconverter succeeded.
  cbp->bdcEnabled().setValue(parent_->getBlockDownconverterEnabled(type, b+1));
}

/**.......................................................................
 * Update second LO frequency for all correlator bands involved in
 * this astro band
 */
void SubarrayControlImpl::ConfigAstroBand::
updateLo2Frequency()
{
  // If this band is offline, skip it completely.
  if ( online_ == false ) return;

  std::vector<SignalPathMapperControl::CorrelatorBand> bandVec = 
        parent_->signalPathMapper_->getCorrelatorBands(astroBand_.bandNo_);

  for(unsigned iCorrBand=0; iCorrBand < bandVec.size(); iCorrBand++) {
    updateLo2Frequency(bandVec[iCorrBand].crate.type, bandVec[iCorrBand].bandNo);
  }

  // And update frequency monitor points

  parent_->updateFrequencyMPs();
}


/**.......................................................................
 * Update second LO frequency for a single correlator band
 */
void SubarrayControlImpl::ConfigAstroBand::
updateLo2Frequency(const ControlCorrelatorDesignation type, const short bandNo)
{
  //if (!lo2DopplerTrackingEnabled_) return;

  // If this band is offline, skip it completely.
  // We don't want to touch the hardware.
  if ( online_ == false) return;

  const CorrelatorSet corrset( type);
  if (corrset.isEmpty()) return;

  ScopedLogNdc ndc("updateLo2Frequency(ControlCorrelatorDesignation type, const short bandNo)");

  const double BASEBAND_CENTER = 0.75; // baseband center freq, GHz

  // Spectral Downconverter, 2nd LO frequency lower limit, GHz
  // In Hawkin's correlator document this range is called fLO3
  const double LO2_LOWER_LIMIT = 1.75;
  // Spectral Downconverter, 2nd LO frequency upper limit, GHz
  const double LO2_UPPER_LIMIT = 4.25;
  const double LO2_LSB_UPPER_LIMIT = LO2_UPPER_LIMIT - BASEBAND_CENTER;

  // Fixed Block downconverter LO freq, GHz. In Hawkin's correlator document,
  // this is called fLO2.
  const double BLOCK_DC_LO     = 10.0;

  // Fixed LSB digitizer sampling clock. fLO4 in Hawkin's document.
  //const double DIGITIZER_SAMPLING_CLOCK = 1.0; // GHz

  const short b = bandNo - 1; // bandIndex
  ControlBandPoints * cbp = 0;

  if ( corrset.isSpectral() ){
    cbp = &(parent_->controlSubsystem_.spectralLineCorrelator().
        slcBand(b).controlBandPoints());
  }
  if ( corrset.isWideband() ){
    cbp = &(parent_->controlSubsystem_.widebandCorrelator().
        wbcBand(b).controlBandPoints());
  }
  if ( corrset.isC3gMax8() ) {
    cbp = &(parent_->controlSubsystem_.c3gMax8Correlator().
        c3gMax8Band(b).controlBandPoints());
  }
  if ( corrset.isC3gMax23() ) {
    cbp = &(parent_->controlSubsystem_.c3gMax23Correlator().
        c3gMax23Band(b).controlBandPoints());
  }

  if (cbp == 0) {
    ThrowCarmaError("updateLo2: Couldn't get pointer for writing to control band points monitor system type = " << type);
  }


  // Get values from monitor points
  //double fcenter         = readCbp->centerFreq().getValue(); // Used for setup
  //double frest           = readCbp->restFreq().getValue();
  //double imagefrest      = readCbp->imageRestFreq().getValue();
  //string transition      = readCbp->transition().getValue();
  //string imageTransition = readCbp->imageTransition().getValue();

  if (fcenter_ > parent_->loRestFreq_) {
    // Upper sideband is index 0, LSB is index 1
    // see ControlBandCommon.mpml.
    cbp->sideband(0).transition().setValue(transition_);
    cbp->sideband(1).transition().setValue(imageTransition_);
    cbp->sideband(0).restFreq().setValue(frest_);
    cbp->sideband(1).restFreq().setValue(imagefrest_);
  }
  else  {
    // Upper sideband is index 0, LSB is index 1
    // see ControlBandCommon.mpml.
    cbp->sideband(1).transition().setValue(transition_);
    cbp->sideband(0).transition().setValue(imageTransition_);
    cbp->sideband(1).restFreq().setValue(frest_);
    cbp->sideband(0).restFreq().setValue(imagefrest_);
  }

  // Compute new IF freq and store as MP; compute and save difference
  double prevIffreq = iffreq_;
  iffreq_ = s1_*(dopfac_*fcenter_ - parent_->loFreq_);
  if ( corrset.isWideband() ) {
      // The IF frequencies/2nd LOs are fixed in the WB correlator.
      iffreq_ = 1.25 + (bandNo-1)*0.5;
  }
  double deltaIffreq = abs(iffreq_ - prevIffreq);
  cbp->ifFreq().setValue(iffreq_);

  // Added by EML to debug IF freq randomly changing for some bands
  /*
  {
    ostringstream os;
    os << "EML: just set iffreq = " << iffreq_ << " for band " << bandNo << " in ConfigAstroBand::updateLo2Frequency()"
       << "  CorrType        = " << corrset.corrTypeString() 
       << "  prevIffreq      = " << prevIffreq 
       << "  ifFreq          = " << iffreq_ 
       << "  s1              = " << s1 
       << "  dopfac          = " << dopfac_ 
       << "  fcenter         = " << fcenter_
       << "  parent->loFreq_ = " << parent_->loFreq_ ;

    programLogInfo(os.str());
  }*/

  // Select LO2 sideband
  REQLO2SB::REQLO2SIDEBAND reqlo2sb;

  switch ( sb_ ) {
    case SB_UPPER:
        reqlo2sb = REQLO2SB::UPPER;
        lo2sb_   = LO2SB::UPPER;
        break;
    case SB_LOWER:
        reqlo2sb = REQLO2SB::LOWER;
        lo2sb_   = LO2SB::LOWER;
        break;
    default:
    case SB_AUTO:
        reqlo2sb = REQLO2SB::AUTO;
    break;
  }

  const bool bdcE = parent_->getBlockDownconverterEnabled( type, b+1 );
  if (reqlo2sb == REQLO2SB::AUTO) {
    // Take into account the Block downconverter when choosing
    // a sideband automatically
    double tmpIf = bdcE ?  BLOCK_DC_LO - iffreq_ : iffreq_ ;
    // If iffreq has changed by < 2 MHz we try not to change sb
    if (deltaIffreq <= 0.002) {
      if (tmpIf > (LO2_UPPER_LIMIT - BASEBAND_CENTER)) {
        lo2sb_ = LO2SB::UPPER;
      }
      else if (tmpIf < (LO2_LOWER_LIMIT + BASEBAND_CENTER)) {
        lo2sb_ = LO2SB::LOWER;
      }
      else {
        // Leave unchanged
      }
    } else {
      // Automatically choose a sideband
      // The downconverter have a problem w/USB so only use if we must
      if (tmpIf < LO2_LSB_UPPER_LIMIT) lo2sb_ = LO2SB::LOWER;
      else                             lo2sb_ = LO2SB::UPPER;
    }
  }

  cbp->lo2Sideband().setValue(lo2sb_);

  // There is a one-to-one mapping of these enumerations;
  //  upper, then lower
  obsRecord2::SidebandType corsb =
    static_cast<obsRecord2::SidebandType>(lo2sb_);
  SpectralDownconverterControl::SidebandType dcsb =
    static_cast<SpectralDownconverterControl::SidebandType>(lo2sb_);

  // Compute 2nd LO freq and store as MP
  // Iffreq is the center of the band in the IF.
  double lo2freq = iffreq_;
  if ( bdcE ) {
    lo2freq = BLOCK_DC_LO - lo2freq;
  }

  if (lo2sb_ == LO2SB::UPPER)
    // lo2 is below the IF frequency
    lo2freq -= BASEBAND_CENTER;
  else
    // lo2 is above the IF frequency
    lo2freq += BASEBAND_CENTER;

  // Debug log
  if (true) {
    ostringstream dbug;
    dbug  << setiosflags(ios::fixed)
      << "BAND=" << b+1
      << " IFfreq=" << setprecision(3) << iffreq_
      << " lo2freq=" << setprecision(6) << lo2freq
      << " dopfac=" << setprecision(3) << dopfac_
      << " frest=" <<  setprecision(6) << frest_
      << " fcenter=" << setprecision(6) << fcenter_
      << " loFreq=" << setprecision(6) << parent_->loFreq_
      << " sb=" << s1_
      //<< " sb2=" << s2
      << " transition=" << transition_
      << " imagerest=" <<  setprecision(6) << imagefrest_
      << " imagetransition=" << imageTransition_;
    Program::getLogger() << Priority::INFO << dbug.str();
  }

  // check band setting with respect to block DC
  BlockDownconverterControl::Block block = bdcE ? 
        BlockDownconverterControl::UPPER : 
        BlockDownconverterControl::LOWER;

  bool inBand = true;
  // Wideband correlator doesn't track 2nd LO, so don't
  // check block DC here.
  if ( corrset.isSpectral() ) {
      // bandwidth in GHz
      float bwGHz = bwmhz_ * 1E-3;
      inBand = parent_->isBandCompletelyInsideBlock(iffreq_, block, bwGHz);
      if ( !inBand ) {
        cbp->configuration().setValue(CONFIG::INVALID);
        ostringstream os;
        os << "Setting Band " << b+1 << " to INVALID "
           << "because it is not fully within a blockDC block" ;
        programLogInfoIfPossible( os.str() );
      }
  }

  // Check for valid LO2 freq
  bool invalidLo2 =  ((lo2freq < LO2_LOWER_LIMIT) || 
                      (lo2freq > LO2_UPPER_LIMIT)); 
  if ( invalidLo2 ) {
    cbp->configuration().setValue(CONFIG::INVALID);
    ostringstream os;
    os << "Setting Band " << b+1 << " to INVALID "
       << "because the LO2 frequency " << lo2freq << " is invalid" ;
    programLogInfoIfPossible( os.str() );
    // Fix LO2 to center of range if invalid
    //lo2freq = (LO2_LOWER_LIMIT + LO2_UPPER_LIMIT)/2.0;
    // No, Let the user see the bad value via RTD, just don't
    // send it to the correlators and SLDC
  }
  if (inBand && (!invalidLo2)) cbp->configuration().setValue(CONFIG::VALID);

  cbp->lo2Freq().setValue(lo2freq);

  //------------------------------------------------------------
  // Now send lo2 & lo2sb and sum of frequencies to this band's
  // correlator
  //------------------------------------------------------------

  if ( !invalidLo2 ) {
      const CorrelatorGroup correlatorGroup = getCorrelatorGroup(parent_, bandNo, type);

      WorkResultSet setDcFreqWrs("correlators::setDCsettings");
      queueFunctorWorkRequestGroup(
                       "Correlator_I::setDownconverterSettings()",
                       makeRemoteObjMethodFunctorGroup(
                                       correlatorGroup,
                                       "setDCsettings", "params",
                                       &Correlator_I::setDownconverterSettings, lo2freq, corsb, bdcE ),
                       setDcFreqWrs,
                       *(parent_->workerPool_) );
      // This previously had a 1.5s timeout but the command was completing
      // in 1.6-1.7s range
      const unsigned long timeoutMillis = 2000UL;
      waitForAllNormal( setDcFreqWrs, timeoutMillis);

      //------------------------------------------------------------
      // Now send lo2 & lo2sb and sum of frequencies to this band's
      // vlbi backends
      //------------------------------------------------------------

      const VlbiGroup vlbiGroup = getVlbiGroup(parent_, bandNo);
      if(!vlbiGroup.empty()) {

        WorkResultSet vlbi_setDcFreqWrs("vlbi::setDCsettings");
        queueFunctorWorkRequestGroup(
                   "Correlator_I::setDownconverterSettings()",
                   makeRemoteObjMethodFunctorGroup(
                           vlbiGroup,
                           "setDCsettings", "params",
                           &Correlator_I::setDownconverterSettings, lo2freq, corsb, bdcE ),
                   vlbi_setDcFreqWrs,
                   *(parent_->workerPool_) );

        try {
          waitForAllNormal( vlbi_setDcFreqWrs, 1500UL ); // 1.5 second timeout
        } catch(const ErrorException & e) {
          // Log error and move on
          e.log(log4cpp::Priority::ERROR);
        }
      }

      //------------------------------------------------------------
      // And send lo2 and lo2sb to the downconverters and the 2nd LO's
      // for spectral-line bands
      //------------------------------------------------------------

      if(   parent_->slDownconverter_.get() != 0 
         && corrset.isSpectral() ) 
      {
        parent_->slDownconverter_->setSlSidebandFrequency(dcsb, lo2freq, bandNo);
      }
  }// if !invalidLo2
}

void SubarrayControlImpl::
ConfigAstroBand::checkConfigurationSuccess()
{
  parent_->signalPathMapper_->checkConfigurationSuccess(astroBand_.bandNo_);
}


void SubarrayControlImpl::
astroBandOnline(::CORBA::Short astroBandNo, ::CORBA::Boolean online)
  try
  {
    ScopedLogNdc ndc("SubarrayControlImpl::astroBandOnline");

    const ControlCorrelatorDesignation cType = csCorrType();

    // hopeless kluge

    const ControlCorrelatorDesignation saCType = getCorrTypeForAstroBandNo(astroBandNo);
    const CorrelatorSet sacCorrSet (saCType);

    const CorrelatorSet corrSet(cType);
    if(!corrSet.includes(saCType) || sacCorrSet.isEmpty() ) {
      ThrowCarmaError("Astroband " << astroBandNo << " is not part of the " 
              << corrSet.corrTypeString() 
              <<  " correlator assigned to this subarray.");
    }

    /* this doesn't work because isValidAstroBand only iterates over astrobands
     * that are switchyard-configured.  So we can never find switchyard-unconfigured
     * astrobands this way
     *
    bool isAssignedToThisSubarray = signalPathMapper_->isValidAstroBand( astroBandNo, cType );
    bool isAssignedToNoSubarray = signalPathMapper_->isValidAstroBand( astroBandNo, util::CORR_NONE );
    bool isAssignedToAnotherSubarray = ( !isAssignedToThisSubarray && !isAssignedToNoSubarray );
    
    if ( isAssignedToAnotherSubarray ) {
        ostringstream os;
        os << "Astroband " 
           << astroBandNo << " is not part of the " 
           << getStringForCorrType(cType) 
           <<  " correlator assigned to this subarray."
           << " assigned to this subarray: "<<boolalpha << isAssignedToThisSubarray
           << " assigned to NO subarray: "<<boolalpha << isAssignedToNoSubarray
           << " assigned to Another subarray: "<<boolalpha << isAssignedToAnotherSubarray
           ;
        throw CARMA_ERROR( os.str() );
    }
    */

      typedef ControlBandPoints::Lo2SidebandMonitorPointEnum    LO2SB;
      typedef ControlBandPoints::CorrBitsMonitorPointEnum       CBITS;

      // note the default constructor band number for an AstroBand(N) is N.
      vector<SignalPathMapperControl::CorrelatorBand> bandVec 
          = signalPathMapper_->getCorrelatorBands(astroBandNo);
      short bandInUse = -1;
      // Need to cover the case of an unconfigured astroband whose default
      // correlator band (i.e. in LL mode) is currently part of another astroband
      // in e.g. FULLSTOKES mode.
      unsigned cbNo = astroBandNo;
      if ( bandVec.empty() ) {

          if ( sacCorrSet.isWideband() ) {
              // Correlator Band numbers start at 1 again for
              // the wideband correlator!
              cbNo -= 8;
          }

          if ( sacCorrSet.isC3gMax23() ) {
              // Correlator Band numbers start at 1 again for each correlator
              cbNo -= 24;
          }

          if ( sacCorrSet.isC3gMax8() ) {
              // Correlator Band numbers start at 1 again for each correlator
              cbNo -= 32;
          }

          // This call returns -1 if the correlator band is unassigned 
          // to any astroband
          bandInUse = signalPathMapper_->getAstroBandForCorrelatorBand( cbNo, saCType );
          if ( bandInUse > 0 ) {
              string abConf = signalPathMapper_->getConfname( bandInUse );
              ostringstream os;
              os << " There is currently no astroband " 
                 << astroBandNo 
                 << " because "
                 << corrSet.corrTypeString() 
                 << " correlator band "
                 << cbNo
                 << " is in use in astroband " << bandInUse
                 << " configured for " << abConf;
              throw CARMA_ERROR(os.str());
          }
      }

      // If the bandVec is empty, that means this astroband has no switchyard
      // configuration. We want to turn it online or offline regardless, so
      // make the assumption that correlator band number = astro band number.
      // This is required, otherwise we can never mark an un-switchyard configured
      // astroband online and further more can't configastroband it, since that
      // skips astrobands that are not online.  Chicken meet egg.
      // THIS IS STILL WRONG BECAUSE EG IN DUAL POL, THERE IS NO ASTROBAND 4 AND
      // WE DONT WANT TO SET CORRELATORBAND4 OFFLINE IN THAT CASE.
      vector<short> bandIndex;
      if ( bandVec.empty() && bandInUse == -1 ) {
          bandIndex.push_back( cbNo - 1 );
      } else {
          for(unsigned bIdx=0; bIdx < bandVec.size(); bIdx++) {
              bandIndex.push_back(bandVec.at(bIdx).bandNo - 1);
          }
      }

      // Now loop over the band indexes and set the corr description string
      // and CorrelatorHandle online value.
      for(unsigned bIdx=0; bIdx < bandIndex.size(); bIdx++) {
          ControlBandPoints* cbp  = 0;
          const short b = bandIndex.at(bIdx);

          // Do the switch on the actual correlator type assigned to this
          // astroband, since cType may be multivalued if multiple correlators
          // are in this subarray.
          if ( sacCorrSet.isSpectral() ) {
                cbp = &controlSubsystem_.spectralLineCorrelator().
                  slcBand(b).controlBandPoints();
          }
          if ( sacCorrSet.isWideband() ) {
                cbp = &controlSubsystem_.widebandCorrelator().
                  wbcBand(b).controlBandPoints();
          }
          if ( sacCorrSet.isC3gMax8() ) {
                cbp = &controlSubsystem_.c3gMax8Correlator().
                  c3gMax8Band(b).controlBandPoints();
          }
          if ( sacCorrSet.isC3gMax23() ) {
                cbp = &controlSubsystem_.c3gMax23Correlator().
                  c3gMax23Band(b).controlBandPoints();
          }


          // Set the online_ member variable of the ConfigAstroBand class
          // associated with this astroband to the new value.
          if ( cabmap_[astroBandNo].get() != 0 ) 
              cabmap_[astroBandNo]->online_ = online;

          // Mark the correlator band online or offline via its CorrelatorHandle.
          // Don't attempt to FPGA mode here because if the astroband is currently
          // unassociated with a switchyard configuration, then this call will not
          // throw an exception when it attempts to read the FPGA configuration.
          if ( cabmap_[astroBandNo].get() != 0 ) 
              cabmap_[astroBandNo]->updateCorrelator(false);

          // Read the correlator mode description string
          string bwstring("X");
          if ( cabmap_[astroBandNo].get() != 0 ) 
              bwstring = cabmap_[astroBandNo]->corrmodeStream_.str();

          if ( sacCorrSet.isSpectral() ) {
            // offline bands get an X
            if( !online )
              slcmodeVec_.at(b) = "X";
            else 
              slcmodeVec_.at(b) = bwstring;
          }
          if ( sacCorrSet.isWideband() ) {
            if( ! online )
              wbcmodeVec_.at(b) = "X";
            else
              wbcmodeVec_.at(b) = bwstring;
          }
          if ( sacCorrSet.isC3gMax8() ) {
            if( ! online )
              c3gMax8modeVec_.at(b) = "X";
            else
              c3gMax8modeVec_.at(b) = bwstring;
          }
          if ( sacCorrSet.isC3gMax23() ) {
            if( ! online )
              c3gMax23modeVec_.at(b) = "X";
            else
              c3gMax23modeVec_.at(b) = bwstring;
          }

          cbp->online().setValue( online );
      } // for

      string desc;

      std::vector<ControlCorrelatorDesignation> corrs = corrSet.getControlCorrelatorDesignations();
      for(unsigned iCorr=0; iCorr < corrs.size(); iCorr++) {
        desc = makeCorrModeDescString(corrs[iCorr]);
        getModeDescMp(corrs[iCorr]).setValue(desc);
      }

      markStateChange(); 
    
  } catch ( ... ) {
      rethrowCaughtAsUser();
  }

//-----------------------------------------------------------------------
// clearAstroBand()
//-----------------------------------------------------------------------
void SubarrayControlImpl::
clearAstroBand(::CORBA::Short astroBandNo)
  try
  {
    ScopedLogNdc ndc("SacI:clearAstroBand");
    cmdlog() << "clearAstroBand( astroBandNo=" << astroBandNo << ")";

    std::vector<carma::signalpath::SignalPathMapperControl::AstroBand> astroBands;

    // If all astrobands for this correlator (0) was specified, first
    // retrieve the vector of astrobands currently configured, so we
    // can tell the corrDataRemapper to clear them

    if(astroBandNo == 0) {
      astroBands = signalPathMapper_->getActiveAstroBands(csCorrType());

      //  Remove all entries from the ConfigAstroBandMap.
      //  mutex protect access to the map with a scoped
      //  lock.  The scope is the curly braces of this if-statement.
      const ScopedLock< ::pthread_mutex_t > lock(gCabMapGuard_);
      cabmap_.clear();
    } else {
      astroBands.resize(1);
      astroBands[0].astroBandNo = astroBandNo;
      {
          // Remove the relevant entry from the map.
          // Again, mutex protect map operations.
          const ScopedLock< ::pthread_mutex_t > lock(gCabMapGuard_);
          if ( cabmap_.find(astroBandNo) != cabmap_.end() )
              cabmap_.erase( cabmap_.find(astroBandNo) );
      }
    }

    // If returned size is zero, we are already cleared.
    if ( astroBands.size() == 0 ) {
        programLogInfoIfPossible("astrobands are already cleared");
        return;
    }

    signalPathMapper_->clearAstroBandConfiguration(astroBandNo, subarrayNo_, csCorrType());

    if ( corrDataRemapper_.get() != 0 ) {
        for(unsigned iAstroBand=0; iAstroBand < astroBands.size(); iAstroBand++)
        {
          corrDataRemapper_->clearAstroBandInputMap(astroBands[iAstroBand].astroBandNo);
        }
    } else {
      programLogWarnIfPossible("CorrDataRemapperHandle is uninitialized.");
    }

  } catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser();
  }

//-----------------------------------------------------------------------
// configAstroBand()
//
// Note: sb is sideband of the 2nd LO downconversion
//
//-----------------------------------------------------------------------
void SubarrayControlImpl::
configAstroBand(
        ::CORBA::Short astroBandNo,
        const char* astroBandConf,
        ::carma::util::CorrelatorBandWidthType bandwidth,
        ::CORBA::Double fcenter,
        ::carma::control::SidebandType sb,
        ::CORBA::Double frest,
        ::CORBA::Double imagefrest,
        ::CORBA::Boolean online,
        const char* transition,
        const char* imageTransition,
        ::carma::util::CorrelatorBitType  bits)
  try
  {
    //------------------------------------------------------------
    // Initialize the ConfigAstroBand helper class
    //------------------------------------------------------------

    configab_ptr p(new ConfigAstroBand(this,
                          astroBandNo,
                          astroBandConf,
                          bandwidth,
                          fcenter,
                          sb,
                          frest,
                          imagefrest,
                          online,
                          transition,
                          imageTransition,
                          bits)
                 );

    ScopedLogNdc ndc("configAstroband");
    //------------------------------------------------------------
    // Now check input arguments
    //------------------------------------------------------------
    p->checkInputArguments();

    // Update everything except LO2
    p->finishSetupAfterInputsValidated();

    {
        // Insert or replace this configuration in the global map
        // Mutex protect access to the map.
        const ScopedLock< ::pthread_mutex_t > lock(gCabMapGuard_);
        if ( cabmap_.find(astroBandNo) == cabmap_.end() )
            cabmap_.insert(std::make_pair<unsigned,configab_ptr>(astroBandNo,p));
        else
            cabmap_[astroBandNo] = p;
    }

    // Update second LO frequencies at the very end, to ensure freshness

    p->updateLo2Frequency();


  } catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser();
  }

void SubarrayControlImpl::ConfigAstroBand::
finishSetupAfterInputsValidated()
try {
    ScopedLogNdc ndc("finishSetupAfterInputsValidated");
    //------------------------------------------------------------
    // Set the doppler factor and sideband for anything that needs it
    //------------------------------------------------------------
    if ((parent_->restFreq_ < 1) || (parent_->skyFreq_ < 1)) {
        dopfac_ = 1.0;
    }
    else {
        dopfac_ = parent_->skyFreq_/parent_->restFreq_;
    }
    if (dopfac_ < 0.001) dopfac_ = 1.0;
    if (fcenter_ > parent_->loRestFreq_) {
        s1_ =  1;
    }
    else  {
        s1_ = -1;
    }
    
    //------------------------------------------------------------
    // Parse the bandwidth
    //------------------------------------------------------------

    parseBandwidth();

    //------------------------------------------------------------
    // Log the command
    //------------------------------------------------------------

    logCommand();

    //------------------------------------------------------------
    // Assert the requested mapping
    //------------------------------------------------------------

    assertAstroBandConfiguration();

    //------------------------------------------------------------
    // Update the correlator data remapper
    //------------------------------------------------------------

    updateCorrDataRemapper();

    //------------------------------------------------------------
    // Update correlator handles associated with this astroband
    //------------------------------------------------------------

    updateCorrelator(true);

    //------------------------------------------------------------
    // Update correlator band monitor points
    //------------------------------------------------------------

    setupCorrMps();

    //------------------------------------------------------------
    // Update downconverter filters
    //------------------------------------------------------------

    selectFilter();

    //------------------------------------------------------------
    // Update the block downconverter settings for all bands.
    // This must be called BEFORE updateLo2Frequency
    //------------------------------------------------------------

    updateBlockDownconverter();

    // Set up correlators

    // C3G bands are done all at once by assertCorrConfig, not here.
    // Note: the assumption is that C3G astro band configuration strings
    // always have "C3G" in them!
    if ( astroBandConf_.find("C3G") != string::npos ) return;

    setupCorrelatorBandwidth();

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser();
}

util::CorrelatorFpgaModeType
SubarrayControlImpl::ConfigAstroBand::getFpgaMode() const {
    return parent_->signalPathMapper_->getFpgaMode(astroBand_.bandNo_);
}

