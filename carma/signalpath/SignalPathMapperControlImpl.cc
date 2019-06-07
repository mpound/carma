#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/CorrDesignation.h"
#include "carma/monitor/LoberotatorSubsystem.h"
#include "carma/monitor/SldcSubsystem.h"
#include "carma/monitor/WbdcSubsystem.h"

#include "carma/signalpath/SignalPathMapperControlImpl.h"

#include "carma/util/corbaSequenceUtils.h"
#include "carma/util/CorrelatorSet.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/UserException.h"

#include "carma/szautil/Exception.h"

#include <vector>

using namespace carma::downconverter;
using namespace carma::signalpath;
using namespace carma::switchyard;
using namespace carma::util;
using namespace carma;

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Define a wrapper for all locking CORBA calls.  This locks a mutex,
 * then instantiates a scoped lock within the try clause, so that the
 * mutex is unlocked if the call successfully exits scope, and
 * explicitly unlocks in the catch clause if the call fails.  
 *
 * Rethrows as an over-the-wire exception
 */
#define THROW_BLOCK(errStr)						\
  {									\
    guard_.UnlockNoThrow();						\
    os << " " << errStr;						\
    throw CARMA_EXCEPTION(carma::util::UserException, os.str().c_str()); \
  }

#define CORBA_WRAPPER(cmd)						\
  {									\
    sza::util::TimeVal macroTimeVal;					\
    macroTimeVal.setToCurrentTime();					\
    ostringstream os;							\
    os << macroTimeVal << " SignalPathMapperControlImpl::" << __FUNCTION__; \
    ScopedLogNdc ndc(os.str().c_str());					\
    try {								\
      ScopedPthreadMutexLock spml(guard_);				\
      cmd;								\
    } catch(sza::util::Exception& err) {				\
      THROW_BLOCK(err.what());						\
    } catch(carma::util::UserException& err) {				\
      THROW_BLOCK(err.errorMsg);					\
    } catch(carma::util::ErrorException& err) {				\
      THROW_BLOCK(err.what());						\
    } catch(...) {							\
      THROW_BLOCK("Unhandled exception");				\
    }									\
  }

/**.......................................................................
 * Do-nothing constructor
 */
SignalPathMapperControlImpl::SignalPathMapperControlImpl() 
{
  initialize();
}
    
/**.......................................................................
 * Do-nothing destructor
 */
SignalPathMapperControlImpl::~SignalPathMapperControlImpl() 
{
  if(writeMonitorSystemTimer_) {
    delete writeMonitorSystemTimer_;
    writeMonitorSystemTimer_ = 0;
  }

  if(checkConfigTimer_) {
    delete checkConfigTimer_;
    checkConfigTimer_ = 0;
  }

  if(ms_) {
    delete ms_;
    ms_ = 0;
  }

  if(msSp_) {
    delete msSp_;
    msSp_ = 0;
  }

  for(unsigned i=0; i < knownConfigurations_.size(); i++) {
    delete knownConfigurations_[i];
  }

}

/**.......................................................................
 * Initialize the cable map from a file
 */
void SignalPathMapperControlImpl::
initializeCableMap(std::string fileName)
{
  CORBA_WRAPPER(
		spMap_.initializeCableMap(fileName);
		spMapChecker_.initializeCableMap(fileName);
		);
}

/**.......................................................................
 * Load a configuration from a file
 */
void SignalPathMapperControlImpl::
loadConfiguration(std::string fileName, std::string confName, std::string astroBandConfName)
{
  CORBA_WRAPPER(
		spMap_.loadConfiguration(fileName, confName, astroBandConfName);
		spMapChecker_.loadConfiguration(fileName, confName, astroBandConfName);
		);
}

/**.......................................................................
 * Configure an astro band
 */
void SignalPathMapperControlImpl::
configureAstroBand(unsigned short bandNo, std::string confName, 
		   unsigned short subarrayNo, util::CorrelatorType type)
{
  CORBA_WRAPPER(

		// Store the 'current' frame count
		COUT("Here 0");
		int frameCount = getFrameCount();

		// Store the vector of correlator bands associated with this astroband

		COUT("Here 1");
		saveCurrentConfiguration(bandNo, type);
		spMap_.clearAstroBandConfiguration(bandNo, subarrayId(subarrayNo), correlatorType(type));
		markSavedConfigurationAsModified(frameCount);

		// Now assert the new configuration

		COUT("Here 2");
		spMap_.configureAstroBand(bandNo, confName, subarrayId(subarrayNo), correlatorType(type));
		saveCurrentConfiguration(bandNo, type);
		markSavedConfigurationAsModified(frameCount);

		// Mark this astroband hardware configuration as valid for the moment

		COUT("Here 3");
		markHardwareConfigurationAsValid(bandNo, true);

		// Finally, update the monitor points

		COUT("Here 4");
		updateMonitorPoints();
		COUT("Here 5");
		// And enable the configuration checker

		enableConfigurationChecker(true);
		COUT("Here 6");
		);
}

/**.......................................................................
 * Check a configuration for validity (throws if not valid)
 */
void SignalPathMapperControlImpl::
checkConfigurationValidity(unsigned short bandNo, std::string confName, 
			   unsigned short subarrayNo, util::CorrelatorType type)
{
  CORBA_WRAPPER(
		spMap_.checkAstroBandConfiguration(bandNo, confName, subarrayId(subarrayNo), correlatorType(type))
		);
}

/**.......................................................................
 * Return the validity of a configuration.  Returns false if not valid
 */
bool SignalPathMapperControlImpl::
configurationIsValid(unsigned short bandNo, std::string confName, 
		     unsigned short subarrayNo, util::CorrelatorType type)
{
  CORBA_WRAPPER(
		return spMap_.astroBandConfigurationIsValid(bandNo, confName, subarrayId(subarrayNo), correlatorType(type));
		);
}

int SignalPathMapperControlImpl::getFrameCount()
{
  // do not do a blocking read here.
  ms_->readNewestConditionalCopy();
  return ms_->getFrameCount();
}

/**.......................................................................
 * Save the list of astrobands and correlator bands currently
 * configured
 */
void SignalPathMapperControlImpl::
saveCurrentConfiguration(unsigned astroBandNo, util::CorrelatorType type)
{
  savedAstroBands_      = getActiveAstroBands(astroBandNo, type);
  savedCorrelatorBands_ = getActiveCorrelatorBands(astroBandNo, type);
}

/**.......................................................................
 * Flag the list of saved astro bands and correlator bands as having
 * been modified
 */
void SignalPathMapperControlImpl::
markSavedConfigurationAsModified(int frameCount)
{
  if(savedCorrelatorBands_.size() > 0) {

    for(unsigned iCorrBand=0; iCorrBand < savedCorrelatorBands_.size(); iCorrBand++) {
      markCorrelatorBandAsModified(savedCorrelatorBands_[iCorrBand], frameCount);
    }

    for(unsigned iAstroBand=0; iAstroBand < savedAstroBands_.size(); iAstroBand++) {
      markAstroBandAsModified(savedAstroBands_[iAstroBand], frameCount);
    }

  }
}

/**.......................................................................
 * Return the list of astro bands currently configured
 */
std::vector<unsigned> SignalPathMapperControlImpl::
getActiveAstroBands(unsigned astroBandNo, util::CorrelatorType type)
{
  std::vector<unsigned> nos;
  if(astroBandNo == 0) {
    nos =  spMap_.getActiveAstroBandNos(correlatorType(type));
  } else {
    nos.push_back(astroBandNo);
  }
  return nos;
}

/**.......................................................................
 * Return the list of correlator bands currently configured
 */
std::vector<carma::signalpath::CorrelatorBandSpec> SignalPathMapperControlImpl::
getActiveCorrelatorBands(unsigned astroBandNo, util::CorrelatorType type)
{
  if(astroBandNo == 0) {
    return spMap_.getActiveCorrelatorBands(correlatorType(type));
  } else {
    return spMap_.getCorrelatorBands(astroBandNo);
  }
}

/**.......................................................................
 * Clear any configuration of the specified astro band
 */
void SignalPathMapperControlImpl::
clearAstroBandConfiguration(unsigned short bandNo, unsigned short subarrayNo, util::CorrelatorType type)
{
  CORBA_WRAPPER(
		clearAstroBandConfigurationLocal(bandNo, subarrayNo, type);	     
		);
}

/**.......................................................................
 * Clear any configuration of the specified astro band
 */
void SignalPathMapperControlImpl::
clearAstroBandConfigurationLocal(unsigned short bandNo, unsigned short subarrayNo, util::CorrelatorType type)
{
  saveCurrentConfiguration(bandNo, type);
  spMap_.clearAstroBandConfiguration(bandNo, subarrayId(subarrayNo), correlatorType(type));
  markSavedConfigurationAsModified(getFrameCount());
  
  // Mark this astroband hardware configuration as valid for the moment
  
  markHardwareConfigurationAsValid(bandNo, true);
  
  // And update other monitor points
  
  updateMonitorPoints();
  
  // Check if this command left us with no astrobands currently
  // configured.  If so, disable the configuration checker
  
  std::vector<unsigned> astroBandNos = spMap_.getActiveAstroBandNos(CORR_ALL);
  
  if(astroBandNos.size() == 0) {
    enableConfigurationChecker(false);
  }
}

/**.......................................................................
 * Set the walsh column explicitly for a single antenna
 */
void SignalPathMapperControlImpl::
assignWalshColumn(SignalPathMapperControl::WalshColumnAssignment wca)
{
  CORBA_WRAPPER(
		spMap_.setWalshColumn(spMap_.antNoToAntName(wca.antNo), wca.walshColNo);
		updateMonitorPoints();
		);
}

/**.......................................................................
 * Set the walsh column explicitly for a single antenna
 */
void SignalPathMapperControlImpl::
clearWalshColumnAssignment(unsigned short antNo)
{
  CORBA_WRAPPER(
		spMap_.clearWalshColumn(spMap_.antNoToAntName(antNo));
		updateMonitorPoints();
		);
}

/**.......................................................................
 * Get the IF switch positions for the current configuration
 */
carma::switchyard::SwitchPositionSeq* SignalPathMapperControlImpl::
getIFSwitchSettings(unsigned short astroBandNo)
{
  CORBA_WRAPPER(
		std::vector<SwitchSetting> swSetVec;

		swSetVec = spMap_.getIFSwitchSettings(astroBandNo);

		SwitchPositionSeq* swSetSeq = new SwitchPositionSeq(swSetVec.size());
		(*swSetSeq).length(swSetVec.size());

		for(unsigned i=0; i < swSetVec.size(); i++) {
		  (*swSetSeq)[i].switchNo  = swSetVec[i].switchNo_;
		  (*swSetSeq)[i].switchPos = SignalPathMap::switchChannelIdToChannelNumber(swSetVec[i].channel_);
		}
    
		return swSetSeq;
		);
}

/**.......................................................................
 * Get the DC LO switch positions for the current configuration
 */
std::vector<carma::signalpath::SwitchSetting> SignalPathMapperControlImpl::
getDCLOSwitchSettingsLocal(unsigned short astroBandNo)
{
  std::vector<SwitchSetting> swSetVec;
  SwitchSetting swSet;
  
  // First get the FPGA mode for this astroband
  
  carma::util::CorrelatorFpgaModeType fpgaMode = getFpgaModeLocal(astroBandNo);
  
  // Now get the vector of correlator bands associated with this astroband
  
  std::vector<carma::signalpath::CorrelatorBandSpec> cVec;
  
  cVec = spMap_.getCorrelatorBands(astroBandNo);
  
  // Iterate over bands
  
  for(unsigned iCorrBand=0; iCorrBand < cVec.size(); iCorrBand++) {
    
    unsigned bandNo                            = cVec[iCorrBand].bandNo_;
    carma::signalpath::CorrelatorType corrType = cVec[iCorrBand].crate_.type_;
    
    if(corrType == carma::signalpath::CORR_SL && 
       bandNo <= carma::signalpath::BlockDownconverter::nBandSl_ 
       && bandNo%2==0) {
      
      swSet.switchNo_ = bandNo/2;
      swSet.channel_  = (fpgaMode == carma::util::CORR_SINGLEPOL) ? 
	carma::signalpath::SW_CHAN_2 : carma::signalpath::SW_CHAN_1;
      swSetVec.push_back(swSet);
    }
  }
  
  return swSetVec;
}

/**.......................................................................
 * Get the DC LO switch positions for the current configuration
 */
carma::switchyard::SwitchPositionSeq* SignalPathMapperControlImpl::
getDCLOSwitchSettings(unsigned short astroBandNo)
{
  CORBA_WRAPPER(
		std::vector<SwitchSetting> swSetVec = getDCLOSwitchSettingsLocal(astroBandNo);

		SwitchPositionSeq* swSetSeq = new SwitchPositionSeq(swSetVec.size());
		(*swSetSeq).length(swSetVec.size());

		for(unsigned i=0; i < swSetVec.size(); i++) {
		  (*swSetSeq)[i].switchNo  = swSetVec[i].switchNo_;
		  (*swSetSeq)[i].switchPos = SignalPathMap::switchChannelIdToChannelNumber(swSetVec[i].channel_);
		}

		return swSetSeq;
		);
}

/**.......................................................................
 * Get the LO switch positions for the current configuration
 */
carma::switchyard::SwitchPositionSeq* SignalPathMapperControlImpl::
getLOSwitchSettings(unsigned short astroBandNo)
{
  CORBA_WRAPPER(
		std::vector<AntennaSpec> antVec;

		antVec = spMap_.getAntennas(astroBandNo, false);

		SwitchPositionSeq* swSetSeq = new SwitchPositionSeq(antVec.size());
		(*swSetSeq).length(antVec.size());

		for(unsigned i=0; i < antVec.size(); i++) {
		  (*swSetSeq)[i].switchNo  = antVec[i].antNo_;
		  (*swSetSeq)[i].switchPos = switchPos(antVec[i].subarrayId_);
		}
    
		return swSetSeq;
		);
}

/**.......................................................................
 * Get the LL switch positions for the current configuration
 */
carma::switchyard::SwitchPositionSeq* SignalPathMapperControlImpl::
getLLSwitchSettings(unsigned short astroBandNo)
{
  return getLOSwitchSettings(astroBandNo);
}

/**.......................................................................
 * Associate an antenna with a subarray
 */
void SignalPathMapperControlImpl::addAntenna(unsigned short antNo, unsigned short subarrayNo)
{
  CORBA_WRAPPER(
		spMap_.addAntenna(antNo, subarrayId(subarrayNo));
		spMapChecker_.addAntenna(antNo, subarrayId(subarrayNo));
		updateMonitorPoints();
		);
}

/**.......................................................................
 * Disassociate an antenna from a subarray
 */
void SignalPathMapperControlImpl::removeAntenna(unsigned short antNo, unsigned short subarrayNo)
{
  CORBA_WRAPPER(
		spMap_.removeAntenna(antNo, subarrayId(subarrayNo));
		spMapChecker_.removeAntenna(antNo, subarrayId(subarrayNo));
		updateMonitorPoints();
		);
}

/**.......................................................................
 * Associate a correlator with a subarray
 */
void SignalPathMapperControlImpl::addCorrelator(util::CorrelatorType type, unsigned short subarrayNo)
{
  //ScopedLogNdc ndc("MWP SignalPathMapperControlImpl::addCorrelator");
  CORBA_WRAPPER(

		CorrelatorSet corrSet(type);
		if(!corrSet.isEmpty() && !corrSet.isSingleCorrelator()) {
		  std::vector<util::CorrelatorType> corrs = corrSet.getControlCorrelatorDesignations();
		  for(unsigned iCorr=0; iCorr < corrs.size(); iCorr++) {
		    type = corrs[iCorr];
		    /* debug - mwp
		       {
		       ostringstream os;
		       os << "adding (type="<<corrSet.corrTypeString()<<",subarrayNo="<<subarrayNo<<")";
		       programLogNotice(os.str());
		       }
		    */
		    addCorrelatorLocal(type, subarrayNo);
		  }
		} else {
		  /* debug - mwp
		     {
		     ostringstream os;
		     os << "adding (type="<<corrSet.corrTypeString()<<",subarrayNo="<<subarrayNo<<")";
		     programLogNotice(os.str());
		     }
		  */
		  addCorrelatorLocal(type, subarrayNo);
		}

		);
}

/**.......................................................................
 * Associate a correlator with a subarray
 */
void SignalPathMapperControlImpl::addCorrelatorLocal(util::CorrelatorType type, unsigned short subarrayNo)
{
  spMap_.addCorrelator(correlatorType(type), subarrayId(subarrayNo));
  spMapChecker_.addCorrelator(correlatorType(type), subarrayId(subarrayNo));
  CARMALOGINFO("updating MPs");
  updateMonitorPoints();
}

/**.......................................................................
 * Disassociate a correlator from a subarray
 */
void SignalPathMapperControlImpl::removeCorrelator(util::CorrelatorType type, unsigned short subarrayNo)
{
  //ScopedLogNdc ndc("MWP SignalPathMapperControlImpl::removeCorrelator");
  CORBA_WRAPPER(

		CorrelatorSet corrSet(type);
		if(!corrSet.isEmpty() && !corrSet.isSingleCorrelator()) {
		  std::vector<util::CorrelatorType> corrs = corrSet.getControlCorrelatorDesignations();
		  for(unsigned iCorr=0; iCorr < corrs.size(); iCorr++) {
		    type = corrs[iCorr];
		    /* debug - mwp
		       {
		       ostringstream os;
		       os << "removing (type="<<type<<",subarrayNo="<<subarrayNo<<")";
		       programLogNotice(os.str());
		       }*/
		    removeCorrelatorLocal(type, subarrayNo);
		  }
		} else {
		  /* debug - mwp {
		     ostringstream os;
		     os << "removing (type="<<type<<",subarrayNo="<<subarrayNo<<")";
		     programLogNotice(os.str());
		     } */
		  removeCorrelatorLocal(type, subarrayNo);
		}

		);
}

/**.......................................................................
 * Disassociate a correlator from a subarray
 */
void SignalPathMapperControlImpl::removeCorrelatorLocal(util::CorrelatorType type, unsigned short subarrayNo)
{
  // We need to store the currently configured astrobands associated
  // with this correlator.  Even though the SPM will clear these
  // bands automatically under the hood, if the request to remove a
  // correlator is successful, we also need to call
  // clearAstroBandConfiguration() so that the monitor points are
  // updated correctly.
  
  std::vector<unsigned> astroBandNos = spMap_.getActiveAstroBandNos(correlatorType(type));
  
  if(spMap_.removeCorrelator(correlatorType(type), subarrayId(subarrayNo))) {
    
    spMapChecker_.removeCorrelator(correlatorType(type), subarrayId(subarrayNo));
    
    for(unsigned iBand=0; iBand < astroBandNos.size(); iBand++) {
      clearAstroBandConfigurationLocal(astroBandNos[iBand], subarrayNo, type);
    }
  }
  
  updateMonitorPoints();
}

/**.......................................................................
 * IDL interface call
 */
carma::util::CorrelatorFpgaModeType SignalPathMapperControlImpl::getFpgaMode(unsigned short astroBandNo)
{
  CORBA_WRAPPER(
		return getFpgaModeLocal(astroBandNo);
		);
}

/**.......................................................................
 * Local call to get the FPGA mode
 */
carma::util::CorrelatorFpgaModeType SignalPathMapperControlImpl::getFpgaModeLocal(unsigned short astroBandNo)
{
  carma::signalpath::AstroBand*               astroBand = spMap_.getAstroBand(astroBandNo);
  carma::signalpath::SwitchyardConfiguration* swConf    = astroBand->getSwitchyardConfiguration();
  carma::signalpath::AstroBandConfiguration*  abConf    = swConf->getAstroBandConfiguration();

  return astroBandConfToCorrFpgaMode(swConf->name_, abConf->name_);
}

/**.......................................................................
 * Get the antennas associated with the requested astroband
 *
 * Note that we default to return only those antennas in the AB
 * configuration that are also currently controlled by the subarray
 * that configured this astroband.
 */
SignalPathMapperControl::AntennaSeq* SignalPathMapperControlImpl::
getAntennas(unsigned short astroBandNo)
{
  CORBA_WRAPPER(

		std::vector<carma::signalpath::AntennaSpec> antVec;
		antVec = spMap_.getAntennas(astroBandNo, false);

		SignalPathMapperControl::AntennaSeq* antSeq = 
		new SignalPathMapperControl::AntennaSeq(antVec.size());
		(*antSeq).length(antVec.size());

		for(unsigned i=0; i < antVec.size(); i++) {
		  (*antSeq)[i].antNo      = antVec[i].antNo_;
		  (*antSeq)[i].type       = antType(antVec[i].type_);
		  (*antSeq)[i].walshColNo = antVec[i].walshColNo_;
		  (*antSeq)[i].subarrayNo = subarrayNumber(antVec[i].subarrayId_);
		}
    
		return antSeq;
		);
}

/**.......................................................................
 * Get the polarizations associated with the requested astroband.
 *
 * Here we default to return all unique polarization states that CAN
 * occur in this astroband configuration, regardless of whether all
 * antennas associated with this AB configuration are currently in the
 * controlling subarray.
 */
SignalPathMapperControl::PolarizationSeq* SignalPathMapperControlImpl::
getPolarizations(unsigned short astroBandNo)
{
  CORBA_WRAPPER(

		std::vector<carma::signalpath::PolarizationType> polVec;
		polVec = spMap_.getPolarizations(astroBandNo, true);

		SignalPathMapperControl::PolarizationSeq* polSeq = 
		new SignalPathMapperControl::PolarizationSeq(polVec.size());
		(*polSeq).length(polVec.size());

		for(unsigned i=0; i < polVec.size(); i++) {
		  (*polSeq)[i] = polType(polVec[i]);
		}
    
		return polSeq;
		);
}

SignalPathMapperControl::AntennaType SignalPathMapperControlImpl::
antType(carma::signalpath::AntennaType type) 
{
  switch (type) {
  case carma::signalpath::ANT_SZA:
    return SignalPathMapperControl::ANT_SZA;
    break;
  case carma::signalpath::ANT_BIMA:
    return SignalPathMapperControl::ANT_BIMA;
    break;
  case carma::signalpath::ANT_OVRO:
    return SignalPathMapperControl::ANT_OVRO;
    break;
  default:
    return SignalPathMapperControl::ANT_NONE;
    break;
  }
}

/**.......................................................................
 * Get the block downconverter settings for the current configuration
 */
SignalPathMapperControl::BlockDownconverterSettingSeq* SignalPathMapperControlImpl::
getBdcSettings(unsigned short astroBandNo)
{
  CORBA_WRAPPER(

		std::vector<carma::signalpath::BlockDownconverterSetting> bdcSetVec;
		bdcSetVec = spMap_.getBdcSettings(astroBandNo);

		SignalPathMapperControl::BlockDownconverterSettingSeq* bdcSetSeq = 
		new SignalPathMapperControl::BlockDownconverterSettingSeq(bdcSetVec.size());
		(*bdcSetSeq).length(bdcSetVec.size());

		for(unsigned i=0; i < bdcSetVec.size(); i++) {
		  (*bdcSetSeq)[i].bdcNo         = bdcSetVec[i].bdcNo_;
		  (*bdcSetSeq)[i].bandNo        = bdcSetVec[i].bandNo_;
		  (*bdcSetSeq)[i].bdcInputType = (bdcSetVec[i].input_ == BD_INP_P1 ? 
						  BlockDownconverterControl::POLARIZATION_1 : 
						  BlockDownconverterControl::POLARIZATION_2);
		}
    
		return bdcSetSeq;
		);
}

/**.......................................................................
 * Get the walsh column assignments for all antennas
 */
SignalPathMapperControl::WalshColumnAssignmentSeq* SignalPathMapperControlImpl::
getWalshColumnAssignment(unsigned short antNo)
{
  CORBA_WRAPPER(

		std::vector<carma::signalpath::WalshColumnAssignment> wcVec;
		wcVec = spMap_.getWalshColumnAssignment(antNo);

		SignalPathMapperControl::WalshColumnAssignmentSeq* wcSeq = 
		new SignalPathMapperControl::WalshColumnAssignmentSeq(wcVec.size());
		(*wcSeq).length(wcVec.size());

		for(unsigned i=0; i < wcVec.size(); i++) {
		  (*wcSeq)[i].antNo      = wcVec[i].antNo_;
		  (*wcSeq)[i].walshColNo = wcVec[i].walshColNo_;
		}
    
		return wcSeq;
		);
}

/**.......................................................................
 * Query active astro bands managed by this correlator
 */
SignalPathMapperControl::AstroBandSeq* SignalPathMapperControlImpl::
getActiveAstroBands(util::CorrelatorType type)
{
  CORBA_WRAPPER(

		std::vector<unsigned> astroBandVec;
		astroBandVec = spMap_.getActiveAstroBandNos(correlatorType(type));
  
		SignalPathMapperControl::AstroBandSeq* astroBandSeq = 
		new SignalPathMapperControl::AstroBandSeq(astroBandVec.size());

		(*astroBandSeq).length(astroBandVec.size());

		for(unsigned i=0; i < astroBandVec.size(); i++) {
		  (*astroBandSeq)[i].astroBandNo = astroBandVec[i];
		}
    
		SignalPathMapperControl::AstroBandSeq_var  abvar( astroBandSeq );
		return abvar._retn();
		) 
    }

SignalPathMapperControl::AstroBandSeq* SignalPathMapperControlImpl::
getAstroBandsForConfiguration(std::string confName,
			      unsigned short subarrayNo, 
			      util::CorrelatorType type)
{
  CORBA_WRAPPER(

		// First, clear all existing astroband configurations

		spMapChecker_.clearAstroBandConfiguration(0, subarrayId(subarrayNo), correlatorType(type));

		std::vector<unsigned> astroBandVec;
		astroBandVec = spMapChecker_.getAstroBandNosForConfiguration(confName, 
									     subarrayId(subarrayNo),
									     correlatorType(type));
  
		SignalPathMapperControl::AstroBandSeq* astroBandSeq = 
		new SignalPathMapperControl::AstroBandSeq(astroBandVec.size());

		(*astroBandSeq).length(astroBandVec.size());

		for(unsigned i=0; i < astroBandVec.size(); i++) {
		  (*astroBandSeq)[i].astroBandNo = astroBandVec[i];
		}
    
		SignalPathMapperControl::AstroBandSeq_var  abvar( astroBandSeq );
		return abvar._retn();
		) 
    }


/**.......................................................................
 * Query active correlator bands managed by this correlator
 */
SignalPathMapperControl::CorrelatorBandSeq* SignalPathMapperControlImpl::
getActiveCorrelatorBands(util::CorrelatorType type)
{
  /// debug -- mwp
  /*
    ScopedLogNdc ndc( "SignalPathMapperControlImpl::getActiveCorrelatorBands");
    {
    ostringstream os;
    os << "(" << type << ") - Entering ";
    programLogInfoIfPossible( os.str() );
    o    }
  */

  CORBA_WRAPPER(

		std::vector<carma::signalpath::CorrelatorBandSpec> cVec;
		cVec = spMap_.getActiveCorrelatorBands(correlatorType(type));
  
		SignalPathMapperControl::CorrelatorBandSeq* cSeq = 
		new SignalPathMapperControl::CorrelatorBandSeq(cVec.size());

		(*cSeq).length(cVec.size());

		for(unsigned i=0; i < cVec.size(); i++) {
		  (*cSeq)[i].crate.type    = correlatorType(cVec[i].crate_.type_);
		  (*cSeq)[i].crate.crateNo = cVec[i].crate_.crateNo_;
		  (*cSeq)[i].bandNo        = cVec[i].bandNo_;
		}
    
		return cSeq;
		) 
    }

/**.......................................................................
 * Query correlator bands associated with this astroband
 */
SignalPathMapperControl::CorrelatorBandSeq* SignalPathMapperControlImpl::
getCorrelatorBands(unsigned short astroBandNo)
{
  CORBA_WRAPPER(

		std::vector<carma::signalpath::CorrelatorBandSpec> cVec;

		cVec = spMap_.getCorrelatorBands(astroBandNo);

		SignalPathMapperControl::CorrelatorBandSeq* cSeq = 
		new SignalPathMapperControl::CorrelatorBandSeq(cVec.size());

		(*cSeq).length(cVec.size());

		for(unsigned i=0; i < cVec.size(); i++) {
		  (*cSeq)[i].crate.type    = correlatorType(cVec[i].crate_.type_);
		  (*cSeq)[i].crate.crateNo = cVec[i].crate_.crateNo_;
		  (*cSeq)[i].bandNo        = cVec[i].bandNo_;
		}
    
		return cSeq;
		) 
    }

/**.......................................................................
 * Get correlator band numbers associated with this astroband
 */
carma::util::SeqShort * 
SignalPathMapperControlImpl::getCorrelatorBandNoSeq(unsigned short astroBandNo)
{
  CORBA_WRAPPER(

		std::vector<carma::signalpath::CorrelatorBandSpec> cVec;
		cVec = spMap_.getCorrelatorBands(astroBandNo);
		// debug -- mwp
		//{ 
		//   ScopedLogNdc ndc("SignalPathMapperControlImpl::getCorrelatorBandNoSeq");
		//   ostringstream os;
		//   os << "Size of correlator band vector " << cVec.size();
		//   programLogInfoIfPossible( os.str() );
		// }
  
  
		SeqShort * cSeq = new SeqShort(cVec.size());

		(*cSeq).length(cVec.size());

		for(unsigned i=0; i < cVec.size(); i++) {
		  (*cSeq)[i] = cVec[i].bandNo_;
		}
		//{ 
		//   ScopedLogNdc ndc("SignalPathMapperControlImpl::getCorrelatorBandNoSeq");
		//   ostringstream os;
		//   os << "Size of correlator band sequence " << cSeq->length();
		//  programLogInfoIfPossible( os.str() );
		//}
    
		return cSeq;
		) 
    }

/**.......................................................................
 * Get astro band numbers  associated with this correlator
 */
carma::util::SeqShort * 
SignalPathMapperControlImpl::getActiveAstroBandNoSeq(util::CorrelatorType type)
{
  CORBA_WRAPPER(

		std::vector<unsigned> cVec;
		cVec= spMap_.getActiveAstroBandNos(correlatorType(type));
		//{ 
		//   ScopedLogNdc ndc("SignalPathMapperControlImpl::getActiveAstroBandNoSeq");
		//   ostringstream os;
		//   os << "Size of active Astroband vector " << cVec.size();
		//   programLogInfoIfPossible( os.str() );
		// }
  
		SeqShort * cSeq = new SeqShort(cVec.size());
		(*cSeq).length(cVec.size());

		for(unsigned i=0; i < cVec.size(); i++) {
		  (*cSeq)[i] = cVec[i];
		}
    
		//{ 
		//   ScopedLogNdc ndc("SignalPathMapperControlImpl::getActiveAstroBandNoSeq");
		//   ostringstream os;
		//  os << "Size of active Astroband sequence " << cSeq->length();
		//   programLogInfoIfPossible( os.str() );
		// }
		//SeqShort_var cSeqvar(cSeq);
		//return cSeqvar._retn();
		return cSeq;
		) 
    }
/**.......................................................................
 * Query correlator band input mapping
 */
SignalPathMapperControl::CorrelatorBandInputSeq* SignalPathMapperControlImpl::
getCorrelatorBandInputMap(SignalPathMapperControl::CorrelatorBand spBand)
{
  CORBA_WRAPPER(

		std::vector<carma::signalpath::CorrelatorBandInputSpec> iVec;

		carma::signalpath::CorrelatorBandSpec band;
		band.bandNo_         = spBand.bandNo;
		band.crate_.crateNo_ = spBand.crate.crateNo;
		band.crate_.type_    = correlatorType(spBand.crate.type);

		iVec = spMap_.getCorrelatorBandInputMap(band);
  
		SignalPathMapperControl::CorrelatorBandInputSeq* iSeq = 
		new SignalPathMapperControl::CorrelatorBandInputSeq(iVec.size());

		(*iSeq).length(iVec.size());

		for(unsigned i=0; i < iVec.size(); i++) {
		  (*iSeq)[i].band.crate.crateNo = iVec[i].band_.crate_.crateNo_;
		  (*iSeq)[i].band.crate.type    = correlatorType(iVec[i].band_.crate_.type_);
		  (*iSeq)[i].band.bandNo        = iVec[i].band_.bandNo_;
		  (*iSeq)[i].antIF.antNo        = iVec[i].antIF_.antNo_;
		  (*iSeq)[i].antIF.polType      = polType(iVec[i].antIF_.polType_);
		  (*iSeq)[i].inputNo            = iVec[i].inputNo_;

		  (*iSeq)[i].aBandInput.inputNo           = iVec[i].astroBandInput_.inputNo_;
		  (*iSeq)[i].aBandInput.aBand.astroBandNo = iVec[i].astroBandInput_.astroBandNo_;
		}
    
		return iSeq;
		);
}
      
/**.......................................................................
 * Query active correlator crates managed by this correlator
 */
SignalPathMapperControl::CorrelatorCrateSeq* SignalPathMapperControlImpl::
getActiveCorrelatorCrates(util::CorrelatorType type)
{
  CORBA_WRAPPER(

		std::vector<carma::signalpath::CorrelatorCrateSpec> cVec;
		cVec = spMap_.getActiveCorrelatorCrates(correlatorType(type));
  
		SignalPathMapperControl::CorrelatorCrateSeq* cSeq = 
		new SignalPathMapperControl::CorrelatorCrateSeq(cVec.size());

		(*cSeq).length(cVec.size());

		for(unsigned i=0; i < cVec.size(); i++) {
		  (*cSeq)[i].type    = correlatorType(cVec[i].type_);
		  (*cSeq)[i].crateNo = cVec[i].crateNo_;
		}
    
		return cSeq;
		) 
    }

/**.......................................................................
 * Query correlator crate input mapping
 */
SignalPathMapperControl::CorrelatorCrateInputSeq* SignalPathMapperControlImpl::
getCorrelatorCrateInputMap(SignalPathMapperControl::CorrelatorCrate spCrate)
{
  CORBA_WRAPPER(

		std::vector<carma::signalpath::CorrelatorCrateInputSpec> iVec;

		carma::signalpath::CorrelatorCrateSpec crate;
		crate.crateNo_ = spCrate.crateNo;
		crate.type_    = correlatorType(spCrate.type);

		iVec = spMap_.getCorrelatorCrateInputMap(crate);
  
		SignalPathMapperControl::CorrelatorCrateInputSeq* iSeq = 
		new SignalPathMapperControl::CorrelatorCrateInputSeq(iVec.size());

		(*iSeq).length(iVec.size());
  
		for(unsigned i=0; i < iVec.size(); i++) {
		  (*iSeq)[i].crate.crateNo  = iVec[i].crate_.crateNo_;
		  (*iSeq)[i].crate.type     = correlatorType(iVec[i].crate_.type_);
		  (*iSeq)[i].inputNo        = iVec[i].inputNo_;
		  (*iSeq)[i].antIF.antNo    = iVec[i].antIF_.antNo_;
		  (*iSeq)[i].antIF.polType  = polType(iVec[i].antIF_.polType_);
		}
    
		return iSeq;
		) 
    }

/**.......................................................................
 * Returns true if the switch setting does not conflict with any
 * current Astroband configuration
 */
bool SignalPathMapperControlImpl::
canAssertSwitchPosition(carma::switchyard::SwitchPosition pos)
{
  CORBA_WRAPPER(

		carma::signalpath::SwitchSetting swSet;

		swSet.switchNo_ = pos.switchNo;
		swSet.channel_  = SignalPathMap::switchChannelNumberToChannelId(pos.switchPos);

		return spMap_.canAssertSwitchPosition(swSet);

		);
}

/**.......................................................................
 * Return the configuration as a printable string
 */
char* SignalPathMapperControlImpl::queryConfiguration()
{
  CORBA_WRAPPER(

		std::string conf = spMap_.printDown();
		return CORBA::string_dup(conf.c_str());

		);
}

/**.......................................................................
 * Contructor initialization for this class
 */
void SignalPathMapperControlImpl::initialize()
{
  const string cableMap = Program::getConfFile("signalpath/c3g_cableMap.txt");
  COUT("Attempting to initialize cable map from: " << cableMap );
  spMap_.initializeCableMap( cableMap );
  spMapChecker_.initializeCableMap( cableMap );

  // Default LL configuration for both correlators

  knownConfigurations_.push_back(new SignalPathMapperControlImpl::SwitchyardConfiguration("LL",         SignalPathMapperControl::LL,         
											  "SINGLE",     "c3g_ll.txt"));

  // LL configuration for both correlators with C11 substituted for C19

  knownConfigurations_.push_back(new SignalPathMapperControlImpl::SwitchyardConfiguration("LL_INTERIM",  SignalPathMapperControl::LL_INTERIM,         
											  "SINGLE",     "ll_interim.txt"));
  // Default RR configuration for SL correlator

  knownConfigurations_.push_back(new SignalPathMapperControlImpl::SwitchyardConfiguration("RR",         SignalPathMapperControl::RR,         
											  "SINGLE",     "rr.txt"));

  // Default CARMA23 configuration for SL correlator + WB continuum (same now as maxsens_carma23_highres)

  knownConfigurations_.push_back(new SignalPathMapperControlImpl::SwitchyardConfiguration("CARMA23",    SignalPathMapperControl::CARMA23,    
											  "DUAL_C23",   "c3g_maxsens_carma23_highres.txt"));

  // Full-Stokes: L and R for C1-15 into the SL correlator

  knownConfigurations_.push_back(new SignalPathMapperControlImpl::SwitchyardConfiguration("FULLSTOKES", SignalPathMapperControl::FULLSTOKES, 
											  "DUAL_FS",    "fullstokes.txt"));

  // Dual-pol: L and R for C1-15 into the SL correlator

  knownConfigurations_.push_back(new SignalPathMapperControlImpl::SwitchyardConfiguration("DUALPOL",    SignalPathMapperControl::DUALPOL, 
											  "DUAL_DP",    "dualpol.txt"));

  // Full-bandwidth LL in SL correlator, plus RR in WB correlator for C1-7,15

  knownConfigurations_.push_back(new SignalPathMapperControlImpl::SwitchyardConfiguration("MAXSENS_DUALPOL", SignalPathMapperControl::MAXSENS_DUALPOL, 
											  "SINGLE",     "maxsens_dualpol.txt"));

  // Full-bandwidth LL in SL correlator, plus LL in WB correlator for C1-7,15

  knownConfigurations_.push_back(new SignalPathMapperControlImpl::SwitchyardConfiguration("MAXSENS_LL", SignalPathMapperControl::MAXSENS_LL, 
											  "SINGLE",     "maxsens_ll.txt"));

  // CARMA23 SL correlator, plus LL in WB correlator for C16-23 (SZAs)                                                                                                                        

  knownConfigurations_.push_back(new SignalPathMapperControlImpl::SwitchyardConfiguration("MAXSENS_CARMA23_LOWRES",SignalPathMapperControl::MAXSENS_CARMA23_LOWRES,
                                                                                          "DUAL_C23",   "maxsens_carma23_lowres.txt"));

  // CARMA23 SL correlator, plus LL in WB correlator for C8-15 (BIMAs)                                                                                                                        

  knownConfigurations_.push_back(new SignalPathMapperControlImpl::SwitchyardConfiguration("MAXSENS_CARMA23_MEDRES",SignalPathMapperControl::MAXSENS_CARMA23_MEDRES,
                                                                                          "DUAL_C23",   "maxsens_carma23_medres.txt"));

  knownConfigurations_.push_back(new SignalPathMapperControlImpl::SwitchyardConfiguration("MAXSENS_CARMA23_HIGHRES",SignalPathMapperControl::MAXSENS_CARMA23_HIGHRES, 
											  "DUAL_C23",   "c3g_maxsens_carma23_highres.txt"));

  // CMTEST configuration

  knownConfigurations_.push_back(new SignalPathMapperControlImpl::SwitchyardConfiguration("CMTEST",SignalPathMapperControl::CMTEST, 
											  "SINGLE",   "cmtest.txt"));
  std::ostringstream os;

  for(unsigned i=0; i < knownConfigurations_.size(); i++) {
    SwitchyardConfiguration* conf = knownConfigurations_[i];

    os.str("");
    os << "signalpath/" << conf->fileName_;
    const string confFile = Program::getConfFile( os.str() );
    COUT("Attempting to initialize " << conf->confName_ << " configuration from: " << confFile );
    try {
      spMap_.loadConfiguration(confFile, conf->confName_.str(), conf->astroBandConfName_.str());
      spMapChecker_.loadConfiguration(confFile, conf->confName_.str(), conf->astroBandConfName_.str());
    } catch(Exception& err) {
      COUT("Caught an error: " << err.what());
    } catch(...) {
      COUT("Caught some other error");
    }
  }

  ms_   = new carma::monitor::CarmaMonitorSystem();
  msSp_ = new carma::monitor::SignalPathSubsystem();

  msCs_ = new carma::monitor::ControlSubsystem();

  // Custom timer for writing the SignalPathSubsystem is
  // implemented below, so don't start an autowriter for it.
  //  msSp_->startAutoWriter(0.1);

  // have a short delay so that the main ControlSubsystem writes that correct values
  // for most of the MP and doesn't get overwritten

  msCs_->startAutoWriter(0.05);

  // Initialize monitor points

  updateMonitorPoints();

  invalidMask_.resize(carma::signalpath::AstroBand::nBandMax_+1);
  invalidMask_.setAllBitsLow();

  initializeConfigurationChecker();

  // Finally, initialize the monitor system writer

  initializeMonitorSystemWriter();
}

/**.......................................................................
 * Initialize the absolute timer on which we will write cached data to
 * the monitor system
 */
void SignalPathMapperControlImpl::initializeMonitorSystemWriter()
{
  writeMonitorSystemTimer_ = 0;
  writeMonitorSystemTimer_ = new sza::util::AbsoluteTimer();

  if(!writeMonitorSystemTimer_) {
    ThrowColorError("Unable to create writeMonitorSystemTimer_", "red");
  }

  writeMonitorSystemTimer_->addHandler(writeMonitorSystemHandler, (void*)this);
  writeMonitorSystemTimer_->enableTimer(true, 
					1, 0,          // Initial delay of 1 second
					50000000,      // Set to fire 50 ms after the absolute second boundary
					0, 500000000); // Interval is every half-second
  

  writeMonitorSystemTimer_->spawn();
}

/**.......................................................................
 * Called whenever the periodic config checker timer fires.  Just
 * check that the hardware configuration matches the expected
 * configuration.
 */
ABSOLUTE_TIMER_HANDLER(SignalPathMapperControlImpl::writeMonitorSystemHandler)
{
  try {
    SignalPathMapperControlImpl* spmc = (SignalPathMapperControlImpl*) args;
    ScopedPthreadMutexLock spml(spmc->guard_);
    spmc->writeMonitorSystemWrapper();
  } catch(...) {
  }
}

/**.......................................................................
 * Write cached data to the monitor system
 */
void SignalPathMapperControlImpl::writeMonitorSystemWrapper()
{
  msSp_->write();
}

/**.......................................................................
 * Initialize the periodic timer on which we will check the hardware
 * configuration
 */
void SignalPathMapperControlImpl::initializeConfigurationChecker()
{
  lastModifiedVec_.resize(carma::signalpath::AstroBand::nBandMax_);

  checkConfigTimer_ = 0;
  checkConfigTimer_ = new sza::util::PeriodicTimer();

  if(!checkConfigTimer_) {
    ThrowColorError("Unable to create checkConfigTimer_", "red");
  }

  checkConfigTimer_->spawn();
  checkConfigTimer_->addHandler(checkConfigHandler, (void*)this);
  checkConfigEnabled_ = false;

  markHardwareConfigurationAsValid(0, true);
}

/**.......................................................................
 * Called whenever the periodic config checker timer fires.  Just
 * check that the hardware configuration matches the expected
 * configuration.
 */
PERIODIC_TIMER_HANDLER(SignalPathMapperControlImpl::checkConfigHandler)
{
  try {
    SignalPathMapperControlImpl* spmc = (SignalPathMapperControlImpl*) args;
    ScopedPthreadMutexLock spml(spmc->guard_);
    spmc->checkConfigurationSuccessWrapper(0);
  } catch(...) {
  }
}

/**.......................................................................
 * Enable/disable the configuration checker
 */
void SignalPathMapperControlImpl::
enableConfigurationChecker(bool enable)
{
  if(checkConfigEnabled_ != enable) {
    checkConfigTimer_->enableTimer(enable, 1);
    checkConfigEnabled_ = enable;
  }

  // If we are disabling checking, this means that no astrobands are
  // currently configured, so mark the hardware configuration as valid

  if(!enable) {
    markHardwareConfigurationAsValid(0, true);
  }
}

/**.......................................................................
 * Utility to convert from carma::signalpath::SubarrayId to LO/LL
 * switch number
 */
unsigned short SignalPathMapperControlImpl::
switchPos(carma::signalpath::SubarrayId subarrayId)
{
  switch (subarrayId) {
  case carma::signalpath::SA_1:
    return 1;
    break;
  case carma::signalpath::SA_2:
    return 2;
    break;
  case carma::signalpath::SA_3:
    return 3;
    break;
  default:
    return 0;
    break;
  }
}

/**.......................................................................
 * Utility to convert from carma::signalpath::SubarrayId to subarray number
 */
unsigned short SignalPathMapperControlImpl::
subarrayNumber(carma::signalpath::SubarrayId subarrayId)
{
  switch (subarrayId) {
  case carma::signalpath::SA_1:
    return 1;
    break;
  case carma::signalpath::SA_2:
    return 2;
    break;
  case carma::signalpath::SA_3:
    return 3;
    break;
  case carma::signalpath::SA_4:
    return 4;
    break;
  default:
    return 0;
    break;
  }
}

/**.......................................................................
 * Utility to convert from subarray number to
 * carma::signalpath::SubarrayId
 */
carma::signalpath::SubarrayId SignalPathMapperControlImpl::
subarrayId(unsigned short subarrayNo)
{
  switch (subarrayNo) {
  case 1:
    return carma::signalpath::SA_1;
    break;
  case 2:
    return carma::signalpath::SA_2;
    break;
  case 3:
    return carma::signalpath::SA_3;
    break;
  default:
    return carma::signalpath::SA_NONE;
    break;
  }
}

/**.......................................................................
 * Utility to convert from carma::util::CorrelatorType to
 * carma::signalpath::CorrelatorType
 */
carma::signalpath::CorrelatorType SignalPathMapperControlImpl::
correlatorType(util::CorrelatorType type)
{
  return static_cast<carma::signalpath::CorrelatorType>(type);
}

/**.......................................................................
 * Utility to convert from carma::signalpath::CorrelatorType to
 * carma::util::CorrelatorType
 */
util::CorrelatorType SignalPathMapperControlImpl::
correlatorType(carma::signalpath::CorrelatorType type)
{
  return static_cast<carma::util::CorrelatorType>(type);
}

/**.......................................................................
 * Utility to convert from carma::signalpath::PolarizationType to
 * carma::antenna::common::PolarizationControl::State
 */
carma::signalpath::SignalPathMapperControl::PolarizationType SignalPathMapperControlImpl::
polType(carma::signalpath::PolarizationType type)
{
  switch (type) {
  case carma::signalpath::POL_LEFT:
    return carma::signalpath::SignalPathMapperControl::POL_LEFT;
    break;
  case carma::signalpath::POL_RIGHT:
    return carma::signalpath::SignalPathMapperControl::POL_RIGHT;
    break;
  default:
    return carma::signalpath::SignalPathMapperControl::POL_NONE;
    break;
  }
}

/**.......................................................................
 * Utility to convert from an astro band configuration to
 * carma::util::CorrelatorFpgaModeType
 */
carma::util::CorrelatorFpgaModeType
SignalPathMapperControlImpl::astroBandConfToCorrFpgaMode(std::string swConfName, std::string abConfName)
{ 
  String swConfNameStr(swConfName);
  swConfNameStr = swConfNameStr.toUpper();

  String abConfNameStr(abConfName);
  abConfNameStr = abConfNameStr.toUpper();

  carma::util::CorrelatorFpgaModeType fpgaMode = carma::util::CORR_SINGLEPOL;

  if(abConfNameStr.str() == "SINGLE") {
    fpgaMode = carma::util::CORR_SINGLEPOL;
  } else if(abConfNameStr.str() == "DUAL_C23") {
    fpgaMode = carma::util::CORR_CARMA23;
  } else if(abConfNameStr.str() == "DUAL_FS") {
    fpgaMode = carma::util::CORR_FULLPOL;
  } else if(abConfNameStr.str() == "DUAL_DP") {
    fpgaMode = carma::util::CORR_DUALPOL;
  } else {
    ThrowColorError("Astro band " << abConfName << " has no associated FPGA mode", "red");
  }

  return fpgaMode;
}

/**.......................................................................
 * Utility to convert from carma::signalpath::ConfigurationType to
 * carma::monitor::ConfTagMonitorPointEnum
 */
carma::monitor::ConfTagMonitorPointEnum::CONFTAG
SignalPathMapperControlImpl::msConfTag(std::string name)
{
  String nameStr(name);
  nameStr = nameStr.toUpper();

  for(unsigned i=0; i < knownConfigurations_.size(); i++) {
    if(nameStr == knownConfigurations_[i]->confName_) {
      return msConfTag(knownConfigurations_[i]->tag_);
    }
  }

  return carma::monitor::ConfTagMonitorPointEnum::UNKNOWN;
}

carma::monitor::ConfTagMonitorPointEnum::CONFTAG
SignalPathMapperControlImpl::msConfTag(carma::signalpath::SignalPathMapperControl::SwitchyardConfiguration confTag)
{
  switch (confTag) {
  case carma::signalpath::SignalPathMapperControl::LL:
    return carma::monitor::ConfTagMonitorPointEnum::LL;
    break;
  case carma::signalpath::SignalPathMapperControl::RR:
    return carma::monitor::ConfTagMonitorPointEnum::RR;
    break;
  case carma::signalpath::SignalPathMapperControl::CARMA23:
    return carma::monitor::ConfTagMonitorPointEnum::CARMA23;
    break;
  case carma::signalpath::SignalPathMapperControl::FULLSTOKES:
    return carma::monitor::ConfTagMonitorPointEnum::FULLSTOKES;
    break;
  case carma::signalpath::SignalPathMapperControl::DUALPOL:
    return carma::monitor::ConfTagMonitorPointEnum::DUALPOL;
    break;
  case carma::signalpath::SignalPathMapperControl::MAXSENS_DUALPOL:
    return carma::monitor::ConfTagMonitorPointEnum::MAXSENS_DUALPOL;
    break;
  case carma::signalpath::SignalPathMapperControl::MAXSENS_CARMA23_LOWRES:
    return carma::monitor::ConfTagMonitorPointEnum::MAXSENS_CARMA23_LOWRES;
    break;
  case carma::signalpath::SignalPathMapperControl::MAXSENS_CARMA23_MEDRES:
    return carma::monitor::ConfTagMonitorPointEnum::MAXSENS_CARMA23_MEDRES;
    break;
  case carma::signalpath::SignalPathMapperControl::MAXSENS_CARMA23_HIGHRES:
    return carma::monitor::ConfTagMonitorPointEnum::MAXSENS_CARMA23_HIGHRES;
    break;
  case carma::signalpath::SignalPathMapperControl::MAXSENS_LL:
    return carma::monitor::ConfTagMonitorPointEnum::MAXSENS_LL;
    break;
  default:
    return carma::monitor::ConfTagMonitorPointEnum::UNKNOWN;
    break;
  }
}

/**.......................................................................
 * Utility to convert from carma::signalpath::PolarizationType to
 * carma::monitor::PolarizationMonitorPointEnum
 */
carma::monitor::PolarizationMonitorPointEnum::POLARIZATION 
SignalPathMapperControlImpl::msPolType(carma::signalpath::PolarizationType type)
{
  switch (type) {
  case carma::signalpath::POL_LEFT:
    return carma::monitor::PolarizationMonitorPointEnum::L;
    break;
  case carma::signalpath::POL_RIGHT:
    return carma::monitor::PolarizationMonitorPointEnum::R;
    break;
  default:
    return carma::monitor::PolarizationMonitorPointEnum::UNKNOWN;
    break;
  }
}

/**.......................................................................
 * Utility to convert from
 * carma::downconverter::BlockDownconverterControl::Polarization
 * carma::monitor::PolarizationMonitorPointEnum
 */
carma::monitor::BlockDownconverter::PolarizationMonitorPointEnum::POLARIZATION 
SignalPathMapperControlImpl::msPolType(carma::downconverter::BlockDownconverterControl::Polarization type)
{
  switch (type) {
  case carma::downconverter::BlockDownconverterControl::POLARIZATION_1:
    return carma::monitor::BlockDownconverter::PolarizationMonitorPointEnum::POLARIZATION_1;
    break;
  case carma::downconverter::BlockDownconverterControl::POLARIZATION_2:
    return carma::monitor::BlockDownconverter::PolarizationMonitorPointEnum::POLARIZATION_2;
    break;
  default:
    return carma::monitor::BlockDownconverter::PolarizationMonitorPointEnum::UNDEFINED;
    break;
  }
}

/**.......................................................................
 * Utility to convert from
 * carma::signalpath::BlockDownconverterInputType
 * carma::monitor::PolarizationMonitorPointEnum
 */
carma::monitor::BlockDownconverter::PolarizationMonitorPointEnum::POLARIZATION 
SignalPathMapperControlImpl::msPolType(carma::signalpath::BlockDownconverterInputType type)
{
  switch (type) {
  case carma::signalpath::BD_INP_P1:
    return carma::monitor::BlockDownconverter::PolarizationMonitorPointEnum::POLARIZATION_1;
    break;
  case carma::signalpath::BD_INP_P2:
    return carma::monitor::BlockDownconverter::PolarizationMonitorPointEnum::POLARIZATION_2;
    break;
  default:
    return carma::monitor::BlockDownconverter::PolarizationMonitorPointEnum::UNDEFINED;
    break;
  }
}

/**.......................................................................
 * Utility to convert from carma::signalpath::SplitterChannelId to
 * carma::monitor::SplitterChannelMonitorPointEnum::SPLITTERCHANNEL
 */
carma::monitor::SplitterChannelMonitorPointEnum::SPLITTERCHANNEL
SignalPathMapperControlImpl::msSplitterChannel(carma::signalpath::SplitterChannelId channelId)
{
  switch (channelId) {
  case carma::signalpath::SP_CHAN_A:
    return carma::monitor::SplitterChannelMonitorPointEnum::A;
    break;
  case carma::signalpath::SP_CHAN_B:
    return carma::monitor::SplitterChannelMonitorPointEnum::B;
    break;
  case carma::signalpath::SP_CHAN_C:
    return carma::monitor::SplitterChannelMonitorPointEnum::C;
    break;
  case carma::signalpath::SP_CHAN_D:
    return carma::monitor::SplitterChannelMonitorPointEnum::D;
    break;
  default:
    return carma::monitor::SplitterChannelMonitorPointEnum::NONE;
    break;
  }
}

/**.......................................................................
 * Utility to convert from carma::signalpath::CorrelatorType to
 * carma::monitor::ControlCorrelEnum
 */
//@TODO replace with monitor::corrTypeToCorrDes()
MonitorCorrelatorDesignation
SignalPathMapperControlImpl::msCorrelatorType(carma::signalpath::CorrelatorType type)
{
  return static_cast<MonitorCorrelatorDesignation>(type);
}

carma::monitor::ControlBandPoints::FpgaModeMonitorPointEnum::FPGAMODE
SignalPathMapperControlImpl::msFpgaMode(carma::util::CorrelatorFpgaModeType type)
{
  switch (type) {
  case carma::util::CORR_SINGLEPOL:
    return carma::monitor::ControlBandPoints::FpgaModeMonitorPointEnum::SINGLEPOL;
    break;
  case carma::util::CORR_CARMA23:
    return carma::monitor::ControlBandPoints::FpgaModeMonitorPointEnum::CARMA23;
    break;
  case carma::util::CORR_DUALPOL:
    return carma::monitor::ControlBandPoints::FpgaModeMonitorPointEnum::DUALPOL;
    break;
  default:
    return carma::monitor::ControlBandPoints::FpgaModeMonitorPointEnum::FULLPOL;
    break;
  }
}

/**.......................................................................
 * Write our internal configuration to the monitor stream
 */
void SignalPathMapperControlImpl::updateMonitorPoints()
{
  clearAstroBandMonitorPoints();
  updateAstroBandMonitorPoints();

  clearCorrBandMonitorPoints();
  updateCorrBandMonitorPoints();

  clearIFSwitchMonitorPoints();
  updateIFSwitchMonitorPoints();

  clearAntennaMonitorPoints();
  updateAntennaMonitorPoints();

  clearSubarrayMonitorPoints();
  updateSubarrayMonitorPoints();
}

/**.......................................................................
 * Mark an astroband configuration as modified
 */
void SignalPathMapperControlImpl::markAstroBandAsModified(unsigned astroBandNo, int frameCount)
{
  lastModifiedVec_[astroBandNo-1] = frameCount;

  msSp_->mapping().astroband(astroBandNo-1).lastModified().setValue(frameCount);
  msSp_->mapping().lastModified().setValue(frameCount);
}

/**.......................................................................
 * Mark a correlator band as modified
 */
void SignalPathMapperControlImpl::markCorrelatorBandAsModified(CorrelatorBandSpec& band, int frameCount)
{
  if(band.crate_.type_ == CORR_SL) {
    msSp_->mapping().slcBand(band.bandNo_-1).lastModified().setValue(frameCount);
  } else {
    msSp_->mapping().wbcBand(band.bandNo_-1).lastModified().setValue(frameCount);
  }
}

/**.......................................................................
 * Update the astro band configuration in the monitor stream
 */
void SignalPathMapperControlImpl::updateAstroBandMonitorPoints()
{
  //ScopedLogNdc ndc(" MWP SPMCI::updateAstroBandMonitorPoints()");
  //------------------------------------------------------------
  // Create a map of the number of antnenas owned by each subarray
  //------------------------------------------------------------

  std::map<unsigned, unsigned> subarrayNantMap;

  // First initialize to zero

  for(unsigned iSubarray=0; iSubarray < SignalPathMap::nSubarray_; iSubarray++) {
    unsigned subarrayNo = (iSubarray == SignalPathMap::nSubarray_-1) ? 0 : iSubarray+1;
    subarrayNantMap[subarrayNo] = 0;
  }

  // Now increment

  for(unsigned iAntenna=0; iAntenna < SignalPathMap::nAnt_; iAntenna++) {
    unsigned antennaNo = iAntenna+1;
    carma::signalpath::Antenna* ant = spMap_.getAntenna(antennaNo);
    subarrayNantMap[subarrayNumber(ant->subarrayId_)] += 1;
  }

  //------------------------------------------------------------
  // Iterate through astro bands, updating settings for each one
  //------------------------------------------------------------

  for(unsigned iAstroBand=0; iAstroBand < carma::signalpath::AstroBand::nBandMax_; iAstroBand++) {

    unsigned nInput = 0;
    unsigned astroBandNo = iAstroBand+1;
    std::map<unsigned, unsigned> antsInConfiguration;

    carma::signalpath::AstroBand* astroBand = spMap_.getAstroBand(astroBandNo);

    carma::monitor::SignalPathSubsystem::Astroband& msAstroBand = 
      msSp_->mapping().astroband(iAstroBand);

    // Now iterate over inputs

    std::vector<AstroBandInput*>& inputs = astroBand->inputs_;
    bool firstInput = true;
    for(unsigned iInput=0; iInput < inputs.size(); iInput++) {

      unsigned astroBandInputNo = iInput+1;
      carma::signalpath::AstroBandInput* astroBandInput = inputs[iInput];
      carma::monitor::Input& msAstroBandInput = msAstroBand.input(nInput);

      // Set the crate type of this input, or continue if this input
      // is not configured

      if(!astroBandInput->isConfigured_) continue;

      try {

        // EML: Of course these methods will throw for C3G bands,
        // which are not associated with any correlator 'crate'
        // hardware!
        //
        // AstroBandInput::getAntennaIF() returns the ant IF
        // regardless of what hardware (C3G digitizer or old-style
        // block-downconverter) it is connected to

        carma::signalpath::CorrelatorCrateInput* corrCrateInput = 0;
        carma::signalpath::CorrelatorCrate*      crate          = 0;
        carma::signalpath::CorrelatorBandInput*  corrBandInput  = 0;
        carma::signalpath::AntennaIF*            antIF          = 0;
        carma::signalpath::CorrelatorType        corrType       = carma::signalpath::CORR_NONE;

        if(astroBandInput->isConnectedToCrate()) {
          corrCrateInput = astroBandInput->getCrateInput();
          crate          = corrCrateInput->crate_; 
          corrBandInput  = corrCrateInput->getCorrelatorBandInput(); 
          antIF          = corrCrateInput->getBdcBand()->getBdcInput()->getSwitch()->getCurrentChannel()->getAntennaIF();
          corrType       = crate->type_;
        } else {
          antIF          = astroBandInput->getAntennaIF();
          corrType       = astroBand->type_;
        }

        // Create an entry in the map for this antennas

        unsigned antennaNo = antIF->antenna_->antNo_;
        antsInConfiguration[antennaNo] = antennaNo;

        // Set the crate type

        msAstroBandInput.CORRELATOR_DESIGNATION_MP().setValue(msCorrelatorType(corrType));

        // Get the correlator crate type of the first input, and set
        // the designation for the whole astroband accordingly (even
        // with future correlators, we don't envision having single
        // astrobands stretching across different correlators)

        if(firstInput) { 
          firstInput = false;
          msAstroBand.CORRELATOR_DESIGNATION_MP().setValue(msCorrelatorType(corrType));
        }

        // Set up the astroband information

        msAstroBandInput.astroBandNo().setValue(astroBandNo);
        msAstroBandInput.astroBandInputNo().setValue(astroBandInputNo);

        // Set the correlator band information.  These monitor points
        // have no meaning for the new correlator, where we've
        // dispensed with 'correlator bands' along with correlator crates
        
        if(astroBandInput->isConnectedToCrate()) {

          msAstroBandInput.corrBandNo().setValue(corrBandInput->band_->bandNo_);
          msAstroBandInput.corrBandNo().setValidity(carma::monitor::MonitorPoint::VALID);
          
          msAstroBandInput.corrBandInputNo().setValue(corrBandInput->inputNo_);
          msAstroBandInput.corrBandInputNo().setValidity(carma::monitor::MonitorPoint::VALID);
        }

        // Set the antenna information for this input, or continue if
        // this input is not configured
        
        msAstroBandInput.antennaNo().setValue(antIF->getAntenna()->antNo_);
        msAstroBandInput.antennaNo().setValidity(carma::monitor::MonitorPoint::VALID);

        msAstroBandInput.polarization().setValue(msPolType(antIF->polType_));

      } catch(...) {
        continue;
      }

      // And increment the number of inputs that were configured

      ++nInput;
    }

    // Mark the number of inputs actually configured

    msAstroBand.nInput().setValue(nInput);

    if(astroBand->conf_) {
      msAstroBand.confName().setValue(astroBand->conf_->name_);
      msAstroBand.confTag().setValue(msConfTag(astroBand->conf_->name_));
      msAstroBand.subarrayNo().setValue(subarrayNumber(astroBand->subarrayId_));

      // Mark the number of unique antennas in this configuration

      msAstroBand.nAntInConfiguration().setValue(antsInConfiguration.size());
    } 
  }
}

/**.......................................................................
 * Iterate through all astrobands, clearing monitor system information
 * for each one
 */
void SignalPathMapperControlImpl::clearAstroBandMonitorPoints()
{
  carma::monitor::SignalPathSubsystem::Mapping& mapping = msSp_->mapping(); 

  // Iterate through astro bands, clearing settings for each one

  for(int iAstroBand=0; iAstroBand < mapping.astrobandCount(); iAstroBand++) {

    unsigned astroBandNo = iAstroBand+1;
    carma::monitor::SignalPathSubsystem::Astroband& msAstroBand = 
      mapping.astroband(iAstroBand);

    // Now iterate over inputs

    for(int iInput=0; iInput < msAstroBand.inputCount(); iInput++) {

      unsigned astroBandInputNo = iInput+1;
      carma::monitor::Input& msAstroBandInput = msAstroBand.input(iInput);

      // Set up the astroband information

      msAstroBandInput.astroBandNo().setValue(astroBandNo);
      msAstroBandInput.astroBandInputNo().setValue(astroBandInputNo);
      msAstroBandInput.CORRELATOR_DESIGNATION_MP().setValue(CorrDesignation::NONE);

      msAstroBandInput.corrBandNo().setValue(0);
      msAstroBandInput.corrBandNo().setValidity(carma::monitor::MonitorPoint::INVALID_NO_HW);

      msAstroBandInput.corrBandInputNo().setValue(0);
      msAstroBandInput.corrBandInputNo().setValidity(carma::monitor::MonitorPoint::INVALID_NO_HW);

      msAstroBandInput.antennaNo().setValue(0);
      msAstroBandInput.antennaNo().setValidity(carma::monitor::MonitorPoint::INVALID_NO_HW);

      msAstroBandInput.polarization().setValue(carma::monitor::PolarizationMonitorPointEnum::UNKNOWN);
    }

    // Mark the number of inputs actually configured

    msAstroBand.nInput().setValue(0);
    msAstroBand.nAntInConfiguration().setValue(0);

    // Set the configuration names and enum tags to NONE

    msAstroBand.confName().setValue("NONE");
    msAstroBand.confTag().setValue(carma::monitor::ConfTagMonitorPointEnum::NONE);

    // Set the subarray number to 0

    msAstroBand.subarrayNo().setValue(0);
    msAstroBand.subarrayNo().setValidity(carma::monitor::MonitorPoint::INVALID_NO_HW);

    // Set the correlator designation for this astroband to NONE

    msAstroBand.CORRELATOR_DESIGNATION_MP().setValue(CorrDesignation::NONE);
  }
}

/**.......................................................................
 * Update the corr band configuration in the monitor stream
 */
void SignalPathMapperControlImpl::updateCorrBandMonitorPoints()
{
  updateCorrBandMonitorPoints(CORR_SL, false);
  updateCorrBandMonitorPoints(CORR_WB, false);
}

/**.......................................................................
 * Clear the corr band configuration in the monitor stream
 */
void SignalPathMapperControlImpl::clearCorrBandMonitorPoints()
{
  updateCorrBandMonitorPoints(CORR_SL, true);
  updateCorrBandMonitorPoints(CORR_WB, true);
}

void SignalPathMapperControlImpl::updateCorrBandMonitorPoints(carma::signalpath::CorrelatorType type, 
							      bool clear)
{
  unsigned nBand  = 0;
  CorrelatorSet corrset(type);
  MonitorCorrelatorDesignation corrDesignation = monitor::corrTypeToCorrDes(type);

  if ( ! corrset.isSingleCorrelator() || corrset.isEmpty() )
    ThrowColorError("Correlator type must resolve to a single valid correlator", "red");

  if ( corrset.isSpectral() ) {
    nBand = carma::signalpath::BlockDownconverter::nBandSl_;
  } else if ( corrset.isWideband() ) {
    nBand = carma::signalpath::BlockDownconverter::nBandWb_;
  } else if ( corrset.isC3gMax8() || corrset.isC3gMax23() ) { 
    nBand = carma::signalpath::BlockDownconverter::nBandC3g_;
  }

  //------------------------------------------------------------
  // Iterate through correlator bands, updating settings for each one
  //------------------------------------------------------------

  for(unsigned iCorrBand=0; iCorrBand < nBand; iCorrBand++) {

    unsigned corrBandNo = iCorrBand+1;
    carma::signalpath::CorrelatorBand* corrBand = spMap_.getCorrBand(type, corrBandNo);

    // Set up defaults for this correlator band

    if(type == CORR_SL) {
      msSp_->mapping().slcBand(iCorrBand).subarrayNo().setValue(0);
      msSp_->mapping().slcBand(iCorrBand).subarrayNo().setValidity(carma::monitor::MonitorPoint::INVALID_NO_HW);
    } else {
      msSp_->mapping().wbcBand(iCorrBand).subarrayNo().setValue(0);
      msSp_->mapping().wbcBand(iCorrBand).subarrayNo().setValidity(carma::monitor::MonitorPoint::INVALID_NO_HW);
    }

    // Now iterate over all inputs for this correlator band:

    std::vector<CorrelatorBandInput*>& inputs = corrBand->inputs_;
    for(unsigned iInput=0; iInput < inputs.size(); iInput++) {

      unsigned corrBandInputNo = iInput+1;
      carma::signalpath::CorrelatorBandInput* corrBandInput = inputs[iInput];
      carma::monitor::Input& msCorrBandInput = (type == CORR_SL ? 
						msSp_->mapping().slcBand(iCorrBand).input(iInput) :
						msSp_->mapping().wbcBand(iCorrBand).input(iInput));

      // Set up monitor points that are always valid

      msCorrBandInput.CORRELATOR_DESIGNATION_MP().setValue(corrDesignation);
      msCorrBandInput.corrBandNo().setValue(corrBandNo);
      msCorrBandInput.corrBandInputNo().setValue(corrBandInputNo);

      // Set up defaults

      msCorrBandInput.astroBandNo().setValue(0);
      msCorrBandInput.astroBandNo().setValidity(carma::monitor::MonitorPoint::INVALID_NO_HW);

      msCorrBandInput.astroBandInputNo().setValue(0);
      msCorrBandInput.astroBandInputNo().setValidity(carma::monitor::MonitorPoint::INVALID_NO_HW);
      
      msCorrBandInput.antennaNo().setValue(0);
      msCorrBandInput.antennaNo().setValidity(carma::monitor::MonitorPoint::INVALID_NO_HW);

      msCorrBandInput.polarization().setValue(carma::monitor::PolarizationMonitorPointEnum::UNKNOWN);

      // Set up astroband mapping information if we are not clearing the monitor points
	
      if(!clear) {

	try {
	  
	  carma::signalpath::CorrelatorCrateInput* corrCrateInput = corrBandInput->getCrateInput();
	  carma::signalpath::AstroBandInput*       astroBandInput = corrCrateInput->getAstroBandInput();
	  carma::signalpath::AntennaIF*            antIF          = 
	    corrCrateInput->getBdcBand()->getBdcInput()->getSwitch()->getCurrentChannel()->getAntennaIF();
	  
	  msCorrBandInput.astroBandNo().setValue(astroBandInput->band_->bandNo_);
	  msCorrBandInput.astroBandNo().setValidity(carma::monitor::MonitorPoint::VALID);

	  msCorrBandInput.astroBandInputNo().setValue(astroBandInput->inputNo_);
	  msCorrBandInput.astroBandInputNo().setValidity(carma::monitor::MonitorPoint::VALID);
	  
	  msCorrBandInput.antennaNo().setValue(antIF->getAntenna()->antNo_);
	  msCorrBandInput.antennaNo().setValidity(carma::monitor::MonitorPoint::VALID);

	  msCorrBandInput.polarization().setValue(msPolType(antIF->polType_));

	  // If this if the first input, set the subarray ownership of
	  // this correlator band from the astroband it is associated
	  // with (don't need to do this for every input, since they
	  // will all trace back to the same subarray id)

	  if(iInput==0) {
	    if(type == CORR_SL) {
	      msSp_->mapping().slcBand(iCorrBand).subarrayNo().setValue(subarrayNumber(astroBandInput->band_->subarrayId_));
	      msSp_->mapping().slcBand(iCorrBand).subarrayNo().setValidity(carma::monitor::MonitorPoint::VALID);
	    } else {
	      msSp_->mapping().wbcBand(iCorrBand).subarrayNo().setValue(subarrayNumber(astroBandInput->band_->subarrayId_));
	      msSp_->mapping().wbcBand(iCorrBand).subarrayNo().setValidity(carma::monitor::MonitorPoint::VALID);
	    }
	  }
	  
	} catch(...) {
	  continue;
	}
      }

    }
  }
}

/**.......................................................................
 * Iterate through all antennas, clearing monitor system information
 * for each one
 */
void SignalPathMapperControlImpl::updateAntennaMonitorPoints()
{
  carma::monitor::SignalPathSubsystem::Mapping& mapping = msSp_->mapping(); 

  //------------------------------------------------------------
  // Create a mapping of antenna <--> correlator designations, and
  // initialize the corr designation to NONE for all antennas
  //------------------------------------------------------------

  std::map<unsigned, CorrelatorType> antCorrTypeMap;

  for(int iAntenna=0; iAntenna < mapping.antennaCount(); iAntenna++) {
    unsigned antennaNo = iAntenna+1;
    antCorrTypeMap[antennaNo] = carma::signalpath::CORR_NONE;
  }

  //------------------------------------------------------------
  // Now iterate over all astrobands that are currently configured, to
  // construct a correlator designation mask for each antenna
  //------------------------------------------------------------

  for(unsigned iAstroBand=0; iAstroBand < carma::signalpath::AstroBand::nBandMax_; iAstroBand++) {

    unsigned astroBandNo = iAstroBand+1;
    carma::signalpath::AstroBand* astroBand = spMap_.getAstroBand(astroBandNo);

    if(astroBand->isConfigured_) {
      std::vector<AstroBandInput*>& inputs = astroBand->inputs_;

      //------------------------------------------------------------
      // Iterate over all configured inputs of this astroband
      //------------------------------------------------------------

      for(unsigned iInput=0; iInput < inputs.size(); iInput++) {

	carma::signalpath::AstroBandInput* astroBandInput = inputs[iInput];

	if(astroBandInput->isConfigured_) {

	  // Get the antenna number associated with this input

	  try {
	    CorrelatorCrateInput* crateInput = astroBandInput->getCrateInput();
	    unsigned corrType  = (unsigned)crateInput->crate_->type_;
	    unsigned antennaNo = crateInput->getBdcBand()->getBdcInput()->getSwitch()->getCurrentChannel()->getAntennaIF()->getAntenna()->antNo_;

	    // OR this correlator type into the mask for this antenna

	    unsigned prevCorrType     = (unsigned)antCorrTypeMap[antennaNo];
	    antCorrTypeMap[antennaNo] = (CorrelatorType)(prevCorrType | corrType);

	  } catch(...) {
	  }

	}
      }
    }
  }
  
  //------------------------------------------------------------
  // Now iterate through antennas, updating settings for each one
  //------------------------------------------------------------

  for(int iAntenna=0; iAntenna < mapping.antennaCount(); iAntenna++) {

    unsigned antennaNo = iAntenna+1;
    carma::signalpath::Antenna* antenna                     = spMap_.getAntenna(antennaNo);
    carma::monitor::SignalPathSubsystem::Antenna& msAntenna = mapping.antenna(iAntenna);
    
    msAntenna.antennaNo().setValue(antennaNo);

    // Set the walsh column number and validity flag

    msAntenna.walshColNo().setValue(antenna->walshCol_.walshColNo_);
    msAntenna.walshColNo().setValidity(carma::monitor::MonitorPoint::VALID);

    // Set the subarray number and validity flag

    msAntenna.subarrayNo().setValue(subarrayNumber(antenna->subarrayId_));
    msAntenna.subarrayNo().setValidity(carma::monitor::MonitorPoint::VALID);

    // Set the correlator designation value for this antenna

    msAntenna.CORRELATOR_DESIGNATION_MP().setValue(msCorrelatorType(antCorrTypeMap[antennaNo]));
  }
}

/**.......................................................................
 * Iterate through all IF switches, updating  monitor system information
 * for each one
 */
void SignalPathMapperControlImpl::updateIFSwitchMonitorPoints()
{
  carma::monitor::SignalPathSubsystem::Mapping& mapping = msSp_->mapping(); 

  // Iterate through switches, updating settings for each one

  for(unsigned iIFSwitch=0; iIFSwitch < carma::signalpath::SignalPathMap::nSwitch_; iIFSwitch++) {

    carma::signalpath::Switch* sw = spMap_.getSwitch(iIFSwitch+1);
    carma::monitor::SignalPathSubsystem::IFSwitch& msIFSwitch = 
      mapping.iFSwitch(iIFSwitch);

    carma::signalpath::SwitchChannelId channelId = carma::signalpath::SW_CHAN_NONE;

    try {
      carma::signalpath::SwitchChannel* swChan = sw->getCurrentChannel();
      channelId = swChan->channelId_;
    } catch(...) {
      msIFSwitch.antennaIFNo().setValue(0);
      msIFSwitch.antennaIFNo().setValidity(carma::monitor::MonitorPoint::INVALID_NO_HW);
    }

    // Now iterate over all switch channels, updating information
    // about the antenna IFs associated with each one

    unsigned iChan=0;
    for(std::map<SwitchChannelId, SwitchChannel*>::iterator iter = sw->channelMap_.begin();
	iter != sw->channelMap_.end(); iter++, iChan++) {

      SwitchChannel* channel = iter->second;
      carma::monitor::SignalPathSubsystem::AntennaIF& msAntIF = msIFSwitch.antennaIF(iChan);

      // Set monitor points for the antenna IF connected to this switch channel, if any

      try {
	carma::signalpath::AntennaIF* antIF = channel->getAntennaIF();
	carma::signalpath::Antenna* ant     = antIF->getAntenna();

	msAntIF.antennaNo().setValue(ant->antNo_);	
	msAntIF.antennaNo().setValidity(carma::monitor::MonitorPoint::VALID);

	msAntIF.polarization().setValue(msPolType(antIF->polType_));
	msAntIF.splitterChannel().setValue(msSplitterChannel(antIF->splitterChannel_));

      } catch(...) {
	msAntIF.antennaNo().setValue(0);	
	msAntIF.antennaNo().setValidity(carma::monitor::MonitorPoint::INVALID_NO_HW);

	msAntIF.polarization().setValue(carma::monitor::PolarizationMonitorPointEnum::UNKNOWN);
	msAntIF.splitterChannel().setValue(carma::monitor::SplitterChannelMonitorPointEnum::NONE);
      }

      // And set the 1-base index of this switchchannel if it matches
      // the currently selected switch channel

      if(channelId == iter->first) {
	msIFSwitch.antennaIFNo().setValue(iChan+1);
	msIFSwitch.antennaIFNo().setValidity(carma::monitor::MonitorPoint::VALID);
      }

    }

  }
}

/**.......................................................................
 * Iterate through all subarrays, updating monitor system information
 * for each one
 */
void SignalPathMapperControlImpl::updateSubarrayMonitorPoints()
{
  //------------------------------------------------------------
  // Create a map of the number of antnenas owned by each subarray
  //------------------------------------------------------------
  ScopedLogNdc ndc("SignalPathMapperControlImpl::updateSubarrayMonitorPoints");

  std::map<unsigned, unsigned> subarrayNantMap;

  // First initialize to zero

  for(unsigned iSubarray=0; iSubarray < SignalPathMap::nSubarray_; iSubarray++) {
    unsigned subarrayNo = (iSubarray == SignalPathMap::nSubarray_-1) ? 0 : iSubarray+1;
    subarrayNantMap[subarrayNo] = 0;
  }

  // Now increment

  for(unsigned iAntenna=0; iAntenna < SignalPathMap::nAnt_; iAntenna++) {
    unsigned antennaNo = iAntenna+1;
    carma::signalpath::Antenna* ant = spMap_.getAntenna(antennaNo);
    subarrayNantMap[subarrayNumber(ant->subarrayId_)] += 1;
  }

  //------------------------------------------------------------
  // Now construct the map of correlator designations
  //------------------------------------------------------------

  std::map<unsigned,util::CorrelatorSet> subarrayCorrSetMap;

  // First initialize to NONE

  for(unsigned iSubarray=0; iSubarray < SignalPathMap::nSubarray_; iSubarray++) {
    unsigned subarrayNo = (iSubarray == SignalPathMap::nSubarray_-1) ? 0 : iSubarray+1;
    subarrayCorrSetMap[subarrayNo].initialize(util::CORR_NONE);
  }

  // Now iterate over correlators, checking which subarray owns each
  // one

  Correlator* corr = 0;
  unsigned subarrayNo;
  unsigned corrType;

  corr = spMap_.getCorrelator(CORR_SL);
  subarrayNo = subarrayNumber(corr->subarrayId_);
  corrType = (unsigned)corr->type_;

  subarrayCorrSetMap[subarrayNo].addSpectral();

  corr = spMap_.getCorrelator(CORR_WB);
  subarrayNo = subarrayNumber(corr->subarrayId_);
  corrType = (unsigned)corr->type_;

  subarrayCorrSetMap[subarrayNo].addWideband();

  corr = spMap_.getCorrelator(CORR_C3GMAX8);
  subarrayNo = subarrayNumber(corr->subarrayId_);
  corrType = (unsigned)corr->type_;

  subarrayCorrSetMap[subarrayNo].addC3gMax8();

  corr = spMap_.getCorrelator(CORR_C3GMAX23);
  subarrayNo = subarrayNumber(corr->subarrayId_);

  subarrayCorrSetMap[subarrayNo].addC3gMax23();

  /*{ // debug
    ostringstream os;
    os << " MWP ";
    for(unsigned i = 1; i< SignalPathMap::nSubarray_+1; ++i ) {
    os << " Subarray #"<<i<<" "<<subarrayCorrSetMap[i].corrTypeString()
    << " / " << subarrayCorrSetMap[i].mpString() << " | ";
    }
    programLogNotice(os.str() );
    }*/


  //------------------------------------------------------------
  // Now iterate through subarrays, updating settings for each one
  //------------------------------------------------------------
  carma::monitor::SignalPathSubsystem::Mapping& mapping = msSp_->mapping(); 
  ms_->readNewestConditionalCopy();


  for(int iSubarray=0; iSubarray < mapping.subarrayCount(); iSubarray++) {

    unsigned subarrayNo = (unsigned)(iSubarray+1);
    carma::monitor::SignalPathSubsystem::Subarray& msSubarray = mapping.subarray(iSubarray);

    msSubarray.subarrayNo().setValue(subarrayNo);
    msSubarray.nAntInSubarray().setValue(subarrayNantMap[subarrayNo]);
    util::CorrelatorType subarrayCorrType =  
      subarrayCorrSetMap[subarrayNo].getControlCorrelatorDesignation();
    msSubarray.CORRELATOR_DESIGNATION_MP().setValue( monitor::corrTypeToCorrDes(subarrayCorrType ) );
    /* debug - mwp
    {
      ostringstream os;
      os << "MWP SIGNALPATH setting SIGNALPATH subsystem corrdesignation value to "
         << subarrayCorrType << " = " 
         << subarrayCorrSetMap[subarrayNo].mpString() 

         << " for subarray number " << subarrayNo;
      programLogNotice( os.str() );
    }
    */


    // Only update the control monitor point for this subarray if this
    // subarray has already been restored
    // That condition means we can never add a correlator during
    // state restoration.

    if(ms_->control().subarray(iSubarray).controllerInitialized().getValue()) {
      msCs_->subarray( iSubarray ).CORRELATOR_DESIGNATION_MP().setValue( monitor::corrTypeToCorrDes( subarrayCorrType ) );
    } 
  }
}

/**.......................................................................
 * Iterate through all antennas, clearing monitor system information
 * for each one
 */
void SignalPathMapperControlImpl::clearAntennaMonitorPoints()
{
  carma::monitor::SignalPathSubsystem::Mapping& mapping = msSp_->mapping(); 

  // Iterate through antennas, clearing settings for each one

  for(int iAntenna=0; iAntenna < mapping.antennaCount(); iAntenna++) {

    carma::monitor::SignalPathSubsystem::Antenna& msAntenna = mapping.antenna(iAntenna);

    msAntenna.antennaNo().setValue(iAntenna+1);

    msAntenna.walshColNo().setValue(0);
    msAntenna.walshColNo().setValidity(carma::monitor::MonitorPoint::INVALID_NO_HW);

    msAntenna.subarrayNo().setValue(0);
    msAntenna.walshColNo().setValidity(carma::monitor::MonitorPoint::INVALID_NO_HW);

    msAntenna.CORRELATOR_DESIGNATION_MP().setValue(CorrDesignation::NONE);
  }
}

/**.......................................................................
 * Iterate through all IF switches, clearing monitor system information
 * for each one
 */
void SignalPathMapperControlImpl::clearIFSwitchMonitorPoints()
{
  carma::monitor::SignalPathSubsystem::Mapping& mapping = msSp_->mapping(); 

  // Iterate through switches, clearing settings for each one

  for(int iIFSwitch=0; iIFSwitch < mapping.iFSwitchCount(); iIFSwitch++) {

    carma::monitor::SignalPathSubsystem::IFSwitch& msIFSwitch = 
      mapping.iFSwitch(iIFSwitch);

    msIFSwitch.antennaIFNo().setValue(0);
    msIFSwitch.antennaIFNo().setValidity(carma::monitor::MonitorPoint::INVALID_NO_HW);

    // Now iterate over antenna IFs associated with this switch

    for(int iAntIF=0; iAntIF < msIFSwitch.antennaIFCount(); iAntIF++) {

      carma::monitor::SignalPathSubsystem::AntennaIF& msAntIF = msIFSwitch.antennaIF(iAntIF);

      msAntIF.antennaNo().setValue(0);
      msAntIF.antennaNo().setValidity(carma::monitor::MonitorPoint::INVALID_NO_HW);

      msAntIF.polarization().setValue(carma::monitor::PolarizationMonitorPointEnum::UNKNOWN);
      msAntIF.splitterChannel().setValue(carma::monitor::SplitterChannelMonitorPointEnum::NONE);
    }

  }
}

/**.......................................................................
 * Iterate through all subarrays, clearing monitor system information
 * for each one
 */
void SignalPathMapperControlImpl::clearSubarrayMonitorPoints()
{
  carma::monitor::SignalPathSubsystem::Mapping& mapping = msSp_->mapping(); 

  // Iterate through subarrays, clearing settings for each one

  for(int iSubarray=0; iSubarray < mapping.subarrayCount(); iSubarray++) {

    unsigned subarrayNo = iSubarray+1;

    carma::monitor::SignalPathSubsystem::Subarray& msSubarray = mapping.subarray(iSubarray);

    msSubarray.subarrayNo().setValue(subarrayNo);
    msSubarray.nAntInSubarray().setValue(0);
    msSubarray.CORRELATOR_DESIGNATION_MP().setValue(CorrDesignation::NONE);

    // Only update the control monitor point for this subarray if this
    // subarray has already been restored

    if(ms_->control().subarray(iSubarray).controllerInitialized().getValue()) {
      msCs_->subarray( iSubarray ).CORRELATOR_DESIGNATION_MP().setValue(CorrDesignation::NONE);
    }
  }
}

/**.......................................................................
 * Check our internal configuration against the current state of the hardware, as 
 * reported in the monitor stream
 */
void SignalPathMapperControlImpl::checkConfigurationSuccess(unsigned short astroBandNo)
{
  CORBA_WRAPPER(checkConfigurationSuccessWrapper(astroBandNo));
}

/**.......................................................................
 * Check our internal configuration against the current state of the hardware, as 
 * reported in the monitor stream
 */
void SignalPathMapperControlImpl::checkConfigurationSuccessWrapper(unsigned short astroBandNo)
{
  //------------------------------------------------------------
  // Get the frame count.  Note that getFrameCount reads the monitor
  // system, so we don't need to do this separately
  //------------------------------------------------------------
  
  int frameCount = getFrameCount();

  if(astroBandNo == 0) {
    std::vector<unsigned> astroBandNos =  spMap_.getActiveAstroBandNos(CORR_ALL);
    for(unsigned iAstroBand=0; iAstroBand < astroBandNos.size(); iAstroBand++) {
      try {
        checkConfigurationSuccessLocal(astroBandNos[iAstroBand], frameCount);
      } catch(...) {
        continue;
      }
    }
  } else {
    checkConfigurationSuccessLocal(astroBandNo, frameCount);
  }
}

/**.......................................................................
 * Check our internal configuration against the current state of the hardware, as 
 * reported in the monitor stream
 */
void SignalPathMapperControlImpl::checkConfigurationSuccessLocal(unsigned short astroBandNo, int frameCount)
{
  try {

    //------------------------------------------------------------
    // Check the last modified time for this astroband.  
    //
    // We will only check configurations that were modified > 10
    // frames in the past, since there is no guarantee that the
    // configuration will have been updated in the monitor stream any
    // sooner than 2 frames from the issuance of a command that
    // modifies the hardware, and in practice, even the lastModified
    // stamps seem to take about 4 frames to show up in the monitor
    // stream.
    //
    // To be safe, I'm imposing a limit of 10 frames on the check
    //------------------------------------------------------------

    int lastModified = lastModifiedVec_[astroBandNo-1];

    if(frameCount > lastModified && (frameCount-lastModified) < 10) {
      return;
    }

    //------------------------------------------------------------
    // Check IF switch settings    
    //------------------------------------------------------------
    
    std::vector<SwitchSetting> swSet = spMap_.getIFSwitchSettings(astroBandNo);

    for(unsigned iSwSet=0; iSwSet < swSet.size(); iSwSet++) {
      checkIFSwitchSetting(swSet[iSwSet]);
    }

    //------------------------------------------------------------
    // Check DC LO switch settings    
    //------------------------------------------------------------
    
    swSet = getDCLOSwitchSettingsLocal(astroBandNo);

    for(unsigned iSwSet=0; iSwSet < swSet.size(); iSwSet++) {
      checkDCLOSwitchSetting(swSet[iSwSet]);
    }

    //------------------------------------------------------------
    // Get the vector of antennas involved in this astroband that
    // also belong to the subarray that configured the astroband
    //------------------------------------------------------------
    
    std::vector<AntennaSpec> antVec = spMap_.getAntennas(astroBandNo, false);

    //------------------------------------------------------------
    // Check LO switch settings    
    //------------------------------------------------------------
    
    for(unsigned iAnt=0; iAnt < antVec.size(); iAnt++) {
      checkLOSwitchSetting(antVec[iAnt].antNo_, switchPos(antVec[iAnt].subarrayId_));
    }

    //------------------------------------------------------------
    // Check LL switch settings    
    //------------------------------------------------------------
    
    for(unsigned iAnt=0; iAnt < antVec.size(); iAnt++) {
      checkLLSwitchSetting(antVec[iAnt].antNo_, switchPos(antVec[iAnt].subarrayId_));
    }
    
    //------------------------------------------------------------
    // Check Walsh column assignments for antennas involved in this
    // astroband only
    //------------------------------------------------------------
    
    for(unsigned iAnt=0; iAnt < antVec.size(); iAnt++) {
      checkWalshColumnAssignment(antVec[iAnt].antNo_, antVec[iAnt].walshColNo_);
    }

    //------------------------------------------------------------
    // Check BDC settings
    //------------------------------------------------------------
    
    std::vector<BlockDownconverterSetting> bdcSet = spMap_.getBdcSettings(astroBandNo);

    for(unsigned iBdcSet=0; iBdcSet < bdcSet.size(); iBdcSet++) {
      checkBdcSetting(bdcSet[iBdcSet]);
    }
    
    //------------------------------------------------------------
    // Check fpga modes
    //------------------------------------------------------------
    
    unsigned iAstroBandStart = astroBandNo==0 ? 0 : astroBandNo-1;
    unsigned iAstroBandStop  = astroBandNo==0 ? AstroBand::nBandMax_ : astroBandNo;
    
    for(unsigned iAstroBand=iAstroBandStart; iAstroBand < iAstroBandStop; iAstroBand++) {

      std::vector<carma::signalpath::CorrelatorBandSpec> bandVec = spMap_.getCorrelatorBands(iAstroBand+1);
      
      if(bandVec.size() > 0) {

	carma::util::CorrelatorFpgaModeType fpgaMode = getFpgaModeLocal(iAstroBand+1);

	for(unsigned iCorrBand=0; iCorrBand < bandVec.size(); iCorrBand++) {
	  checkFpgaMode(bandVec[iCorrBand], fpgaMode);
	}
      }
    }
  
    //------------------------------------------------------------
    // Write to monitor system to indicate valid hardware configuration
    //------------------------------------------------------------
    
    markHardwareConfigurationAsValid(astroBandNo, true);

  } catch(Exception& err) {

    //------------------------------------------------------------
    // Write to monitor point to indicate invalid hardware
    // configuration.  But only if the astroband number is valid!
    //------------------------------------------------------------
    
    if(astroBandNo <= carma::signalpath::AstroBand::nBandMax_)
      markHardwareConfigurationAsValid(astroBandNo, false);

    // And rethrow

    throw err;
  }
}

/**.......................................................................
 * Set the validity of the current hardware configuration.
 */
void SignalPathMapperControlImpl::
markHardwareConfigurationAsValid(unsigned short astroBandNo, bool valid)
{
  unsigned iAstroBandStart = (astroBandNo == 0) ? 0 : astroBandNo-1;
  unsigned iAstroBandStop  = (astroBandNo == 0) ? carma::signalpath::AstroBand::nBandMax_-1 : astroBandNo-1;

  for(unsigned iAstroBand=iAstroBandStart; iAstroBand <= iAstroBandStop; iAstroBand++) {

    unsigned currAstroBandNo = iAstroBand + 1;

    // If the astroband configuration is valid, remove the bit from the
    // invalid mask.  Else, set it.

    if(valid) {
      invalidMask_.setBitLow(currAstroBandNo);
    } else {
      invalidMask_.setBitHigh(currAstroBandNo);
    }

    // Set the validity flag for this astroband

    msSp_->mapping().astroband(iAstroBand).hardwareConfValid().setValue(valid);
  }

  // If after setting bits for the individual astroband, no bands are
  // invalid, set the global validity flag to true.  Else set it to
  // false

  if(invalidMask_.allBitsAreLow())
    msSp_->mapping().hardwareConfValid().setValue(true);
  else
    msSp_->mapping().hardwareConfValid().setValue(false);
}

/**.......................................................................
 * Check a single walsh column assignment against the monitor stream
 */
void SignalPathMapperControlImpl::
checkWalshColumnAssignment(unsigned antNo, unsigned walshColNo)
{
  if(walshColNo > 0) {
    unsigned mpWalshColNo = (unsigned) ms_->loberotator().channel(antNo-1).walshColumn().getValue();

    if(mpWalshColNo != walshColNo) {
      ThrowColorError("Walsh column number from Loberotator.Channel" << antNo << " (" << mpWalshColNo 
		      << ") is unexpected.  Should be "<< walshColNo, "red");
    }
  }
}

/**.......................................................................
 * Check a single FPGA mode against the monitor stream
 */
void SignalPathMapperControlImpl::
checkFpgaMode(carma::signalpath::CorrelatorBandSpec& band, carma::util::CorrelatorFpgaModeType spFpgaMode)
{
  carma::monitor::ControlBandPoints::FpgaModeMonitorPointEnum::FPGAMODE mpFpgaMode;

  if(band.crate_.type_ == CORR_SL) {
    mpFpgaMode = ms_->control().spectralLineCorrelator().slcBand(band.bandNo_-1).controlBandPoints().fpgaMode().getValue();

    if(mpFpgaMode != msFpgaMode(spFpgaMode)) {
      ThrowColorError("FPGA mode for Control.SpectralLineCorrelator.SlcBand" << band.bandNo_ 
		      << ".ControlBandPoints.fpgaMode"
		      << "(" << mpFpgaMode << ") is unexpected.  Should be " << msFpgaMode(spFpgaMode), "red");
    }
  } else {
    mpFpgaMode = ms_->control().widebandCorrelator().wbcBand(band.bandNo_-1).controlBandPoints().fpgaMode().getValue();

    if(mpFpgaMode != msFpgaMode(spFpgaMode)) {
      ThrowColorError("FPGA mode for Control.WidebandCorrelator.WbcBand" << band.bandNo_ 
		      << ".ControlBandPoints.fpgaMode"
		      << "(" << mpFpgaMode << ") is unexpected.  Should be " << msFpgaMode(spFpgaMode), "red");
    }
  }
}

/**.......................................................................
 * Check a single BDC setting against the monitor stream
 */
void SignalPathMapperControlImpl::
checkBdcSetting(BlockDownconverterSetting& setting)
{
  // Only check spectral-line block downconverters

  if(setting.bdcNo_ <= SignalPathMap::nSl_) {
    carma::monitor::BlockDownconverter::PolarizationMonitorPointEnum::POLARIZATION pol = 
      ms_->sldc().blockDownconverterContainer(setting.bdcNo_-1).blockDownconverter().polarization(setting.bandNo_-1).getValue();
    
    if(pol != msPolType(setting.input_)) {
      ThrowColorError("Polarization for Sldc.BlockDownconverterContainer" << setting.bdcNo_
		      << ".BlockDownconverter.polarization" << setting.bandNo_ 
		      << " (" << pol << ") is incorrect for the current configuration.  Should be "
		      << msPolType(setting.input_), "red");
    }
  }
}

/**.......................................................................
 * Check a single IF switch setting against the monitor stream
 */
void SignalPathMapperControlImpl::
checkIFSwitchSetting(carma::signalpath::SwitchSetting& swPos)
{
  unsigned short msSwPos = 
    ms_->signalPath().iFSwitchyard().switchyard().switchPosition(swPos.switchNo_-1).getValue();

  unsigned short spSwPos = spMap_.switchChannelIdToChannelNumber(swPos.channel_);

  if(msSwPos != spSwPos) {
    ThrowColorError("IF Switch " << swPos.switchNo_ << " is in the wrong position (" 
		    << msSwPos << ") for the current configuration."
		    << " Should be " << spSwPos, "red");
  }
}

/**.......................................................................
 * Check a single DC LO switch setting against the monitor stream
 */
void SignalPathMapperControlImpl::
checkDCLOSwitchSetting(carma::signalpath::SwitchSetting& swPos)
{
  unsigned short msSwPos = 
    ms_->signalPath().dCLOSwitchyard().switchyard().switchPosition(swPos.switchNo_-1).getValue();

  unsigned short spSwPos = spMap_.switchChannelIdToChannelNumber(swPos.channel_);

  if(msSwPos != spSwPos) {
    ThrowColorError("DC LO Switch " << swPos.switchNo_ << " is in the wrong position (" 
		    << msSwPos << ") for the current configuration."
		    << " Should be " << spSwPos, "red");
  }
}

/**.......................................................................
 * Check a single LO switch setting against the monitor stream
 */
void SignalPathMapperControlImpl::
checkLOSwitchSetting(unsigned short switchNo, unsigned short switchPos)
{
  unsigned short msSwPos = 
    ms_->signalPath().lOSwitchyard().switchyard().switchPosition(switchNo-1).getValue();

  unsigned short spSwPos = switchPos;

  if(msSwPos != spSwPos) {
    ThrowColorError("LO Switch " << switchNo << " is in the wrong position (" 
		    << msSwPos << ") for the current configuration."
		    << " Should be " << spSwPos, "red");
  }
}

/**.......................................................................
 * Check a single LL switch setting against the monitor stream
 */
void SignalPathMapperControlImpl::
checkLLSwitchSetting(unsigned short switchNo, unsigned short switchPos)
{
  unsigned short msSwPos = ms_->signalPath().lLSwitchyard().switchyard().switchPosition(switchNo-1).getValue();
  unsigned short spSwPos = switchPos;

  if(msSwPos != spSwPos) {
    ThrowColorError("LL Switch " << switchNo << " is in the wrong position (" << msSwPos 
		    << ") for the current configuration."
		    << " Should be " << spSwPos, "red");
  }
}

char * SignalPathMapperControlImpl::
getConfName(unsigned short astroBandNo) 
{
  CORBA_WRAPPER(

		string config("NONE");
		AstroBand * ab = spMap_.getAstroBand(astroBandNo);
		if ( ab != 0 ) {
		  signalpath::SwitchyardConfiguration * swc = ab->getSwitchyardConfiguration();
		  if ( swc != 0 ) {
		    signalpath::AstroBandConfiguration * abc = swc->getAstroBandConfiguration();
		    if ( abc != 0 ) {
		      config = (*abc).name_;
		    }
		  }
		}

		return CORBA::string_dup(config.c_str());

		);
}

short SignalPathMapperControlImpl::
getAstroBandForCorrelatorBand(unsigned short corrBandNo, 
                              util::CorrelatorType type )
{
  short value = -1;
  CORBA_WRAPPER(
		const signalpath::CorrelatorType sct = correlatorType(type);
		vector<unsigned> abNo = spMap_.getActiveAstroBandNos( sct );
		vector<unsigned>::const_iterator iBegin = abNo.begin();
		vector<unsigned>::const_iterator   iEnd = abNo.end();
		for(vector<unsigned>::const_iterator i = iBegin; i != iEnd; ++i) {
		  AstroBand * iBand = spMap_.getAstroBand(*i);
		  if ( iBand == 0 ) continue;
		  vector<AstroBandInput*>::const_iterator aiBegin = iBand->inputs_.begin();
		  vector<AstroBandInput*>::const_iterator   aiEnd = iBand->inputs_.end();
		  for(vector<AstroBandInput*>::const_iterator j = aiBegin; j != aiEnd; ++j) 
		  {
		    // Unassigned astrobands can have null pointers for their
		    // members. Be sure to check for them (good coding practice anyway!)
		    if ( &(*j) == 0 ) continue;

		    AstroBandInput* abInput = *j;
		    CorrelatorBand* band = 0;

		    if(abInput->isConnectedToCrate()) {
		      try {
			band = abInput->getCrateInput()->getCorrelatorBandInput()->band_;
		      } catch(...) {
			continue;
		      }
		    } else if(abInput->isConnectedToBand()) {
		      try {
			band = abInput->getBandInput()->band_;
		      } catch(...) {
			continue;
		      }
		    } else 
		      continue;

		    if(band->bandNo_ == corrBandNo)
		      return iBand->bandNo_;
		  }
		}

		return value;

		);
}

// get correlator type associated with this astroband.

util::CorrelatorType 
SignalPathMapperControlImpl::getCorrTypeForAstroBand(unsigned short astroBandNo)
{
  util::CorrelatorType defaultType = carma::util::CORR_NONE;

  CORBA_WRAPPER(
		AstroBand * iBand = spMap_.getAstroBand(astroBandNo);
		if ( iBand == 0 ) return carma::util::CORR_NONE;
		vector<AstroBandInput*>::const_iterator aiBegin = iBand->inputs_.begin();
		vector<AstroBandInput*>::const_iterator   aiEnd = iBand->inputs_.end();

		for(vector<AstroBandInput*>::const_iterator j = aiBegin; j != aiEnd; ++j) 
		{
		  // Unassigned astrobands can have null pointers for their
		  // members. Be sure to check for them (good coding practice anyway!)
		  if ( &(*j) == 0 ) return carma::util::CORR_NONE;

		  AstroBandInput* abInput = *j;

		  if(abInput->isConnectedToCrate()) {
		    signalpath::CorrelatorCrateInput* corrInput = abInput->getCrateInput();

		    // continue to next crate if this one is null.
		    // If they all are null, then the initial value of
		    // cType will be returned (CORR_NONE).

		    if(corrInput->crate_ == 0) 
		      continue;
		    else 
		      return correlatorType(corrInput->crate_->type_);
              
		  } else if(abInput->isConnectedToBand()) {
		    return correlatorType(abInput->band_->type_);
		  } else
		    continue;
		}
		return defaultType;
		);
}
