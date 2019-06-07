#ifndef CARMA_SIGNALPATH_SIGNALPATHCONTROLIMPL_H
#define CARMA_SIGNALPATH_SIGNALPATHCONTROLIMPL_H

#include "carma/control/CorrDefs.h"

#include "carma/monitor/BlockDownconverter.h"
#include "carma/monitor/ControlBandCommon.h"
#include "carma/monitor/CorrDesignation.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/SignalPathSubsystem.h"

#include "carma/signalpath/SignalPathMapperControl.h"
#include "carma/signalpath/SignalPathMap.h"

#include "carma/util/PthreadMutex.h"
#include "carma/util/SeqTypedefs.h"
#include "carma/util/corrUtils.h"

#include "carma/szautil/AbsoluteTimer.h"
#include "carma/szautil/BitMask.h"
#include "carma/szautil/PeriodicTimer.h"
#include "carma/szautil/String.h"

namespace carma {
  namespace signalpath {

    class SignalPathMapperControlImpl {
    public:

      class SwitchyardConfiguration {
      public:
	std::string fileName_;
	sza::util::String confName_;
	sza::util::String astroBandConfName_;
	carma::util::CorrelatorFpgaModeType fpgaMode_;
	SignalPathMapperControl::SwitchyardConfiguration tag_;

	SwitchyardConfiguration(std::string confName, 
				SignalPathMapperControl::SwitchyardConfiguration tag, 
				std::string astroBandConfName,
				std::string fileName) 
	{
	  confName_          = confName;
	  confName_          = confName_.toUpper();

	  tag_               = tag;

	  astroBandConfName_ = astroBandConfName;
	  astroBandConfName_ = astroBandConfName_.toUpper();

	  fileName_          = fileName;
	};

	SwitchyardConfiguration(const SwitchyardConfiguration& conf) {
	  *this = conf;
	};

	void operator=(const SwitchyardConfiguration& conf) {
	  *this = (SwitchyardConfiguration&) conf;
	};

	void operator=(SwitchyardConfiguration& conf) {
	  fileName_          = conf.fileName_;
	  confName_          = conf.confName_;
	  astroBandConfName_ = conf.astroBandConfName_;
	  tag_               = conf.tag_;
	};

      };

      SignalPathMapperControlImpl();
    
      ~SignalPathMapperControlImpl();

      //-----------------------------------------------------------------------
      // Begin IDL interface
      //-----------------------------------------------------------------------

      // Initialize the hardware (cable) connections from a file

      void initializeCableMap(std::string fileName);

      // Load a new configuration from a file

      void loadConfiguration(std::string fileName, std::string confName, std::string astroBandConfName);

      // Add an antenna

      void addAntenna(unsigned short antNo, unsigned short subarrayNo);

      // Remove an antenna

      void removeAntenna(unsigned short antNo, unsigned short subarrayNo);

      // Add a correlator

      void addCorrelator(carma::util::CorrelatorType type, unsigned short subarrayNo);

      // Remove a correlator

      void removeCorrelator(carma::util::CorrelatorType type, unsigned short subarrayNo);

      // Assert a named configuration for the specified astro band

      void configureAstroBand(unsigned short bandNo, std::string confName, 
			      unsigned short subarrayNo=0,      
			      carma::util::CorrelatorType type=carma::util::CORR_ALL);

      // Check the validity of a configuration

      void checkConfigurationValidity(unsigned short bandNo, std::string confName, 
				      unsigned short subarrayNo=0,      
				      carma::util::CorrelatorType type=carma::util::CORR_ALL);

      // Return the validity of a configuration
 
     bool configurationIsValid(unsigned short bandNo, std::string confName, 
			       unsigned short subarrayNo=0,      
			       carma::util::CorrelatorType type=carma::util::CORR_ALL);

      // Clear any configuration of the specified astro band

     void clearAstroBandConfiguration(unsigned short bandNo, 
				      unsigned short subarrayNo=0,      
				      carma::util::CorrelatorType type=carma::util::CORR_ALL);

      // Set a walsh column for the named antenna

      void assignWalshColumn(SignalPathMapperControl::WalshColumnAssignment wca);

      // Clear a walsh column assignment

      void clearWalshColumnAssignment(unsigned short antNo);
      
      // Get the switch positions for the current configuration

      carma::switchyard::SwitchPositionSeq* 
	getIFSwitchSettings(unsigned short astroBandNo);

      carma::switchyard::SwitchPositionSeq* 
	getLOSwitchSettings(unsigned short astroBandNo);

      carma::switchyard::SwitchPositionSeq* 
	getDCLOSwitchSettings(unsigned short astroBandNo);

      carma::switchyard::SwitchPositionSeq* 
	getLLSwitchSettings(unsigned short astroBandNo);

      // Get the FPGA mode of this astro band

      carma::util::CorrelatorFpgaModeType 
	getFpgaMode(unsigned short astroBandNo);

      // Get antennas associated with the requested astroband

      SignalPathMapperControl::AntennaSeq* 
	getAntennas(unsigned short astroBandNo=0);

      // Get polarizations associated with the requested astroband
      
      SignalPathMapperControl::PolarizationSeq* 
	getPolarizations(unsigned short astroBandNo=0);

      // Get block downconverter settings for the requested astroband

      SignalPathMapperControl::BlockDownconverterSettingSeq* 
	getBdcSettings(unsigned short astroBandNo=0);

      // Get the walsh column assignments for all antennas

      SignalPathMapperControl::WalshColumnAssignmentSeq* 
	getWalshColumnAssignment(unsigned short antNo=0);

      // Query active correlator bands managed by this correlator

      SignalPathMapperControl::CorrelatorBandSeq* 
	getActiveCorrelatorBands(carma::util::CorrelatorType type);

      // Query correlator band input mapping

      SignalPathMapperControl::CorrelatorBandInputSeq* 
	getCorrelatorBandInputMap(SignalPathMapperControl::CorrelatorBand band);
      
      // Query corelator bands associated with this astroband

      SignalPathMapperControl::CorrelatorBandSeq* 
	getCorrelatorBands(unsigned short astroBandNo);

      // Get correlator band numbers  associated with this astroband

      carma::util::SeqShort * getCorrelatorBandNoSeq(unsigned short astroBandNo);

      // Get astro band numbers  associated with this correlator

      carma::util::SeqShort * getActiveAstroBandNoSeq(carma::util::CorrelatorType type);

      // Query active correlator crates managed by this correlator

      SignalPathMapperControl::CorrelatorCrateSeq* 
	getActiveCorrelatorCrates(carma::util::CorrelatorType type);

      // Query active astrobands bands managed by this correlator

      SignalPathMapperControl::AstroBandSeq* 
	getActiveAstroBands(carma::util::CorrelatorType type);

      //  Query astrobands specified by this configuration

      SignalPathMapperControl::AstroBandSeq* 
	getAstroBandsForConfiguration(std::string confName,
				      unsigned short subarrayNo, 
				      carma::util::CorrelatorType type);

      // Query correlator crate input mapping

      SignalPathMapperControl::CorrelatorCrateInputSeq* 
	getCorrelatorCrateInputMap(SignalPathMapperControl::CorrelatorCrate crate);

      // Returns true if the switch setting does not conflict with any
      // current Astroband configuration

      bool canAssertSwitchPosition(carma::switchyard::SwitchPosition pos);

      // Return the configuration as a printable string

      char* queryConfiguration();

      // Check configuration by synchronizing with the monitor stream

      void checkConfigurationSuccess(unsigned short astroBandNo);

      // get the switchyard configuration name for the input astroband number
      char * getConfName(unsigned short astroBandNo );

      // get the astroband number associated with this correlator band
      // or -1 if no association.

      short getAstroBandForCorrelatorBand(unsigned short corrBandNo, 
					  carma::util::CorrelatorType type );

      // Get the correlator type associated with this astroband.

      carma::util::CorrelatorType getCorrTypeForAstroBand(unsigned short astroBandNo);

      //-----------------------------------------------------------------------
      // End IDL interface
      //-----------------------------------------------------------------------

    private:

      // A bitmask of validity for all astrobands

      sza::util::BitMask invalidMask_;

      std::vector<int> lastModifiedVec_;

      sza::util::AbsoluteTimer* writeMonitorSystemTimer_;

      sza::util::PeriodicTimer* checkConfigTimer_;
      bool checkConfigEnabled_;

      std::vector<unsigned> savedAstroBands_;
      std::vector<CorrelatorBandSpec> savedCorrelatorBands_;

      std::vector<carma::signalpath::SignalPathMapperControlImpl::SwitchyardConfiguration*> knownConfigurations_;

      carma::util::PthreadMutex guard_;

      carma::signalpath::SignalPathMap spMap_;
      carma::signalpath::SignalPathMap spMapChecker_;

      carma::monitor::CarmaMonitorSystem* ms_;
      carma::monitor::SignalPathSubsystem* msSp_;
      carma::monitor::ControlSubsystem* msCs_;

      // Methods to do with the monitor system writer

      void initializeMonitorSystemWriter();
      static ABSOLUTE_TIMER_HANDLER(writeMonitorSystemHandler);
      void writeMonitorSystemWrapper();

      // Methods to do with the configuration checker

      void initializeConfigurationChecker();
      void enableConfigurationChecker(bool enable);
      static PERIODIC_TIMER_HANDLER(checkConfigHandler);

      void checkConfigurationSuccessWrapper(unsigned short astroBandNo);
      void checkConfigurationSuccessLocal(unsigned short astroBandNo, int frameCount);

      void addCorrelatorLocal(carma::util::CorrelatorType type, unsigned short subarrayNo);

      void removeCorrelatorLocal(carma::util::CorrelatorType type, unsigned short subarrayNo);

      // Other methods

      void markHardwareConfigurationAsValid(unsigned short astroBandNo, bool valid);


      void clearAstroBandConfigurationLocal(unsigned short bandNo, 
					    unsigned short subarrayNo=0,
					    carma::util::CorrelatorType type=carma::util::CORR_ALL);

      SignalPathMapperControl::AntennaType 
	antType(carma::signalpath::AntennaType type); 

      unsigned short 
	switchPos(carma::signalpath::SubarrayId);

      unsigned short 
	subarrayNumber(carma::signalpath::SubarrayId);

      carma::signalpath::SubarrayId
	subarrayId(unsigned short subarrayNo);

      carma::signalpath::CorrelatorType correlatorType(carma::util::CorrelatorType type);

      carma::util::CorrelatorType 
        correlatorType(carma::signalpath::CorrelatorType type);

      MonitorCorrelatorDesignation
        msCorrelatorType(carma::signalpath::CorrelatorType type);

      carma::signalpath::SignalPathMapperControl::PolarizationType 
        polType(carma::signalpath::PolarizationType type);

      carma::monitor::PolarizationMonitorPointEnum::POLARIZATION
        msPolType(carma::signalpath::PolarizationType type);

      carma::monitor::BlockDownconverter::PolarizationMonitorPointEnum::POLARIZATION 
        msPolType(carma::downconverter::BlockDownconverterControl::Polarization type);

      carma::monitor::BlockDownconverter::PolarizationMonitorPointEnum::POLARIZATION 
        msPolType(carma::signalpath::BlockDownconverterInputType type);

      carma::monitor::SplitterChannelMonitorPointEnum::SPLITTERCHANNEL
        msSplitterChannel(carma::signalpath::SplitterChannelId channelId);

      carma::monitor::ConfTagMonitorPointEnum::CONFTAG
	msConfTag(std::string name);

      carma::monitor::ConfTagMonitorPointEnum::CONFTAG
	msConfTag(carma::signalpath::SignalPathMapperControl::SwitchyardConfiguration confTag);

      carma::util::CorrelatorFpgaModeType
	astroBandConfToCorrFpgaMode(std::string swConfName, std::string abConfName);

      carma::monitor::ControlBandPoints::FpgaModeMonitorPointEnum::FPGAMODE
	msFpgaMode(carma::util::CorrelatorFpgaModeType type);

      void checkIFSwitchSetting(carma::signalpath::SwitchSetting& swPos);
      void checkLOSwitchSetting(unsigned short switchNo, unsigned short switchPos);
      void checkDCLOSwitchSetting(carma::signalpath::SwitchSetting& swPos);
      void checkLLSwitchSetting(unsigned short switchNo, unsigned short switchPos);
      void checkBdcSetting(BlockDownconverterSetting& setting);
      void checkFpgaMode(carma::signalpath::CorrelatorBandSpec& band, carma::util::CorrelatorFpgaModeType spFpgaMode);
      void checkWalshColumnAssignment(unsigned antNo, unsigned walshColNo);

      void initialize();

      void updateMonitorPoints();

      void updateAstroBandMonitorPoints();
      void clearAstroBandMonitorPoints();

      void updateCorrBandMonitorPoints();
      void clearCorrBandMonitorPoints();
      void updateCorrBandMonitorPoints(carma::signalpath::CorrelatorType type, bool clear);
      
      void updateIFSwitchMonitorPoints();
      void clearIFSwitchMonitorPoints();

      void updateAntennaMonitorPoints();
      void clearAntennaMonitorPoints();

      void updateSubarrayMonitorPoints();
      void clearSubarrayMonitorPoints();

      carma::util::CorrelatorFpgaModeType getFpgaModeLocal(unsigned short astroBandNo);
      
      std::vector<carma::signalpath::SwitchSetting>
	getDCLOSwitchSettingsLocal(unsigned short astroBandNo);


      void saveCurrentConfiguration(unsigned astroBandNo, carma::util::CorrelatorType type);

      int getFrameCount();
      void markSavedConfigurationAsModified(int frameCount);

      void markAstroBandAsModified(unsigned astroBandNo, int frameCount);
      void markCorrelatorBandAsModified(CorrelatorBandSpec& band, int frameCount);

      std::vector<unsigned> 
	getActiveAstroBands(unsigned astroBandNo, carma::util::CorrelatorType type);

      std::vector<CorrelatorBandSpec> 
	getActiveCorrelatorBands(unsigned astroBandNo, carma::util::CorrelatorType type);

    }; // class SignalpathControlImpl

  } // namespace signalpath

} // namespace carma

#endif
