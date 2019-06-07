#include <iostream>

#include "carma/corba/Client.h"
#include "carma/util/UserException.h"
#include "carma/util/CorrelatorType.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/Program.h"
#include "carma/szautil/String.h"

#include "carma/signalpath/SignalPathMapperControl_skel.h"


using namespace std;
using namespace sza::util;
using namespace carma::util;

PROGRAM_KEYWORDS = {
  { "printconf",  "t",       "b", USAGE "True to print final configuration"},
  { "abtest",     "f",       "b", USAGE "True to test configuring astrobands"},
  { "band",       "1",       "i", USAGE "Astro band to configure (or corr band to list)"},
  { "conf",       "carma23", "s", USAGE "Configuration to assign"},
  { "corr",       "all",     "s", USAGE "Correlator type"},
  { "sa",         "1",       "i", USAGE "Subarray number"},

  { "clear",      "f",       "b", USAGE "True to clear an astroband configuration"},
  { "check",      "f",       "b", USAGE "True to check an astroband configuration"},

  { "conffile",   "",        "s", USAGE "Configuration file to load"},
  { "confname",   "test",    "s", USAGE "Configuration name to assign it"},
  { "abconfname", "single",  "s", USAGE "Astro band configuration to assign it"},
  { "load",       "f",       "b", USAGE "True to load a configuration file"},

  { "cablemap",   "",        "s", USAGE "Cable map to load"},
  { "loadcablemap", "f",     "b", USAGE "True to load a cable map"},

  { "walsh",      "f",       "b", USAGE "True to assign a walsh column"},
  { "antno",      "1",       "i", USAGE "Antenna to configure"},
  { "walshcol",   "1",       "i", USAGE "Walsh column to use"},

  { "qllsw",      "f",       "b", USAGE "True to query LL switch settings"},

  { "qlosw",      "f",       "b", USAGE "True to query LO switch settings"},

  { "qifsw",      "f",       "b", USAGE "True to query IF switch settings"},

  { "qdclosw",    "f",       "b", USAGE "True to query DC LO switch settings"},

  { "qant",       "f",       "b", USAGE "True to query antennas"},

  { "qpol",       "f",       "b", USAGE "True to query polarizations"},

  { "qbdc",       "f",       "b", USAGE "True to query BDC settings"},

  { "qab",        "f",       "b", USAGE "True to query astrobands specified by a configuration"},

  { "qwalsh",     "f",       "b", USAGE "True to query Walsh column assignments"},

  { "qcorrbands", "f",       "b", USAGE "True to query correlator band mapping"},
  
  { "qcorrbandmap", "f",     "b", USAGE "True to query input mapping for a correlator"},

  { "qallbands",   "f",      "b", USAGE "True to query all bands"},

  { "qfpga",       "f",      "b", USAGE "True to query FPGA modes"},

  { "qsuccess",    "f",      "b", USAGE "True to query success of a band configuration"},

  { "testaddant",  "f",      "b", USAGE "True to add/remove an antenna"},
  { "addant",      "f",      "b", USAGE "If testaddant=true, addant=true means add, addant=false means remove"},
  { "ant",         "0",      "i", USAGE "Antenna to add/remove"},
  { "allant",      "f",      "b", USAGE "If true, list all antennas involved in an astroband.  If false, list only antennas also belonging to the subarray"},
  {"testaddcorr",  "f",      "b", USAGE "True to add/aremove an antenna"},
  {"addcorr",      "f",      "b", USAGE "If testaddcorr=true, addcorr=true means add, addcorr=false means remove"},
  {"defaultconf",  "f",      "b", USAGE "Initialize the system to default configuration"},
  {"doname",       "carma.signalpathmapper",     "s", USAGE "The DO name to resolve"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

carma::util::CorrelatorType corrNameToCorrType(std::string name);

int Program::main(void)
{
  try {


    //-----------------------------------------------------------------------
    // Get a handle to the SPM control object.
    //-----------------------------------------------------------------------
    std::ostringstream os;
    os << Program::getParameter("doname");
    carma::signalpath::SignalPathMapperControl_var spControl_ = 0;
    carma::corba::Client & client = Program::getProgram().getCorbaClient();
    spControl_ = client.resolveName<carma::signalpath::SignalPathMapperControl>(os.str());
    
    //-----------------------------------------------------------------------
    // Initialize the system
    //-----------------------------------------------------------------------

    if(Program::getBoolParameter("defaultconf")) {

      COUT("Removing correlators from any existing configuration");

      spControl_->removeCorrelator(carma::util::CORR_SPECTRAL, 1);
      spControl_->removeCorrelator(carma::util::CORR_SPECTRAL, 2);
      spControl_->removeCorrelator(carma::util::CORR_WIDEBAND, 1);
      spControl_->removeCorrelator(carma::util::CORR_WIDEBAND, 2);

      COUT("Adding correlators to default subarrays");

      spControl_->addCorrelator(carma::util::CORR_SPECTRAL, 1);
      spControl_->addCorrelator(carma::util::CORR_WIDEBAND, 2);

      COUT("Adding antennas to default subarrays");

      for(unsigned iAnt=1; iAnt <= 15; iAnt++) 
        spControl_->addAntenna(iAnt, 1);

      for(unsigned iAnt=16; iAnt <= 23; iAnt++) 
        spControl_->addAntenna(iAnt, 2);

      for(unsigned iBand=1; iBand < 9; iBand++) {
	COUT("Configuring band " << iBand);
	spControl_->configureAstroBand(iBand, "LL", 1, carma::util::CORR_SPECTRAL);
      }

      for(unsigned iBand=9; iBand < 25; iBand++) {
	COUT("Configuring band " << iBand);
	spControl_->configureAstroBand(iBand, "LL", 2, carma::util::CORR_WIDEBAND);
      }
    }

    //-----------------------------------------------------------------------
    // Test adding/removing correlators
    //-----------------------------------------------------------------------

    if(Program::getBoolParameter("testaddcorr")) {
      if(Program::getBoolParameter("addcorr")) {
	spControl_->addCorrelator(corrNameToCorrType(Program::getParameter("corr")), Program::getIntParameter("sa"));
      } else {
	spControl_->removeCorrelator(corrNameToCorrType(Program::getParameter("corr")), Program::getIntParameter("sa"));
      }
    }

    //-----------------------------------------------------------------------
    // Test adding/removing antennas
    //-----------------------------------------------------------------------

    if(Program::getBoolParameter("testaddant")) {
      if(Program::getBoolParameter("addant")) {
	spControl_->addAntenna(Program::getIntParameter("ant"), Program::getIntParameter("sa"));
      } else {
	spControl_->removeAntenna(Program::getIntParameter("ant"), Program::getIntParameter("sa"));
      }
    }

    //-----------------------------------------------------------------------
    // Test configuration of an astro band
    //-----------------------------------------------------------------------

    if(Program::getBoolParameter("abtest")) {

      if(Program::getBoolParameter("clear")) {
	spControl_->clearAstroBandConfiguration((unsigned short)Program::getIntParameter("band"),
						Program::getIntParameter("sa"),
						corrNameToCorrType(Program::getParameter("corr")));
      } else {
	spControl_->configureAstroBand((unsigned short)Program::getIntParameter("band"), 
				       Program::getParameter("conf").c_str(),
				       Program::getIntParameter("sa"),
				       corrNameToCorrType(Program::getParameter("corr")));

      }
    }

    //------------------------------------------------------------
    // Assign a walsh column
    //------------------------------------------------------------

    if(Program::getBoolParameter("walsh")) {
      unsigned antNo = Program::getIntParameter("antno");
      unsigned walshColNo = Program::getIntParameter("walshcol");
      carma::signalpath::SignalPathMapperControl::WalshColumnAssignment wca;
      wca.antNo = antNo;
      wca.walshColNo = walshColNo;

      if(Program::getBoolParameter("clear")) {
	spControl_->clearWalshColumnAssignment(antNo);
      } else {
	spControl_->assignWalshColumn(wca);
      }

    }

    //------------------------------------------------------------
    // Query DC LO switch settings for the current configuration
    //------------------------------------------------------------

    if(Program::getBoolParameter("qdclosw")) {
      unsigned bandNo = Program::getBoolParameter("qallbands") ? 0 : Program::getIntParameter("band");
      COUT("About to call getDCLO");
      carma::switchyard::SwitchPositionSeq* swSet = spControl_->getDCLOSwitchSettings(bandNo);
      COUT("There are: " << (*swSet).length() << " DC LO switch settings");
      for(unsigned iSwitch=0; iSwitch < (*swSet).length(); iSwitch++) {
	carma::switchyard::SwitchPosition& swPos = (*swSet)[iSwitch];
	COUT("LO Switch: " << swPos.switchNo << " " << swPos.switchPos);
      }
    }

    //------------------------------------------------------------
    // Query switch settings for the current configuration
    //------------------------------------------------------------

    if(Program::getBoolParameter("qifsw")) {
      unsigned bandNo = Program::getBoolParameter("qallbands") ? 0 : Program::getIntParameter("band");
      carma::switchyard::SwitchPositionSeq* swSet = spControl_->getIFSwitchSettings(bandNo);
      COUT("There are: " << (*swSet).length() << " IF switch settings");
      for(unsigned iSwitch=0; iSwitch < (*swSet).length(); iSwitch++) {
	carma::switchyard::SwitchPosition& swPos = (*swSet)[iSwitch];
	COUT("IF Switch: " << swPos.switchNo << " " << swPos.switchPos);
      }
    }

    //------------------------------------------------------------
    // Query LO switch settings for the current configuration
    //------------------------------------------------------------

    if(Program::getBoolParameter("qlosw")) {
      unsigned bandNo = Program::getBoolParameter("qallbands") ? 0 : Program::getIntParameter("band");
      carma::switchyard::SwitchPositionSeq* swSet = spControl_->getLOSwitchSettings(bandNo);
      COUT("There are: " << (*swSet).length() << " LO switch settings");
      for(unsigned iSwitch=0; iSwitch < (*swSet).length(); iSwitch++) {
	carma::switchyard::SwitchPosition& swPos = (*swSet)[iSwitch];
	COUT("LO Switch: " << swPos.switchNo << " " << swPos.switchPos);
      }
    }

    //------------------------------------------------------------
    // Query LL switch settings for the current configuration
    //------------------------------------------------------------

    if(Program::getBoolParameter("qllsw")) {
      unsigned bandNo = Program::getBoolParameter("qallbands") ? 0 : Program::getIntParameter("band");
      carma::switchyard::SwitchPositionSeq* swSet = spControl_->getLLSwitchSettings(bandNo);
      COUT("There are: " << (*swSet).length() << " LL switch settings");
      for(unsigned iSwitch=0; iSwitch < (*swSet).length(); iSwitch++) {
	carma::switchyard::SwitchPosition& swPos = (*swSet)[iSwitch];
	COUT("LL Switch: " << swPos.switchNo << " " << swPos.switchPos);
      }
    }

    //------------------------------------------------------------
    // Query block downconverter settings for the current configuration
    //------------------------------------------------------------

    if(Program::getBoolParameter("qbdc")) {
      unsigned bandNo = Program::getBoolParameter("qallbands") ? 0 : Program::getIntParameter("band");
      carma::signalpath::SignalPathMapperControl::BlockDownconverterSettingSeq* bdcSeq = spControl_->getBdcSettings(bandNo);

      COUT("There are: " << (*bdcSeq).length() << " BDC settings");
      for(unsigned iBdc=0; iBdc < (*bdcSeq).length(); iBdc++) {
	carma::signalpath::SignalPathMapperControl::BlockDownconverterSetting& bdcSetting = (*bdcSeq)[iBdc];
	COUT("BDC: " << bdcSetting.bdcNo << " " << bdcSetting.bandNo << " " << bdcSetting.bdcInputType);
      }
    }

    //------------------------------------------------------------
    // Query walsh column assignments for the current configuration
    //------------------------------------------------------------

    if(Program::getBoolParameter("qwalsh")) {
      carma::signalpath::SignalPathMapperControl::WalshColumnAssignmentSeq* wSeq = spControl_->getWalshColumnAssignment(Program::getIntParameter("ant"));

      COUT("There are: " << (*wSeq).length() << " Walsh column assignments");
      for(unsigned iWalsh=0; iWalsh < (*wSeq).length(); iWalsh++) {
	carma::signalpath::SignalPathMapperControl::WalshColumnAssignment& wSetting = (*wSeq)[iWalsh];
	COUT("W: " << wSetting.antNo << " " << wSetting.walshColNo);
      }
    }

    //------------------------------------------------------------
    // Query astrobands specified by a named configuration
    //------------------------------------------------------------

    if(Program::getBoolParameter("qab")) {
      carma::signalpath::SignalPathMapperControl::AstroBandSeq* abSeq = 
	spControl_->getAstroBandsForConfiguration(Program::getParameter("conf").c_str(),
						  Program::getIntParameter("sa"),
						  corrNameToCorrType(Program::getParameter("corr")));

      COUT("There are: " << (*abSeq).length() << " astroBand nos");
      for(unsigned iAb=0; iAb < (*abSeq).length(); iAb++) {
	carma::signalpath::SignalPathMapperControl::AstroBand& ab = (*abSeq)[iAb];
	COUT("AB: " << ab.astroBandNo);
      }
    }

    //------------------------------------------------------------
    // Query antenna subarray assignments for the current configuration
    //------------------------------------------------------------

    if(Program::getBoolParameter("qant")) {
      carma::signalpath::SignalPathMapperControl::WalshColumnAssignmentSeq* wSeq = spControl_->getWalshColumnAssignment(0);

      COUT("There are: " << (*wSeq).length() << " Walsh column assignments");
      for(unsigned iWalsh=0; iWalsh < (*wSeq).length(); iWalsh++) {
	carma::signalpath::SignalPathMapperControl::WalshColumnAssignment& wSetting = (*wSeq)[iWalsh];
	COUT("W: " << wSetting.antNo << " " << wSetting.walshColNo);
      }
    }

    //------------------------------------------------------------
    // Query correlator bands for the current configuration
    //------------------------------------------------------------

    if(Program::getBoolParameter("qcorrbands")) {
      carma::signalpath::SignalPathMapperControl::CorrelatorBandSeq* bSeq = 
	spControl_->getActiveCorrelatorBands(corrNameToCorrType(Program::getParameter("corr")));

      COUT("There are: " << (*bSeq).length() << " Active correlator bands");
      for(unsigned iBand=0; iBand < (*bSeq).length(); iBand++) {
	carma::signalpath::SignalPathMapperControl::CorrelatorBand& band = (*bSeq)[iBand];
	COUT("CB: " << band.bandNo << " crate = " << band.crate.crateNo << " type = " << band.crate.type);
      }
    }

    //------------------------------------------------------------
    // Query correlator band input mapping for the current configuration
    //------------------------------------------------------------

    if(Program::getBoolParameter("qcorrbandmap")) {

      carma::signalpath::SignalPathMapperControl::CorrelatorBand band;
      band.bandNo     = Program::getIntParameter("band");
      band.crate.type = corrNameToCorrType(Program::getParameter("corr"));

      carma::signalpath::SignalPathMapperControl::CorrelatorBandInputSeq* bSeq = 
	spControl_->getCorrelatorBandInputMap(band);

      COUT("There are: " << (*bSeq).length() << " correlator band inputs for band: " << band.bandNo);
      for(unsigned iInp=0; iInp < (*bSeq).length(); iInp++) {
	carma::signalpath::SignalPathMapperControl::CorrelatorBandInput& input = (*bSeq)[iInp];
	COUT("INP: " << input.inputNo << " antenna = " << input.antIF.antNo << " pol = " << input.antIF.polType 
	     << " astrobandno    = " << input.aBandInput.aBand.astroBandNo 
	     << " astrobandinput = " << input.aBandInput.inputNo);
      }
    }

    //------------------------------------------------------------
    // Query antennas
    //------------------------------------------------------------

    if(Program::getBoolParameter("qant")) {

      unsigned astroBandNo = Program::getBoolParameter("qallbands") ? 0 : Program::getIntParameter("band");

      carma::signalpath::SignalPathMapperControl::AntennaSeq* aSeq = 
	spControl_->getAntennas(astroBandNo);

      COUT("There are: " << (*aSeq).length() << " antennas in astroband: " << astroBandNo);
      for(unsigned iInp=0; iInp < (*aSeq).length(); iInp++) {
	carma::signalpath::SignalPathMapperControl::Antenna& ant = (*aSeq)[iInp];
	COUT("ANT: " << ant.antNo << " type = " << ant.type << " walshColNo = " << ant.walshColNo << " subarrayNo = " << ant.subarrayNo);
      }
    }

    //------------------------------------------------------------
    // Query polarizations
    //------------------------------------------------------------

    if(Program::getBoolParameter("qpol")) {

      unsigned astroBandNo = Program::getBoolParameter("qallbands") ? 0 : Program::getIntParameter("band");

      carma::signalpath::SignalPathMapperControl::PolarizationSeq* pSeq = 
	spControl_->getPolarizations(astroBandNo);

      COUT("There are: " << (*pSeq).length() << " polarizations in astroband: " << astroBandNo);
      for(unsigned iInp=0; iInp < (*pSeq).length(); iInp++) {
	carma::signalpath::SignalPathMapperControl::PolarizationType pol = (*pSeq)[iInp];
	COUT("Pol: " << pol);
      }
    }

    //------------------------------------------------------------
    // Query correlator FPGA modes
    //------------------------------------------------------------

    if(Program::getBoolParameter("qfpga")) {
      unsigned short bandNo = Program::getIntParameter("band");
      carma::util::CorrelatorFpgaModeType fpgaMode = spControl_->getFpgaMode(bandNo);
      COUT("FPGA mode for band: " << bandNo << " is " << fpgaMode);
    }

    //------------------------------------------------------------
    // Check a configuration
    //------------------------------------------------------------

    if(Program::getBoolParameter("check")) {
      unsigned astroBandNo     = Program::getIntParameter("band");
      spControl_->checkConfigurationValidity(astroBandNo,
					     Program::getParameter("conf").c_str(),
					     Program::getIntParameter("sa"),
					     corrNameToCorrType(Program::getParameter("corr")));
    }

    //------------------------------------------------------------
    // Check success of a configuration
    //------------------------------------------------------------

    if(Program::getBoolParameter("qsuccess")) {
      unsigned astroBandNo     = Program::getIntParameter("band");
      spControl_->checkConfigurationSuccess(astroBandNo);
    }

    //------------------------------------------------------------
    // Load a cable map file if requested
    //------------------------------------------------------------

    if(Program::getBoolParameter("loadcablemap")) {
      spControl_->initializeCableMap(Program::getParameter("cablemap").c_str());
    }

    //------------------------------------------------------------
    // Load a configuration file if requested
    //------------------------------------------------------------

    if(Program::getBoolParameter("load")) {
      spControl_->loadConfiguration(Program::getParameter("conffile").c_str(), 
				    Program::getParameter("confname").c_str(),
				    Program::getParameter("abconfname").c_str());
    }

    //------------------------------------------------------------
    // Print the configuration on exit
    //------------------------------------------------------------

    if(Program::getBoolParameter("printconf")) {
      std::string confStr(spControl_->queryConfiguration());
      COUT(confStr);
    }

  } catch(carma::util::UserException& err) {
    COUT(err.errorMsg);
  } catch(...) {
    COUT("Caught an unknown error");
  }

  return 0;
}

/**.......................................................................
 * Convert from correlator name string to carma::control::CorrType
 */
carma::util::CorrelatorType corrNameToCorrType(std::string name)
{
  COUT("Inside corrNameToCorrType with name = " << name);

  String str(name);
  str = str.toUpper();

  COUT("Inside corrNameToCorrType with name = " << name << " str = " << str);

  if(str.str() == "SL") {
    return carma::util::CORR_SPECTRAL;
  }

  if(str.str() == "WB") {
    return carma::util::CORR_WIDEBAND;
  }

  if(str.str() == "ALL") {
    return carma::util::CORR_ALL;
  }

#ifdef EML_NEW_CORR
  if(str.str() == "C3GMAX8") {
    return carma::util::CORR_C3GMAX8;
  }

  if(str.str() == "C3GMAX23") {
    return carma::util::CORR_C3GMAX23;
  }
#endif

  return carma::util::CORR_NONE;
}
