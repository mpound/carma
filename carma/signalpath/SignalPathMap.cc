#include "carma/signalpath/SignalPathMap.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/String.h"
#include "carma/szautil/XtermManip.h"

#include "carma/util/CorrelatorSet.h"
#include "carma/util/programLogging.h"

#include <iomanip>
#include <fstream>
#include <sstream>
#include <string.h>

#define INTERIM_MAPPING

using namespace std;
using namespace sza::util;
using namespace carma::signalpath;
using namespace carma;
using namespace carma::util;

const unsigned SignalPathMap::nSza_         =  8;
const unsigned SignalPathMap::nBima_        =  9;
const unsigned SignalPathMap::nOvro_        =  6;
const unsigned SignalPathMap::nAnt_         =  SignalPathMap::nSza_ + SignalPathMap::nBima_ + SignalPathMap::nOvro_;

const unsigned SignalPathMap::nSubarray_    =  5;

const unsigned SignalPathMap::nSl_          = 15;
const unsigned SignalPathMap::nWb_          =  8;

const unsigned SignalPathMap::nSwitch_      = 2*SignalPathMap::nSl_ + SignalPathMap::nWb_;
const unsigned SignalPathMap::nBdc_         =   SignalPathMap::nSl_ + SignalPathMap::nWb_;

const unsigned SignalPathMap::nDigitizer_   = 46;

const unsigned WalshColumn::nWalshCol_      = 32;

const unsigned CorrelatorCrate::nInput_     = 16;

const unsigned BlockDownconverter::nBandSl_ =  8;
const unsigned BlockDownconverter::nBandWb_ = 16;
const unsigned BlockDownconverter::nBandC3g_=  8;

const unsigned AstroBand::nBandMax_         = 40;

const unsigned AstroBand::nInputMax_        = 2*CorrelatorCrate::nInput_;

const unsigned CorrelatorBand::nBandMax_    = BlockDownconverter::nBandSl_ + BlockDownconverter::nBandWb_;

const unsigned CorrelatorBand::nInputSl_    = SignalPathMap::nSl_;
const unsigned CorrelatorBand::nInputWb_    = SignalPathMap::nWb_;
const unsigned CorrelatorBand::nInputC3g_   = SignalPathMap::nDigitizer_;

/**.......................................................................
 * Constructor.
 */
SignalPathMap::SignalPathMap() 
{
  try {
    clearWalshColumnMask();
    
    // Initialize devices
    
    initializeAntennaMap();
    initializeSwitchMap();
    initializeCorrelatorMap();
    initializeCrateMap();
    initializeDigitizerMap();
    
    associateBdcBandsAndCrates();
    
    // Initialize device mapping
    
    initializeDefaultCableMap();
    
    // Initialize astro bands
    
    initializeAstroBands();
    
    // Initialize known fpga configurations
    
    initializeKnownAstroBandConfigurations();
    
    // Initialize default configurations
    
    initializeKnownConfigurations();

  } catch(Exception& err) {
    COUT(err.what());
    throw err;
  }
}

/**.......................................................................
 * Destructor.
 */
SignalPathMap::~SignalPathMap() 
{
  for(unsigned i=0; i < crates_.size(); i++) {
    delete crates_[i];
  }

  for(unsigned i=0; i < bdcs_.size(); i++) {
    delete bdcs_[i];
  }

  for(unsigned i=0; i < switches_.size(); i++) {
    delete switches_[i];
  }

  for(unsigned i=0; i < antennas_.size(); i++) {
    delete antennas_[i];
  }

  for(unsigned i=0; i < astroBands_.size(); i++) {
    delete astroBands_[i];
  }

  for(unsigned i=0; i < correlators_.size(); i++) {
    delete correlators_[i];
  }

  for(unsigned i=0; i < corrBands_.size(); i++) {
    delete corrBands_[i];
  }

  for(unsigned i=0; i < digitizers_.size(); i++) {
    delete digitizers_[i];
  }
}

/**.......................................................................
 * Initialize the array of known antennas
 */
void SignalPathMap::initializeAntennaMap()
{
  std::ostringstream os;

  for(unsigned iAnt=0; iAnt < nOvro_; iAnt++) {
    Antenna* ant = new Antenna(iAnt + 1, ANT_OVRO);
    antennas_.push_back(ant);

    os.str("");
    os << "C" << iAnt+1;
    antennaMap_[os.str()] = ant;

    os.str("");
    os << "OVRO" << iAnt;
    antennaMap_[os.str()] = ant;
  }

  for(unsigned iAnt=0; iAnt < nBima_; iAnt++) {
    Antenna* ant = new Antenna(iAnt + 1 + nOvro_, ANT_BIMA);
    antennas_.push_back(ant);

    os.str("");
    os << "C" << iAnt+1+nOvro_;
    antennaMap_[os.str()] = ant;

    os.str("");
    os << "BIMA" << iAnt;
    antennaMap_[os.str()] = ant;
  }

  for(unsigned iAnt=0; iAnt < nSza_; iAnt++) {
    Antenna* ant = new Antenna(iAnt + 1 + nOvro_ + nBima_, ANT_SZA);
    antennas_.push_back(ant);

    os.str("");
    os << "C" << iAnt+1+nOvro_+nBima_;
    antennaMap_[os.str()] = ant;

    os.str("");
    os << "SZA" << iAnt;
    antennaMap_[os.str()] = ant;
  }
}

/**.......................................................................
 * Initialize the array of known correlators
 */
void SignalPathMap::initializeCorrelatorMap()
{
  Correlator* corr = new Correlator(CORR_SL, SA_NONE);
  correlators_.push_back(corr);
  correlatorMap_[CORR_SL] = corr;

  corr = new Correlator(CORR_WB, SA_NONE);
  correlators_.push_back(corr);
  correlatorMap_[CORR_WB] = corr;

  Correlator* c3g8 = new Correlator(CORR_C3GMAX8, SA_NONE);
  correlators_.push_back(c3g8);
  correlatorMap_[CORR_C3GMAX8] = c3g8;

  Correlator* c3g23 = new Correlator(CORR_C3GMAX23, SA_NONE);
  correlators_.push_back(c3g23);
  correlatorMap_[CORR_C3GMAX23] = c3g23;
}

/**.......................................................................
 * Initialize the array of known correlator crates
 */
void SignalPathMap::initializeCrateMap()
{
  std::ostringstream os;

  //------------------------------------------------------------
  // The first 8 crates handle the 8 bands of the spectral-line
  // correlator
  //------------------------------------------------------------

  for(unsigned iCrate=1; iCrate <= CorrelatorCrate::nCrateSl_; iCrate++) {
    os.str("");
    os << "SLCOR" << iCrate;

    CorrelatorCrate* crate = new CorrelatorCrate(CORR_SL, iCrate);

    crates_.push_back(crate);
    crateMap_[os.str()] = crate;

    // Add the single correlator band that corresponds to this
    // spectral line crate

    CorrelatorBand* band   = new CorrelatorBand(crate, iCrate);
    corrBands_.push_back(band);
    corrBandMap_[os.str()] = band;
  }

  //------------------------------------------------------------
  // The next 8 crates handle the 16 bands of the wideband
  // correlator
  //------------------------------------------------------------

  for(unsigned iCrate=1; iCrate <= CorrelatorCrate::nCrateWb_; iCrate++) {
    os.str("");
    os << "WBCOR" << iCrate;

    CorrelatorCrate* crate = new CorrelatorCrate(CORR_WB, iCrate);

    crates_.push_back(crate);
    crateMap_[os.str()] = crate;

    // Add the two correlator bands that correspond to each wideband
    // crate

    CorrelatorBand* band1  = new CorrelatorBand(crate, 2*iCrate-1);
    CorrelatorBand* band2  = new CorrelatorBand(crate, 2*iCrate);
    corrBands_.push_back(band1);
    corrBands_.push_back(band2);

    os.str("");
    os << "WBCOR" << 2*iCrate-1;
    corrBandMap_[os.str()] = band1;

    os.str("");
    os << "WBCOR" << 2*iCrate;
    corrBandMap_[os.str()] = band2;
  }

  //------------------------------------------------------------
  // Lastly, initialize the correlator bands for the C3G correlator
  //------------------------------------------------------------

  for(unsigned iBand=1; iBand <= BlockDownconverter::nBandC3g_; iBand++) {
    CorrelatorBand* band   = new CorrelatorBand(CORR_C3G, iBand);
    corrBands_.push_back(band);
    corrBandMap_[band->name_] = band;
  }
}

/**.......................................................................
 * Initialize the array of known digitizers
 */
void SignalPathMap::initializeDigitizerMap()
{
  std::ostringstream os;

  for(unsigned iDig=1; iDig <= SignalPathMap::nDigitizer_; iDig++) {
    os.str("");
    os << "DIG" << iDig;

    Digitizer* dig = new Digitizer(this, CORR_C3G, iDig);
    dig->name_ = os.str();

    digitizers_.push_back(dig);
    digitizerMap_[os.str()] = dig;
  }
}

/**.......................................................................
 * Associate block downconverter bands with correlator crates
 */
void SignalPathMap::associateBdcBandsAndCrates()
{
  for(unsigned iBdc=0; iBdc < bdcs_.size(); iBdc++) {
    BlockDownconverter* bdc = bdcs_[iBdc];

    for(unsigned iBand=0; iBand < bdc->bands_.size(); iBand++) {
      Band* band = bdc->bands_[iBand];
      mapBdcBandToCrate(bdc, band);
    }
  }
}

/**.......................................................................
 * Map a block downconverter band to a correlator crate
 */
void SignalPathMap::mapBdcBandToCrate(BlockDownconverter* bdc, Band* band)
{
  ostringstream os;
  os.str("");

  // Bands 1-8 map to crates 1-8 for the spectral-line correlator (SLCOR1-8), and
  // to crates 9-16 (WBCOR1-8) for the wideband correlator

  CorrelatorCrate*      crate = 0;
  CorrelatorCrateInput* input = 0;

  unsigned iCrateInput;
  if(bdc->type_ == CORR_SL) {
    os << "SLCOR" << band->bandNo_;

    crate = crateMap_[os.str()];
    iCrateInput = bdc->bdcNo_ - 1;
    input = crate->inputs_[iCrateInput];

  } else {
    bool firstHalf      = (band->bandNo_%2 > 0);
    unsigned crateIndex = (band->bandNo_/2 + band->bandNo_%2);
    os << "WBCOR" << crateIndex;

    crate = crateMap_[os.str()];
    iCrateInput = (firstHalf ? bdc->bdcNo_ - nSl_ : bdc->bdcNo_ - nSl_ + nWb_) - 1;
    input = crate->inputs_[iCrateInput];
  }

  input->bdcBand_  = band;
  band->corrInput_ = input;
}

/**.......................................................................
 * Initialize the array of known switches
 */
void SignalPathMap::initializeSwitchMap()
{
  std::ostringstream os;

  //------------------------------------------------------------
  // According to Mark Hodges' email of 6 Dec 2010, the first 30
  // switches alternate between P2 ('R') and P1 ('L') inputs for
  // inputs 1-15 of the spectral line correlator.  
  //------------------------------------------------------------

  for(unsigned iSwitch=0; iSwitch < nSl_; iSwitch++) {

    // Switches are always associated with a corresponding block
    // downconverter, which I am numbering consecutively from 1-23
    // across correlators

    os.str("");
    os << "BD_SL" << iSwitch+1;

    BlockDownconverter* bdc = new BlockDownconverter(iSwitch+1, CORR_SL);
    bdcs_.push_back(bdc);
    bdcMap_[os.str()] = bdc;

    // Now create the 'R' switch object

    os.str("");
    os << "SL" << (iSwitch+1) << "R";

    Switch* rsw = new Switch(2*iSwitch+1, os.str());

    switches_.push_back(rsw);
    switchMap_[os.str()] = rsw;

    // Also add a consecutive label to the switch map

    os.str("");
    os << "SW" << 2*iSwitch+1;
    switchMap_[os.str()] = rsw;

    // And create the 'L' switch object

    os.str("");
    os << "SL" << (iSwitch+1) << "L";

    Switch* lsw = new Switch(2*iSwitch+2, os.str());
    switches_.push_back(lsw);
    switchMap_[os.str()] = lsw;

    // Also add a consecutive label to the switch map

    os.str("");
    os << "SW" << 2*iSwitch+2;
    switchMap_[os.str()] = lsw;

    // Set the P1 and P2 inputs for this block downconverter pointing to the
    // correct switches

    bdc->inputMap_[BD_INP_P2]->switch_ = rsw;
    bdc->inputMap_[BD_INP_P1]->switch_ = lsw;

    // And set the input for these switches pointing to the correct
    // block cownconverter inputs

    rsw->bdcInput_ = bdc->inputMap_[BD_INP_P2];
    lsw->bdcInput_ = bdc->inputMap_[BD_INP_P1];
  }

  //------------------------------------------------------------
  // The next 8 switches are for wideband inputs (L polarization)
  //------------------------------------------------------------

  for(unsigned iSwitch=1; iSwitch <= nWb_; iSwitch++) {

    // Switches are always associated with a corresponding block downconverter

    os.str("");
    os << "BD_WB" << iSwitch;

    BlockDownconverter* bdc = new BlockDownconverter(iSwitch + nSl_, CORR_WB);
    bdcs_.push_back(bdc);
    bdcMap_[os.str()] = bdc;

    // Now create the switch object

    os.str("");
    os << "WB" << iSwitch;

    Switch* sw = new Switch(iSwitch + 2*nSl_, os.str());
    switches_.push_back(sw);
    switchMap_[os.str()] = sw;

    os.str("");
    os << "SW" << iSwitch+2*nSl_;
    switchMap_[os.str()] = sw;

    // Set the P1 input for this block downconverter pointing to the
    // correct switch

    bdc->inputMap_[BD_INP_P1]->switch_ = sw;

    // And set the input for this switch pointing to the correct input

    sw->bdcInput_ = bdc->inputMap_[BD_INP_P1];
  }
}

void SignalPathMap::initializeAstroBands()
{
  for(unsigned i=0; i < AstroBand::nBandMax_; i++) {
    AstroBand* band = new AstroBand(i+1);
    astroBands_.push_back(band);
    astroBandMap_[i+1] = band;
  }
}

/**.......................................................................
 * Map a set of antenna IFs to a set of switches
 */
void SignalPathMap::hardwareMapAntennaIFToSwitch(std::string antennaIFSpec, std::string switchSpec)
{
  // Parse the antenna IF specification

  std::vector<unsigned> carmaIndices;
  std::vector<SplitterChannelId> splitterChannels;

  PolarizationType polType;
  parseIfSpecification(antennaIFSpec, polType, carmaIndices, splitterChannels);

  // If no splitter channels were specified, default for the specified
  // polarization type

  if(splitterChannels.size() == 0) {
    splitterChannels = getDefaultSplitterChannelsForPolarization(polType);
  }

  // Parse the switch channel specification

  std::vector<unsigned> switchIds;
  std::vector<SwitchChannelId> switchChannels;

  parseSwitchSpecification(switchSpec, switchIds, switchChannels);

  // There must be a 1-1 mapping of antennas to switch numbers

  if(carmaIndices.size() != switchIds.size()) {
    ThrowColorError("You cannot map " << carmaIndices.size() << " antenna IFs (" << antennaIFSpec << ") to " 
		    << switchIds.size() << " switches (" << switchSpec << ")", "red");
  }

  // There must also be a 1-1 mapping of splitter channels to switch channels

  if(splitterChannels.size() != switchChannels.size()) {
    ThrowColorError("You cannot map " << splitterChannels.size() << " splitter channels to " 
		    << switchChannels.size() << " switch channels", "red");
  }

  // Now perform the individual mappings corresponding to the
  // specification that was passed

  for(unsigned iIf=0; iIf < carmaIndices.size(); iIf++) {
    for(unsigned iChan=0; iChan < splitterChannels.size(); iChan++) {
      hardwareMapAntennaIFToSwitch(carmaIndices[iIf], polType, splitterChannels[iChan], 
				   switchIds[iIf], switchChannels[iChan]);
    }
  }
}

/**.......................................................................
 * Map a single antenna IF to a switch channel
 */
void SignalPathMap::hardwareMapAntennaIFToSwitch(unsigned carmaAntIndex,  
						 PolarizationType type, 
						 SplitterChannelId splitterChannelId,
						 unsigned carmaSwitchIndex, 
						 SwitchChannelId switchChannelId)
{
  checkArguments(carmaAntIndex, type, splitterChannelId, carmaSwitchIndex, switchChannelId);

  unsigned iAnt    =    carmaAntIndex - 1;
  unsigned iSwitch = carmaSwitchIndex - 1;

  // Get the requested antenna and IF

  Antenna* ant = antennas_[iAnt];

  if(ant->ifMap_.find(splitterChannelId) == ant->ifMap_.end()) {
    ThrowSimpleError("Splitter channel: " << splitterChannelId << " not found for C" << ant->antNo_);
  }

  AntennaIF* antIf = ant->ifMap_[splitterChannelId];

  // Get the requested switch and switch channel

  Switch* sw               =              switches_[iSwitch];
  SwitchChannel* swChannel = sw->channelMap_[switchChannelId];

  // Now map the switch channel and antenna IF

  antIf->mapTo((ConnectableNode*)swChannel);
  swChannel->if_ = antIf;
}

/**.......................................................................
 * Check map arguments for validity
 */
void SignalPathMap::checkArguments(unsigned carmaId,  PolarizationType type, SplitterChannelId splitterChannel,
				   unsigned switchId, SwitchChannelId switchChannel)
{
  if(carmaId < 1 || carmaId > nAnt_+1) {
    ThrowSimpleError("Invalid CARMA antenna id: " << carmaId << ".  Should be (1-" << nAnt_+1 << ")");
  }

  if(type == POL_NONE) {
    ThrowSimpleError("You must specify a valid polarization type");
  }

  if(type == POL_RIGHT && splitterChannel != SP_CHAN_NONE) {
    ThrowSimpleError("Invalid splitter channel: " << splitterChannel << " (Polarization RIGHT is not split)");
  }

  if(type == POL_LEFT && splitterChannel == SP_CHAN_NONE) {
    ThrowSimpleError("You must specify a splitter channel for polarization LEFT");
  }

  if(switchId < 1 || switchId > nSwitch_+1) {
    ThrowSimpleError("Invalid switch id: " << switchId << ".  Should be (1-" << nSwitch_+1 << ")");
  }

  if(switchChannel == SW_CHAN_NONE) {
    ThrowSimpleError("You must specify a valid switch channel");
  }
}

/**.......................................................................
 * Check map arguments for validity
 */
void SignalPathMap::checkArguments(unsigned carmaId,  PolarizationType type, SplitterChannelId splitterChannel,
				   std::string digitizerId)
{
  if(carmaId < 1 || carmaId > nAnt_+1) {
    ThrowSimpleError("Invalid CARMA antenna id: " << carmaId << ".  Should be (1-" << nAnt_+1 << ")");
  }

  if(type == POL_NONE) {
    ThrowSimpleError("You must specify a valid polarization type");
  }

  if(type == POL_RIGHT && splitterChannel != SP_CHAN_NONE) {
    ThrowSimpleError("Invalid splitter channel: " << splitterChannel << " (Polarization RIGHT is not split)");
  }

  if(type == POL_LEFT && splitterChannel == SP_CHAN_NONE) {
    ThrowSimpleError("You must specify a splitter channel for polarization LEFT");
  }

  try {
    validateDigitizer(digitizerId);
  } catch(...) {
    ThrowSimpleError("Invalid digitizer id: " << digitizerId);
  }

}

/**.......................................................................
 * Map a set of antenna IFs to a set of digitizers
 */
void SignalPathMap::mapAntennaIFToDigitizer(std::string antennaIFSpec, std::string digSpec,
					    bool doMapping, unsigned baseIndex, CorrelatorType type, AstroBandInfo* info)

{
  // Parse the antenna IF specification

  std::vector<unsigned> carmaIndices;
  std::vector<SplitterChannelId> splitterChannels;

  PolarizationType polType;
  parseIfSpecification(antennaIFSpec, polType, carmaIndices, splitterChannels);

  // If no splitter channels were specified, default for the specified
  // polarization type

  if(splitterChannels.size() == 0) {
    splitterChannels = getDefaultSplitterChannelsForPolarization(polType);
  }

  // Parse the digitizer specification

  std::vector<std::string> digIds;
  parseDigitizerSpecification(digSpec, digIds, baseIndex, doMapping);

  // There must be a 1-1 mapping of antennas x splitter channels to digitizers

  if(carmaIndices.size()*splitterChannels.size() != digIds.size()) {
    ThrowColorError("You cannot map " << carmaIndices.size()*splitterChannels.size() << " antenna IFs (" << antennaIFSpec << ") to " 
		    << digIds.size() << " digitizers (" << digSpec << ")", "red");
  }

  // Now perform the individual mappings corresponding to the
  // specification that was passed

  unsigned iDig=0;
  for(unsigned iIf=0; iIf < carmaIndices.size(); iIf++) {
    for(unsigned iChan=0; iChan < splitterChannels.size(); iChan++, iDig++) {
      mapAntennaIFToDigitizer(carmaIndices[iIf], polType, splitterChannels[iChan], digIds[iDig],
			      doMapping, type, info);
    }
  }
}

/**.......................................................................
 * Map a set of antenna IFs to a set of digitizers
 */
void SignalPathMap::hardwareMapAntennaIFToDigitizer(std::string antennaIFSpec, std::string digSpec)
{
  // Parse the antenna IF specification

  std::vector<unsigned> carmaIndices;
  std::vector<SplitterChannelId> splitterChannels;

  PolarizationType polType;
  parseIfSpecification(antennaIFSpec, polType, carmaIndices, splitterChannels);

  // If no splitter channels were specified, default for the specified
  // polarization type

  if(splitterChannels.size() == 0) {
    splitterChannels = getDefaultSplitterChannelsForPolarization(polType);
  }

  // Parse the digitizer specification

  std::vector<std::string> digIds;
  parseDigitizerSpecification(digSpec, digIds);

  // There must be a 1-1 mapping of antennas x splitter channels to digitizers

  if(carmaIndices.size()*splitterChannels.size() != digIds.size()) {
    ThrowColorError("You cannot map " << carmaIndices.size()*splitterChannels.size() << " antenna IFs (" << antennaIFSpec << ") to " 
		    << digIds.size() << " digitizers (" << digSpec << ")", "red");
  }

  // Now perform the individual mappings corresponding to the
  // specification that was passed

  unsigned iDig=0;
  for(unsigned iIf=0; iIf < carmaIndices.size(); iIf++) {
    for(unsigned iChan=0; iChan < splitterChannels.size(); iChan++, iDig++) {
      hardwareMapAntennaIFToDigitizer(carmaIndices[iIf], polType, splitterChannels[iChan], 
				      digIds[iDig]);
    }
  }
}

/**.......................................................................
 * Map a single antenna IF to a digitizer
 */
void SignalPathMap::hardwareMapAntennaIFToDigitizer(unsigned carmaAntIndex,  PolarizationType type, 
						    SplitterChannelId splitterChannelId, std::string digId)
{
  checkArguments(carmaAntIndex, type, splitterChannelId, digId);

  unsigned iAnt =       carmaAntIndex - 1;

  // Get the requested antenna and IF

  Antenna* ant = antennas_[iAnt];

  if(ant->ifMap_.find(splitterChannelId) == ant->ifMap_.end()) {
    ThrowSimpleError("Splitter channel: " << splitterChannelId << " not found for C" << ant->antNo_);
  }

  AntennaIF* antIf = ant->ifMap_[splitterChannelId];

  // Get the requested digitizer

  Digitizer* dig = getDigitizer(digId);

  // Now map the digitizer and antenna IF
	
  antIf->mapTo((ConnectableNode*)dig);
  dig->mapFrom((ConnectableNode*)antIf);
}

/**.......................................................................
 * Map a set of antenna IFs to a set of block downconverter bands
 */
void SignalPathMap::mapAntennaIFToBdc(std::string antennaIFSpec, std::string bdcSpec, 
				      bool doMapping, unsigned baseIndex, CorrelatorType type, AstroBandInfo* info)
{
  // Parse the antenna IF specification

  std::vector<unsigned> antNos;
  std::vector<SplitterChannelId> splitterChannels;

  PolarizationType polType;
  parseIfSpecification(antennaIFSpec, polType, antNos, splitterChannels);

  // If a splitter channel was specified, make sure there is only one

  if(splitterChannels.size() > 1) {
    ThrowColorError("You cannot map more than one splitter channel to a block downconverter input: " 
		    << antennaIFSpec, "red");
  }

  // Parse the bdc specification

  std::vector<unsigned> bdcIndices;
  std::vector<unsigned> bandIndices;

  parseBdcSpecification(bdcSpec, bdcIndices, bandIndices, baseIndex, doMapping);

  // There must be a 1-1 mapping of antennas to bdc indices

  if(antNos.size() != bdcIndices.size()) {
    ThrowColorError("You cannot map " << antNos.size() << " antenna IFs (" << antennaIFSpec << ") to " 
		    << bdcIndices.size() << " block downconverters (" << bdcSpec << ")", "red");
  }

  // Now perform the individual mappings corresponding to the
  // specification that was passed

  for(unsigned iIf=0; iIf < antNos.size(); iIf++) {
    SplitterChannelId scId = (splitterChannels.size() == 0) ? SP_CHAN_NONE : splitterChannels[0];
    mapAntennaIFToBdc(antNos[iIf], polType, scId,
		      bdcIndices[iIf], bandIndices, doMapping, type, info);
  }
}

/**.......................................................................
 * Map a single antenna IF to a set of block downconverter bands
 */
void SignalPathMap::mapAntennaIFToBdc(unsigned antNo,  PolarizationType polType, SplitterChannelId scId,
				      unsigned bdcNo, std::vector<unsigned>& bandNos,
				      bool doMapping, CorrelatorType type, AstroBandInfo* info)
{
  // Get the requested block downconverter

  BlockDownconverter* bdc = getBdc(bdcNo);

  // If no band numbers were passed, default to all bands of this
  // block downconverter type

  std::vector<unsigned> bands = bandNos;

  if(bands.size() == 0) {
    bands.resize(bdc->nBand());

    for(unsigned iBand=0; iBand < bands.size(); iBand++) {
      bands[iBand] = iBand+1;
    }
  }

  for(unsigned iBand=0; iBand < bands.size(); iBand++) {
    mapAntennaIFToBdc(antNo, polType, scId,
		      bdcNo, bands[iBand], doMapping, type, info);
  }
}

/**.......................................................................
 * Map a single antenna IF to a single block downconverter band
 */
void SignalPathMap::mapAntennaIFToBdc(unsigned antNo, PolarizationType polType, SplitterChannelId scId,
				      unsigned bdcNo, unsigned bandNo,
				      bool doMapping, CorrelatorType type, AstroBandInfo* info)
{
  // Get the requested antenna and block downconverter

  Antenna* ant            = getAntenna(antNo);
  BlockDownconverter* bdc = getBdc(bdcNo);
  Band* band              = bdc->getBand(bandNo);

  // If this mapping is for a different astro band than the requested
  // astro band, just return quietly.  (We only want to execute the
  // parts of this mapping that relate to the requested astro band,
  // and don't care if the mapping specifies other bands too)
  //
  // We no longer skip this check if doMapping==false, since the
  // astroband configuration _has_ to be asserted even if
  // doMapping==false, or else the subsequent check for antenna
  // ownership will fail

  if(!band->belongsTo(info))
    return;

  // Check if the passed correlator type matches the type that we are
  // trying to route to

  if(doMapping) {
    if(!(type & bdc->type_)) {
      ThrowColorError(std::endl << "You are attempting to modify the input to a block downconverter "
		      << "(BD" << bdc->bdcNo_ << ") that you are not allowed to modify (controlling subarray owns " << type << ")" << std::endl, "red");
    }
  }

  // See if any of the requested antennaIFs can be routed to the
  // requested BDC input
  
  for(std::map<SplitterChannelId, AntennaIF*>::iterator iter=ant->ifMap_.begin(); 
      iter != ant->ifMap_.end(); iter++) {

    AntennaIF* antIf      = iter->second;
    ConnectableNode* node = (ConnectableNode*) antIf;

    // See if this IF is connected to a switch channel.  If it is not,
    // then there is no routing to a BDC

    SwitchChannel* swChan = dynamic_cast<SwitchChannel*>(node->toNode_);

    if(swChan == 0)
      continue;

    // If it is, get the switch it's connected to

    Switch* sw            = swChan->switch_;

    // If this IF matches the requested polarization type, and is
    // connected to the requested BDC, set the corresponding
    // switch to select it.

    if(antIf->polType_     == polType && 
       sw->bdcInput_->bdc_ == bdc && 
       (scId == SP_CHAN_NONE || antIf->splitterChannel_ == scId)) {

      // Increment the number of devices that would be
      // configured for this band by this configuration

      AstroBandInfo::incrementConfiguredDevices(info);
	
      // We no longer claim the antenna for this subarray on
      // configuration of the astroband.  This is done separately by
      // calls to addAntenna()/removeAntenna().  
      //
      // During configuration, however we still set up switches for
      // antennas that are part of this configuration, regardless of
      // whether or not they are in the subarray.  This is so that
      // adding an antenna back into a subarray with configured
      // astrobands will not require any changes to the IF switchyard
      // if that antenna is already part of the configuration.

      if(!AstroBandInfo::isConflicted(info)) {
	
	// Only set the switch if this configuration is currently
	// allowed
	
	if(sw->canBeConfiguredBy(info, swChan)) {
	  
	  // If we are actually doing the mapping, select the switch now
	  
	  if(doMapping) {

	    sw->selectChannel(swChan->channelId_);
	    sw->setOwnershipTo(info);
	    
	    band->mapBdcInput(sw->bdcInput_);
	    band->setOwnershipTo(info);
	    
	    // And register the astroband input corresponding to this bdc
	    // band as required by the current configuration
	    
	    if(band->corrInput_) {
	      if(band->corrInput_->isConnectedToAstroBand()) {
		band->corrInput_->getAstroBandInput()->isConfigured_ = true;
	      }
	    }

	  }
	  
	} else {
	  AstroBandInfo::registerConflict(info, sw);
	}
      }

      // Having found a match. return

      return;
    }
  }

  // Getting here means that no matching antenna IF could be connected to the requested BDC input
  
  if(scId == SP_CHAN_NONE) {
    ThrowColorError("C" << antNo << polType 
		    << " cannot be routed to BD" << bdcNo << ":" << bandNo, "red");
  } else {
    ThrowColorError("C" << antNo << polType 
		    << ": " << scId << " cannot be routed to BD" << bdcNo << ":" << bandNo, "red");
  }
}

/**.......................................................................
 * Map a set of antenna IFs to a set of correlator inputs
 */
void SignalPathMap::mapAntennaIFToCorr(std::string antennaIFSpec, std::string corrSpec, 
				       bool doMapping, unsigned baseIndex, CorrelatorType type, AstroBandInfo* info)
{
  // Parse the antenna IF specification

  std::vector<unsigned> antNos;
  std::vector<SplitterChannelId> splitterChannels;

  PolarizationType polarType;
  parseIfSpecification(antennaIFSpec, polarType, antNos, splitterChannels);

  // If a splitter channel was specified, make sure there is only one

  if(splitterChannels.size() > 1) {
    ThrowColorError("You cannot map more than one splitter channel to a block downconverter input: " << antennaIFSpec, "red");
  }

  // Parse the corr specification

  std::vector<std::string> crateNames;
  std::vector<unsigned> inputIndices;

  parseCorrSpecification(corrSpec, crateNames, inputIndices, baseIndex, doMapping);

  // There must be a 1-1 mapping of antennas to correlator input numbers

  if(antNos.size() != inputIndices.size()) {
    ThrowColorError("You cannot map " << antNos.size() << " antenna IFs (" << antennaIFSpec << ") to " 
		    << inputIndices.size() << " correlator inputs (" << corrSpec << ")", "red");
  }

  // Now perform the individual mappings corresponding to the
  // specification that was passed

  for(unsigned iIf=0; iIf < antNos.size(); iIf++) {
    for(unsigned iCrate=0; iCrate < crateNames.size(); iCrate++) {

      SplitterChannelId scId = (splitterChannels.size() == 0) ? SP_CHAN_NONE : splitterChannels[0];

      mapAntennaIFToCorr(antNos[iIf], polarType, scId,
			 crateNames[iCrate], inputIndices[iIf], doMapping, type, info);
    }
  }
}

/**.......................................................................
 * Map a single antenna IF to a set of correlator inputs
 */
void SignalPathMap::mapAntennaIFToCorr(unsigned antNo,  PolarizationType polType, SplitterChannelId scId,
				       std::string crateName, unsigned inputNo, 
				       bool doMapping, CorrelatorType type, AstroBandInfo* info)
{
  // Get the requested crate and input

  CorrelatorCrate* crate      = getCrate(crateName);
  CorrelatorCrateInput* input = crate->getInput(inputNo);

  // If this mapping is for a different astro band than the requested
  // astro band, just return quietly.  (We only want to execute the
  // parts of this mapping that relate to the requested astro band,
  // and don't care if the mapping specifies other bands too)
  //
  // We no longer skip this check if doMapping==false, since the
  // astroband configuration _has_ to be asserted even if
  // doMapping==false, or else the subsequent check for antenna
  // ownership will fail

  if(!input->belongsTo(info))
    return;

  // Check if the passed correlator type matches the type that we are
  // trying to route to

  if(doMapping) {
    if(!(type & crate->type_)) {
      CARMALOGINFO("EML(1): type = " << type << " crate->type = " << crate->type_);
      ThrowColorError(std::endl << "You are attempting to modify the input to a crate "
		      << "(" << crate->name_ << ") that you are not allowed to modify (controlling subarray owns " << type << ")" << std::endl, "red");
    }
  }

  // See if the requested antenna IF can be routed to this correlator
  // input

  Band* band = input->getBdcBand();
  BlockDownconverter* bdc = band->bdc_;

  for(std::map<BlockDownconverterInputType, BlockDownconverterInput*>::iterator bdcIter = bdc->inputMap_.begin();
      bdcIter != bdc->inputMap_.end(); bdcIter++) {

    Switch* sw = bdcIter->second->switch_;

    for(std::map<SwitchChannelId, SwitchChannel*>::iterator swIter=sw->channelMap_.begin();
	swIter != sw->channelMap_.end(); swIter++) {

      SwitchChannel* channel = swIter->second;

      if(channel->if_) {
	AntennaIF* antIf = channel->if_;
	Antenna*   ant   = antIf->antenna_;

	if((ant->antNo_             == antNo) && 
	   (antIf->polType_         == polType) &&
	   (antIf->splitterChannel_ == scId || scId == SP_CHAN_NONE)) {
	  
	  // Increment the number of devices that would be
	  // configured for this band by this configuration
	  
	  AstroBandInfo::incrementConfiguredDevices(info);

	  // We no longer claim the antenna for this subarray on
	  // configuration of the astroband.  This is done separately by
	  // calls to addAntenna()/removeAntenna().  
	  //
	  // During configuration, however, we still set up switches
	  // for antennas that are part of this configuration,
	  // regardless of whether or not they are in the subarray.
	  //
	  // This is so that adding an antenna back into a subarray
	  // with configured astrobands will not require any changes
	  // to the IF switchyard if that antenna is already part of
	  // the configuration.

	  // Only set the switch if this configuration is currently
	  // allowed
	  
	  if(!AstroBandInfo::isConflicted(info)) {
	    
	    if(sw->canBeConfiguredBy(info, channel)) {
	    
	      // Only set the switch if we are actually doing the
	      // mapping, and not just checking it

	      if(doMapping) {
		sw->selectChannel(channel->channelId_);
		sw->setOwnershipTo(info);
		
		band->mapBdcInput(sw->bdcInput_);
		band->setOwnershipTo(info);
		
		// And register the astroband input coresponding to this
		// correlator input as required by the current
		// configuration
		
		if(input->isConnectedToAstroBand()) {
		  input->getAstroBandInput()->isConfigured_ = true;
		}
	      }
	      
	    } else {
	      AstroBandInfo::registerConflict(info, sw);
	    }
	  }

	  // Having found a match, return, regardless of whether we
	  // handled it or not

	  return;
	}
      }
    }
  }
  
  // Getting here means that no matching antenna IF could be connected to the requested CORR input
  
  if(scId == SP_CHAN_NONE) {
    ThrowColorError("C" << antNo << polType << " cannot be routed to " 
		    << crateName << ":" << inputNo, "red");
  } else {
    ThrowColorError("C" << antNo << polType << ": " << scId << " cannot be routed to " 
		    << crateName << ":" << inputNo, "red");
  }
}

/**.......................................................................
 * Map a single antenna IF to a set of correlator inputs
 */
void SignalPathMap::mapAntennaIFToDigitizer(unsigned antNo,  PolarizationType polType, SplitterChannelId scId,
					    std::string digName,
					    bool doMapping, CorrelatorType type, AstroBandInfo* info)
{
  // Get the requested digitizer

  Digitizer* dig = getDigitizer(digName);

  // For the new correlator (the only correlator with
  // directly-connected digitizers), every digitizer is inherently
  // part of every potential astroband, so we don't do any checks on
  // astroband ownership here
  //
  // We no longer skip this check if doMapping==false, since the
  // astroband configuration _has_ to be asserted even if
  // doMapping==false, or else the subsequent check for antenna
  // ownership will fail

  if(info) {
    COUT("Checking if " << info->astroBandNo_ << " belongs to " << *dig);
  } else {
    COUT("Checking if " << "(null)" << " belongs to " << *dig);
  }

  if(!(type & dig->type_) || !dig->belongsTo(info)) {
    return;
  }

  if(info) {
    COUT("Checking if " << info->astroBandNo_ << " belongs to " << *dig << " ... it does");
  } else {
    COUT("Checking if " << "(null)" << " belongs to " << *dig << " ... it does");
  }

  if(doMapping) {
    if(!(type & dig->type_)) {
      CARMALOGINFO("EML(1): type = " << type << " dig->type = " << dig->type_);
      ThrowColorError(std::endl << "You are attempting to assert a mapping to a digitizer "
		      << "(" << dig->name_ << ") that you are not allowed to modify (controlling subarray owns " << type << ")" << std::endl, "red");
    }
  }

  // See if the requested antenna IF can be routed to this digitizer

  AntennaIF* antIf = dynamic_cast<AntennaIF*>(dig->fromNode_);

  if(antIf) {
    Antenna*   ant   = antIf->antenna_;

    if((ant->antNo_             == antNo) && 
       (antIf->polType_         == polType) &&
       (antIf->splitterChannel_ == scId || scId == SP_CHAN_NONE)) {
      
      // Increment the number of devices that would be
      // configured for this band by this configuration
	  
      AstroBandInfo::incrementConfiguredDevices(info);

      // We no longer claim the antenna for this subarray on
      // configuration of the astroband.  This is done separately by
      // calls to addAntenna()/removeAntenna().  
      //
      // During configuration, however, we still set up switches
      // for antennas that are part of this configuration,
      // regardless of whether or not they are in the subarray.
      //
      // This is so that adding an antenna back into a subarray
      // with configured astrobands will not require any changes
      // to the IF switchyard if that antenna is already part of
      // the configuration.
      
      // Only set the switch if this configuration is currently
      // allowed
      
      if(!AstroBandInfo::isConflicted(info)) {
	
	if(dig->canBeConfiguredBy(info)) {
	  
	  // Only set up the digitizer if we are actually doing the
	  // mapping, and not just checking it
	  
	  if(doMapping) {

	    dig->setOwnershipTo(info);
	    
	    // Now iterate through any astroband inputs corresponding
	    // to this astroband, marking them as required by the
	    // current configuration
	    
	    for(unsigned iBf=0; iBf < dig->bandFormers_.size(); iBf++) {
	      FpgaBoard* brd = dig->bandFormers_[iBf];

	      for(unsigned iInput=0; iInput < brd->corrBandInputs_.size(); iInput++) {
		CorrelatorBandInput* input = brd->corrBandInputs_[iInput];

		if(input->isConnectedToAstroBand()) {

		  AstroBandInput* abInput = input->getAstroBandInput();
		  
		  if(abInput->band_->bandNo_ == info->astroBandNo_) {
		    abInput->isConfigured_ = true;
		  }
		}
	      }
	    }
	  }
	  
	} else {
	  AstroBandInfo::registerConflict(info, dig);
	}
      }
      
      // Having found a match, return, regardless of whether we
      // handled it or not
      
      return;
    }
  }
  
  // Getting here means that no matching antenna IF could be connected to the requested Digitizer
  
  if(scId == SP_CHAN_NONE) {
    ThrowColorError("C" << antNo << polType << " cannot be routed to " 
		    << digName, "red");
  } else {
    ThrowColorError("C" << antNo << polType << ": " << scId << " cannot be routed to " 
		    << digName, "red");
  }
}

/**.......................................................................
 * Clear a hardwired walsh column assignment for an antenna
 */
void SignalPathMap::clearWalshColumn(std::string antName)
{
  Antenna* ant = getAntenna(antName);
  ant->walshCol_.setHardwired(false);
}

/**.......................................................................
 * Assert a fixed walsh column for an antenna, by name
 */
void SignalPathMap::setWalshColumn(std::string antName, unsigned walshColNo)
{
  Antenna* ant = getAntenna(antName);
  setWalshColumn(ant, walshColNo);
}

/**.......................................................................
 * Assert a fixed walsh column for an antenna.
 *
 * This sets the walsh column assignment, then checks it against the
 * current configuration for conflicts.
 */
void SignalPathMap::setWalshColumn(Antenna* ant, unsigned walshColNo)
{
  // Store the old walsh column in case configuration fails

  WalshColumn wcOld = ant->walshCol_;

  ant->walshCol_.setWalshColNo(walshColNo, true);

  try {
    configureWalshColumns();
  } catch(Exception& err) {
    ant->walshCol_ = wcOld;
    throw err;
  }
}

/**.......................................................................
 * Iterate over astrobands, returning a vector of currently configured
 * bands corresponding to the requested correlator
 */
std::vector<unsigned> SignalPathMap::getActiveAstroBandNos(CorrelatorType type)
{
  std::vector<unsigned> nos;

  for(unsigned iAb=0; iAb < astroBands_.size(); iAb++) {

    AstroBand* ab = astroBands_[iAb];

    // If this astro band is configured, iterate through its inputs

    if(ab->isConfigured_) {
      
      try {
        CorrelatorCrate* crate = ab->getInput(1)->getCrateInput()->crate_;
        if(type & crate->type_) {
          nos.push_back(iAb+1);
        }
      } catch(...) {
        continue;
      }

    }

  }

  return nos;
}

/**.......................................................................
 * Return a vector of astro bands for which the named configuration
 * specifies a mapping
 */
std::vector<unsigned> SignalPathMap::getAstroBandNosForConfiguration(std::string confName, 
								     SubarrayId saId, CorrelatorType type)
{
  std::vector<unsigned> nos = AstroBand::getAstroBandNos(type);

  std::vector<unsigned> validNos;
  for(unsigned iAb=0; iAb < nos.size(); iAb++)
    if(astroBandConfigurationIsValid(nos[iAb], confName, saId, type))
      validNos.push_back(nos[iAb]);

  return validNos;
}

/**.......................................................................
 * Return a list of correlator crates with inputs corresponding to
 * currently configured AstroBands
 */
std::vector<CorrelatorCrateSpec> SignalPathMap::getActiveCorrelatorCrates(CorrelatorType type)
{
  std::map<std::string, CorrelatorCrate*> crateMap;
  const util::CorrelatorSet corrset( type );

  std::vector<CorrelatorCrateSpec> crates;

  for(unsigned iAb=0; iAb < astroBands_.size(); iAb++) {
    AstroBand* ab = astroBands_[iAb];

    // If this astro band is configured, iterate through its inputs

    if(ab->isConfigured_) {
      for(unsigned iInp=0; iInp < ab->inputs_.size(); iInp++) {
        AstroBandInput* abInput = ab->inputs_[iInp];

        // If this astroband input is connected to a crate input, push
        // it into the map

        if(abInput->isConnectedToCrate()) {
          CorrelatorCrate* crate = abInput->getCrateInput()->crate_;

          if( corrset.includes(crate->type_) ) { 
            crateMap[crate->name_] = crate;
          }
        }
      }
    }
  }

  // Now we have a map of unique crates. Convert to
  // CorrelatorCrateSpec and return

  crates.resize(crateMap.size());
  unsigned iCrate=0;

  for(std::map<std::string, CorrelatorCrate*>::iterator iter=crateMap.begin();
      iter != crateMap.end(); iter++, iCrate++) {
    CorrelatorCrate* crate = iter->second;
    crates[iCrate].type_    = crate->type_;
    crates[iCrate].crateNo_ = crate->crateNo_;
  }

  return crates;
}

std::vector<CorrelatorCrateInputSpec> SignalPathMap::getCorrelatorCrateInputMap(CorrelatorCrateSpec crateSpec)
{
  CorrelatorCrate* crate = getCrate(crateSpec.type_, crateSpec.crateNo_);

  std::vector<CorrelatorCrateInputSpec> inputs(crate->inputs_.size());

  for(unsigned iInp=0; iInp < crate->inputs_.size(); iInp++) {
    CorrelatorCrateInput* input = crate->inputs_[iInp];
    AntennaIF* antIF = 0;

    inputs[iInp].crate_.type_    = crate->type_;
    inputs[iInp].crate_.crateNo_ = crate->crateNo_;
    inputs[iInp].inputNo_        = input->inputNo_;

    try {
      antIF = input->getBdcBand()->getBdcInput()->getSwitch()->getCurrentChannel()->getAntennaIF();
      inputs[iInp].antIF_.antNo_   = antIF->antenna_->antNo_;
      inputs[iInp].antIF_.polType_ = antIF->polType_;
    } catch(...) {
      inputs[iInp].antIF_.antNo_   = 0;
      inputs[iInp].antIF_.polType_ = POL_NONE;
    }
  }

  return inputs;
}

/**.......................................................................
 * Return a list of correlator bands with inputs corresponding to
 * currently configured AstroBands
 */
std::vector<CorrelatorBandSpec> SignalPathMap::getActiveCorrelatorBands(CorrelatorType type)
{
  std::map<std::string, CorrelatorBand*> bandMap;

  std::vector<CorrelatorBandSpec> bands;

  for(unsigned iAb=0; iAb < astroBands_.size(); iAb++) {

    AstroBand* ab = astroBands_[iAb];

    // If this astro band is configured, iterate through its inputs

    if(ab->isConfigured_) {

      for(unsigned iInp=0; iInp < ab->inputs_.size(); iInp++) {
	AstroBandInput* abInput = ab->inputs_[iInp];

	//------------------------------------------------------------
	// If this astroband input is connected to a crate input, push
	// it into the map
	//------------------------------------------------------------

	if(abInput->isConnectedToCrate() && abInput->isConfigured_) {

	  CorrelatorCrateInput* crateInput = abInput->getCrateInput();

	  if(type & crateInput->crate_->type_) {

	    if(crateInput->corrBandInput_) {
	      CorrelatorBandInput* bandInput = crateInput->corrBandInput_;
	      bandInput->band_->tmpType_ = abInput->band_->type_;
	      bandMap[bandInput->band_->name_] = bandInput->band_;
	    }
	  }
	}

	//------------------------------------------------------------
	// Also push it onto the map if it is connected to a
	// correlator band
	//------------------------------------------------------------

	if(abInput->isConnectedToBand() && abInput->isConfigured_) {

	  CorrelatorBandInput* bandInput = abInput->getBandInput();

	  if(type & bandInput->band_->type_) 
	    bandMap[bandInput->band_->name_] = bandInput->band_;
	}

      }
    }
  }

  // Now we have a map of unique bands. Convert to CorrelatorBandSpec
  // and return

  bands.resize(bandMap.size());
  unsigned iBand=0;

  for(std::map<std::string, CorrelatorBand*>::iterator iter=bandMap.begin();
      iter != bandMap.end(); iter++, iBand++) {
    CorrelatorBand* band = iter->second;
    bands[iBand].crate_.type_    = band->crate_ ? band->crate_->type_ : band->tmpType_;
    bands[iBand].crate_.crateNo_ = band->crate_ ? band->crate_->crateNo_ : 0;
    bands[iBand].bandNo_         = band->bandNo_;
  }

  return bands;
}

/**.......................................................................
 * Return a vector of antenna numbers required by this astro band
 */
std::vector<AntennaSpec> SignalPathMap::getAntennas(unsigned astroBandNo, bool all)
{
  std::map<unsigned, Antenna*> antennaMap;

  AstroBand* ab = getAstroBand(astroBandNo);

  // If this astro band is configured, iterate through its inputs
  
  if(ab->isConfigured_) {
    
    for(unsigned iInp=0; iInp < ab->inputs_.size(); iInp++) {
      AstroBandInput* abInput = ab->inputs_[iInp];
      
      //------------------------------------------------------------
      // If this astroband input is connected to an antenna IF, push
      // its parent onto the map
      //------------------------------------------------------------
      
      if(abInput->isUsedByConfiguredAstroBand()) {

	try {
	  Antenna* antenna = abInput->getAntennaIF()->getAntenna();
	  
	  // Map this antenna if either all antennas in this astroband
	  // were requested, or if it belongs to the subarray
	  // controlling this astroband
	  
	  if(all || antenna->subarrayId_ == ab->subarrayId_)
	    antennaMap[antenna->antNo_] = antenna;
	  
	} catch(...) {
	  continue;
	}
	
      }

    }
  }

  // Now iterate through the map, and return as a vector of AntennaSpec
  
  std::vector<AntennaSpec> antVec(antennaMap.size());

  unsigned iAnt=0;
  for(std::map<unsigned, Antenna*>::iterator iter=antennaMap.begin(); iter != antennaMap.end(); iter++, iAnt++) {
    Antenna* ant = iter->second;
    antVec[iAnt].antNo_      = ant->antNo_;
    antVec[iAnt].type_       = ant->type_;
    antVec[iAnt].subarrayId_ = ant->subarrayId_;
    antVec[iAnt].walshColNo_ = ant->walshCol_.walshColNo_;
  }

  return antVec;
}

/**.......................................................................
 * Return a list of correlator bands with inputs corresponding to
 * the requested astroband
 */
std::vector<CorrelatorBandSpec> SignalPathMap::getCorrelatorBands(unsigned astroBandNo)
{
  std::map<std::string, CorrelatorBand*> bandMap;
  std::vector<CorrelatorBandSpec> bands;

  unsigned iStart = astroBandNo==0 ? 0 : astroBandNo-1;
  unsigned iStop  = astroBandNo==0 ? AstroBand::nBandMax_ : astroBandNo;

  for(unsigned iAstroBand=iStart; iAstroBand < iStop; iAstroBand++) {
    AstroBand* ab = getAstroBand(iAstroBand+1);

    // If this astro band is configured, iterate through its inputs
  
    if(ab->isConfigured_) {
      
      for(unsigned iInp=0; iInp < ab->inputs_.size(); iInp++) {
	AstroBandInput* abInput = ab->inputs_[iInp];
	
	//------------------------------------------------------------
	// If this astroband input is connected to a crate input, push
	// it into the map
	//------------------------------------------------------------
	
	if(abInput->isConnectedToCrate() && abInput->isConfigured_) {

	  CorrelatorCrateInput* crateInput = abInput->getCrateInput();
	  
	  if(crateInput->corrBandInput_) {
	    CorrelatorBandInput* bandInput = crateInput->corrBandInput_;
	    bandMap[bandInput->band_->name_] = bandInput->band_;
	  }

	  //------------------------------------------------------------
	  // Astroband inputs can also now be connected directly to
	  // CorrelatorBandInputs
	  //------------------------------------------------------------

	} else if(abInput->isConnectedToBand() && abInput->isConfigured_) {
	  CorrelatorBandInput* bandInput = abInput->getBandInput();
	  bandInput->band_->tmpType_ = abInput->band_->type_;
	  bandMap[bandInput->band_->name_] = bandInput->band_;
	}

      }
    }
  }
  
  // Now we have a map of unique bands. Convert to CorrelatorBandSpec
  // and return
  
  bands.resize(bandMap.size());
  unsigned iBand=0;
  
  for(std::map<std::string, CorrelatorBand*>::iterator iter=bandMap.begin();
      iter != bandMap.end(); iter++, iBand++) {
    CorrelatorBand* band = iter->second;
    bands[iBand].crate_.type_    = band->crate_ ? band->crate_->type_ : band->tmpType_;
    bands[iBand].crate_.crateNo_ = band->crate_ ? band->crate_->crateNo_ : 0;
    bands[iBand].bandNo_         = band->bandNo_;
  }

  return bands;
}

/**.......................................................................
 * Get the mapping of correlator band inputs to correlator crate
 * inputs and antenna IFs.
 */
std::vector<CorrelatorBandInputSpec> SignalPathMap::getCorrelatorBandInputMap(CorrelatorBandSpec bandSpec)
{
  CorrelatorBand* band = getCorrBand(bandSpec.crate_.type_, bandSpec.bandNo_);

  std::vector<CorrelatorBandInputSpec> inputs(band->inputs_.size());

  for(unsigned iInp=0; iInp < band->inputs_.size(); iInp++) {
    CorrelatorBandInput* input = band->inputs_[iInp];

    //------------------------------------------------------------
    // Not all band inputs are connected to crates anymore.  Some are
    // just connected directly to astroband inputs
    //------------------------------------------------------------

    inputs[iInp].band_.crate_.type_    = band->crate_ ? band->crate_->type_ : band->type_;
    inputs[iInp].band_.crate_.crateNo_ = band->crate_ ? band->crate_->crateNo_ : 0;
    inputs[iInp].band_.bandNo_         = band->bandNo_;
    inputs[iInp].inputNo_              = input->inputNo_;

    try {
      AntennaIF* antIF = 0;
      antIF = input->getAntennaIF();
      inputs[iInp].antIF_.antNo_   = antIF->antenna_->antNo_;
      inputs[iInp].antIF_.polType_ = antIF->polType_;

    } catch(...) {
      inputs[iInp].antIF_.antNo_   = 0;
      inputs[iInp].antIF_.polType_ = POL_NONE;
    }

    try {
      AstroBandInput* astroBandInput = 
	input->getCrateInput()->getAstroBandInput();

      if(astroBandInput->isUsedByConfiguredAstroBand()) {
	inputs[iInp].astroBandInput_.astroBandNo_ = astroBandInput->band_->bandNo_;
	inputs[iInp].astroBandInput_.inputNo_     = astroBandInput->inputNo_;
      } else {
	inputs[iInp].astroBandInput_.astroBandNo_ = 0;
	inputs[iInp].astroBandInput_.inputNo_     = 0;
      }

    } catch(...) {
      inputs[iInp].astroBandInput_.astroBandNo_ = 0;
      inputs[iInp].astroBandInput_.inputNo_     = 0;
    }
  }

  return inputs;
}

/**.......................................................................
 * Return a list of unique polarization states occurring in the
 * requested astroband.
 *
 * If all=true, return unique polarizations of any antenna occurring
 * in this astroband.
 *
 * If all=false, return unique polarizations only of antennas
 * currently belonging to the subarray controlling this astroband
 */
std::vector<PolarizationType> 
SignalPathMap::getPolarizations(unsigned astroBandNo, bool all)
{
  std::map<PolarizationType, PolarizationType> polMap;
  std::vector<PolarizationType> pols;

  unsigned iStart = astroBandNo==0 ? 0 : astroBandNo-1;
  unsigned iStop  = astroBandNo==0 ? AstroBand::nBandMax_ : astroBandNo;

  for(unsigned iAstroBand=iStart; iAstroBand < iStop; iAstroBand++) {
    AstroBand* ab = getAstroBand(iAstroBand+1);

    // If this astro band is configured, iterate through its inputs
  
    if(ab->isConfigured_) {
      
      for(unsigned iInp=0; iInp < ab->inputs_.size(); iInp++) {
	AstroBandInput* abInput = ab->inputs_[iInp];
	
	// If this astroband input is connected to a crate input, push
	// it into the map
	
	if(abInput->isConfigured_) {

	  try {
	    AntennaIF* antIF = abInput->getAntennaIF();
	    Antenna* antenna = antIF->getAntenna();
	    
	    // Map this antenna if either all antennas in this astroband
	    // were requested, or if it belongs to the subarray
	    // controlling this astroband
	    
	    if(all || antenna->subarrayId_ == ab->subarrayId_)
	      polMap[antIF->polType_] = antIF->polType_;
	    
	  } catch(...) {
	    continue;
	  }
	}

      }
    }
  }
  
  // Now we have a map of unique polarizations.  Convert to a vector and return.
  
  pols.resize(polMap.size());
  unsigned iPol=0;
  
  for(std::map<PolarizationType, PolarizationType>::iterator iter=polMap.begin();
      iter != polMap.end(); iter++, iPol++) {

    PolarizationType polType = iter->second;
    pols[iPol] = polType;
  }

  return pols;
}

std::vector<WalshColumnAssignment> SignalPathMap::getWalshColumnAssignment(unsigned antNo)
{
  std::vector<WalshColumnAssignment> wcs;

  if(antNo == 0) {

    wcs.resize(nAnt_);
    for(unsigned i=0; i < nAnt_; i++) {
      Antenna* ant = antennas_[i];
      wcs[i].antNo_      = ant->antNo_;
      wcs[i].walshColNo_ = ant->walshCol_.getWalshColNo();
    }

  } else {

    validateAntenna(antNo);
    wcs.resize(1);
    Antenna* ant = getAntenna(antNoToAntName(antNo));
    wcs[0].antNo_      = ant->antNo_;
    wcs[0].walshColNo_ = ant->walshCol_.getWalshColNo();

  }

  return wcs;
}

void SignalPathMap::validateAntennaIFToCorrMapping(std::string ifSpec, std::string corrSpec, unsigned baseIndex)
{
  mapAntennaIFToCorr(ifSpec, corrSpec, false, baseIndex);
}

/**.......................................................................
 * Map a set of antenna IFs to a set of device inputs.  
 * 
 * Can either be a mapping to block downconverter inputs, or a mapping
 * to correlator crate inputs.
 */
void SignalPathMap::mapAntennaIFToInput(std::string antennaIFSpec, std::string inputSpec, 
					bool doMapping, unsigned baseIndex, CorrelatorType type, AstroBandInfo* info)
{
  String inputStr(inputSpec);

  if(inputStr.contains("COR")) {
    mapAntennaIFToCorr(antennaIFSpec, inputSpec, doMapping, baseIndex, type, info);
  } else if(inputStr.contains("BD")) {
    mapAntennaIFToBdc(antennaIFSpec,  inputSpec, doMapping, baseIndex, type, info);
  } else if(inputStr.contains("DIG")) {
    mapAntennaIFToDigitizer(antennaIFSpec, inputSpec, doMapping, baseIndex, type, info);
  } else {
    ThrowColorError("Unrecognized input specification: " << inputSpec << "." << std::endl
		    << "You should either specify block converter bands (e.g., BD[1-15]:[1-8]),"
		    << " correlator crate inputs (e.g., SLCOR[1-8]:[1-15]),"
		    << " or digitizers (e.g., DIG[1-46])", "red");
  }
}

void SignalPathMap::validateAntennaIFToInputMapping(std::string antennaIFSpec, std::string inputSpec, unsigned baseIndex)
{
  mapAntennaIFToInput(antennaIFSpec, inputSpec, false, baseIndex);
}

/**.......................................................................
 * Parse specification of a set of antenna IFs
 *
 * Ifs can be specified like:
 *
 *  C*R
 *  C12L:A
 *  C12L:[A,B]
 *  C[12,13]L:A
 *  C[12,13]L:[A,B]
 *  C[1-15]L:A
 *  C[1-15]L:[A-D]
 */
void SignalPathMap::parseIfSpecification(std::string ifSpec, 
					 PolarizationType& type, 
					 std::vector<unsigned>& antNos, 
					 std::vector<SplitterChannelId>& channels)
{
  String str(ifSpec);
  str = str.toUpper();
  str.strip(' ');

  bool isLeft = str.contains("L");
  String antStr, chanStr;

  if(isLeft) {
    type = POL_LEFT;
    antStr  = str.findNextInstanceOf("C", true, "L", true, false);
    chanStr = str.findNextInstanceOf(":", true, " ", false);
  } else if(str.contains("R")) {
    type = POL_RIGHT;
    antStr  = str.findNextInstanceOf("C", true, "R", true, false);
    chanStr = str.findNextInstanceOf(":", true, " ", false);
  } else {
    ThrowColorError("You must specify a valid polarization code (L|R)", "red");
  }

  // Get the CARMA ids corresponding to this specification

  antNos = extractIndexRange(antStr, 1, nAnt_);
  
  // If LEFT polarization was specified, we also need the switch
  // channels

  channels.resize(0);
  if(isLeft && !chanStr.isEmpty()) {

    chanStr.replace('A', '1');
    chanStr.replace('B', '2');
    chanStr.replace('C', '3');
    chanStr.replace('D', '4');
      
    std::vector<unsigned> chanNos = extractIndexRange(chanStr, 1, 4);
      
    channels.resize(chanNos.size());
      
    for(unsigned i=0; i < chanNos.size(); i++) {
      channels[i] = splitterChannelNumberToChannelId(chanNos[i]);
    }

  }
}

std::vector<SplitterChannelId> 
SignalPathMap::getDefaultSplitterChannelsForPolarization(PolarizationType type)
{
  std::vector<SplitterChannelId> channels;

  // If LEFT polarization was specified, we also need the switch
  // channels

  if(type == POL_LEFT) {
    channels.push_back(SP_CHAN_A);
    channels.push_back(SP_CHAN_B);
    channels.push_back(SP_CHAN_C);
    channels.push_back(SP_CHAN_D);

    // Else the switch channel is irrelevant (for RIGHT)

  } else {
    channels.push_back(SP_CHAN_NONE);
  }

  return channels;
}

/**.......................................................................
 * Parse specification of a set of switch inputs
 *
 * Switches can be specified like:
 *
 *  SW12:1
 *  SW12:[1,2]
 *  SW[12,13]:1
 *  SW[12,13]:[1,2]
 *  SW[1-15]:1
 *  SW[1-15]:[1-3]
 */
void SignalPathMap::parseSwitchSpecification(std::string ifSpec, 
					     std::vector<unsigned>& switchIds, 
					     std::vector<SwitchChannelId>& channels)
{
  String str(ifSpec);
  str = str.toUpper();
  str.strip(' ');

  String swStr, chanStr;

  swStr   = str.findNextInstanceOf("SW", true, ":", true, false);
  chanStr = str.findNextInstanceOf(":", true, " ", false);

  // Get the switch ids corresponding to this specification

  switchIds = extractIndexRange(swStr, 1, nSwitch_);

  // Get the switch channel specification too

  std::vector<unsigned> chanNos = extractIndexRange(chanStr, 1, 4);
  
  channels.resize(chanNos.size());
  
  for(unsigned i=0; i < chanNos.size(); i++) {
    channels[i] = switchChannelNumberToChannelId(chanNos[i]);
  }
}
 
/**.......................................................................
 * Parse specification of a set of downconverter bands
 *
 * Bands can be specified like:
 *
 *  BD12
 *  BD12:1
 *  BD12:[1,2]
 *  BD[12,13]:1
 *  BD[12,13]:[1,2]
 *  BD[1-15]:1
 *  BD[1-15]:[1-3]
 */
void SignalPathMap::parseBdcSpecification(std::string bdcSpec, 
					  std::vector<unsigned>& bdcIndices, 
					  std::vector<unsigned>& bandIndices,
					  unsigned baseIndex,
					  bool actualIndex)
{
  String str(bdcSpec);
  str = str.toUpper();
  str.strip(' ');

  String bdcStr, bandStr;

  bdcStr  = str.findNextInstanceOf("BD", true, ":", false, false);
  bandStr = str.findNextInstanceOf(":",  true, " ", false);

  // Get the bdc indices corresponding to this specification

  bdcIndices = extractIndexRange(bdcStr, 1, nBdc_);
  
  // If specific bands were specified, parse these now

  if(!bandStr.isEmpty()) {
    unsigned maxChan = BlockDownconverter::nBand(CORR_WB);
    bandIndices = extractIndexRange(bandStr, 1, maxChan, baseIndex, actualIndex);
  } else {
    bandIndices.resize(0);
  }
}

/**.......................................................................
 * Parse specification of a set of correlator crate inputs
 *
 * Correlators can be specified like:
 *
 *  C1R        --> SLCOR:1           Map C1R to input 1 of all SLCOR crates
 *  C1R        --> SLCOR[1-3]:1      Map C1R to input 1 of SLCOR[1-3]
 *  C[1-15]L:A --> SLCOR[1-8]:[1-15] Map C[1-15]L:A to inputs 1-15 of SLCOR[1-8]
 */
void SignalPathMap::parseCorrSpecification(std::string crateSpec, 
					   std::vector<std::string>& crateNames,
					   std::vector<unsigned>& inputIndices,
					   unsigned baseIndex,
					   bool actualIndex)
{
  String str(crateSpec);
  str = str.toUpper();
  str.strip(' ');

  String crateStr, inputStr;
  unsigned nCrate=0;
  std::string prefix;

  if(str.contains("SLCOR")) {
    crateStr  = str.findNextInstanceOf("SLCOR", true, ":", false, false);
    inputStr  = str.findNextInstanceOf(":",  true, " ", false);
    nCrate = CorrelatorCrate::nCrateSl_;
    prefix = "SLCOR";
  } else if(str.contains("WBCOR")) {
    crateStr  = str.findNextInstanceOf("WBCOR", true, ":", false, false);
    inputStr  = str.findNextInstanceOf(":",  true, " ", false);
    nCrate = CorrelatorCrate::nCrateWb_;
    prefix = "WBCOR";
  } else {
    ThrowColorError("Unrecognized correlator specification: " << str, "red");
  }

  // Get the crate indices corresponding to this specification

  std::vector<unsigned> crateIndices;
  crateIndices = extractIndexRange(crateStr, 1, nCrate, baseIndex, actualIndex);

  crateNames.resize(crateIndices.size());
  ostringstream os;
  for(unsigned i=0; i < crateIndices.size(); i++) {
    os.str("");
    os << prefix << crateIndices[i];
    crateNames[i] = os.str();
  }
  
  // If specific inputs were specified, parse these now

  if(!inputStr.isEmpty()) {
    inputIndices = extractIndexRange(inputStr, 1, CorrelatorCrate::nInput_);
  } else {
    inputIndices.resize(0);
  }
}

/**.......................................................................
 * Parse specification of a set of correlator band inputs
 *
 * Correlators can be specified like:
 *
 *  SLCOR:1           input 1 of all SLCOR crates
 *  SLCOR[1-3]:1      input 1 of SLCOR[1-3]
 *  SLCOR[1-8]:[1-15] inputs 1-15 of SLCOR[1-8]
 */
void SignalPathMap::parseCorrBandSpecification(std::string bandSpec, 
					       std::vector<std::string>& bandNames,
					       std::vector<unsigned>& inputIndices,
					       unsigned baseIndex,
					       bool actualIndex)
{
  String str(bandSpec);
  str = str.toUpper();
  str.strip(' ');

  String bandStr, inputStr;
  unsigned nBand=0;
  std::string prefix;

  if(str.contains("C3G")) {
    bandStr  = str.findNextInstanceOf("C3G", true, ":", false, false);
    inputStr  = str.findNextInstanceOf(":",  true, " ", false);
    nBand = BlockDownconverter::nBandC3g_;
    prefix = "C3G";
  } else {
    ThrowColorError("Unrecognized correlator band specification: " << str, "red");
  }

  // Get the band indices corresponding to this specification

  std::vector<unsigned> bandIndices;
  bandIndices = extractIndexRange(bandStr, 1, nBand, baseIndex, actualIndex);

  bandNames.resize(bandIndices.size());
  ostringstream os;
  for(unsigned i=0; i < bandIndices.size(); i++) {
    os.str("");
    os << prefix << bandIndices[i];
    bandNames[i] = os.str();
  }
  
  // If specific inputs were specified, parse these now

  if(!inputStr.isEmpty()) {
    inputIndices = extractIndexRange(inputStr, 1, CorrelatorBand::nInputC3g_);
  } else {
    inputIndices.resize(0);
  }
}

/**.......................................................................
 * Parse specification of a set of digitizers
 *
 * Digitizers can be specified like:
 *
 *  DIG1
 *  DIG[12,13]
 *  DIG[1-23]
 */
void SignalPathMap::parseDigitizerSpecification(std::string digSpec, std::vector<string>& digIds,
						unsigned baseIndex, bool actualIndex)

{
  String str(digSpec);
  str = str.toUpper();
  str.strip(' ');

  String digStr = str.findNextInstanceOf("DIG", true, " ", false, false);

  // Get the digitizer ids corresponding to this specification

  std::vector<unsigned> digNos = extractIndexRange(digStr, 1, nDigitizer_, baseIndex, actualIndex);
  digIds.resize(digNos.size());

  std::ostringstream os;
  for(unsigned iDig=0; iDig < digNos.size(); iDig++) {
    os.str("");
    os << "DIG" << digNos[iDig];
    digIds[iDig] = os.str();
  }

  return;
}

/**.......................................................................
 * Parse an index range of the form [1,2,3] or [3-5] or 6, into a
 * vector of requested indices.
 */
std::vector<unsigned> SignalPathMap::extractIndexRange(String& indStr, unsigned lowestValid, unsigned highestValid,
						       unsigned baseIndex, bool actualIndex)
{
  std::vector<unsigned> indices;
  
  //------------------------------------------------------------
  // If the string contains a "[", then this is a list or range of
  // indices
  //------------------------------------------------------------
  
  if(indStr.contains("[")) {

    // Is this a range? (ie, [1-15] )

    if(indStr.contains("-")) {
      
      // Was an increment specified?
      
      unsigned iStart, iStop, incr=1;

      String startStr;
      String stopStr;
      
      if(indStr.contains(";")) {
	startStr = indStr.findNextInstanceOf("[", true, "-", true, false);
	stopStr  = indStr.findNextInstanceOf("-", true, ";", true, false);
	
	String incrStr  = indStr.findNextInstanceOf(";", true, "]", true, false);
	incr = incrStr.toInt();
      } else {
	startStr = indStr.findNextInstanceOf("[", true, "-", true, false);
	stopStr  = indStr.findNextInstanceOf("-", true, "]", true, false);
      }
      
      iStart = parseIndexExpression(startStr, baseIndex, actualIndex, lowestValid, highestValid);
      iStop  = parseIndexExpression(stopStr,  baseIndex, actualIndex, lowestValid, highestValid);

      if(iStart > iStop) {
	ThrowColorError("Invalid range: " << indStr 
			<< " (Your start index must be less than your stop index)", "red");
      }
      
      for(unsigned iInd=iStart; iInd <= iStop; iInd += incr) {
	addIndex(indices, iInd, lowestValid, highestValid);
      }

      // Else a list of indices? (ie, [1,2,3] )
      
    } else if(indStr.contains(",")) {

      String ind;

      ind = indStr.findNextInstanceOf("[", true, ",", true, false);

      if(!ind.isEmpty()) {
	addIndex(indices, parseIndexExpression(ind, baseIndex, actualIndex, lowestValid, highestValid), 
		 lowestValid, highestValid);
      } else {
	ThrowColorError("Invalid list: " << indStr 
			<< " (A list of indices should be of the form: [1,2,3])", "red");
      }

      do {
	ind = indStr.findNextInstanceOf(",", true, ",", true, false);

	if(!ind.isEmpty()) {
	  addIndex(indices, parseIndexExpression(ind, baseIndex, actualIndex, lowestValid, highestValid), 
		   lowestValid, highestValid);
	}

      } while(!ind.isEmpty());

      ind = indStr.findNextInstanceOf(",", true, "]", true, false);

      if(!ind.isEmpty()) {
	addIndex(indices, parseIndexExpression(ind, baseIndex, actualIndex, lowestValid, highestValid), 
		 lowestValid, highestValid);
      } else {
	ThrowColorError("Invalid list: " << indStr 
			<< " (A list of indices should be of the form: [1,2,3])", "red");
      }

      // Else a single index was specified

    } else {
      String ind = indStr.findNextInstanceOf("[", true, "]", false);
      addIndex(indices, parseIndexExpression(ind, baseIndex, actualIndex, lowestValid, highestValid), 
	       lowestValid, highestValid);
    }

    //------------------------------------------------------------
    // Else this was not a list.  Check for allowed regexp (*)
    //------------------------------------------------------------

  } else {

    if(indStr[0] == '*') {

      for(unsigned iInd=lowestValid; iInd <= highestValid; iInd++) {
	addIndex(indices, iInd, lowestValid, highestValid);
      }

      // Else this was a single number

    } else {
      addIndex(indices, indStr.toInt(), lowestValid, highestValid);
    }
  }

  return indices;
}

/**.......................................................................
 * Add an index to a vector of requested indices
 */
void SignalPathMap::addIndex(std::vector<unsigned>& indices, unsigned index, 
			     unsigned lowestValid, unsigned highestValid)
{
  if(index < lowestValid || index > highestValid) {
    ThrowColorError("Invalid index: " 
		    << index << " (should be " << lowestValid << "-" << highestValid << ")", "red");
  }

  indices.push_back(index);
}

/**.......................................................................
 * Parse an index expression of the form N+M*i
 */
unsigned SignalPathMap::parseIndexExpression(String& str, 
					     unsigned baseIndex,   unsigned actualIndex, 
					     unsigned lowestValid, unsigned highestValid)
{
  unsigned op1, op2;
  unsigned index = baseIndex;

  if(str.contains("+")) {
    parseIndexOperands(str, op1, op2, "+", baseIndex, actualIndex, lowestValid, highestValid);
    index = op1+op2;
  } else if(str.contains("*")) {
    parseIndexOperands(str, op1, op2, "*", baseIndex, actualIndex, lowestValid, highestValid);
    index = op1*op2;
  } else if(str.contains("ANY")) {

    if(baseIndex == 0 && actualIndex) {
      ThrowColorError("No base index has been specified for an implicit rule: " << str, "red");
    }

    if(!actualIndex) {
      index = lowestValid;
    }

  } else if(str.contains("EVEN")) {

    if(actualIndex) {
      if(baseIndex % 2 != 0) {
	ThrowColorError("An implicit rule requires an even index (" << str << "), but you have specified an odd index: " << baseIndex, "red");
      } 
    } else {
      index = firstEvenIndex(lowestValid, highestValid);
    }

  } else if(str.contains("ODD")) {

    if(actualIndex) {
      if(baseIndex % 2 == 0) {
	ThrowColorError("An implicit rule requires an odd index (" << str << "), but you have specified an even index: " << baseIndex, "red");
      }
    } else {
      index = firstOddIndex(lowestValid, highestValid);
    }
  } else {
    index = str.toInt();
  }

  return index;
}

/**.......................................................................
 * Return the value of two operands in an expression like: 'i op j'
 */
void SignalPathMap::parseIndexOperands(String& str, unsigned& op1, unsigned& op2, std::string op,
				       unsigned baseIndex,   unsigned actualIndex, 
				       unsigned lowestValid, unsigned highestValid)

{
  String op1Str = str.findNextInstanceOf(" ", false, op, true, false);
  String op2Str = str.findNextInstanceOf(op,  true,  op, false, false);

  op1 = parseIndexExpression(op1Str, baseIndex, actualIndex, lowestValid, highestValid);
  op2 = parseIndexExpression(op2Str, baseIndex, actualIndex, lowestValid, highestValid);
}

unsigned SignalPathMap::firstEvenIndex(unsigned lowestValid, unsigned highestValid)
{
  if(lowestValid % 2 == 0)
    return lowestValid;

  if(lowestValid+1 > highestValid) {

    ThrowColorError("No valid even index can be constructed from the range: [" << lowestValid << "-" << highestValid << "]", "red");

    // We will never get here -- just to avoid compiler warnings.

    return 0;

  } else {
    return lowestValid+1;
  }
}

unsigned SignalPathMap::firstOddIndex(unsigned lowestValid, unsigned highestValid)
{
  if(lowestValid % 2 != 0)
    return lowestValid;

  if(lowestValid+1 > highestValid) {

    ThrowColorError("No valid odd index can be constructed from the range: [" << lowestValid << "-" << highestValid << "]", "red");

    // We will never get here -- just to avoid compiler warnings.

    return 0;

  } else {
    return lowestValid+1;
  }
}

/**.......................................................................
 * Convert from 0-relative index for splitter channels to splitter
 * channel ID
 */
SplitterChannelId SignalPathMap::splitterChannelNumberToChannelId(unsigned iChan)
{
  std::map<unsigned, SplitterChannelId> chanMap;

  chanMap[0] = SP_CHAN_NONE;
  chanMap[1] = SP_CHAN_A;
  chanMap[2] = SP_CHAN_B;
  chanMap[3] = SP_CHAN_C;
  chanMap[4] = SP_CHAN_D;

  if(chanMap.find(iChan) == chanMap.end()) {
    ThrowColorError("Invalid splitter channel index: " << iChan, "red");
  }

  return chanMap[iChan];
}

/**.......................................................................
 * Convert from 0-relative index for switch channels to switch
 * channel ID
 */
SwitchChannelId SignalPathMap::switchChannelNumberToChannelId(unsigned iChan)
{
  std::map<unsigned, SwitchChannelId> chanMap;

  chanMap[0] = SW_CHAN_NONE;
  chanMap[1] = SW_CHAN_1;
  chanMap[2] = SW_CHAN_2;
  chanMap[3] = SW_CHAN_3;
  chanMap[4] = SW_CHAN_4;

  if(chanMap.find(iChan) == chanMap.end()) {
    ThrowColorError("Invalid switch channel index: " << iChan, "red");
  }

  return chanMap[iChan];
}

/**.......................................................................
 * Convert from switch channel ID to 0-relative index for switch
 * channels
 */
unsigned SignalPathMap::switchChannelIdToChannelNumber(SwitchChannelId id)
{
  std::map<SwitchChannelId, unsigned> chanMap;

  chanMap[SW_CHAN_NONE] = 0;
  chanMap[SW_CHAN_1]    = 1; 
  chanMap[SW_CHAN_2]    = 2; 
  chanMap[SW_CHAN_3]    = 3; 
  chanMap[SW_CHAN_4]    = 4;

  if(chanMap.find(id) == chanMap.end()) {
    ThrowColorError("Invalid switch channel id: " << id, "red");
  }

  return chanMap[id];
}

std::string SignalPathMap::antNoToAntName(unsigned antNo)
{
  std::ostringstream os;
  os << "C" << antNo;
  return os.str();
}

signalpath::CorrelatorType SignalPathMap::corrNameToCorrType(std::string name)
{
  String nameStr(name);
  nameStr = nameStr.toUpper();

  if(nameStr.str() == "SLCOR") {
    return CORR_SL;
  } else if (nameStr.str() == "WBCOR") {
    return CORR_WB;
  } else if (nameStr.str() == "C3G") { //? added by MWP
    return CORR_C3G;
  }

  ThrowColorError("Unrecognized correlator type: " << name, "red");
  
  return CORR_NONE;
}


/**.......................................................................
 * Clear the current cable map
 */
void SignalPathMap::clearCableMap()
{
  for(unsigned iAnt=0; iAnt < antennas_.size(); iAnt++) {
    antennas_[iAnt]->clearIfMap();
  }

  for(unsigned iSw=0; iSw < switches_.size(); iSw++) {
    switches_[iSw]->clearChannelMap();
  }
}

/**.......................................................................
 * Clear the current configuration switchyard configuration
 */
void SignalPathMap::clearConfiguration(CorrelatorType type)
{
  const util::CorrelatorSet corrset(type);
  for(unsigned iSw=0; iSw < switches_.size(); iSw++) {
    Switch* sw = switches_[iSw];

    //if(type == CORR_ANY || type==sw->bdcInput_->bdc_->type_) {
    if( corrset.includes(sw->bdcInput_->bdc_->type_) ) {
      sw->clearSwitchSetting();
    }

  }

  for(unsigned iBdc=0; iBdc < bdcs_.size(); iBdc++) {
    BlockDownconverter* bdc = bdcs_[iBdc];

    //if(type == CORR_ANY || type==bdc->type_) {
    if( corrset.includes(bdc->type_) ) {
      bdc->clear();
    }

  }

}

/**.......................................................................
 * Clear an astro band configuration
 */
void SignalPathMap::clearAstroBandConfiguration(unsigned bandNo, SubarrayId saId, CorrelatorType type)
{
  // If no band was specified, clear all bands

  if(bandNo == 0) {
    for(unsigned iAb=0; iAb < astroBands_.size(); iAb++) {
      AstroBand* band = astroBands_[iAb];

      if(!(type & band->type_)) continue;

      // Clear all bands allowed for this correlator type.  Just
      // quietly catch any bands that aren't allowed: we interpret
      // bandNo==0 to mean 'all bands for this correlator'

      try {
        band->clear(type, saId);
      } catch(...) {
      }

    }
  } else {
    AstroBand* band = getAstroBand(bandNo);

    if(!(type & band->type_)) {
      ThrowColorError(std::endl << "You are attempting to clear an astroband (AB" << bandNo
		      << ") that you are not allowed to modify (controlling subarray owns " << type 
		      << " but this band requires " << band->type_ << ")" << std::endl, "red");
    }

    band->clear(type, saId);
  }
}

/**.......................................................................
 * Assert a named configuration for the specified astro band
 */
void SignalPathMap::configureAstroBand(unsigned bandNo, std::string confName, 
				       SubarrayId saId, CorrelatorType type)
{
  // First check that this astroband can be configured by the caller

  AstroBand* ab = getAstroBand(bandNo);
  if(!(ab->type_ & type)) {
    ThrowColorError(std::endl << "You are attempting to configure an astroband (AB" << bandNo
		      << ") that you are not allowed to modify (controlling subarray owns " << type 
		      << " but this band requires " << ab->type_ << ")" << std::endl, "red");
  }

  // First get the specified switchyard configuration 

  SwitchyardConfiguration& swConf = getConfiguration(confName);

  // Now get the astroBand configuration that corresponds to it

  AstroBandConfiguration& abConf = *swConf.astroBandConf_;

  // First set up the astroBand according to the astroBand
  // configuration associated with the requested switchyard
  // configuration

  AstroBandInfo info(bandNo, saId);
  selectAstroBandConfiguration(abConf.name_, &info, type);

  if(info.conflicted_) {
    ThrowColorError("Requested configuration '" << confName << "' for band AB" << bandNo
		    << " is in conflict with " << AstroBandInfo::listConflictedBands(&info), "red");
  }

  if(info.nDevicesConfigured_ == 0) {
    ThrowColorError("Requested configuration '" << confName << "' doesn't specify any mapping for band AB" << bandNo, "red");
  }

  // Now select the switchyard configuration

  info.reset();
  selectConfiguration(swConf.name_, 0, type, &info);

  if(info.nDevicesConfigured_ == 0) {
    ThrowColorError("Requested configuration '" << swConf.name_ << "' doesn't specify any mapping for band AB" << bandNo, "red");
  }

  // If this configuration resulted in conflicts, process them now

  if(info.conflicted_) {

    // If there were conflicts with antenna ownership, report them

    if(info.antConflictMask_ != 0x0) {
      std::ostringstream os;
      os << "Requested configuration '" << confName << "' for band AB" << bandNo
	 << " involves antennas (" << AstroBandInfo::listConflictedAntennas(&info) 
	 << ") which are owned by another subarray "
	 << "(" << info.conflictedSubarrayId_ << ")" << std::endl;
      
      ThrowColorError(os.str(), "red");
    }

    // If there were conflicts with correlator inputs, report them

    if(!info.conflictMask_.allBitsAreLow()) {
      std::ostringstream os;
      os << "Requested configuration (1)'" << confName << "' for band AB" << bandNo
	 << " would require switch settings that are incompatible with the configuration for " 
	 << AstroBandInfo::listConflictedBands(&info) << std::endl;
      os << "The following configurations are compatible: " << std::endl << std::endl
	 << listUnconflictedConfigurations(type, info);
      
      ThrowColorError(os.str(), "red");
    }
  }

  // Point this astroband to the current configuration, and mark it as
  // configured

  AstroBand* astroBand = getAstroBand(bandNo);
  astroBand->conf_         = &swConf;
  astroBand->isConfigured_ = true;
  astroBand->subarrayId_   = saId;

  // If a band was successfully configured, recalculate walsh column assignments

  configureWalshColumns();
}

/**.......................................................................
 * Return true if the requested configuration specifies a mapping for
 * this bandNo
 */
bool SignalPathMap::astroBandConfigurationSpecifiesMapping(unsigned bandNo, std::string confName, SubarrayId saId, CorrelatorType type)
{
  try {

    // First get the specified switchyard configuration 

    SwitchyardConfiguration& swConf = getConfiguration(confName);

    // Now get the astroBand configuration that corresponds to it

    AstroBandConfiguration& abConf = *swConf.astroBandConf_;

    // First validate the astroBand configuration associated with the
    // requested switchyard configuration
    
    AstroBandInfo info(bandNo, saId);
    validateAstroBandConfiguration(abConf, 0, type, &info);

    if(info.conflicted_) {
      ThrowColorError("Requested configuration '" << confName << "' for band AB" << bandNo
		      << " is in conflict with " << AstroBandInfo::listConflictedBands(&info), "red");
    }
    
    if(info.nDevicesConfigured_ == 0) {
      ThrowColorError("Requested configuration '" << confName << "' doesn't specify any mapping for band AB" << bandNo, "red");
    }

    return true;
  } catch(...) {
    return false;
  }
}

/**.......................................................................
 * Return true if the requested configuration is valid
 */
 bool SignalPathMap::astroBandConfigurationIsValid(unsigned bandNo, std::string confName, SubarrayId saId, CorrelatorType type)
{
  try {
    checkAstroBandConfiguration(bandNo, confName, saId, type);
    return true;
  } catch(...) {
    return false;
  }
}

/**.......................................................................
 * Test whether we can assert a named configuration for the specified astro band
 */
void SignalPathMap::checkAstroBandConfiguration(unsigned bandNo, std::string confName, SubarrayId saId, CorrelatorType type)
{
  // First get the specified switchyard configuration 

  SwitchyardConfiguration& swConf = getConfiguration(confName);

  // Now get the astroBand configuration that corresponds to it

  AstroBandConfiguration& abConf = *swConf.astroBandConf_;

  // First validate the astroBand configuration associated with the
  // requested switchyard configuration

  AstroBandInfo info(bandNo, saId);
  validateAstroBandConfiguration(abConf, 0, type, &info);

  if(info.conflicted_) {
    ThrowColorError("Requested configuration '" << confName << "' for band AB" << bandNo
		    << " is in conflict with " << AstroBandInfo::listConflictedBands(&info), "red");
  }

  if(info.nDevicesConfigured_ == 0) {
    ThrowColorError("Requested configuration '" << confName << "' doesn't specify any mapping for band AB" << bandNo, "red");
  }

  // If the astroband configuration is valid, assert it, so we can
  // check validity of the subsequent switchyard configuration.
  // There's no other way to do this: if we don't assert the astroband
  // configuration, then we have no mapping to antenna IFs, and the
  // subsequent check for antenna subarray ownership in
  // validateConfiguration() will fail

  selectAstroBandConfiguration(abConf.name_, &info, type);

  // Now select the switchyard configuration

  info.reset();
  validateConfiguration(swConf, 0, type, &info);

  // If there were conflicts with antenna ownership, report them

  if(info.antConflictMask_ != 0x0) {
    std::ostringstream os;
    os << "Requested configuration '" << confName << "' for band AB" << bandNo
       << " involves antennas (" << AstroBandInfo::listConflictedAntennas(&info) << ") which are owned by another subarray "
       << "(" << info.conflictedSubarrayId_ << ")" << std::endl;
    
    ThrowColorError(os.str(), "red");
  }
  
  // If there were conflicts with correlator inputs, report them
  
  if(!info.conflictMask_.allBitsAreLow()) {
    std::ostringstream os;
    os << "Requested configuration (2)'" << confName << "' for band AB" << bandNo
       << " would require switch settings that are incompatible with the configuration for " 
       << AstroBandInfo::listConflictedBands(&info) << std::endl;

    // Uncommenting this block will result in an infinite loop, since
    // listUnconflictedConfigurations() now calls
    // checkAstrobandConfiguration()

#if 0
    os << "The following configurations are compatible: " << std::endl << std::endl
       << listUnconflictedConfigurations(type, info);
#endif
    
    ThrowColorError(os.str(), "red");
  }

  // If no devices were configured, throw an error

  if(info.nDevicesConfigured_ == 0) {
    ThrowColorError("Requested configuration '" << confName << "' doesn't map anything for band AB" << bandNo, "red");
  }

}

std::string SignalPathMap::listUnconflictedConfigurations(CorrelatorType type, AstroBandInfo& info)
{
  std::ostringstream os;

  for(std::map<std::string, SwitchyardConfiguration>::iterator iter=knownConfigurations_.begin();
      iter != knownConfigurations_.end(); iter++) {

    try {
      checkAstroBandConfiguration(info.astroBandNo_, iter->second.name_, info.subarrayId_, type);
      os << iter->first << std::endl;
    } catch(...) {
    }

  }

  return os.str();
}

/**.......................................................................
 * Initialize a cable map from a file
 */
void SignalPathMap::initializeCableMap(std::string fileName)
{
  clearCableMap();

  std::ifstream ifStr;
  ifStr.open(fileName.c_str(), ios::in);
    
  if(!ifStr.is_open()) {
    ThrowSimpleError("Couldn't open file: " << fileName);
  }
    
  String line;
    
  while(!ifStr.eof()) {
    line.initialize();
    getline(ifStr, line.str());
      
    if(line.contains("C") && line[0] != '#') {
      String ifSpec    = line.findNextStringSeparatedByChars(" ", false);
      String inputSpec = line.findNextStringSeparatedByChars(" ", true);
	
      // The cable map is specified as connections between Antenna IFs
      // and hardware devices.  Antenna IFs can be connected either to
      // switches, or to digitizers.  
      //
      // (For generality, I should really add an additional mapping
      // between switches and digitizers, since this is also a
      // possibility, although there is currently no plan to use this
      // functionality)

      if(inputSpec.contains("SW")) {
	hardwareMapAntennaIFToSwitch(ifSpec.str(), inputSpec.str());
      } else {
	hardwareMapAntennaIFToDigitizer(ifSpec.str(), inputSpec.str());
      }
    }
  }

  ifStr.close();
}

/**.......................................................................
 * Initialize a default cable map
 */
void SignalPathMap::initializeDefaultCableMap()
{
  hardwareMapAntennaIFToSwitch("C[1-15]L:A",  "SW[2-30;2]:1");
  hardwareMapAntennaIFToSwitch("C[16-23]L:A", "SW[31-38]:1");

  hardwareMapAntennaIFToSwitch("C[1-7]L:B",   "SW[2-14;2]:2");
  hardwareMapAntennaIFToSwitch("C[8-15]L:B",  "SW[31-38]:2");
  hardwareMapAntennaIFToSwitch("C[16-23]L:B", "SW[16-30;2]:2");

  hardwareMapAntennaIFToSwitch("C[1-7]L:C",   "SW[31-37]:3");
  hardwareMapAntennaIFToSwitch("C[8-14]L:C",  "SW[2-14;2]:3");
  hardwareMapAntennaIFToSwitch("C15L:C",      "SW38:3");
  hardwareMapAntennaIFToSwitch("C[16-23]L:C", "SW[16-30;2]:3");

  hardwareMapAntennaIFToSwitch("C[1-7]L:D",   "SW[2-14;2]:4");
  hardwareMapAntennaIFToSwitch("C8L:D",       "SW15:3");
  hardwareMapAntennaIFToSwitch("C[9-15]L:D",  "SW[1-13;2]:3");
  hardwareMapAntennaIFToSwitch("C[16-23]L:D", "SW[16-30;2]:4");

  hardwareMapAntennaIFToSwitch("C[1-15]R",    "SW[1-29;2]:4");
}

/**.......................................................................
 * Return a handle to the requested antenna (by antenna number)
 */
Antenna* SignalPathMap::getAntenna(unsigned antNo)
{
  ostringstream os;
  os << "C" << antNo;
  return getAntenna(os.str());
}

/**.......................................................................
 * Return a handle to the requested antenna (by name)
 */
Antenna* SignalPathMap::getAntenna(std::string antName)
{
  validateAntenna(antName);
  return antennaMap_[antName];
}

/**.......................................................................
 * Return a handle to the requested astro band (by input number)
 */
AstroBand* SignalPathMap::getAstroBand(unsigned astroBandNo)
{
  validateAstroBand(astroBandNo);
  return astroBandMap_[astroBandNo];
}

/**.......................................................................
 * Check that an astroband number represents a valid astroband
 */
void SignalPathMap::validateAstroBand(unsigned astroBandNo)
{
  if(astroBandMap_.find(astroBandNo) == astroBandMap_.end()) {
    ThrowColorError("Invalid astroBand: " << astroBandNo, "red");
  }
}

/**.......................................................................
 * Return a handle to the requested block downconverter
 */
BlockDownconverter* SignalPathMap::getBdc(unsigned bdcNo)
{
  validateBdc(bdcNo);
  return bdcs_[bdcNo-1];
}

/**.......................................................................
 * Check that a block downconverter index represents a valid block
 * downconverter
 */
void SignalPathMap::validateBdc(unsigned bdcNo)
{
  if(bdcNo > bdcs_.size()) {
    ThrowColorError("No such block downconverter: " << bdcNo, "red");
  }
}

/**.......................................................................
 * Return a pointer to the requested correlator
 */
Correlator* SignalPathMap::getCorrelator(CorrelatorType type)
{
  for(unsigned iCorr=0; iCorr < correlators_.size(); iCorr++) {
    Correlator* corr = correlators_[iCorr];

    if(corr->type_ == type)
      return corr;
  }

  ThrowError("Invalid correlator requested: " << type);

  return 0;
}

/**.......................................................................
 * Check that a antenna number represents a valid antenna
 */
void SignalPathMap::validateAntenna(unsigned antNo)
{
  validateAntenna(antNoToAntName(antNo));
}

/**.......................................................................
 * Check that a antenna name represents a valid antenna
 */
void SignalPathMap::validateAntenna(std::string antName)
{
  if(antennaMap_.find(antName) == antennaMap_.end()) {
    ThrowColorError("Invalid antenna: " << antName, "red");
  }
}

/**.......................................................................
 * Return a handle to the requested band (by name)
 */
CorrelatorBand* SignalPathMap::getCorrBand(CorrelatorType type, unsigned bandNo)
{
  return getCorrBand(corrTypeAndBandNoToCorrBandName(type, bandNo));
}

/**.......................................................................
 * Return a handle to the requested band (by name)
 */
CorrelatorBand* SignalPathMap::getCorrBand(std::string bandName)
{
  validateCorrBand(bandName);
  return corrBandMap_[bandName];
}

/**.......................................................................
 * Check that a correlator band name represents a valid band
 */
void SignalPathMap::validateCorrBand(std::string bandName)
{
  if(corrBandMap_.find(bandName) == corrBandMap_.end()) {
    ThrowColorError("Invalid correlator band: " << bandName, "red");
  }
}


std::string SignalPathMap::corrTypeAndBandNoToCorrBandName(CorrelatorType type, unsigned bandNo)
{
  std::ostringstream os;

  switch (type) {
  case CORR_SL:
    os << "SLCOR" << bandNo;
    break;
  case CORR_WB:
    os << "WBCOR" << bandNo;
    break;
  case CORR_C3GMAX8:
    os << "C3G"  << bandNo;
    break;
  case CORR_C3GMAX23:
    os << "C3G"  << bandNo;
    break;
  default:
    ThrowColorError("CorelatorType " << type << " cannot be parsed to a specific type", "red");
    break;
  }

  return os.str();
}

/**.......................................................................
 * Return a handle to the requested crate (by name)
 */
CorrelatorCrate* SignalPathMap::getCrate(CorrelatorType type, unsigned crateNo)
{
  return getCrate(corrTypeAndCrateNoToCrateName(type, crateNo));
}

std::string SignalPathMap::corrTypeAndCrateNoToCrateName(CorrelatorType type, unsigned crateNo)
{
  std::ostringstream os;

  switch (type) {
  case CORR_SL:
    os << "SLCOR" << crateNo;
    break;
  case CORR_WB:
    os << "WBCOR" << crateNo;
    break;
  default:
    ThrowColorError("CorrelatorType " << type << " cannot be parsed to a specific type", "red");
    break;
  }

  return os.str();
}

/**.......................................................................
 * Return a handle to the requested digitizer (by name)
 */
Digitizer* SignalPathMap::getDigitizer(std::string digName)
{
  validateDigitizer(digName);
  return digitizerMap_[digName];
}

/**.......................................................................
 * Return a handle to the requested digitizer (by number)
 */
Digitizer* SignalPathMap::getDigitizer(unsigned digNo)
{
  std::ostringstream os;
  os << "DIG" << digNo;
  return getDigitizer(os.str());
}

/**.......................................................................
 * Check that a digitizer name represents a valid digitizer
 */
void SignalPathMap::validateDigitizer(std::string digitizerName)
{
  if(digitizerMap_.find(digitizerName) == digitizerMap_.end()) {
    ThrowColorError("Invalid digitizer: " << digitizerName, "red");
  }
}

/**.......................................................................
 * Return a handle to the requested crate (by name)
 */
CorrelatorCrate* SignalPathMap::getCrate(std::string crateName)
{
  validateCrate(crateName);
  return crateMap_[crateName];
}

/**.......................................................................
 * Check that a crate name represents a valid crate
 */
void SignalPathMap::validateCrate(std::string crateName)
{
  if(crateMap_.find(crateName) == crateMap_.end()) {
    ThrowColorError("Invalid crate: " << crateName, "red");
  }
}

/**.......................................................................
 * Return a handle to the requested switch (by number)
 */
Switch* SignalPathMap::getSwitch(unsigned switchNo)
{
  std::ostringstream os;
  os << "SW" << switchNo;
  return getSwitch(os.str());
}

/**.......................................................................
 * Return a handle to the requested switch (by name)
 */
Switch* SignalPathMap::getSwitch(std::string switchName)
{
  validateSwitch(switchName);
  return switchMap_[switchName];
}

/**.......................................................................
 * Check that a switch name represents a valid switch
 */
void SignalPathMap::validateSwitch(std::string switchName)
{
  if(switchMap_.find(switchName) == switchMap_.end()) {
    ThrowColorError("Invalid switch: " << switchName, "red");
  }
}

/**.......................................................................
 * Print the switchyard map from the antennas down
 */
std::string SignalPathMap::printDown()
{
  std::stringstream os;
  XtermManip xtm;

  os << xtm.bg("black") << xtm.fg("white") << std::endl;

  for(unsigned iAnt=0; iAnt < antennas_.size(); iAnt++) {
    os << antennas_[iAnt]->printDown() << std::endl;
  }

  os << xtm.fg("default") << xtm.bg("default") << xtm.textMode("normal");

  return os.str();
}

/**.......................................................................
 * Print an antenna down
 */
std::string SignalPathMap::printDownAntenna(std::string antName)
{
  String antStr(antName);
  antStr = antStr.toUpper();

  std::ostringstream os;
  os << getAntenna(antStr.str())->printDown() << std::endl;
  return os.str();
}

/**.......................................................................
 * Print a switch down
 */
std::string SignalPathMap::printDownSwitch(std::string switchName)
{
  String switchStr(switchName);
  switchStr = switchStr.toUpper();

  std::ostringstream os;
  os << getSwitch(switchStr.str()) << std::endl;
  return os.str();
}

/**.......................................................................
 * Print the switchyard map from the correlator inputs up
 */
std::string SignalPathMap::printUp()
{
  std::ostringstream os;
  XtermManip xtm;

  os << xtm.bg("black") << xtm.fg("white") << std::endl;

  for(unsigned iBdc=0; iBdc < bdcs_.size(); iBdc++) {
    os << bdcs_[iBdc]->printUp() << std::endl;
  }

  os << xtm.fg("default") << xtm.bg("default") << xtm.textMode("normal");

  return os.str();
}

/**.......................................................................
 * Initialize configurations we know about
 */
void SignalPathMap::initializeKnownConfigurations()
{
  addLLConfiguration();
  addRRConfiguration();
  addCarma23Configuration();
  addFullStokesConfiguration();
}

/**.......................................................................
 * Initialize astro band configurations we know about
 */
void SignalPathMap::initializeKnownAstroBandConfigurations()
{
  addSingleAstroBandConfiguration();
  addCarma23AstroBandConfiguration();
  addFullStokesAstroBandConfiguration();
  addDualPolAstroBandConfiguration();
}

/**.......................................................................
 * Add a configuration
 */
void SignalPathMap::addConfiguration(SwitchyardConfiguration& conf, std::string confName, std::string astroBandConfName)
{
  validateConfiguration(conf);

  String nameStr(confName);
  nameStr = nameStr.toUpper();

  // Set the name of ths configuration

  conf.name_ = nameStr.str();

  // And store the astroBand configuration that corresponds to it

  conf.astroBandConf_ = &getAstroBandConfiguration(astroBandConfName);
    
  // Finally, add it to the map of known configurations

  knownConfigurations_[conf.name_] = conf;
}

void SignalPathMap::addAstroBandConfiguration(std::string name, AstroBandConfiguration& conf)
{
  validateAstroBandConfiguration(conf);

  String nameStr(name);
  nameStr = nameStr.toUpper();
  conf.name_ = nameStr.str();
    
  knownAstroBandConfigurations_[conf.name_] = conf;
}

/**.......................................................................
 * Add single-crate configuration
 */
void SignalPathMap::addSingleAstroBandConfiguration()
{
  AstroBandConfiguration fpgaConf;

  //------------------------------------------------------------
  // Default WB configuration maps the first 8 inputs of each WBCOR to
  // odd-numbered AstroBands, and the next 8 inputs of each WBCOR to
  // even-numbered AstroBands
  //------------------------------------------------------------

  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("WBCOR[1-8]:[1-8]",  "AB[9-23;2]:[1-8]"));
  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("WBCOR[1-8]:[9-16]", "AB[10-24;2]:[1-8]"));

  //----------------------------------------------------------------------
  // Default SL configuration maps each SLCOR crate to the same
  // numbered AstroBand
  //----------------------------------------------------------------------

  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("SLCOR[1-8]:[1-15]", "AB[1-8]:[1-15]"));

  //-----------------------------------------------------------------------
  // Default C3G 'SINGLE' configuration maps the first 15 inputs of
  // each correlator band to the first 15 inputs of astrobands 25-32
  // (for the 15-station configuration), and inputs 16-23 of each
  // correlator band to the first 8 inputs of astrobands 33-40 (for
  // the 8-station configuration)
  //-----------------------------------------------------------------------

  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("C3G[1-8]:[1-15]",  "AB[25-32]:[1-15]"));
  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("C3G[1-8]:[16-23]", "AB[33-40]:[1-8]"));

  addAstroBandConfiguration("SINGLE",  fpgaConf);
  addAstroBandConfiguration("DEFAULT", fpgaConf);
}

/**.......................................................................
 * Add Carma23 configuration
 */
void SignalPathMap::addCarma23AstroBandConfiguration()
{
  AstroBandConfiguration fpgaConf;

  //------------------------------------------------------------
  // EML adding WB specification as of 19 Sep 2011, even though the WB
  // FPGA configuration can't be changed, so that hybrid
  // configurations (like max sens) can be associated with a single
  // FPGA configuration
  //------------------------------------------------------------

  // Default WB configuration maps the first 8 inputs of each WBCOR to
  // odd-numbered AstroBands, and the next 8 inputs of each WBCOR to
  // even-numbered AstroBands

  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("WBCOR[1-8]:[1-8]",  "AB[9-23;2]:[1-8]"));
  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("WBCOR[1-8]:[9-16]", "AB[10-24;2]:[1-8]"));

  //------------------------------------------------------------
  // Dual-band FPGA configuration maps inputs from pairs of SLCOR
  // crates
  //------------------------------------------------------------

#ifdef INTERIM_MAPPING
  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("SLCOR[1-7;2]:[1-15]", "AB[1-7;2]:[1-15]"));
  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("SLCOR[2-8;2]:[1-15]", "AB[1-7;2]:[16-30]"));
#else
  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("SLCOR[1-7;2]:[1-16]", "AB[1-7;2]:[1-16]"));
  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("SLCOR[2-8;2]:[1-16]", "AB[1-7;2]:[17-32]"));
#endif

  //-----------------------------------------------------------------------
  // Default C3G configuration maps the first 23 inputs of each
  // correlator band to the first 23 inputs of each astroband
  //-----------------------------------------------------------------------

  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("C3G[1-8]:[1-23]", "AB[25-32]:[1-23]"));

  addAstroBandConfiguration("DUAL_C23", fpgaConf);
}

/**.......................................................................
 * Add fullstokes configuration
 */
void SignalPathMap::addFullStokesAstroBandConfiguration()
{
  AstroBandConfiguration fpgaConf;

  //------------------------------------------------------------
  // EML adding WB specification as of 19 Sep 2011, even though the WB
  // FPGA configuration can't be changed, so that hybrid
  // configurations (like max sens) can be associated with a single
  // FPGA configuration
  //------------------------------------------------------------

  // Default WB configuration maps the first 8 inputs of each WBCOR to
  // odd-numbered AstroBands, and the next 8 inputs of each WBCOR to
  // even-numbered AstroBands

  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("WBCOR[1-8]:[1-8]",  "AB[9-23;2]:[1-8]"));
  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("WBCOR[1-8]:[9-16]", "AB[10-24;2]:[1-8]"));

  //------------------------------------------------------------
  // Dual-band FPGA configuration maps inputs from pairs of SLCOR
  // crates
  //------------------------------------------------------------

#ifdef INTERIM_MAPPING
  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("SLCOR[1-7;2]:[1-15]", "AB[1-7;2]:[1-15]"));
  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("SLCOR[2-8;2]:[1-15]", "AB[1-7;2]:[16-30]"));
#else
  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("SLCOR[1-7;2]:[1-16]", "AB[1-7;2]:[1-16]"));
  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("SLCOR[2-8;2]:[1-16]", "AB[1-7;2]:[17-32]"));
#endif

  addAstroBandConfiguration("DUAL_FS", fpgaConf);
}

/**.......................................................................
 * Add dualpol configuration
 */
void SignalPathMap::addDualPolAstroBandConfiguration()
{
  AstroBandConfiguration fpgaConf;

  //------------------------------------------------------------
  // EML adding WB specification as of 19 Sep 2011, even though the WB
  // FPGA configuration can't be changed, so that hybrid
  // configurations (like max sens) can be associated with a single
  // FPGA configuration
  //------------------------------------------------------------

  // Default WB configuration maps the first 8 inputs of each WBCOR to
  // odd-numbered AstroBands, and the next 8 inputs of each WBCOR to
  // even-numbered AstroBands

  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("WBCOR[1-8]:[1-8]",  "AB[9-23;2]:[1-8]"));
  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("WBCOR[1-8]:[9-16]", "AB[10-24;2]:[1-8]"));

  // Dual-band FPGA configuration maps inputs from pairs of SLCOR
  // crates

#ifdef INTERIM_MAPPING
  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("SLCOR[1-7;2]:[1-15]", "AB[1-7;2]:[1-15]"));
  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("SLCOR[2-8;2]:[1-15]", "AB[1-7;2]:[16-30]"));
#else
  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("SLCOR[1-7;2]:[1-16]", "AB[1-7;2]:[1-16]"));
  fpgaConf.inputMappings_.push_back(AstroBandInputMapping("SLCOR[2-8;2]:[1-16]", "AB[1-7;2]:[17-32]"));
#endif

  addAstroBandConfiguration("DUAL_DP", fpgaConf);
}

/**.......................................................................
 * The "default" configuration maps each antenna to the corresponding
 * block downconverter
 */
void SignalPathMap::addLLConfiguration()
{
  SwitchyardConfiguration config;

  config.ifMappings_.push_back(AntennaIFMapping("C[1-15]L:A",  "SLCOR[1-8]:[1-15]"));
  config.ifMappings_.push_back(AntennaIFMapping("C[16-23]L:A", "WBCOR[1-8]:[1-8]"));
  config.ifMappings_.push_back(AntennaIFMapping("C[16-23]L:A", "WBCOR[1-8]:[9-16]"));
  //  config.ifMappings_.push_back(AntennaIFMapping("C[1-23]L:D",  "DIG[1-23]"));

  // Add it as the 'LL' configuration

  addConfiguration(config, "LL", "SINGLE");
}

/**.......................................................................
 * The RR configuration maps RR for C1-15 to each band of the SL
 * correlator
 */
void SignalPathMap::addRRConfiguration()
{
  SwitchyardConfiguration config;

  config.ifMappings_.push_back(AntennaIFMapping("C[1-15]R",  "SLCOR[1-8]:[1-15]"));

  addConfiguration(config, "RR", "SINGLE");
}

/**.......................................................................
 * The "carma23" configuration maps L for all antennas to pairs of bands
 */
void SignalPathMap::addCarma23Configuration()
{
  SwitchyardConfiguration config;

  // Alternative mapping by corr numbers

  config.ifMappings_.push_back(AntennaIFMapping("C[1-7]L",   "SLCOR[1-7;2]:[1-7]"));
  config.ifMappings_.push_back(AntennaIFMapping("C[16-23]L", "SLCOR[1-7;2]:[8-15]"));
  config.ifMappings_.push_back(AntennaIFMapping("C[9-15]L",  "SLCOR[2-8;2]:[1-7]"));
  config.ifMappings_.push_back(AntennaIFMapping("C8L:D",     "SLCOR[2-8;2]:8"));

  addConfiguration(config, "CARMA23", "DUAL_C23");
}

/**.......................................................................
 * The "full-stokes" configuration maps both polarizations of C1-15 to
 * pairs of bands in the SL correlator
 */
void SignalPathMap::addFullStokesConfiguration()
{
  SwitchyardConfiguration config;

  config.ifMappings_.push_back(AntennaIFMapping("C[1-15]L:A", "BD[1-15]:[1-7;2]"));
  config.ifMappings_.push_back(AntennaIFMapping("C[1-15]R",   "BD[1-15]:[2-8;2]"));

  addConfiguration(config, "FULLSTOKES", "DUAL_FS");
}

/**.......................................................................
 * Load a new configuration from a file
 */
void SignalPathMap::loadConfiguration(std::string fileName, std::string confName, std::string astroBandConfName)
{
  String confStr(confName);
  confStr = confStr.toUpper();

  std::ifstream ifStr;
  ifStr.open(fileName.c_str(), ios::in);
  
  if(!ifStr.is_open()) {
    ThrowSimpleError("Couldn't open file: " << fileName);
  }

  // Initialize the new configuration

  SwitchyardConfiguration config;
  String line;

  while(!ifStr.eof()) {
    line.initialize();
    getline(ifStr, line.str());

    if(line.contains("C") && line[0] != '#') {

      String ifSpec    = line.findNextStringSeparatedByChars(" ", false);
      String inputSpec = line.findNextStringSeparatedByChars(" ", true);

      validateAntennaIFToInputMapping(ifSpec.str(), inputSpec.str());
      config.ifMappings_.push_back(AntennaIFMapping(ifSpec.str(), inputSpec.str()));
    }
  }

  // Close the file

  ifStr.close();

  // If everything has checked out, install this configuration in the
  // map of known configurations

  addConfiguration(config, confName, astroBandConfName);
}

/**.......................................................................
 * Validate a given antenna IF to BlockDownconverter mapping.  We do
 * this by calling the same mapping method with doMapping=false, so
 * that the selection is not actually asserted.
 *
 * This will throw an error if a specification is invalid, or an
 * impossible routing has been requested.
 */
void SignalPathMap::validateAntennaIFToBdcMapping(std::string ifSpec, std::string bdcSpec, unsigned baseIndex)
{
  mapAntennaIFToBdc(ifSpec, bdcSpec, false, baseIndex);
}

/**.......................................................................
 * Return the requested switchyard configuration
 */
 SwitchyardConfiguration& SignalPathMap::getConfiguration(std::string name)
{
  String nameStr(name);
  nameStr = nameStr.toUpper();

  if(knownConfigurations_.find(nameStr.str()) != knownConfigurations_.end()) {

    return knownConfigurations_[nameStr.str()];

  } else {

    std::ostringstream os;
    
    os << std::endl << std::endl << "Unrecognized switchyard configuration: " << name << std::endl << std::endl;
    os << "Recognized configurations are: " << std::endl << std::endl;

    for(std::map<std::string, SwitchyardConfiguration>::iterator iter=knownConfigurations_.begin();
	iter != knownConfigurations_.end(); iter++) {
      os << "  " << iter->first << std::endl;
    }

    ThrowColorError(os.str(), "red");

    // We will never get here -- this is just to avoid compiler
    // warnings.

    return knownConfigurations_["INVALID"];
  }

}

/**.......................................................................
 * Validate a configuration by name
 */
void SignalPathMap::validateConfiguration(std::string name, unsigned baseIndex, CorrelatorType type, 
					  AstroBandInfo* info)
{
  SwitchyardConfiguration& conf = getConfiguration(name);
  validateConfiguration(conf, baseIndex, type, info);
}

/**.......................................................................
 * Validate a configuration
 */
void SignalPathMap::validateConfiguration(SwitchyardConfiguration& conf, unsigned baseIndex, CorrelatorType type, AstroBandInfo* info)
{
  for(unsigned i=0; i < conf.ifMappings_.size(); i++) {
    AntennaIFMapping& mp = conf.ifMappings_[i];

    mapAntennaIFToInput(mp.ifSpec_, mp.inputSpec_, false, baseIndex, type, info);
  }
}

AstroBandConfiguration& SignalPathMap::getAstroBandConfiguration(std::string name)
{
  String nameStr(name);
  nameStr = nameStr.toUpper();

  if(knownAstroBandConfigurations_.find(nameStr.str()) != knownAstroBandConfigurations_.end()) {

    return knownAstroBandConfigurations_[nameStr.str()];

  } else {

    std::ostringstream os;
    
    os << std::endl << std::endl << "Unrecognized astroband configuration: " << name << std::endl << std::endl;
    os << "Recognized configurations are: " << std::endl << std::endl;

    for(std::map<std::string, AstroBandConfiguration>::iterator iter=knownAstroBandConfigurations_.begin();
	iter != knownAstroBandConfigurations_.end(); iter++) {
      os << "  " << iter->first << std::endl;
    }

    ThrowColorError(os.str(), "red");

    // We will never get here -- it's just to avoid compiler warnings.

    return knownAstroBandConfigurations_["INVALID"];
  }

}

/**.......................................................................
 * Validate an FPGA configuration by name
 */
void SignalPathMap::validateAstroBandConfiguration(std::string name, unsigned baseIndex, CorrelatorType type, 
						   AstroBandInfo* info)
{
  AstroBandConfiguration& conf = getAstroBandConfiguration(name);
  validateAstroBandConfiguration(conf, baseIndex, type, info);
}

/**.......................................................................
 * Validate an FPGA configuration
 */
void SignalPathMap::validateAstroBandConfiguration(AstroBandConfiguration& conf, unsigned baseIndex, 
						   CorrelatorType type, AstroBandInfo* info)
{
  if(info)
    info->astroBandConf_ = &conf;

  for(unsigned i=0; i < conf.inputMappings_.size(); i++) {

    AstroBandInputMapping& mp = conf.inputMappings_[i];
    String inputStr(mp.inputSpec_);

    if(inputStr.contains("C3G")) {
      mapBandToAstroBand(mp.inputSpec_, mp.astroSpec_, false, baseIndex, type, info);
    } else if(inputStr.contains("COR")) {
      mapCorrToAstroBand(mp.inputSpec_, mp.astroSpec_, false, baseIndex, type, info);
    } else {
      ThrowColorError("Astroband mapping includes invalid string: " << inputStr, "red");
    }

  }
}

/**.......................................................................
 * Assert an AstroBand configuration
 */
void SignalPathMap::selectAstroBandConfiguration(std::string name, AstroBandInfo* info, CorrelatorType type)
{
  // Get a reference to the requested configuration

  AstroBandConfiguration& conf = getAstroBandConfiguration(name);

  // Check that the configuration is valid before asserting it

  validateAstroBandConfiguration(conf, 0, type, info);

  // Now assert it if everything checks out

  info->astroBandConf_ = &conf;

  if(AstroBandInfo::isOk(info)) {
    for(unsigned i=0; i < conf.inputMappings_.size(); i++) {
      AstroBandInputMapping& mp = conf.inputMappings_[i];
      String inputStr(mp.inputSpec_);

      if(inputStr.contains("C3G")) {
	mapBandToAstroBand(mp.inputSpec_, mp.astroSpec_, true, 0, type, info);
      } else if(inputStr.contains("COR")) {
	mapCorrToAstroBand(mp.inputSpec_, mp.astroSpec_, true, 0, type, info);
      } else {
	ThrowColorError("Astroband mapping includes invalid string: " << inputStr, "red");
      }

    }
  }
}

/**.......................................................................
 * Assert a configuration
 */
void SignalPathMap::selectConfiguration(std::string name, unsigned baseIndex, CorrelatorType type, 
					AstroBandInfo* info)
{
  // Get a reference to the requested configuration

  SwitchyardConfiguration& conf = getConfiguration(name);
  
  // Check that the configuration is valid before asserting it

  validateConfiguration(conf, baseIndex, type, info);

  // Now assert it if everything checks out

  for(unsigned i=0; i < conf.ifMappings_.size(); i++) {
    AntennaIFMapping& mp = conf.ifMappings_[i];
    mapAntennaIFToInput(mp.ifSpec_, mp.inputSpec_, true, baseIndex, type, info);
  }
}

/**.......................................................................
 * Get the walsh column for an antenna
 */
unsigned SignalPathMap::getWalshColumn(unsigned antNo)
{
  Antenna* ant = getAntenna(antNo);
  return ant->walshCol_.getWalshColNo();
}

/**.......................................................................
 * Query IF switch settings for the current configuration
 */
std::vector<SwitchSetting> SignalPathMap::getIFSwitchSettings(unsigned astroBandNo)
{
  std::vector<SwitchSetting> settings;
  AstroBandInfo info(astroBandNo);

  SwitchSetting swSet;

  for(unsigned i=0; i < switches_.size(); i++) {
    Switch* sw = switches_[i];
    
    if(sw->currentChannel_ && sw->belongsTo(&info)) {

      swSet.switchNo_ = sw->switchNo_;
      swSet.channel_  = sw->currentChannel_->channelId_;

      settings.push_back(swSet);
    }

  }
  
  return settings;
}

/**.......................................................................
 * Query block downconverter settings for the requested astroband
 */
std::vector<BlockDownconverterSetting> SignalPathMap::getBdcSettings(unsigned astroBandNo)
{
  std::vector<BlockDownconverterSetting> settings;
  AstroBandInfo info(astroBandNo);

  BlockDownconverterSetting bdcSet;

  for(unsigned iBdc=0; iBdc < bdcs_.size(); iBdc++) {
    BlockDownconverter* bdc = bdcs_[iBdc];

    for(unsigned iBand=0; iBand < bdc->bands_.size(); iBand++) {

      Band* band = bdc->bands_[iBand];

      if(band->requestorMatches(&info) && band->bdcInput_) {
	bdcSet.bdcNo_  = bdc->bdcNo_;
	bdcSet.bandNo_ = band->bandNo_;
        bdcSet.input_  = band->bdcInput_->type_;

	settings.push_back(bdcSet);
      }
    }
  }
 
  return settings;
}

//-----------------------------------------------------------------------
// Methods of Antenna class
//-----------------------------------------------------------------------

Antenna::Antenna(unsigned antNo, AntennaType type)
{
  initialize(antNo, type);
}

Antenna::Antenna(const Antenna& ant)
{
  *this = ant;
}

Antenna::Antenna(Antenna& ant)
{
  *this = ant;
}

void Antenna::operator=(const Antenna& ant)
{
  *this = (Antenna&)ant;
}

void Antenna::operator=(Antenna& ant)
{
  initialize(ant.antNo_, ant.type_);
}

void Antenna::initialize(unsigned antNo, AntennaType type)
{
  antNo_         = antNo;
  type_          = type;
  subarrayId_    = SA_NONE;

  astroBandMask_.resize(AstroBand::nBandMax_+1);
  astroBandMask_.setAllBitsLow();

  initializeIfMap(type);
}

void Antenna::initializeIfMap(AntennaType type) 
{
  switch (type) {
    
    // Only BIMA and OVRO antenna types have RIGHT polarizations
    
  case ANT_BIMA:
  case ANT_OVRO:

    ifMap_[SP_CHAN_NONE] = new AntennaIF(this, POL_RIGHT, SP_CHAN_NONE);

    // Deliberate fall-through: all have LEFT

  case ANT_SZA:    

    ifMap_[SP_CHAN_A]    = new AntennaIF(this, POL_LEFT, SP_CHAN_A);
    ifMap_[SP_CHAN_B]    = new AntennaIF(this, POL_LEFT, SP_CHAN_B);
    ifMap_[SP_CHAN_C]    = new AntennaIF(this, POL_LEFT, SP_CHAN_C);
    ifMap_[SP_CHAN_D]    = new AntennaIF(this, POL_LEFT, SP_CHAN_D);

    break;

  default:
    ThrowColorError("Unrecognized antenna type: " << type, "red");
  }
}

void Antenna::clearIfMap()
{
  std::map<SplitterChannelId, AntennaIF*>::iterator slot;

  for(slot = ifMap_.begin(); slot != ifMap_.end(); slot++) {

    if(slot->second->toNode_) {
      slot->second->toNode_->clearFrom((ConnectableNode*)this);
      slot->second->toNode_ = 0;
    }

  }
}

 Antenna::~Antenna()
{
  std::map<SplitterChannelId, AntennaIF*>::iterator slot;

  for(slot = ifMap_.begin(); slot != ifMap_.end(); slot++) {
    delete slot->second;
  }
}

bool Antenna::canBeConfiguredBy(AstroBandInfo* info)
{
  if(!info || info->subarrayId_ == SA_NONE) {
    return true;
  } else {
    return subarrayId_ == SA_NONE || subarrayId_ == info->subarrayId_;
  }
}

void Antenna::setOwnershipTo(AstroBandInfo* info)
{
  if(info) {
    if(info->astroBandNo_ > 0) {
      astroBandMask_.setBitHigh(info->astroBandNo_);
    }

    subarrayId_ = info->subarrayId_;
  }
}

/**.......................................................................
 * Remove the requestor from the set of 'owners' of this antenna
 */
void Antenna::removeFromOwnership(AstroBandInfo* info)
{
  if(info) {
    if(info->astroBandNo_ > 0) {
      astroBandMask_.setBitLow(info->astroBandNo_);
    }
  }
}

std::string Antenna::printDown()
{
  std::ostringstream os;
  XtermManip xtm;

  std::map<SplitterChannelId, AntennaIF*>::const_iterator ifMapIter = ifMap_.begin();

  for(; ifMapIter != ifMap_.end(); ifMapIter++) {
    AntennaIF* antIf = ifMapIter->second;

    if(antIf->isUsedByConfiguredAstroBand()) {
      os << xtm.fg("green") << xtm.textMode("bold");
    } else {
      os << xtm.fg("white") << xtm.textMode("normal");
    }

    os << "C" << setw(3) << std::left << antNo_ << "(W" << walshCol_ << ")(S" << subarrayId_ << ")" << ": " << antIf->printDown() << std::endl;
  }

  return os.str();
}

//-----------------------------------------------------------------------
// Methods of AntennaIF class
//-----------------------------------------------------------------------

AntennaIF::AntennaIF(Antenna* parent, PolarizationType polType, SplitterChannelId splitterChannel)
{
  antenna_         = parent;
  polType_         = polType;
  splitterChannel_ = splitterChannel;
}

 AntennaIF::~AntennaIF()
 {
 }

std::string AntennaIF::printUp()
{
  std::ostringstream os;

  os << "C" << std::left << setw(2) << antenna_->antNo_ << ": " 
     << polType_ << " SP CHAN: " << splitterChannel_;

  return os.str();
}


std::string AntennaIF::printDown()
{
  std::ostringstream os;

  os << polType_ << " SP CHAN: " << splitterChannel_;

  // If connected to a switch channel

  SwitchChannel* swChan = dynamic_cast<SwitchChannel*>(toNode_);

  if(swChan) {
    os << "--> " << swChan->printDown();
    return os.str();
  }

  // Else if connected to a digitizer

  Digitizer* dig = dynamic_cast<Digitizer*>(toNode_);

  if(dig) {
    os << "--> " << dig->printDown();
    return os.str();
  }

  // Else no connection

  os << "--> NONE";

  return os.str();
}

/**.......................................................................
 * Return true if this antenna IF is connected to a currently
 * configured astroband
 */
bool AntennaIF::isUsedByConfiguredAstroBand()
{
  // If connected to a switch channel

  SwitchChannel* swChan = dynamic_cast<SwitchChannel*>(toNode_);
  if(swChan && (swChan == swChan->switch_->currentChannel_)) {
    return swChan->switch_->isUsedByConfiguredAstroBand();
  }

  // If connected to a digitizer

  Digitizer* dig = dynamic_cast<Digitizer*>(toNode_);
  if(dig) {
    return dig->isUsedByConfiguredAstroBand();
  }

  return false;
}

void AntennaIF::validateAntenna()
{
  if(!antenna_) {
    ThrowColorError("No antenna is currently associated with this IF", "red");
  }
}

Antenna* AntennaIF::getAntenna()
{
  validateAntenna();
  return antenna_;
}

//-----------------------------------------------------------------------
// Methods of the AntennaIFMapping class
//-----------------------------------------------------------------------

AntennaIFMapping::AntennaIFMapping(std::string ifSpec, std::string inputSpec)
{
  ifSpec_    = ifSpec;
  inputSpec_ = inputSpec;
}

AntennaIFMapping::AntennaIFMapping(const AntennaIFMapping& mp)
{
  *this = mp;
}

AntennaIFMapping::AntennaIFMapping(AntennaIFMapping& mp)
{
  *this = mp;
}

void AntennaIFMapping::operator=(const AntennaIFMapping& mp)
{
  *this = (AntennaIFMapping&)mp;
}

void AntennaIFMapping::operator=(AntennaIFMapping& mp)
{
  ifSpec_    = mp.ifSpec_;
  inputSpec_ = mp.inputSpec_;
}

AntennaIFMapping::~AntennaIFMapping()
{
}

//-----------------------------------------------------------------------
// Methods of AstroBand class
//-----------------------------------------------------------------------

AstroBand::AstroBand(unsigned bandNo) 
{
  initialize(bandNo);
}

 AstroBand::AstroBand(const AstroBand& band)
{
  *this = band;
}

AstroBand::AstroBand(AstroBand& band)
{
  *this = band;
}

void AstroBand::operator=(const AstroBand& band)
{
  *this = (AstroBand&)band;
}

void AstroBand::operator=(AstroBand& band)
{
  initialize(band.bandNo_);
}

void AstroBand::initialize(unsigned bandNo)
{
  conf_         = 0;
  isConfigured_ = false;
  bandNo_       = bandNo;
  subarrayId_   = SA_NONE;

  if (bandNo >0 && bandNo < 9 ) {
      type_ = CORR_SL;
  } else if ( bandNo >= 9 && bandNo < 25 ) { 
      type_ = CORR_WB;
  } else if(bandNo >= 25 && bandNo <= 32) {
    type_ = CORR_C3GMAX23;
  } else if(bandNo >= 33 && bandNo <= 40) {
    type_ = CORR_C3GMAX8;
  } else {
    type_ = CORR_NONE;
  }

  initializeInputMap();
}

/**.......................................................................
 * Clear this astroband, and any configurable devices that were
 * configured by it
 */
void AstroBand::clear(CorrelatorType type, SubarrayId saId)
{
  AstroBandInfo info(bandNo_, saId);

  // Now clear any devices up the chain that may be connected to us

  for(unsigned i=0; i < inputs_.size(); i++) {

    AstroBandInput* input            = inputs_[i];

    if(input->isConnectedToCrate())
      input->clearCrateInput(type, info);
    
    if(input->isConnectedToBand()) {
      input->clearBandInput(type, info);
    }
  }

  // And mark this band as no longer configured

  isConfigured_ = false;
  subarrayId_   = SA_NONE;
  conf_         = 0;
}

void AstroBand::initializeInputMap()
{
  for(unsigned i=0; i < nInputMax_; i++) {
    inputs_.push_back(new AstroBandInput(this, i+1));
  }
}

AstroBand::~AstroBand()
{
  for(unsigned i=0; i < inputs_.size(); i++) {
    delete inputs_[i];
  }
}

AstroBandInput* AstroBand::getInput(unsigned inputNo)
{
  validateInput(inputNo);
  int inputIndex = (int)(inputNo) - 1;
  return inputs_[inputIndex];
}

void AstroBand::validateInput(unsigned inputNo)
{
  if(inputNo > inputs_.size()) {
    ThrowColorError("AstroBand: " << bandNo_ << " has no input: " << inputNo, "red");
  }
}

SwitchyardConfiguration* AstroBand::getSwitchyardConfiguration()
{
  validateSwitchyardConfiguration();
  return conf_;
}

void AstroBand::validateSwitchyardConfiguration()
{
  if(!conf_) {
    ThrowError("Astroband " << bandNo_ << " is not currently associated with any switchyard configuration");
  }
}

std::vector<unsigned> AstroBand::getAstroBandNos(CorrelatorType type)
{
  std::vector<unsigned> nos;


  if(type & CORR_SL) {
    for(unsigned i=1; i <= 8; i++)
      nos.push_back(i);
  }

  if(type & CORR_WB) {
    for(unsigned i=9; i <= 24; i++)
      nos.push_back(i);
  }
  
  if(type & (CORR_C3GMAX23)) {
    for(unsigned i=25; i <= 32; i++)
      nos.push_back(i);
  }

  if(type & CORR_C3GMAX8) {
    for(unsigned i=33; i <= 40; i++)
      nos.push_back(i);
  }
    
  return nos;
}

//-----------------------------------------------------------------------
// Methods of ConfigurableDevice class
//-----------------------------------------------------------------------

ConfigurableDevice::ConfigurableDevice()
{
  astroBandNo_ = 0;
}

ConfigurableDevice::~ConfigurableDevice()
{
}

bool ConfigurableDevice::canBeConfiguredBy(unsigned astroBandNo)
{
  AstroBandInfo info(astroBandNo);
  return canBeConfiguredBy(&info);
}

 bool ConfigurableDevice::canBeConfiguredBy(AstroBandInfo* info)
{
  return belongsTo(info) || isOwnedByNoone() || !info->checkCurrentlyAllowed_;
}

bool ConfigurableDevice::belongsTo(AstroBandInfo* info)
{
  return requestorMatches(info);
}

bool ConfigurableDevice::isOwnedByNoone()
{
  return astroBandNo_ == 0;
}

bool ConfigurableDevice::requestorMatches(AstroBandInfo* info)
{
  return requestorOwnsAll(info) || info->astroBandNo_ == astroBandNo_;
}

bool ConfigurableDevice::requestorOwnsAll(AstroBandInfo* info)
{
  return info == 0 || info->astroBandNo_ == 0;
}

void ConfigurableDevice::setOwnershipTo(AstroBandInfo* info)
{
  if(info) {
    astroBandNo_ = info->astroBandNo_;
  }
}

void ConfigurableDevice::removeFromOwnership(AstroBandInfo* info)
{
  if(info)
    removeFromOwnership(info->astroBandNo_);
}

void ConfigurableDevice::removeFromOwnership(unsigned astroBandNo)
{
  if(astroBandNo_ == astroBandNo) {
    astroBandNo_ = 0;
  }
}

void ConfigurableDevice::setOwnershipTo(unsigned astroBandNo)
{
  astroBandNo_ = astroBandNo;
}

unsigned ConfigurableDevice::getCurrentAstroBandNo()
{
  return astroBandNo_;
}

//-----------------------------------------------------------------------
// Methods of the ConfigurableDeviceMultiAstroBand class
//-----------------------------------------------------------------------

/**.......................................................................
 * Parse the ownership mask
 */
std::string ConfigurableDeviceMultiAstroBand::listOwners()
{
  std::ostringstream os;
  bool first=true;

  for(unsigned iBand=1; iBand <= AstroBand::nBandMax_; iBand++) {

    if(astroBandMask_.bitIsHigh(iBand))
    {

      if(first) {
        os << "AB" << iBand;
        first = false;
          } else {
        os << ", AB" << iBand;
      }
    }
      
  }

  return os.str();
}

bool ConfigurableDeviceMultiAstroBand::isSoleOwner(AstroBandInfo* info)
{
  if(!info) {
    return true;
  } else {
    return isSoleOwner(info->astroBandNo_);
  }
}

bool ConfigurableDeviceMultiAstroBand::isSoleOwner(unsigned astroBandNo)
{
  if(astroBandNo == 0) {
    return true;
  } else {
    return astroBandMask_.allBitsAreLowBut(astroBandNo);
  }
}

/**.......................................................................
 * Add the requestor to the set of 'owners' of this switch
 */
void ConfigurableDeviceMultiAstroBand::setOwnershipTo(AstroBandInfo* info)
{
  if(info && info->astroBandNo_ > 0)
    setOwnershipTo(info->astroBandNo_);
}

/**.......................................................................
 * Add the requestor to the set of 'owners' of this switch
 */
void ConfigurableDeviceMultiAstroBand::setOwnershipTo(unsigned astroBandNo)
{
  if(astroBandNo > 0) {
    astroBandMask_.setBitHigh(astroBandNo);
  }
}

/**.......................................................................
 * Remove the requestor from the set of 'owners' of this switch
 */
void ConfigurableDeviceMultiAstroBand::removeFromOwnership(AstroBandInfo* info)
{
  if(info && info->astroBandNo_ > 0)
    removeFromOwnership(info->astroBandNo_);
}

/**.......................................................................
 * Remove the requestor from the set of 'owners' of this switch
 */
void ConfigurableDeviceMultiAstroBand::removeFromOwnership(unsigned astroBandNo)
{
  if(astroBandNo > 0) {
    astroBandMask_.setBitLow(astroBandNo);
  }
}

/**.......................................................................
 * Return true if this astro band is one of the owners of this switch
 */
bool ConfigurableDeviceMultiAstroBand::requestorMatches(AstroBandInfo* info)
{
  return requestorOwnsAll(info) || isOwnedByNoone() || astroBandMask_.bitIsHigh(info->astroBandNo_);
}

/**.......................................................................
 * Return true if this astro band is one of the owners of this switch
 */
bool ConfigurableDeviceMultiAstroBand::requestorMatches(unsigned astroBandNo)
{
  return requestorOwnsAll(astroBandNo) || isOwnedByNoone() || astroBandMask_.bitIsHigh(astroBandNo);
}

/**.......................................................................
 * Return true if this switch is owned by nobody
 */
bool ConfigurableDeviceMultiAstroBand::isOwnedByNoone()
{
  return astroBandMask_.allBitsAreLow();
}

bool ConfigurableDeviceMultiAstroBand::requestorOwnsAll(unsigned astroBandNo)
{
  return astroBandNo == 0;
}

bool ConfigurableDeviceMultiAstroBand::requestorOwnsAll(AstroBandInfo* info)
{
  return info == 0 || astroBandMask_.allBitsAreLow();
}

/**.......................................................................
 * Return true if this device belongs to this astro band
 */
bool ConfigurableDeviceMultiAstroBand::belongsTo(AstroBandInfo* info)
{
  return requestorOwnsAll(info) || astroBandMask_.bitIsHigh(info->astroBandNo_);
}

/**.......................................................................
 * Return true if this device belongs to this astro band
 */
bool ConfigurableDeviceMultiAstroBand::belongsTo(unsigned astroBandNo)
{
  return requestorOwnsAll(astroBandNo) || astroBandMask_.bitIsHigh(astroBandNo);
}

bool ConfigurableDeviceMultiAstroBand::canBeConfiguredBy(AstroBandInfo* info)
{
  return (belongsTo(info) && isSoleOwner(info)) || isOwnedByNoone();
}

bool ConfigurableDeviceMultiAstroBand::canBeConfiguredBy(unsigned astroBandNo)
{
  return (belongsTo(astroBandNo) && isSoleOwner(astroBandNo)) || isOwnedByNoone();
}

//-----------------------------------------------------------------------
// Methods of AstroBandInfo class
//-----------------------------------------------------------------------
 
AstroBandInfo::AstroBandInfo(unsigned astroBandNo, SubarrayId saId)
{
  astroBandNo_ = astroBandNo;
  subarrayId_  = saId;
  checkCurrentlyAllowed_ = true;
  astroBandConf_ = 0;

  reset();
}

void AstroBandInfo::reset()
{
  nDevicesConfigured_   = 0;
  conflicted_           = false;

  conflictMask_.resize(AstroBand::nBandMax_+1);
  conflictMask_.setAllBitsLow();

  antConflictMask_      = 0x0;
  conflictedSubarrayId_ = SA_NONE;
}

/**.......................................................................
 * Increment the number of devices actually configured for this band
 */
void AstroBandInfo::incrementConfiguredDevices(AstroBandInfo* info)
{
  if(info) {
    ++info->nDevicesConfigured_;
  }
}

/**.......................................................................
 * Register a conflict with an antenna
 */
void AstroBandInfo::registerConflict(AstroBandInfo* info, Antenna* ant)
{
  if(info) {
    info->conflicted_ = true;
    info->antConflictMask_ |= (1<<ant->antNo_);
    info->conflictedSubarrayId_ = ant->subarrayId_;
  }
}

/**.......................................................................
 * Register a conflict with a switch setting (an astro band
 * configuration trying to change a switch setting needed for another
 * astro band)
 */
void AstroBandInfo::registerConflict(AstroBandInfo* info, Switch* sw)
{
  if(info) {
    info->conflicted_ = true;
    info->conflictMask_ |= sw->astroBandMask_;
  }
}

/**.......................................................................
 * Register a conflict with a digitizer setting (an astro band
 * configuration trying to change a digitizer routing needed for another
 * astro band)
 */
void AstroBandInfo::registerConflict(AstroBandInfo* info, Digitizer* dig)
{
  if(info) {
    info->conflicted_ = true;
    info->conflictMask_ |= dig->astroBandMask_;
  }
}

void AstroBandInfo::registerConflict(AstroBandInfo* info, FpgaBoard* part)
{
  if(info) {
    info->conflicted_ = true;
    info->conflictMask_ |= part->astroBandMask_;
  }
}

/**.......................................................................
 * Register a conflict with correlatorCrateInput ownership (an astro
 * band trying to map a correlator input that belongs to another astro band)
 */
void AstroBandInfo::registerConflict(AstroBandInfo* info, CorrelatorCrateInput* input)
{
  if(info) {
    info->conflicted_ = true;

    if(input->isConnectedToAstroBand()) {
      info->conflictMask_.setBitHigh(input->getAstroBandInput()->band_->bandNo_);
    }
  }
}

/**.......................................................................
 * Register a conflict with correlatorBandInput ownership (an astro
 * band trying to map a correlator band input that belongs to another astro
 * band)
 */
void AstroBandInfo::registerConflict(AstroBandInfo* info, CorrelatorBandInput* input)
{
  if(info) {
    info->conflicted_ = true;

    if(input->isConnectedToAstroBand()) {
      info->conflictMask_.setBitHigh(input->getAstroBandInput()->band_->bandNo_);
    }
  }
}

/**.......................................................................
 * Return true if the requested configuration for this band has led to
 * conflicts
 */
bool AstroBandInfo::isConflicted(AstroBandInfo* info)
{
  if(info) {
    return info->conflicted_;
  } else {
    return false;
  }
}

/**.......................................................................
 * Return true if the requested configuration for this band has led to
 * no conflicts, and has resulted in a non-zero number of devices
 * actually being configured
 */
bool AstroBandInfo::isOk(AstroBandInfo* info)
{
  if(info) {
    return !(info->conflicted_ || info->nDevicesConfigured_==0);
  } else {
    return true;
  }
}

/**.......................................................................
 * Parse the conflict mask into antennas
 */
 std::string AstroBandInfo::listConflictedAntennas(AstroBandInfo* info)
{
  std::ostringstream os;
  bool first=true;

  if(info) {
    for(unsigned iAnt=1; iAnt <= SignalPathMap::nAnt_; iAnt++) {

      if(info->antConflictMask_ & (1<<iAnt)) {

	if(first) {
	  os << "C" << iAnt;
	  first = false;
	} else {
	  os << ", C" << iAnt;
	}
      }
      
    }
  }

  return os.str();
}

/**.......................................................................
 * Parse the conflict mask into bands
 */
std::string AstroBandInfo::listConflictedBands(AstroBandInfo* info)
{
  std::ostringstream os;
  bool first=true;

  if(info) {
    for(unsigned iBand=1; iBand <= AstroBand::nBandMax_; iBand++) {

      if(info->conflictMask_.bitIsHigh(iBand) && iBand != info->astroBandNo_) {

        if(first) {
          os << "AB" << iBand;
          first = false;
        } else {
          os << ", AB" << iBand;
        }
      }
      
    }
  }

  return os.str();
}

std::ostream& carma::signalpath::operator<<(std::ostream& os, const AstroBandInfo& info)
{
  os << "nDevicesConfigured_   = " << info.nDevicesConfigured_  << std::endl;
  os << "conflicted_           = " << info.conflicted_ << std::endl;
  //os << "conflictMask_         = " << info.conflictMask_ << std::endl;
  os << "antConflictMask_      = " << info.antConflictMask_ << std::endl;
  os << "conflictedSubarrayId_ = " << info.conflictedSubarrayId_ << std::endl;

  return os;
}  

//-----------------------------------------------------------------------
// Methods of AstroBandInput class
//-----------------------------------------------------------------------

AstroBandInput::AstroBandInput(AstroBand* parent, unsigned inputNo) 
{
  band_         = parent;
  inputNo_      = inputNo;
  isConfigured_ = 0;
}

AstroBandInput::~AstroBandInput()
{
}

void AstroBandInput::clearCrateInput(CorrelatorType type, AstroBandInfo& info)
{
  CorrelatorCrateInput* crateInput = 0;
    
  try {
    crateInput = getCrateInput();
  } catch(...) {
    
    // It is not an error for an astroband input not to be connected
    // to any correlator crate input
    
    return;
  }

  // Throw an error if this input is connected to a correlator we are not allowed to modify

  if(type & crateInput->crate_->type_)
  {
      
    // Get the block downconverter band and switch this input is
    // connected to, and mark them as no longer owned by us
      
    try {
      
      // Mark the band as no longer owned by us
      
      Band* band = getCrateInput()->getBdcBand();
      band->removeFromOwnership(&info);
      
      // Mark the switch as no longer owned by us
      
      Switch* sw = band->getBdcInput()->getSwitch();
      sw->removeFromOwnership(&info);
      
      // Also mark the antenna as no longer owned by us
      
      Antenna* ant = sw->getCurrentChannel()->getAntennaIF()->getAntenna();
      ant->removeFromOwnership(&info);
      
    } catch(...) {
      
      // It's possible that an astroband input is mapped to a crate
      // input that is not currently connected to anything -- just
      // continue in this case
      
    }
    
    crateInput->clearTo(this, type);
    clearFrom(crateInput, type);
    
  } else {
    
    ThrowColorError(std::endl << "You are attempting to modify the input to a crate "
		    << "(" << crateInput->crate_->name_ << ") that you are not allowed to modify (controlling subarray owns " << type << ")" << std::endl, "red");
    
  }
}

void AstroBandInput::clearBandInput(CorrelatorType type, AstroBandInfo& info)
{
  CorrelatorBandInput* bandInput = 0;
    
  try {
    bandInput = getBandInput();
  } catch(...) {
    
    // It is not an error for an astroband input not to be connected
    // to any correlator band input
    
    return;
  }

  // Throw an error if this input is connected to a correlator we are not allowed to modify

  if(type & bandInput->band_->type_)
  {
    bandInput->clearTo(this, type);
    clearFrom(bandInput, type);
  } else {
      
    ThrowColorError(std::endl << "You are attempting to modify the input to a band "
		    << "(" << bandInput->band_->name_ << ") that you are not allowed to modify (controlling subarray owns " << type << ")" << std::endl, "red");
    
  }
}

/**.......................................................................
 * Clear any mapping to this astroband input
 */
void AstroBandInput::clearFrom(ConnectableNode* node, CorrelatorType type)
{
  // Just unmap anyone who is currently mapped to us

  if(fromNode_)
    fromNode_->clearTo(this, type);

  mapFrom((ConnectableNode*) 0, type);
  isConfigured_ = false;
}

/**.......................................................................
 * Mapping this astroband input from another connectable node
 */
void AstroBandInput::mapFrom(ConnectableNode* node, CorrelatorType type)
{
  // If we are already mapped from someone, clear that node's mapping
  // first

  if(fromNode_ != 0 && fromNode_ != node)
    fromNode_->clearTo(this, type);
  
  // Now remap to the current input
  
  fromNode_ = node;
}

bool AstroBandInput::isUsedByConfiguredAstroBand()
{
  return isConfigured_ && band_->isConfigured_;
}

/**.......................................................................
 * Methods to access any CorrelatorCrateInput we may be connected from
 */
CorrelatorCrateInput* AstroBandInput::getCrateInput()
{
  validateCrateInput();
  return dynamic_cast<CorrelatorCrateInput*>(fromNode_);
}

void AstroBandInput::validateCrateInput()
{
  CorrelatorCrateInput* input = dynamic_cast<CorrelatorCrateInput*>(fromNode_);

  if(!input) {
    ThrowColorError("This astro band input is not currently conected to any crate input", "red");
  }
}

bool AstroBandInput::isConnectedToCrate()
{
  try {
    validateCrateInput();
    return true;
  } catch(...) {
    return false;
  }
}

AntennaIF* AstroBandInput::getAntennaIF()
{
  AntennaIF* antIf = 0;

  if(isConnectedToCrate()) {
    antIf = getCrateInput()->getBdcBand()->getBdcInput()->getSwitch()->
      getCurrentChannel()->getAntennaIF();
    return antIf;
  } else if(isConnectedToBand()) {
    antIf = getBandInput()->getAntennaIF();
    return antIf;
  }

  ThrowColorError("This astro band input is not currently associated with any antenna IF", "red");
  return antIf;
}


/**.......................................................................
 * Methods to access any CorrelatorBandInput we may be connected from
 */
CorrelatorBandInput* AstroBandInput::getBandInput()
{
  validateBandInput();
  return dynamic_cast<CorrelatorBandInput*>(fromNode_);
}

void AstroBandInput::validateBandInput()
{
  CorrelatorBandInput* input = dynamic_cast<CorrelatorBandInput*>(fromNode_);

  if(!input) {
    ThrowColorError("This astro band input is not currently conected to any band input", "red");
  }
}

bool AstroBandInput::isConnectedToBand()
{
  try {
    validateBandInput();
    return true;
  } catch(...) {
    return false;
  }
}

//-----------------------------------------------------------------------
// Methods of the Band class
//-----------------------------------------------------------------------

Band::Band(BlockDownconverter* parent, unsigned bandNo)
{
  bdc_        = parent;
  bdcInput_   = 0;
  bandNo_     = bandNo;
  corrInput_  = 0;
}

bool Band::isUsedByConfiguredAstroBand()
{
  try {
    CorrelatorCrateInput* input = getCorrelatorCrateInput();
    return input->isUsedByConfiguredAstroBand();
  } catch(...) {
    return false;
  }
}

void Band::clear()
{
  // Remove ourselves from any input to which we are currently mapped

  if(bdcInput_) {
    bdcInput_->removeBand(this);
  }

  bdcInput_ = 0;
}

std::string Band::printUp()
{
  std::ostringstream os;

  os << "BD " << std::left << setw(2) << bdc_->bdcNo_ << ": " << setw(2) << bandNo_;
  
  if(bdcInput_) {
    os << " --> " << bdcInput_->printUp();
  } else {
    os << " --> NO INPUT";
  }

  return os.str();
}

/**.......................................................................
 * Map a new block downconverter input to this band
 */
void Band::mapBdcInput(BlockDownconverterInput* input)
{
  // First clear any hardware mapping associated with this band

  clear();

  // Now assign the new input as our input mapping

  bdcInput_ = input;

  // And add ourselves to the list of bands mapped to that input

  bdcInput_->addBand(this);
}

bool Band::isSelected()
{
  if(bdcInput_) {
    if(bdcInput_->switch_) {
      if(bdcInput_->switch_->currentChannel_) {
	return true;
      }
    }
  }

  return false;
}

 void Band::validateBdc()
{
  if(!bdc_) {
    ThrowColorError("No block downconverter is currently associated with this band", "red");
  }
}

BlockDownconverter* Band::getBdc()
{
  validateBdc();
  return bdc_;
}

void Band::validateBdcInput()
{
  if(!bdcInput_) {
    ThrowColorError("No block downconverter input is currently associated with BD" << getBdc()->bdcNo_ << ":" << bandNo_, "red");
  }
}

BlockDownconverterInput* Band::getBdcInput()
{
  validateBdcInput();
  return bdcInput_;
}

void Band::validateCorrelatorCrateInput()
{
  if(!corrInput_) {
    ThrowColorError("No correlator crate input is currently associated with BD" << getBdc()->bdcNo_ << ":" << bandNo_, "red");
  }
}

CorrelatorCrateInput* Band::getCorrelatorCrateInput()
{
  validateCorrelatorCrateInput();
  return corrInput_;
}

//-----------------------------------------------------------------------
// Methods of BlockDownconverter class
//-----------------------------------------------------------------------

BlockDownconverter::BlockDownconverter(unsigned bdcIndex, CorrelatorType type)
{
  initialize(bdcIndex, type);
}

BlockDownconverter::BlockDownconverter(const BlockDownconverter& bdc)
{
  *this = bdc;
}

BlockDownconverter::BlockDownconverter(BlockDownconverter& bdc)
{
  *this = bdc;
}

void BlockDownconverter::operator=(const BlockDownconverter& bdc)
{
  *this = (BlockDownconverter&)bdc;
}

void BlockDownconverter::operator=(BlockDownconverter& bdc)
{
  initialize(bdc.bdcNo_, bdc.type_);
}

void BlockDownconverter::initialize(unsigned bdcNo, CorrelatorType type)
{
  bdcNo_ = bdcNo;
  type_  = type;

  initializeInputMap(type_);
  initializeBands(type_);
}

void BlockDownconverter::initializeInputMap(CorrelatorType type) 
{
  // Initialize inputs for this downconverter

  // All downconverters have P1 inputs

  inputMap_[BD_INP_P1] = new BlockDownconverterInput(this, BD_INP_P1);

  // Only the spectral line downconverters have a P2 input

  if(type == CORR_SL) {
    inputMap_[BD_INP_P2] = new BlockDownconverterInput(this, BD_INP_P2);
  }
}

 void BlockDownconverter::initializeBands(CorrelatorType type) 
{
  // Now initialize the inputs for each band

  switch (type) {
  case CORR_SL:
    for(unsigned i=0; i < nBandSl_; i++) {
      bands_.push_back(new Band(this, i+1));
    }
    break;
  case CORR_WB:
    for(unsigned i=0; i < nBandWb_; i++) {
      bands_.push_back(new Band(this, i+1));
    }
    break;
  default:
    ThrowColorError("Invalid downconverter type: " << type, "red");
    break;
  }
}

/**.......................................................................
 * Clear any hardware mappings managed by this class
 */
void BlockDownconverter::clear()
{
  std::map<BlockDownconverterInputType, BlockDownconverterInput*>::iterator slot;

  for(slot = inputMap_.begin(); slot != inputMap_.end(); slot++) {
    slot->second->clear();
  }

  for(unsigned i=0; i < bands_.size(); i++) {
    bands_[i]->clear();
  }
}

BlockDownconverter::~BlockDownconverter()
{
  std::map<BlockDownconverterInputType, BlockDownconverterInput*>::iterator slot;

  for(slot = inputMap_.begin(); slot != inputMap_.end(); slot++) {
    delete slot->second;
  }

  for(unsigned i=0; i < bands_.size(); i++) {
    delete bands_[i];
  }

  bands_.resize(0);
}

Band* BlockDownconverter::getBand(unsigned bandNo)
{
  validateBand(bandNo);
  return bands_[bandNo-1];
}

void BlockDownconverter::validateBand(unsigned bandNo)
{
  if(bandNo > bands_.size()) {
    ThrowColorError("Invalid band number: " << bandNo << " for BD" << bdcNo_, "red");
  }
}

BlockDownconverterInput* BlockDownconverter::getInput(BlockDownconverterInputType type)
{
  validateInput(type);
  return inputMap_[type];
}

void BlockDownconverter::validateInput(BlockDownconverterInputType type)
{
  if(inputMap_.find(type) == inputMap_.end()) {
    ThrowColorError("Input: " << type << " is invalid for BD " << bdcNo_, "red");
  }
}

 unsigned BlockDownconverter::nBand()
{
  return nBand(type_);
}

unsigned BlockDownconverter::nBand(CorrelatorType type)
{
  switch (type) {
  case CORR_SL:
    return 8;
    break;
  case CORR_WB:
    return 16;
    break;
  default:
    ThrowColorError("Invalid downconverter type: " << type, "red");
    return 0;
    break;
  }
}

/**.......................................................................
 * Print a block downconverter
 */
std::string BlockDownconverter::printUp()
{
  std::ostringstream os;
  XtermManip xtm;
  std::vector<Band*> p1;
  std::vector<Band*> p2;
  std::vector<Band*> none;

  for(unsigned iBand=0; iBand < bands_.size(); iBand++) {
    Band* band = bands_[iBand];

    if(band->bdcInput_) {
      switch (band->bdcInput_->type_) {
      case BD_INP_P1:
	p1.push_back(band);
	break;
      case BD_INP_P2:
	p2.push_back(band);
	break;
      default:
	ThrowColorError("Unrecognized input: " << band->bdcInput_->type_, "red");
	break;
      }
    } else {
      none.push_back(band);
    }
  }

  os << printBandVec(p1);
  os << printBandVec(p2);
  os << printBandVec(none);

  return os.str();
}

std::string BlockDownconverter::printBandVec(std::vector<Band*>& bands)
{
  XtermManip xtm;
  std::ostringstream os;
  os.str("");

  if(bands.size() == 0) {
    return os.str();
  }

  if(bands[0]->isSelected()) {
    os << xtm.fg("green") << xtm.textMode("bold");
  } else {
    os << xtm.fg("white") << xtm.textMode("normal");
  }

  os << "BD " << std::left << setw(2) << bdcNo_ << " [";

  for(unsigned i=0; i < bands.size(); i++) {
    os << bands[i]->bandNo_;
    if(i < bands.size()-1) {
      os << ",";
    }
  }

  os << "] ";

  if(bands[0]->isSelected()) {
    os << xtm.fg("green") << xtm.textMode("bold");
    os << " --> " << bands[0]->bdcInput_->printUp() << std::endl;
  } else {
    os << xtm.fg("white") << xtm.textMode("normal");
    os << " --> NO INPUT";
  }

  return os.str();
}

//-----------------------------------------------------------------------
// Methods of BlockDownconverterInput class
//-----------------------------------------------------------------------

BlockDownconverterInput::BlockDownconverterInput(BlockDownconverter* parent, BlockDownconverterInputType type)
{
  bdc_    = parent;
  type_   = type;

  clear();
}

bool BlockDownconverterInput::isUsedByConfiguredAstroBand()
{
  bool selected = false;

  for(unsigned iBand=0; iBand < bands_.size(); iBand++) {
    selected |= bands_[iBand]->isUsedByConfiguredAstroBand();
  }

  return selected;
}

void BlockDownconverterInput::clear()
{
  switch_ = 0;
  bands_.resize(0);
}

void BlockDownconverterInput::removeBand(Band* band)
{
  for(unsigned i=0; i < bands_.size(); i++) {
    if(bands_[i] == band) {
      bands_.erase(bands_.begin()+i);
      break;
    }
  }
}

void BlockDownconverterInput::addBand(Band* band)
{
  // Make sure the band isn't already in our list

  removeBand(band);

  // And add it

  bands_.push_back(band);
}

void BlockDownconverterInput::validateBdc()
{
  if(!bdc_) {
    ThrowColorError("No block downconverter is currently associated with this band", "red");
  }
}

BlockDownconverter* BlockDownconverterInput::getBdc()
{
  validateBdc();
  return bdc_;
}

void BlockDownconverterInput::validateSwitch()
{
  if(!switch_) {
    ThrowColorError("No switch is currently associated with BD" << getBdc()->bdcNo_ << ":" << type_, "red");
  }
}

Switch* BlockDownconverterInput::getSwitch()
{
  validateSwitch();
  return switch_;
}

std::string BlockDownconverterInput::printDown(std::ostringstream& ios, bool printBands)
{
  std::ostringstream os;
  bool first = true;

  os << " BD " << std::left << setw(2) << bdc_->bdcNo_ << ": " << type_;
  os << "[";

  if(printBands) {

    first = true;
    for(unsigned i=0; i < bands_.size(); i++) {
      Band* band = bands_[i];

      if(band->isUsedByConfiguredAstroBand()) {
	
	if(first) {
	  os << band->bandNo_;
	  first = false;
	} else {
	  os << "," << band->bandNo_;
	}
      }
    }

  }

  os << "]";

  // Print correlator crate inputs too

  os << "--> [";

  if(printBands) {

    first = true;
    for(unsigned i=0; i < bands_.size(); i++) {
      Band* band = bands_[i];

      if(band->isUsedByConfiguredAstroBand()) {

	if(band->corrInput_) {
	  CorrelatorCrateInput* inp = band->corrInput_;
	
	  if(first) {
	    os << inp->crate_->name_ << ":" << inp->inputNo_;
	    first = false;
	  } else {
	    os << "," << inp->crate_->name_ << ":" << inp->inputNo_;
	  }
	}
      }
    }
    
  }

  os << "]";

  // Print astro band inputs too

  os << "--> [";

  if(printBands) {

    first = true;
    for(unsigned i=0; i < bands_.size(); i++) {
      Band* band = bands_[i];

      if(band->isUsedByConfiguredAstroBand()) {

	if(band->corrInput_) {
	  CorrelatorCrateInput* crateInput = band->corrInput_;

	  if(crateInput) {
	    if(crateInput->isConnectedToAstroBand()) {
	      AstroBandInput* astroBandInput = crateInput->getAstroBandInput();

	      if(first) {
		os << "AB" << astroBandInput->band_->bandNo_ << ":" << astroBandInput->inputNo_;
		first = false;
	      } else {
		os << ",AB" << astroBandInput->band_->bandNo_ << ":" << astroBandInput->inputNo_;
	      }
	    }
	  }
	}
      }
    }

  }

  os << "]";

  return os.str();

}

/**.......................................................................
 * Print a block downconverter input
 */
std::string BlockDownconverterInput::printUp()
{
  std::ostringstream os;

  os << type_;

  if(switch_) {

    os << " --> " << "SW " << std::left << setw(2) << switch_->switchNo_
       << " (" << setw(5) << std::left << switch_->name_ << ")" << " CHAN ";

    if(switch_->currentChannel_) {
      os << switch_->currentChannel_->printUp();
    }

  } 

  return os.str();
}

//-----------------------------------------------------------------------
// Methods of CorrelatorBand class
//-----------------------------------------------------------------------

CorrelatorBand::CorrelatorBand(CorrelatorCrate* crate, unsigned bandNo)
{
  crate_  = crate;
  type_   = crate->type_;
  bandNo_ = bandNo;

  std::ostringstream os;

  if(crate->type_ == CORR_SL) {
    os << "SLCOR" << bandNo;
  } else if(crate->type_ == CORR_WB) {
    os << "WBCOR" << bandNo;
  } else {
    os << "C3G" << bandNo;
  }

  name_ = os.str();

  initializeInputMap();
}

CorrelatorBand::CorrelatorBand(CorrelatorType type, unsigned bandNo)
{
  crate_  = 0;
  type_   = type;
  bandNo_ = bandNo;

  std::ostringstream os;

  if(type_ == CORR_SL) {
    os << "SLCOR" << bandNo;
  } else if(type_ == CORR_WB) {
    os << "WBCOR" << bandNo;
  } else {
    os << "C3G" << bandNo;
  }

  name_ = os.str();

  initializeInputMap();
}

void CorrelatorBand::initializeInputMap()
{
  // Spectral-line bands have a 1-1 mapping to correlator crate inputs

  if(type_ == CORR_SL) {
    for(unsigned i=0; i < nInputSl_; i++) {
      unsigned inputNo = i+1;
      CorrelatorBandInput* input = new CorrelatorBandInput(this, inputNo);
      input->mapTo(crate_->getInput(inputNo));
      inputs_.push_back(input);
    }

  // Wideband bands map to the lower half of correlator crate inputs
  // for odd-numbered bands, and the upper half for even numbered
  // bands

  } else if(type_ == CORR_WB) {
    for(unsigned i=0; i < nInputWb_; i++) {
      unsigned inputNo = i+1;
      CorrelatorBandInput* input = new CorrelatorBandInput(this, inputNo);
 
      if(bandNo_ % 2 != 0) {
	input->mapTo(crate_->getInput(inputNo));
      } else {
	input->mapTo(crate_->getInput(inputNo + nInputWb_));
      }

      inputs_.push_back(input);
    }

    // Else create nInputC3g_ inputs for the C3G bands, and don't map them to anything

  } else {
    for(unsigned i=0; i < nInputC3g_; i++) {
      unsigned inputNo = i+1;
      CorrelatorBandInput* input = new CorrelatorBandInput(this, inputNo);
      inputs_.push_back(input);
    }
  }
}

CorrelatorBand::~CorrelatorBand()
{
  for(unsigned i=0; i < inputs_.size(); i++) {
    delete inputs_[i];
  }
}

CorrelatorBandInput* CorrelatorBand::getInput(unsigned inputNo)
{
  validateInput(inputNo);
  int inputIndex = (int)(inputNo) - 1;
  return inputs_[inputIndex];
}

void CorrelatorBand::validateInput(unsigned inputNo)
{
  if(inputNo > inputs_.size()) {
    ThrowColorError("CorrelatorBand: " << bandNo_ << " has no input: " << inputNo, "red");
  }
}

//-----------------------------------------------------------------------
// Methods of CorrelatorBandInput class
//-----------------------------------------------------------------------

CorrelatorBandInput::CorrelatorBandInput(CorrelatorBand* parent, unsigned inputNo) 
{
  band_       = parent;
  inputNo_    = inputNo;
  crateInput_ = 0;

  // If this is a band of the WB correlator, the Walsh colums are
  // hardwired

  if(parent->type_ == CORR_WB) {
    walshCol_.setWalshColNo(inputNo);
    walshCol_.setHardwired(true);
  }

}

CorrelatorBandInput::~CorrelatorBandInput()
{
  
}

AntennaIF* CorrelatorBandInput::getAntennaIF()
{
  AntennaIF* antIf = 0;

  if(isConnectedToCrate()) {
    antIf = getCrateInput()->getBdcBand()->getBdcInput()->getSwitch()->getCurrentChannel()->getAntennaIF();
    return antIf;
  } else {
    ConnectableNode* node = fromNode_;
    for(; node != 0; node = node->fromNode_) {
      antIf = dynamic_cast<AntennaIF*>(node);
      if(antIf != 0) {
	return antIf;
      }
    }
  }

  ThrowColorError("Band input " << band_->name_ << ":" << inputNo_ << " is not currently associated with any antenna IF", "red");
  return 0;
}

/**.......................................................................
 * Return the astro band input to which this crate input is assigned
 */
AstroBandInput* CorrelatorBandInput::getAstroBandInput()
{
  validateAstroBandInput();
  return dynamic_cast<AstroBandInput*>(toNode_);
}

void CorrelatorBandInput::validateAstroBandInput()
{
  if(toNode_ == 0) {
    ThrowColorError("No astro band input is currently assigned to correlator input: " 
		    << band_->name_ << ":" << inputNo_, "red");
  }
}

bool CorrelatorBandInput::isConnectedToAstroBand()
{
  try {
    validateAstroBandInput();
    return true;
  } catch(...) {
    return false;
  }
}

bool CorrelatorBandInput::canBeConfiguredBy(AstroBandInfo* info)
{
  // Return false if this band is already in use by another configured
  // astroband

  if(!ConfigurableDevice::canBeConfiguredBy(info))
    return false;

  // Return false if this band is connected to an FPGA board that has
  // a different configuration

  if(band_->fromNode_) {
    FpgaBoard* brd = dynamic_cast<FpgaBoard*>(band_->fromNode_);
    if(!brd->canBeConfiguredBy(info))
      return false;
  }

  return true;
}

void CorrelatorBandInput::setOwnershipTo(AstroBandInfo* info)
{
  // If this band input is connected to an FPGA board, set its ownership too

  if(fromNode_) {
    FpgaBoard* brd = dynamic_cast<FpgaBoard*>(fromNode_);
    brd->setOwnershipTo(info);
  }

  ConfigurableDevice::setOwnershipTo(info);
}

void CorrelatorBandInput::removeFromOwnership(unsigned astroBandNo)
{
  // If this band is connected to an FPGA board, set its ownership too

  if(fromNode_) {
    FpgaBoard* brd = dynamic_cast<FpgaBoard*>(fromNode_);
    brd->removeFromOwnership(astroBandNo);
  }

  ConfigurableDevice::removeFromOwnership(astroBandNo);
}

/**.......................................................................
 * Define what it means to clear our to node
 */
void CorrelatorBandInput::clearTo(ConnectableNode* node, CorrelatorType type)
{
  AstroBandInput* input = dynamic_cast<AstroBandInput*>(node);
    
  if(input == 0)
    ThrowError("CorrelatorBandInputs can only unmap from AstroBandInputs");

  if(input && (band_->type_ & type)) {
    removeFromOwnership(input->band_->bandNo_);
    toNode_ = 0;
  }
}

/**.......................................................................
 * Define what it means for an CorrelatorBandInput to map to an
 * AstroBandInput
 */
void CorrelatorBandInput::mapTo(ConnectableNode* node, CorrelatorType type)
{
  AstroBandInput* input = dynamic_cast<AstroBandInput*>(node);
    
  if(input == 0)
    ThrowError("CorrelatorBandInputs can only map to AstroBandInputs");

  if(toNode_ && toNode_ != node)
    toNode_->clearFrom(this, type);

  // Now remap to the current input
  
  toNode_ = node;
}

void CorrelatorBandInput::mapTo(CorrelatorCrateInput* input)
{
  crateInput_ = input;
  crateInput_->mapTo(this);
}

 CorrelatorCrateInput*  CorrelatorBandInput::getCrateInput()
 {
   validateCrateInput();
   return crateInput_;
 }

 void  CorrelatorBandInput::validateCrateInput()
 {
   if(crateInput_ == 0) {
     ThrowColorError("Band input: " << band_->name_ << ":" << inputNo_ 
		     << " has not been assigned to any crate input", "red");
   }
 }

bool CorrelatorBandInput::isConnectedToCrate()
{
  try {
    validateCrateInput();
    return true;
  } catch(...) {
    return false;
  }
}

//-----------------------------------------------------------------------
// Methods of Correlator class
//-----------------------------------------------------------------------

Correlator::Correlator(CorrelatorType type, SubarrayId subarrayId)
{
  type_       = type;
  subarrayId_ = subarrayId;
}

Correlator::~Correlator()
{
}

void Correlator::requestToControl(SubarrayId subarrayId)
{
  // First check for ownership of mutually exclusive correlators

  if(exclusiveCorrelators_.size() > 0) {
    for(unsigned iCorr=0; iCorr < exclusiveCorrelators_.size(); iCorr++) {
      Correlator* corr = exclusiveCorrelators_[iCorr];
      if(corr->subarrayId_ == SA_NONE || corr->subarrayId_ == subarrayId) {
        corr->requestToRelease(subarrayId);
      } else {
        ThrowColorError("You cannot control correlator: " << type_ 
                << " because a conflicting correlator (" << corr->type_ << ") is already owned by subarray: " << corr->subarrayId_, "red");
      }
    }
  }

  // Now check for our own ownership

  if(subarrayId_ == SA_NONE || subarrayId_ == subarrayId) {
    subarrayId_ = subarrayId;
  } else {
    ThrowColorError("You cannot control correlator: " << type_ 
                << " because it is already owned by subarray: " << subarrayId_, "red");
  }
}

 bool Correlator::requestToRelease(SubarrayId subarrayId)
{
  if(subarrayId_ == SA_NONE) {
    return false;
  } else if(subarrayId_ == subarrayId) {
    subarrayId_ = SA_NONE;
    return true;
  } else {
    // Not owned by this subarray?  That's ok -- just ignore the request
    return false;
  }
}

//-----------------------------------------------------------------------
// Methods of CorrelatorCrate class
//-----------------------------------------------------------------------

const unsigned CorrelatorCrate::nCrateSl_        =  8;
const unsigned CorrelatorCrate::nCrateWb_        =  8;
const unsigned CorrelatorCrate::nInputPerBandSl_ = 16; // Each SL crate handles one 'band'
const unsigned CorrelatorCrate::nInputPerBandWb_ =  8; // Each WB crate handles two 'bands'

CorrelatorCrate::CorrelatorCrate(CorrelatorType type, unsigned crateNo)
{
  std::ostringstream os;
  os << (type == CORR_SL ? "SLCOR" : "WBCOR") << crateNo;

  name_    = os.str();
  type_    = type;
  crateNo_ = crateNo;

  // All crates have the same number of inputs

  inputs_.resize(nInput_);

  for(unsigned i=0; i < inputs_.size(); i++) {
    inputs_[i] = new CorrelatorCrateInput(this, i+1);
  }
}

CorrelatorCrate::~CorrelatorCrate()
{
  for(unsigned i=0; i < inputs_.size(); i++) {
    delete inputs_[i];
  }
}

CorrelatorCrateInput* CorrelatorCrate::getInput(unsigned inputNo)
{
  validateInput(inputNo);
  return inputs_[inputNo-1];
}

void CorrelatorCrate::validateInput(unsigned inputNo)
{
  if(inputNo > inputs_.size()) {
    ThrowColorError("Crate: " << name_ << " has no input: " << inputNo, "red");
  }
}

//-----------------------------------------------------------------------
// Methods of CorrelatorIndex class
//-----------------------------------------------------------------------

CorrelatorCrateInput::CorrelatorCrateInput(CorrelatorCrate* parent, unsigned inputNo)
{
  crate_          = parent;
  inputNo_        = inputNo;
  bdcBand_        = 0;
  corrBandInput_  = 0;

  // Wideband crate inputs have hardwired walsh columns.  In
  // particular, each band is configured to use a fixed walsh column
  // (1-8) for the corresponding input of each band, i.e.,  
  // 
  // Input 1-8  --> Walsh col 1-8
  // Input 9-16 --> Walsh col 1-8

  if(parent->type_ == CORR_WB) {
    if(inputNo_ > CorrelatorCrate::nInputPerBandWb_) {
      walshCol_.setWalshColNo(inputNo - CorrelatorCrate::nInputPerBandWb_);   
    } else {
      walshCol_.setWalshColNo(inputNo);
    }

    walshCol_.setHardwired(true);
  }
}

CorrelatorCrateInput::~CorrelatorCrateInput()
{
}

 bool CorrelatorCrateInput::isUsedByConfiguredAstroBand()
{
  try {
    AstroBandInput* abInput = getAstroBandInput();
    return abInput->isUsedByConfiguredAstroBand();
  } catch(...) {
    return false;
  }
}

/**.......................................................................
 * Return the block downconverter band associated with this correlator
 * crate input
 */
Band* CorrelatorCrateInput::getBdcBand()
{
  validateBdcBand();
  return bdcBand_;
}

void CorrelatorCrateInput::validateBdcBand()
{
  if(!bdcBand_) {
    ThrowColorError("No block downconverter band is currently connected to correlator input: " 
		    << crate_->name_ << ":" << inputNo_, "red");
  }
}

/**.......................................................................
 * Return the correlator band input to which this crate input is assigned
 */
CorrelatorBandInput* CorrelatorCrateInput::getCorrelatorBandInput()
{
  validateCorrelatorBandInput();
  return corrBandInput_;
}

void CorrelatorCrateInput::validateCorrelatorBandInput()
{
  if(!corrBandInput_) {
    ThrowColorError("No correlator band input is currently assigned to correlator input: " 
		    << crate_->name_ << ":" << inputNo_, "red");
  }
}

/**.......................................................................
 * Return the astro band input to which this crate input is assigned
 */
AstroBandInput* CorrelatorCrateInput::getAstroBandInput()
{
  validateAstroBandInput();
  return dynamic_cast<AstroBandInput*>(toNode_);
}

void CorrelatorCrateInput::validateAstroBandInput()
{
  if(toNode_ == 0) {
    ThrowColorError("No astro band input is currently assigned to correlator input: " 
		    << crate_->name_ << ":" << inputNo_, "red");
  }
}

bool CorrelatorCrateInput::isConnectedToAstroBand()
{
  try {
    validateAstroBandInput();
    return true;
  } catch(...) {
    return false;
  }
}

/**.......................................................................
 * Map a correlator crate input to a correlator band input
 */
 void CorrelatorCrateInput::mapTo(CorrelatorBandInput* input)
{
  corrBandInput_ = input;
}

/**.......................................................................
 * Map a correlator crate input to an astro band input
 */
 void CorrelatorCrateInput::mapTo(ConnectableNode* node, CorrelatorType type)
{
  AstroBandInput* input = dynamic_cast<AstroBandInput*>(node);

  if(input == 0) {
    ThrowError("CorrelatorCrateInputs can only be mapped to AstroBandInputs");
  }

  // If we are already mapped to an astro band, clear that astro band's
  // mapping first

  if(toNode_ != 0 && toNode_ != node) 
    toNode_->clearTo(this, type);

  // Now remap to the current input

  toNode_ = node;

  // Set ownership of this device to the current astro band

  setOwnershipTo(input->band_->bandNo_);

  // And set ownership of any BDC band we are currently connected to
  // to this device

  if(bdcBand_) {
    bdcBand_->setOwnershipTo(input->band_->bandNo_);
  }
}

/**.......................................................................
 * Clear any mappings of this correlator crate input
 */
void CorrelatorCrateInput::clearTo(ConnectableNode* node, CorrelatorType type)
{
  if(type & crate_->type_) {
    toNode_ = 0;
    setOwnershipTo((unsigned)0);
  } else {
    ThrowColorError(std::endl << "You are attempting to modify the input to a crate (" << crate_->name_ << ")"
		    << " that you are not allowed to modify (controlling subarray owns " << type << ")" << std::endl, "red");
  }
}

//-----------------------------------------------------------------------
// Methods of AstroBandConfiguration class
//-----------------------------------------------------------------------

AstroBandConfiguration::AstroBandConfiguration()
{
}

AstroBandConfiguration::AstroBandConfiguration(const AstroBandConfiguration& conf)
{
  *this = conf;
}

AstroBandConfiguration::AstroBandConfiguration(AstroBandConfiguration& conf)
{
  *this = conf;
}

void AstroBandConfiguration::operator=(const AstroBandConfiguration& conf)
{
  *this = (AstroBandConfiguration&)conf;
}

void AstroBandConfiguration::operator=(AstroBandConfiguration& conf)
{
  name_          = conf.name_;
  inputMappings_ = conf.inputMappings_;
}

AstroBandConfiguration::~AstroBandConfiguration()
{
}

//-----------------------------------------------------------------------
// Methods of the AstroBandInputMapping class
//-----------------------------------------------------------------------

AstroBandInputMapping::AstroBandInputMapping(std::string spec1, std::string spec2)
{
  String spec1Str(spec1);
  String spec2Str(spec2);

  if(spec1Str.contains("AB") && spec2Str.contains("COR")) {
    astroSpec_ = spec1;
    inputSpec_ = spec2;
  } else if(spec1Str.contains("COR") && spec2Str.contains("AB")) {
    astroSpec_ = spec2;
    inputSpec_ = spec1;
  } else if(spec1Str.contains("C3G") && spec2Str.contains("AB")) {
    astroSpec_ = spec2;
    inputSpec_ = spec1;
  } else {
    ThrowColorError("You must specify a mapping between a set of hardware inputs "
		    "(BD, SLCOR, or WBCOR) or correlator band inputs (e.g., C3G[1-8]) and astro band inputs (AB)", "red");
  }
}

AstroBandInputMapping::AstroBandInputMapping(const AstroBandInputMapping& mp)
{
  *this = mp;
}

AstroBandInputMapping::AstroBandInputMapping(AstroBandInputMapping& mp)
{
  *this = mp;
}

void AstroBandInputMapping::operator=(const AstroBandInputMapping& mp)
{
  *this = (AstroBandInputMapping&)mp;
}

void AstroBandInputMapping::operator=(AstroBandInputMapping& mp)
{
  inputSpec_ = mp.inputSpec_;
  astroSpec_ = mp.astroSpec_;
}

AstroBandInputMapping::~AstroBandInputMapping()
{
}

//-----------------------------------------------------------------------
// Methods of Switch class
//-----------------------------------------------------------------------

Switch::Switch(unsigned id, std::string name)
{
  initialize(id, name);
}

Switch::Switch(const Switch& sw)
{
  *this = sw;
}

Switch::Switch(Switch& sw)
{
  *this = sw;
}

void Switch::operator=(const Switch& sw)
{
  *this = (Switch&)sw;
}

void Switch::operator=(Switch& sw)
{
  initialize(sw.switchNo_, sw.name_);
}

void Switch::initialize(unsigned switchNo, std::string name)
{
  switchNo_       = switchNo;
  name_           = name;
  bdcInput_       = 0;
  currentChannel_ = 0;

  astroBandMask_.resize(AstroBand::nBandMax_+1);
  astroBandMask_.setAllBitsLow();

  initializeChannelMap();
}

void Switch::validateBdcInput()
{
  if(!bdcInput_) {
    ThrowColorError("No block downconverter input is currently associated with switch SW" << switchNo_ << "(" << name_ << ")", "red");
  }
}

BlockDownconverterInput* Switch::getBdcInput()
{
  validateBdcInput();
  return bdcInput_;
}

void Switch::validateCurrentChannel()
{
  if(!currentChannel_) {
    ThrowColorError("No channel is currently selected for switch SW" << switchNo_ << "(" << name_ << ")", "red");
  }
}

 SwitchChannel* Switch::getCurrentChannel()
{
  validateCurrentChannel();
  return currentChannel_;
}

void Switch::initializeChannelMap()
{
  // And initialize the vector of channels for this switch
  
  channelMap_[SW_CHAN_1] = new SwitchChannel(this, SW_CHAN_1);
  channelMap_[SW_CHAN_2] = new SwitchChannel(this, SW_CHAN_2);
  channelMap_[SW_CHAN_3] = new SwitchChannel(this, SW_CHAN_3);
  channelMap_[SW_CHAN_4] = new SwitchChannel(this, SW_CHAN_4);
}

/**.......................................................................
 * Return true if the switch can be configured by the requested astro
 * band.  We return true if either of this switch is the sole owner of
 * the switch, or if the requested channel is the same as the current
 * switch setting.
 */
bool Switch::canBeConfiguredBy(AstroBandInfo* info, SwitchChannel* channel)
{
  return ConfigurableDeviceMultiAstroBand::canBeConfiguredBy(info) || currentChannel_ == 0 || currentChannel_ == channel;
}

void Switch::clearChannelMap()
{
  std::map<SwitchChannelId, SwitchChannel*>::iterator slot;

  for(slot = channelMap_.begin(); slot != channelMap_.end(); slot++) {
    slot->second->if_ = 0;
  }
}

void Switch::clearSwitchSetting()
{
  currentChannel_ = 0;
}

Switch::~Switch()
{
  std::map<SwitchChannelId, SwitchChannel*>::iterator slot;

  for(slot = channelMap_.begin(); slot != channelMap_.end(); slot++) {
    delete slot->second;
  }
}

void Switch::validateChannel(SwitchChannelId chanId)
{
  if(channelMap_.find(chanId) == channelMap_.end()) {
    ThrowColorError("Switch: " << name_ << " has no channel: " << chanId, "red");
  }
}

/**.......................................................................
 * Return true if this switch is connected to any currently configured
 * astroband input
 */
bool Switch::isUsedByConfiguredAstroBand()
{
  try {
    BlockDownconverterInput* bdcInput = getBdcInput();
    return bdcInput->isUsedByConfiguredAstroBand();
  } catch(...) {
    return false;
  }
}

void Switch::selectChannel(SwitchChannelId chanId)
{
  validateChannel(chanId);
  currentChannel_ = channelMap_[chanId];
}

std::string Switch::printDown()
{
  std::ostringstream os;

  return os.str();
}

//-----------------------------------------------------------------------
// Methods of SwitchChannel class
//-----------------------------------------------------------------------

SwitchChannel::SwitchChannel(Switch* sw, SwitchChannelId channelId)
{
  switch_    = sw;
  channelId_ = channelId;
  if_        = 0;
}

 SwitchChannel::~SwitchChannel()
 {
 }

std::string SwitchChannel::printUp()
{
  std::ostringstream os;

  os << std::left << setw(5) << channelId_;

  if(if_) {
    os << " --> " << if_->printUp();
  }

  return os.str();
}

std::string SwitchChannel::printDown()
{
  std::ostringstream os;

  os << "SW " << std::left << setw(2) << switch_->switchNo_
     << " (" << setw(5) << std::left << switch_->name_ << ")" << " CHAN " << channelId_;
  os << " (";

  if(this == switch_->currentChannel_) {
    os << setw(strlen("NOT SELECTED")) << "SELECTED";
    os << ") --> "<< switch_->bdcInput_->printDown(os, true);
  } else {
    os << setw(strlen("NOT SELECTED")) << "NOT SELECTED";
    os << ") --> "<< switch_->bdcInput_->printDown(os, false);
  }
  return os.str();
}

 void SwitchChannel::validateAntennaIF()
{
  if(!if_) {
    ThrowColorError("Switch channel SW" << switch_->switchNo_ << ":" << channelId_
		    << " is not currently associated with any antenna IF", "red");
  }
}

AntennaIF* SwitchChannel::getAntennaIF()
{
  validateAntennaIF();
  return if_;
}

//-----------------------------------------------------------------------
// Methods of SwitchyardConfiguration class
//-----------------------------------------------------------------------

SwitchyardConfiguration::SwitchyardConfiguration()
{
  astroBandConf_ = 0;
}

SwitchyardConfiguration::SwitchyardConfiguration(const SwitchyardConfiguration& conf)
{
  *this = conf;
}

SwitchyardConfiguration::SwitchyardConfiguration(SwitchyardConfiguration& conf)
{
  *this = conf;
}

void SwitchyardConfiguration::operator=(const SwitchyardConfiguration& conf)
{
  *this = (SwitchyardConfiguration&)conf;
}

void SwitchyardConfiguration::operator=(SwitchyardConfiguration& conf)
{
  name_          = conf.name_;
  ifMappings_    = conf.ifMappings_;
  astroBandConf_ = conf.astroBandConf_;
}

SwitchyardConfiguration::~SwitchyardConfiguration()
{
}

AstroBandConfiguration* SwitchyardConfiguration::getAstroBandConfiguration()
{
  validateAstroBandConfiguration();
  return astroBandConf_;
}

void SwitchyardConfiguration::validateAstroBandConfiguration()
{
  if(!astroBandConf_) {
    ThrowError("Switchyard configuration " << name_ << " is not currently associated with any astro band configuration");
  }
}

//-----------------------------------------------------------------------
// Methods of Digitizer class
//-----------------------------------------------------------------------

Digitizer::Digitizer(SignalPathMap* spm, CorrelatorType corrType, unsigned digitizerNo) 
{ 
  type_        = corrType;
  digitizerNo_ = digitizerNo;
  
  astroBandInputs_.resize(0);
  
  astroBandMask_.resize(AstroBand::nBandMax_+1);
  astroBandMask_.setAllBitsLow();

  // Initialize the two bandformer boards that process this digitizer
  
  FpgaBoard* brd = 0;

  brd = new FpgaBoard(spm, digitizerNo_, "BF1", corrType);
  brd->mapFrom(this, type_);
  bandFormers_.push_back(brd);

  brd = new FpgaBoard(spm, digitizerNo_, "BF2", corrType);
  brd->mapFrom(this, type_);
  bandFormers_.push_back(brd);
}

bool Digitizer::canBeConfiguredBy(AstroBandInfo* info)
{
  return ConfigurableDeviceMultiAstroBand::belongsTo(info);
}

/**.......................................................................
 * Define what it means to map to this object
 */
void Digitizer::mapFrom(ConnectableNode* node, CorrelatorType type)
{
  AntennaIF* input = dynamic_cast<AntennaIF*>(node);

  if(input == 0) {
    ThrowError("Currently, only AntennaIFs can be connected to Digitizers");
  }

  fromNode_ = node;
}

/**.......................................................................
 * Define what it means for an AstroBandInput to map to  a digitizer
 */
void Digitizer::mapTo(ConnectableNode* node, CorrelatorType type)
{
  AstroBandInput* input = dynamic_cast<AstroBandInput*>(node);
    
  if(input == 0)
    ThrowError("Digitizers can only map to AstroBandInputs");

  // If the input is not null, and the correlator type is one of ours,
  // add the input to our map

  if(input && (type_ & type)) {

    std::vector<AstroBandInput*>::iterator iter=astroBandInputs_.begin();
    for(; iter != astroBandInputs_.end(); iter++) {
      if(*iter == input)
	break;
    }

    if(iter == astroBandInputs_.end()) {
      astroBandInputs_.push_back(input);
    }
  }
}

/**.......................................................................
 * Define what it means for an AstroBandInput to unmap from a digitizer
 */
void Digitizer::clearTo(ConnectableNode* node, CorrelatorType type)
{
  AstroBandInput* input = dynamic_cast<AstroBandInput*>(node);
    
  if(input == 0)
    ThrowError("Digitizers can only map to AstroBandInputs");

  // If the input is not null, and the correlator type is one of ours,
  // erase the input from our map if it exists

  if(input && (type_ & type)) {

    std::vector<AstroBandInput*>::iterator iter=astroBandInputs_.begin();
    for(; iter != astroBandInputs_.end(); iter++) {
      if(*iter == input)
	break;
    }

    if(iter != astroBandInputs_.end()) {
      astroBandInputs_.erase(iter);
    }
  }
}

/**.......................................................................
 * Return true if this digitizer is used by a currently-configured
 * astroband
 */
bool Digitizer::isUsedByConfiguredAstroBand()
{
  // Iterate over bandformer boards of this digitizer

  for(unsigned iBf=0; iBf < bandFormers_.size(); iBf++) {
    FpgaBoard* brd = bandFormers_[iBf];
    
    // Iterate over all corrband inputs this board processes

    for(unsigned iInput=0; iInput < brd->corrBandInputs_.size(); iInput++) {
      CorrelatorBandInput* input = brd->corrBandInputs_[iInput];
	
      // If this input is connected to an astroband, return whether or
      // not it is configured

      if(input->isConnectedToAstroBand()) {
	AstroBandInput* abInput = input->getAstroBandInput();

	if(abInput->isConfigured_)
	  return true;
      }
    }
  }

  return false;
}
 
Digitizer::~Digitizer() 
{
  for(unsigned i=0; i < bandFormers_.size(); i++) {
    delete bandFormers_[i];
  }
  
  bandFormers_.resize(0);
}

std::string Digitizer::printDown()
{
  std::ostringstream os;

  os << setw(7) << std::left << name_;
  
  // Print correlator band inputs
  
  os << "--> [";
  
  bool first = true;
  for(unsigned iBf=0; iBf < bandFormers_.size(); iBf++) {
    FpgaBoard* brd = bandFormers_[iBf];

    for(unsigned iInput=0; iInput < brd->corrBandInputs_.size(); iInput++) {
      CorrelatorBandInput* input = brd->corrBandInputs_[iInput];
	
      if(input->isConnectedToAstroBand()) {

	if(first) {
	  os << input->band_->name_ << ":" << input->inputNo_;
	  first = false;
	} else {
	  os << "," << input->band_->name_ << ":" << input->inputNo_;
	}
      }
    }
  }	  
  
  os << "]";

  // Print astroband inputs too
  
  os << "--> [";
  
  first = true;
  for(unsigned iBf=0; iBf < bandFormers_.size(); iBf++) {
    FpgaBoard* brd = bandFormers_[iBf];

    for(unsigned iInput=0; iInput < brd->corrBandInputs_.size(); iInput++) {
      CorrelatorBandInput* input = brd->corrBandInputs_[iInput];
	
      if(input->isConnectedToAstroBand()) {
	AstroBandInput* abInput = input->getAstroBandInput();
	
	if(first) {
	  os << "AB" << abInput->band_->bandNo_ << ":" << abInput->inputNo_;
	  first = false;
	} else {
	  os << ",AB" << abInput->band_->bandNo_ << ":" << abInput->inputNo_;
	}
      }
    }
  }	  
  
  os << "]";
  
  return os.str();
}

AntennaIF* Digitizer::getAntennaIF()
{
  ConnectableNode* node = fromNode_;
  for(; node != 0; node = node->fromNode_) {
    AntennaIF* antIf = dynamic_cast<AntennaIF*>(node);
    if(antIf != 0) {
      return antIf;
    }
  }

  ThrowColorError("Digitizer " << name_ << " is not currently associated with any antenna IF", "red");
  return 0;
}

//-----------------------------------------------------------------------
// Methods of the FpgaBoard class
//-----------------------------------------------------------------------

FpgaBoard::FpgaBoard(SignalPathMap* spm, unsigned inputNo, std::string name, CorrelatorType type)
{
  name_          = name;
  type_          = type;
  astroBandConf_ = 0;
  
  astroBandMask_.resize(AstroBand::nBandMax_+1);
  astroBandMask_.setAllBitsLow();

  // If these are bandformer boards, associate the correct bands with
  // them now

  std::ostringstream os;

  if(name_ == "BF1") {
    for(unsigned iBand=0; iBand < 4; iBand++) {
      os.str("");
      os << "C3G" << iBand+1;
      CorrelatorBand* band = spm->getCorrBand(os.str());
      CorrelatorBandInput* input = band->getInput(inputNo);
      corrBandInputs_.push_back(input);
      input->mapFrom(this);
    }
  } else if(name_ == "BF2") {
    for(unsigned iBand=4; iBand < 8; iBand++) {
      os.str("");
      os << "C3G" << iBand+1;
      CorrelatorBand* band = spm->getCorrBand(os.str());
      CorrelatorBandInput* input = band->getInput(inputNo);
      corrBandInputs_.push_back(input);
      input->mapFrom(this);
    }
  }

}
 
FpgaBoard::~FpgaBoard()
{
}

/**.......................................................................
 * Return true if this FPGA board can be configured by the passed
 * astroband
 */
bool FpgaBoard::canBeConfiguredBy(AstroBandInfo* info)
{
  if(astroBandConf_ != 0 && astroBandConf_ != info->astroBandConf_)
    return false;
  else
    return true;
}

/**.......................................................................
 * Add this astroband to the set that we are connected to, and set our
 * FPGA configuration to that of the configuring astroband
 */
void FpgaBoard::setOwnershipTo(AstroBandInfo* info)
{
  if(astroBandConf_ && astroBandConf_ != info->astroBandConf_)
    ThrowColorError(std::endl << "Attempt to change the FPGA configuration of " << *this 
		    << " to " << info->astroBandConf_->name_ 
		    << ", which conflicts with the current configuration " << astroBandConf_->name_ 
		    << " currently in use by " << listOwners() << std::endl, "red");

  astroBandConf_ = info->astroBandConf_;

  // Mark this device as owned by this astroband

  ConfigurableDeviceMultiAstroBand::setOwnershipTo(info);

  // And mark the parent digitizer of this device as also owned by
  // this digitizer

  Digitizer* dig = dynamic_cast<Digitizer*>(fromNode_);

  if(dig == 0) {
    ThrowError("FpgaBoard is not connected to any digitizer.");
  }

  dig->setOwnershipTo(info);
}

/**.......................................................................
 * Remove this astroband from ownership of this device
 */
void FpgaBoard::removeFromOwnership(unsigned astroBandNo)
{
  // If we are now the sole owner of this board, clear its FPGA
  // configuration

  if(ConfigurableDeviceMultiAstroBand::isSoleOwner(astroBandNo))
    astroBandConf_ = 0;

  // Remove this astroband from our mask of owners

  ConfigurableDeviceMultiAstroBand::removeFromOwnership(astroBandNo);

  // Remove this astroband from our parent Digitizer's mask of owners

  Digitizer* dig = dynamic_cast<Digitizer*>(fromNode_);

  if(dig == 0) {
    ThrowError("FpgaBoard is not connected to any digitizer.");
  }
  
  dig->removeFromOwnership(astroBandNo);
}

//-----------------------------------------------------------------------
// Methods of SignalPathMap class
//-----------------------------------------------------------------------

/**.......................................................................
 * Map a set of correlator inputs to a set of astroband inputs
 */
void SignalPathMap::mapCorrToAstroBand(std::string corrSpec, std::string astroSpec, 
				       bool doMapping, unsigned baseIndex, CorrelatorType type, AstroBandInfo* info)
{
  // Parse the corr specification

  std::vector<std::string> crateNames;
  std::vector<unsigned> crateInputNos;

  parseCorrSpecification(corrSpec, crateNames, crateInputNos, baseIndex, doMapping);

  // If no inputs were specified, replace with all indices of a crate

  if(crateInputNos.size() == 0) {
    for(unsigned iInd=0; iInd < CorrelatorCrate::nInput_; iInd++) {
      crateInputNos.push_back(iInd+1);
    }
  }

  // Now parse the astroband specification

  std::vector<unsigned> astroBandNos;
  std::vector<unsigned> astroBandInputNos;

  parseAstroBandSpecification(astroSpec, astroBandNos, astroBandInputNos, baseIndex, doMapping);

  // Valid specifications can map multiple correlator crates to a
  // single astroBand, or multiple correlator crates to the same
  // number of astroBands, but not anything in between

  if(crateNames.size() != astroBandNos.size()) {
    if(astroBandNos.size() > 1) {
      ThrowColorError("You cannot map multiple correlator crates (" << corrSpec << ") "
		      "to a different number of astroBands (" << astroSpec << ")", "red");
    }
  }

  // If no inputs were specified, replace with the total number of
  // crate indices that were specified

  unsigned nCrateInputs=crateInputNos.size();

  if(astroBandNos.size() == 1) {
    nCrateInputs = crateNames.size() * crateInputNos.size();
  }  else {
    nCrateInputs = crateInputNos.size();
  }

  if(astroBandInputNos.size() == 0) {

    // If only one astroBand was specified, the implicit number of
    // indices must be equal to the total number of correlator crate
    // indices

    if(astroBandNos.size() == 1) {
      if(nCrateInputs > AstroBand::nInputMax_) {
	ThrowColorError("You can't map: " << nCrateInputs << " correlator crate inputs ("
		   << corrSpec << ") to " << AstroBand::nInputMax_ << " astroBand inputs ("
			<< astroSpec << ")", "red");
      }
    }

    // Now assign implicit astroBand input nos to match the number of
    // crate inputs that were specified

    for(unsigned iInd=0; iInd < nCrateInputs; iInd++) {
      astroBandInputNos.push_back(iInd+1);
    }

  }

  if(astroBandInputNos.size() != nCrateInputs) {
    ThrowColorError("You can't map: " << nCrateInputs << " correlator crate inputs ("
	       << corrSpec << ") to " << astroBandInputNos.size() << " astroBand inputs ("
		    << astroSpec << ")", "red");
  }

  // Now perform the individual mappings corresponding to the
  // specification that was passed

  if(astroBandNos.size() > 1) {

    for(unsigned iCrate=0; iCrate < crateNames.size(); iCrate++) {
      for(unsigned iCrateInput=0; iCrateInput < crateInputNos.size(); iCrateInput++) {
	mapCorrInputToAstroBandInput(crateNames[iCrate], crateInputNos[iCrateInput],
				     astroBandNos[iCrate], astroBandInputNos[iCrateInput], doMapping, type, info);
      }
    }

  } else {
    unsigned iAstroBandInput=0;
    
    // Attempt only the part of the mapping associated with the
    // requested astroBandNo


    for(unsigned iCrate=0; iCrate < crateNames.size(); iCrate++) {
      for(unsigned iCrateInput=0; iCrateInput < crateInputNos.size(); iCrateInput++, iAstroBandInput++) {
	mapCorrInputToAstroBandInput(crateNames[iCrate], crateInputNos[iCrateInput],
				     astroBandNos[0], astroBandInputNos[iAstroBandInput], doMapping, type, info);
      }
    }

  }

  return;
}

/**.......................................................................
 * Map a set of correlator band inputs to a set of astroband inputs
 */
void SignalPathMap::mapBandToAstroBand(std::string bandSpec, std::string astroSpec, 
				       bool doMapping, unsigned baseIndex, CorrelatorType type, AstroBandInfo* info)
{
  // Parse the band specification

  std::vector<std::string> bandNames;
  std::vector<unsigned> bandInputNos;

  parseCorrBandSpecification(bandSpec, bandNames, bandInputNos, baseIndex, doMapping);

  // If no inputs were specified, replace with all indices of a band

  if(bandInputNos.size() == 0) {
    for(unsigned iInd=0; iInd < CorrelatorBand::nInputC3g_; iInd++) {
      bandInputNos.push_back(iInd+1);
    }
  }

  // Now parse the astroband specification

  std::vector<unsigned> astroBandNos;
  std::vector<unsigned> astroBandInputNos;

  parseAstroBandSpecification(astroSpec, astroBandNos, astroBandInputNos, baseIndex, doMapping);

  // Valid specifications can map multiple correlator crates to a
  // single astroBand, or multiple correlator crates to the same
  // number of astroBands, but not anything in between

  if(bandNames.size() != astroBandNos.size()) {
    if(astroBandNos.size() > 1) {
      ThrowColorError("You cannot map multiple correlator bands (" << bandSpec << ") "
		      "to a different number of astroBands (" << astroSpec << ")", "red");
    }
  }

  // If no inputs were specified, replace with the total number of
  // band indices that were specified

  unsigned nBandInputs=bandInputNos.size();

  if(astroBandNos.size() == 1) {
    nBandInputs = bandNames.size() * bandInputNos.size();
  }  else {
    nBandInputs = bandInputNos.size();
  }

  if(astroBandInputNos.size() == 0) {

    // If only one astroBand was specified, the implicit number of
    // indices must be equal to the total number of correlator band
    // indices

    if(astroBandNos.size() == 1) {
      if(nBandInputs > AstroBand::nInputMax_) {
	ThrowColorError("You can't map: " << nBandInputs << " correlator band inputs ("
		   << bandSpec << ") to " << AstroBand::nInputMax_ << " astroBand inputs ("
			<< astroSpec << ")", "red");
      }
    }

    // Now assign implicit astroBand input nos to match the number of
    // band inputs that were specified

    for(unsigned iInd=0; iInd < nBandInputs; iInd++) {
      astroBandInputNos.push_back(iInd+1);
    }

  }

  if(astroBandInputNos.size() != nBandInputs) {
    ThrowColorError("You can't map: " << nBandInputs << " correlator band inputs ("
	       << bandSpec << ") to " << astroBandInputNos.size() << " astroBand inputs ("
		    << astroSpec << ")", "red");
  }

  // Now perform the individual mappings corresponding to the
  // specification that was passed

  if(astroBandNos.size() > 1) {

    for(unsigned iBand=0; iBand < bandNames.size(); iBand++) {
      for(unsigned iBandInput=0; iBandInput < bandInputNos.size(); iBandInput++) {
	mapBandInputToAstroBandInput(bandNames[iBand], bandInputNos[iBandInput],
				     astroBandNos[iBand], astroBandInputNos[iBandInput], doMapping, type, info);
      }
    }

  } else {
    unsigned iAstroBandInput=0;
    
    // Attempt only the part of the mapping associated with the
    // requested astroBandNo


    for(unsigned iBand=0; iBand < bandNames.size(); iBand++) {
      for(unsigned iBandInput=0; iBandInput < bandInputNos.size(); iBandInput++, iAstroBandInput++) {
	mapBandInputToAstroBandInput(bandNames[iBand], bandInputNos[iBandInput],
				     astroBandNos[0], astroBandInputNos[iAstroBandInput], doMapping, type, info);
      }
    }

  }

  return;
}

/**.......................................................................
 * Map a set of digitizers to a set of astroband inputs
 */
void SignalPathMap::mapDigitizerToAstroBand(std::string digSpec, std::string astroSpec, 
					    bool doMapping, unsigned baseIndex, CorrelatorType type, AstroBandInfo* info)
{
  // Parse the digitizer specification

  std::vector<std::string> digIds;
  parseDigitizerSpecification(digSpec, digIds);

  // Now parse the astroband specification

  std::vector<unsigned> astroBandNos;
  std::vector<unsigned> astroBandInputNos;

  parseAstroBandSpecification(astroSpec, astroBandNos, astroBandInputNos, baseIndex, doMapping);

  // Valid specifications require a one-to-one mapping of digitizers
  // to astro band inputs

  if(digIds.size() != astroBandInputNos.size())
    ThrowColorError("You must map digitizers (" << digSpec << ") to the same number of astroBand inputs (" << astroSpec, "red");

  // If no inputs were specified, the implicit numbe of indices must
  // be equal to the number of digitizers specified

  if(astroBandInputNos.size() == 0) {

    if(digIds.size() != AstroBand::nInputMax_) {
      ThrowColorError("You can't map: " << digIds.size() << " digitizers (" << digSpec << ") to " << AstroBand::nInputMax_ << "("
		      << astroSpec << ") astroBand inputs", "red");
    }

    // Now assign implicit astroBand input nos to match the number of
    // crate inputs that were specified

    for(unsigned iInd=0; iInd < AstroBand::nInputMax_; iInd++)
      astroBandInputNos.push_back(iInd+1);
  }

  // Now perform the individual mappings corresponding to the
  // specification that was passed

  for(unsigned iAstroBand=0; iAstroBand < astroBandNos.size(); iAstroBand++) {
    for(unsigned iAstroBandInput=0; iAstroBandInput < astroBandInputNos.size(); iAstroBandInput++) {
      mapDigitizerToAstroBandInput(digIds[iAstroBandInput], astroBandNos[iAstroBand], astroBandInputNos[iAstroBandInput], doMapping, type, info);
    }
  }

  return;
}

/**.......................................................................
 * Map a single correlator crate input to an astro band input
 */
void SignalPathMap::mapCorrInputToAstroBandInput(std::string crateName, unsigned crateInputNo, 
						 unsigned astroBandNo,  unsigned astroBandInputNo, 
						 bool doMapping, CorrelatorType type, AstroBandInfo* info)
{
  // Map a single crate and index to a single astroband and index

  CorrelatorCrate* crate           = getCrate(crateName);
  CorrelatorCrateInput* crateInput = crate->getInput(crateInputNo);

  AstroBand* astroBand             = getAstroBand(astroBandNo);
  AstroBandInput* astroBandInput   = astroBand->getInput(astroBandInputNo);

  // If this mapping is for a different astro band than the requested
  // astro band, just return quietly.  (We only want to execute the
  // parts of this mapping that relate to the requested astro band,
  // and don't care if the mapping specifies other bands too)

  if(info && astroBandNo != info->astroBandNo_)
    return;

  // Throw an error if someone is trying to modify this input who
  // shouldn't be

  if(!(type & crate->type_)) {
    ThrowColorError(std::endl << "You are attempting to modify the input to a crate ("
		    << crate->name_ << ") that you are not allowed to modify (controlling subarray owns " << type << ")" << std::endl, "red");
  }

  // If this input can be mapped, increment the number of inputs that
  // will be (or would be, if doMapping=false) mapped by this call.
  //
  // Else if this crate input is already mapped to a different band,
  // register the conflict and return

  if(!crateInput->canBeConfiguredBy(info)) {
    AstroBandInfo::registerConflict(info, crateInput);
    return;
  } else {
    AstroBandInfo::incrementConfiguredDevices(info);
  }
    
  // Now do the mapping if requested

  if(doMapping) {
    crateInput->mapTo((ConnectableNode*)astroBandInput);
    astroBandInput->mapFrom((ConnectableNode*)crateInput);
  }
}

/**.......................................................................
 * Map a single correlator band input to an astro band input
 */
void SignalPathMap::mapBandInputToAstroBandInput(std::string bandName, unsigned bandInputNo, 
						 unsigned astroBandNo,  unsigned astroBandInputNo, 
						 bool doMapping, CorrelatorType type, AstroBandInfo* info)
{
  // Map a single band and index to a single astroband and index

  CorrelatorBand* band           = getCorrBand(bandName);
  CorrelatorBandInput* bandInput = band->getInput(bandInputNo);

  AstroBand* astroBand             = getAstroBand(astroBandNo);
  AstroBandInput* astroBandInput   = astroBand->getInput(astroBandInputNo);

  // If this mapping is for a different astro band than the requested
  // astro band, just return quietly.  (We only want to execute the
  // parts of this mapping that relate to the requested astro band,
  // and don't care if the mapping specifies other bands too)

  if(info && astroBandNo != info->astroBandNo_)
    return;

  // Throw an error if someone is trying to modify this input who
  // shouldn't be

  if(!(type & band->type_)) {
    ThrowColorError(std::endl << "You are attempting to modify the input to a band ("
		    << band->name_ << ") that you are not allowed to modify (controlling subarray owns " << type << ")" << std::endl, "red");
  }

  // If this input can be mapped, increment the number of inputs that
  // will be (or would be, if doMapping=false) mapped by this call.
  //
  // Else if this band input is already mapped to a different band,
  // register the conflict and return

  if(!bandInput->canBeConfiguredBy(info)) {
    AstroBandInfo::registerConflict(info, bandInput);
    return;
  } else {
    AstroBandInfo::incrementConfiguredDevices(info);
  }
    
  // Now do the mapping if requested

  if(doMapping) {
    bandInput->setOwnershipTo(info);
    bandInput->mapTo((ConnectableNode*)astroBandInput, type);
    astroBandInput->mapFrom((ConnectableNode*)bandInput, type);
  }

}

/**.......................................................................
 * Map a single digitizer to an astro band input
 */
  void SignalPathMap::mapDigitizerToAstroBandInput(std::string digId,
						   unsigned astroBandNo,  unsigned astroBandInputNo, 
						   bool doMapping, CorrelatorType type, AstroBandInfo* info)
{
  //------------------------------------------------------------
  // Map a single digitizer a single astroband and input no
  //------------------------------------------------------------

  Digitizer* dig                 = getDigitizer(digId);

  AstroBand* astroBand           = getAstroBand(astroBandNo);
  AstroBandInput* astroBandInput = astroBand->getInput(astroBandInputNo);

  //------------------------------------------------------------
  // If this mapping is for a different astro band than the requested
  // astro band, just return quietly.  (We only want to execute the
  // parts of this mapping that relate to the requested astro band,
  // and don't care if the mapping specifies other bands too)
  //------------------------------------------------------------

  if(info && astroBandNo != info->astroBandNo_) {
    return;
  }

  //------------------------------------------------------------
  // Throw an error if someone is trying to modify this input who
  // shouldn't be
  //------------------------------------------------------------

  if(!(type & dig->type_)) {
    ThrowColorError(std::endl << "You are attempting to modify the input to a digitizer ("
		    << dig->name_ << ") that you are not allowed to modify (controlling subarray owns " << type << ")" << std::endl, "red");
  }

  //------------------------------------------------------------
  // If this digitizer can be mapped, increment the number of inputs
  // that will be (or would be, if doMapping=false) mapped by this
  // call.
  //
  // Else register the conflict and return
  //------------------------------------------------------------

  if(!dig->canBeConfiguredBy(info)) {
    return;
  } else {
    AstroBandInfo::incrementConfiguredDevices(info);
  }
    
  //------------------------------------------------------------
  // Now do the mapping if requested
  //------------------------------------------------------------

  if(doMapping) {
    dig->mapTo((ConnectableNode*)astroBandInput);
    astroBandInput->mapFrom((ConnectableNode*)dig);
  }
}

/**.......................................................................
 * Parse a specification of the form:
 *
 * AB[1-8]:[1-15]
 *
 */
void SignalPathMap::parseAstroBandSpecification(std::string astroSpec, 
						std::vector<unsigned>& bandIndices, 
						std::vector<unsigned>& inputIndices, 
						unsigned baseIndex, bool actualIndex)
{
  String str(astroSpec);
  str = str.toUpper();
  str.strip(' ');

  String bandStr;
  String inputStr;

  if(str.contains("AB")) {
    bandStr  = str.findNextInstanceOf("AB", true, ":", false, false);
    inputStr = str.findNextInstanceOf(":",  true, " ", false);
  } else {
    ThrowColorError("Unrecognized astroBand specification: " << str, "red");
  }

  // Get the crate indices corresponding to this specification
  
  bandIndices = extractIndexRange(bandStr, 1, AstroBand::nBandMax_, baseIndex, actualIndex);

  // If specific inputs were specified, parse these now

  if(!inputStr.isEmpty()) {
    inputIndices = extractIndexRange(inputStr, 1, AstroBand::nInputMax_);
  } else {
    inputIndices.resize(0);
  }

}

std::string SignalPathMap::fill(char fillChar, unsigned nChar)
{
  std::ostringstream os;

  for(unsigned i=0; i < nChar; i++) {
    os << fillChar;
  }

  return os.str();
}

unsigned& SignalPathMap::getWalshColMask(CorrelatorType type)
{
  switch (type) {
  case CORR_WB:
    return walshColMaskWb_;
    break;
  case CORR_SL:
    return walshColMaskSl_;
    break;
  default:
    if(type & CORR_C3G)
      return walshColMaskC3g_;
    ThrowColorError("Invalid request for walsh column mask for correlator: " << type, "red");

    // We'll never get here -- just to avoid compiler warnings

    return walshColMaskSl_;
    break;
  }
}

void SignalPathMap::clearWalshColumnMask(CorrelatorType type)
{

  if(type & CORR_WB)
    walshColMaskWb_ = 0x0;

  if(type & CORR_SL)
    walshColMaskSl_ = 0x0;

  if(type & CORR_C3G)
    walshColMaskC3g_ = 0x0;

}

bool SignalPathMap::isSetInWalshColumnMask(WalshColumn& wf, CorrelatorType type)
{
  unsigned wcno = wf.getWalshColNo();
  unsigned iwc  = wcno - 1;

  unsigned& mask = getWalshColMask(type);

  return (mask & (1U << iwc)) != 0x0;
}

void SignalPathMap::addToWalshColumnMask(WalshColumn& wf, CorrelatorType type)
{
  unsigned wcno = wf.getWalshColNo();
  unsigned iwc  = wcno - 1;

  unsigned& mask = getWalshColMask(type);

  mask |= (1U << iwc);
}

unsigned SignalPathMap::getNextUniqueWalshColNo(CorrelatorType type)
{
  unsigned& mask = getWalshColMask(type);

  for(unsigned i=0; i < WalshColumn::nWalshCol_; i++) {
    if((mask & (1U << i)) == 0x0) {
      return i+1;
    }
  }

  ThrowColorError("All walsh columns are in use for " << type << " correlator", "red");

  return 0;
}

/**.......................................................................
 * Iterate through all correlator crate inputs and antennas, setting
 * the walsh columns appropriately
 *
 * Function will only be called after clearWalshColumns(), so we are
 * guaranteed that the only Walsh columns that are set at this point
 * are ones that are required, either by hardware constraints
 * (correlator crates) or antenna configuration requests (user
 * specifying which Walsh function an antenna should have).
 */
void SignalPathMap::assignHardwiredWalshColumns()
{
  assignHardwiredCorrelatorWalshColumns();
  assignHardwiredAntennaWalshColumns();
}

/**.......................................................................
 * Propagate any hardwired antenna walsh column specifications down to
 * correlator crate inputs to which they are connected.
 */
void SignalPathMap::assignHardwiredAntennaWalshColumns()
{
  // Iterate through all antennas propagating any hardwired walsh
  // configuration down to any correlator crate inputs they are
  // connected to.
  
  for(unsigned iAnt=0; iAnt < antennas_.size(); iAnt++) {
    Antenna* ant = antennas_[iAnt];

    if(ant->walshCol_.isHardwired()) {
      propagateWalshColumnAssignment(ant);
    }
  }
}

/**.......................................................................
 * Propagate any hardwired correlator crate input walsh column
 * specifications to the corresponding antennas, and all correlator
 * crate inputs to which they are connected.
 */
void SignalPathMap::assignHardwiredCorrelatorWalshColumns()
{
  // Iterate through all correlator crate inputs, assigning walsh to
  // antennas connected to correlator inputs with hardwired walsh
  // functions
  
  for(unsigned iCrate=0; iCrate < crates_.size(); iCrate++) {
    CorrelatorCrate* crate = crates_[iCrate];

    for(unsigned iInput=0; iInput < crate->inputs_.size(); iInput++) {
      CorrelatorCrateInput* input = crate->inputs_[iInput];
      WalshColumn& cwc = input->walshCol_;

      // We only care about Walsh functions for inputs corresponding
      // to configured astro bands

      if(input->isConnectedToAstroBand()) {

	AstroBandInput* astroBandInput = input->getAstroBandInput();

	if(astroBandInput->isUsedByConfiguredAstroBand()) {
	
	  AstroBand* astroBand = astroBandInput->band_;
	  
	  // If this input is hard-wired, try to assign its walsh
	  // function to any antenna connected to it.
	  
	  if(cwc.isHardwired()) {
	    
	    // Get the antenna to which this input is currently connected
	    
	    Antenna* ant = 0;
	    
	    try {
	      
	      ant = input->getBdcBand()->getBdcInput()->getSwitch()->getCurrentChannel()->getAntennaIF()->getAntenna();
	      
	      // It is not an error for inputs that are owned by an astro
	      // band not to be connected to anything.  Just continue in
	      // this case
	      
	    } catch(Exception& err) {
	      continue;
	    }

	    WalshColumn& awc = ant->walshCol_;
	    
	    // Throw an error if this antenna has already been assigned
	    // a Walsh column, but it is different from ours.  We will
	    // only assign the walsh column if the antenna actually
	    // belongs to the subarray that has asserted this astroband
	    // configuration
	    
	    if(awc.isSet() && (ant->subarrayId_ == astroBand->subarrayId_)) {
	      
	      if(awc != cwc) {
		ThrowColorError("Antenna " << antNoToAntName(ant->antNo_) << " has been assigned walsh column " << awc 
				<< " but is connected to " << crate->name_ << ":" << input->inputNo_
				<< " which requires walsh column " << cwc, "red");
	      }
	      
	      // Else assign this input's walsh function to it
	      
	    } else {
	      
	      // Check that this walsh function isn't already in use by
	      // another antenna attached to this correlator.  But only
	      // if we are actually going to set this antenna's walsh
	      // function (is, if it belongs to us).
	      
	      if(ant->subarrayId_ == astroBand->subarrayId_) {
		
		if(isSetInWalshColumnMask(cwc, crate->type_)) {
		  ThrowColorError("Crate: " << crate->name_ << ":" << input->inputNo_ 
				  << " requires walsh column " << cwc 
				  << " for C" << ant->antNo_ 
				  << " but this walsh column is already in use by another antenna "
				  "attached to this correlator", "red");
		}
		
		// Only set this antenna's walsh column if this antenna is
		// owned by the subarray configuring this astroband
		
		awc.setWalshColNo(cwc.getWalshColNo());
	      }
	      
	      // Mark the associated walsh column as in-use for this
	      // correlator
	      
	      addToWalshColumnMask(cwc, crate->type_);
	      
	      // And propagate the walsh column assignment to all other
	      // inputs to which this antenna is attached.  But only if
	      // the antenna was actually assigned a walsh column, which
	      // will only be the case if it is owned by the configuring
	      // astroband
	      
	      if(ant->subarrayId_ == astroBand->subarrayId_) {
		propagateWalshColumnAssignment(ant);
	      }
	    }
	  }
	}
      }
    }
  }
}

/**.......................................................................
 * Propagate an antenna walsh column assignment to all correlator
 * crate inputs connected to this antenna.  At this point, the only
 * walsh functions that should be set are ones that are hardwired.
 */
void SignalPathMap::propagateWalshColumnAssignment(Antenna* antProp)
{
  propagateWalshColumnAssignment(antProp, antProp->walshCol_.getWalshColNo());
}

void SignalPathMap::propagateWalshColumnAssignment(Antenna* antProp, unsigned walshColNo)
{
  propagateWalshColumnAssignmentToCrates(antProp, walshColNo);
  propagateWalshColumnAssignmentToBands(antProp, walshColNo);
}

/**.......................................................................
 * Propagate an antenna walsh column assignment to all correlator
 * crate inputs connected to this antenna.  At this point, the only
 * walsh functions that should be set are ones that are hardwired.
 */
void SignalPathMap::propagateWalshColumnAssignmentToBands(Antenna* antProp, unsigned walshColNo)
{

  // Iterate through all correlator band inputs, assigning this
  // antennas walsh function to all correlator inputs connected to it.
  
  for(unsigned iBand=0; iBand < corrBands_.size(); iBand++) {
    CorrelatorBand* band = corrBands_[iBand];

    // We only propagate to bands of the C3G correlator, since the
    // crates have already been propagated to

    if(band->type_ != CORR_C3G)
      continue;

    for(unsigned iInput=0; iInput < band->inputs_.size(); iInput++) {

      CorrelatorBandInput* input = band->inputs_[iInput];
      WalshColumn&           cwc = input->walshCol_;

      AstroBandInput* astroBandInput = 0;

      try {
	astroBandInput = input->getAstroBandInput();
      } catch(...) {
	continue;
      }

      if(astroBandInput->isUsedByConfiguredAstroBand()) {
	
	Antenna* ant = 0;
	AstroBand* astroBand = astroBandInput->band_;
	
	try {
	  
	  // Get the antenna to which this input is currently connected
	  
	  ant = input->getAntennaIF()->getAntenna();
	  
	  // It is not an error for inputs that are owned by an astro
	  // band not to be connected to anything.  Just continue in
	  // this case
	  
	} catch(Exception& err) {
	  continue;
	}
	
	// If the antennas match, attempt to assign the input's walsh
	// function.  
	//
	// But only if the antenna is owned by the same subarray that
	// configured the astroband in which this correlator crate
	// input is involved.
	//
	// If not, we don't care if the current crate input walsh
	// column is different.
	
	if(ant == antProp && ant->subarrayId_ == astroBand->subarrayId_) {
	  
	  WalshColumn& awc = ant->walshCol_;
	  
	  // If the input's walsh function is already set, it had
	  // better match the one we are trying to assign, or an
	  // incompatible assignment of walsh functions has been
	  // specified
	  
	  if(cwc.isSet()) {
	    if(cwc != awc) {
	      ThrowColorError("You are attempting to set the Walsh column for band " 
			      << band->name_ << ":" << input->inputNo_
			      << " to the walsh column for antenna C" << antProp->antNo_ 
			      << "(" << awc << ")"
			      << " but this input has already been assigned an incompatible "
			      "walsh column: " << cwc, "red");
	    }
	    
	    // If no walsh column has been set, set it now.  Note that
	    // we don't check here if this walsh column is already in
	    // use, since it is not an error for multiple correlator
	    // crate inputs to use the same walsh column
	    
	  } else {
	    cwc.setWalshColNo(walshColNo);
	    addToWalshColumnMask(cwc, band->type_);
	  }
	}
      }
    }
  }
}

/**.......................................................................
 * Propagate an antenna walsh column assignment to all correlator
 * crate inputs connected to this antenna.  At this point, the only
 * walsh functions that should be set are ones that are hardwired.
 */
void SignalPathMap::propagateWalshColumnAssignmentToCrates(Antenna* antProp, unsigned walshColNo)
{
  // Iterate through all correlator crate inputs, assigning this
  // antennas walsh function to all correlator inputs connected to it.
  
  for(unsigned iCrate=0; iCrate < crates_.size(); iCrate++) {
    CorrelatorCrate* crate = crates_[iCrate];

    for(unsigned iInput=0; iInput < crate->inputs_.size(); iInput++) {

      CorrelatorCrateInput* input = crate->inputs_[iInput];
      WalshColumn&            cwc = input->walshCol_;

      // We only care about Walsh functions for inputs corresponding
      // to configured astro bands
      
      if(input->isConnectedToAstroBand()) {

	AstroBandInput* astroBandInput = input->getAstroBandInput();

	if(astroBandInput->isUsedByConfiguredAstroBand()) {
	
	  Antenna* ant = 0;
	  AstroBand* astroBand = astroBandInput->band_;
	  
	  try {
	    
	    // Get the antenna to which this input is currently connected
	    
	    ant = input->getBdcBand()->getBdcInput()->getSwitch()->
	      getCurrentChannel()->getAntennaIF()->getAntenna();
	    
	    // It is not an error for inputs that are owned by an astro
	    // band not to be connected to anything.  Just continue in
	    // this case
	    
	  } catch(Exception& err) {
	    continue;
	  }
	  
	  // If the antennas match, attempt to assign the input's walsh
	  // function.  
	  //
	  // But only if the antenna is owned by the same subarray that
	  // configured the astroband in which this correlator crate
	  // input is involved.
	  //
	  // If not, we don't care if the current crate input walsh
	  // column is different.
	  
	  if(ant == antProp && ant->subarrayId_ == astroBand->subarrayId_) {
	    
	    WalshColumn& awc = ant->walshCol_;
	    
	    // If the input's walsh function is already set, it had
	    // better match the one we are trying to assign, or an
	    // incompatible assignment of walsh functions has been
	    // specified
	    
	    if(cwc.isSet()) {
	      if(cwc != awc) {
		ThrowColorError("You are attempting to set the Walsh column for crate " 
				<< crate->name_ << ":" << input->inputNo_
				<< " to the walsh column for antenna C" << antProp->antNo_ 
				<< "(" << awc << ")"
				<< " but this input has already been assigned an incompatible "
				"walsh column: " << cwc, "red");
	      }
	      
	      // If no walsh column has been set, set it now.  Note that
	      // we don't check here if this walsh column is already in
	      // use, since it is not an error for multiple correlator
	      // crate inputs to use the same walsh column
	      
	    } else {
	      cwc.setWalshColNo(walshColNo);
	      addToWalshColumnMask(cwc, crate->type_);
	    }
	  }
	}
      }
    }
  }
}

/**.......................................................................
 * Assign walsh functions to antennas whose walsh functions have not
 * already been specified.
 *
 * At this point, hardwired walsh functions have already been
 * propagated, so we are guaranteed that antennas connected to both
 * correlators have registered their walsh functions in each
 * correlator's mask, which means that a walsh function that is free
 * is free for both correlators.
 *
 * We are also guaranteed that any correlator input that is unassigned
 * at this point is connected to an antenna that is also unassigned
 */
void SignalPathMap::assignFreeWalshColumns()
{
  assignFreeWalshColumnsToCrates();
  assignFreeWalshColumnsToBands();
}

void SignalPathMap::assignFreeWalshColumnsToCrates()
{
  // Iterate through all correlator crate inputs, assigning free walsh
  // functions to any unassigned antennas.
  
  for(unsigned iCrate=0; iCrate < crates_.size(); iCrate++) {
    CorrelatorCrate* crate = crates_[iCrate];

    for(unsigned iInput=0; iInput < crate->inputs_.size(); iInput++) {

      CorrelatorCrateInput* input = crate->inputs_[iInput];
      WalshColumn&            cwc = input->walshCol_;

      // We only care about Walsh functions for inputs corresponding
      // to configured astro bands, and only inputs whose walsh
      // columns aren't already assigned.

      if(input->isConnectedToAstroBand()) {
	AstroBandInput* astroBandInput = input->getAstroBandInput();

	if(astroBandInput->isUsedByConfiguredAstroBand() && !cwc.isSet()) {
	  
	  Antenna*   ant       = 0;
	  AstroBand* astroBand = astroBandInput->band_;
	  
	  try {
	    
	    // Get the antenna to which this input is currently connected
	    
	    ant = input->getBdcBand()->getBdcInput()->getSwitch()->
	      getCurrentChannel()->getAntennaIF()->getAntenna();
	    
	    // It is not an error for inputs that are owned by an astro
	    // band not to be connected to anything.  Just continue in
	    // this case
	    
	  } catch(Exception& err) {
	    continue;
	  }
	  
	  WalshColumn& awc = ant->walshCol_;
	  
	  // Only assign a walsh column for this antenna if it isn't
	  // already set for this crate type
	  
	  unsigned walshColNo = awc.getWalshColNo(crate->type_);
	  
	  if(!awc.isSet(crate->type_)) {
	    
	    // Generate the next unique walsh column for this crate
	    
	    walshColNo = getNextUniqueWalshColNo(crate->type_);
	    
	    // Set the walsh column assignment that _would_ be asserted
	    // in this crate.  Do this regardless of subarray ownership,
	    // since the next time we check isSet(crate->type_) on this
	    // antenna, we need it to be true.  Note that this does not
	    // assert the walsh column for this antenna; that is done by
	    // setWalshColNo(walshColNo).
	    
	    awc.setWalshColNo(walshColNo, crate->type_);
	    
	    // If this antenna is owned by the subarray configuring this
	    // astroband, set the antenna's walsh column now
	    
	    if(ant->subarrayId_ == astroBand->subarrayId_) {
	      awc.setWalshColNo(walshColNo);
	    }
	  }  
	  
	  // Assign this walsh function to the crate input, regardless
	  // of whether we assigned it to the antenna.  Ie, we want to
	  // set up correlator crate inputs _as if_ the antennas are all
	  // present, even if they are not, so that if an antenna is
	  // added back into this configuration, only its walsh column
	  // has to change.
	  
	  cwc.setWalshColNo(walshColNo);
	  
	  // And mark the associated walsh column as in-use for this
	  // correlator type.
	  
	  addToWalshColumnMask(cwc, crate->type_);
	  
	  // And propagate it to any other inputs to which this antenna
	  // is connected
	  
	  propagateWalshColumnAssignment(ant, walshColNo);
	}
      }
    }
  }
}

void SignalPathMap::assignFreeWalshColumnsToBands()
{

  // Iterate through all correlator crate inputs, assigning free walsh
  // functions to any unassigned antennas.
  
  for(unsigned iBand=0; iBand < corrBands_.size(); iBand++) {
    CorrelatorBand* band = corrBands_[iBand];
    
    if(!(band->type_ & CORR_C3G))
      continue;
    
    for(unsigned iInput=0; iInput < band->inputs_.size(); iInput++) {
      
      CorrelatorBandInput* input = band->inputs_[iInput];
      WalshColumn&            cwc = input->walshCol_;
      
      // We only care about Walsh functions for inputs corresponding
      // to configured astro bands, and only inputs whose walsh
      // columns aren't already assigned.
      
      try {
	
	AstroBandInput* astroBandInput = input->getAstroBandInput();
	
	if(astroBandInput->isUsedByConfiguredAstroBand() && !cwc.isSet()) {
	  
	  Antenna*   ant       = 0;
	  AstroBand* astroBand = astroBandInput->band_;
	  
	  // Get the antenna to which this input is currently connected
	  
	  ant = input->getAntennaIF()->getAntenna();
	  
	  WalshColumn& awc = ant->walshCol_;
	  
	  // Only assign a walsh column for this antenna if it isn't
	  // already set for this band type
	  
	  unsigned walshColNo = awc.getWalshColNo(band->type_);
	  
	  if(!awc.isSet(band->type_)) {
	    
	    // Generate the next unique walsh column for this band
	    
	    walshColNo = getNextUniqueWalshColNo(band->type_);
	    
	    // Set the walsh column assignment that _would_ be asserted
	    // in this band.  Do this regardless of subarray ownership,
	    // since the next time we check isSet(band->type_) on this
	    // antenna, we need it to be true.  Note that this does not
	    // assert the walsh column for this antenna; that is done by
	    // setWalshColNo(walshColNo).
	    
	    awc.setWalshColNo(walshColNo, band->type_);
	    
	    // If this antenna is owned by the subarray configuring this
	    // astroband, set the antenna's walsh column now
	    
	    if(ant->subarrayId_ == astroBand->subarrayId_) {
	      awc.setWalshColNo(walshColNo);
	    }
	  }  
	  
	  // Assign this walsh function to the band input, regardless
	  // of whether we assigned it to the antenna.  Ie, we want to
	  // set up correlator band inputs _as if_ the antennas are all
	  // present, even if they are not, so that if an antenna is
	  // added back into this configuration, only its walsh column
	  // has to change.
	  
	  cwc.setWalshColNo(walshColNo);
	  
	  // And mark the associated walsh column as in-use for this
	  // correlator type.
	  
	  addToWalshColumnMask(cwc, band->type_);
	  
	  // And propagate it to any other inputs to which this antenna
	  // is connected
	  
	  propagateWalshColumnAssignment(ant, walshColNo);
	}
	
	// It is not an error for inputs that are owned by an astro
	// band not to be connected to anything.  Just continue in
	// this case
	
      } catch(Exception& err) {
	continue;
      }
    }
  }
}

void SignalPathMap::clearWalshColumns(CorrelatorType type)
{
  clearWalshColumnsFromCrates(type);
  clearWalshColumnsFromBands(type);
}

void SignalPathMap::clearWalshColumnsFromCrates(CorrelatorType type)
{
  // Clear the mask of used walsh functions

  clearWalshColumnMask(type);

  // Clear correlator crate input assignments

  for(unsigned iCrate=0; iCrate < crates_.size(); iCrate++) {
    CorrelatorCrate* crate = crates_[iCrate];

    const util::CorrelatorSet corrset(type);
    //if(type == CORR_ANY || type == crate->type_) {
    if ( corrset.includes(crate->type_) ) {
      for(unsigned iInput=0; iInput < crate->inputs_.size(); iInput++) {
	CorrelatorCrateInput* input = crate->inputs_[iInput];

	// We only care about Walsh functions for inputs corresponding
	// to configured astro bands

	if(input->isConnectedToAstroBand()) {

	  AstroBandInput* astroBandInput = input->getAstroBandInput();

	  if(astroBandInput->isUsedByConfiguredAstroBand()) {
	
	    try {
	      
	      // Get the antenna to which this input is currently connected
	      
	      Antenna* ant = input->getBdcBand()->getBdcInput()->getSwitch()->
		getCurrentChannel()->getAntennaIF()->getAntenna();
	      
	      // And clear the walsh column assignments for both
	      
	      input->walshCol_.clear();
	      ant->walshCol_.clear();
	      
	    } catch(Exception& err) {
	      continue;
	    }
	  }
	}
      }
    }
  }
}

void SignalPathMap::clearWalshColumnsFromBands(CorrelatorType type)
{
  // Clear the mask of used walsh functions

  clearWalshColumnMask(type);

  // Clear correlator crate input assignments

  for(unsigned iBand=0; iBand < corrBands_.size(); iBand++) {
    CorrelatorBand* band = corrBands_[iBand];

    if(!(type & band->type_))
      continue;

    //if(type == CORR_ANY || type == band->type_) {
    const util::CorrelatorSet corrset(type);
    if( corrset.includes( band->type_ ) ) {

      for(unsigned iInput=0; iInput < band->inputs_.size(); iInput++) {

	CorrelatorBandInput* input = band->inputs_[iInput];
	
	// We only care about Walsh functions for inputs corresponding
	// to configured astro bands
	
	try {

	  AstroBandInput* astroBandInput = input->getAstroBandInput();
	  if(astroBandInput->isUsedByConfiguredAstroBand()) {
	    
	    // Get the antenna to which this input is currently connected
	    
	    Antenna* ant = input->getAntennaIF()->getAntenna();
	    
	    // And clear the walsh column assignments for both
	    
	    input->walshCol_.clear();
	    ant->walshCol_.clear();
	  }

	} catch(Exception& err) {
	  continue;
	}
      }
    }
  }
}


/**.......................................................................
 * Reconfigure walsh columns for the current astro band configuration
 *
 * First we clear all walsh column assignments, then assign walsh
 * columns that are 'hardwired', either by correlator hardware, or by
 * request of a specific walsh column for a given antenna.  If those
 * steps succeed, we assign remaining Walsh columns from the set of
 * walsh columns available.
 */
void SignalPathMap::configureWalshColumns()
{
  clearWalshColumns();
  assignHardwiredWalshColumns();
  assignFreeWalshColumns();
}

/**.......................................................................
 * Return true if the requested switch position can be asserted
 * without conflict.
 *
 * We return true if the switch is not currently in use, or if the
 * requested switch position is the position currently asserted.
 */
bool SignalPathMap::canAssertSwitchPosition(SwitchSetting swSet)
{
  Switch* sw = getSwitch(swSet.switchNo_);
  return (sw->isOwnedByNoone() || swSet.channel_ == sw->currentChannel_->channelId_);
}

/**.......................................................................
 * Mark an antenna as belonging to the passed subarray
 */
void SignalPathMap::addAntenna(unsigned antNo, SubarrayId subarrayId) 
{
  Antenna* ant = getAntenna(antNo);

  if((ant->subarrayId_ == SA_NONE) || (ant->subarrayId_ == subarrayId)) {
    ant->subarrayId_ = subarrayId;
    COUT("Adding antenna: " << *ant << " to subarray " << subarrayId);
  } else {
    ThrowColorError("Antenna C" << antNo << " is currently owned by another subarray (" 
		    << ant->subarrayId_ << ")" << std::endl
		    << "You must first use removeAntenna() in that subarray before you can add "
		    "it to this one", "red");
  }

  // If an antenna was successfully added, recalculate walsh function
  // assignments

  configureWalshColumns();
}

/**.......................................................................
 * Mark an antenna as no longer belonging to the passed subarray
 */
void SignalPathMap::removeAntenna(unsigned antNo, SubarrayId subarrayId) 
{
  Antenna* ant = getAntenna(antNo);

  if((ant->subarrayId_ != SA_NONE) && (ant->subarrayId_ != subarrayId)) {
    ThrowColorError("Antenna C" << antNo << " is currently owned by another subarray (" 
		    << ant->subarrayId_ << ")" << std::endl
		    << "You don't have permission to remove this antenna", "red");
  }

  ant->subarrayId_ = SA_NONE;

  // If an antenna was successfully removed, recalculate walsh function
  // assignments

  configureWalshColumns();
}

/**.......................................................................
 * Method by which a subarray will add a correlator to the set which
 * it controls
 */
void SignalPathMap::addCorrelator(CorrelatorType type, SubarrayId subarrayId)
{
  if(correlatorMap_.find(type) == correlatorMap_.end()) {
    ThrowColorError("No correlator corresponds to " << type, "red");
  }

  Correlator* corr = correlatorMap_[type];
  corr->requestToControl(subarrayId);
}

/**.......................................................................
 * Method by which a subarray will remove a correlator from the set which
 * it controls
 */
bool SignalPathMap::removeCorrelator(CorrelatorType type, SubarrayId subarrayId)
{
  if(correlatorMap_.find(type) == correlatorMap_.end()) {
    ThrowColorError("No correlator corresponds to " << type, "red");
  }

  Correlator* corr = correlatorMap_[type];
  bool released = corr->requestToRelease(subarrayId);

  // If this request actually resulted in a change of ownership for
  // this correlator, clear any astrobands that are currently
  // configured for this correlator

  if(released) {
    clearAstroBandConfiguration(0, subarrayId, type);
  }

  return released;
}

//-----------------------------------------------------------------------
// Methods of the WalshColumn class
//-----------------------------------------------------------------------

WalshColumn::WalshColumn()
{
  walshColNo_                = 0;
  hardwired_                 = false;
  corrWalshColMap_[CORR_SL]  = 0;
  corrWalshColMap_[CORR_WB]  = 0;
  corrWalshColMap_[CORR_C3G] = 0;
}

void WalshColumn::setHardwired(bool hardwired)
{
  hardwired_ = hardwired;
}

void WalshColumn::validateWalshColNo(unsigned walshColNo)
{
  if(walshColNo > nWalshCol_) {
    ThrowColorError("Invalid walsh column number: " << walshColNo
	       << " should be 1-" << nWalshCol_, "red");
  }
}

void WalshColumn::setWalshColNo(unsigned walshColNo, bool override)
{
  validateWalshColNo(walshColNo);

  if(!isHardwired() || override) {
    walshColNo_ = walshColNo;
    setHardwired(override);
  }
}

unsigned WalshColumn::getWalshColNo()
{
  return walshColNo_;
}

bool WalshColumn::isSet(CorrelatorType type)
{
  if(corrWalshColMap_.find(type) != corrWalshColMap_.end()) {
    return corrWalshColMap_[type] > 0;
  }

  return false;
}

void WalshColumn::setWalshColNo(unsigned walshColNo, CorrelatorType type)
{
  corrWalshColMap_[type] = walshColNo;
}

unsigned WalshColumn::getWalshColNo(CorrelatorType type)
{
  if(corrWalshColMap_.find(type) == corrWalshColMap_.end()) {
    ThrowColorError("No walsh column has been assigned for correlator: " << type, "red");
  }

  return corrWalshColMap_[type];
}

bool WalshColumn::isHardwired()
{
  return hardwired_;
}

void WalshColumn::clear()
{
  setWalshColNo(0);
  setWalshColNo(0, CORR_SL);
  setWalshColNo(0, CORR_WB);
  setWalshColNo(0, CORR_C3G);
}

bool WalshColumn::isSet()
{
  return walshColNo_ != 0;
}

bool WalshColumn::operator==(WalshColumn& wc)
{
  return walshColNo_ == wc.walshColNo_;
}

bool WalshColumn::operator!=(WalshColumn& wc)
{
  return walshColNo_ != wc.walshColNo_;
}

void WalshColumn::operator=(WalshColumn& wc)
{
  walshColNo_ = wc.walshColNo_;
  hardwired_  = wc.hardwired_;
}

//-----------------------------------------------------------------------
// Globals
//-----------------------------------------------------------------------

std::ostream& carma::signalpath::operator<<(std::ostream& os, const Antenna& ant)
{
  os << "C" << ant.antNo_ << ": " << std::endl;

  std::map<SplitterChannelId, AntennaIF*>::const_iterator ifMapIter = ant.ifMap_.begin();

  for(; ifMapIter != ant.ifMap_.end(); ifMapIter++) {
    os << *(ifMapIter->second) << std::endl;
  }

  return os;
}

std::ostream&  carma::signalpath::operator<<(std::ostream& os, const AntennaIF& antIf)
{
  os << "Polarization: " << antIf.polType_ << " SP CHAN: " << antIf.splitterChannel_;

  ConnectableNode* toNode = ((ConnectableNode&)(antIf)).toNode_;
  if(toNode == 0) {
    os << "--> NONE";
  } else {
    os << *toNode;
  }

  return os;
}

std::ostream& carma::signalpath::operator<<(std::ostream& os, const AntennaType& type)
{
  switch (type) {
  case ANT_NONE:
    os << "NONE";
    break;
  case ANT_SZA:
    os << "SZA";
    break;
  case ANT_BIMA:
    os << "BIMA";
    break;
  case ANT_OVRO:
    os << "OVRO";
    break;
  default:
    os << "UNKNOWN";
    break;
  }

  return os;
}

std::ostream& carma::signalpath::operator<<(std::ostream& os, const CorrelatorType& type)
{
  switch (type) {
  case CORR_NONE:
    os << setw(5) << "NONE";
    break;
  case CORR_SL:
    os << setw(5) << "SL";
    break;
  case CORR_WB:
    os << setw(5) << "WB";
    break;
  case CORR_C3GMAX8:
    os << setw(5) << "C3GMAX8";
    break;
  case CORR_C3GMAX23:
    os << setw(5) << "C3GMAX23";
    break;
  default:
    os << "UNKNOWN";
    break;
  }

  return os;
}

std::ostream& carma::signalpath::operator<<(std::ostream& os, const BlockDownconverterInput& input)
{
  os << input.type_;
  return os;
}

std::ostream& carma::signalpath::operator<<(std::ostream& os, const BlockDownconverterInputType& type)
{
  switch (type) {
  case BD_INP_NONE:
    os << setw(4) << "NONE";
    break;
  case BD_INP_P1:
    os << setw(4) << "P1";
    break;
  case BD_INP_P2:
    os << setw(4) << "P2";
    break;
  default:
    os << "UNKNOWN";
    break;
  }

  return os;
}

std::ostream&  carma::signalpath::operator<<(std::ostream& os, const PolarizationType& type)
{
  switch (type) {
  case POL_NONE:
    os << setw(4) << std::left << "NONE";
    break;
  case POL_LEFT:
    os << setw(4) << std::left << "L";
    break;
  case POL_RIGHT:
    os << setw(4) << std::left << "R";
    break;
  default:
    os << "UNKNOWN";
    break;
  }

  return os;
}

std::ostream&  carma::signalpath::operator<<(std::ostream& os, const SplitterChannelId& id)
{
  switch (id) {
  case SP_CHAN_NONE:
    os << setw(4) << "NONE";
    break;
  case SP_CHAN_A:
    os << setw(4) << "A";
    break;
  case SP_CHAN_B:
    os << setw(4) << "B";
    break;
  case SP_CHAN_C:
    os << setw(4) << "C";
    break;
  case SP_CHAN_D:
    os << setw(4) << "D";
    break;
  default:
    os << "UNKNOWN";
    break;
  }

  return os;
}

std::ostream& carma::signalpath::operator<<(std::ostream& os, const Switch& sw)
{
  os << " BD " << setw(2) << sw.bdcInput_->bdc_->bdcNo_ << ": " << sw.bdcInput_->type_;
  os << "[";

  BlockDownconverterInput* bdcInput = sw.bdcInput_;
  for(unsigned i=0; i < bdcInput->bands_.size(); i++) {
    Band* band = sw.bdcInput_->bands_[i];
    os << band->bandNo_;
    if(i < bdcInput->bands_.size()-1) {
      os << ",";
    }
  }

  os << "]";

  return os;

}

std::ostream& carma::signalpath::operator<<(std::ostream& os, const SwitchChannel& swChan)
{
  XtermManip xtm;

  os << "SW " << setw(5) << std::left << swChan.switch_->name_ << " CHAN " << swChan.channelId_;
  os << " (";

  if(&swChan == swChan.switch_->currentChannel_) {
    os << xtm.textMode("bold") 
       << setw(strlen("NOT SELECTED")) << "SELECTED";
  } else {
    os << xtm.textMode("normal")
       << setw(strlen("NOT SELECTED")) << "NOT SELECTED";
  }

  os << ") --> "<< *swChan.switch_;

  return os;
}

std::ostream&  carma::signalpath::operator<<(std::ostream& os, const SwitchChannelId& id)
{
  switch (id) {
  case SW_CHAN_NONE:
    os << setw(4) << "NONE";
    break;
  case SW_CHAN_1:
    os << setw(4) << "1";
    break;
  case SW_CHAN_2:
    os << setw(4) << "2";
    break;
  case SW_CHAN_3:
    os << setw(4) << "3";
    break;
  case SW_CHAN_4:
    os << setw(4) << "4";
    break;
  default:
    os << "UNKNOWN";
    break;
  }

  return os;
}

std::ostream& carma::signalpath::operator<<(std::ostream& os, const SignalPathMap& swMap)
{
  for(unsigned iAnt=0; iAnt < swMap.antennas_.size(); iAnt++) {
    os << *swMap.antennas_[iAnt] << std::endl;
  }

  return os;
}

std::ostream& carma::signalpath::operator<<(std::ostream& os, const WalshColumn& wc)
{
  os << wc.walshColNo_;
  return os;
}

std::ostream& carma::signalpath::operator<<(std::ostream& os, ConnectableNode& node)
{
  // Enumerate here what ConnectableNode can be:
  
  SwitchChannel* swChan    = dynamic_cast<SwitchChannel*>(&node);
  if(swChan) {
    os << *swChan;
  }
  
  AntennaIF* antIF         = dynamic_cast<AntennaIF*>(&node);
  if(antIF) {
    os << *antIF;
  }
  
  Digitizer* dig           = dynamic_cast<Digitizer*>(&node);
  if(dig) {
    os << *dig;
  }
  
  AstroBandInput* abInput  = dynamic_cast<AstroBandInput*>(&node);
  if(abInput) {
    os << *abInput;
  }

  return os;
}

std::ostream& carma::signalpath::operator<<(std::ostream& os, const AstroBandInput& input)
{
  os << "AB" << input.band_->bandNo_ << ":" << input.inputNo_;
  return os;
}

std::ostream& carma::signalpath::operator<<(std::ostream& os, const CorrelatorBandInput& input)
{
  os << input.band_->name_ << ":" << input.inputNo_;
  return os;
}

std::ostream& carma::signalpath::operator<<(std::ostream& os, const Digitizer& dig)
{
  os << dig.name_;
  return os;
}

std::ostream& carma::signalpath::operator<<(std::ostream& os, const FpgaBoard& brd)
{
  Digitizer* dig = 0;

  if(brd.fromNode_)
    dig = dynamic_cast<Digitizer*>(brd.fromNode_);

  if(dig) {
    os << dig->name_ << ":" << brd.name_;
  } else {
    os << "(no owner):" << brd.name_;
  }

  return os;
}

//-----------------------------------------------------------------------
// ConnectableNode interface
//-----------------------------------------------------------------------

 ConnectableNode::ConnectableNode()
 {
   toNode_   = 0;
   fromNode_ = 0;
 }

 ConnectableNode::~ConnectableNode()
 {
 }

 void ConnectableNode::clearFrom(ConnectableNode* caller, CorrelatorType type)
 {
   fromNode_ = 0;
 }

 void ConnectableNode::clearTo(ConnectableNode* caller, CorrelatorType type)
 {
   toNode_ = 0;
 }

 void ConnectableNode::mapFrom(ConnectableNode* caller, CorrelatorType type)
 {
   if(fromNode_ != caller) 
     fromNode_ = caller;
 }

 void ConnectableNode::mapTo(ConnectableNode* caller, CorrelatorType type)
 {
   if(toNode_ != caller)
     toNode_ = caller;
 }

