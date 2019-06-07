#include "carma/antenna/sza/antenna/corba/AntennaInitializer.h"
#include "carma/antenna/sza/antenna/corba/AntennaProxy.h"
#include "carma/antenna/sza/antenna/corba/DriveProxy.h"
#include "carma/antenna/sza/antenna/corba/LOProxy.h"
#include "carma/antenna/sza/antenna/corba/IFProxy.h"
#include "carma/antenna/sza/antenna/corba/FrontEndProxy.h"
#include "carma/antenna/sza/antenna/corba/RxProxy.h"
#include "carma/antenna/sza/antenna/corba/RxSelector.h"

#include "carma/antenna/sza/antenna/canbus/Receiver.h"

#include "carma/szautil/LoOsc.h"
#include "carma/szautil/TimeVal.h"

#include "carma/szautil/Program.h"

#include "carma/services/AstroTime.h"

#include <time.h>

using namespace std;

using namespace sza::antenna::corba;
using namespace sza::antenna::canbus;

/**.......................................................................
 * Constructor.
 */
AntennaInitializer::AntennaInitializer(AntennaProxy* antenna) 
{
  antenna_ = antenna;
  iAnt_    = antenna->getAnt()->getIntId();
  initialize();
}

/**.......................................................................
 * Destructor.
 */
AntennaInitializer::~AntennaInitializer() {}

/**.......................................................................
 * External call to initialize the antenna to default settings
 */
void AntennaInitializer::initializeAntenna()
{
  //------------------------------------------------------------
  // site -118:08:29.91261, 37:16:49.36981, 2196.265 (szaArrayControl.init)
  //------------------------------------------------------------

  antenna_->drive_->setSite(longitude_, latitude_, altitude_);
  wait();
  
  //------------------------------------------------------------
  // In the same order as in pointing.init
  //------------------------------------------------------------

  antenna_->drive_->setEncoderCountsPerTurn(encoderAzCountsPerTurn_,  encoderElCountsPerTurn_);
  wait();

  antenna_->drive_->setEncoderLimits(encoderAzMinLimit_[iAnt_], encoderAzMaxLimit_[iAnt_], 
				     encoderElMinLimit_[iAnt_], encoderElMaxLimit_[iAnt_]);
  wait();

  antenna_->drive_->setCollimation(sza::util::PointingMode::OPTICAL, opticalCollimationX_[iAnt_], opticalCollimationY_[iAnt_]);
  wait();
  antenna_->drive_->setFlexure(sza::util::PointingMode::OPTICAL, opticalFlexureSin_[iAnt_], opticalFlexureCos_[iAnt_]);
  wait();

  antenna_->drive_->setEncoderZeros(encoderAzZero_[iAnt_], encoderElZero_[iAnt_]);
  wait();

  antenna_->drive_->setTilts(haTilt_[iAnt_], latTilt_[iAnt_], elTilt_[iAnt_]);
  wait();

  antenna_->drive_->setFlexure(sza::util::PointingMode::RADIO,     radioFlexureSin_[iAnt_],   radioFlexureCos_[iAnt_]);
  wait();
  antenna_->drive_->setCollimation(sza::util::PointingMode::RADIO, radioCollimationX_[iAnt_], radioCollimationY_[iAnt_]);
  wait();

  antenna_->drive_->selectAperture(carma::antenna::common::DriveControl::RADIO1CM);
  wait();

  //------------------------------------------------------------
  // locations.init
  //------------------------------------------------------------

  // Now that CARMA is initializing the antennas, we don't want to
  // initialize internal UEN locations.  The CARMA control system
  // commands latitude + longitude that already include the pad
  // offsets.

#if 0
  antenna_->drive_->setLocation(up_[iAnt_], east_[iAnt_], north_[iAnt_]);
  wait();
#endif

  //------------------------------------------------------------
  // lo.init
  //------------------------------------------------------------

#if 0
  antenna_->rxSelector_->getRxProxy()->getLOProxy()->setLoopGainResistance(sza::util::LoOsc::YIG,      yigLoopGainResistance_[iAnt_]);
  wait();

  if(hasDampingGainResistance_[iAnt_]) {
    antenna_->rxSelector_->getRxProxy()->getLOProxy()->setDampingGainResistance(dampingGainResistance_[iAnt_]);
    wait();
  }

  if(hasVaractorLoopGainResistance_[iAnt_]) {
    antenna_->rxSelector_->getRxProxy()->getLOProxy()->setLoopGainResistance(sza::util::LoOsc::VARACTOR, varactorLoopGainResistance_[iAnt_]);
    wait();
  }
#endif

  antenna_->rxSelector_->getRxProxy()->getLOProxy()->setDefaultLOTermAtten(defaultLOTermAtten_[iAnt_]);
  wait();

  antenna_->rxSelector_->getRxProxy()->getLOProxy()->setDefaultYigFrequency(defaultYigFrequency_);
  wait();

  antenna_->rxSelector_->getRxProxy()->getLOProxy()->setDefaultGunnVoltage(defaultGunnVoltage_[iAnt_]);
  wait();

  //------------------------------------------------------------
  // caltert.init
  //------------------------------------------------------------

  antenna_->rxSelector_->getRxProxy()->storeEncoderPosition(sza::util::Rx::RX30GHZ, caltert30GHzEncoderPos_[iAnt_]);
  wait();
  antenna_->rxSelector_->getRxProxy()->storeEncoderPosition(sza::util::Rx::RX90GHZ, caltert90GHzEncoderPos_[iAnt_]);
  wait();

  //------------------------------------------------------------
  // rxBias.init
  //------------------------------------------------------------

  for(unsigned iBias=0; iBias < 16; iBias++) {
    unsigned biasIndex = biasOrder_[iBias];
    antenna_->rxSelector_->getRxProxy()->getFrontEndProxy()->setDefaultBias(biasIndex, defaultBiases_[iAnt_][biasIndex]);
    wait();
  }

  // EML: New as of 20 August 2013.  Re-initialize from cmDewars.tab
  // and mmDewars.tab.  I still perform the internal initialization
  // above to make sure that there are sensible default values in case
  // the dewars file is not available for some reason, but if present,
  // these will override the internal values we just set

  initializeBiasesFromConfigurationFile();

  //------------------------------------------------------------
  // ifAtten.init
  //------------------------------------------------------------

  antenna_->rxSelector_->getRxProxy()->getIFProxy()->setDefaultAtten(default30GHzIFAttenSky_[iAnt_],  sza::util::Rx::RX30GHZ, sza::util::CalPos::SKY);
  wait();
  antenna_->rxSelector_->getRxProxy()->getIFProxy()->setDefaultAtten(default30GHzIFAttenLoad_[iAnt_], sza::util::Rx::RX30GHZ, sza::util::CalPos::AMBIENT);
  wait();

  antenna_->rxSelector_->getRxProxy()->getIFProxy()->setDefaultAtten(default90GHzIFAttenSky_[iAnt_],  sza::util::Rx::RX90GHZ, sza::util::CalPos::SKY);
  wait();
  antenna_->rxSelector_->getRxProxy()->getIFProxy()->setDefaultAtten(default90GHzIFAttenLoad_[iAnt_], sza::util::Rx::RX90GHZ, sza::util::CalPos::AMBIENT);
  wait();

  //------------------------------------------------------------
  // selectRx rx1cm, ant=all (szaTranslator.init)
  //------------------------------------------------------------

  antenna_->rxSelector_->selectRx(sza::util::Rx::RX30GHZ);
  wait();

  antenna_->drive_->stop();
  wait();

  //------------------------------------------------------------
  // Send an update of the equation of the equinoxes and UT1-UTC
  // ephemeris
  //------------------------------------------------------------

  sza::util::TimeVal time;
  time.setToCurrentTime();

  carma::services::AstroTime astroTime;

  time.incrementSeconds(-30.0);
  for(unsigned i=0; i < 3; i++) {

    // Set the equation of the equinoxes in radians

    antenna_->drive_->setEqnEqx(time.getMjd(), astroTime.eqnEqx(time.getMjd())/(24*3600) * 2*M_PI);
    antenna_->drive_->setUt1Utc(time.getMjd(), astroTime.ut1Utc(time.getMjd()));
    time.incrementSeconds(+30.0);
  }
  wait();

  //------------------------------------------------------------
  // And halt the antenna
  //------------------------------------------------------------

  antenna_->drive_->stop();
  wait();
}

void AntennaInitializer::reloadBiasTables()
{
  initializeBiases();
  initializeBiasesFromConfigurationFile();
}

void AntennaInitializer::initialize()
{
  initializeBiases();
  initializeArrays();
}

void AntennaInitializer::initializeBiases()
{
  initializeBiasTables();
  initializeBiasConversionFactors();
  initializeBiasIndices();
}

/**.......................................................................
 * Initialize arrays to default values
 */
void AntennaInitializer::initializeArrays()
{
  latitude_.setDegrees("37:16:49.36981"); 
  longitude_.setDegrees("-118:08:29.91261");
  altitude_.setMeters(2196.265);

  // Set the default YIG frequency.  Note that this is different for CARMA than for SZA (11.963 GHz vs. 9 GHz)

  defaultYigFrequency_.setMHz(11963);

  // Set up encoder counts per turn for all antennas

  encoderAzCountsPerTurn_ = 16777216;
  encoderElCountsPerTurn_ = 16777216;

  // Set up limits for all antennas

  encoderAzMinLimit_.resize(sza::util::AntNum::NANT);
  encoderAzMaxLimit_.resize(sza::util::AntNum::NANT);
  encoderElMinLimit_.resize(sza::util::AntNum::NANT);
  encoderElMaxLimit_.resize(sza::util::AntNum::NANT);
  
  encoderAzMinLimit_[0] = 372140; encoderAzMaxLimit_[0] = 22555426; encoderElMinLimit_[0] = 616615; encoderElMaxLimit_[0] = 4018673;
  encoderAzMinLimit_[1] = 499664; encoderAzMaxLimit_[1] = 22682865; encoderElMinLimit_[1] = 799242; encoderElMaxLimit_[1] = 4013659;
  encoderAzMinLimit_[2] = 209900; encoderAzMaxLimit_[2] = 22393120; encoderElMinLimit_[2] = 625477; encoderElMaxLimit_[2] = 3967397;
  encoderAzMinLimit_[3] = 359168; encoderAzMaxLimit_[3] = 22542371; encoderElMinLimit_[3] = 645209; encoderElMaxLimit_[3] = 4047251;
  encoderAzMinLimit_[4] = 235143; encoderAzMaxLimit_[4] = 22418236; encoderElMinLimit_[4] = 716408; encoderElMaxLimit_[4] = 4118450;
  encoderAzMinLimit_[5] = 229350; encoderAzMaxLimit_[5] = 22412574; encoderElMinLimit_[5] = 665606; encoderElMaxLimit_[5] = 4067643;
  encoderAzMinLimit_[6] = 329424; encoderAzMaxLimit_[6] = 22512724; encoderElMinLimit_[6] = 673614; encoderElMaxLimit_[6] = 4075660;
  encoderAzMinLimit_[7] = 641944; encoderAzMaxLimit_[7] = 22825190; encoderElMinLimit_[7] = 784647; encoderElMaxLimit_[7] = 4186686;

  //------------------------------------------------------------
  // Set up the pointing model -- pointing.init
  //------------------------------------------------------------

  opticalCollimationX_.resize(sza::util::AntNum::NANT);
  opticalCollimationY_.resize(sza::util::AntNum::NANT);

  opticalCollimationX_[0].setDegrees(-0.134792); opticalCollimationY_[0].setDegrees(0.000000);
  opticalCollimationX_[1].setDegrees(-0.127827); opticalCollimationY_[1].setDegrees(0.000000);
  opticalCollimationX_[2].setDegrees(-0.225719); opticalCollimationY_[2].setDegrees(0.000000);
  opticalCollimationX_[3].setDegrees(-0.221998); opticalCollimationY_[3].setDegrees(0.000000);
  opticalCollimationX_[4].setDegrees( 0.080215); opticalCollimationY_[4].setDegrees(0.000000);
  opticalCollimationX_[5].setDegrees(-0.317608); opticalCollimationY_[5].setDegrees(0.000000);
  opticalCollimationX_[6].setDegrees(-0.013820); opticalCollimationY_[6].setDegrees(0.000000);
  opticalCollimationX_[7].setDegrees(-0.016012); opticalCollimationY_[7].setDegrees(0.000000);

  opticalFlexureSin_.resize(sza::util::AntNum::NANT);
  opticalFlexureCos_.resize(sza::util::AntNum::NANT);

  opticalFlexureSin_[0].setDegrees(0.013045); opticalFlexureCos_[0].setDegrees(0.000000);
  opticalFlexureSin_[1].setDegrees(0.014561); opticalFlexureCos_[1].setDegrees(0.000000);
  opticalFlexureSin_[2].setDegrees(0.015633); opticalFlexureCos_[2].setDegrees(0.000000);
  opticalFlexureSin_[3].setDegrees(0.016256); opticalFlexureCos_[3].setDegrees(0.000000);
  opticalFlexureSin_[4].setDegrees(0.020532); opticalFlexureCos_[4].setDegrees(0.000000);
  opticalFlexureSin_[5].setDegrees(0.014697); opticalFlexureCos_[5].setDegrees(0.000000);
  opticalFlexureSin_[6].setDegrees(0.015496); opticalFlexureCos_[6].setDegrees(0.000000);
  opticalFlexureSin_[7].setDegrees(0.015779); opticalFlexureCos_[7].setDegrees(0.000000);

  encoderAzZero_.resize(sza::util::AntNum::NANT);
  encoderElZero_.resize(sza::util::AntNum::NANT);

  encoderAzZero_[0].setDegrees(154.877185); encoderElZero_[0].setDegrees(-1.792108);
  encoderAzZero_[1].setDegrees(159.003890); encoderElZero_[1].setDegrees(-1.884911);
  encoderAzZero_[2].setDegrees(151.837765); encoderElZero_[2].setDegrees(-1.671903);
  encoderAzZero_[3].setDegrees(156.310330); encoderElZero_[3].setDegrees(-1.188764);
  encoderAzZero_[4].setDegrees(154.497748); encoderElZero_[4].setDegrees( 0.361511);
  encoderAzZero_[5].setDegrees(153.632297); encoderElZero_[5].setDegrees(-0.682821);
  encoderAzZero_[6].setDegrees(155.661559); encoderElZero_[6].setDegrees(-0.553059);
  encoderAzZero_[7].setDegrees(161.728025); encoderElZero_[7].setDegrees( 1.824035);

  haTilt_.resize(sza::util::AntNum::NANT);
  latTilt_.resize(sza::util::AntNum::NANT);
  elTilt_.resize(sza::util::AntNum::NANT);

  haTilt_[0].setDegrees( 0.001350); latTilt_[0].setDegrees( 0.002184); elTilt_[0].setDegrees( 0.003924);
  haTilt_[1].setDegrees( 0.004257); latTilt_[1].setDegrees(-0.000195); elTilt_[1].setDegrees( 0.002330);
  haTilt_[2].setDegrees(-0.003898); latTilt_[2].setDegrees( 0.000319); elTilt_[2].setDegrees(-0.009471);
  haTilt_[3].setDegrees(-0.011461); latTilt_[3].setDegrees(-0.002093); elTilt_[3].setDegrees(-0.005601);
  haTilt_[4].setDegrees( 0.004377); latTilt_[4].setDegrees( 0.002552); elTilt_[4].setDegrees(-0.006860);
  haTilt_[5].setDegrees( 0.005100); latTilt_[5].setDegrees( 0.003115); elTilt_[5].setDegrees(-0.005105);
  haTilt_[6].setDegrees( 0.004323); latTilt_[6].setDegrees( 0.002761); elTilt_[6].setDegrees( 0.000321);
  haTilt_[7].setDegrees(-0.001910); latTilt_[7].setDegrees( 0.000308); elTilt_[7].setDegrees( 0.001113);

  radioCollimationX_.resize(sza::util::AntNum::NANT);
  radioCollimationY_.resize(sza::util::AntNum::NANT);

  radioCollimationX_[0].setDegrees( 0.043057); radioCollimationY_[0].setDegrees(0.000000);
  radioCollimationX_[1].setDegrees( 0.040429); radioCollimationY_[1].setDegrees(0.000000);
  radioCollimationX_[2].setDegrees(-0.002624); radioCollimationY_[2].setDegrees(0.000000);
  radioCollimationX_[3].setDegrees( 0.031000); radioCollimationY_[3].setDegrees(0.000000);
  radioCollimationX_[4].setDegrees( 0.022252); radioCollimationY_[4].setDegrees(0.000000);
  radioCollimationX_[5].setDegrees( 0.016967); radioCollimationY_[5].setDegrees(0.000000);
  radioCollimationX_[6].setDegrees(-0.000214); radioCollimationY_[6].setDegrees(0.000000);
  radioCollimationX_[7].setDegrees( 0.075013); radioCollimationY_[7].setDegrees(0.000000);

  radioFlexureSin_.resize(sza::util::AntNum::NANT);
  radioFlexureCos_.resize(sza::util::AntNum::NANT);

  radioFlexureSin_[0].setDegrees(-0.015211); radioFlexureCos_[0].setDegrees( 0.000067);
  radioFlexureSin_[1].setDegrees(-0.006226); radioFlexureCos_[1].setDegrees( 0.005725);
  radioFlexureSin_[2].setDegrees(-0.014882); radioFlexureCos_[2].setDegrees(-0.000515);
  radioFlexureSin_[3].setDegrees(-0.016781); radioFlexureCos_[3].setDegrees(-0.003536);
  radioFlexureSin_[4].setDegrees(-0.025855); radioFlexureCos_[4].setDegrees(-0.007170);
  radioFlexureSin_[5].setDegrees(-0.020129); radioFlexureCos_[5].setDegrees(-0.002491);
  radioFlexureSin_[6].setDegrees(-0.019418); radioFlexureCos_[6].setDegrees(-0.004617);
  radioFlexureSin_[7].setDegrees(-0.010982); radioFlexureCos_[7].setDegrees( 0.001601);

  //-----------------------------------------------------------------------
  // From lo.init
  //-----------------------------------------------------------------------

  yigLoopGainResistance_.resize(sza::util::AntNum::NANT);

  yigLoopGainResistance_[0] = 9000;
  yigLoopGainResistance_[1] = 9000;
  yigLoopGainResistance_[2] = 9000;
  yigLoopGainResistance_[3] = 9000;
  yigLoopGainResistance_[4] = 9000;
  yigLoopGainResistance_[5] = 9000;
  yigLoopGainResistance_[6] = 9000;
  yigLoopGainResistance_[7] = 9000;

  dampingGainResistance_.resize(sza::util::AntNum::NANT);
  hasDampingGainResistance_.resize(sza::util::AntNum::NANT);

  for(unsigned iAnt=0; iAnt < sza::util::AntNum::NANT; iAnt++) 
    hasDampingGainResistance_[iAnt] = false;

  dampingGainResistance_[0]    = 380;
  dampingGainResistance_[1]    = 380;
  dampingGainResistance_[6]    = 380;
  hasDampingGainResistance_[0] = true;
  hasDampingGainResistance_[1] = true;
  hasDampingGainResistance_[6] = true;

  varactorLoopGainResistance_.resize(sza::util::AntNum::NANT);
  hasVaractorLoopGainResistance_.resize(sza::util::AntNum::NANT);

  for(unsigned iAnt=0; iAnt < sza::util::AntNum::NANT; iAnt++) 
    hasVaractorLoopGainResistance_[iAnt] = false;
  
  varactorLoopGainResistance_[0] = 1000;
  varactorLoopGainResistance_[2] = 1000;
  hasVaractorLoopGainResistance_[0] = true;
  hasVaractorLoopGainResistance_[2] = true;

  defaultLOTermAtten_.resize(sza::util::AntNum::NANT);
  
  defaultLOTermAtten_[0].setdB(8);
  defaultLOTermAtten_[1].setdB(11);
  defaultLOTermAtten_[2].setdB(14);
  defaultLOTermAtten_[3].setdB(16);
  defaultLOTermAtten_[4].setdB(5);
  defaultLOTermAtten_[5].setdB(15);
  defaultLOTermAtten_[6].setdB(20);
  defaultLOTermAtten_[7].setdB(4);

  defaultGunnVoltage_.resize(sza::util::AntNum::NANT);

  defaultGunnVoltage_[0].setCentiVolts(980);
  defaultGunnVoltage_[1].setCentiVolts(958);
  // EML modifying for D. Glynn testing to default to 9.7 V instead of 9.8 V
  // 26 Apr 2011 EML modifying for D. Glynn testing to default to 9.4 V instead of 9.6 V 
  defaultGunnVoltage_[2].setCentiVolts(940);
  //  defaultGunnVoltage_[2].setCentiVolts(980);
  //  defaultGunnVoltage_[3].setCentiVolts(915);
  defaultGunnVoltage_[3].setCentiVolts(950);
  defaultGunnVoltage_[4].setCentiVolts(940);
  defaultGunnVoltage_[5].setCentiVolts(940);
  defaultGunnVoltage_[6].setCentiVolts(980);

  // 22 Feb 2011: EML modifying for D. Glynn testing to default to 9.5
  // V instead of 9.4 V
  //  defaultGunnVoltage_[7].setCentiVolts(939);
  defaultGunnVoltage_[7].setCentiVolts(950);

  //------------------------------------------------------------
  // Caltert setup: caltert.init
  //------------------------------------------------------------

  caltert30GHzEncoderPos_.resize(sza::util::AntNum::NANT);

  caltert30GHzEncoderPos_[0] = 19720;
  caltert30GHzEncoderPos_[1] = 20900;
  caltert30GHzEncoderPos_[2] = 19560;
  caltert30GHzEncoderPos_[3] = 19548;
  caltert30GHzEncoderPos_[4] = 19895;
  caltert30GHzEncoderPos_[5] = 19730;
  caltert30GHzEncoderPos_[6] = 20085;
  caltert30GHzEncoderPos_[7] = 20054;

  caltert90GHzEncoderPos_.resize(sza::util::AntNum::NANT);

  caltert90GHzEncoderPos_[0] = 23056;
  caltert90GHzEncoderPos_[1] = 24141;
  caltert90GHzEncoderPos_[2] = 22753;
  caltert90GHzEncoderPos_[3] = 22775;
  caltert90GHzEncoderPos_[4] = 22980;
  caltert90GHzEncoderPos_[5] = 22900;
  caltert90GHzEncoderPos_[6] = 23328;
  caltert90GHzEncoderPos_[7] = 23270;

  //------------------------------------------------------------
  // Receiver biases -- ref rxBias.init
  //------------------------------------------------------------

  biasOrder_[0]  =  3;
  biasOrder_[1]  =  7;
  biasOrder_[2]  =  2;
  biasOrder_[3]  =  6;
  biasOrder_[4]  =  1;
  biasOrder_[5]  =  5;
  biasOrder_[6]  =  0;
  biasOrder_[7]  =  4;
  biasOrder_[8]  =  9;
  biasOrder_[9]  = 10;
  biasOrder_[10] = 11;
  biasOrder_[11] = 12;
  biasOrder_[12] = 13;
  biasOrder_[13] = 14;
  biasOrder_[14] = 16;
  biasOrder_[15] = 15;

  defaultBiases_.resize(sza::util::AntNum::NANT);

  for(unsigned iAnt=0; iAnt < sza::util::AntNum::NANT; iAnt++)
    defaultBiases_[iAnt].resize(16);

  //=========================================
  // Antenna 0 - RX#7 "HEKTOR"
  //=========================================

  // 30 GHz bias
  defaultBiases_[0][3] = 1500; // VD4 (mV) 
  defaultBiases_[0][7] =  500; // ID4 (cmA)
  defaultBiases_[0][2] = 1500; // VD3 (mV) 
  defaultBiases_[0][6] =  500; // ID3 (cmA)
  defaultBiases_[0][1] = 1200; // VD2 (mV) 
  defaultBiases_[0][5] =  300; // ID2 (cmA)
  defaultBiases_[0][0] = 1000; // VD1 (mV) 
  defaultBiases_[0][4] =  300; // ID1 (cmA)
	       		 
  //90 GHz bias   	 
  defaultBiases_[0][ 9] =  175; // MMIC1 VG1 (mV)
  defaultBiases_[0][10] =  150; // MMIC1 VG2
  defaultBiases_[0][11] =  175; // MMIC2 VG1
  defaultBiases_[0][12] =  200;	// MMIC2 VG2
  defaultBiases_[0][13] = 1080; // MMIC1 VD  (mV)
  defaultBiases_[0][14] = 1133; // MMIC2 VD  (mV)
  defaultBiases_[0][16] = -410; // IF    VG  (mV)
  defaultBiases_[0][15] = 2390; // IF    VD  (mV)

  //=========================================
  // Antenna 1 - RX#4 "GAMMORRAH"
  //=========================================
  // 30 GHz bias
  defaultBiases_[1][3] = 1500;
  defaultBiases_[1][7] =  500;
  defaultBiases_[1][2] = 1500;
  defaultBiases_[1][6] =  500;
  defaultBiases_[1][1] = 1200;
  defaultBiases_[1][5] =  300;
  defaultBiases_[1][0] = 1000;
  defaultBiases_[1][4] =  300;

  //90 GHz bias
  //defaultBiases_[1][ 9] =  200;
  //defaultBiases_[1][10] =  200;
  //defaultBiases_[1][11] =  175;
  //defaultBiases_[1][12] =  200;
  //defaultBiases_[1][13] =  450;
  //defaultBiases_[1][14] = 1030;
  //defaultBiases_[1][16] = -410;
  //defaultBiases_[1][15] = 1230;

  // EML putting in new bias settings for the Caltech MMICs 20 Jan 2011

  defaultBiases_[1][ 9] =  170;
  defaultBiases_[1][10] =  170;
  defaultBiases_[1][11] =  300;
  defaultBiases_[1][12] =  300;
  defaultBiases_[1][13] =  820;
  defaultBiases_[1][14] = 1030;
  defaultBiases_[1][16] = -440;
  defaultBiases_[1][15] = 2900;

  //=========================================
  // Antenna 2- RX#1 "GUIDO" 
  //=========================================
  // 30 GHz bias
  defaultBiases_[2][3] = 1500;
  defaultBiases_[2][7] =  500;
  defaultBiases_[2][2] = 1500;
  defaultBiases_[2][6] =  500;
  defaultBiases_[2][1] = 1200;
  defaultBiases_[2][5] =  300;
  defaultBiases_[2][0] = 1000;
  defaultBiases_[2][4] =  300;

  //90 GHz bias
  defaultBiases_[2][ 9] =  225;
  defaultBiases_[2][10] =  200;
  defaultBiases_[2][11] =  225;
  defaultBiases_[2][12] =  225;
  defaultBiases_[2][13] =  450;
  defaultBiases_[2][14] = 1325;
  defaultBiases_[2][16] = -440;
  defaultBiases_[2][15] = 1500;


  //=========================================
  // Antenna 3 - RX#3 "SODOM"
  //=========================================
  // 30 GHz bias
  defaultBiases_[3][3] = 1500;
  defaultBiases_[3][7] =  500;
  defaultBiases_[3][2] = 1500;
  defaultBiases_[3][6] =  500;
  defaultBiases_[3][1] = 1200;
  defaultBiases_[3][5] =  300;
  defaultBiases_[3][0] = 1000;
  defaultBiases_[3][4] =  300;

  defaultBiases_[3][ 9] =  175;
  defaultBiases_[3][10] =  175;
  defaultBiases_[3][11] =  325;
  defaultBiases_[3][12] =  300;
  defaultBiases_[3][13] = 1168;
  defaultBiases_[3][14] =  685;
  defaultBiases_[3][16] = -410;
  defaultBiases_[3][15] = 1245;


  //=========================================
  // Antenna 4 - RX#8 "ELVIRA"
  //=========================================
  // 30 GHz bias
  defaultBiases_[4][3] = 1500;
  defaultBiases_[4][7] =  500;
  defaultBiases_[4][2] = 1500;
  defaultBiases_[4][6] =  500;
  defaultBiases_[4][1] = 1200;
  defaultBiases_[4][5] =  300;
  defaultBiases_[4][0] = 1000;
  defaultBiases_[4][4] =  300;

  //90 GHz bias  
  defaultBiases_[4][ 9] =  200;
  defaultBiases_[4][10] =  200;
  defaultBiases_[4][11] =  200;
  defaultBiases_[4][12] =  200;
  defaultBiases_[4][13] =  580;
  defaultBiases_[4][14] = 1753;
  defaultBiases_[4][16] = -439;
  defaultBiases_[4][15] = 2929;

  //=========================================
  // Antenna 5 - RX#2 "RINTI"
  //=========================================
  // 30 GHz bias
  defaultBiases_[5][3] = 1500;
  defaultBiases_[5][7] =  500;
  defaultBiases_[5][2] = 1500;
  defaultBiases_[5][6] =  500;
  defaultBiases_[5][1] = 1200;
  defaultBiases_[5][5] =  300;
  defaultBiases_[5][0] = 1000;
  defaultBiases_[5][4] =  300;

  //90 GHz bias  
  defaultBiases_[5][ 9] =  200;
  defaultBiases_[5][10] =  225;
  defaultBiases_[5][11] =  225;
  defaultBiases_[5][12] =  225;
  defaultBiases_[5][13] =  643;
  defaultBiases_[5][14] = 1400;
  defaultBiases_[5][16] = -440;
  defaultBiases_[5][15] = 1500;

  //=========================================
  // Antenna 6 - RX#6 "BASTIAN"
  //=========================================
  // 30 GHz bias
  defaultBiases_[6][3] = 1500;
  defaultBiases_[6][7] =  500;
  defaultBiases_[6][2] = 1500;
  defaultBiases_[6][6] =  500;
  defaultBiases_[6][1] = 1200;
  defaultBiases_[6][5] =  300;
  defaultBiases_[6][0] = 1000;
  defaultBiases_[6][4] =  300;

  //90 GHz bias
  defaultBiases_[6][ 9] =  200;
  defaultBiases_[6][10] =  200;
  defaultBiases_[6][11] =  275;
  defaultBiases_[6][12] =  275;
  defaultBiases_[6][13] =  650;
  defaultBiases_[6][14] =  670;
  defaultBiases_[6][16] = -410;
  defaultBiases_[6][15] = 1650;

  //=========================================
  // Antenna 7 - RX#7 "JEZEBEL"
  //=========================================
  // 30 GHz bias
  defaultBiases_[7][3] = 1500;
  defaultBiases_[7][7] =  500;
  defaultBiases_[7][2] = 1500;
  defaultBiases_[7][6] =  500;
  defaultBiases_[7][1] = 1200;
  defaultBiases_[7][5] =  300;
  defaultBiases_[7][0] = 1000;
  defaultBiases_[7][4] =  300;

  // 90 GHz bias  
  defaultBiases_[7][ 9] =  225;
  defaultBiases_[7][10] =  225;
  defaultBiases_[7][11] =  275;
  defaultBiases_[7][12] =  275;
  defaultBiases_[7][13] =  550;
  defaultBiases_[7][14] =  724;
  defaultBiases_[7][16] = -440;
  defaultBiases_[7][15] = 2890;

  //------------------------------------------------------------
  // IF Attenuations -- ref ifAtten.init
  //------------------------------------------------------------

  default30GHzIFAttenSky_.resize(sza::util::AntNum::NANT);
  default30GHzIFAttenLoad_.resize(sza::util::AntNum::NANT);
  default90GHzIFAttenSky_.resize(sza::util::AntNum::NANT);
  default90GHzIFAttenLoad_.resize(sza::util::AntNum::NANT);

  //========================================
  // 30GHz default Attens
  //========================================

  default30GHzIFAttenSky_[0].setdB(13);
  default30GHzIFAttenSky_[1].setdB(8);
  default30GHzIFAttenSky_[2].setdB(10);
  default30GHzIFAttenSky_[3].setdB(0);
  default30GHzIFAttenSky_[4].setdB(0);
  default30GHzIFAttenSky_[5].setdB(0);
  default30GHzIFAttenSky_[6].setdB(10);
  default30GHzIFAttenSky_[7].setdB(0);

  default30GHzIFAttenLoad_[0].setdB(22);
  default30GHzIFAttenLoad_[1].setdB(17);
  default30GHzIFAttenLoad_[2].setdB(19);
  default30GHzIFAttenLoad_[3].setdB(9);
  default30GHzIFAttenLoad_[4].setdB(9);
  default30GHzIFAttenLoad_[5].setdB(9);
  default30GHzIFAttenLoad_[6].setdB(19);
  default30GHzIFAttenLoad_[7].setdB(9);

  //========================================
  // 90GHz default Attens
  //========================================

  default90GHzIFAttenSky_[0].setdB(12);
  default90GHzIFAttenSky_[1].setdB(2);
  default90GHzIFAttenSky_[2].setdB(6);
  default90GHzIFAttenSky_[3].setdB(13);
  default90GHzIFAttenSky_[4].setdB(12);
  default90GHzIFAttenSky_[5].setdB(18);
  default90GHzIFAttenSky_[6].setdB(6);
  default90GHzIFAttenSky_[7].setdB(12);

  default90GHzIFAttenLoad_[0].setdB(18);
  default90GHzIFAttenLoad_[1].setdB(8);
  default90GHzIFAttenLoad_[2].setdB(12);
  default90GHzIFAttenLoad_[3].setdB(19);
  default90GHzIFAttenLoad_[4].setdB(18);
  default90GHzIFAttenLoad_[5].setdB(24);
  default90GHzIFAttenLoad_[6].setdB(12);
  default90GHzIFAttenLoad_[7].setdB(18);

  up_.resize(sza::util::AntNum::NANT);
  east_.resize(sza::util::AntNum::NANT);
  north_.resize(sza::util::AntNum::NANT);
  
  up_[0].setMeters(-0.44758); east_[0].setMeters( -5.54988); north_[0].setMeters(-38.99612);
  up_[1].setMeters(-0.28308); east_[1].setMeters( -4.53118); north_[1].setMeters(-27.51975);
  up_[2].setMeters( 0.01613); east_[2].setMeters( 47.91590); north_[2].setMeters(-11.41744);
  up_[3].setMeters( 0.50612); east_[3].setMeters(-25.61721); north_[3].setMeters( 27.75501);
  up_[4].setMeters(-0.26985); east_[4].setMeters(  2.93494); north_[4].setMeters(-26.60022);
  up_[5].setMeters(-0.51279); east_[5].setMeters(  2.12972); north_[5].setMeters(-42.48391);
  up_[6].setMeters(-0.38741); east_[6].setMeters( -8.97398); north_[6].setMeters(-33.43549);
  up_[7].setMeters(-0.48138); east_[7].setMeters(  1.25337); north_[7].setMeters(-35.56000);
}

void AntennaInitializer::wait()
{
  struct timespec ts;

  ts.tv_sec  = 0;
  ts.tv_nsec = 10000000; // 10 ms wait

  nanosleep(&ts, 0);
}

/**.......................................................................
 * Initialize bias tables from configuration files
 */
void AntennaInitializer::initializeBiasTables()
{
  std::ostringstream os;
  os << Program::getConfDir() << "antenna/sza/rx/cmDewars.tab";
  cmBiasTable_.open(os.str());

  os.str("");
  os << Program::getConfDir() << "antenna/sza/rx/mmDewars.tab";
  mmBiasTable_.open(os.str());
}

void AntennaInitializer::initializeBiasConversionFactors()
{
  std::map<std::string, unsigned> conv;

  biasConvFactors_["Vd1"]   = 1000; // Convert from V to mV
  biasConvFactors_["Vd2"]   = 1000; // Convert from V to mV
  biasConvFactors_["Vd3"]   = 1000; // Convert from V to mV
  biasConvFactors_["Vd4"]   = 1000; // Convert from V to mV

  biasConvFactors_["Id1"]   =  100; // Convert from mA to 100 mA
  biasConvFactors_["Id2"]   =  100; // Convert from mA to 100 mA
  biasConvFactors_["Id3"]   =  100; // Convert from mA to 100 mA
  biasConvFactors_["Id4"]   =  100; // Convert from mA to 100 mA

  biasConvFactors_["M1Vg1"] = 1000; // Convert from V to mV
  biasConvFactors_["M1Vg2"] = 1000; // Convert from V to mV
  biasConvFactors_["M2Vg1"] = 1000; // Convert from V to mV
  biasConvFactors_["M2Vg2"] = 1000; // Convert from V to mV

  biasConvFactors_["M1Vd"] = 1000; // Convert from V to mV
  biasConvFactors_["M2Vd"] = 1000; // Convert from V to mV

  biasConvFactors_["IFVg"] = 1000; // Convert from V to mV
  biasConvFactors_["IFVd"] = 1000; // Convert from V to mV
}

void AntennaInitializer::initializeBiasIndices()
{
  biasIndices_["Vd1"]   = Receiver::Amp30GHzRFStage1Vg;
  biasIndices_["Vd2"]   = Receiver::Amp30GHzRFStage2Vg;
  biasIndices_["Vd3"]   = Receiver::Amp30GHzRFStage3Vg;
  biasIndices_["Vd4"]   = Receiver::Amp30GHzRFStage4Vg;

  biasIndices_["Id1"]   = Receiver::Amp30GHzRFStage1Id;
  biasIndices_["Id2"]   = Receiver::Amp30GHzRFStage2Id;
  biasIndices_["Id3"]   = Receiver::Amp30GHzRFStage3Id;
  biasIndices_["Id4"]   = Receiver::Amp30GHzRFStage4Id;

  biasIndices_["M1Vg1"] = Receiver::Amp90GHzRF1Stage1Vg;
  biasIndices_["M1Vg2"] = Receiver::Amp90GHzRF1Stage2Vg;

  biasIndices_["M2Vg1"] = Receiver::Amp90GHzRF2Stage1Vg;
  biasIndices_["M2Vg2"] = Receiver::Amp90GHzRF2Stage2Vg;

  biasIndices_["M1Vd"]  = Receiver::Amp90GHzRF1Vd;
  biasIndices_["M2Vd"]  = Receiver::Amp90GHzRF2Vd;

  biasIndices_["IFVg"]  = Receiver::Amp90GHzIFVg;
  biasIndices_["IFVd"]  = Receiver::Amp90GHzIFVd;
}

/**.......................................................................
 * Initialize CM and MM biases from system configuration files
 */
void AntennaInitializer::initializeBiasesFromConfigurationFile()
{
  //------------------------------------------------------------
  // Set the CM biases
  //------------------------------------------------------------

  setDefaultBias(cmBiasTable_, "Vd1");
  setDefaultBias(cmBiasTable_, "Id1");

  setDefaultBias(cmBiasTable_, "Vd2");
  setDefaultBias(cmBiasTable_, "Id2");

  setDefaultBias(cmBiasTable_, "Vd3");
  setDefaultBias(cmBiasTable_, "Id3");

  setDefaultBias(cmBiasTable_, "Vd4");
  setDefaultBias(cmBiasTable_, "Id4");

  //------------------------------------------------------------
  // Now the MMIC biases
  //------------------------------------------------------------

  setDefaultBias(mmBiasTable_, "M1Vg1");
  setDefaultBias(mmBiasTable_, "M1Vg2");
  setDefaultBias(mmBiasTable_, "M1Vd");

  setDefaultBias(mmBiasTable_, "M2Vg1");
  setDefaultBias(mmBiasTable_, "M2Vg2");
  setDefaultBias(mmBiasTable_, "M2Vd");

  setDefaultBias(mmBiasTable_, "IFVg");
  setDefaultBias(mmBiasTable_, "IFVd");
}

/**.......................................................................
 * Set a single default bias
 */
void AntennaInitializer::setDefaultBias(carma::services::Table& tab, std::string bias)
{
  std::vector<double> dVals = tab.getDoubleColumn(bias);

  short biasValue = (short)(dVals[iAnt_] * biasConvFactors_[bias]);
  unsigned biasIndex = biasIndices_[bias];

  antenna_->rxSelector_->getRxProxy()->getFrontEndProxy()->setDefaultBias(biasIndex, biasValue);
  wait();
}
