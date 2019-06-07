/** @file
 * Implementation of Antenna IF CAN Device.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.21 $
 * $Date: 2012/02/21 21:06:56 $
 * $Id: AntennaIF.cc,v 1.21 2012/02/21 21:06:56 abeard Exp $
 *
 * $CarmaCopyright$
 */

// Carma includes
#include "carma/antenna/bima/AntennaIF.h"
#include "carma/antenna/bima/Rx.h"
#include "carma/antenna/bima/SharedMemory.h"
#include "carma/antenna/common/IFControl.h"
#include "carma/antenna/common/RxTypeInfo.h"
#include "carma/monitor/CanbusCommon.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

using namespace carma::canbus;
using namespace carma::antenna::bima;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

// Oy this is bad
// Should be getting this from IFCanMaster
#define POL1_NODE_ID 1
#define POL2_NODE_ID 2

// -----------------------------------------------------------------------------
AntennaIF::AntennaIF(
    nodeType node,
    CanOutput &io,
    Rx &rx,
    carma::monitor::StateMonitorPointEnum & state,
    carma::monitor::AntennaIF &ifMon,
    carma::monitor::Xac &xacMon)
    :
    carma::antenna::common::AntennaIF(node, io, state, ifMon, xacMon),
    rx_(rx),
    log_(Program::getLogger())
{
    _bimaShm = new SharedMemory( rx_.getConfig().getAntenna().c_str() );
}

// -----------------------------------------------------------------------------
AntennaIF::~AntennaIF()
{
    // Nothing here
}

// -----------------------------------------------------------------------------
void AntennaIF::processFastChannel1Packet(DataVector &data)
{
  if (data.size() != 8)
      throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
          "AntennaIF::processBlankingFramePacket1 - Data size != 8!");

  // CANNOT ASSUME THAT THIS WILL ALWAYS BE IF TOTAL POWER
  // IT COULD BE ONE OF 3 ITEMS SEE API 224 MSGID 0x004
   float ifFastPow = dataToFloat(data);
   int ticks = (int)dataToUshort(data);

   if ( getNode() == POL1_NODE_ID )
   {
     _bimaShm->putData( "IF1CANFASTPOW", &ifFastPow, 1 );
     _bimaShm->putData( "IFCANFASTTICK", &ticks, 1 );
   }
   else if ( getNode() == POL2_NODE_ID )
   {
     _bimaShm->putData( "IF2CANFASTPOW", &ifFastPow, 1 );
     _bimaShm->putData( "IFCANFASTTICK", &ticks, 1 );
   }

   // log_ << Priority::INFO << "Got Fast Ch1 Packet, val=" << laserOptPower
   //   << " val2=" << ticks;
}

// -----------------------------------------------------------------------------
void AntennaIF::processFastChannel2Packet(DataVector &data)
{
  if (data.size() != 8)
    throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
	"AntennaIF::processBlankingFramePacket1 - Data size != 8!");
}

// -----------------------------------------------------------------------------
void AntennaIF::processTotalPowerResponse(DataVector &data)
{
  float ifTotPowermW;
  float ifTotPowerActualV;

  if (data.size() != 8)
    throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
	"AntennaIF::processBlankingFramePacket1 - Data size != 8!");

  // Unpack the data
  ifTotPowermW = dataToFloat(data);
  ifTotPowerActualV = dataToFloat(data);

  updateIFTimer();

  log_ << Priority::INFO << "Got Total Power Response Packet, ifTotPowermW="
    << ifTotPowermW << ", ifTotPowerActualV=" << ifTotPowerActualV;
}

// -----------------------------------------------------------------------------
void AntennaIF::processFastTotalPowerPacket(DataVector &data)
{
     int ticks = (int)dataToUshort(data);
     float ifFastPow = dataToFloat(data);

     // add fast total power point to the Lpow or Rpow array, update npts;
     // copy entire array to shared memory every time a new point arrives,
     // since I don't know how many points will be sent

     if ( getNode() == POL1_NODE_ID ) {
       if (ticks < NMAX) {
         Lpow[ticks-1] = ifFastPow;
         _bimaShm->putData( "LCP_PNPTS", &ticks);
         _bimaShm->putData( "LCPCANPOW", Lpow, NMAX);
       }

     } else if ( getNode() == POL2_NODE_ID ) {
       if (ticks < NMAX) {
         Rpow[ticks-1] = ifFastPow;
         _bimaShm->putData( "RCP_PNPTS", &ticks);
         _bimaShm->putData( "RCPCANPOW", Rpow, NMAX);
       }
     } else {
	log_ << Priority::WARN << "Unknown node " << getNode() << " in processFastTotalPowerPacket";
     }
}

// -----------------------------------------------------------------------------
void AntennaIF::processSwitchStatusOnChange(DataVector &data)
{
  unsigned char switchStat;

  if (data.size() != 8)
    throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
	"AntennaIF::processBlankingFramePacket1 - Data size != 8!");

  // Unpack the data
  switchStat = dataToUbyte(data);

  log_ << Priority::INFO << "Got Switch Status On Change Packet, switchStat="
    << (int)switchStat;
}

// -----------------------------------------------------------------------------
void AntennaIF::processPAMStatusOnChange(DataVector &data)
{
  unsigned char pamStat;
  unsigned char crap;
  float attenSet;

  if (data.size() != 8)
    throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
	"AntennaIF::processBlankingFramePacket1 - Data size != 8!");

  // Unpack the data
  pamStat = dataToUbyte(data);
  crap = dataToUbyte(data);
  crap = dataToUbyte(data);
  crap = dataToUbyte(data);
  attenSet = dataToFloat(data);

  if ( pamStat != 1 ) // Pam is changing, don't store the atten sent, it's 0
  {
    _bimaShm->putData( "IFTOTATTENRP", &attenSet, 1 );
    updateIFTimer();
  }

  log_ << Priority::INFO << "Got PAM Status On Change Packet, pamStat="
    << (int)pamStat << ", attenSet=" << attenSet;

}

// -----------------------------------------------------------------------------
  void AntennaIF::selectRx()
{
  using namespace carma::antenna::common;

  unsigned short band = rx_.getBand() - 'A';
  unsigned short selband = 0;

  if ( band == 0 || band == 3 )
    selband = RxTypeInfo::ifSwitchPositionFromRxType( RxControl::RX1MM );
  else if ( band == 1 )
    selband = RxTypeInfo::ifSwitchPositionFromRxType( RxControl::RX3MM );
  else if ( band == 4 )
    selband = RxTypeInfo::ifSwitchPositionFromRxType( RxControl::RX1CM );

  // Use bima overridden selectBand.
  selectBand( selband );
}

// -----------------------------------------------------------------------------
  void AntennaIF::selectBand( ::CORBA::UShort band )
{
  try
  {
    log_ << Priority::INFO
      << "(AntennaIF) IFControlImpl::selectBand("
      << static_cast<unsigned short>( band ) << ")";

    carma::antenna::common::AntennaIF::selectBand(
	static_cast<unsigned short>( band ) );
  }
  catch (...)
  {
    logCaughtAsErrorAndRethrowAsUser( log_ );
  }
}

// -----------------------------------------------------------------------------
  void AntennaIF::setAtten( ::CORBA::Float atten )
{
  try
  {
    log_ << Priority::INFO << "(AntennaIF) IFControlImpl::setAtten() - "
      << "Setting attenuation to " << static_cast<float>( atten );

    setIFtotalAttenuation( static_cast<float>( atten ) );
    updateIFTimer();
  }
  catch (...)
  {
    logCaughtAsErrorAndRethrowAsUser( log_ );
  }
}

// -----------------------------------------------------------------------------
  void AntennaIF::setPower( ::CORBA::Float pow )
{
  try
  {
    log_ << Priority::INFO << "(AntennaIF) IFControlImpl::setPower() - "
      << "Setting IF total power to " << static_cast<float>( pow ) << "mW";
    setIFlevel( static_cast<float>( pow ) );
    updateIFTimer();
  }
  catch (...)
  {
    logCaughtAsErrorAndRethrowAsUser( log_ );
  }
}

// -----------------------------------------------------------------------------
  void AntennaIF::setPresetPower()
{
  try
  {
    log_ << Priority::INFO << "(AntennaIF) IFControlImpl::setPresetPower()";
    setOutputPowerToPreset();
  }
  catch (...)
  {
    logCaughtAsErrorAndRethrowAsUser( log_ );
  }
}

// -----------------------------------------------------------------------------
void AntennaIF::reset( )
{
  try {
    log_ << Priority::INFO << "reset.";
    Device::reset( );
  } catch (...) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
  }
}

// arrg, why was common::AntennaIF set up with annon
// name space??
namespace {
  const carma::canbus::msgType STOP_FAST_CH1               = 0x002;
  const carma::canbus::msgType STOP_FAST_CH2               = 0x003;
  const carma::canbus::msgType START_FAST_CH1              = 0x004;
  const carma::canbus::msgType START_FAST_CH2              = 0x005;
};

void AntennaIF::startFastSample( unsigned short chan )
{
  carma::canbus::msgType  start;
  start = START_FAST_CH1; // need to make chan 2 handler?

  carma::canbus::Message msg(createId(false, getApi(), getNode(),
	START_FAST_CH1), getBusId());
  msg << static_cast<unsigned short>(chan);
  io_.postMessage(msg);
}

void AntennaIF::stopFastSample()
{
  carma::canbus::msgType stop;

  stop = STOP_FAST_CH1;

  carma::canbus::Message msg(createId(false, getApi(), getNode(),
	stop), getBusId());
  io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void AntennaIF::blankingFrameHook1( float ifoutTotPower, float pamTemp )
{

//  cout << "Got frame hook 1, node: " << (int)getNode();
//  cout << "  pow: " << ifoutTotPower << " temp: " << pamTemp  << endl;
  if ( getNode() == POL1_NODE_ID )
  {
    _bimaShm->putData( "IF1CANPOW", &ifoutTotPower, 1 );
    _bimaShm->putData( "IF1CANTEMP", &pamTemp, 1 );
  } else if ( getNode() == POL2_NODE_ID )
  {
    _bimaShm->putData( "IF2CANPOW", &ifoutTotPower, 1 );
    _bimaShm->putData( "IF2CANTEMP", &pamTemp, 1 );
  }
}

// -----------------------------------------------------------------------------
void AntennaIF::blankingFrameHook2( float attenSet, unsigned char pamStat,
    unsigned char ifSwitchStat, unsigned char laserStat,
    unsigned char nErrors )
{
  _bimaShm->putData( "IFTOTATTEN", &attenSet, 1 );
  // Need to store char as an int TODO
  //    _bimaShm->putData( "IFCANSTAT", pamStat, 1 );
  //    _bimaShm->putData( "IFCANSWSTAT", ifSwitchStat, 1 );
  //    _bimaShm->putData( "IFCANLASTAT", laserStat, 1 );
  //    _bimaShm->putData( "IFCANERRS", nErrors, 1 );
}

// -----------------------------------------------------------------------------
void AntennaIF::blankingFrameHook3( float laserOpticalPow, float laserTemp )
{
  _bimaShm->putData( "IFCANOPTPOW", &laserOpticalPow, 1 );
  _bimaShm->putData( "IFCANLSRTEMP", &laserTemp, 1 );
}

// -----------------------------------------------------------------------------
void AntennaIF::blankingFrameHook4( float inputAttenSet, float outputAttenSet )
{
  CPTRACE( Trace::TRACE6, "Blanking Frame Packet 4 recv" );
  _bimaShm->putData( "IFINATTEN", &inputAttenSet, 1 );
  _bimaShm->putData( "IFOUTATTEN", &outputAttenSet, 1 );
}

// -----------------------------------------------------------------------------
void AntennaIF::updateIFTimer()
{
  int powt;
  _bimaShm->getData( "IFPOWTIMER", &powt, 1 );
  powt++;
  _bimaShm->putData( "IFPOWTIMER", &powt, 1 );
}
