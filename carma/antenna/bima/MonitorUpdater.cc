/**@file
 * Class implementation for MonitorUpdater
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.140 $
 * $Date: 2014/10/06 20:57:37 $
 * $Id: MonitorUpdater.cc,v 1.140 2014/10/06 20:57:37 mpound Exp $
 */


// CARMA/BIMA includes 
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/antenna/bima/MonitorUpdater.h"
#include "carma/services/Physical.h"
#include "carma/antenna/bima/ProcessMonitor.h"
#include "carma/util/Time.h"



// Get min
#include <algorithm>

using namespace std;
using namespace log4cpp;
using namespace carma::util;

using namespace carma::monitor;
using namespace carma::antenna::bima;
using namespace carma::services::constants;

// Shorthand for wordy enums.
typedef AntennaCommon::YigStateMonitorPointEnum YigStateMPE; 
typedef AntennaCommon::LoStateMonitorPointEnum LoStateMPE; 
typedef AntennaCommon::RxStateMonitorPointEnum RxStateMPE;
typedef AntennaCommon::CurrentRxMonitorPointEnum CurrentRxMPE;

// ******************************************************************************
  MonitorUpdater::MonitorUpdater( Configuration &config, 
                                  log4cpp::Category &logger,
                                  const double autoWriteDelayInS )
: _config( config ),
  _logger( logger )
{
  _monPnts = new BimaSubsystem( _config.getAntennaNo() );
  _readMonPnts = new BimaSubsystem( _config.getAntennaNo() );

  if ( _monPnts != NULL )
    _monPnts->startAutoWriter( autoWriteDelayInS );

  _rx = new Rx( _config );
  _dewarReg = new DewarRegulation( _config, false, false );
  _calwheel = new CalWheel( _config );
  _secondary = new Secondary( _config );
  _polarizer = new Polarizer( _config );
  _lo = new LO( _config );
  _if = new IF( _config );
  _drives = new Drives( _config );

  BimaSubsystem::Constants &bimaDrvPC = _monPnts->bimaSpecific().drive().point().constants();
  _apcCount = min((int)DriveCommand::dazCoefsCount, bimaDrvPC.getNumApc());
  _epcCount = min((int)DriveCommand::delCoefsCount, bimaDrvPC.getNumEpc());

  deadManOk();
}

// ******************************************************************************
MonitorUpdater::~MonitorUpdater() 
{
  // destroy.  Destroy.... DESTROY!!!
}

void MonitorUpdater::run( void )
{

}


void *MonitorUpdater::startThread( void *anObj )
{
  MonitorUpdater *monitorUpdater = static_cast<MonitorUpdater *>(anObj);

  CPTRACE( Trace::TRACE2, "Starting MonitorUpdater Thread" );
  monitorUpdater->monitorThread();

  return NULL;
}

void MonitorUpdater::setAntCommonLoMonitorPoints( 
    AntennaCommon::LO & antComLO,
    bima::Rx & rx,
    bima::LO & lo,
    YigStateMPE::YIGSTATE & yigstate, 
    LoStateMPE::LOSTATE & lostate,
    const carma::monitor::VaractorModule & varactor,
    const int strobe )
{
  const double loFreq = rx.getLO1Freq();

  antComLO.loFreq().setValue( rx.getLO1Freq() );
  antComLO.oscFreq().setValue( rx.getOscFreq() );
  antComLO.yigFreq().setValue( static_cast<double>( lo.xBandFreq() ) );
  antComLO.yigIFLevel().setValue( static_cast<double>( lo.xBandIFLevel() ) );
  antComLO.yigError().setValue( static_cast<double>( lo.xBandErrorVolts() ) );
  antComLO.yigSweep().setValue( AntennaCommon::YigSweepMonitorPointEnum::ON );

  // Set common yig state based on bima lock info
  {
    const int lockinfo = lo.getLockInfo(); // 1 is search, 2 is optimizing
    const bool locked = lo.xLockStatus(); // true locked

    if ( lockinfo == 1 ) 
      yigstate = YigStateMPE::SEARCH;
    // omit OPTIMIZING case for YIG; it is brief and I want to reserve keyword for SIS bias
    // else if ( lockinfo == 2 )        
    //  yigstate = YigStateMPE::OPTIMIZING;
    else if ( lockinfo == 0 && (strobe % 8) == 0 ) {
        if ( locked )
          yigstate = YigStateMPE::LOCK;
        else
          yigstate = YigStateMPE::FAILED;
    }

    antComLO.yigState().setValue( yigstate );
  }
      
  // Based on requested lo freq, set common lo state from either
  // the bima mmlock info (if lo > 75GHz) or the varactor module.

  if ( loFreq > 60.0 ) { // mm observing

    const int mmlockinfo = rx.getMMLockInfo(); // 1 search, 2 optimizing
    const bool mmlockstat = rx.mmLockStatus();

    if ( mmlockinfo == 1 ) {
      lostate = LoStateMPE::SEARCH;
    // omit OPTIMIZING case for mmosc; it is brief and I want to reserve keyword for SIS bias
    // } else if ( mmlockinfo == 2 ) {
    //  lostate = LoStateMPE::OPTIMIZING;
    } else if ( mmlockinfo == 0 ) {
      if ( mmlockstat ) 
        lostate = LoStateMPE::LOCK;
      else 
        lostate = LoStateMPE::FAILED;
    }

  } else { // loFreq < 60.0GHz - cm observing
     
    // Set lo lock status based on the varactor
    typedef VaractorModule::LockStatusMonitorPointEnum LockStatusSPE;

    const LockStatusSPE & varactorLockStat = varactor.lockStatus();

    if ( varactorLockStat.isValid() ) {

      if ( varactorLockStat.getValue() == LockStatusSPE::LOCKED ) {
        lostate = LoStateMPE::LOCK;
      } else {
        lostate = LoStateMPE::FAILED;
      }

    } else {
      lostate = LoStateMPE::FAILED;
    }
  }

  antComLO.loState().setValue( lostate );

  // Set lo sweep based on mmSweep MP.
  if ( rx.getSweepState() == Rx::ON ) {
    antComLO.loSweep().setValue( AntennaCommon::LoSweepMonitorPointEnum::ON );
  } else {
    antComLO.loSweep().setValue( AntennaCommon::LoSweepMonitorPointEnum::OFF );
  }

} // MonitorUpdator::setAntCommonLoMonitorPoints


void MonitorUpdater::setAntCommonRxMonitorPoints( 
    AntennaCommon::Receivers & antComRx,
    bima::Rx & rx,
    RxStateMPE::RXSTATE & rxGoodmpe,
    const YigStateMPE::YIGSTATE & yigState, 
    const LoStateMPE::LOSTATE & loState )
{
  const double loFreq = rx.getLO1Freq();

  if ( loFreq < 75.0 ) { 
    antComRx.currentRx().setValue( CurrentRxMPE::RX1CM );
  } else if ( loFreq < 150.0 ) {
    antComRx.currentRx().setValue( CurrentRxMPE::RX3MM );
  } else {
    antComRx.currentRx().setValue( CurrentRxMPE::RX1MM );
  }
  
  antComRx.tuneSeqNum().setValue( rx.getCurTuneSequenceNo() );
  antComRx.dewarTemp().setValue( 
    static_cast<double>( rx.getDewar().stage3temp() ) );

  // mapping of bima rx state to RxStateMonitorPointEnum:
  //   BAD        if 10MHz is unlocked (or if state is unknown)
  //   YIG_BAD    if YIG is unlocked
  //   GUNN_BAD   if mm Gunn osc OR cm varactor is unlocked
  //   SEARCH     while searching for mmosc lock
  //   RX_BAD     if mixer is too hot or receiver has not yet been tuned
  //   TUNE       while setting WBA13 and SIS bias to tuning table values
  //   OPTIMIZE   while scanning SIS bias on amb/sky to optimize Y-factor
  //   GOOD       tuning complete; all oscillators locked; no guarantee of perfect SIS bias 

  const bool tenMHzLocked = rx.getLO10MHzStatus( );     
  int bimaTuneState = rx.getBimaTuneState( );

  rxGoodmpe = AntennaCommon::RxStateMonitorPointEnum::BAD;    // default is 'BAD'

  if ( yigState == YigStateMPE::FAILED )  
    rxGoodmpe = AntennaCommon::RxStateMonitorPointEnum::YIG_BAD;
  else if ( yigState == YigStateMPE::SEARCH ) 
    rxGoodmpe = AntennaCommon::RxStateMonitorPointEnum::SEARCH;
  else if ( yigState == YigStateMPE::LOCK && tenMHzLocked ) {     

    if ( loState == LoStateMPE::FAILED ) 
      rxGoodmpe = AntennaCommon::RxStateMonitorPointEnum::GUNN_BAD;
    else if ( loState == LoStateMPE::SEARCH ) 
      rxGoodmpe = AntennaCommon::RxStateMonitorPointEnum::SEARCH;
    else if ( loState == LoStateMPE::LOCK ) {
   
      if ( loFreq < 60.0 ) bimaTuneState = 3;   // cm observing; ignore SIS bias conditions
      if ( bimaTuneState == 0 )        // SIS mxr too hot, or receiver has not yet been tuned
        rxGoodmpe = AntennaCommon::RxStateMonitorPointEnum::RX_BAD;   
      else if ( bimaTuneState == 1 )    
        rxGoodmpe = AntennaCommon::RxStateMonitorPointEnum::TUNE;   
      else if ( bimaTuneState == 2 )    
        rxGoodmpe = AntennaCommon::RxStateMonitorPointEnum::OPTIMIZE;   
      else if ( bimaTuneState == 3 )    
        rxGoodmpe = AntennaCommon::RxStateMonitorPointEnum::GOOD;   
    }
  }
  antComRx.rxState().setValue( rxGoodmpe );

} // MonitorUpdater::setAntCommonRxMonitorPoints


void MonitorUpdater::monitorThread( void )
{
  // shortcuts to accessors...
  AntennaCommon &antCom = _monPnts->antennaCommon();
  AntennaCommon::Optics &antComOpt = _monPnts->antennaCommon().optics();
  AntennaCommon::LO &antComLO = _monPnts->antennaCommon().lO();
  AntennaCommon::Drive &comDrv = _monPnts->antennaCommon().drive();
  AntennaCommon::Track &comDrvTrk = comDrv.track();
  AntennaCommon::Point &comDrvP = comDrv.point();
  AntennaCommon::Constants &comDrvPC = comDrvP.constants();
  AntennaCommon::Limit &comDrvLim = comDrv.limit();
  AntennaCommon::Receivers &antComRx = _monPnts->antennaCommon().receivers();
  VaractorModule & varactor = _readMonPnts->gunn1cm().varactor();

  // Unneeded?
  //  AntennaCommon::OpticalTel &optTel = _monPnts->antennaCommon().opticalTel();
  BimaSubsystem::Refs &bimaRefs = _monPnts->bimaSpecific().refs();
  BimaSubsystem::Drive &bimaDrv = _monPnts->bimaSpecific().drive();  // for later?
  BimaSubsystem::Optics &bimaOpt = _monPnts->bimaSpecific().optics();
  BimaSubsystem::Track &bimaTrk = bimaDrv.track();
  BimaSubsystem::Point &bimaDrvP = bimaDrv.point();
  BimaSubsystem::Constants &bimaDrvPC = bimaDrvP.constants();
  BimaSubsystem::Telemetry &telm = _monPnts->bimaSpecific().telemetry();
  BimaSubsystem::LOB &bimaLOB = _monPnts->bimaSpecific().lOB();
  BimaSubsystem::LOD &bimaLOD = _monPnts->bimaSpecific().lOD();
  BimaSubsystem::Dewar &bimaDewar = _monPnts->bimaSpecific().dewar();
  BimaSubsystem::Regulation &bimaDewarReg = bimaDewar.regulation();
  BimaSubsystem::Plate &bimaPlate = _monPnts->bimaSpecific().plate();
  BimaSubsystem::MMlock &bimaMMlock = _monPnts->bimaSpecific().mMlock();
  BimaSubsystem::SIS &bimaSIS = _monPnts->bimaSpecific().sIS();
  BimaSubsystem::WBA &bimaWBA = _monPnts->bimaSpecific().wBA();
  BimaSubsystem::CalPlate &calPlate = _monPnts->bimaSpecific().calPlate();
  BimaSubsystem::LOTerm &loTerm = _monPnts->bimaSpecific().lOTerm();
  BimaSubsystem::StatusBits &bimaStatus = _monPnts->bimaSpecific().statusBits();
  BimaSubsystem::BimaSpecific &bimaSp = _monPnts->bimaSpecific();
  BimaSubsystem::Temperatures &bimaTemps = _monPnts->bimaSpecific().temperatures();

  //BimaSubsystem::StateMonitorPointEnum smpe( string( "convience obj" ) );


  AntennaCommon::YigStateMonitorPointEnum::YIGSTATE yigstate 
    = AntennaCommon::YigStateMonitorPointEnum::FAILED;
  AntennaCommon::LoStateMonitorPointEnum::LOSTATE lostate
    = AntennaCommon::LoStateMonitorPointEnum::FAILED;
  AntennaCommon::RxStateMonitorPointEnum::RXSTATE rxGoodmpe 
    = AntennaCommon::RxStateMonitorPointEnum::BAD;
  BimaSubsystem::StateMonitorPointEnum::STATE mmstate
    = BimaSubsystem::StateMonitorPointEnum::FAILED;
  AntennaCommon::StateMonitorPointEnum::STATE dsmpe
    = AntennaCommon::StateMonitorPointEnum::DISABLE;
  AntennaCommon::CalStateMonitorPointEnum::CALSTATE csmpe
    = AntennaCommon::CalStateMonitorPointEnum::ERROR;
  AntennaCommon::ModeMonitorPointEnum::MODE dmmpe
    = AntennaCommon::ModeMonitorPointEnum::STOP;
  AntennaCommon::RefractionModelMonitorPointEnum::REFRACTIONMODEL drmpe
    = AntennaCommon::RefractionModelMonitorPointEnum::OPTICAL;
  AntennaCommon::WrapLogicMonitorPointEnum::WRAPLOGIC wlmpe
    = AntennaCommon::WrapLogicMonitorPointEnum::ZERO;
  AntennaCommon::SelectedApertMonitorPointEnum::SELECTEDAPERT sampe
    = AntennaCommon::SelectedApertMonitorPointEnum::RADIO3MM;
  BimaSubsystem::SismodeMonitorPointEnum::SISMODE smpe
    = BimaSubsystem::SismodeMonitorPointEnum::I;
  BimaSubsystem::AxisVelLimitMonitorPointEnum::AXISVELLIMIT avlmpe
    = BimaSubsystem::AxisVelLimitMonitorPointEnum::NOLIMIT;
  AntennaCommon::FocusStateMonitorPointEnum::FOCUSSTATE fsmpe
    = AntennaCommon::FocusStateMonitorPointEnum::ACQUIRED;
  AntennaCommon::AzSwLimitMonitorPointEnum::AZSWLIMIT azswlimmpe
    = AntennaCommon::AzSwLimitMonitorPointEnum::OK;
  AntennaCommon::ElSwLimitMonitorPointEnum::ELSWLIMIT elswlimmpe
    = AntennaCommon::ElSwLimitMonitorPointEnum::OK;
  AntennaCommon::AzHwLimitMonitorPointEnum::AZHWLIMIT azhwlimmpe
    = AntennaCommon::AzHwLimitMonitorPointEnum::OK;
  AntennaCommon::ElHwLimitMonitorPointEnum::ELHWLIMIT elhwlimmpe
    = AntennaCommon::ElHwLimitMonitorPointEnum::OK;
  AntennaCommon::SafeStateMonitorPointEnum::SAFESTATE collisionmpe
    = AntennaCommon::SafeStateMonitorPointEnum::SAFE;
      
  CalWheel::Positions lastcwp = CalWheel::ERROR;

  float azLow, azHigh, elLow, elHigh;

  Polarizer::PolPos polPos;

  // Unneeded?
  //  BimaSubsystem::TenStatusMonitorPointEnum::TENSTATUS tsmpe
  //    = BimaSubsystem::TenStatusMonitorPointEnum::BAD;


  double phaseNoise = 0.0;
  // float id = 0.;
  // float vg = 0.;
  // float vgc = 0.;

  int value;
  int strobe = 0; // this is used to delay certain points from being
  // queried every .5 secs, reducing canbus/telem load

  char *sourcechr = (char *)malloc(11);
  if ( sourcechr == NULL )
    throw CARMA_ERROR( "Unable to allocate memory" );

  carma::services::Units units;

  FrameAlignedTimer framer;
  framer.ResetNextFireTime();

  double curAz, curEl;
  curAz = curEl = 0.0;

  int lastFrameCnt = 0;
  string xmlver, firmver;

  xmlver = _rx->getTelemetryVersion();

  while ( true )
  {
    try
    {
      CPTRACE( Trace::TRACE1, "deadManOk()" );
      deadManOk();

      framer.WaitForNextFireTime();
      //      CARMA_CPTRACE( Trace::TRACE7, "Frame Timer Fire!");
      //_logger << Priority::WARN << "Frame Fire ";

      strobe++;

      int fastPacket = 0;
      int slowPacket = 0;
      bool FP1 = false;
      bool FP2 = false;

      //  Monitor Updater is going...
      //  END of Monitor Updater info...

      _rx->putData( "TMSTG", &strobe );
      //  TELEMETRY MONITOR INFORMATION ------------------------------------------
      // this is a little dirty.  Since all ant objects are parented by
      // TelemetryClient, whose parent is SharedMemory, any of the objects
      // support direct lookups into the shared mem...
      _rx->getData( "FRAMECNT", &value, 1 );
      _rx->putData( "MONITORUP", &value ); // used to monitor this thread
      // FRAMECNT is a two byte number sent across the telemetry bus - it rolls
      // over approximately every eleven minutes.  We account for it here.
      int frameDiff = 0;
      if ( value < lastFrameCnt ) { // Rollover
        frameDiff = ( 65536 - lastFrameCnt ) + value;
      } else {
        frameDiff = value - lastFrameCnt;
      }
      telm.frames().setValue( frameDiff * 2 );
      lastFrameCnt = value;

      firmver = _rx->getFirmwareVersion();
      telm.firmWareVer().setValue( firmver );
      telm.telemXMLVer().setValue( xmlver );
      telm.verMatch().setValue( xmlver.compare( firmver ) == 0 );

      double telemUP, ifUP, driveUP, rxUP, controlUP, initialized;
      bool telgood, ifgood, rxgood, controlgood, drivegood, initgood;
      _rx->getData( "TELEMUP", &telemUP, 1);
      _rx->getData( "IFUP", &ifUP, 1);
      _rx->getData( "DRIVEUP", &driveUP, 1);
      _rx->getData( "RXUP", &rxUP, 1);
      _rx->getData( "CONTRLUP", &controlUP, 1);
      _rx->getData( "INITIAL", &initialized, 1);
      double time = Time::MJD();
      telgood = abs(time - telemUP) <= MAXGAP;
      ifgood = abs(time - ifUP) <= MAXGAP;
      drivegood = abs(time - driveUP) <= MAXGAP;
      rxgood = abs(time - rxUP) <= MAXGAP;
      controlgood = abs(time - controlUP) <= MAXGAP;
      initgood = (initialized != 0.0);

      
      //_logger << Priority::WARN << oss5.str();
      if(telgood && ifgood && drivegood && rxgood && controlgood && initgood){
	_monPnts->antennaCommon().initialized().setValue(true);
      }
      else{
	_monPnts->antennaCommon().initialized().setValue(false);
      }
      time = Time::MJD();
      double writeMJD;
      _rx->getData("FPK01MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXFASTGAP){
	fastPacket |= 0x00000001;
	FP1 = true;
      }
      _rx->getData("FPK02MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXFASTGAP){
	FP2 = true;
	fastPacket |= 0x00000002;
       }
      _rx->getData("FPK03MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXFASTGAP){
	fastPacket |= 0x00000004;
      }
      _rx->getData("FPK04MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXFASTGAP){
	fastPacket |= 0x00000008;
      }
      _rx->getData("FPK05MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXFASTGAP){
	fastPacket |= 0x00000010;
      }
      _rx->getData("FPK06MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXFASTGAP){
	fastPacket |= 0x00000020;
      }
      _rx->getData("FPK07MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXFASTGAP){
	fastPacket |= 0x00000040;
       }
      _rx->getData("FPK08MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXFASTGAP){
	fastPacket |= 0x00000080;
      }
      _rx->getData("FPK09MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXFASTGAP){
	fastPacket |= 0x00000100;
      }
      _rx->getData("FPK10MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXFASTGAP){
	fastPacket |= 0x00000200;
      }
      _rx->getData("FPK11MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXFASTGAP){
	fastPacket |= 0x00000400;
      }
      _rx->getData("FPK12MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXFASTGAP){
	fastPacket |= 0x00000800;
      }
      _rx->getData("FPK13MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXFASTGAP){
	fastPacket |= 0x00001000;
      }
      _rx->getData("FPK14MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXFASTGAP){
	fastPacket |= 0x00002000;
      }

      _rx->getData("SPK01MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXSLOWGAP){
	slowPacket |= 0x00000001;
      }
      _rx->getData("SPK02MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXSLOWGAP){
	slowPacket |= 0x00000002;
      }
      _rx->getData("SPK03MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXSLOWGAP){
	slowPacket |= 0x00000004;
      }
      _rx->getData("SPK04MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXSLOWGAP){
	slowPacket |= 0x00000008;
      }
      _rx->getData("SPK05MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXSLOWGAP){
	slowPacket |= 0x00000010;
      }
      _rx->getData("SPK06MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXSLOWGAP){
	slowPacket |= 0x00000020;
      }
      _rx->getData("SPK07MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXSLOWGAP){
	slowPacket |= 0x00000040;
      }
      _rx->getData("SPK08MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXSLOWGAP){
	slowPacket |= 0x00000080;
      }
      _rx->getData("SPK09MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXSLOWGAP){
	slowPacket |= 0x00000100;
      }
      _rx->getData("SPK10MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXSLOWGAP){
	slowPacket |= 0x00000200;
      }
      _rx->getData("SPK11MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXSLOWGAP){
	slowPacket |= 0x00000400;
      }
      _rx->getData("SPK12MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXSLOWGAP){
	slowPacket |= 0x00000800;
      }
      _rx->getData("SPK13MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXSLOWGAP){
	slowPacket |= 0x00001000;
      }
      _rx->getData("SPK14MJD",&writeMJD,1);
      if(abs(time - writeMJD) <= MAXSLOWGAP){
	slowPacket |= 0x00002000;
      }

      _monPnts->bimaSpecific().telemetry().fastMonitorData().setValue(fastPacket);
      _monPnts->bimaSpecific().telemetry().fastMonitorData().setValidity(MonitorPoint::VALID);
      _monPnts->bimaSpecific().telemetry().slowMonitorData().setValue(slowPacket);
      _monPnts->bimaSpecific().telemetry().slowMonitorData().setValidity(MonitorPoint::VALID);
//  TELEMETRY MONITOR INFORMATION ------------------------------------------

      _rx->putData( "RMSTG", &strobe );
      //  RECEIVER MONITOR INFORMATION -------------------------------------------
      double LO10MHzOptPwrUW =  _rx->getLO10MHzOptPwr()/0.027; // v -> uW
      double LO10MHzOptPwrDBM =  -99.9; // Default
      if (LO10MHzOptPwrUW > 0.001) {
          LO10MHzOptPwrDBM = 10*log10(0.001*LO10MHzOptPwrUW);
      }
      double LO50MHzOptPwrUW =  _rx->getLO50MHzOptPwr()/0.027; // v -> uW
      double LO50MHzOptPwrDBM =  -99.9; // Default
      if (LO50MHzOptPwrUW > 0.001) {
          LO50MHzOptPwrDBM = 10*log10(0.001*LO50MHzOptPwrUW);
      }
      bimaRefs.tenOptPwr().setValue(LO10MHzOptPwrDBM);
      bimaRefs.tenStatus().setValue( ( _rx->getLO10MHzStatus() ?
            BimaSubsystem::TenStatusMonitorPointEnum::LOCK :
            BimaSubsystem::TenStatusMonitorPointEnum::BAD ) );
      bimaRefs.fiftyOptPwr().setValue(LO50MHzOptPwrDBM);

      bimaRefs.yigCommanded().setValue( _lo->getCommanded() );

      // Dewar/Heater
      bimaDewar.stage1().setValue( (double)_rx->getDewar().stage1temp() );
      bimaDewar.stage2().setValue( (double)_rx->getDewar().stage2temp() );
      bimaDewar.stage3().setValue( (double)_rx->getDewar().stage3temp() );
      bimaDewar.stage4().setValue( (double)_rx->getDewar().stage4temp() );
      bimaDewar.stage5().setValue( (double)_rx->getDewar().stage5temp() );
      // This needs to be converted to ma/V or ask dick if just the set V and mW are okay instead...
      bimaDewar.heater3v().setValue( (double)_rx->getDewar().getHeater3mW() );
      bimaDewar.heater3ma().setValue( (double)_rx->getDewar().getHeater3mW() );

      _rx->putData( "RLCKMSTG", &strobe );
      // mm lock info
      // should be made a const... 1 is search, 2 is opt
      // this should probably just be moved into LO... as getLockInfo()...
      int mmlockinfo = _rx->getMMLockInfo();
      bool mmlockstat = _rx->mmLockStatus();
      if ( mmlockinfo == 1 )
        mmstate = BimaSubsystem::StateMonitorPointEnum::SEARCH;
      else if ( mmlockinfo == 2 )
        mmstate = BimaSubsystem::StateMonitorPointEnum::OPTIMIZING;
      else if ( mmlockinfo == 0 )
      {

        if ( mmlockstat == true )
          mmstate = BimaSubsystem::StateMonitorPointEnum::LOCK;
        else
          mmstate = BimaSubsystem::StateMonitorPointEnum::FAILED;
      }

      bimaMMlock.state().setValue( mmstate );

      bimaMMlock.mmband().setValue (
          static_cast<BimaSubsystem::MmbandMonitorPointEnum::MMBAND>(
            _rx->getPhaseLockBand()) ); 

      bimaMMlock.error().setValue( _rx->mmErrorVolts() );
      bimaMMlock.iFLevel().setValue( _rx->getMMIFLevel() );
      bimaMMlock.loopgain().setValue( _rx->getLoopGain() );


      BimaSubsystem::FiftyMHzRefMonitorPointEnum::FIFTYMHZREF fmrmpe;
      if ( _rx->getPhaseLockRefState() == Rx::OK )
        fmrmpe = BimaSubsystem::FiftyMHzRefMonitorPointEnum::OK;
      else
        fmrmpe = BimaSubsystem::FiftyMHzRefMonitorPointEnum::NOTOK;
      bimaMMlock.fiftyMHzRef().setValue( fmrmpe );


      BimaSubsystem::SweepMonitorPointEnum::SWEEP swmpe;
      if ( _rx->getSweepState() == Rx::ON )
        swmpe = BimaSubsystem::SweepMonitorPointEnum::ON;
      else
        swmpe = BimaSubsystem::SweepMonitorPointEnum::OFF;
      bimaMMlock.sweep().setValue( swmpe );

      BimaSubsystem::OscMonitorPointEnum::OSC ompe;
      if ( _rx->getMMOscState() == Rx::ON )
        ompe = BimaSubsystem::OscMonitorPointEnum::ON;
      else
        ompe = BimaSubsystem::OscMonitorPointEnum::OFF;
      bimaMMlock.osc().setValue( ompe );

      /* if ( strobe % 8 == 0 ) */
      phaseNoise = _rx->getPhaseNoise();

      bimaMMlock.phasenoise().setValue( phaseNoise );

      bimaMMlock.vop().setValue( _rx->getVopCommanded() );

      // LO Plate B info (mm phase lock)
      bimaLOB.mmosc().setValue( (unsigned short)_rx->getMMOscB().position() );
      bimaLOB.mmbck().setValue( (unsigned short)_rx->getMMBckB().position() );
      bimaLOB.atten().setValue( (unsigned short)_rx->getAttenB().position() );
      if ( _config.hasModulatorB() )
        bimaLOB.mod().setValue( _rx->getModBmA() );
      bimaLOB.vop().setValue( (double)_rx->getVopB() );
      bimaLOB.ref().setValue( (unsigned short)_rx->getMMOscB().getADMax() ); // ref on lo plate?

      // LO Plate AD info (mm phase lock)
      bimaLOD.mmosc().setValue( (unsigned short)_rx->getMMOscAD().position() );
      bimaLOD.mmbck().setValue( (unsigned short)_rx->getMMBckAD().position() );
      bimaLOD.atten().setValue( (unsigned short)_rx->getAttenAD().position() );
      if ( _config.hasModulatorD() )
        bimaLOD.mod().setValue( _rx->getModADmA() );
      bimaLOD.vop().setValue( (double)_rx->getVopAD() );
      bimaLOD.ref().setValue( (unsigned short)_rx->getMMOscAD().getADMax() ); // ref on lo plate?

      // General Plate info
      bimaPlate.temp().setValue( _if->getTemp() );
      bimaPlate.heater().setValue( _if->getHeater() );

      // PAM info
      // PAM monitor data is handled by the bimaIFHost program...

      // _rx->getDewarBand() points to SIS band selection..
      bimaSIS.sisband().setValue( 
          static_cast<BimaSubsystem::SisbandMonitorPointEnum::SISBAND>(_rx->getDewarBand() ) );

      switch ( (char)_rx->getSISMode() )
      {
        case 'i':
          smpe = BimaSubsystem::SismodeMonitorPointEnum::I;
          break;
        case 'v':
          smpe = BimaSubsystem::SismodeMonitorPointEnum::V;
          break;
        case 'o':
        default:
          smpe = BimaSubsystem::SismodeMonitorPointEnum::O;
          break;
      }
      bimaSIS.sismode().setValue( smpe );
      bimaSIS.sisbiasout().setValue( _rx->getSISBiasOut() );
      bimaSIS.sisreadback().setValue( _rx->getInstantSISVBias() );
      bimaSIS.sisreadbacku().setValue( _rx->getInstantSISIBias() );


      /*
      if ( (strobe % 16 == 0) )
      {
        id = _rx->getIDrain();
        vg = _rx->getVGate();
        vgc = _rx->getVGateCmd();
      }
      */

      bimaWBA.idrain().setValue( _rx->getIDrain() );
      bimaWBA.vgate().setValue( _rx->getVGate() );
      bimaWBA.vgatecmded().setValue( _rx->getVGateCmd() );

      // Input
      polPos = _polarizer->getCanonicalPosition();
      if ( polPos == Polarizer::MOVING )
        antComOpt.polarization().setValue( AntennaCommon::PolarizationMonitorPointEnum::MOVING );
      else if ( polPos == Polarizer::BAD )
        antComOpt.polarization().setValue( AntennaCommon::PolarizationMonitorPointEnum::BAD );
      else if ( polPos == Polarizer::LCP )
        antComOpt.polarization().setValue( AntennaCommon::PolarizationMonitorPointEnum::LCP );
      else if ( polPos == Polarizer::RCP )
        antComOpt.polarization().setValue( AntennaCommon::PolarizationMonitorPointEnum::RCP );
      else if ( polPos == Polarizer::H )
        antComOpt.polarization().setValue( AntennaCommon::PolarizationMonitorPointEnum::H );
      else if ( polPos == Polarizer::V )
        antComOpt.polarization().setValue( AntennaCommon::PolarizationMonitorPointEnum::V );
      else if ( polPos == Polarizer::CLEAR )
        antComOpt.polarization().setValue( AntennaCommon::PolarizationMonitorPointEnum::CLEAR );

      bimaOpt.polpos().setValue( _polarizer->getInstantPosition() );

      bimaOpt.calpos().setValue( _calwheel->getInstantPosition() );

      // sorry about this, but I am going to use direct lookup from shared memory here
      // rather than writing hundreds of sick little routines
      if ( _config.cmOpticsInstalled() ) {   // cm optics ARE installed
        int statusbits ;
        _rx->getData( "BITSIN5F", &statusbits, 1 );
        if ( !(statusbits & 0x04) && (statusbits & 0x08)) {
          bimaOpt.beamSelect().setValue( BimaSubsystem::BeamSelectMonitorPointEnum::CM ) ;
        } else if ( (statusbits & 0x04) && !(statusbits & 0x08) ) {
          bimaOpt.beamSelect().setValue( BimaSubsystem::BeamSelectMonitorPointEnum::MM ) ;
        } else {
          bimaOpt.beamSelect().setValue( BimaSubsystem::BeamSelectMonitorPointEnum::ERROR ) ;
        }

        if (statusbits & 0x80) {
          bimaOpt.opticsCntrl().setValue( BimaSubsystem::OpticsCntrlMonitorPointEnum::MANUAL );
        } else {
          bimaOpt.opticsCntrl().setValue( BimaSubsystem::OpticsCntrlMonitorPointEnum::COMPUTER ) ;
        }

        // beamControl bit was never wired up - both cal load and beam select are controlled
        // by a single manual/computer switch, now called opticsCntrl
        // if (statusbits & 0x40) {
        //  bimaOpt.beamControl().setValue( BimaSubsystem::BeamControlMonitorPointEnum::MANUAL );
        //} else {
        //  bimaOpt.beamControl().setValue( BimaSubsystem::BeamControlMonitorPointEnum::COMPUTER );
        //}

      } else {                       // cm optics are NOT installed
        bimaOpt.beamSelect().setValue( BimaSubsystem::BeamSelectMonitorPointEnum::NONE ) ;
        bimaOpt.opticsCntrl().setValue( BimaSubsystem::OpticsCntrlMonitorPointEnum::NONE );
      }

      CalWheel::Positions cwp = _calwheel->getPosition( _rx->getDewarWindow() );

      CPTRACE( Trace::TRACE2, "CalWheel: " << cwp );
      switch ( cwp )
      {
        case CalWheel::SKY:
          csmpe = ( cwp == lastcwp ? 
                    AntennaCommon::CalStateMonitorPointEnum::SKY :
                    AntennaCommon::CalStateMonitorPointEnum::MOVING );
          break;
        case CalWheel::AMB:
          csmpe = ( cwp == lastcwp ? 
                    AntennaCommon::CalStateMonitorPointEnum::AMB :
                    AntennaCommon::CalStateMonitorPointEnum::MOVING );
          break;
        case CalWheel::FIXED:
          csmpe = ( cwp == lastcwp ?
                    AntennaCommon::CalStateMonitorPointEnum::FIXED :
                    AntennaCommon::CalStateMonitorPointEnum::MOVING );
          break;
        case CalWheel::REFLEC:
          csmpe = ( cwp == lastcwp ? 
                    AntennaCommon::CalStateMonitorPointEnum::REFLEC :
                    AntennaCommon::CalStateMonitorPointEnum::MOVING );
          break;
        case CalWheel::MOVING:
          csmpe = AntennaCommon::CalStateMonitorPointEnum::MOVING;
          break;
        case CalWheel::ERROR:
          csmpe = AntennaCommon::CalStateMonitorPointEnum::ERROR;
          break;
        default:
          csmpe = AntennaCommon::CalStateMonitorPointEnum::ERROR;
          break;
      }

      lastcwp = cwp;

      CPTRACE( Trace::TRACE2, "csmpe: " << csmpe );
      antCom.calibrator().calState().setValue( csmpe );
      antCom.calibrator().calSeqNum().setValue( _calwheel->getCurSequenceNo() );
      antCom.calibrator().ambTemp().setValue( 
        _calwheel->getAmbTemp() - Physical::ABS_ZERO);
      antCom.calibrator().skyTemp().setValue( 
            0.94 * _drives->getAmbWeatherTemp() );

      Secondary::FocusState fs = _secondary->getState();
      switch ( fs )
      {
        case Secondary::MOVING:
          fsmpe = AntennaCommon::FocusStateMonitorPointEnum::MOVING;
          break;
        case Secondary::ACQUIRED:
          fsmpe = AntennaCommon::FocusStateMonitorPointEnum::ACQUIRED;
          break;
        case Secondary::FAILED:
          fsmpe = AntennaCommon::FocusStateMonitorPointEnum::FAILED;
          break;
      }
      antCom.secondary().focusState().setValue( fsmpe );

      antCom.secondary().focusZ().setValue( _secondary->getFocus() );

      calPlate.tempAmb().setValue( _calwheel->getAmbTemp() ); // therm:

      // LOTerm RF info (50MHz)
      double LOtermOptPwrMW =  _rx->getLOTermPower()/0.32; // v -> mW
      double LOtermPwrDBM =  -99.9; // Default
      if (LOtermOptPwrMW > 0.000001) {
          LOtermPwrDBM = 10*log10(LOtermOptPwrMW);
      }

      loTerm.optPower().setValue(LOtermPwrDBM);
      loTerm.rFin().setValue( (_rx->getLOTermRFin()*50.)-34 ); // v -> dBm
      loTerm.rFout().setValue( (_rx->getLOTermRFout()*50)-34 ); // v -> dBm
      loTerm.atten().setValue( _rx->getLOTermAtten() );
      loTerm.temp().setValue( _rx->getLOTermTemp() );

      // Compressor temps
      bimaDewar.tinlet().setValue( (double)_rx->getDewar().getCompressorInletTemp() );
      bimaDewar.tdisch().setValue( (double)_rx->getDewar().getCompressorDischTemp() );
      bimaDewar.texch().setValue( (double)_rx->getDewar().getCompressorExchTemp() );
      bimaDewar.tsump().setValue( (double)_rx->getDewar().getCompressorSumpTemp() );

      // He Pressure values
      bimaDewar.psupply().setValue( (double)_rx->getDewar().getHeSupplyP() );
      bimaDewar.preturn().setValue( (double)_rx->getDewar().getHeReturnP() );

      //  DEWAR REGULATION MONITOR INFORMAITON -----------------------------------

      bimaDewarReg.setPoint().setValue( (float)_dewarReg->getPoint() );
      bimaDewarReg.setPointMJD().setValue( (double)_dewarReg->getPointMJD() );

      bimaDewarReg.htrV().setValue( _dewarReg->getRunningV() );
      bimaDewarReg.minHtrV().setValue( _dewarReg->getMinV() );
      bimaDewarReg.maxHtrV().setValue( _dewarReg->getMaxV() );
      bimaDewarReg.htrPow().setValue( _rx->getDewar().getHeater3mW() );

      bimaDewarReg.curMixrT().setValue( _dewarReg->getAvgTemp() );
      bimaDewarReg.minMixrT().setValue( _dewarReg->getMinAvgTemp() );
      bimaDewarReg.maxMixrT().setValue( _dewarReg->getMaxAvgTemp() );

      bimaDewarReg.rmsSetPnt().setValue( _dewarReg->getTempSetPointRMS() );

      bimaDewarReg.defrosting().setValue( _dewarReg->isDefrosting() );
      bimaDewarReg.regulating().setValue( _dewarReg->isOn() );

      //  DEWAR REGULATION MONITOR INFORMAITON -----------------------------------

      //  DRIVES MONITOR INFORMATION ---------------------------------------------

      bimaTrk.azResolver().setValue( _drives->getInstantAzResolver() );
      bimaTrk.azDigVeFilter().setValue( _drives->getAzDigVeFilter() );

      if ( _drives->getAzCheckDrivePower() == true )
        bimaTrk.azCheckDrivePower().setValue( BimaSubsystem::AzCheckDrivePowerMonitorPointEnum::CHECK );
      else
        bimaTrk.azCheckDrivePower().setValue( BimaSubsystem::AzCheckDrivePowerMonitorPointEnum::OK );

      bimaTrk.azCosEnc().setValue( _drives->getInstantAzCosEnc());
      bimaTrk.azSinEnc().setValue( _drives->getInstantAzSinEnc() );

      curAz = _drives->getAz();
      comDrvTrk.requestedAzimuth().setValue( _drives->getAzRequested() );
      comDrvTrk.actualAzimuth().setValue( curAz );

      if(!FP1){
	comDrvTrk.actualAzimuth().setValidity(MonitorPoint::INVALID_NO_DATA);
	bimaTrk.azSinEnc().setValidity(MonitorPoint::INVALID_NO_DATA);
	bimaTrk.azCosEnc().setValidity(MonitorPoint::INVALID_NO_DATA);
      }

      bimaTrk.elResolver().setValue( _drives->getInstantElResolver() );
      bimaTrk.elDigVeFilter().setValue( _drives->getElDigVeFilter() );

      if ( _drives->getElCheckDrivePower() == true )
        bimaTrk.elCheckDrivePower().setValue( BimaSubsystem::ElCheckDrivePowerMonitorPointEnum::CHECK );
      else
        bimaTrk.elCheckDrivePower().setValue( BimaSubsystem::ElCheckDrivePowerMonitorPointEnum::OK );

      bimaTrk.elCosEnc().setValue( _drives->getInstantElCosEnc() );
      bimaTrk.elSinEnc().setValue( _drives->getInstantElSinEnc() );

      curEl = _drives->getEl();
      comDrvTrk.requestedElevation().setValue( _drives->getElRequested() );
      comDrvTrk.actualElevation().setValue( curEl );
      if(!FP2){
	comDrvTrk.actualElevation().setValidity(MonitorPoint::INVALID_NO_DATA);
	bimaTrk.elCosEnc().setValidity(MonitorPoint::INVALID_NO_DATA);
	bimaTrk.elSinEnc().setValidity(MonitorPoint::INVALID_NO_DATA);
      }

      bimaDrvP.tilt1counts().setValue( _drives->getTilt1Counts() );
      bimaDrvP.tilt2counts().setValue( _drives->getTilt2Counts() );

      bimaDrvP.tilt1().setValue( _drives->getTilt1Arcmin() );
      bimaDrvP.tilt2().setValue( _drives->getTilt2Arcmin() );

      bimaDrvP.tilt1outliers().setValue( _drives->getTilt1Outliers() );
      bimaDrvP.tilt2outliers().setValue( _drives->getTilt2Outliers() );


      switch ( _drives->getActiveState() )
      {
        FLATTENCASEENUM( Drives::TRACK, AntennaCommon::StateMonitorPointEnum::TRACK, dsmpe );
        FLATTENCASEENUM( Drives::CLOSE, AntennaCommon::StateMonitorPointEnum::CLOSE, dsmpe );
        FLATTENCASEENUM( Drives::SLEW, AntennaCommon::StateMonitorPointEnum::SLEW, dsmpe );
        FLATTENCASEENUM( Drives::STOW, AntennaCommon::StateMonitorPointEnum::STOW, dsmpe );
        FLATTENCASEENUM( Drives::SNOW, AntennaCommon::StateMonitorPointEnum::SNOW, dsmpe );
        FLATTENCASEENUM( Drives::STOP, AntennaCommon::StateMonitorPointEnum::STOP, dsmpe );
        FLATTENCASEENUM( Drives::DISABLE, AntennaCommon::StateMonitorPointEnum::DISABLE, dsmpe );
        FLATTENCASEENUM( Drives::HWLIMIT, AntennaCommon::StateMonitorPointEnum::HWLIMIT, dsmpe );
        FLATTENCASEENUM( Drives::SWLIMIT, AntennaCommon::StateMonitorPointEnum::SWLIMIT, dsmpe );
        FLATTENCASEENUM( Drives::ERROR, AntennaCommon::StateMonitorPointEnum::ERROR, dsmpe );
      }
      comDrv.state().setValue( dsmpe );
      comDrvTrk.trackTolerance().setValue( (float)_drives->getTolerance() );
      comDrvTrk.azimuthRate().setValue( (float)_drives->getAzRate() );
      comDrvTrk.elevationRate().setValue( (float)_drives->getElRate() );

      if ( _drives->isKey() == false )
        bimaDrv.keySwitch().setValue( BimaSubsystem::KeySwitchMonitorPointEnum::ENABLE );
      else
        bimaDrv.keySwitch().setValue( BimaSubsystem::KeySwitchMonitorPointEnum::DISABLE );

      if ( _drives->isComputerCtl() == false )
        bimaDrv.controlSwitch().setValue( 
            BimaSubsystem::ControlSwitchMonitorPointEnum::MANUAL );
      else
        bimaDrv.controlSwitch().setValue( 
            BimaSubsystem::ControlSwitchMonitorPointEnum::COMPUTER );

      int stallLevel = 0;
      if ( _drives->getAzStalls() > 0 || _drives->getElStalls() > 0 )
      {
        if ( _drives->getAzStalls() > _drives->getElStalls() )
          stallLevel = _drives->getAzStalls();
        else
          stallLevel = _drives->getElStalls();
      }

      switch ( stallLevel )
      {
        FLATTENCASEENUM( 0, BimaSubsystem::AxisVelLimitMonitorPointEnum::NOLIMIT, avlmpe );
        FLATTENCASEENUM( 1, BimaSubsystem::AxisVelLimitMonitorPointEnum::SLOW, avlmpe );
        FLATTENCASEENUM( 2, BimaSubsystem::AxisVelLimitMonitorPointEnum::SLOWER, avlmpe );
        FLATTENCASEENUM( 3, BimaSubsystem::AxisVelLimitMonitorPointEnum::SLOWEST, avlmpe );
        default: avlmpe = BimaSubsystem::AxisVelLimitMonitorPointEnum::SLOWEST;
      }
      bimaDrv.axisVelLimit().setValue( avlmpe );

      float azErr = _drives->getAzErr();
      float azErrSky = _drives->getAzErrSky();
      float elErr = _drives->getElErr();
      float errSky = hypot( azErrSky, elErr );


      if(dsmpe == AntennaCommon::StateMonitorPointEnum::STOW ||
	 dsmpe == AntennaCommon::StateMonitorPointEnum::SNOW ||
	 dsmpe == AntennaCommon::StateMonitorPointEnum::STOP ||
	 dsmpe == AntennaCommon::StateMonitorPointEnum::DISABLE ||
	 dsmpe == AntennaCommon::StateMonitorPointEnum::HWLIMIT ||
	 dsmpe == AntennaCommon::StateMonitorPointEnum::SWLIMIT){
	comDrvTrk.azimuthAxisState().setValue(AntennaCommon::AzimuthAxisStateMonitorPointEnum::IRRELEVANT);
	comDrvTrk.elevationAxisState().setValue(AntennaCommon::ElevationAxisStateMonitorPointEnum::IRRELEVANT);
      }
      else{
	float trackThreshold = (float)_drives->getTolerance() * 60;  // convert to arcsec

	if(azErrSky <= trackThreshold/2.0){
	  comDrvTrk.azimuthAxisState().setValue(AntennaCommon::AzimuthAxisStateMonitorPointEnum::TRACK);
	}
	else if(azErrSky <= trackThreshold){
	  comDrvTrk.azimuthAxisState().setValue(AntennaCommon::AzimuthAxisStateMonitorPointEnum::CLOSE);
	}
	else{
	  comDrvTrk.azimuthAxisState().setValue(AntennaCommon::AzimuthAxisStateMonitorPointEnum::SLEW);
	}
	if(elErr <= trackThreshold/2.0){
	  comDrvTrk.elevationAxisState().setValue(AntennaCommon::ElevationAxisStateMonitorPointEnum::TRACK);
	}
	else if(elErr <= trackThreshold){
	  comDrvTrk.elevationAxisState().setValue(AntennaCommon::ElevationAxisStateMonitorPointEnum::CLOSE);
	}
	else{
	  comDrvTrk.elevationAxisState().setValue(AntennaCommon::ElevationAxisStateMonitorPointEnum::SLEW);
	}
      }

      if ( ::isinf( errSky ) ) 
          errSky = std::numeric_limits<float>::max();

      comDrvTrk.errorAzimuth().setValue( azErr );
      comDrvTrk.errorAzimuthSky().setValue( azErrSky );
      comDrvTrk.errorElevation().setValue( elErr );
      comDrv.errorSky().setValue( errSky );

      // this needs to be put down in some other object...
      _drives->getData( "SOURCE", sourcechr, 10 );

      // Sanitize string on way out...
      for ( int c = 0; c < 10; c++ )
        if ( isspace(*(sourcechr+c)) && *(sourcechr+c) != ' ' )
          *(sourcechr+c) = ' ';

      sourcechr[10] = '\0';

      comDrv.sourcename().setValue( string( sourcechr ) );

      comDrv.rightAscension().setValue( _drives->getRA() );
      comDrv.declination().setValue( _drives->getDec() );

      switch ( _drives->getModel() )
      {
        FLATTENCASEENUM( Drives::OPTICAL,
            AntennaCommon::RefractionModelMonitorPointEnum::OPTICAL,
            drmpe );
        FLATTENCASEENUM( Drives::RADIO,
            AntennaCommon::RefractionModelMonitorPointEnum::RADIO,
            drmpe );
      }
      comDrvP.refractionModel().setValue( drmpe );
      comDrvP.refraction().setValue( _drives->getRefract() );

      switch ( _drives->getMode() )
      {
        FLATTENCASEENUM( Drives::AZEL, AntennaCommon::ModeMonitorPointEnum::AZEL, dmmpe );
        FLATTENCASEENUM( Drives::EQUAT, AntennaCommon::ModeMonitorPointEnum::EQUAT, dmmpe );
        FLATTENCASEENUM( Drives::STOWMODE, AntennaCommon::ModeMonitorPointEnum::STOW, dmmpe );
        FLATTENCASEENUM( Drives::SNOWMODE, AntennaCommon::ModeMonitorPointEnum::SNOW, dmmpe );
        FLATTENCASEENUM( Drives::STOPMODE, AntennaCommon::ModeMonitorPointEnum::STOP, dmmpe );
      }
      comDrv.mode().setValue( dmmpe );

      switch ( _drives->getWrapLogic() )
      {
        FLATTENCASEENUM( Drives::ZERO, AntennaCommon::WrapLogicMonitorPointEnum::ZERO, wlmpe );
        FLATTENCASEENUM( Drives::ADD, AntennaCommon::WrapLogicMonitorPointEnum::ADD, wlmpe );
        FLATTENCASEENUM( Drives::SUB, AntennaCommon::WrapLogicMonitorPointEnum::SUB, wlmpe );
      }
      comDrvTrk.wrapLogic().setValue( wlmpe );

      if ( _drives->isKey() == false )
        comDrvTrk.emergencyOff().setValue(
            AntennaCommon::EmergencyOffMonitorPointEnum::OK );
      else
        comDrvTrk.emergencyOff().setValue(
            AntennaCommon::EmergencyOffMonitorPointEnum::OFF );

      if ( _drives->isComputerCtl() )
        comDrvTrk.manualSwitch().setValue(
            AntennaCommon::ManualSwitchMonitorPointEnum::OK );
      else
        comDrvTrk.manualSwitch().setValue(
            AntennaCommon::ManualSwitchMonitorPointEnum::MANUAL );

      comDrvP.offsetAz().setValue( _drives->getOffsetAz() );
      comDrvP.offsetEl().setValue( _drives->getOffsetEl() );

      comDrvP.mountOffsetAz().setValue( _drives->getMountOffsetAz() );
      comDrvP.mountOffsetEl().setValue( _drives->getMountOffsetEl() );

      comDrvPC.coefChange().setValue( _drives->getCoefChange() );


      DriveCommand::ApertureType curap = _drives->getAperture();
      switch ( curap )
      {
        FLATTENCASEENUM( DriveCommand::OPTICAL,
            AntennaCommon::SelectedApertMonitorPointEnum::OPTICAL, sampe );
        FLATTENCASEENUM( DriveCommand::RADIO1MM,
            AntennaCommon::SelectedApertMonitorPointEnum::RADIO1MM, sampe );
        FLATTENCASEENUM( DriveCommand::RADIO3MM,
            AntennaCommon::SelectedApertMonitorPointEnum::RADIO3MM, sampe );
        FLATTENCASEENUM( DriveCommand::RADIO1CM,
            AntennaCommon::SelectedApertMonitorPointEnum::RADIO1CM, sampe );
      }

      comDrvPC.selectedApert().setValue( sampe );

      double apc[_apcCount];
      _drives->getData( "APC", apc, _apcCount );
      for(int i=0; i < _apcCount; i++) {
        bimaDrvPC.apc(i).setValue(apc[i]);
      }

      double epc[_epcCount];
      _drives->getData( "EPC", epc, _epcCount );
      for(int i=0; i < _epcCount; i++) {
        bimaDrvPC.epc(i).setValue(epc[i]);
      }

      typedef AntennaCommon::SelectedApertMonitorPointEnum SelectedApertureMPE;

      AntennaCommon::ApertureCoefficients & com1mmApc = 
        comDrvPC.apertureCoefficients( SelectedApertureMPE::RADIO1MM );

      com1mmApc.crossElCollErr().setValue(
          _drives->getApertureOffsetAz( DriveCommand::RADIO1MM ));
      com1mmApc.elCollErr().setValue(
          _drives->getApertureOffsetEl( DriveCommand::RADIO1MM ));
      com1mmApc.sag().setValue(
          _drives->getApertureOffsetSag( DriveCommand::RADIO1MM ));

      bimaDrvPC.aprt1mmAz().setValue(
          _drives->getApertureOffsetAz( DriveCommand::RADIO1MM ));
      bimaDrvPC.aprt1mmEl().setValue(
          _drives->getApertureOffsetEl( DriveCommand::RADIO1MM ));
      bimaDrvPC.aprt1mmSag().setValue(
          _drives->getApertureOffsetSag( DriveCommand::RADIO1MM ));

      AntennaCommon::ApertureCoefficients & com3mmApc = 
        comDrvPC.apertureCoefficients( SelectedApertureMPE::RADIO3MM );
      com3mmApc.crossElCollErr().setValue(
          _drives->getApertureOffsetAz( DriveCommand::RADIO3MM ));
      com3mmApc.elCollErr().setValue(
          _drives->getApertureOffsetEl( DriveCommand::RADIO3MM ));
      com3mmApc.sag().setValue(
          _drives->getApertureOffsetSag( DriveCommand::RADIO3MM ));

      bimaDrvPC.aprt3mmAz().setValue(
          _drives->getApertureOffsetAz( DriveCommand::RADIO3MM ));
      bimaDrvPC.aprt3mmEl().setValue(
          _drives->getApertureOffsetEl( DriveCommand::RADIO3MM ));
      bimaDrvPC.aprt3mmSag().setValue(
          _drives->getApertureOffsetSag( DriveCommand::RADIO3MM ));

      AntennaCommon::ApertureCoefficients & com1cmApc = 
        comDrvPC.apertureCoefficients( SelectedApertureMPE::RADIO1CM );
      com1cmApc.crossElCollErr().setValue(
          _drives->getApertureOffsetAz( DriveCommand::RADIO1CM ));
      com1cmApc.elCollErr().setValue(
          _drives->getApertureOffsetEl( DriveCommand::RADIO1CM ));
      com1cmApc.sag().setValue(
          _drives->getApertureOffsetSag( DriveCommand::RADIO1CM ));

      bimaDrvPC.aprt1cmAz().setValue(
          _drives->getApertureOffsetAz( DriveCommand::RADIO1CM ));
      bimaDrvPC.aprt1cmEl().setValue(
          _drives->getApertureOffsetEl( DriveCommand::RADIO1CM ));
      bimaDrvPC.aprt1cmSag().setValue(
          _drives->getApertureOffsetSag( DriveCommand::RADIO1CM ));

      AntennaCommon::ApertureCoefficients & comOptApc = 
        comDrvPC.apertureCoefficients( SelectedApertureMPE::OPTICAL );
      comOptApc.crossElCollErr().setValue(
          _drives->getApertureOffsetAz( DriveCommand::OPTICAL ));
      comOptApc.elCollErr().setValue(
          _drives->getApertureOffsetEl( DriveCommand::OPTICAL ));
      comOptApc.sag().setValue(
          _drives->getApertureOffsetSag( DriveCommand::OPTICAL ));

      bimaDrvPC.aprtOpAz().setValue(
          _drives->getApertureOffsetAz( DriveCommand::OPTICAL ));
      bimaDrvPC.aprtOpEl().setValue(
          _drives->getApertureOffsetEl( DriveCommand::OPTICAL ));
      bimaDrvPC.aprtOpSag().setValue(
          _drives->getApertureOffsetSag( DriveCommand::OPTICAL ));

      comDrv.driveSeqNum().setValue( _drives->getCurSequenceNo() );
      // Waiting for mpml to catch up

      // Limits info for Drive system

      azswlimmpe = AntennaCommon::AzSwLimitMonitorPointEnum::OK;
      elswlimmpe = AntennaCommon::ElSwLimitMonitorPointEnum::OK;
      azhwlimmpe = AntennaCommon::AzHwLimitMonitorPointEnum::OK;
      elhwlimmpe = AntennaCommon::ElHwLimitMonitorPointEnum::OK;
      
      if ( _drives->getActiveState() == Drives::SWLIMIT )
      {

        if ( curAz >= _drives->getswAzHiLim() )
          azswlimmpe = AntennaCommon::AzSwLimitMonitorPointEnum::PLUSLIM;
        else if ( curAz <= _drives->getswAzLoLim() )
          azswlimmpe = AntennaCommon::AzSwLimitMonitorPointEnum::MINUSLIM;

        if ( curEl >= _drives->getswElHiLim() )
          elswlimmpe = AntennaCommon::ElSwLimitMonitorPointEnum::PLUSLIM;
        else if ( curEl <= _drives->getswElLoLim() )
          elswlimmpe = AntennaCommon::ElSwLimitMonitorPointEnum::MINUSLIM;

      }
      else if ( _drives->getActiveState() == Drives::HWLIMIT )
      {

        if ( curAz >= _drives->getswAzHiLim() )
          azhwlimmpe = AntennaCommon::AzHwLimitMonitorPointEnum::PLUSLIM;
        else if ( curAz <= _drives->getswAzLoLim() )
          azhwlimmpe = AntennaCommon::AzHwLimitMonitorPointEnum::MINUSLIM;

        if ( curEl >= _drives->getswElHiLim() )
          elhwlimmpe = AntennaCommon::ElHwLimitMonitorPointEnum::PLUSLIM;
        else if ( curEl <= _drives->getswElLoLim() )
          elhwlimmpe = AntennaCommon::ElHwLimitMonitorPointEnum::MINUSLIM;

      }

      comDrvLim.azSwLimit().setValue( azswlimmpe );
      comDrvLim.elSwLimit().setValue( elswlimmpe );
      comDrvLim.azHwLimit().setValue( azhwlimmpe );
      comDrvLim.elHwLimit().setValue( elhwlimmpe );
      
      _drives->getSoftLimits( azLow, azHigh, elLow, elHigh );
      comDrvLim.azLowSwLimitVal().setValue( azLow );
      comDrvLim.azHighSwLimitVal().setValue( azHigh );
      comDrvLim.elLowSwLimitVal().setValue( elLow );
      comDrvLim.elHighSwLimitVal().setValue( elHigh );

      _drives->getHardLimits( azLow, azHigh, elLow, elHigh );
      comDrvLim.azLowHwLimitVal().setValue( azLow );
      comDrvLim.azHighHwLimitVal().setValue( azHigh );
      comDrvLim.elLowHwLimitVal().setValue( elLow );
      comDrvLim.elHighHwLimitVal().setValue( elHigh );

      if ( _drives->safeCalled() == true )
      {
        _drives->getSafeRange( azLow, azHigh, elLow, elHigh );

        comDrv.safeAzLow().setValue( azLow );
        comDrv.safeAzHigh().setValue( azHigh );
        comDrv.safeElLow().setValue( elLow );
        comDrv.safeElHigh().setValue( elHigh );

        // This method of declaring safe/unsafe will update the condition on the
        // monitor point update rate (2Hz).  This may not be acceptable in the
        // future if checking on an antenna entering or leaving a safe range
        // needs a faster update.
        double adjAz;

        // curAz is current azimuth
        // adjAz is current azimuth adjusted by 2PI for comparison with safe range
        if ( curAz >= 360.0 ) 
          adjAz = curAz - 360.0;
        else
          adjAz = curAz;

        // adjust for safe ranges < 0
        if ( azLow <= 0.0 && (fabs(azLow) + curAz) >= 360.0 )
          adjAz = curAz - 360.0;

        if ( (adjAz > azLow && adjAz < azHigh) && (curEl > elLow && curEl < elHigh) ) {
          collisionmpe = AntennaCommon::SafeStateMonitorPointEnum::SAFE;
        // It is possible that both azLow and adjAz are less than zero 
        // but that adjAz plus 2PI would put it in the safe azimuth range.
        // That is, the antenna is physically at a safe azimuth, but on the negative
        // wrap.  So check for that case here.
        } else if (( ((adjAz+360) > azLow) && ((adjAz+360) < azHigh))  
                 &&  (curEl > elLow && curEl < elHigh) ) {
                collisionmpe = AntennaCommon::SafeStateMonitorPointEnum::SAFE;
        } else{
          collisionmpe = AntennaCommon::SafeStateMonitorPointEnum::UNSAFE;
        }
      }
      else
      {
        comDrv.safeAzLow().setValidity( MonitorPoint::INVALID_NO_DATA );
        comDrv.safeAzHigh().setValidity( MonitorPoint::INVALID_NO_DATA );
        comDrv.safeElLow().setValidity( MonitorPoint::INVALID_NO_DATA );
        comDrv.safeElHigh().setValidity( MonitorPoint::INVALID_NO_DATA );
        collisionmpe = AntennaCommon::SafeStateMonitorPointEnum::UNSAFE;
      }


      comDrv.safeState().setValue( collisionmpe );

      //  DRIVES MONITOR INFORMATION ---------------------------------------------

      //  STATUS BITS INFORMATION    ---------------------------------------------

      // Using the _drives obj to get at status bit info, but any of the
      // TelemetryClient derived objects will do...
      bimaStatus.computerCtl().setValue( _drives->isComputerCtl() );
      bimaStatus.azLimit().setValue( _drives->isAzLim() );
      bimaStatus.elLimit().setValue( _drives->isElLim() );
      bimaStatus.azULimit().setValue( _drives->isAzULim() );
      bimaStatus.elULimit().setValue( _drives->isElULim() );
      bimaStatus.key().setValue( _drives->isKey() );
      bimaStatus.wtrPrssr().setValue( _drives->isWtrPrssrNotNorm() );
      bimaStatus.azDrvBoxTNorm().setValue( _drives->isAzDrvTempNorm() );
      bimaStatus.elDrvBoxTNorm().setValue( _drives->isElDrvTempNorm() );
      bimaStatus.rxTNorm().setValue( _drives->isRxTempNorm() );
      bimaStatus.cabinTNorm().setValue( _drives->isCabTempNorm() );
      bimaStatus.spareTNorm().setValue( _drives->isSpareTempNorm() );
      bimaStatus.collision().setValue( _drives->isCollision() );
      bimaStatus.collisionOff().setValue( _drives->isCollisionDectOff() );
      bimaStatus.cameraFlapSafe().setValue( _drives->isCameraSafe() );
      bimaStatus.cameraFlapOpen().setValue( _drives->isCamFlapOpen() );
      bimaStatus.cameraFlapBypass().setValue( _drives->isCamFlapBypass() );
      bimaStatus.cabinPwrOff().setValue( _drives->isCabPwrOff() );

      bimaStatus.statusByteAErr().setValue( _drives->getNumByteAErrs() );
      bimaStatus.statusByteBErr().setValue( _drives->getNumByteBErrs() );
      bimaStatus.statusByteCErr().setValue( _drives->getNumByteCErrs() );

      //  STATUS BITS INFORMATION    ---------------------------------------------

      //  TEMPERATURES INFORMATION    ---------------------------------------------
      // Need to make a more generic container to pick up these anncillary
      // temps/cabin related points
      bimaTemps.tcabinTop().setValue( _rx->thermistor(_rx->atodin("T1FNOUT")*THERMI));
      bimaTemps.twaterIn().setValue( _rx->thermistor(_rx->atodin("T2FNOUT")*THERMI));
      bimaTemps.twaterOut().setValue( _rx->thermistor(_rx->atodin("T3FNOUT")*THERMI));
      bimaTemps.tcabinFan().setValue( _rx->thermistor(_rx->atodin("T4FNOUT")*THERMI));
      bimaTemps.tsubrefl().setValue( _rx->thermistor(_rx->atodin("T13FNOUT")*THERMI));
      //  TEMPERATURES INFORMATION    ---------------------------------------------

      //  STATUS INFO INFORMATION    ---------------------------------------------
      bimaSp.rxStatusInfo().setValue( _rx->getStatusInfo() );
      //  STATUS INFO INFORMATION    ---------------------------------------------
      _readMonPnts->readNewest();
      setAntCommonLoMonitorPoints( antComLO, *_rx, *_lo, 
                                   yigstate, lostate, varactor, strobe );
      setAntCommonRxMonitorPoints( antComRx, *_rx, rxGoodmpe,
                                   yigstate, lostate );

      antCom.optics().opticsSeqNum().setValue( _rx->getCurOpticsSequenceNo() );

      antCom.location().latitude().setValue( _drives->getLatitude() );
      antCom.location().longitude().setValue( _drives->getLongitude() );
      antCom.location().altitude().setValue( _drives->getAltitude() );

      // ---- retrieve a few sisrx monitor pts used by tuning algorithms, put into bima shared memory ----
      float vj = _monPnts->rx1mm().sisReceiver().actualVj().getValue() ;
      _rx->putData("VJ", &vj) ;
      float ij = _monPnts->rx1mm().sisReceiver().actualIj().getValue() ;
      _rx->putData("IJ", &ij) ;
      int sisseqno = _monPnts->antennaCommon().receivers().tuneSeqNum().getValue() ;
      _rx->putData("SISSEQNO", &sisseqno) ;
//      SisRxTuneState::TUNESTATE tuneState = _monPts->rx1mm().sisReceiver().tuneState().getValue();

      _monPnts->timestamp().setValue( Time::MJD() ); 
      _monPnts->online().setValue(true);

    }
    catch ( ErrorException &eex )
    {
      CPTRACE(Trace::TRACE2,"ERROR");
      _logger << Priority::WARN << "MonitorUpdater thread threw exception! "
        << eex.what();
      sleep(1);
    }
  }
}

// vim: set expandtab sw=2 ts=2 cindent :

