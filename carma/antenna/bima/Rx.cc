/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.98 $
 * $Date: 2013/12/09 17:40:33 $
 * $Id: Rx.cc,v 1.98 2013/12/09 17:40:33 iws Exp $
 */


// CARMA includes
#include "carma/antenna/bima/Rx.h"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::services;
using namespace carma::antenna::bima;
using namespace carma::antenna::common;

const int Rx::MAX_HIST; // see Rx.h
const Rx::SISSetMode Rx::SISv; // see Rx.h
const Rx::SISSetMode Rx::SISi; // see Rx.h
const Rx::SISSetMode Rx::SISo; // see Rx.h

const int Rx::MAX_SCAN_STEPS;

const unsigned short AntennaIFClient::SET_IF_LEVEL;
const unsigned short AntennaIFClient::SET_IF1_LEVEL;
const unsigned short AntennaIFClient::SET_IF2_LEVEL;
const unsigned short AntennaIFClient::SELECT_IF_BAND;
const unsigned short AntennaIFClient::SET_IF_ATTEN;
const unsigned short AntennaIFClient::SET_IF1_ATTEN;
const unsigned short AntennaIFClient::SET_IF2_ATTEN;
const unsigned short AntennaIFClient::START_IF1_FASTSAMP;
const unsigned short AntennaIFClient::STOP_IF1_FASTSAMP;
const unsigned short AntennaIFClient::START_IF2_FASTSAMP;
const unsigned short AntennaIFClient::STOP_IF2_FASTSAMP;

// Magical mystical IF power setting
#define TARGETIFPOWER 0.25
#define AMBIFPOWER 0.30

Rx::Rx( Configuration& config )
  : TelemetryClient( config ), _config( config )
{
  CPTRACE( Trace::TRACE3,"START rx");
  _name = "rx";
  _mmoscAD = new Motor( string( "mmoscAD" ), _config );
  _mmbckAD = new Motor( string( "mmbckAD" ), _config );
  _attnD = new Motor( string( "attnD" ), _config );
  _mmoscB = new Motor( string( "mmoscB" ), _config );
  _mmbckB = new Motor( string( "mmbckB" ), _config );
  _attnB = new Motor( string( "attnB" ), _config );
  _oscADtab = loadOscTable( config.getoscADFile() );
  _oscBtab = loadOscTable( config.getoscBFile() );
  // CM focus are offsets from the 3MM value
  _cmFocusOffset = loadFocusTable( config.getCmFocusConfFile() );
  _logger << Priority::INFO << "Have focus offset " << _cmFocusOffset;
  _attensafe = 18000; // default...
  CPTRACE(Trace::TRACE3,"DEWAR");
  _dewar = new Dewar( config );
  CPTRACE( Trace::TRACE1, "Rx - " << _dewar );
  _pol = new Polarizer( config );
  _band = getBand();            	// returns char; deprecated
  _iband = bandCheck();                 // returns integer 0-4
  _if = new AntennaIFClient( config );
  CPTRACE( Trace::TRACE1, "Rx - Acquired AntennaIFClient handle" );
  _calwheel = new CalWheel( config );
  setNextTuneSequenceNo( (int)0 );
  setCurTuneSequenceNo( (int)0 );
  _tmmFocusPosition = -100.0;
}


Table* Rx::loadOscTable( string name )
{
  Table *osctab;
  try
  {
    osctab = new Table( name );
  }
  catch ( carma::util::FileNotFoundException &fnfex )
  {
    ostringstream oss;
    oss << fnfex.what() << ".  Check entry for '"
      << _config.getAntenna() << "' in file "
      << _config.getDescTableFile();
    throw CARMA_ERROR( oss.str() );
  }
  if ( osctab == (Table *)0 )
    throw CARMA_ERROR( string( "Table pointer null after attempting to open: " ) + name );
  return osctab;
}

float Rx::loadFocusTable( string name )
{
  Table foctab(name);
  _logger << Priority::INFO << "Getting focus";
  vector<string> antName = foctab.getColumn( "Ant" );
  vector<string>::iterator i;
  int entry = 0;
  for ( i = antName.begin(); i != antName.end(); ++i )
  {
    CPTRACE( Trace::TRACE3, "AntName '" << *i << "'" );
    if ( i->compare( _config.getAntenna() ) == 0 )
      return foctab.getDoubleColumn("Zoffset").at(entry);
    entry++;
  }
  //  SHOULD THROW HERE
  return 0;
}

void Rx::turnMMOscOn()
{
  unsigned char mask = 0x40;
  unsigned char bits = 0x40;
  setbits( "BITSTO60", bits, mask );
}

void Rx::turnMMOscOff()
{
  unsigned char mask = 0x40;
  unsigned char bits = 0x00;
  setbits( "BITSTO60", bits, mask );
}

bool Rx::getOscOn()
{
  int bits;
  getData( "BITSTO60", &bits, 1 );
  return ( (bits & 0x40) != 0 ) ? true : false;
}

Rx::OnOrOffState Rx::getMMOscState()      // converts from boolean to enum 
{
  OnOrOffState state;
  if ( getOscOn() )
    state = Rx::ON;
  else 
    state = Rx::OFF;
  return state;
}

void Rx::turnMMSweepOn()
{
  unsigned char mask = 0x80;
  unsigned char bits = 0x00;
  setbits( "BITSTO60", bits, mask );
}

void Rx::turnMMSweepOff()
{
  unsigned char mask = 0x80;
  unsigned char bits = 0x80;
  setbits( "BITSTO60", bits, mask );
}

Rx::OnOrOffState Rx::getSweepState()
{
  int bits;
  getData( "BITSTO60", &bits, 1 );
  return ( (bits & 0x80) != 0 ) ? Rx::OFF : Rx::ON;
}

Rx::OKOrNOTState Rx::getPhaseLockRefState()
{
  OKOrNOTState state;
  if ( checkPhaseLockRef() )
    state = Rx::OK;
  else
    state = Rx::NOTOK;
  return state;
}


// notes on bands:
//  iband=0, _band='A'   unused 70-90 GHz band
//  iband=1, _band='B'   3mm band
//  iband=2, _band='C'   unused 2mm band
//  iband=3, _band='D'   1mm band
//  iband=4, _band='E'   1cm band

// --- accept band as char A-E, save as integer (0-4) in shared memory, set all switches ---
void Rx::setBand( Rx::Band band )
{
  _band = toupper( band );
  _iband = band - 65 ;
  int iband = band - 65 ;
  if ( (_iband < 0) || (_iband > 4) ) {
    _errMsg.seekp(0);
    _errMsg << "Band Check, invalid band: " << _band;
    _logger << Priority::WARN << "Invalid band: " << _band;
    throw CARMA_ERROR( _errMsg.str() );
  }
  _logger << Priority::INFO << "Setting band " << _iband;
    //cout << "setBand setting band to " << _band << endl;
  putData("IBAND", &iband);	   // save to shared memory
  selectMotorsAndMod( iband );    // set up pointers to motors
  selectMirror( iband ) ;	  // move cm/mm mirror into position
  setDewarBand( iband );          // set hardware register for dewar control
  setPhaseLockBand( iband );	  // set Agilent switch and mmVop switch
  setIFSwitch( iband );           // set IF switch (through the PAM)
  setDewarWindow( iband );        // set dewar window position (for calwheel)
  _logger << Priority::INFO << "Setting focus in setBand";
  setFocus( iband );            // set the focus, but only if we are changing bands
}

void Rx::setFocus( int iband ){
  FocusControl_var foc;
  if(_tmmFocusPosition < -30.0 ){
    _logger << Priority::INFO << "Getting tmm focus";
    getData("TMMFOCUS", &_tmmFocusPosition);
    _logger << Priority::INFO << "Have 3mm focus " << _tmmFocusPosition;
  }
    
  for(int i = 0; i < 3; i++){
    _logger << Priority::INFO << "resolveName to get focus.";
    corba::Client & client = Program::getProgram().getCorbaClient();
    foc = client.resolveName<carma::antenna::common::FocusControl>("carma." + _config.getAntenna() + "." + FOCUS_NAME);
    if(!(CORBA::is_nil(foc))){
      // jump out of the for loop if we have a connection
      break;
    } else if(i == 2) {
      // if we have tried 3 times then quit with an error
      _logger << Priority::INFO << "System reference not found for focus control.";
      return;
    } else {
      sleep(10);
    }
  }
  _logger << Priority::INFO << "Setting focus";
  int seqNo = 1234;
  if(iband == 1){
    _logger << Priority::INFO << "Focus 3mm " << _tmmFocusPosition;
    foc->setZ(_tmmFocusPosition,seqNo);
    _logger << Priority::INFO << "Focus 3mm " << _tmmFocusPosition << " done";
  }
  else if(iband == 3){
    _logger << Priority::INFO << "Focus 1mm " << _tmmFocusPosition;
    foc->setZ(_tmmFocusPosition,seqNo);
    _logger << Priority::INFO << "Focus 1mm " << _tmmFocusPosition << " done";
  }
  else if(iband == 4){
    _logger << Priority::INFO << "Focus 1cm " << _tmmFocusPosition << "+" << _cmFocusOffset;
    foc->setZ(_tmmFocusPosition + _cmFocusOffset,seqNo);
    _logger << Priority::INFO << "Focus 1cm " << _tmmFocusPosition + _cmFocusOffset << " done";
  }
}


// --- reads iband from shared memory, returns integer from 0-4 ---
int Rx::bandCheck()
{
  int iband;
  getData("IBAND", &iband);
  return iband;
}

// --- reads iband from shared memory, returns char from A-E ---
char Rx::getBand()
{
  int iband = bandCheck(); 
  char bands[5] = { 'A', 'B', 'C', 'D', 'E'};
  _band = bands[ iband ];		// note: _band is global variable
  return _band;
}

// --- for lab tests it's possible to override normal band selection for dewar window ---
void Rx::setDewarWindow( int iband )
{
  cout << "... setting dewar window to " << iband << endl;
  putData( "DEWARWINDOW", &iband ) ;
}

int Rx::getDewarWindow()
{
  int dewarwindow;
  getData( "DEWARWINDOW", &dewarwindow );
  return dewarwindow ;
}

// --- selects between motors for bands B and D ---
void Rx::selectMotorsAndMod( int iband )
{
  if ( iband == 0 || iband == 3 ) {
//    cout << "... selectMotorsandMod band is D " << endl;
    _modulator = _config.hasModulatorD();
    _mmosc = _mmoscAD;
    _mmbck = _mmbckAD;
    _attn = _attnD;
  } else if ( iband == 1 ) {
//    cout << "... selectMotorsandMod band is B " << endl;
    _modulator = _config.hasModulatorB();
    _mmosc = _mmoscB;
    _mmbck = _mmbckB;
    _attn = _attnB;
  } 
}

// --- sets dewar hardware register (SIS and WBA13) based on iband ---
void Rx::setDewarBand()
{
  setDewarBand( bandCheck() );
}

void Rx::setDewarBand( int iband )
{
  unsigned char ifbits[5] = {0, 1, 2, 3, 1};                /* dewar; wrap band E = band B */
  unsigned char mask = 0x03;
  unsigned char bits = ifbits[ iband ];
  printf("... switch receiver bias to band %d\n", bits );
  setbits( "BITSTO50", bits, mask );
}

// --- read dewar band set in hardware ---
int Rx::getDewarBand()
{
  int bits;
  getData( "BITSTO50", &bits, 1 );
  return (bits & 0x03) ;
}

// --- sets phaselock switch hardware register based on iband ---
void Rx::setPhaseLockBand()
{
  setPhaseLockBand ( bandCheck() );
}

void Rx::setPhaseLockBand( int iband )
{
  unsigned char phbits[5] = {0, 1, 2, 0, 3};      /* mmVop and HP switch */
  unsigned char bits = phbits[ iband ] << 2;
  unsigned char mask = 0x0c;
  printf("... switch mm phaselock to band %d\n", iband);
  _logger << Priority::INFO << "Setting MM Phase Lock Band: " 
          << iband << ",  bits=" << int(bits);
  setbits("BITSTO60", bits, mask);
}

// --- get phaselock band from hardware, return integer 0-3  ---
int Rx::getPhaseLockBand()
{
  int bits;
  getData( "BITSTO60", &bits, 1 );
  bits = ((bits & 0x0c) >> 2); 
  return bits;
}

// --- move cm/mm mirror into proper position ---
// set bit whether or not cm optics are installed on this telescope
// since the mirror takes 10-20 secs to move, do NOT wait until it reaches
//   the target position; MonitorUpdater is responsible for reporting
//   actual mirror position
void Rx::selectMirror( int iband )
{
  char mask = 0x40 ;
  char mmPos = 0x00 ;
  char cmPos = 0x40 ;
  if ( iband == 4  ) {
    cout << "... send flip mirror to cm position" << endl;
    setbits( "BITSTO50", cmPos, mask ) ;
  } else {  
    cout << "... send flip mirror to mm position" << endl;
    setbits( "BITSTO50", mmPos, mask ) ;
  }
}

// --- sets IF switch based on iband ---
void Rx::setIFSwitch()
{
  setIFSwitch( bandCheck() );
}

void Rx::setIFSwitch( int iband )
{
  unsigned char ifbits[5] = {0, 1, 2, 3, 4};                /* IF switch; band E = pos 0 */
  unsigned char mask = 0x03;
  unsigned char bits = ifbits[ iband ];
  printf("... set IF switch for band %d\n", iband );
  setbits( "BITSTO60", bits, mask );                               // set telem; does nothing but saves state
  short ibnd = iband ;
  _if->command( AntennaIFClient::SELECT_IF_BAND, ibnd);   // set via PAM, CANbus
}

void Rx::setVop( double Vop )
{
  unsigned short vop_counts;
  setVopCommanded( Vop );
  vop_counts = (unsigned short)(( Vop - 7.5 ) * 11.25 * 204.8 / 2.5);
  CPTRACE( Trace::TRACE3, "Set Vop to " << Vop << " Volts, or " << vop_counts << " counts" );
  tpoke( "MMOSCVOP", vop_counts );
}

bool Rx::mmLockStatus()
{
  int bits;
  getData( "BITSIN60", &bits, 1 );
  return ( (bits & 0x01) == 0 ) ? false : true;
}

int Rx::getMMLockInfo()
{
  int value;
  getData( "MMLOCKINFO", &value, 1 );
  return value;
}

void Rx::setMMLockInfo( int value )
{
  putData( "MMLOCKINFO", &value, 1 );
}

double Rx::getVop()
{
  double vop_readback = 0.0;

  if ( _band == 'A' || _band == 'D' )
    vop_readback = getVopAD();
  else if ( _band == 'B' )
    vop_readback = getVopB();

  return vop_readback;
}

double Rx::getVopAD()
{
    return (double)( atodin( "MMOSCVOPAD" ) * VOLTS10 );
}

double Rx::getVopB()
{
    return (double)( atodin( "MMOSCVOPB" ) * VOLTS10P );
}

double Rx::getDrainCurrent(unsigned short deviceID){
  return _dewar->getDrainCurrent(deviceID);
}

double Rx::getGateVoltage(unsigned short deviceID){
  return _dewar->getGateVoltage(deviceID);
}

double Rx::getIFCurrent(){
  return _dewar->getIFCurrent();
}

bool Rx::checkPhaseLockRef()
{
  int status;
  getData( "BITSIN60", &status, 1 );
  return ( (( status & 0x02 ) != 0 ) ? true : false );
}

// --- finds mmosc tuning parameters by interpolation of tuning table ---
// --- restrictions on tuning table layout: (1) begins with highest freq!
//                                          (2) 2 lines with identical freq mark discontinuities!
//
void Rx::getTuningParameters( double &lo_ghz, unsigned short &mmosctarg,
                              unsigned short &mmbcktarg, double &voptarg,
                              double &lgaintarg )
{
  vector<double> loCol, vopCol, lgainCol;
  vector<int> mmoscCol, mmbckCol;
  int mmosc1,mmosc2,mmosc3;
  int mmbck1,mmbck2,mmbck3;
  double vop1,vop2,vop3;
  double lgain1,lgain2,lgain3;
  double lo1,lo2,lo3;
  Table *osctab;

  if ( _band == 'A' || _band == 'D' )
    osctab = _oscADtab;
  else
    osctab = _oscBtab;
  if ( osctab == (Table *)0 )
  {
    _errMsg << "osctab pointer came up null, should not happen, aborting...";
    throw CARMA_ERROR( _errMsg.str() );
  }

  loCol    = osctab->getDoubleColumn(0);
  mmoscCol = osctab->getIntColumn(1);
  mmbckCol = osctab->getIntColumn(2);
  vopCol   = osctab->getDoubleColumn(3);
  lgainCol = osctab->getDoubleColumn(4);

  int i = 2;
  int l = osctab->getNrows();

  if ( l > 1 )
  {
    lo1    = loCol.at(0);
    mmosc1 = mmoscCol.at(0);
    mmbck1 = mmbckCol.at(0);
    vop1   = vopCol.at(0);
    lgain1 = lgainCol.at(0);
  
    lo2    = loCol.at(1);
    mmosc2 = mmoscCol.at(1);
    mmbck2 = mmbckCol.at(1);
    vop2   = vopCol.at(1);
    lgain2 = lgainCol.at(1);
  }
  else
  {
    _errMsg << "Tuning parameters file does not have at least two entries!";
    _logger << Priority::ERROR << _errMsg.str();
    throw CARMA_ERROR( _errMsg.str() );
  }
  while ( lo_ghz < lo2 && i < l )
  {
    lo3    = loCol.at(i);
    mmosc3 = mmoscCol.at(i);
    mmbck3 = mmbckCol.at(i);
    vop3   = vopCol.at(i);
    lgain3 = lgainCol.at(i);
    lo1 = lo2; mmosc1 = mmosc2; mmbck1 = mmbck2; vop1 = vop2; lgain1 = lgain2;
    lo2 = lo3; mmosc2 = mmosc3; mmbck2 = mmbck3; vop2 = vop3; lgain2 = lgain3;
    i++;
  }
  voptarg = vop1 + (lo_ghz - lo1) * (vop2 - vop1) / (lo2 - lo1);
  lgaintarg = lgain1 + (lo_ghz - lo1) * (lgain2 - lgain1) / (lo2 - lo1);
  mmosctarg = (unsigned short)(mmosc1 + (lo_ghz - lo1) * (mmosc2 - mmosc1) / (lo2 - lo1));
  mmbcktarg = (unsigned short)(mmbck1 + (lo_ghz - lo1) * (mmbck2 - mmbck1) / (lo2 - lo1));
}

/*  lockmmosc is called by lockmm; it steps the mmosc motor slowly through the search
 *  range until it phaselocks with acceptable error volts
 */
void Rx::lockmmosc ( int osctarget, double maxerrv )
{
  int delta = 200;
/*  int hi_limit = 32768; */
  const int ssSize = 80;
  char ss[ssSize];

  setMMLockInfo( 1 ); // 1 is search in this context, needs to be made a const...

  int targ1 = osctarget + delta;
  int targ2 = osctarget - delta;
  snprintf(ss, ssSize, "move mmosc to beginning of search range %d to %d",targ1,targ2);
  printf("%s\n", ss);
  _logger << Priority::INFO << ss ;
//  printf("... using mmosc: %s\n",_mmosc->getName() );
  _mmosc->moveToTarget( targ1 + 100 );	// move above target to avoid backlash
  _mmosc->moveDown( targ1 );	

  unsigned short pos, oldpos;
  bool lockstat = false;
  bool didLock = false;
  int tries = 0;
  double errv;
  unsigned short speed = _mmosc->_slow ;
  oldpos = 32768;
  int n;
  int nsteps = 0;	// keeps track of how often we resend the toggle stuff

  _mmosc->enableLock();
  _mmosc->setDirection( Motor::DOWN );
  snprintf(ss, ssSize, "beginning scan through search range; speed = %d", speed);
  printf("%s\n", ss);
  _logger << Priority::INFO << ss ;

  while ( ( pos = _mmosc->position() ) > targ2 )
  {
    lockstat = mmLockStatus() ;

    if ( (pos < targ1) && lockstat )
    {
      setMMLockInfo( 2 ); // 2 - optimizing, needs to be made a const
      didLock = true;
      errv = mmErrorVolts() ;
      snprintf(ss, ssSize, "... pos %5d  lock %d  errv %5.2f", pos, lockstat, errv);
      // _logger << Priority::INFO << ss ;
      printf("%s\n", ss);

      if ( errv < maxerrv ) {		// lock on large neg error volt is allowed! better than no lock at all
	_mmosc->disableToggleBit();
        printf("... stop the motor\n") ;
        for (n=0; n<10; n++) { 
	  printf("... pos %5d  lock %d  errv %5.2f\n", _mmosc->position(), mmLockStatus(), mmErrorVolts() );
	}
	printf("... wait 2 sec for lock to stabilize\n") ;
        snprintf(ss, ssSize, "Lock MM Osc: locked at %d (predicted %d); errv %5.2f (max allowed %5.2f)",
	   _mmosc->position(), osctarget, mmErrorVolts(), maxerrv); 
        printf("%s\n", ss);
        _logger << Priority::INFO << ss ;
	setMMLockInfo( 0 );
	return;				// this is the desired exit point
      }
    }

    if (nsteps-- <= 0) {
      _mmosc->step( speed );
      nsteps = 50;
    }
    usleep( 10000 );
    if ( pos < oldpos ) {
      oldpos = pos;
      tries = 0;
    } else if ( tries++ > 500 ) {
      _errMsg.seekp(0);
      _errMsg << "Lock MM Osc: mmosc motor is stuck at " << pos << ", won't tune down "
	<< "after " << tries << " tries";
      _logger << Priority::WARN << _errMsg.str();
      printf("%s\n", _errMsg.str().c_str() );
      throw CARMA_ERROR( _errMsg.str() );
    }
  } // while ( ( pos = _mmosc->position() ) > targ2 )

  cout << "exiting the loop; didLock = " << didLock << endl;

  _mmosc->disableToggleBit();
  setMMLockInfo( 0 ); // nothing happening, should be made a const...

  if ( didLock )
  {
    _errMsg.seekp(0);
    _errMsg << "Lock MM Osc: lost lock while zeroing error volts (maxerrv: "
      << maxerrv <<")";
    _logger << Priority::WARN << _errMsg.str();
    cout << _errMsg.str() << endl;
    throw (int)1;
  }
  else
  {
    _errMsg.seekp(0);
    _errMsg << "Lock MM Osc: no lock (searched " << targ1 << " to " << targ2 << ")";
    _logger << Priority::WARN << _errMsg.str();
    cout << _errMsg.str() << endl;
    throw (int)1;
  }
}

void Rx::lockmm( double oscfreq )
{
  unsigned short mmosctarg, mmbcktarg, lastMMosc;
  int intLastMMosc;
  double lastOscfreq, lastLgain;
  double voptarg, lgaintarg, lgain;
  const int ssSize = 100;
  char ss[ssSize];
  
  setOscFreq( oscfreq );	// put oscillator freq in shared memory

  if ( _band == 'A' || _band == 'D' ) {
    cout << "osc file: " << _config.getoscADFile() << endl;
    getData( "LASTD_OSCFREQ", &lastOscfreq );
    getData( "LASTD_MMOSC", &intLastMMosc );
    getData( "LASTD_LGAIN", &lastLgain );
  }
  else {
    cout << "osc file: " << _config.getoscBFile() << endl;
    getData( "LASTB_OSCFREQ", &lastOscfreq );
    getData( "LASTB_MMOSC", &intLastMMosc );
    getData( "LASTB_LGAIN", &lastLgain );
  }
  lastMMosc = intLastMMosc;     // converts to short

  turnMMOscOn();
  turnMMSweepOn();
  snprintf(ss, ssSize, "Loading tuning parameters for oscfreq = %.3f GHz", oscfreq );
  _logger << Priority::INFO << ss;
  getTuningParameters( oscfreq, mmosctarg, mmbcktarg, voptarg, lgaintarg );

  CPTRACE( Trace::TRACE2, "mmosctarg: " << mmosctarg << " mmbcktarg: " << mmbcktarg
      << " voptarg: " << voptarg << " lgaintarg: " << lgaintarg );

  // Rescale mmosctarg and mxr_targ to correct for error in Vref
  short admax = _mmosc->getADMax();
  if ( admax < 32600 ) {
    snprintf(ss, ssSize, "admax from LO plate is %d, unusually low (normally > 32600)", admax);
    printf("%s\n",ss);
    _logger << Priority::WARN << ss ;
  }
  if ( admax > 30000 ) {
    mmosctarg = (short)nearbyint( mmosctarg * (float)admax/32750 );
    mmbcktarg = (short)nearbyint( mmbcktarg * (float)admax/32750 );
    snprintf(ss, ssSize, "scaling mmosc and mmbck by %d/32768; new targets are mmosc = %d, mmbck = %d",
	admax, mmosctarg, mmbcktarg );
    _logger << Priority::INFO << ss ;
  }  

  setVop( voptarg );
  usleep( 500000 ); // sleep half a second...
  double vop_readback = getVop();
  snprintf(ss, ssSize, "Set mmVop to %.2f V; readback %.2f V", voptarg, vop_readback);
  if ( fabs( vop_readback - voptarg ) > 0.5 )
    _logger << Priority::WARN << ss;
  else
    _logger << Priority::INFO << ss;

  if ( _modulator == true )
    setModulator( 300.0 );

  if ( ( _attn->position() < _attensafe && (abs(mmosctarg - _mmosc->position()) > 200) ) ) {
    _logger << Priority::INFO << "Increasing attenuator before tuning mmosc";
    printf("setting attenuator to %d\n",_attensafe);
    _attn->moveToTarget( _attensafe );
  }

  // tune mmbck position if needed
  short delta_mmbck = 30;
  if ( abs( _mmbck->position() - mmbcktarg) > delta_mmbck ) {
    setMMLockInfo( 1 ); // 1 is  SEARCH
    _logger << Priority::INFO << "Tuning mm backshort to " << mmbcktarg ;
    printf("tuning mmbck to %d\n",mmbcktarg);
    _mmbck->moveToTarget( mmbcktarg + 300);	// start from above target to avoid backlash
    _mmbck->moveToTarget( mmbcktarg );
    setMMLockInfo( 0 ); // 0 is LOCK - to avoid getting stuck in SEARCH
  }
  else {
    _logger << Priority::INFO << "... mmbck already at correct position";
    printf("... mmbck already at correct position\n");
  }


  // if mmosc was previously locked within 20 MHz of this freq:
  //  o restore previous loop gain
  //  o skip tuning if:
  //       - mmosc is locked & 
  //       - motor position matches lastMMosc position &
  //       - error volts is in range
  int doit = 1;
  if (fabs(oscfreq - lastOscfreq) < .02) {
    setLoopGain( lastLgain );
    if ( mmLockStatus() && (abs(_mmosc->position() - lastMMosc) < 20) && (abs(mmErrorVolts()) < 3.)) {
    _logger << Priority::INFO << "... mmosc already locked at correct frequency";
      printf("... mmosc already locked at correct frequency\n"); 
      doit = 0;
    } 
  }

  // set mm phaselock loop gain 0.5 V above table value to lock it up initially
  if (doit) {
    lgain = lgaintarg + 0.5;
    snprintf(ss, ssSize, "Setting mm phaselock loop gain to %.2f", lgain);
    _logger << Priority::INFO << ss ;
    printf("%s\n",ss);
    setLoopGain( lgain );

    double maxerrv = 1.0;   // on first attempt, try to keep errv < 1. 
    try
    { 
      if ( mmLockStatus() == false || (abs(mmosctarg - _mmosc->position()) > 20) ) {
        printf("entering lockmmosc; mmosctarg = %d, maxerrv = %.2f\n",mmosctarg,maxerrv);
        lockmmosc( mmosctarg, maxerrv );
      }
      else {
        setMMLockInfo( 0 ); // Make sure value in lock info isn't stale...
      }
    }
    catch ( int lockErr )
    {
      // Try again with larger maxerrv, and squelch int exception if it fails again...
      maxerrv = 3.0; 
      printf("  retrying lockmmosc; mmosctarg = %d, maxerrv = %.2f\n",mmosctarg,maxerrv);
      try
      {
        lockmmosc( mmosctarg, maxerrv );
      }
      catch ( int lockErr )
      {
        double zero = 0.;  // zero lasttune freq to indicate failure
        if ( _band == 'A' || _band == 'D' )
          putData( "LASTD_OSCFREQ", &zero );
        else
          putData( "LASTB_OSCFREQ", &zero );
        throw CARMA_ERROR ( string( "Failed to lock mm osc after two attempts" ) );
      }
    }
    printf("optimize loop gain\n");
    setStatusInfo( "optimize mm phaselock loop gain" );
    optimizeLoopGain( lgaintarg );		// default value is lgaintarg

    // store lasttune values
    intLastMMosc = _mmosc->position();	        // note: converting from short to integer
    lastLgain = getLoopGain();			// final optimized loop gain
    if ( _band == 'A' || _band == 'D' ) {
      putData( "LASTD_OSCFREQ", &oscfreq );
      putData( "LASTD_MMOSC", &intLastMMosc );
      putData( "LASTD_LGAIN", &lastLgain );
    }
    else {
      putData( "LASTB_OSCFREQ", &oscfreq );
      putData( "LASTB_MMOSC", &intLastMMosc );
      putData( "LASTB_LGAIN", &lastLgain );
    }
  }
  _logger << Priority::INFO <<  "finished locking mmosc";
  printf("finished locking mmosc\n");
}

// retrieve lasttune values, skip optimization if mixer was previously tune to this freq
//   AND mixer temp within 0.2 K of saved value AND Ij is within 2 uA
// must set Vj to saved value before measuring Ij, since we may have been tuned to
//   the other freq band
// note 1: LASTx_OSCFREQ is used by mmlock, whereas LASTx_LO1 is used by tune;
//   by the time we are here, LASTx_OSCFREQ is guaranteed to match oscfreq - that 
//   does not signify that mixer bias has been optimized!
// note 2: the SIS Rx module handles only the 1mm band; so its Vj should already
//   be correct if the other conditions are fulfilled

bool Rx::alreadyTuned()
{
  bool tuned = false;		// default is to reoptimize		
  double lastLO1, lastTemp, lastVbias, lastIbias;
  const int ssSize = 80;
  char ss[ssSize];

  try {
    if (getDewarBand() == 3) { 
      getData( "LASTD_LO1", &lastLO1 );
      getData( "LASTD_TEMP", &lastTemp );
      getData( "LASTD_VBIAS", &lastVbias );
      getData( "LASTD_IBIAS", &lastIbias );
    } 
    else {
      getData( "LASTB_LO1", &lastLO1 );
      getData( "LASTB_TEMP", &lastTemp );
      getData( "LASTB_VBIAS", &lastVbias );
      getData( "LASTB_IBIAS", &lastIbias );
    }
  } catch (...) {
    return tuned; 	       // must retune if lasttune parameters are missing
  }

  double loGHz = getLO1Freq();
  if  (abs(loGHz - lastLO1) < 0.02) {		// LO1 must be within 20 MHz of previous LO
    double temp = getSISMixerTemp();
    if (abs(temp - lastTemp) > 0.2) {		// mixer physical temp must be within 0.2 K of previous value
      snprintf(ss, ssSize, "reoptimize SIS mixer bias: previous Tmxr = %.2f K, now Tmxr = %.2f K", lastTemp, temp );
      _logger << Priority::INFO << ss;
      printf("%s\n", ss);
    } else {
      setSIS( lastVbias, SISv );                // so far so good; now reset Vj to previous value, measure Ij
      double Ij = getInstantSISIBias();
      if (abs(Ij - lastIbias) > 2.) { 		// Ij must be within 2 uA of previous value
        snprintf(ss, ssSize, "reoptimize SIS mixer bias: previous Ij = %.2f uA, now Ij = %.2f uA", lastIbias, Ij );
        _logger << Priority::INFO << ss;
        printf("%s\n", ss);
      } else {
	tuned = true;				// everything checks out; no need to reoptimize
        snprintf(ss, ssSize, "mixer previously optimized at this LO1, temperature" );
        _logger << Priority::INFO << ss;
        printf("%s\n", ss);
      } 
    }
  }
  return tuned;

}  

// save lasttune parameters in shared memory if tuning succeeded
// everything is read from telemetry
void Rx::saveSisTuneParameters( )
{
  double loGHz = getLO1Freq();
  double lastVbias = getInstantSISVBias();
  double lastIbias = getInstantSISIBias();
  double temp = getSISMixerTemp();

  if (getDewarBand() == 3) { 
    putData( "LASTD_LO1", &loGHz );
    putData( "LASTD_TEMP", &temp );
    putData( "LASTD_VBIAS", &lastVbias );
    putData( "LASTD_IBIAS", &lastIbias );
  } else {
    putData( "LASTB_LO1", &loGHz );
    putData( "LASTB_TEMP", &temp );
    putData( "LASTB_VBIAS", &lastVbias );
    putData( "LASTB_IBIAS", &lastIbias );
  } 
}

// relockmm is intended only to fix occasional dropouts; it assumes that mmosc is at the
// correct mechanical position, mmosc is on, Vop already is correct, etc.

void Rx::relockmm()
{
  unsigned short mmosctarg, mmbcktarg;
  double voptarg, lgaintarg, lgain;
  double maxerrv = 1.0;
  double oscfreq = getOscFreq();
  const int ssSize = 80;
  char ss[ssSize];

  // mmosctarg = _mmosc->position();

  _logger << Priority::INFO << "relockmm";

  turnMMOscOn();
  turnMMSweepOn();

  snprintf(ss, ssSize, "Loading tuning parameters for oscfreq = %.3f GHz", oscfreq );
  _logger << Priority::INFO << ss;
  getTuningParameters( oscfreq, mmosctarg, mmbcktarg, voptarg, lgaintarg );

  // Rescale mmosctarg and mxr_targ to correct for error in Vref
  short admax = _mmosc->getADMax();
  if ( admax < 32600 ) {
    snprintf(ss, ssSize, "admax from LO plate is %d, unusually low (normally > 32600)", admax);
     printf("%s\n",ss);
    _logger << Priority::WARN << ss ;
  }
  if ( admax > 30000 ) {
    mmosctarg = (short)nearbyint( mmosctarg * (float)admax/32750 );
    mmbcktarg = (short)nearbyint( mmbcktarg * (float)admax/32750 );
    snprintf(ss, ssSize,  "scaling mmosc and mmbck by %d/32768; new targets are mmosc = %d, mmbck = %d",
        admax, mmosctarg, mmbcktarg );
    _logger << Priority::INFO << ss ;
  }

  /*  set mm phaselock loop gain 0.5 V above table value to lock it up initially */
  lgain = lgaintarg + 0.5;
  snprintf(ss, ssSize,  "Setting mm phaselock loop gain to %.2f", lgain);
  _logger << Priority::INFO << ss ;
  printf("%s\n",ss);
  setLoopGain( lgain );

  try
  {
    if ( mmLockStatus() == false )
      {
          lockmmosc( mmosctarg, maxerrv );
      }
    else
        setMMLockInfo( 0 ); // Make sure value in lock info isn't stale...

  }
  catch ( int lockErr )
    {
        CPTRACE( Trace::TRACE2, " Caught lockErr, retrying lockmmosc" );
          maxerrv = 3.0;  // Lower standard magic!

    // Try again with larger maxerrv, and squelch int exception if it fails again...
    try
    {
      lockmmosc( mmosctarg, maxerrv );
    }
    catch ( int lockErr )
      {
          throw CARMA_ERROR ( string( "Failed to relock mm osc after two attempts" ) );
      }
  }

  optimizeLoopGain( lgain );
  printf("finished relocking mmosc\n");
  _logger << Priority::INFO << "finished relockmm";
}
  
// Rx::tune sets WBA13 amplifier bias, SIS mixer (Vj,Ij), total pwr on ambient
// tuning strategy:
//  - exit immediately if optimizeReceiver = false; usually this happens for refreq,
//     so set tuneState to GOOD even though there is no guarantee
//  - set WBA13 bias in case we have switched bands
//  - if alreadyTuned() = true, mixer Vj,Ij values were set previously for this LO1;
//      set Vj and skip to setting IFpower level if possible
//  - if not previously tuned to this freq, or if mixer temp has changed, or
//      if Ij does not match previous value, then optimize bias
//  - if tuning table dvMinus,dvPlus !=0, then scan over range of Vj to
//      optimize Y-factor on amb/sky; else measure Vgap, set Vj, and exit

void Rx::tune( double loGHz, bool leaveAbsorber, bool optimizeReceiver )
{
  double temp, vpredL, ipredL, dvMinus, dvPlus, dvgap, igapL, ialtbias;
  double vgapL = 0.;
  float vpredR = 0.;
  float ipredR = 0.;
  float vbiasR, vbiasL, vgapR;
  double VGate, IDrain;
  const int ssSize = 100;
  char ss[ssSize];
  char ss1[ssSize];
  char ss2[ssSize];
  int bimaTuneState;	
  bool succeed = true;	// save tuning parameters when finished if succeed=true

  // exit immediately if optimizeReceiver = false
  if (optimizeReceiver == false) {
    bimaTuneState = 3;	        // -> RxStateMonitorPointEnum::GOOD  (in case of refreq)
    putData( "BIMATUNESTATE", &bimaTuneState );
    return;
  } else {
    int bimaTuneState = 1;	// -> RxStateMonitorPointEnum::TUNE
    putData( "BIMATUNESTATE", &bimaTuneState );
  }

  // set gate voltages on LCP WBA13 amplifier
  // note: I am using actual dewar band as set in the hardware register
  setStatusInfo( "Setting Vgate on amplifier, optimizing Vj, Ij on SIS mixer" );
  VGate = (getDewarBand() == 1) ? _dewar->getGateB() : _dewar->getGateD() ;
  if (VGate > .01) {
    IDrain = getIDrain();
    if (IDrain < 2.) {
      printf("... Idrain < 2 mA, so reset gate to 0 V, then 1.1 V\n");
      setVGate( 0. );
      usleep(30000);
      setVGate( 1.1 );
      usleep(30000);
      printf("... readback:  WBA13 Vgate = %0.2f V, Idrain = %0.2f mA\n", getVGate(), getIDrain() );
    }
    printf("... setting WBA13 gate voltage to %.2f V\n", VGate );
    setVGate( VGate );
    usleep(500000);	// 0.5 sec delay
    snprintf(ss, ssSize,  "WBA13 Vgate = %0.2f V, Idrain = %0.2f mA", getVGate(), getIDrain() );
    printf("... readback: %s\n", ss);
    _logger << Priority::INFO << ss ;
  }

  /* ---- omit for now; faulty sensor on one dewar gives 14 K! ------
  // exit with error if SIS mixer temperture is > 7 K
  if ( (temp = getSISMixerTemp() ) > 7.0 ) {
    int bimaTuneState = 0;	// -> RxStateMonitorPointEnum::RX_BAD
    putData( "BIMATUNESTATE", &bimaTuneState );
    throw CARMA_ERROR( "SIS mixer is too hot!" );
  } 
  -----------------------------------------------------------------*/

  temp = getSISMixerTemp();

  // alreadyTuned() checks if we are returning to same LO1 freq; if mixer temp is within 0.2 K
  // of saved value, it restores Vj (LCP only) to previous value; if Ij (LCP only) is 
  // within 2 uA of saved value, we assume there is no need to optimize Vj further;
  // note: the only way to force reoptimization at same LO1 freq is to zero the LASTB_LO1
  // or LASTD_LO1 using bimaValue (e.g., bimaValue name=LASTB_LO1 value=0.)

  if ( not alreadyTuned() ) {

    // at 1mm tune both LCP and RCP mixers
    bool doRCP = false;
    if (loGHz > 150.) doRCP = true;

    // instantiate a monitor system to read back RCP parameters from the SisRx CAN module
    // carma::monitor::BimaSubsystem  bimaMon( _config.getAntennaNo() );

    // predict SIS bias from tuning table, for POL1 only; SIS Rx module handles POL2
    interpSISBiases( loGHz, temp, vpredL, ipredL, dvMinus, dvPlus, dvgap, igapL, ialtbias );
    vbiasL = vpredL;
    
    // if dvgap is non-zero (fast tuning) or doRCP is true, measure gap voltage, then offset Vj
    if ( doRCP || (dvgap != 0.) ) {

      // fully attenuate the LO
      int attnTarg = 28000;                                       // this should correspond to full attenuation
      selectMotorsAndMod( bandCheck() );                          // set up pointers to motors
      _attn->moveToTarget( attnTarg );                            // fully attenuate LO

      // initiate tuning of RCP mixer, if required
      if (doRCP) {                                                 // initiate tuning of RCP mixer
        _if->command( AntennaIFClient::SISRX_TUNE, (float)loGHz );  
      }

      // measure Vgap of LCP mixer, set Vbias to target value
      if ( dvgap != 0. ) { 	        
        setSIS( igapL, SISi );                 // set Ij to gap value
        sleep(1);
        vgapL = getInstantSISVBias();          // measure Vj = Vgap
        vpredL = vgapL + dvgap;
        snprintf(ss, ssSize, "LCP: measured vgap = %.3f mV; predict Vj = %.3f mV, Ij = %.2f uA", vgapL,vpredL,ialtbias) ;       
	printf("%s\n", ss); 
        _logger << Priority::INFO << ss ;
        setSIS( vpredL, SISv );	              // set Vj to predicted optimum
      } 

      // if necessary, wait for RCP tuning to finish, read back target Vj and Ij
      if (doRCP) {
	sleep(5);		// allow a few secs to make sure RCP module has time to start
	// typedef carma::monitor::SisReceiver::TuneStateSensePointEnum SisRxTuneState;
        int tuneState;
	int maxloops = 20;		// time out after 10 secs
	bool done = false;
	while ( ( !done) && (maxloops--) ) {
	  usleep(500000);	// wait 0.5 sec
	  // bimaMon.readNewest(); 
	  // const SisRxTuneState::TUNESTATE tuneState = bimaMon.rx1mm().sisReceiver().tuneState().getValue();
	  // if ( tuneState == SisRxTuneState::TUNED || tuneState == SisRxTuneState::SET_IJ ) done = true;
	  getData( "RTUNESTATE", &tuneState );
	  if ( tuneState == 6 || tuneState == 5 || tuneState == 7 ) done = true;
	}

	// skip further RCP tuning if SisRx module does not respond - might be offline
	if (maxloops <= 0) {
          snprintf(ss, ssSize, "SisRx tune cmd timed out; after 15 sec tuneState = %d; abort RCP tuning", tuneState );
	  printf("%s\n", ss); 
          _logger << Priority::ERROR << ss ;
	  doRCP = false;
        } else {
	  getData( "RCP_VGAP", &vgapR );
	  getData( "RCP_VJSET", &vpredR );
	  getData( "RCP_IJSET", &ipredR );
	  // vgapR = bimaMon.rx1mm().sisReceiver().gapVoltage().getValue();	// measured gap voltage
	  // vpredR = bimaMon.rx1mm().sisReceiver().requestedVj().getValue();	// RCP Vj from CAN tuning table
	  // ipredR = bimaMon.rx1mm().sisReceiver().requestedIj().getValue();	// RCP Ij from CAN tuning table
	  snprintf(ss, ssSize, "RCP: measured vgap = %.3f mV; predict Vj = %.3f mV, Ij = %.1f uA", vgapR,vpredR,ipredR);
	  printf("%s\n", ss); 
          _logger << Priority::INFO << ss ;
        }
      }

      // set LO bias on LCP mixer
      if (dvgap != 0.)
        setLOpwr( ialtbias );
    }

    // dvgap == 0 indicates no rapid tuning parameters; set LCP Vj,Ij without measuring Vgap
    if (dvgap == 0.) { 
      setSIS( vpredL, SISv );   // this sets the bias voltage
      setLOpwr( ipredL );       // this sets the LO pwr (note: selectMotorsAndMod is inside setLOpwr)
    }

    // ================ optimize Y-factor on amb/sky if dvMinus or dvPlus are non-zero ================
    if ( (dvMinus != 0.) || (dvPlus != 0.) ) 
    {
      int bimaTuneState = 2;	// -> RxStateMonitorPointEnum::OPTIMIZE
      putData( "BIMATUNESTATE", &bimaTuneState );

      setStatusInfo( "Optimizing SIS mixer bias on amb/sky" );
      Rx::Scan LscanSky,LscanAmb,RscanSky,RscanAmb;
      printf("Find SIS mixer bias that gives highest Y-factor\n");

      // LCP sweep is centered on vpredL 
      LscanSky.v1 = LscanAmb.v1 = vpredL + dvMinus;
      LscanSky.v2 = LscanAmb.v2 = vpredL + dvPlus;
      LscanSky.dv = LscanAmb.dv = 4. * 20./2048.;	// 4 integral steps
      
      // set up simultaneous RCP sweep if needed
      if (doRCP) {
        RscanSky.v1 = RscanAmb.v1 = vpredR + dvMinus;
        RscanSky.v2 = RscanAmb.v2 = vpredR + dvPlus;
        RscanSky.dv = RscanAmb.dv = LscanSky.dv ;	// keep step size the same
      } else {
	RscanSky.dv = RscanAmb.dv = 0.;			// no Rscan
      }

      printf("... moving calwheel to Sky\n" );
      _calwheel->setPosition( CalWheel::SKY, getDewarWindow() );
      _logger << Priority::INFO << "  _calwheel.setPosition( SKY ) ";


      printf("... setting IF power to %.2f mW on Sky\n", (float)TARGETIFPOWER);
      ifPowerAtten( AntennaIFClient::SET_IF1_LEVEL, (float)TARGETIFPOWER );
      if (doRCP) {
        ifPowerAtten( AntennaIFClient::SET_IF2_LEVEL, (float)TARGETIFPOWER );
      }
      sleep(1) ;

//      float dbsky = getIFdB();
//      float dbamb = dbsky;
//      printf("... attenuation on sky is %.1f dB\n",dbsky);

      printf("... recording IF power vs. SIS bias voltage on Sky\n");
      scanSIS( LscanSky, RscanSky );

      setSIS( vpredL, SISv );  // restore nominal SIS bias before going to ambient
      printf("... moving calwheel to AMB\n" );
      _calwheel->setPosition( CalWheel::AMB, getDewarWindow() );
      _logger << Priority::INFO << "  _calwheel.setPosition( AMB ) ";

//      ifPowerAtten( AntennaIFClient::SET_IF_LEVEL, (float)TARGETIFPOWER );
//      sleep(2);
//      dbamb = getIFdB();
//      printf("... IF attenuator setting on AMB is %.1f dB\n",dbamb);
  
      printf("... recording IF power vs. SIS bias voltage on Amb\n");
      scanSIS( LscanAmb, RscanAmb );

      // avoid range that typically is affected by Josephson noise (1mm mixers only) 
      float vbadlo = 0.;
      float vbadhi = 0.;
      if (doRCP) {
        vbadlo = 8.* loGHz/242. - 0.2; // affected by Josephson noise
        vbadhi = 8.* loGHz/242. + 0.2;
        printf("... avoid voltage range from %.3f to %.3f mV\n", vbadlo,vbadhi) ;
      }

//        ifamb[j] *= pow(10.,0.1*(dbamb-dbsky));

      // fill out Y-factor arrays
      float yL[MAX_SCAN_STEPS], yR[MAX_SCAN_STEPS];
      for (int j = 0; j < LscanSky.npts; j++) {
        yL[j] = (LscanSky.pow[j] > 0.) ? LscanAmb.pow[j]/LscanSky.pow[j] : 1.0 ;      // protects from division by zero
        if (doRCP) yR[j] = (RscanSky.pow[j] > 0.) ? RscanAmb.pow[j]/RscanSky.pow[j] : 1.0 ;   
      }	
      // find array index for max gain, Y-factor, etc
      int jmaxgainL = 0;	// index for max gain
      int jmaxgainR = 0;
      int jmaxyL = 0;	        // index for max Y-factor
      int jmaxyR = 0;
      int jpredL = 0;       	// index for which voltage comes closest to predicted
      int jpredR = 0;
      for (int j = 1; j < LscanSky.npts; j++) {
	if ( fabs(LscanSky.Vj[j] - vpredL) < fabs(LscanSky.Vj[jpredL] - vpredL) ) jpredL = j;
	if ( (LscanSky.Vj[j] < vbadlo) || (LscanSky.Vj[j] > vbadhi) ) {
	  if ( LscanSky.pow[j] > LscanSky.pow[jmaxgainL] ) jmaxgainL = j;
	  if ( yL[j] > yL[jmaxyL] ) jmaxyL = j;	// note: we used to insist that pow > 0.35 maxpow; requires a 2nd loop
        }
        if ( doRCP ) {
	  if ( fabs(RscanSky.Vj[j] - vpredR) < fabs(RscanSky.Vj[jpredR] - vpredR) ) jpredR = j;
	  if ( (RscanSky.Vj[j] < vbadlo) || (RscanSky.Vj[j] > vbadhi) ) {
	    if ( RscanSky.pow[j] > RscanSky.pow[jmaxgainR] ) jmaxgainR = j;
	    if ( yR[j] > yR[jmaxyR]) jmaxyR = j;	
          }
        }
      }

      // now dump out scan table(s); print "*" for maxY, "G" for maxPow, "P" for predicted, "X" for avoid
      char cL[4], cR[4];
      for (int j = 0; j < LscanSky.npts; j++) {
	cL[0] = cL[1] = cL[2] = cL[3] = cR[0] = cR[1] = cR[2] = cR[3] = ' ';
        int iL = 0;
	if ( (LscanSky.Vj[j] > vbadlo) && (LscanSky.Vj[j] < vbadhi) ) cL[iL++] = 'X';	       // avoid
	if (j==jpredL) cL[iL++] = 'P';                                                         // predicted
	if (j == jmaxgainL) cL[iL++] = 'G';					               // gain peak
	if (j == jmaxyL) cL[iL++] = '*';						       // best Y factor
        snprintf(ss1, ssSize, "%3d   %7.3f %7.2f %6.3f %6.3f %6.3f %c%c%c%c",
	   j, LscanSky.Vj[j], LscanSky.Ij[j], LscanAmb.pow[j], LscanSky.pow[j], yL[j], cL[0], cL[1], cL[2], cL[3]) ;
	if (doRCP) {
          int iR = 0;
	  if ( (RscanSky.Vj[j] > vbadlo) && (RscanSky.Vj[j] < vbadhi) ) cR[iR++] = 'X';	       // avoid
	  if (j==jpredR) cR[iR++] = 'P';                                                       // predicted
	  if (j == jmaxgainR) cR[iR++] = 'G';					               // gain peak
	  if (j == jmaxyR) cR[iR++] = '*';						       // best Y factor
          snprintf(ss2, ssSize, "  %7.3f %7.2f %6.3f %6.3f %6.3f %c%c%c%c",
	     RscanSky.Vj[j], RscanSky.Ij[j], RscanAmb.pow[j], RscanSky.pow[j], yR[j], cR[0], cR[1], cR[2], cR[3]) ;
        } else {
	  snprintf(ss2, ssSize, " ");
	} 
        printf("%s%s\n", ss1,ss2);
        _logger << Priority::INFO << ss1 << ss2 ;
      }

      // set Vj to optimum values (or to default if no significant peak in Y-factor)
      if (yL[jmaxyL] < 1.1) {
        printf("no significant peak in LCP y-factor -> using predicted Vj\n" );
	vbiasL = vpredL;
        setStatusInfo( "SIS bias optimization failed - no significant peak in Y-factor" );
	succeed = false;		// do not save tuning parameters
      } else {
        vbiasL = LscanSky.v1 + jmaxyL*LscanSky.dv;    // note: set to target value rather than readback value
        printf("found LCP y-factor peak, setting SIS bias to %.2f mV\n", vbiasL );
        setStatusInfo( "SIS bias optimization complete" );
      }
      setSIS( vbiasL, SISv );

      if (doRCP) {
        if (yR[jmaxyR] < 1.1) {
	  vbiasR = vpredR;
          printf("no significant peak in RCP y-factor -> using predicted Vj\n" );
        } else {
          vbiasR = RscanSky.Vj[jmaxyR];    // note: set to readback value for RCP
          printf("found RCP y-factor peak, setting SIS bias to %.2f mV\n", vbiasR );
        }
        _if->command( AntennaIFClient::SISRX_SET_VJ, (float)vbiasR );
      }

      // if Y-factor was optimized, store summary in log for possible future analysis
      snprintf(ss, ssSize, "LCP: %6.2f %4.2f  Vgap %5.2f  targ,opt Vj,Ij,(Y)  %5.2f %5.2f   %5.1f %5.1f   %4.2f %4.2f  ",
	   loGHz, temp, vgapL, vpredL, vbiasL, ipredL, getInstantSISIBias(), yL[jpredL], yL[jmaxyL] );
      printf("%s\n", ss );
      _logger << Priority::INFO << ss ;
      if (doRCP) {
	sleep(2) ;	                  // allow time for Vj,Ij to stabilize before final readout
	// bimaMon.readNewest(); 
	// float actualIj = bimaMon.rx1mm().sisReceiver().actualIj().getValue();
	// float actualVj = bimaMon.rx1mm().sisReceiver().actualVj().getValue();
	float actualIj,actualVj;
	getData( "RCP_IJACT", &actualIj );
	getData( "RCP_VJACT", &actualVj );
        snprintf(ss, ssSize, "RCP: %6.2f %4.2f  Vgap %5.2f  targ,opt Vj,Ij,(Y)  %5.2f %5.2f   %5.1f %5.1f   %4.2f %4.2f  ",
	   loGHz, temp, vgapR, vpredR, actualVj, ipredR, actualIj,  yR[jpredR], yR[jmaxyR] );
        printf("%s\n", ss );
        _logger << Priority::INFO << ss ;
      }
    }  // ============================  end Y-factor optimization ============================

    // set IF power to nominal value
    _calwheel->setPosition( CalWheel::AMB, getDewarWindow() );
    ifPowerAtten( AntennaIFClient::SET_IF1_LEVEL, (float)AMBIFPOWER );
    if (doRCP)
      ifPowerAtten( AntennaIFClient::SET_IF2_LEVEL, (float)AMBIFPOWER );
    printf("reset IF power to %.2f mW on AMB; IF attenuator setting is %.1f dB\n", (float)AMBIFPOWER, getIFdB() );
    usleep(40000);	// 0.04 sec delay to make certain that vbias,ibias readbacks are correct
    if (succeed) 
      saveSisTuneParameters() ;		// succeed=false if no significant peak in Y-factor

  } else {
    setStatusInfo( "Restored SIS mixer values to previous settings - tuning complete" );
  }

  bimaTuneState = 3;	// -> RxStateMonitorPointEnum::GOOD
  putData( "BIMATUNESTATE", &bimaTuneState );

  if ( leaveAbsorber == true ) {
    _calwheel->setPosition( CalWheel::AMB, getDewarWindow() );
    printf("leaving absorber in light path\n");
  } else {
    printf("removing absorber from light path\n");
    _calwheel->setPosition( CalWheel::SKY, getDewarWindow() );
  }
}

void Rx::cmTune(){
  return;
}

void Rx::getSISVIBias( double& vbias, double& ibias )
{
  const int nsamples = 10;
  vbias = ibias = 0.;
  for ( int i = 0; i < nsamples; i++ )
  {
    ibias += getInstantSISIBias();
    vbias += getInstantSISVBias();
    if ( _config.isEmulating() == false )
      usleep( 10000 );
  }
  ibias /= nsamples;
  vbias /= nsamples;
}

double Rx::getInstantSISIBias()
{
  int raw;
  raw = atodin( "SISMXI" );
  return (double)( raw * UAMPS );
}

double Rx::getInstantSISVBias()
{
  int raw;
  raw = atodin( "SISMXV" );
  return (double)( raw * MVOLTS );
}

string Rx::getSISMixerName()
{
  if ( _dewar == (Dewar *)0 )
    throw CARMA_ERROR( "_dewar showed up NULL!" );
  if (getDewarBand() == 1) 
    return _dewar->getSISMixerBName();
  else
    return _dewar->getSISMixerDName();
}

double Rx::getSISMixerTemp()
{
  double tmxr, t3;
  const int ssSize = 80;
  char ss[ssSize];

  if ( _dewar == (Dewar *)NULL )
    throw CARMA_ERROR( "_dewar pointer came up NULL!" );
  tmxr = ( getDewarBand() == 1) ? _dewar->stage4temp() : _dewar->stage5temp() ;
  t3 = _dewar->stage3temp();

  if ( (tmxr < 2.5) || (tmxr > 310.) ) {
    if ( (t3 < 2.5) || (t3 > 310.) ) {
      snprintf(ss, ssSize,  "# getSISMixerTemp: BAD SENSOR READINGS, Tmxr = %.2f K, T3 = %.2f K", tmxr, t3) ;
      printf("%s\n", ss);
      _logger << Priority::WARN << ss ;
      snprintf(ss, ssSize,  "# ... guessing Tmxr = 4.0 K");
      printf("%s\n", ss);
      _logger << Priority::WARN << ss ;
      tmxr = 4.0;
    } else {
      snprintf(ss, ssSize,  "# getSISMixerTemp: BAD SENSOR READING, Tmxr = %.2f K", tmxr);
      printf("%s\n", ss);
      _logger << Priority::WARN << ss ;
      snprintf(ss, ssSize,  "#... using T3 = %.2f K instead", t3) ;
      printf("%s\n", ss);
      _logger << Priority::WARN << ss ;
      tmxr = t3;
    }
  }
  return tmxr;
}

// adjust LO attenuator to get proper LO power onto SIS mixer
void Rx::setLOpwr( double targetbias )
{
  // ensure bias viability
  if ( (targetbias < 0.) || (targetbias > 500.) )
  {
    _logger << Priority::ERROR << "setLOpwr: target bias " << targetbias
      << " uA, outside 0-500 uA, defaulting to: 30.0";
    targetbias = 30.;
  }

  float startbias, mxrbias;
  selectMotorsAndMod( bandCheck() );    // set up pointers to motors
  startbias = mxrbias = getInstantSISIBias();

  printf("setLOpwr: adjust LO pwr to SIS mixer; target bias current is %.2f uA\n", targetbias); 
  cout.flush();

  // Return immediately if bias is within 1 uA of requested value
  if ( fabs( mxrbias - targetbias ) < 1. )
  {
    _logger << Priority::INFO << "setLOpwr: already at correct bias; target=" << targetbias
      << ", actual=" << mxrbias;
    printf("... exiting - already at correct bias\n");
    return;
  }

  // Increase or decrease attenuator as required to reach bias
  unsigned short atten, startatten, oldatten;
  startatten = oldatten = _attn->position();
  printf("... starting from atten %d, mxrbias %.1f uA\n", startatten, mxrbias);
  cout.flush();

  // Prepare motor to step
  _attn->enableLock();

  // --- begin by attenuating to bias 5 uA less than target, or max atten ---
  int i = 0;
  int nsteps = 0;
  _attn->setDirection( Motor::UP );
  printf("... increasing atten..." );

  while (  ( (atten = _attn->position()) < 32500 ) 
      && ( (mxrbias = getInstantSISIBias()) > (targetbias - 5.) ) )
  {
    //    printf("...atten: %d  bias: %6.2f\n", atten, mxrbias);
    if (atten > oldatten) {
      oldatten = atten ;
      nsteps = 0 ;
    }    
    if (nsteps++ > 200) {
      printf("\nattenuator is stuck\n");	// also saves us if admax < 32500
      break;
    }	
    if (i-- <= 0) {
      _attn->step( _attn->_fast );	// refresh command only every 0.2 secs to reduce CANbus traffic
      i = 20;
    }  
    if ( _config.isEmulating() == false )
      usleep (10000) ;			// 0.01 sec delay
  }   
  _attn->disableToggleBit(); 		// either we are below target bias or atten is maxed out
  printf(" stop at atten %d, mxrbias %.1f uA\n", _attn->position(), getInstantSISIBias() );  // final position for first step

  if ( mxrbias > targetbias )
    printf("...hey - I can't attenuate enough\n");

  // --- now scan rapidly to lower attenuation until bias passes through target bias ---

  i = 0;
  nsteps = 0;
  oldatten = 33000;
  _attn->setDirection( Motor::DOWN );
  printf("... decrease atten rapidly through setpoint...");
  cout.flush();
 
  // because of occasional mxrbias readout glitches on C11 that cause loop to
  // exit prematurely, I am adding a variable previousLow; now we will keep
  // lowering the attenuation until TWO SUCCESSIVE mxrbias readings exceed target
  bool previousLow = true;	
  while (  ( (atten = _attn->position()) > 5000 ) 
      && ( (mxrbias = getInstantSISIBias()) < (targetbias + 1.0 ) || previousLow) ) 
  {
    printf("...atten: %d  bias: %6.2f\n", atten, mxrbias);
    if (atten < oldatten) {
      oldatten = atten ;
      nsteps = 0 ;
    }    
    if (nsteps++ > 1000) {
      printf(" ATTENUATOR IS STUCK...");
      cout.flush();
      break;
    }	
    if (i-- <= 0) {
      _attn->step( _attn->_fast );	// limit traffic on CANbus by refreshing only every 20 steps
      i = 20;
    }  

    if (mxrbias > (targetbias + 1.))
      previousLow = false;
    else
      previousLow = true;

    usleep (10000) ;			// 0.01 sec delay
  }   
  _attn->disableToggleBit(); 		// either we are above target bias or atten is at minimum
  printf(" stop at atten %d, mxrbias %.1f uA\n", _attn->position(), getInstantSISIBias() );  

  if ( mxrbias < (targetbias - 1.) )
    printf("...hey - I can't get enough LO pwr\n");

  // --- now back up 300 counts to take out backlash --- //
  printf("... increase atten 300 counts\n");
  _attn->moveToTarget( atten + 300 );

  // --- now crawl slowly down until bias exceeds target bias --- //
  printf("... decrease atten slowly...");
  i = 0;       
  oldatten = 0;
  _attn->setDirection( Motor::DOWN );

  while (  ( (atten = _attn->position()) > 5000 ) 
      && ( (mxrbias = getInstantSISIBias()) < targetbias ) ) 
  {
    //    printf("...atten: %d  bias: %6.2f\n", atten, mxrbias);
    if (atten < oldatten) {
      oldatten = atten ;
      nsteps = 0 ;
    }    
    if (nsteps++ > 1000) {
      printf(" ATTENUATOR IS STUCK...");
      break;
    }	
    if (i-- <= 0) {
      _attn->step( _attn->_crawl );	// need to refresh step command only every 0.2 secs
      i = 20;
    }  
    if ( _config.isEmulating() == false )
      usleep (10000) ;			// 0.01 sec delay
  }   
  _attn->disableToggleBit(); 		// either we are above target bias or atten is at minimum
  printf(" stop at atten %d, mxrbias %.1f uA\n", _attn->position(), getInstantSISIBias() );  // final position

  // Final check
  if ( _config.isEmulating() == false )
    usleep( 500000 );  // wait .5 sec for settling, then, final check

  atten = _attn->position();
  mxrbias = getInstantSISIBias();
  printf("... final check: atten = %d, mxrbias = %.1f uA\n", atten,mxrbias);

  if ( fabs(mxrbias - targetbias) < 2. )
  {
    _logger << Priority::INFO << "setLOpwr: atten " << startatten << " --> " << atten
      << ", bias " << startbias << " --> " << mxrbias << " (target " << targetbias << ")";
  }
  else
  {
    _logger << Priority::WARN << "setLOpwr: atten " << startatten << " --> " << atten
      << ", bias " << startbias << " --> " << mxrbias << " (target " << targetbias << ")";
  }
}

double Rx::getIMxrA()
{
  int counts;
  counts = atodin( "IMXRA" );
  return (double)( counts * 4.516129/32768.*124.38);
}

double Rx::getModADmA()
{ 
  return (double)( atodin( "MODAIMON" ) * -4.516129/32768.*200. );
} 

double Rx::getModBmA()
{
  return (double)( atodin( "MODBIMON" ) * -4.516129/32768.*200. );
} 

void Rx::setModulator( double mAtarg )
{
  printf("setModulator to %.2f mA\n",mAtarg);
  if ((mAtarg < -350.0) || (mAtarg > 350.0))
  {
    _errMsg << "Set Modulator target mA " << mAtarg << " out of range!";
    _logger << Priority::WARN << _errMsg.str();
    //    throw CARMA_ERROR( _errMsg.str() );
  }

  unsigned short mod_count = modmAtoCounts( mAtarg );
  const char *addr;

  if ( _band == 'B' )
    addr = "MODLTRB";
  else 
    addr = "MODLTRA";

  tpoke( addr, mod_count );
  if ( _config.isEmulating() == false )
    usleep( 100000 );  // wait 1/10th sec


  double read_mA;

  if ( getBand() == 'B' )
    read_mA = getModBmA();
  else
    read_mA = getModADmA();

  if ( fabs(read_mA - mAtarg) > 20.0 )
  {
    _errMsg.seekp(0);
    _errMsg << "Set Modulator target: " << mAtarg << " mA (counts: " << mod_count << "),"
      << " readback: " << read_mA << " (delta: " << fabs(read_mA - mAtarg) << " max: "
      << "20.0)";
    _logger << Priority::WARN << _errMsg.str();

    //    throw CARMA_ERROR( _errMsg.str() );
  }
}


unsigned short Rx::modmAtoCounts( double mA )
{
  return (unsigned short)(mA * 1023.0/350.0) + 0x400;
}

void Rx::setLoopGain( double lgain )
{
  unsigned short loop_gain = (unsigned short)(2048.*lgain/10.);
  if ( loop_gain > 2047 ) loop_gain = 2047 ;
  putData( "LOOPGAIN", &lgain, 1 );
  tpoke( "MMLOOPGN", loop_gain );

  CPTRACE( Trace::TRACE3, "setLoopGain( " << lgain << " )" );
}

double Rx::mmErrorVolts()
{
  int raw = atodin( "MMLKERRV" );
  return (double)(((raw * 4.516129/32768.*2.697) - 4.18167));
}


void Rx::optimizeLoopGain( double optlgain )
{
  double pn, gain;
  const int ssSize = 80;
  char ss[ssSize];

  _logger << Priority::INFO << "Optimizing Loop Gain...";
  float lowestpn = 100.;

  // loop through plausible range of gains; note that loop gain is inverted such
  //  that 6.5 is a very *low* gain, while 3.5 is a *high* gain; normally the
  //  phaselock will drop out of lock when gain drops below about 4.5; before
  //  this happens the phasenoise should increase by more than 0.2 V

  for ( gain = 6.5; gain > 3.5; gain -= .05 )
  {
    setLoopGain( gain );  
    if ( _config.isEmulating() == false )
      usleep( 50000 );  // wait 0.05 sec; was 0.03 sec but slowed down because of trouble at CF
    pn = getPhaseNoise();
    if (pn < lowestpn) lowestpn = pn;		// save lowest pn, which often is the first
    snprintf(ss, ssSize,  "   lgain = %.2f, phasenoise = %5.3f, lock = %d", gain, pn, mmLockStatus() ) ;
    printf("%s\n", ss);
    _logger << Priority::INFO << ss;

    // exit loop, use default lgain, if mmosc drops out of lock before pn increases by 0.2 V
    if ( mmLockStatus() == false ) {
      snprintf(ss, ssSize,  "Dropped out of lock while optimizing loop gain; use lgain = %.2f", optlgain);
      printf("%s\n", ss);
      _logger << Priority::WARN << ss;
      break;
    }

    // when pn exceeds previous value by 0.3, go back 0.5 to get to optimum value
    if ( pn > ( lowestpn + 0.3 ) )
    {
      optlgain = gain + 0.5;		// temporarily changed to 0.5 because of problems at CF
      snprintf(ss, ssSize, "Set lgain to optimum = %.2f", optlgain);
      printf("%s\n",ss);
      _logger << Priority::INFO << ss ;
      break;
    }

  }
  setLoopGain( optlgain );

  // final check; if out of lock, try to regain lock, then don't meddle with loop gain!
  if ( _config.isEmulating() == false )
    usleep (500000) ;   	// wait 0.5 sec
  if (mmLockStatus() == false ) 
  {
    printf("try to relock\n");
    double maxerrv = 3.0 ;				// don't be fussy! accept large errv	
    unsigned short mmosctarg = _mmosc->position() ;	// save mmosc position

    lockmmosc( mmosctarg, maxerrv );			// try to regain lock, then exit
  } 
}


double Rx::getPhaseNoise ()
{
  int raw;
  getData( "PHNOISE", &raw );
  return (double)(atodin( raw ) * VOLTS4);
}


void Rx::setSIS( double setpoint, Rx::SISSetMode mode )
{
  char mask = 0x14;
  char toopen = 0x10;
  char tovolt = 0x04;
  char tocurr = 0x00;
  unsigned short counts;
  double readback;

  putData( "SISBIASOUT", &setpoint, 1 );

  switch ( tolower( (char)mode ) )
  {
    case SISi:
      if ( (setpoint < 0.) || (setpoint > 497.) )
      { 
	_logger << Priority::WARN << "setSIS: target bias = " << setpoint
	  << " uA outside of 0-497 uA";
	return;
      }
      setbits( "BITSTO50", tocurr, mask );
      counts = (unsigned short)nearbyint( setpoint * 2048./497. );
      tpoke( "BITS56", counts );
      usleep( 100000 );
      readback = getInstantSISIBias();
/* 
      if ( fabs( readback - setpoint ) > 1. )
	_logger << Priority::WARN << "setSIS:## set mixer bias to " << setpoint 
	  << " uA (readback " << readback << " uA)";
      else
	_logger << Priority::INFO << "setSIS: set mixer bias to " << setpoint
	  << " uA (readback " << readback << " uA)";
*/
      break;

    case SISo:
      if ( (setpoint < 0.) || (setpoint > 20.) )
      {
	_logger << Priority::WARN << "setSIS: target bias = " << setpoint
	  << " mV outside of 0-20 mV";
	return;
      }
      setbits( "BITSTO50", toopen, mask );
      counts = (unsigned short)nearbyint( setpoint * 2048./20. );
      tpoke( "BITS56", counts );
      usleep( 100000 );
      break;

    case SISv:
      CPTRACE( Trace::TRACE3, "setSIS - In v mode" );
      if ( (setpoint < 0.) || (setpoint > 20.) )
      {
	_logger << Priority::WARN << "setSIS: target bias = " << setpoint
	  << " mV outside of 0-20 mV";
	return;
      }
      setbits( "BITSTO50", tovolt, mask );
      counts = (unsigned short)nearbyint( setpoint * 2048./20. );
      tpoke( "BITS56", counts );
      //    printf("setting SIS bias to %d = %.3f mV\n", counts, setpoint);
      usleep( 100000 );
      readback = getInstantSISVBias();
 /*
      if ( fabs( readback - setpoint ) > 0.3 )
	_logger << Priority::WARN << "setSIS:## set mixer bias to " << setpoint
	  << " mV (readback " << readback << " mV)";
      else
	_logger << Priority::INFO << "setSIS: set mixer bias to " << setpoint
	  << " mV (readback " << readback << " mV)";
 */
      break;

    default:
      _logger << Priority::WARN << "setSIS: mode " << (char)mode << "is not"
	<< " recognized (o,v,i allowed)";
      return;
      break;
  }
}


Table* Rx::loadSISTable( string filename )
{
  ostringstream oss;
  Table *sisTab;

  try
  {
    sisTab = new Table( filename );
  }
  catch ( carma::util::FileNotFoundException &fnfex )
  { 
    oss << fnfex.what() << ".  Check mixer entry for '"
      << _config.getDewarName() << "' in file "
      << _config.getDewarConfFile();
    throw CARMA_ERROR( oss.str() );
  }

  if ( sisTab == (Table *)0 )
    throw CARMA_ERROR( string( "Table pointer null after attempting to open: " )
	+ filename );

  return sisTab;
}

#define EXPO 4.0
void Rx::interpSISBiases( double& loGHz, double& temp, double& vbias, double& ibias,
    double& dvMinus, double& dvPlus, double& dvgap, double& igap, double& ialtbias  )
{
  const int ssSize = 80;
  char ss[ssSize];

  CARMA_CHECK( _dewar != (Dewar *)NULL );

  // Depending on band, fetch file name of the mixer table for this dewar...
  // switched from getBand to getDewarBand to make receiver tests easier in Berkeley
  string filename;
  if ( getDewarBand() == 1 )		// note: band 1 = B_3MM
    filename = _dewar->getSISMixerBConfFile();
  else
    filename = _dewar->getSISMixerDConfFile(); // A and D are the same...

  cout << "loading SIS bias file: " << filename << endl;
  _logger << Priority::INFO << "loading SIS bias file: " << filename ;

  Table *mixerTab = loadSISTable( filename );

  vector<double> lo = mixerTab->getDoubleColumn( "loGHz" ); // using lo to resemble old code...
  vector<double> t = mixerTab->getDoubleColumn( "t" );
  vector<double> v = mixerTab->getDoubleColumn( "v" );
  vector<double> i = mixerTab->getDoubleColumn( "i" );
  vector<double> dvm = mixerTab->getDoubleColumn( "dvm" );
  vector<double> dvp = mixerTab->getDoubleColumn( "dvp" );

  int np = lo.size();

  if ( np == 0 )
    _logger << Priority::ERROR << "interpSISBiases(sislook), no data in file "
      << filename;

  if ( (np != (int)t.size()) || (np != (int)v.size()) || (np != (int)i.size()) ||
      (np != (int)dvm.size()) || (np != (int)dvp.size()) )
    _logger << Priority::ERROR << "Column length mismatch in file '" << filename
      << "', expected all columns to be " << np << " line(s) long";

  double lo1 = 0.;
  double lo2 = 1000.;
  for ( int n = 0; n < np; n++ )
  {
    if ( (t[n] > 0.) && (lo[n] < loGHz) && (lo[n] > lo1))
      lo1 = lo[n];
    if ( (t[n] > 0.) && (lo[n] >= loGHz) && (lo[n] < lo2))
      lo2 = lo[n];
  }
  if (lo1 == 0.) lo1 = lo2;      // loGHz is lower than any value in table
  if (lo2 == 1000.) lo2 = lo1;   // loGHz is higher than any value in table

  t.push_back(999.); // Push an impossible temp onto the end of the vector

  // for each freq, find 2 nearest temperatures
  int n11, n12, n21, n22;
  n11 = n12 = n21 = n22 = np;   // thus t[nxx] = 999. initially

  for ( int n=0; n < np; n++ )
  {
    if ( (t[n] > 0.) && (fabs(lo[n]-lo1)<1.e-10) && (fabs(t[n]-temp) < fabs(t[n11]-temp)) )
      n11 = n;
    if ( (t[n] > 0.) && (fabs(lo[n]-lo2)<1.e-10) && (fabs(t[n]-temp) < fabs(t[n21]-temp)) )
      n21 = n;
  }
  for ( int n=0; n < np; n++ )
  {
    if ( (t[n] > 0.) && (fabs(lo[n]-lo1)<1.e-10) && (t[n] != t[n11]) && (fabs(t[n]-temp) < fabs(t[n12]-temp)) )
      n12 = n;
    if ( (t[n] > 0.) && (fabs(lo[n]-lo2)<1.e-10) && (t[n] != t[n21]) && (fabs(t[n]-temp) < fabs(t[n22]-temp)) )
      n22 = n;
  }

  if ( n12 == np ) n12 = n11;   // only 1 value of temperature for lo1
  if ( n22 == np ) n22 = n12;   // only 1 value of temperature for lo2

  printf("standard tuning parameters:\n") ;
  printf("... [ n11=%3d  lo=%8.3f  T=%5.2f  v=%7.3f  i=%6.1f ]\n", n11, lo[n11], t[n11], v[n11], i[n11]);
  printf("... [ n12=%3d  lo=%8.3f  T=%5.2f  v=%7.3f  i=%6.1f ]\n", n12, lo[n12], t[n12], v[n12], i[n12]);
  printf("... [ n21=%3d  lo=%8.3f  T=%5.2f  v=%7.3f  i=%6.1f ]\n", n21, lo[n21], t[n21], v[n21], i[n21]);
  printf("... [ n22=%3d  lo=%8.3f  T=%5.2f  v=%7.3f  i=%6.1f ]\n", n22, lo[n22], t[n22], v[n22], i[n22]);
        // note: in the worst case (only 1 entry in table, all values will be identical!

  // temperature interpolation for lo1 and lo2
  float i1, i2, v1, v2, dvm1, dvm2, dvp1, dvp2;
  i1 = i2 = v1 = v2 = dvm1 = dvm2 = dvp1 = dvp2 = 0.;

  i1 = i[n11];
  v1 = v[n11];
  dvm1 = dvm[n11];
  dvp1 = dvp[n11];
  if (n12 != n11)
  {
    i1 += (temp-t[n11])*(i[n12]-i[n11])/(t[n12]-t[n11]);
    v1 += (pow(temp,EXPO)-pow(t[n11],EXPO))*(v[n12]-v[n11])/(pow(t[n12],EXPO)-pow(t[n11],EXPO));
    dvm1 = (dvm1 + dvm[n12]) / 2.;
    dvp1 = (dvp1 + dvp[n12]) / 2.;
  }

  i2 = i[n21];
  v2 = v[n21];
  dvm2 = dvm[n21];
  dvp2 = dvp[n21];
  if (n22 != n12)
  {
    i2 += (temp-t[n21])*(i[n22]-i[n21])/(t[n22]-t[n21]);
    v2 += (pow(temp,EXPO)-pow(t[n21],EXPO))*(v[n22]-v[n21])/(pow(t[n22],EXPO)-pow(t[n21],EXPO));
    dvm2 = (dvm2 + dvm[n22]) / 2.;
    dvp2 = (dvp2 + dvp[n22]) / 2.;
  }

  /* -- linear frequency interpolation -- */
  vbias = v1;
  ibias = i1;
  dvMinus = dvm1;
  dvPlus = dvp1;
  if (lo2 != lo1)
  {
    vbias += (loGHz - lo1) * (v2 - v1) / (lo2 - lo1);
    ibias += (loGHz - lo1) * (i2 - i1) / (lo2 - lo1);
    dvMinus += (loGHz - lo1) * (dvm2 - dvm1) / (lo2 - lo1);
    dvPlus += (loGHz - lo1) * (dvp2 - dvp1) / (lo2 - lo1);
  }

  snprintf(ss, ssSize,  "... for LO = %.2f, Tmxr = %.3f, predict Vbias = %.3f mV, Ibias = %.2f uA", loGHz, temp, vbias, ibias) ;	
  printf("%s\n",ss);
  _logger << Priority::INFO << ss ;

  // alternative fasttune parameters - same format, but T=0
  // restrictions: (1) must specify vgap,igap on line where T=0 and lo=0
  //   (2) there must be at least 2 table entries in addition to the gap
  //   (3) freqs must increase monotonically
  //   (4) 2 lines per discontinuity, with identical lo

  int n0,n1,n2;
  dvgap = 0.;
  igap = 0.;
  ialtbias = 0.;
  n0 = np;
  for ( int n = 0; n < np; n++ ) {
    if ( (t[n] == 0.) && (lo[n] == 0.) )
      n0 = n;       //  vgap,igap = v[n0],i[n0]
  }

  n1 = n0 + 1;
  n2 = n0 + 2;
  if ( n2 < np) {     // proceed only if vgap exists and there are at least 2 more table entries 
    igap = i[n0];     // gap current
    while ( (loGHz > lo[n2]) && ( (n2+1) < np ) ) {
      n1++;
      n2++;
    }
    printf("quicktune parameters:\n") ;
    printf("... [ table Vgap = %.3f mV, Igap = %.2f uA ]\n", v[n0],igap) ;
    printf("... [ lo=%8.3f  T=%5.2f  v=%6.3f  i=%6.1f ]\n", lo[n1],t[n1],v[n1],i[n1]) ;
    printf("... [ lo=%8.3f  T=%5.2f  v=%6.3f  i=%6.1f ]\n", lo[n2],t[n2],v[n2],i[n2]) ;
    dvgap = (loGHz - lo[n1]) * (v[n2] - v[n1]) / (lo[n2] - lo[n1]) + v[n1] - v[n0] ;
    dvMinus = (loGHz - lo[n1]) * (dvm[n2] - dvm[n1]) / (lo[n2] - lo[n1]) + dvm[n1] ;
    dvPlus = (loGHz - lo[n1]) * (dvp[n2] - dvp[n1]) / (lo[n2] - lo[n1]) + dvp[n1] ;
    ialtbias = (loGHz - lo[n1]) * (i[n2] - i[n1]) / (lo[n2] - lo[n1]) + i[n1] ;
    snprintf(ss, ssSize,  "... for LO = %.2f, predict (Vbias-Vgap) = %.3f mV, Ibias = %.2f uA", loGHz, dvgap, ialtbias) ;
    _logger << Priority::INFO << ss ;
    printf("%s\n",ss);
  } else {
    _logger << Priority::INFO << "no quicktune parameters" ;
    printf("no quicktune parameters\n") ;
  }
}

float Rx::getIFdB()
{
  float dB;

  getData( "IFTOTATTENRP", &dB, 1 );

  return dB;
}

double Rx::getIFTotPower()
{
  double volts;

  volts = (double)(atodin( "TOTIFPOW" ) * VOLTS4);

  //   return pow((double)volts,_dewar->getPowExp()) ;
  return ( volts );
}

double Rx::getIF1TotPower()
{
  float volts;
  getData( "IF1CANPOW", &volts, 1 );
  return ( (double)volts );
}

double Rx::getIF2TotPower()
{
  float volts;
  getData( "IF2CANPOW", &volts, 1 );
  return ( (double)volts );
}

double Rx::getIF1FastPower()
{
  float volts;
  getData( "IF1CANFASTPOW", &volts, 1 );
  return ( (double)volts );
}

double Rx::getIF2FastPower()
{
  float volts;
  getData( "IF2CANFASTPOW", &volts, 1 );
  return ( (double)volts );
}

void Rx::startIF1FastSample()
{
  _if->command( AntennaIFClient::START_IF1_FASTSAMP, (short)0);
}

void Rx::startIF2FastSample()
{
  _if->command( AntennaIFClient::START_IF2_FASTSAMP, (short)0);
}

void Rx::stopIF1FastSample()
{
  _if->command( AntennaIFClient::STOP_IF1_FASTSAMP, (short)0);
}

void Rx::stopIF2FastSample()
{
  _if->command( AntennaIFClient::STOP_IF2_FASTSAMP, (short)0);
}

// scanSis steps the SIS mixer voltage bias from V1 to V2, measures Vj, Ij, pow
// it handles pol1 (LCP) and pol2 (RCP) simultaneously
void Rx::scanSIS( Rx::Scan &Lscan, Rx::Scan &Rscan )
{
  printf("# LSCAN: %.3f %.3f %.3f\n", Lscan.v1, Lscan.v2, Lscan.dv);
  printf("# RSCAN: %.3f %.3f %.3f\n", Rscan.v1, Rscan.v2, Rscan.dv);
  int ivscan = 1;    
  int zero = 0;

  // initiate pol2 (RCP) scan if needed; handled autonomously by SisRx and PAM modules, occurs in
  // parallel with the pol1 (LCP) scan

  if (Rscan.dv != 0.) {
    if (fabs( (Rscan.v2-Rscan.v1)/Rscan.dv ) > float(MAX_SCAN_STEPS - 1) ) 
       Rscan.dv = (Rscan.v2-Rscan.v1)/(float)(MAX_SCAN_STEPS - 2);	// make sure data won't overflow arrays 

    // shared memory values need by SisReceiver::processIVCurvePoint
    putData( "RCP_IVNPTS", &zero );		// keeps track of total number of IVpoints accumulated
    putData( "RCP_PNPTS", &zero );		// keeps track of total number of IFpower points
    putData( "RCP_IVSCAN", &ivscan );		// set to 0 when scan is finished

    // scan is initiated by calls to AntennaIFClient which enter values into the IFIPQ;
    // IFCanMaster reads the IFIPQ, reads v1,v2,dv, sends 'DO IV-CURVE' (0x086) cmd to SisRx CAN module
    // the scan itself is handled completely autonomously, occurs in parallel with the LCP scan
    // we check shared memory variable RCP_IVSCAN to determine if scan is finished

    _if->command( AntennaIFClient::SISRX_SCANV1, Rscan.v1 );
    _if->command( AntennaIFClient::SISRX_SCANV2, Rscan.v2 );
    _if->command( AntennaIFClient::SISRX_SCANDV, Rscan.dv );
    _if->command( AntennaIFClient::SISRX_IVCURVE, (float)0. );
  }   
    
  // pol1 (LCP) scan is handled through the old Bima receiver control box; we send out voltages one by
  // one through the shared memory, read back Vj, Ij, and total power 

  if (Lscan.dv != 0.) {
  
    // dtoa is rather coarse, so make certain that we step by integral number of steps each time
    int istart = (int)((Lscan.v1)*2048./20.) ;
    int istop = (int)((Lscan.v2)*2048./20.) ;
    int istep = (int)((Lscan.dv)*2048./20.+ 0.5);   // round to nearest int
    if (istep < 1) istep = 1;

    // ensure that total number of steps does not exceed MAX_SCAN_STEPS
    int nsteps;
    while ( (nsteps = abs(istop-istart+1)/istep) > MAX_SCAN_STEPS) {
     istep++;
    }
    nsteps = abs(istop-istart+1)/istep ;
    Lscan.npts = nsteps;
//    printf("istart = %d, istop = %.d, istep = %d\n", istart,istop,istep);
//    printf("Lscan.v1 = %.3f, Lscan.v2 = %.3f, Lscan.dv = %.3f, nsteps = %d\n",
//	Lscan.v1, Lscan.v2, Lscan.dv, nsteps);

    int nsamp = 3;
    double setpoint;

    for (int j=0; j<nsteps; j++) {
      setpoint = (double)((istart + j*istep)*20./2048.) ; 
      setSIS( setpoint, Rx::SISv );
      usleep(20000);     // allow .02 sec for command to propagate out

      if ( j == 0 )
        sleep(1);        // allow extra time for first point to settle

      Lscan.Vj[j] = 0.;
      Lscan.Ij[j] = 0.;
      Lscan.pow[j] = 0.;

      for (int isamp = 0; isamp < nsamp; isamp++) {
        Lscan.Vj[j] += (float)getInstantSISVBias();
        Lscan.Ij[j] += (float)getInstantSISIBias();
        Lscan.pow[j] += (float)getIFTotPower() ;	// measured through telem, not through CANbus interface	
        usleep( 10000 );    // .01 sec
      }
      Lscan.Vj[j] /= nsamp;
      Lscan.Ij[j] /= nsamp;
      Lscan.pow[j] /= nsamp;
    }
  }  // finished with pol1 (LCP) scan, if initiated

  // wait for pol2 scan to finish, transfer results from shared memory to Rscan structure

  if (Rscan.dv != 0.) {
    int nloop = 60;	        		// timeout after 30 secs
    getData( "RCP_IVSCAN", &ivscan);            // ivscan=0 indicates scan is finished
    while ( (ivscan) && (nloop--)) {
       usleep(500000);                          // check every 0.5 sec
       getData( "RCP_IVSCAN", &ivscan);         // indicates last IV packet received from RxSis module
       // cout << "RCP_IVSCAN = " << ivscan << endl;
    }
    usleep(100000);                             // allow time to receive last FASTTOTPWR packet from PAM

    int Rpowpts;
    getData( "RCP_VJ", Rscan.Vj, MAX_SCAN_STEPS );
    getData( "RCP_IJ", Rscan.Ij, MAX_SCAN_STEPS );
    getData( "RCP_IVNPTS", &Rscan.npts );
    getData( "RCPCANPOW", Rscan.pow, MAX_SCAN_STEPS);
    getData( "RCP_PNPTS", &Rpowpts);		// check to make sure PNPTS = IVNPTS

    // shmBima.getData( "LCP_PNPTS", &Lpts);
    // shmBima.getData( "LCPCANPOW", Lpow, nmax);

    printf("# scanSIS: LCP_NPTS = %d, RCP_NIVPTS = %d, RCP_PNPTS = %d\n", Lscan.npts, Rscan.npts, Rpowpts );

  }  // finished with pol2 (RCP) scan, if initiated
}

void Rx::ifPowerAtten( unsigned short command, float value )
{
  int powt = 0;
  getData( "IFPOWTIMER", &powt, 1 );
  _if->command( command, value );
  int powtChg = powt, timeout = 0;
  while (powtChg == powt && timeout++ != 100 )
  {
    getData( "IFPOWTIMER", &powtChg, 1 );
    if ( _config.isEmulating() == false )
      usleep(10000);
  }

  // This could be a race condition if multiple threads/processes are
  // requesting a change...
  if ( timeout == 1000000 )
  {
    _logger << Priority::WARN << "ifPowerAtten - PAM response to "
      << "to power/atten change request went unanswered";

    // insert code or, make this flow a method to be called for setting IF pow
  }
}

void Rx::ifPowerAtten( unsigned short command, short value )
{
  int powt = 0;
  getData( "IFPOWTIMER", &powt, 1 );
  _if->command( command, value );
  int powtChg = powt, timeout = 0;
  while (powtChg == powt && timeout++ != 100 )
  {
    getData( "IFPOWTIMER", &powtChg, 1 );
    if ( _config.isEmulating() == false )
      usleep(10000);
  }

  // This could be a race condition if multiple threads/processes are
  // requesting a change...
  if ( timeout == 1000000 )
  {
    _logger << Priority::WARN << "ifPowerAtten - PAM response to "
      << "to power/atten change request went unanswered";

    // insert code or, make this flow a method to be called for setting IF pow
  }
}

void Rx::setOscFreq( double freq )
{
  putData( "OSCFREQ", &freq, 1 );
}

double Rx::getOscFreq()
{
  double freq;
  getData( "OSCFREQ", &freq );
  return freq;
}


void Rx::setLO1Freq( double freq )
{
  putData( "LO1FREQ", &freq, 1 );
  int bimaTuneState = 1;	// -> RxStateMonitorPointEnum::TUNE
  putData( "BIMATUNESTATE", &bimaTuneState );
}

double Rx::getLO1Freq()
{
  double freq;
  getData( "LO1FREQ", &freq, 1 );
  return freq;
}

void Rx::setLOTermAtten( int dB )
{
  int atten[5] = {1, 2, 4, 8, 16};
  unsigned char attenmask = 0x1f;
  unsigned char out = 0;
  int attenCnts = 4;
  int sum = 0;

  for (attenCnts=4; attenCnts >=0; attenCnts--)	// note: begin with largest step
  {
    if ((sum + atten[attenCnts]) <= dB)
    {
      sum += atten[attenCnts]; 
      out += 0x01 << attenCnts;
    }
  }
  //   printf("... out = %0X, sum = %d\n", out, sum);
  setbits( "LOTERMBITS", ~out, attenmask );
  putData( "LOTERMSET", &dB );
}

void Rx::setLOTermRFout( float volts )
{
  const int ssSize = 80;
  char ss[ssSize];
  int power = 16, db = 16;
  float rangeV = 0.015; // range for volts setting +-
  float RFout;

  setLOTermAtten( db );       // start at midscale

  while ( power >= 0 )
  {
    if ( _config.isEmulating() == false )
      usleep( 1000000 );           // wait 1 sec
    RFout = getLOTermRFout() ;

    snprintf(ss, ssSize,  "... LOtermAtten = %2d dB, RFout = %.3f V", db, RFout);
    printf("%s\n", ss);
    _logger << Priority::INFO << ss ;

    if (power == 0) break;              // we entered loop just to print out the status; now exit
    power = power >> 1;                 // shift right = divide by 2; on last iteration, power = 0

    if ( RFout > (volts + rangeV) ) {
      if (power == 0) break ;		// there is no more attenuation to ADD, so quit loop
      db += power;
    }
    else if ( RFout < (volts - rangeV) ) {
      db -= power;
      if (power == 0) db -= 1;		// only way to get atten = 0; loop one more time to get readout
    }
    else
      break;				// RFout lies within acceptable limits, so quit loop

    setLOTermAtten( db );
  }
}

int Rx::getLOTermAtten()
{
  int atten;
  getData( "LOTERMSET", &atten, 1 );

  return atten;
}

float Rx::getLOTermPower()
{
  return (float)( atodin( "LOPHOTOCUR" ) * VOLTS4 );
}

float Rx::getLOTermRFin()
{
  return (float)( atodin( "LORFIN" ) * VOLTS4 );
}

float Rx::getLOTermRFout()
{
  return (float)( atodin( "LORFOUT" ) * VOLTS4 );
}

float Rx::getLOTermTemp()
{
  return (float)( ( atodin( "LOTEMP" ) * VOLTS4 ) * 100);
}


Rx::SISSetMode Rx::getSISMode()
{
  int bits;
  Rx::SISSetMode sismode = Rx::SISi;

  getData( "BITSTO50", &bits );

  if ( (bits & 0x1c) == 0x10 )
    sismode = Rx::SISo;
  else if ( (bits & 0x1c) == 0x04 )
    sismode = Rx::SISv;

  return sismode;
}

int Rx::getBimaTuneState()
{
  int bimaTuneState = 0;
  try {
    getData( "BIMATUNESTATE", &bimaTuneState );
  } catch (...) {
     bimaTuneState = 0;
  }
  return bimaTuneState;
}

float Rx::getLO10MHzOptPwr()
{
  return (float)( atodin( "LO10MHZ" ) * VOLTS4 );
}

bool Rx::getLO10MHzStatus()
{
  int bits;
  getData( "BITSIN60", &bits );
  return ( ((bits & 4) == 0 ? true : false) );
}

float Rx::getLO50MHzOptPwr()
{
  return (float)( atodin( "50MHZOPTI" ) * VOLTS4 );
}


double Rx::getSISBiasOut()	// used only by monitor updater
{
  double bias;
  getData( "SISBIASOUT", &bias, 1 );
  return bias;
}

double Rx::getMMIFLevel()
{
  return (double)(atodin( "MMIFLVL" ) * VOLTS10 );
}

void Rx::setObsFreq( double freq )
{
  putData( "OBSFREQ", &freq );
}

double Rx::getObsFreq()
{
  double freq;
  getData( "OBSFREQ", &freq );
  return freq;
}

void Rx::setVopCommanded( double vop )
{
  putData( "VOPCMDED", &vop );
}

double Rx::getVopCommanded( )
{
  double vop;
  getData( "VOPCMDED", &vop );
  return vop;
}

double Rx::getLoopGain()
{
  double lgain;
  getData( "LOOPGAIN", &lgain, 1 );
  return lgain;
}

// HEATER2 should now control WBA13 gate bias, so don't use!
// void Rx::setHtr2 ( float volts )
// {
//  unsigned short hexval = (unsigned short)(2048.*(volts/10.));
//  tpoke ( "HEATER2", hexval );
// }   

// WBA13 gate bias; 10 V from dtoa becomes 12 V after buffer amp
// inside the dewar control box; actual voltage to gate is 1/3
// the voltage from the buffer amp
void Rx::setVGate ( float volts )
{
  unsigned short hexval = (unsigned short)(2048.*(3.*volts/12.0));
  tpoke ( "WBA13D", hexval );
  double v = (double)volts;
  putData( "CMDWBA13D", &v );
}   

void Rx::setCMVGate ( unsigned int hStage, float volts )
{
  ostringstream hempStage;
  hempStage << "HEMP" << hStage << "VG";
  unsigned short hexval = (unsigned short)(2048.*(volts));
  tpoke ( hempStage.str().c_str(), hexval );
  double v = (double)volts;
  hempStage << "CM" << hempStage;
  putData( hempStage.str().c_str(), &v );
}

void Rx::setCMIDrain ( unsigned int hStage, float mAmps )
{
  ostringstream hempStage;
  hempStage << "HEMP" << hStage << "ID";
  unsigned short hexval = (unsigned short)(2048.*(mAmps));
  tpoke ( hempStage.str().c_str(), hexval );
  double i = (double)mAmps;
  hempStage << "CM" << hempStage;
  putData( hempStage.str().c_str(), &i );
}

double Rx::getCMVGateCmd ( unsigned int hStage )
{
  double v;
  ostringstream hempStage;
  hempStage << "CMHEMP" << hStage << "VG";
  getData( hempStage.str().c_str(), &v );
  return v;
}

double Rx::getCMIDrainCmd ( unsigned int hStage )
{
  double i;
  ostringstream hempStage;
  hempStage << "CMHEMP" << hStage << "ID";
  getData( hempStage.str().c_str(), &i );
  return i;
}

double Rx::getCMVGate ( unsigned int hStage )
{
  int raw;
  ostringstream hempStage;
  hempStage << "HEMP" << hStage << "VG";
  getData( hempStage.str().c_str(), &raw );
  return (double)(atodin(raw) * 1);
}

double Rx::getCMIDrain ( unsigned int hStage )
{
  int raw;
  ostringstream hempStage;
  hempStage << "HEMP" << hStage << "ID";
  getData( hempStage.str().c_str(), &raw );
  return (double)(atodin(raw) * 1);
}

double Rx::getVGateCmd()
{
  double v;
  getData( "CMDWBA13D", &v );
  return v;
}

double Rx::getVGate ( void )
{
  int raw;
  getData( "WBA13DVG", &raw );
  return (double)(atodin(raw) * VOLTS4);
}   

double Rx::getVDrain ( void)   // not wired up on atod mux boards!
{
  int raw;
  getData( "WBA13DVD", &raw );
  return (double)(atodin(raw) * VOLTS4);
}   

double Rx::getIDrain ( void)
{
  int raw;
  getData( "WBA13DID", &raw );
  return (double)(atodin(raw) * VOLTS4 * 10.);
}   

void Rx::setNextTuneSequenceNo( int seqno )
{
  putData( "TUNENXTSEQ", &seqno );
}

unsigned long Rx::getNextTuneSequenceNo()
{
  int seqno = 0;
  getData( "TUNENXTSEQ", &seqno );
  return (unsigned long)seqno;
}

void Rx::setCurTuneSequenceNo( int seqno )
{
  putData( "TUNECURSEQ", &seqno );
}

unsigned long Rx::getCurTuneSequenceNo()
{
  int seqno = 0;
  getData( "TUNECURSEQ", &seqno );
  return (unsigned long)seqno;
}

void Rx::setNextOpticsSequenceNo( int seqno )
{
  putData( "OPTICNXTSEQ", &seqno );
}

unsigned long Rx::getNextOpticsSequenceNo()
{
  int seqno = 0;
  getData( "OPTICNXTSEQ", &seqno );
  return (unsigned long)seqno;
}

void Rx::setCurOpticsSequenceNo( int seqno )
{
  putData( "OPTICCURSEQ", &seqno );
}

unsigned long Rx::getCurOpticsSequenceNo()
{
  int seqno = 0;
  getData( "OPTICCURSEQ", &seqno );
  return (unsigned long)seqno;
}

bool Rx::isRelockEnabled()
{
  unsigned short status;
  getData( "RELOCK", &status );
  return ( status == 1 );
}

void Rx::disableRelock()
{
  unsigned short status = 0;
  putData( "RELOCK", &status );
}

void Rx::enableRelock()
{
  unsigned short status = 1;
  putData( "RELOCK", &status );
  zeroRelock();
}

int Rx::numTriesRelock()
{
  unsigned short status;
  getData( "RELOCKCNT", &status );
  return( (int)status );
}

// Not thread safe
void Rx::incRelock()
{
  unsigned short status = numTriesRelock();
  status++;
  if ( status > 3 )
    disableRelock();
  else
    putData( "RELOCKCNT", &status );
}

void Rx::zeroRelock()
{
  unsigned short status = 0;
  putData( "RELOCKCNT", &status );
}

// forceRelock zeros the appropriate lasttune parameter so 
//  mmosc motor is forced to tune again
void Rx::forceRelock( )
{
  _logger << Priority::INFO << "forceRelock() called";
  zeroRelock(); 
  double zero    = 0.0;
  int    intzero = 0;
  if ( _band == 'D' ) {
    putData("LASTD_OSCFREQ", &zero);
    putData("LASTD_LO1",     &zero);
    putData("LASTD_MMOSC",   &intzero);

  }
  else if ( _band == 'B' ) {
    putData("LASTB_OSCFREQ", &zero);
    putData("LASTB_LO1",     &zero);
    putData("LASTB_MMOSC",   &intzero);
  } 
}


ostream& operator<<( ostream& os, Rx& rx )
{
  os << " Rx configuration info: " << endl;
  //  os << rx.getConfig() << endl;
  return os;
}


