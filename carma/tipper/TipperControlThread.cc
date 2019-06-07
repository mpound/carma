/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Kim Drongesen, ported by: Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.10 $
 * $Date: 2013/02/26 17:35:13 $
 * $Id: TipperControlThread.cc,v 1.10 2013/02/26 17:35:13 iws Exp $
 */

#include <string>
#include <iostream>
#include <fstream>

#include <errno.h>
// for strerror
#include <string.h>

#include <math.h>

// CARMA includes
#include "carma/tipper/TipperControlThread.h"
#include "carma/tipper/TipperDeviceException.h"
#include "carma/tipper/TipperDataSet.h"
#include "carma/tipper/TipperReduction.h"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::monitor;
using namespace carma::tipper;

#ifndef TIPCTHROW
#define TIPCTHROW( a ) \
  do { \
        ostringstream _errs; \
        _errs << __FUNCTION__ << ": " << a; \
        throw CARMA_EXCEPTION( TipperDeviceException, _errs ); \
  } while (0)
#else
#error "TIPCTHROW is already defined, it will probably break this code."
#endif

// Convenience macros for setting/getting monitor point values
#define SET_TIP_MP(m,v)   _monitor->m().setValue(v)
#define GET_TIP_MP(m)     _monitor->m().getValue()


// These are number of steps and the corresponding angle from zenith.
// Each tip starts with the mirror pointing straight up, then rotates down
// 1.8 degrees per step until the last measurement is taken. The mirror is
// then pointing near the horizon.
//
// To avoid snow buildup during inclement weather, the mirror is then
// pointed straight down.
const TipperControlThread::TipperAngles TipperControlThread::_tipAngles[TipperControlThread::_numAirmasses] =
{
  { 0,  0.0 },
  { 4,  7.2 },
  { 15, 34.2 },
  { 6,  45.0 },
  { 4,  52.2 },
  { 3,  57.6 },
  { 2,  61.2 },
  { 1,  63.0 },
  { 1,  64.8 },
  { 1,  66.6 },
  { 1,  68.4 },
  { 1,  70.2 },
};

namespace 
{

  void sleepSomeNanos( const long nanos )
    {
      struct timespec req;

      req.tv_sec = 0;
      req.tv_nsec = nanos;

      while ( true )
      {
        struct timespec rem;

        if ( ::nanosleep( &req, &rem ) == 0 )
          return;

        if ( errno != EINTR )
          break;

        req = rem;
      }

      TIPCTHROW( "nanosleep failure:" << strerror(errno) );
    }

}


TipperControlThread::TipperControlThread( string device, string dataDir,
   double autoWriterDelayInS,  bool emulate ) :
  _portDevName( device ),
  _dataDir( dataDir ),
  _autoWriterDelayInS( autoWriterDelayInS ),
  _monitor(new ::carma::monitor::OpacityMonitorSubsystem()),
  _emulate( emulate ),
  _logger( Program::getLogger() )
{ 
  CPTRACE( Trace::TRACE3, "   TipperControlThread()" );
  _loDataByte = 0x00;
  _numTips = 0;

  _monitor->startAutoWriter( _autoWriterDelayInS );

  // setup some default values for monitor system
  setStatus( IDLE );
  SET_TIP_MP( lastTip, 0.0 );
  SET_TIP_MP( lastReading, 0.0 );
  SET_TIP_MP( tau225, 0.0 );
  SET_TIP_MP( sequence, 0 );
  SET_TIP_MP( tipperCurr, 0.0 );
}


TipData::TipData()
{
}

void TipperControlThread::setStatus( TipperStatus status )
{
  _status = status;

  switch ( _status )
  {
    case STEP: SET_TIP_MP( status, OpacityMonitorSubsystem::StatusMonitorPointEnum::STEP ); break;
    case TIP_STARTED: SET_TIP_MP( status, OpacityMonitorSubsystem::StatusMonitorPointEnum::TIP_STARTED ); break;
    case PORT_ERROR: SET_TIP_MP( status, OpacityMonitorSubsystem::StatusMonitorPointEnum::PORT_ERROR ); break;
    case DIRECT: SET_TIP_MP( status, OpacityMonitorSubsystem::StatusMonitorPointEnum::DIRECT ); break;
    case GO_ZENITH: SET_TIP_MP( status, OpacityMonitorSubsystem::StatusMonitorPointEnum::GO_ZENITH ); break;
    case IDLE: SET_TIP_MP( status, OpacityMonitorSubsystem::StatusMonitorPointEnum::IDLE ); break;
    case OPEN_PORT: SET_TIP_MP( status, OpacityMonitorSubsystem::StatusMonitorPointEnum::OPEN_PORT ); break;
    case INTEGRATING: SET_TIP_MP( status, OpacityMonitorSubsystem::StatusMonitorPointEnum::INTEGRATING ); break;
  }
}

void TipperControlThread::doTip()
{
  carma::util::ScopedPthreadMutexLock lock( _tipMeasurementLock );

  CPTRACE( Trace::TRACE3, "   " << __FUNCTION__ << " called" );
  _logger << Priority::INFO << __FUNCTION__ << "()";

  setStatus( TIP_STARTED );

  const double startMjd = Time::MJD( );

  try
  {
    openPort();

    CPTRACE( Trace::TRACE4, "    setDirection CLOCKWISE" );
    setDirection( TipperControlThread::CLOCKWISE ); 

    // Don't go too fast...
    sleepSomeNanos( 250L * 1000L * 1000L ); // 250ms
    CPTRACE( Trace::TRACE2, "  goToZenith" );
    goToZenith();


    CPTRACE( Trace::TRACE2, "  doMeasurement" );
    TipData tipData[_numAirmasses];
    doMeasurement( tipData );
    writeToFile( tipData );
  }
  catch ( TipperDeviceException &tde )
  {
    _logger << Priority::ERROR << tde.what();
    CPTRACE( Trace::TRACE1, tde.what() );
  }
  catch ( ... )
  {
    _logger << Priority::ERROR << getCaughtBacktraceAsString();
    CPTRACE( Trace::TRACE1, getCaughtBacktraceAsString() );
  }

  closePort();
  
  const double endMjd = Time::MJD( );
  const double tipSecs = ( endMjd - startMjd ) * Time::SECONDS_PER_DAY;
  _logger << Priority::INFO << " Tip completed in " << tipSecs << " seconds.";

}

void TipperControlThread::getCurrentFileName( string &theName )
{
  CPTRACE( Trace::TRACE3, "   " << __FUNCTION__ << " called" );
  char rawbuf[256];
  int MJDay = static_cast<int>(Time::MJD());
  ::snprintf( rawbuf, 255, "%s/%d.tau", _dataDir.c_str(), MJDay );
  theName = string(rawbuf);
}

void TipperControlThread::writeToFile( TipData *tipData )
{
  CPTRACE( Trace::TRACE3, "   " << __FUNCTION__ << " called" );
  string theName;

  getCurrentFileName( theName );

  fstream tauFile( theName.c_str(), ::fstream::app | ::fstream::out );

  if ( tauFile.good() != true )
  {
    string err = string("Unable to write to '" + theName + "' - ") + + strerror( errno );
    char *cErr = new char[err.length()];

    throw cErr;
  }

  tauFile << "//--------------------------------------------------------------------------------\n";

  // If we've gotten this far, then we've run a tip.  Unfortunately,
  // there's an inefficiency here from the old code in having tipNum in TipData...
  _numTips++;

  for ( int i = 0; i < _numAirmasses; i++ )
  {
    tipData[i]._tipNumber = _numTips;
    tauFile << tipData[i].toString() << endl;
  }

  CPTRACE( Trace::TRACE3, "   Setup data for Tau computation" );
  // Now compute tau
  // Yes, the name DataSet is redundant, but this comes from the old code...
  // Keeping this pattern of copying from TipData into TipDataSet and then
  // reducing the data this way, keeping to a port instead of a refactoring
  // makes my teeth ache.
  TipperDataSet tauData;
  for ( int i = 0; i < _numAirmasses; i++ )
  {
    tauData.addReduction(
       (int)tipData[i]._tipNumber,
       (double)tipData[i]._tipAngle,
       (double)tipData[_numAirmasses-1]._mjdOfReading,
       (double)tipData[i]._scaledAvgSigRef,
       (double)tipData[i]._scaledRmsSigRef,
       (double)tipData[i]._scaledChannelData[1],
       (double)tipData[i]._scaledChannelData[3],
       (double)tipData[i]._scaledChannelData[5],
       (double)tipData[i]._scaledChannelData[4] );
  }

  CPTRACE( Trace::TRACE3, "   Computing Tau" );
  TipperReduction tipReduce( tauData );

  char rawbuf[256];

  CPTRACE( Trace::TRACE3, "   Set up output Tau string" );
  ::snprintf( rawbuf, 255,
      "//! seq %3d mjd %0.5f CGain %5.3f tauZ %5.3f tauTip %5.3f +- %5.3f tauIter %5.3f +- %5.3f gain %5.3f",
      tipReduce.seq, tipReduce.mjd, tipReduce.aveGain, tipReduce.tauZen,
      tipReduce.tauTip, tipReduce.rmsTauTip, tipReduce.tauTipIter,
      tipReduce.rmsTauTipIter, tipReduce.gainTipIter);

  tauFile << rawbuf << endl;

  tauFile.close();

  // Now update monitor stream
  SET_TIP_MP( lastTip, tipReduce.mjd);
  // Need to triple check that this is the reported tau225
  SET_TIP_MP( tau225, tipReduce.tauTip );
  SET_TIP_MP( gain, tipReduce.aveGain );
  SET_TIP_MP( tipNumber, _numTips );
}

void TipperControlThread::doStep( int steps )
{
  CPTRACE( Trace::TRACE3, "   " << __FUNCTION__ << " called" );
  CPTRACE( Trace::TRACE4, "    num steps: " << steps );

  for ( int i = 0; i < steps; i++ )
  {
    _loDataByte |= 0x02;
    sendTipperCommand();

    // Give tipper time to move.  If you don't sleep
    // here, you miss some of the steps
    // sleep for .5 seconds
    sleepSomeNanos( 500L * 1000L * 1000L );

    _loDataByte &= 0xFD;
    sendTipperCommand();
  }
}

void TipperControlThread::doMeasurement( TipData *tipData )
{
  CPTRACE( Trace::TRACE3, "   " << __FUNCTION__ << " called" );

  TipData tipDatum;
  int stepstraveled = 0;

  for ( int i = 0; i < _numAirmasses; i++ )
  {
    setStatus( STEP );
    doStep( _tipAngles[i].steps );
    stepstraveled += _tipAngles[i].steps;

    CPTRACE( Trace::TRACE4, "    Allowing tipper to stabilze for 10 seconds" );
    sleep( 10 ); // Allow tipper to stablize

    doAvgSignalRef( tipDatum );

    tipDatum._airMassNumber = i;
    SET_TIP_MP( sequence, i );

    tipDatum._tipAngle = _tipAngles[i].angle;
    SET_TIP_MP( commandedAngle, tipDatum._tipAngle );

    tipDatum._mjdOfReading = Time::MJD();
    SET_TIP_MP( lastReading, tipDatum._mjdOfReading );

    readAllChannels( tipDatum );
    tipDatum.convertAndScaleChannels();

    SET_TIP_MP( avgSigRef, tipDatum._scaledAvgSigRef );
    SET_TIP_MP( rmsSigRef, tipDatum._scaledRmsSigRef );
    SET_TIP_MP( sigRef, tipDatum._scaledChannelData[0] );
    SET_TIP_MP( hotRef, tipDatum._scaledChannelData[1] );
    SET_TIP_MP( powRef, tipDatum._scaledChannelData[2] );
    SET_TIP_MP( refTemp, tipDatum._scaledChannelData[3] );
    SET_TIP_MP( hotTemp, tipDatum._scaledChannelData[4] );
    SET_TIP_MP( ambTemp, tipDatum._scaledChannelData[5] );
    SET_TIP_MP( chassisTemp, tipDatum._scaledChannelData[6] );
    SET_TIP_MP( mixerCurr, tipDatum._scaledChannelData[7] );
    SET_TIP_MP( triplerCurr, tipDatum._scaledChannelData[8] );
    SET_TIP_MP( gunnCurr, tipDatum._scaledChannelData[9] );
    SET_TIP_MP( battery, tipDatum._scaledChannelData[10] );
    SET_TIP_MP( supplyV, tipDatum._scaledChannelData[12] );
    SET_TIP_MP( supplyA, tipDatum._scaledChannelData[15] );

    tipData[i] = tipDatum;
  }

  // point the mirror straight down to avoid snow buildup
  {
    // 1.8 degrees per step * 100 steps -> 180.0 degrees (straight down)
    const int stepstogo = 100 - stepstraveled;
    doStep( stepstogo );

    // now we are done and the instrument is idle
    setStatus( IDLE );
  }
}

void TipperControlThread::readAllChannels( TipData &tipDatum )
{
  CPTRACE( Trace::TRACE3, "   " << __FUNCTION__ << " called" );

  for ( int i = 0; i < TipData::_numChannels; i++ )
    readOneChannel( i, tipDatum._channelData[i] );
}

void TipperControlThread::doAvgSignalRef( TipData &tipDatum )
{
  CPTRACE( Trace::TRACE3, "   " << __FUNCTION__ << " called" );

  double cummTot, rmsTot;
  float channelDatum;

  int sqNumReads =
    TipData::_numReadings * TipData::_numReadings;

  cummTot = rmsTot = 0.0;
  for ( int i = 0; i < TipData::_numReadings; i++ )
  {
    sleepSomeNanos( 100L * 1000L * 1000L ); // sleep 100ms between reads for stabilizing

    readOneChannel( 0, channelDatum );

    cummTot += channelDatum;
    rmsTot += (channelDatum * channelDatum);
  }

  tipDatum._avgSigRef = cummTot / TipData::_numReadings;
  tipDatum._rmsSigRef = sqrt( (rmsTot/TipData::_numReadings)
      - ( (cummTot * cummTot) / sqNumReads ) );
}

void TipperControlThread::setPortTimeout( int seconds )
{
  CPTRACE( Trace::TRACE3, "   " << __FUNCTION__ << " called" );
  CPTRACE( Trace::TRACE4, "    timeout: " << seconds << "s" );

  if ( tcgetattr( _portFD, &_tios ) < 0 )
  {
    setStatus( PORT_ERROR );
    TIPCTHROW( "Error tcgetattr (getting tty line attributes): " 
        << strerror( errno ) );
  }

  _tios.c_cc[VTIME] = seconds * 10; 

  if ( tcsetattr( _portFD, TCSADRAIN, &_tios ) < 0 )
  {
    setStatus( PORT_ERROR );
    TIPCTHROW( "Error tcsetattr (setting timeout line attribute): "
        << strerror( errno ) );
  }

}

void TipperControlThread::readOneChannel ( char channelToRead, float &data )
{
  CPTRACE( Trace::TRACE3, "   " << __FUNCTION__ << " called" );
  CPTRACE( Trace::TRACE4, "    channelToRead: " << (int)channelToRead );

  // The old code mixed putting this buffer clearing before and
  // after the sending of the prefix.  In this case, it was before...
  clearReadBuffer();
  sendTipperPrefix();

  setPortTimeout( 2 );

  _command[0] = 0x00;
  _command[1] = channelToRead;
  _command[2] = 0x47;
  _command[3] = 0x48;

  CPTRACE( Trace::TRACE4, "    writing read command" );
  write( _command, 4 );

  // Read back, give it 10 tries
  char readInfo[3];
  int bytesRead;
  int i = bytesRead = 0;
  while ( (i++ < 10) && (bytesRead < 1) )
  {
    CPTRACE( Trace::TRACE5, "     attempting read back" );
    bytesRead = read( _portFD, readInfo, 3 );
    CPTRACE( Trace::TRACE4, "     bytesRead: " << bytesRead ); 
    if ( bytesRead > 0 )
    {
      CPTRACE( Trace::TRACE6, "     readInfo: " << (int)(readInfo[0]) << " "
          << (int)(readInfo[1]) << " " << (int)(readInfo[2]) );
    }
  }

  if ( i > 9 )
  {
    setStatus( PORT_ERROR );
    TIPCTHROW( "Error reading from tipper, not responding after 10 tries" );
  }

  // Combine data bytes
  int dataConv = (readInfo[1] << 8) + (((int)readInfo[2]) & 0xFF );
  data = (float)dataConv;
}


void TipperControlThread::goToZenith()
{
  CPTRACE( Trace::TRACE3, "   " << __FUNCTION__ << " called" );

  setStatus( GO_ZENITH );

  _loDataByte |= 0x01;
  CPTRACE( Trace::TRACE4, "    send zenith command" );
  sendTipperCommand();

  CPTRACE( Trace::TRACE2, "  sleep 6s to get to zenith" );
  sleep( 6 ); // sleep 6 seconds to allow tipper time to zenith

  // Clear zenith command
  _loDataByte &= 0xFE;
  CPTRACE( Trace::TRACE4, "    clear zenith command" );
  sendTipperCommand();
}

void TipperControlThread::setDirection( TipperDirection dir )
{
  CPTRACE( Trace::TRACE3, "   " << __FUNCTION__ << " called" );

  if ( dir == CLOCKWISE )
  {
    _loDataByte |= 0x04; // set bit for clockwise
  }
  else
  {
    _loDataByte &= 0xFB; // unset bit for counter clockwise
  }

  sendTipperCommand();
}

void TipperControlThread::clearReadBuffer()
{
  CPTRACE( Trace::TRACE3, "   " << __FUNCTION__ << " called" );

  int bytesRead = 1;
  char crap[1];
  crap[0] = '\0';;
  setPortTimeout( 0 );
  CPTRACE( Trace::TRACE6, "      clear out read buffer" );
  CPTRACE( Trace::TRACE6, "      looking for crap on line" );
  while ( bytesRead > 0 )
  {
    bytesRead = read( _portFD, crap, 1 );
    CPTRACE( Trace::TRACE6, "      bytesRead: " << bytesRead << " "
        << ( ::isprint(crap[0]) ? (char)crap[0] : (int)(crap[0])));
  }
}

void TipperControlThread::sendTipperCommand()
{
  CPTRACE( Trace::TRACE3, "   " << __FUNCTION__ << " called" );

  // The old code mixed putting this buffer clearing before and
  // after the sending of the prefix.  In this case, it was after...
  sendTipperPrefix();

  clearReadBuffer();

  setPortTimeout( 2 );

  _command[0] = 0x80;
  _command[1] = 0x80;
  _command[2] = 0x00;
  _command[3] = _loDataByte;

  CPTRACE( Trace::TRACE4, "    writing command" );
  write( _command, 4 );

  // Read back ACK, give it 10 tries
  char ack[2];
  ack[0] = '\0';
  ack[1] = '\0';
  int bytesRead;
  int i = bytesRead = 0;
  CPTRACE( Trace::TRACE4, "    looking for ACK" );
  while ( (i++ < 10) && (bytesRead < 1) )
  {
    bytesRead = read( _portFD, ack, 2 );
    CPTRACE( Trace::TRACE5, "     bytesRead: " << bytesRead ); 
    if ( bytesRead > 0 )
    {
      if ( ::isprint(ack[0]) && ::isprint(ack[1]) )
      {
        CPTRACE( Trace::TRACE5, "     ack : " << ack );
      }
      else
      {
        CPTRACE( Trace::TRACE5, "     ack : " << (int)(ack[0]) << " " << (int)(ack[1]) );
      }
    }
  }

  CPTRACE( Trace::TRACE4, "    attempts: " << i-1 );

  if ( i > 9 )
  {
    setStatus( PORT_ERROR );
    TIPCTHROW( "Error reading ACK from tipper, not responding after 10 tries" );
  }

  sleepSomeNanos( 50L * 1000L * 1000L ); // 50ms don't overwhelm the tipper
}


void TipperControlThread::write( const char * buff, size_t count )
{
  const ssize_t result = ::write( _portFD, &buff[0], count);
  if ( result != (ssize_t)count )
  {
    setStatus( PORT_ERROR );
    TIPCTHROW( "Error writing to port (expected to send 1 bytes, sent: "
        << result << " error: " << strerror( errno ) );
  }
  sleepSomeNanos( 100L * 1000L * 1000L );
}

void TipperControlThread::setParity( const Parity parity )
{
  if ( tcgetattr( _portFD, &_tios ) < 0 )
  {
    setStatus( PORT_ERROR );
    TIPCTHROW( "Error tcgetattr (getting tty line attributes): " 
        << strerror( errno ) );
  }

  _tios.c_cflag &= ~(PARODD); // Clear the flag regardless
  if ( parity == ODD ) _tios.c_cflag |= PARODD;

  if ( tcsetattr( _portFD, TCSADRAIN, &_tios ) < 0 )
  {
    setStatus( PORT_ERROR );
    TIPCTHROW( "Error tcsetattr (setting tty line attributes, "
        << " parity switching to even): " << strerror( errno ) );
  }
}

void TipperControlThread::sendTipperPrefix()
{
  CPTRACE( Trace::TRACE3, "   " << __FUNCTION__ << " called" );
  //
  // Tipper expects prefix to be in a different parity than commands
  CPTRACE( Trace::TRACE6, "      change prefix parity" );

  setParity( EVEN );

  sleepSomeNanos( 50L * 1000L * 1000L ); // 50ms don't overwhelm the tipper

  //
  // Write prefix byte - Hex 16
  char prefix[1] = {0x16};
  CPTRACE( Trace::TRACE6, "      write prefix" );
  write( prefix, 1 );

  setParity( ODD );

  sleepSomeNanos( 50L * 1000L * 1000L ); // 50ms don't overwhelm the tipper

}

void TipperControlThread::closePort()
{
  CPTRACE( Trace::TRACE3, "   " << __FUNCTION__ << " called" );
  //
  // if _portFD is 0,1,2 that means it's latched onto
  // one of the std ports and we wanna avoid closing those
  // if it's -1, then it's bad anyway, etc...
  if ( _portFD > 2 )
  {
    CPTRACE( Trace::TRACE5, "     restoring previous port settings." );
    if ( tcsetattr( _portFD, TCSANOW, &_tios ) < 0 )
    {
      setStatus( PORT_ERROR );
      TIPCTHROW( "Error tcsetattr (setting tty line attributes): "
          << strerror( errno ) );
    }

    CPTRACE( Trace::TRACE5, "     closing port" );
    if ( close( _portFD ) < 0 )
    {
      setStatus( PORT_ERROR );
      TIPCTHROW( "Error closing port" );
    }

    _portFD = -1;
  }
}

void TipperControlThread::openPort()
{
  CPTRACE( Trace::TRACE3, "   " << __FUNCTION__ << " called" );
  setStatus( OPEN_PORT );

  if ( _emulate == true )
  {
    _logger << Priority::WARN << "Tipper device emulation not yet supported!";
    CPTRACE( Trace::TRACE1, "Tipper device emulation not yet supported!" );
  }
  else
  {
    CPTRACE( Trace::TRACE4, "    opening " << _portDevName );
    _portFD = open( _portDevName.c_str(), (O_RDWR | O_NOCTTY), 0 );
    if ( _portFD == -1 )
    {
      setStatus( PORT_ERROR );
      TIPCTHROW( "Unable to open " << _portDevName << ": " << strerror( errno ) );
    }

    // set baud, turn off line editing and buffering
    CPTRACE( Trace::TRACE6, "      set baud, turn off line editing and buffering" );
    if ( tcgetattr( _portFD, &_oldtios ) < 0 )
    {
      setStatus( PORT_ERROR );
      TIPCTHROW( "Error tcgetattr (getting tty line attributes): "
          << strerror( errno ) );
    }

    if ( tcgetattr( _portFD, &_tios ) < 0 )
    {
      setStatus( PORT_ERROR );
      TIPCTHROW( "Error tcgetattr (getting tty line attributes): "
          << strerror( errno ) );
    }

    CPTRACE( Trace::TRACE6, "      setting baud" );
    const int ispeed = cfsetispeed( &_tios, B9600 );
    if ( ispeed != 0 )
    {
      setStatus( PORT_ERROR );
      TIPCTHROW( "Error cfsetispeed returned " << strerror( errno ) );
    }

    const int ospeed = cfsetospeed( &_tios, B9600 );
    if ( ospeed != 0 )
    {
      setStatus( PORT_ERROR );
      TIPCTHROW( "Error cfsetospeed " << strerror( errno ) );
    }

    _tios.c_cflag = (CS8 | CLOCAL | CREAD | PARODD | PARENB) ;
    _tios.c_iflag &= ~(INLCR | ICRNL | IUCLC | BRKINT | IXON) ;
    _tios.c_oflag &= ~OPOST ;
    _tios.c_lflag &= ~(ICANON | ISIG | ECHO) ;
    _tios.c_cc[VMIN] = 0 ;  // read in each byte (termio: VEOF/4)
    _tios.c_cc[VTIME] = 20 ;  // timeout time - 2 sec (termio: VEOL/5)

    CPTRACE( Trace::TRACE6, "      setting tty attribs" );
    if ( tcsetattr( _portFD, TCSANOW, &_tios ) < 0 )
    {
      setStatus( PORT_ERROR );
      TIPCTHROW( "Error tcsetattr (setting tty line attributes): "
          << strerror( errno ) );
    }

    clearReadBuffer();

    // Do a little sleep to avoid overwhelming the device
    sleepSomeNanos( 250L * 1000L * 1000L ); // 250 millisecond
  }
}

ostream& operator<<( ostream& os, TipperControlThread& tControl )
{

  os << "TipperControlThread: ... ";

  return os;
}

// vim: set expandtab sw=2 ts=2 cindent :

