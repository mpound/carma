
/**@file
 * Class implementation for TelemetryClient on the BIMA antennas.
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill </dl>
 * $Revision: 1.36 $
 * $Date: 2011/08/24 15:52:47 $
 * $Id: TelemetryClient.cc,v 1.36 2011/08/24 15:52:47 abeard Exp $
 */


#include "carma/antenna/bima/TelemetryClient.h"

#include <cmath>

#include <sys/time.h>

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::canbus;
using namespace carma::antenna::bima;

static SAXParser::ValSchemes validation = SAXParser::Val_Auto;

TelemetryConfigHandler* TelemetryClient::_tmConfig = (TelemetryConfigHandler *)0;
SemaphoreOperator* TelemetryClient::_semOp = (SemaphoreOperator *)0;

PthreadMutex TelemetryClient::mConfigGuard;
::std::map<std::string, unsigned short> TelemetryClient::_nameMap;

  TelemetryClient::TelemetryClient( Configuration& config, bool checkTelemHost )
: SharedMemory( config.getAntenna().c_str() ),
  _config( config )
{
  string configFileName( _config.getTelemConfFile() );

  CPTRACE( Trace::TRACE4, "    TelemetryClient called. "
      << (_config.isEmulating() ? "EMULATE MODE" : "" ) );

  // Attempt to check if the telemetry server is running
  int retrycnt = 0;

  if ( checkTelemHost )
  {

    while ( true )
    {

      if ( status() == true || _config.isEmulating() == true )
      {
	CPTRACE( Trace::TRACE5, "     status() == " << (boolalpha) << status()
	    << " emulate: " << _config.isEmulating() );
	break;
      }
      else
      {
	if ( retrycnt++ > 20 )
	{
	  string msg = "No updates to received frame counts.  Is the telemetry host running?";
	  throw CARMA_ERROR( msg );
	}
      }

      sleep(1);
    }

    // Attempt to be robust in grabbing IPQ handle when first starting up...
    retrycnt = 0;
    while ( true )
    {

      try
      {
	if ( _config.isEmulating() == false )
	{
	  CPTRACE( Trace::TRACE6, "      Aquiring handle to TELEMETRYIPQ: "
	      << TELEMETRYIPQ );
	  _tcWriter =
	    new IPQwriter<TelemetryCommand>( TELEMETRYIPQ, false, IPQLEN );
	}
	else
	{
	  _tcWriter = NULL;
	  CPTRACE( Trace::TRACE6, "      Emulation mode, ignoring attempt to aquire "
	      <<  "handle to TELEMETRYIPQ" );
	}

	break;
      }
      catch ( carma::util::ErrorException &eex )
      {
	if ( retrycnt++ > 20 )
	{
	  ostringstream oss;
	  string msg;
	  oss << "Unable to aquire Telemetry IPQ ('" << TELEMETRYIPQ << "')"
	    << "! Is bimaTelemetryHost running? "
	    << eex.what();
	  msg = oss.str();
	  throw CARMA_ERROR( msg );
	}
      }

      sleep(1);
    }

    {
      ScopedPthreadMutexLock scopeLock( mConfigGuard );

      if ( _config.getTelemConfigHandlerP() == (TelemetryConfigHandler *)0 )
      {
	CPTRACE( Trace::TRACE4, "    Creating new TelemetryConfigHandler" );
	_tmConfig = new TelemetryConfigHandler();
	_config.setTelemConfigHandlerP( _tmConfig );

	XMLPlatformUtils::Initialize();
	DTDValidator *validator = new DTDValidator;
	SAXParser *parser = new SAXParser( validator );

	validator->setErrorReporter( parser );

	parser->setDocumentHandler( _tmConfig );
	parser->setErrorHandler( _tmConfig );
	parser->setValidationScheme( validation );
	parser->parse( configFileName.c_str() );

	// if emulating is false, this will start up normally
	// as a client listening in on semaphore operations
	// if emulating is true, this will create the semaphore
	// set as if it were the server, allowing emulation to
	// work in a standalone mode.
	if ( _config.getSemOpP() == (SemaphoreOperator *)0 )
	{
	  CPTRACE( Trace::TRACE4, "    Creating new SemaphoreOperator" );
	  _semOp = new SemaphoreOperator( _config.isEmulating() ); 
	  _config.setSemOpP( _semOp );
	}
	else
	  _semOp = _config.getSemOpP();
	//

	if ( parser->getErrorCount() > 0 )
	{
	  ostringstream oss;
	  oss << parser->getErrorCount()
	    << " error" << (parser->getErrorCount() > 1 ? "s" : "")
	    << " encountered while parsing " << configFileName
	    << " Fix errors and try again.";

	  throw CARMA_ERROR( oss.str() );
	}

	// Now fill in name map for fast lookups based on point name instead of canbus id
	for ( tmMap::iterator i = _tmConfig->begin(); i != _tmConfig->end(); ++i )
	{
	  TelemetryInfo *tmI = i->second;
	  int len = tmI->getLength();

	  for ( int j = 0; j < len; j++ )
	  {
	    string name( tmI->getName(j) );

	    _semOp->addNameAndId( name, tmI->getSemNum(j) );
	    _nameMap[name] = tmI->getAddr(j);
	    CPTRACE( Trace::TRACE7, "Adding _nameMap["
		<< name << "]: 0x" << hex << _nameMap[name] );
	  }
	}
      }
      else
      {
	CPTRACE( Trace::TRACE4, "    No need to reload TelemetryConfig!" );
	_tmConfig = _config.getTelemConfigHandlerP();
      }
    } // scoped lock

  } // checkTelemHost

}

TelemetryClient::~TelemetryClient()
{
  // no-op
}


void TelemetryClient::tpoke( const char *ccname, unsigned char value )
{
  unsigned short addr;
  const std::string name( ccname );

  if ( _nameMap.find(name) != _nameMap.end() )
  {
    addr = _nameMap[name];
    if ( _tcWriter != NULL ) // emulating
    {
      _tcWriter->setMsgType( Telemetry::VMEWRITE );
      _tcWriter->set( addr, (char&)value );
      _tcWriter->write();

      CPTRACE( Trace::TRACE6, "IPQ write to telemetry" 
	  << " name: " << name
	  << " type: 0x" << (hex) << (int)_tcWriter->getMsgType()
	  << " (VMEWRITE char) addr: 0x" << hex << addr
	  << " value: 0x" << hex << (int)value );
    }
  }
  else
    _logger << Priority::WARN << "Didn't find address in _nameMap["
      << name << "], hope that's okay..." ;

  if ( _config.isEmulating() == false )
    usleep( 10000 ); // avoid saturating canbus...
}

void TelemetryClient::tpeekWriteSpace( const char *ccname, unsigned short *value )
{
  unsigned char addr;
  const std::string name( ccname );

  if ( _nameMap.find(name) != _nameMap.end() )
  {
    addr = _nameMap[name];

    if ( _tcWriter != NULL ) // emulating
    {
      CPTRACE( Trace::TRACE4, "IPQ write to telemetry"
	  << " name: " << name
	  << " type: 0x" << (hex) << (int)_tcWriter->getMsgType() 
	  << " (WRITESPACEQRY) addr: 0x" << hex << (int)addr
	  << " value: 0x" << hex << (int)0 );

      _tcWriter->setMsgType( Telemetry::WRITESPACEQRY );
      _tcWriter->set( (short)addr, (short)0 );
      _tcWriter->write();
      CPTRACE( Trace::TRACE2, "waitOn( " << name << " )" );
      _semOp->waitOn( name ); 
    }

    if ( _config.isEmulating() == false )
      usleep( 10000 ); // avoid saturating canbus...
  }
  else
    _logger << Priority::WARN << "Didn't find address in _nameMap["
      << name << "], hope that's okay..." ;

  int intVal;
  getData( ccname, &intVal, 1 );
  *value = (unsigned short)intVal;
  /*
     unsigned short ushortVal;
     getData( ccname, &ushortVal, 1 );
   *value = ushortVal;
   */
}

void TelemetryClient::tpeek( const char *ccname, unsigned short *value )
{
  unsigned short addr;
  const std::string name( ccname );

  CPTRACE( Trace::TRACE4, "tpeek( '" << ccname << "' )" );

  if ( _nameMap.find(name) != _nameMap.end() )
  {
    addr = _nameMap[name];
    CPTRACE( Trace::TRACE4,
	"READSPACEQRY sent for address: 0x"
	<< (hex) << addr << " waiting on update to: " << name );

    if ( _tcWriter != NULL )
    {
      CPTRACE( Trace::TRACE4, "IPQ write to telemetry"
	  << " name: " << name
	  << " type: 0x" << (hex) << (int)_tcWriter->getMsgType() 
	  << " (READSPACEQRY) addr: 0x" << hex << addr
	  << " value: 0x" << hex << (int)0 );

      _tcWriter->setMsgType( Telemetry::READSPACEQRY );
      _tcWriter->set( addr, (short)0 );
      _tcWriter->write();
      CPTRACE( Trace::TRACE2, "waitOn( " << name << " )" );
      _semOp->waitOn( name );
    }

    if ( _config.isEmulating() == false )
      usleep( 10000 ); // avoid saturating canbus...
  }
  else
    _logger << Priority::WARN << "Didn't find address in _nameMap["
      << name << "], hope that's okay..." ;

  int intVal;
  getData( ccname, &intVal, 1 );
  *value = (unsigned short)intVal;
}

void TelemetryClient::tpoke( const char *ccname, unsigned short value )
{
  unsigned short addr;
  const string name( ccname );

  if ( _tcWriter == NULL )
  {
    CPTRACE( Trace::TRACE4, "tpoke EMULATED" );
    return;
  }

  if ( _nameMap.find(name) != _nameMap.end() )
  {
    addr = _nameMap[name];
    CPTRACE( Trace::TRACE4, "Found addr: " << hex
	<< (unsigned short)addr << " for name: " << name );

    _tcWriter->setMsgType( Telemetry::VMEWRITE );
    _tcWriter->set( addr, (short&)value );
    _tcWriter->write();
    CPTRACE( Trace::TRACE4, "IPQ write to telemetry"
	<< " name: " << name
	<< " type: 0x" << (hex) << (int)_tcWriter->getMsgType() 
	<< " (VMEWRITE short) addr: 0x" << hex << addr
	<< " value: 0x" << hex << (int)value );

    if ( _config.isEmulating() == false )
      usleep( 10000 ); // avoid saturating canbus...
  }
  else
    _logger << Priority::WARN << "Didn't find address in _nameMap["
      << name << "], hope that's okay..." ;

}

void TelemetryClient::tpoke( const char *ccname, unsigned long value )
{
  unsigned short addr;
  const std::string name( ccname );

  if ( _tcWriter == NULL )
  {
    CPTRACE( Trace::TRACE4, "tpoke EMULATED" );
    return;
  }

  if ( _nameMap.find(name) != _nameMap.end() )
  {
    addr = _nameMap[name];
    _tcWriter->setMsgType( Telemetry::VMEWRITE );
    _tcWriter->set( addr, (long&)value );
    _tcWriter->write();
    CPTRACE( Trace::TRACE4, "IPQ write to telemetry"
	<< " name: " << name
	<< " type: 0x" << (hex) << (int)_tcWriter->getMsgType() 
	<< " (VMEWRITE long) addr: 0x" << hex << addr
	<< " value: 0x" << hex << (int)value );

    if ( _config.isEmulating() == false )
      usleep( 10000 ); // avoid saturating canbus...
  }

}

void TelemetryClient::cpoke( unsigned short msgType, vector<byteType> data )
{
  if ( _tcWriter != NULL )
  {
    _tcWriter->setMsgType( msgType );
    _tcWriter->set( data );
    CPTRACE( Trace::TRACE6, "IPQ write to telemetry"
	<< " type: 0x" << (hex) << (int)_tcWriter->getMsgType() 
	<< " (cpoke)"
	<< " value: 0x" << hex
	<< (int)data.at(0) << " " << (int)data.at(1) << " " << (int)data.at(2)
	<< " " << (int)data.at(3) << " " << (int)data.at(4) << " "
	<< (int)data.at(5) << " " << (int)data.at(6) << " " <<  (int)data.at(7) );
    _tcWriter->write();
  }
  else
    CPTRACE( Trace::TRACE6, "cpoke EMULATED" );

  if ( _config.isEmulating() == false )
  {
    usleep( 10000 ); // avoid saturating canbus...
    usleep( 10000 ); // avoid saturating canbus...
  }
}

void TelemetryClient::enableMonitorPackets( void )
{
  vector<byteType> payload;

  for ( int i = 0; i < 8; i++ )
    payload.push_back(0);

  cpoke( Telemetry::TMTYENABLE, payload );
}

void TelemetryClient::disableMonitorPackets( void )
{
  vector<byteType> payload;

  // Magical disabling signature
  payload.push_back( 0xDE );
  payload.push_back( 0xAD );
  payload.push_back( 0xBE );
  payload.push_back( 0xEF );
  payload.push_back( 0x1E );
  payload.push_back( 0x96 );
  payload.push_back( 0x69 );
  payload.push_back( 0xC3 );

  cpoke( Telemetry::TMTYDISABLE, payload );
}

void TelemetryClient::enableToggleBit( const char *ccname, int mask, int update )
{
  vector<byteType> payload;
  int addr = 0;

  const std::string name( ccname );

  // need to throw an exception if this doesn't work...
  if ( _nameMap.find(name) != _nameMap.end() )
    addr = _nameMap[name];

  payload.push_back( (byteType)addr );
  payload.push_back( (byteType)mask );
  payload.push_back( (byteType)update ); // in 1/100ths of second
  payload.push_back( 0x00 ); 
  payload.push_back( 0x00 ); 
  payload.push_back( 0x00 ); 
  payload.push_back( 0x00 ); 
  payload.push_back( 0x00 ); 

  //  CPTRACE( Trace::TRACE4, "cpoke: TOGGLEBIT enable name:" << name << " addr:" << addr << " mask:" << mask << " update:" << update);
  CPTRACE( Trace::TRACE4, "cpoke: TOGGLEBIT enable name:" << name << " addr:"
      << addr << " mask:" << mask << " update:" << update << " _nameMap.size():" << _nameMap.size() );

  cpoke( Telemetry::TOGGLEBIT, payload );
}

void TelemetryClient::disableToggleBit()
{
  vector<byteType> payload;

  payload.push_back( 0x00 );
  payload.push_back( 0x00 );
  payload.push_back( 0x00 );
  payload.push_back( 0x00 ); 
  payload.push_back( 0x00 ); 
  payload.push_back( 0x00 ); 
  payload.push_back( 0x00 ); 
  payload.push_back( 0x00 ); 

  CPTRACE( Trace::TRACE4, "cpoke: TOGGLEBIT disable" );

  cpoke( Telemetry::TOGGLEBIT, payload );
}

// Currently believed to not be necesary...
// 29mar05 - colby
bool TelemetryClient::atodSaturated()
{
  int raw[1000],i,j,k;
  unsigned short address;
  unsigned short start_add = 112, number = 100;
  bool isbad = false;
  double avg;
  ostringstream saddr;

  start_add = 112; number = 100;
  isbad = false;

  // First, write out 100 queries to give the
  // telemetry system a chance to get initial values for
  // all of these atod addresses into shared memory...
  for ( address = start_add; address < (start_add + number); address++ )
  {
    _tcWriter->setMsgType( Telemetry::READSPACEQRY );
    _tcWriter->set( address, (short)0 );
    _tcWriter->write();
  }

  for ( i=0; i < 10; i++ )
  {
    for ( j=0; j < number; j++ )
    {
      k = (j)*10 + i;
      address = (j*2)+start_add;

      // Send out queries to get updated values to determine if
      // atod saturated...  We don't care if all of the values
      // are the most up to date value because the average
      // will be either maxushort or minushort...
      _tcWriter->setMsgType( Telemetry::READSPACEQRY );
      _tcWriter->set( address, (short)0 );
      _tcWriter->write();

      // The atod lookaside stores a copy in ADDRNUM
      // (see Telemetry::processMsg)
      saddr.seekp(0);
      saddr << "ADDR" << (hex) << address;
      raw[k] = atodin( saddr.str().c_str() );
    }
  }

  for ( j=0; j < number; j++ )
  {
    avg = 0;

    for ( i = (j*10); i < ((j*10)+10); i++ )
      avg += raw[i];

    avg /= 10.0;

    if ( (avg == 32768.0) || (avg == -32767.0) )
    {
      _logger << Priority::WARN << "ATOD Saturdated: a/d addr " << (j * 2 + 112)
	<< " is saturated, value is " << avg;
      isbad = true;
      break;
    }
  }

  return isbad;
}

int TelemetryClient::atodin( const char *ccname )
{
  int value; 
  getData( ccname, &value, 1 );

  return atodin( value );
}


void TelemetryClient::setbits( const char *bitsaddr, unsigned char value, unsigned char mask )
{       
  string name = string( bitsaddr );
  vector<byteType> payload;
  int addr = 0;

  // need to throw an exception if this doesn't work...
  if ( _nameMap.find(name) != _nameMap.end() )
    addr = _nameMap[name];

  payload.push_back( (byteType)addr );
  payload.push_back( (byteType)value );
  payload.push_back( (byteType)mask ); // in 1/100ths of second
  payload.push_back( 0x00 );
  payload.push_back( 0x00 );
  payload.push_back( 0x00 );
  payload.push_back( 0x00 );
  payload.push_back( 0x00 );

  CPTRACE( Trace::TRACE4, "cpoke: SETBITS" );

  cpoke( Telemetry::SETBITS, payload );
}

void TelemetryClient::serialEnable()
{
  vector<byteType> payload;

  payload.push_back( 0xc3 );
  payload.push_back( 0x69 );
  payload.push_back( 0x96 ); 
  payload.push_back( 0x1e );
  payload.push_back( 0xef );
  payload.push_back( 0xbe );
  payload.push_back( 0xab );
  payload.push_back( 0xde );

  CPTRACE( Trace::TRACE4, "cpoke: SERIAL ENABLE" );

  cpoke( Telemetry::SERIALENABLE, payload );
}

void TelemetryClient::serialDisable()
{
  vector<byteType> payload;

  payload.push_back( 0x00 );
  payload.push_back( 0x00 );
  payload.push_back( 0x00 ); 
  payload.push_back( 0x00 );
  payload.push_back( 0x00 );
  payload.push_back( 0x00 );
  payload.push_back( 0x00 );
  payload.push_back( 0x00 );

  CPTRACE( Trace::TRACE4, "cpoke: SERIAL DISABLE" );

  cpoke( Telemetry::SERIALDISABLE, payload );
}

// This should probably go somewhere else.... But.. Where?
double TelemetryClient::thermistor( double volts )
{
  double ohms,lnr,overt ;
  double a = .0011303, b = .0002339, c = 8.863e-8 ;
  double result = -999.;

  if ( volts > .10 && volts < 4.45 )
  {
    ohms = 10000. /((+4.516129/volts) - 1.0);
    lnr = log(ohms);
    overt = a + (b * lnr) + (c * pow(lnr,3));
    result = ( 1./overt - 273.16);
  }

  return result;
}

/*  NEED TO ACCOUNT FOR THIS...
    if ( (ant<=2) || (ant>7) ) {
    pressure = (double)(raw * 4.516129/32768.*2.737*30.) ;
    pressure -= 30.;        // 0-300 psi = 1-11 V on new sensors 
    }    
    else {
    pressure = (double)(raw * 4.516129/32768.*2.697*30.) ;
    }
 */
double TelemetryClient::psi( int value )
{
  double pressure = (double)(value * 4.516129/32768.*2.737*30.) ;
  pressure += (float)_config.getPSIMod();    
  return pressure;
}

void TelemetryClient::limit( double& value, double min, double max )
{
  if ( value < min )
    value = min;
  else if ( value > max )
    value = max;
}


bool TelemetryClient::status()
{
  int cnt, newcnt;
  bool status = false;

  getData( "FRAMECNT", &cnt );
  if ( _config.isEmulating() == false )
    usleep(100000); // give time for more telem packets to arrive
  getData( "FRAMECNT", &newcnt );

  if ( newcnt != cnt )
    status = true;
  else
    status = false;

  return status;
}

unsigned char TelemetryClient::getStatusByteA()
{
  int b;

  getData( "STATUSA", &b );

  return (unsigned char)b;
}

unsigned char TelemetryClient::getStatusByteB()
{
  int b;

  getData( "STATUSB", &b );

  return (unsigned char)(b);
}

unsigned char TelemetryClient::getStatusByteC()
{
  int b;

  getData( "STATUSC", &b );

  return (unsigned char)(b);
}

/**
 * Elevation Limit Check
 * Can drive in opposite direction
 * @return boolean status false=ok true=fault
 */
bool TelemetryClient::isElLim()
{
  return (getStatusByteC() & 0x01);
}

/**
 * Elevation Ultimate Limit Check
 * Can drive in opposite direction, with manual overide.
 * Cannot drive at all with computer control.
 * @return boolean status false=ok true=fault
 */
bool TelemetryClient::isElULim()
{
  return (getStatusByteC() & 0x80);
}

/**
 * Azimuth Limit Check
 * Can drive in opposite direction
 * @return boolean status false=ok true=fault
 */
bool TelemetryClient::isAzLim()
{
  return (getStatusByteC() & 0x40);
}

/**
 * Elevation Ultimate Limit Check
 * Can drive in opposite direction, with manual overide.
 * Cannot drive at all with computer control.
 * @return boolean status false=ok true=fault
 */
bool TelemetryClient::isAzULim()
{
  return (getStatusByteC() & 0x04);
}

int TelemetryClient::getNumByteAErrs()
{  
  int cnt;
  getData( "STATUSAERR", &cnt );
  return cnt;
}

int TelemetryClient::getNumByteBErrs()
{  
  int cnt;
  getData( "STATUSBERR", &cnt );
  return cnt;
}

int TelemetryClient::getNumByteCErrs()
{  
  int cnt;
  getData( "STATUSCERR", &cnt );
  return cnt;
}


/**
 * Key in control box
 * Key must be ON, ultimate limits must be ok and
 * all interlock switches (red buttons/collision dect)
 * be in their operating positions to drive antenna.
 * No antenna drive if on ultimate limit or key is off
 * or if one of the safety interlock switches are open.
 * @return boolean status false=ok true=fault
 */
bool TelemetryClient::isKey()
{
  return (getStatusByteC() & 0x08);
}

bool TelemetryClient::isWtrPrssrNotNorm()
{
  return (getStatusByteC() & 0x10);
}

bool TelemetryClient::isCollisionDectOff()
{
  return (getStatusByteC() & 0x20);
}

bool TelemetryClient::isCollision()
{
  return (getStatusByteB() & 0x80);
}

bool TelemetryClient::isAzDrvTempNorm()
{
  return (getStatusByteB() & 0x01);
}

bool TelemetryClient::isElDrvTempNorm()
{
  return (getStatusByteB() & 0x04);
}

bool TelemetryClient::isRxTempNorm()
{
  return (getStatusByteB() & 0x08);
}

bool TelemetryClient::isCabTempNorm()
{
  return (getStatusByteB() & 0x10);
}

bool TelemetryClient::isSpareTempNorm()
{
  return (getStatusByteB() & 0x20);
}

bool TelemetryClient::isCameraSafe()
{
  return (getStatusByteB() & 0x02);
}

bool TelemetryClient::isComputerCtl()
{
  return (getStatusByteB() & 0x40);
}

bool TelemetryClient::isCamFlapOpen()
{
  return (getStatusByteA() & 0x40);
}

bool TelemetryClient::isCamFlapBypass()
{
  return (getStatusByteA() & 0x10);
}

bool TelemetryClient::isCabPwrOff()
{
  return (getStatusByteA() & 0x80);
}


void TelemetryClient::setStatusInfo( string msg )
{
  // limit to 80 characters
  putData( "STATUSINFO", msg.substr(0,80).c_str(), 80 );
}

string TelemetryClient::getStatusInfo()
{
  char msg[81]; // room for null
  getData( "STATUSINFO", msg, 80 );

  return string( msg );
}

string TelemetryClient::getFirmwareVersion()
{
  char ver[20];
  int maj,min,sub;
  getData( "MAJOR", &maj, 1 );
  getData( "MINOR", &min, 1 );
  getData( "SUBMINOR", &sub, 1 );
  snprintf(ver, 19, "%d.%d.%d", maj, min, sub );

  return string( ver );
}

// DEPRECATED, DO NOT USE set function!
// IF YOU CHANGE THE SIZE OF THESE, YOU MUST ALSO CHANGE
// THEM IN TELEMETRYCLIENT
void TelemetryClient::setTelemetryVersion( string ver )
{
  //  putData( "TELEMXMLVER", ver.substr(0,19).c_str(), 19 );
}

// IF YOU CHANGE THE SIZE OF THESE, YOU MUST ALSO CHANGE
// THEM IN TELEMETRYCLIENT
string TelemetryClient::getTelemetryVersion()
{
  char ver[20];
  getData( "TELEMXMLVER", ver, 19 );

  return string( ver );
}

