/**@file
 * Class definition for Telemetry for Carma CANBus API # ???.
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.52 $
 * $Date: 2013/02/13 15:57:47 $
 * $Id: Telemetry.cc,v 1.52 2013/02/13 15:57:47 friedel Exp $
 */


// CARMA/BIMA includes 
#include "carma/antenna/bima/Telemetry.h"

using namespace std;
using namespace log4cpp;
using namespace carma::canbus;
using namespace carma::util;
using namespace carma::antenna::bima;

static SAXParser::ValSchemes validation = SAXParser::Val_Auto;

// Monitor packets
// Looking for the msgType definitions?  See telemetry.xml


// Shared resource between threads.
CanDio *Telemetry::_canBus;

// ******************************************************************************
Telemetry::Telemetry( int canbusModNo, int canbusSlotNo, Configuration &config, SharedMemory *shm )
  : _logger (Program::getLogger() )
{

  _emulate = config.isEmulating();

  _tilt1len = _tilt2len = 0;
  if ( (_tilt1samples = (int *)malloc( TILTSAMPLES*sizeof(int) )) == NULL  )
    throw CARMA_ERROR( "Unable to allocate memory for tilt1samples!" );
  if ( (_tilt2samples = (int *)malloc( TILTSAMPLES*sizeof(int) )) == NULL  )
    throw CARMA_ERROR( "Unable to allocate memory for tilt2samples!" );

  _azrsamples = new int[RESOLVERSAMPLES];
  _elrsamples = new int[RESOLVERSAMPLES];
  _azcsamples = new int[ENCODERSAMPLES];
  _elcsamples = new int[ENCODERSAMPLES];
  _azssamples = new int[ENCODERSAMPLES];
  _elssamples = new int[ENCODERSAMPLES];
  _azrlen = _elrlen = _azclen = _elclen = _azslen = _elslen = 0;

  if ( config.isEmulating() )
  {
    _logger << Priority::INFO << "Not opening can device, emulating!";
  }
  else
  {
    ostringstream canDev;

    canDev << "/dev/dpm_" << hex << canbusModNo << dec << canbusSlotNo;

    _canBus = new CanDio( canbusModNo, canbusSlotNo );
    _canBus->reset(); // to properly enable DIO lines...

    _node = 1; // Always 1

    _logger << Priority::INFO << "Opened can device: " << canDev.str();
  }

  XMLPlatformUtils::Initialize();
  DTDValidator *validator = new DTDValidator;
  SAXParser *parser = new SAXParser( validator );

  validator->setErrorReporter( parser );

  _tmConfig = new TelemetryConfigHandler();
  parser->setDocumentHandler( _tmConfig );
  parser->setErrorHandler( _tmConfig );
  parser->setValidationScheme( validation );
  parser->parse( config.getTelemConfFile().c_str() );

  if ( parser->getErrorCount() > 0 )
  {
    ostringstream oss;
    oss << parser->getErrorCount()
      << " error" << (parser->getErrorCount() > 1 ? "s" : "")
      << " encountered while parsing " << config.getTelemConfFile()
      << " Fix errors and try again.";

    throw CARMA_ERROR( oss.str() );
  }


  _bimaShm = shm;

  // write out version of telemetry xml file
  // THIS IS SEGFAULTING FOR SOME REASON
  // FIX
  setTelemetryVersion( _tmConfig->getTelemetryVersion() );

  CPTRACE( Trace::TRACE3, "Creating _semOp" );
  _semOp = new SemaphoreOperator( true ); // we're the server!

  CPTRACE( Trace::TRACE3, "Attaching _tcReader" );
  _tcReader = new IPQreader<TelemetryCommand>( TELEMETRYIPQ, true, IPQLEN );

  // Now fill in name map for fast lookups based on point name instead of canbus id
  // This ought to be moved into a separate class, because this is used in two
  // classes now (Telemetry and TelemetryClient...)
  CPTRACE( Trace::TRACE3, "Creating id and semOp maps" );
  for ( tmMap::iterator i = _tmConfig->begin(); i != _tmConfig->end(); ++i )
    {
      TelemetryInfo *tmI = i->second;
      int len = tmI->getLength();

      for ( int j = 0; j < len; j++ )
      {
        const string & name = tmI->getName(j);

        _idMap[tmI->getAddr(j)] = name;
        _semOp->addNameAndId( name, tmI->getSemNum(j) );
        CPTRACE( Trace::TRACE7,
                 "_idMap[" << tmI->getAddr(j) << "]: "
                 << _idMap[tmI->getAddr(j)] << " sem id: " << tmI->getSemNum(j) );
      }
    }

  _lastByteA = _lastByteB = _lastByteC
    = _lastByteAI = _lastByteBI = _lastByteCI
    = _lastByteAC = _lastByteBC = _lastByteCC = 0;

  CPTRACE( Trace::TRACE2, "Telemetry Object Constructed..." );
}

// ******************************************************************************
Telemetry::~Telemetry() 
{
  // destroy.  Destroy.... DESTROY!!!
}

// ******************************************************************************
// Sanity checking routine.  CAN message payloads should always be 8 bytes...
// private functions, to be moved into different location later
bool Telemetry::assertDataAndSize( vector<byteType> &data )
{
  if ( data.size() < 8 )
  {
    ostringstream errbuf;

    errbuf << "data.size() == " << data.size() << " should always be 8!";
    throw CARMA_ERROR( errbuf.str() );
  }

  return true;
}


// ******************************************************************************
apiType Telemetry::getApiId() 
{
  return API_ID;
}

// ******************************************************************************
map<carma::canbus::msgType, string> Telemetry::getHalfSecMonitors() const 
{    
  static map<carma::canbus::msgType, string> halfsecs;
  static bool hereAlready = false;
  
  hereAlready = true;

  return halfsecs;
}

// ******************************************************************************
map<carma::canbus::msgType, string> Telemetry::getSlowMonitors() const
{
  static map<carma::canbus::msgType, string> fivesecs;
  static bool hereAlready = false;

  hereAlready = true;

  return fivesecs;
}


// ******************************************************************************
void Telemetry::processMsg( carma::canbus::msgType mid,
    double rxMJD, std::vector<byteType> data, bool sim ) 
{
  TelemetryInfo *tInfo;
  static timeval tv;
  static suseconds_t lastRorEsample;
  static unsigned long pFrameCnt = 0;
  static unsigned short lastFrameCount = 0, thisFrameCount = 0;
  static int timeFrameCount = 0, sFrameCntChkPnt = 0;
  static time_t timeStart = 0, timeEnd = 0;
  static int allEncResSampled = 0;
  ostringstream saddr; // used to expidite handling of raw address access (atodsat...)

  pFrameCnt++;

  // If data is NULL or data.size() < 8, there's no reason to continue...
  if ( data.size() != 8 ) {
    ostringstream err;
    err << "data.size() == " << data.size() << " should always be 8 "
        << "for msg addressed to telemetry with hex mid " << hex << mid << " "
        << "and payload " << hex;
    for ( DataVector::size_type i = 0; i < data.size(); ++i )
        err << static_cast< unsigned short >( data.at( i ) ) << " ";
    _logger << Priority::ERROR << err << ".";
    return; // Drop on the floor rather than throw.
  }
 
  CPTRACE( Trace::TRACE7, "Telemetry CAN packet received MID: 0x" << hex << mid );
 
  _semOp->updateStale();

  if ( mid == WRITESPACERPY )
  {
    unsigned char addr[4];
    unsigned char datum[4];
    
    for ( int i = 0; i < 4; i++ )
      addr[i] = dataToUbyte( data );

    for ( int i = 0; i < 4; i++ )
      datum[i] = dataToUbyte( data );

    CPTRACE( Trace::TRACE4, "WRITESPACERPY, "
                            << " addr0: 0x"  << (hex) << (int)addr[0]
                            << " datum0: 0x" << (hex) << (int)datum[0]
                            << " addr1: 0x"  << (hex) << (int)addr[1]
                            << " datum1: 0x" << (hex) << (int)datum[1]
                            << " addr2: 0x"  << (hex) << (int)addr[2]
                            << " datum2: 0x" << (hex) << (int)datum[2]
                            << " addr3: 0x"  << (hex) << (int)addr[3]
                            << " datum3: 0x" << (hex) << (int)datum[3] );


    const string & name = _idMap[addr[0]];

    if ( !name.empty() ) 
    {
      CPTRACE( Trace::TRACE4, "addr[" << 0 << "] maps to name: " << name );

/*
      unsigned short val = datum[0];
      val <<= 8;
      val |= datum[1];
      _bimaShm->putData( name->c_str(), &val, 1 );
*/
      int val = datum[0];
      val <<= 8;
      val |= datum[1];
      _bimaShm->putData( name.c_str(), &val, 1 );

      // Let any processes waiting for an update know that it has occured
      _semOp->update( name );
    }
    else // this probably ought to be logged as a warning...
      CPTRACE( Trace::TRACE4,
               "Received WRITESPACERPY with address: 0x"
               << (hex) << addr[0]
               << " Unknown address!  Throwing out!" );

  }
  else if ( mid == READSPACERPY )
  {
    // Format of these messages is:
    // |byte0|byte1|byte0|byte1|byte0|byte1|byte0|byte1|
    //  addr0...... addr1...... datum0..... datum1.....

    int addr[2];
    int datum[2];

    for ( int i = 0; i < 2; i++ )
      addr[i] = (int)dataToUshort( data );

    for ( int i = 0; i < 2; i++ )
      datum[i] = (int)dataToUshort( data );

    CPTRACE( Trace::TRACE4, "READSPACERPY, "
                            << " addr0: 0x" << (hex) << addr[0]
                            << " datum0: 0x" << (hex) << datum[0]
                            << " addr1: 0x" << (hex) << addr[1]
                            << " datum1: 0x" << (hex) << datum[1] );

    for ( int i = 0; i < 2; i++ )
    {
      const string & name = _idMap[addr[i]];

      // Set up special look aside space for addresses 112-212 (atod's)
      // This will allow functions to cycle through the atod addresses
      // to check for saturation...for example.
      // No longer needed?
      /*
      if ( (addr[0] > 111) && (addr[0] < 213) )
      {
        saddr.seekp(0);
        saddr << "ADDR" << (hex) << addr[0];
        _bimaShm->putData( saddr.str().c_str(), &datum[i], 1 );
      }
      */

      if ( !name.empty() )
      {
        CPTRACE( Trace::TRACE3, "addr[" << i << "] maps to name: " << name
                                << " data: " << datum[i] );
        _bimaShm->putData( name.c_str(), &datum[i], 1 );

        // Let any processes waiting for an update know that it has occured
        _semOp->update( name );
      }
      else // this probably ought to be logged as a warning...
        CPTRACE( Trace::TRACE3,
                 "Received READSPACERPY with address: 0x"
                 << (hex) << addr[i]
                 << " Unknown address!  Throwing out!" );
    }
  }
  else if ( (tInfo = _tmConfig->getInfo( mid )) != (TelemetryInfo *)0 )
  {
    //_logger << Priority::INFO << "Telemetry: " << tInfo->_packetName->c_str();

    if ( mid == FAST_PACKET_01 ) {
      _bimaShm->putData( "FPKT1MJD", &rxMJD );
      _bimaShm->putData( "FPK01MJD", &rxMJD );
    }
    else if( mid == FAST_PACKET_02 ) {
      _bimaShm->putData( "FPK02MJD", &rxMJD );
    }
    else if( mid == FAST_PACKET_03 ) {
      _bimaShm->putData( "FPK03MJD", &rxMJD );
    }
    else if( mid == FAST_PACKET_04 ) {
      _bimaShm->putData( "FPK04MJD", &rxMJD );
    }
    else if( mid == FAST_PACKET_05 ) {
      _bimaShm->putData( "FPKM05JD", &rxMJD );
    }
    else if( mid == FAST_PACKET_06 ) {
      _bimaShm->putData( "FPK06MJD", &rxMJD );
    }
    else if( mid == FAST_PACKET_07 ) {
      _bimaShm->putData( "FPK07MJD", &rxMJD );
    }
    else if( mid == FAST_PACKET_08 ) {
      _bimaShm->putData( "FPK08MJD", &rxMJD );
    }
    else if( mid == FAST_PACKET_09 ) {
      _bimaShm->putData( "FPK09MJD", &rxMJD );
    }
    else if( mid == FAST_PACKET_10 ) {
      _bimaShm->putData( "FPK10MJD", &rxMJD );
    }
    else if( mid == FAST_PACKET_11 ) {
      _bimaShm->putData( "FPK11MJD", &rxMJD );
    }
    else if( mid == FAST_PACKET_12 ) {
      _bimaShm->putData( "FPK12MJD", &rxMJD );
    }
    else if( mid == FAST_PACKET_13 ) {
      _bimaShm->putData( "FPK13MJD", &rxMJD );
    }
    else if( mid == FAST_PACKET_14 ) {
      _bimaShm->putData( "FPK14MJD", &rxMJD );
    }
    else if( mid == SLOW_PACKET_01 ) {
      _bimaShm->putData( "SPK01MJD", &rxMJD );
    }
    else if( mid == SLOW_PACKET_02 ) {
      _bimaShm->putData( "SPK02MJD", &rxMJD );
    }
    else if( mid == SLOW_PACKET_03 ) {
      _bimaShm->putData( "SPK03MJD", &rxMJD );
    }
    else if( mid == SLOW_PACKET_04 ) {
      _bimaShm->putData( "SPK04MJD", &rxMJD );
    }
    else if( mid == SLOW_PACKET_05 ) {
      _bimaShm->putData( "SPK05MJD", &rxMJD );
    }
    else if( mid == SLOW_PACKET_06 ) {
      _bimaShm->putData( "SPK06MJD", &rxMJD );
    }
    else if( mid == SLOW_PACKET_07 ) {
      _bimaShm->putData( "SPK07MJD", &rxMJD );
    }
    else if( mid == SLOW_PACKET_08 ) {
      _bimaShm->putData( "SPK08MJD", &rxMJD );
    }
    else if( mid == SLOW_PACKET_09 ) {
      _bimaShm->putData( "SPK09MJD", &rxMJD );
    }
    else if( mid == SLOW_PACKET_10 ) {
      _bimaShm->putData( "SPK10MJD", &rxMJD );
    }
    else if( mid == SLOW_PACKET_11 ) {
      _bimaShm->putData( "SPK11MJD", &rxMJD );
    }
    else if( mid == SLOW_PACKET_12 ) {
      _bimaShm->putData( "SPK12MJD", &rxMJD );
    }
    else if( mid == SLOW_PACKET_13 ) {
      _bimaShm->putData( "SPK13MJD", &rxMJD );
    }
    else if( mid == SLOW_PACKET_14 ) {
      _bimaShm->putData( "SPK14MJD", &rxMJD );
    }
  

    int len = tInfo->getLength();

    for ( int i = 0; i < len; i++ )
    {
      int value;

      switch ( tInfo->getSize(i) )
      {
       case 1:
        value = (int)dataToUbyte( data );
        break;
       case 2:
        value = (int)dataToUshort( data );
        break;
      }

      // Encoders and Resolvers are sampled to provide a smooth projection
      // of what their values are, with rejection of deviating points that
      // are probably due to errors reading the devices.
      // Only sample these about every .085 seconds, as the devices cannot
      // put out a new sensed value but once every .08 seconds...
      // This helps reduce compute load as well.
      gettimeofday( &tv, NULL );
      if ( (tv.tv_usec - lastRorEsample ) > 85000 )
      {
        if ( strcmp( tInfo->getName(i), AZRESOLVER ) == 0 )
        {
          allEncResSampled |= 0x1;

          if ( _azrlen < RESOLVERSAMPLES )
            _azrlen++;
        }
        else if ( strcmp( tInfo->getName(i), ELRESOLVER ) == 0 )
        {
          allEncResSampled |= 0x2;

          if ( _elrlen < RESOLVERSAMPLES )
            _elrlen++;
        }
        else if ( strcmp( tInfo->getName(i), AZCOSENC ) == 0 )
        {
          allEncResSampled |= 0x4;

          if ( _azclen < ENCODERSAMPLES )
            _azclen++;
           
          MASSAGEENCVALUE( value );
        }
        else if ( strcmp( tInfo->getName(i), ELCOSENC ) == 0 )
        {
          allEncResSampled |= 0x8;

          if ( _elclen < ENCODERSAMPLES )
            _elclen++;
           
          MASSAGEENCVALUE( value );
        }
        else if ( strcmp( tInfo->getName(i), AZSINENC ) == 0 )
        {
          allEncResSampled |= 0x10;

          if ( _azslen < ENCODERSAMPLES )
            _azslen++;
           
          MASSAGEENCVALUE( value );
        }
        else if ( strcmp( tInfo->getName(i), ELSINENC ) == 0 )
        {
          allEncResSampled |= 0x20;

          if ( _elslen < ENCODERSAMPLES )
            _elslen++;
           
          MASSAGEENCVALUE( value );
        }
      }

      // Make sure that all encoders/resolvers have been sampled before reseting
      // these counters.
      if ( ( (tv.tv_usec - lastRorEsample) > 85000 ) && ( allEncResSampled == 0x3f ) )
      {
        lastRorEsample = tv.tv_usec;
        allEncResSampled = 0;
      }
    
      // Smooth out sampling of status bytes
      // First check if this packet applies to 'STATUS'
      // Then refine the checks to STATUSA/STATUSB/STATUSC
      if ( strncmp( tInfo->getName(i), "STATUS", 6 ) == 0 )
      {

	if ( strncmp( tInfo->getName(i), "STATUSA", 7 ) == 0 )
	{
	  if ( _lastByteA == value )
	  {
	    if ( _lastByteAI < 49 )
	      _lastByteAI++;
	  }
	  else
	  {
	    if ( _lastByteAI > 0 )
	      _lastByteAI--;
	    else
	      _lastByteA = value;
	  }

	  if ( _lastByteAC == 0 )
	  {
	    int err;
	    err = 49 - _lastByteAI;
	    _bimaShm->putData( "STATUSA", &_lastByteA );
	    _bimaShm->putData( "STATUSAERR", &err );
	  }

	  _lastByteAC++;
	  if ( _lastByteAC > 49 )
	    _lastByteAC = 0;

	}
	else if ( strncmp( tInfo->getName(i), "STATUSB", 7 ) == 0 )
	{
	  if ( _lastByteB == value )
	  {
	    if ( _lastByteBI < 49 )
	      _lastByteBI++;
	  }
	  else
	  {
	    if ( _lastByteBI > 0 )
	      _lastByteBI--;
	    else
	      _lastByteB = value;
	  }

	  if ( _lastByteBC == 0 )
	  {
	    int err;
	    err = 49 - _lastByteBI;
	    _bimaShm->putData( "STATUSB", &_lastByteB );
	    _bimaShm->putData( "STATUSBERR", &err );
	  }

	  _lastByteBC++;
	  if ( _lastByteBC > 49 )
	    _lastByteBC = 0;

	}
	else if ( strncmp( tInfo->getName(i), "STATUSC", 7 ) == 0 )
	{
	  if ( _lastByteC == value )
	  {
	    if ( _lastByteCI < 49 )
	      _lastByteCI++;
	  }
	  else
	  {
	    if ( _lastByteCI > 0 )
	      _lastByteCI--;
	    else
	      _lastByteC = value;
	  }

	  if ( _lastByteCC == 0 )
	  {
	    int err;
	    err = 49 - _lastByteCI;
	    _bimaShm->putData( "STATUSC", &_lastByteC );
	    _bimaShm->putData( "STATUSCERR", &err );
	  }

	  _lastByteCC++;
	  if ( _lastByteCC > 49 )
	    _lastByteCC = 0;

	}

      }

      // Tilt meters are sampled and averaged for the monitor stream
      // with outliers removed from the average if they are too far out
      // of the rms.  This list is read by bimaMonitorHost.
      if ( strcmp( tInfo->getName(i), "TILTMTR1" ) == 0 )
      {
        if ( _tilt1len == TILTSAMPLES )
        {
          CPTRACE( Trace::TRACE5, "TILTMTR1 store, len:" << _tilt1len );
          _bimaShm->putData( "TILT1SAMPLES", _tilt1samples, TILTSAMPLES );
          _tilt1len = 0;
        }
        else
        {
          _tilt1samples[_tilt1len] = value;
          CPTRACE( Trace::TRACE5, "   _tilt1samples[" << _tilt1len << "]: "
                   << _tilt1samples[_tilt1len]);
          _tilt1len++;
        }
      }
    
      if ( strcmp( tInfo->getName(i), "TILTMTR2" ) == 0 )
      {
        if ( _tilt2len == TILTSAMPLES )
        {
          CPTRACE( Trace::TRACE5, "TILTMTR2 store, len:" << _tilt2len );
          _bimaShm->putData( "TILT2SAMPLES", _tilt2samples, TILTSAMPLES );
          _tilt2len = 0;
        }
        else
        {
          _tilt2samples[_tilt2len] = value;
          CPTRACE( Trace::TRACE7, "   _tilt2samples[" << _tilt2len << "]: "
                   << _tilt2samples[_tilt2len]);
          _tilt2len++;
        }
      }

          
      if ( strcmp( tInfo->getName(i), "FRAMECNT" ) == 0 )
      {
        lastFrameCount = thisFrameCount;
        thisFrameCount = value;

        if ( (thisFrameCount - lastFrameCount) > 1 )
          _logger << Priority::INFO
                  << "Frame count jumped by "
                  << thisFrameCount - lastFrameCount;
  
        timeEnd = time((time_t *)NULL);
  
        if ( ++timeFrameCount == 100000 ) // every 100 seconds
        {
          float pps;
  
          if ( (timeEnd - timeStart) > 0 )
            pps = pFrameCnt / (timeEnd - timeStart);
          else
            pps = -1;
  
          ostringstream oss;
          oss.precision(5);
  
          oss << pps << " f/s, "
              << " frames received in this period: "
              << pFrameCnt;
  
          _logger << Priority::INFO << oss.str().c_str();
  
          pFrameCnt = timeFrameCount = 0;
          sFrameCntChkPnt = thisFrameCount;
          timeStart = timeEnd;
        } // if ++timeFrameCount == 1000
      } // if strcmp "FRAMECNT"

      _bimaShm->putData( tInfo->getName(i), &value, 1 );
    } // for len
  }
  else // tInfo pointer null, unrecognized message...
  {
    CPTRACE( Trace::TRACE7,
             "Telemetry CAN 0x" << hex << mid
             << " packet not recognized, add to telemetry.xml?" );
  }
}


// ******************************************************************************
carma::canbus::Message Telemetry::simulateMsg(carma::canbus::msgType mid) 
{
  carma::canbus::Message msg;

  return msg;
}

void *Telemetry::startEmulateReaderThread( void *anObj )
{
  Telemetry *telemetry = static_cast<Telemetry *>(anObj);

  CPTRACE( Trace::TRACE2, "Starting emulated Reader Thread" );
  telemetry->emulateReaderThread();

  return NULL;
}

void *Telemetry::startReaderThread( void *anObj )
{
  Telemetry *telemetry = static_cast<Telemetry *>(anObj);

  CPTRACE( Trace::TRACE2, "Starting Reader Thread tied to canbus" );
  telemetry->readerThread();

  return NULL;
}

void *Telemetry::startEmulateWriterThread( void *anObj )
{
  Telemetry *telemetry = static_cast<Telemetry *>(anObj);
 
  CPTRACE( Trace::TRACE2, "Starting emulated Writer Thread" );
  telemetry->emulateWriterThread();

  return NULL;
}

void *Telemetry::startWriterThread( void *anObj )
{
  Telemetry *telemetry = static_cast<Telemetry *>(anObj);
 
  CPTRACE( Trace::TRACE2, "Starting Writer Thread tied to canbus" );
  telemetry->writerThread();

  return NULL;
}

// In this context Reader thread means to read from the CANbus.
// And store into the shared memory.
void Telemetry::emulateReaderThread( void )
{
  int framecnt = 0;
  srand( time( NULL ) );

  CPTRACE( Trace::TRACE5, "Emulated updater sleeping..." );

  while ( true )
  {
    framecnt++;
    _bimaShm->putData( "FRAMECNT", &framecnt );
    usleep( 10000 ); 
  }
}

void Telemetry::emulateWriterThread( void )
{
  CPTRACE( Trace::TRACE1, "Entering Emulated Writer Thread..." );

  // Ignore whatever is already in the ipq buffer...
  _tcReader->setNoneAvailable();

  try
  {
    while ( true )
    {

      CPTRACE( Trace::TRACE6, "Entering ipq read()... (emulating)" );
      _tcReader->read();

      _logger << Priority::INFO << "Throwing out write to telemetry, we are emulating!";
      CPTRACE( Trace::TRACE2, "Throwing out write to telemetry, we are emulating!");
      // Ignore it...
    }
  }
  catch ( ... )
  {
    _logger << Priority::ERROR
      << "Telemetry Emulated Writer thread exiting, uncaught exception";
  }
}

// In this context Reader thread means to read from the CANbus.
// And store into the shared memory.
void Telemetry::readerThread( void )
{
  carma::canbus::Message msg;
  carma::canbus::msgType mid;
  carma::canbus::apiType apit;
  carma::canbus::nodeType nodet;
  carma::canbus::idType id;
  double rxMJD = 0.0;

  try
  {
    while ( true )
    {
      CPTRACE( Trace::TRACE7, "Waiting for _canBus->getMessage()" );

      msg = _canBus->getMessage();
      id = msg.getId();
      fromId( apit, nodet, mid, id );
      rxMJD = msg.getRxMjd();
      processMsg( mid, rxMJD, msg.getData(), false );
    }
  }
  catch ( ErrorException &be )
  {
    _logger << Priority::INFO << "OMG!! " << be.what();
  }
  catch ( ... )
  {
    _logger << Priority::ERROR
      << "Telemetry Reader thread exiting, uncaught default exception";
  }
}


// In this context Writer thread means to write to the CANbus.
void Telemetry::writerThread( void )
{
  CPTRACE( Trace::TRACE1, "Entering Writer Thread..." );

  // Ignore whatever is already in the ipq buffer...
  _tcReader->setNoneAvailable();

  try
  {
    bool sendit = true;

    while ( true )
    {
      unsigned short addr;
      TelemetryCommand::PayloadType payload;

      CPTRACE( Trace::TRACE6, "Entering ipq read()..." );
      _tcReader->read();
      CPTRACE( Trace::TRACE6, " returned from ipq read()" );

      addr = (unsigned short)_tcReader->getAddr();
      CPTRACE( Trace::TRACE6, "  addr: 0x" << (hex) << (unsigned short)addr );

      idType id = createId( false, getApiId(), 1, _tcReader->getMsgType() );
      vector<byteType> data;
      CPTRACE( Trace::TRACE6, "  msgType: 0x" << (hex) << (int)_tcReader->getMsgType() );

      if ( _tcReader->getMsgType() == VMEWRITE )
      {
	unsigned char baddr = addr;

	uByteToData( data, baddr );

	if ( _tcReader->getDataType() == 'c' )
	{
	  _tcReader->getPayload( payload.c );
	  CPTRACE( Trace::TRACE4, "VMEWRITE to addr: 0x" << hex << (int)addr
	      << ", payload.c: '0x" << hex
	      << (int)((char)payload.c) << "'" );
	  uByteToData( data, payload.c );
	  uShortToData( data, 0x0000 );
	  uLongToData( data, 0x00000000 );
	}
	else if ( _tcReader->getDataType() == 's' )
	{
	  _tcReader->getPayload( payload.s );
	  CPTRACE( Trace::TRACE6, "VMEWRITE to addr: 0x" << hex << (int)addr
	      << ", payload.s: '0x" << hex << (short)payload.s << "'" );
	  uByteToData( data, payload.bytes[1] );
	  uByteToData( data, addr + 1 );
	  uByteToData( data, payload.bytes[0] );
	  uLongToData( data, 0x00000000 );
	}
	else if ( _tcReader->getDataType() == 'l' )
	{
	  _tcReader->getPayload( payload.l );
	  CPTRACE( Trace::TRACE4, "VMEWRITE to addr: 0x" << hex << (int)addr
	      << ", payload.l: '0x" << hex << (long)payload.l << "'" );
	  uLongToData( data, payload.l );
	  uByteToData( data, 0x00 );
	  uByteToData( data, 0x00 );
	  uByteToData( data, 0x00 );
	}
	else if ( _tcReader->getDataType() == 'f' )
	{
	  _tcReader->getPayload( payload.f );
	  CPTRACE( Trace::TRACE4, "VMEWRITE to addr: 0x" << hex << (int)addr
	      << ", payload.f: '0x" << hex << (float)payload.f << "'" );
	  floatToData( data, payload.f );
	  uByteToData( data, 0x00 );
	  uByteToData( data, 0x00 );
	  uByteToData( data, 0x00 );
	}
	else if ( _tcReader->getDataType() == 'd' )
	{
	  _tcReader->getPayload( payload.d );
	  CPTRACE( Trace::TRACE4, "VMEWRITE to addr: 0x" << hex << (int)addr
	      << ", payload.d: '0x" << hex << (double)payload.d << "'" );
	  doubleToData( data, payload.d );
	}
      }
      else if ( _tcReader->getMsgType() == READSPACEQRY )
      {
	CPTRACE( Trace::TRACE4, "READSPACEQRY addr: 0x" << (hex) << (int)addr );
	uShortToData( data, addr );
	uShortToData( data, 0x0 );
	uLongToData( data, 0x0 );
      }
      else if ( _tcReader->getMsgType() == WRITESPACEQRY )
      {
	CPTRACE( Trace::TRACE4,
	    "WRITESPACEQRY addr: 0x"
	    << (hex) << (int)addr << (int)addr + 1 );
	uByteToData( data, addr );
	uByteToData( data, addr + 1 );
	uShortToData( data, 0x0 );
	uLongToData( data, 0x0 );
      }
      else if ( _tcReader->getMsgType() == TOGGLEBIT )
      {
	_tcReader->getPayload( data );
	CPTRACE( Trace::TRACE4,
	    "TOGGLEBIT addr: 0x" << (hex) << (int)data.at(0)
	    << " value: 0x" << hex << (int)data.at(1)
	    << " mask: 0x" << hex << (int)data.at(2) );
      }
      else if ( _tcReader->getMsgType() == SETBITS )
      {
	_tcReader->getPayload( data );
	CPTRACE( Trace::TRACE4,
	    "SETBITS addr: 0x" << (hex) << (int)data.at(0)
	    << " value: 0x" << hex << (int)data.at(1)
	    << " mask: 0x" << hex << (int)data.at(2) );
      }
      else // data must be in vector form...
      {
	_errMsg.seekp(0);
	_errMsg << "Unknown out put packet to telemetry, dropping, MsgType: "
	  << (int)_tcReader->getMsgType();
	CPTRACE( Trace::TRACE1, _errMsg.str().c_str() );
	_logger << Priority::WARN << _errMsg.str();
	sendit = false;
      }

      if ( sendit == true )
      {
	carma::canbus::Message msg( id, ALL_BUSSES );

	msg.setData( data );

	CPTRACE( Trace::TRACE6, "Posting to canbus..." );
	_canBus->postMessage( msg );
	//        usleep( 10000 ); // Don't saturate the canbus
      }
      else
	sendit = true;

    }
  }
  catch ( ErrorException &be )
  {
    _logger << Priority::ERROR
      << "Telemetry writer thread exiting, " << be.what();
  }
  catch ( ... )
  {
    _logger << Priority::ERROR
      << "Telemetry writer thread exiting, uncaught default exception";
  }

} 

// IF YOU CHANGE THE SIZE OF THESE, YOU MUST ALSO CHANGE
// THEM IN TELEMETRYCLIENT
void Telemetry::setTelemetryVersion( string version )
{
  CPTRACE( Trace::TRACE3, "  setting telemetry version: " << version );

  _bimaShm->putData( "TELEMXMLVER", version.substr(0,19).c_str(), 19 );
}

// IF YOU CHANGE THE SIZE OF THESE, YOU MUST ALSO CHANGE
// THEM IN TELEMETRYCLIENT
string Telemetry::getTelemetryVersion()
{
  char ver[21];
  _bimaShm->getData( "TELEMXMLVER", ver, 19 );

  return string( ver );
}

