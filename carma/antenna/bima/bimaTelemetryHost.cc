/** @file
 * Program::main implementation for the Carma BIMA telemetry to CAN
 * processing.
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.19 $
 * $Date: 2012/08/01 14:13:04 $
 * $Id: bimaTelemetryHost.cc,v 1.19 2012/08/01 14:13:04 friedel Exp $
 */

// C++ includes
#include <string>

// System includes
#include <unistd.h>
#include <pthread.h>
#include <errno.h>

// CARMA includes
#include "carma/antenna/bima/SharedMemory.h"
#include "carma/antenna/bima/Telemetry.h"
#include "carma/antenna/bima/AntennaNameResolver.h"
#include "carma/antenna/bima/ProcessMonitor.h"
#include "carma/canbus/exceptions.h"
#include "carma/util/Program.h"
#include "carma/util/Logger.h"
#include "carma/util/Time.h"

using namespace std;
using namespace log4cpp;
using namespace carma::canbus;
using namespace carma::util;
using namespace carma::antenna::bima;

/** 
 * @version $Revision: 1.19 $
 *
 * @usage Usage: bimaTelemetryHost [board=<0x0 - 0xf>] [canbus=<0 or 1>] [emulate=false]
 * Example: bimaTelemetryHost board=0 canbus=1 
 *
 * @description
 * This program is the main CANbus monitor and control application for 
 * BIMA antenna computers.
 *
 * @key emulate false b Emulate (true for emulation). 
 * @key board       0 i The board number of the Janz CAN/Dio card.
 * @key canbus      0 i The canBusId number of the CANbus to be controlled
 * @key antenna     @noDefault s Used to uniquely identify this instance, by antenna.
 *                               Will use gethostname() if not specified.
 * @key tmconfig  "antenna/bima/telemetry.xml"   s File that holds CANBus message types->Telemetry mapping
 * @logger DEFAULT_FACILITY carma.bima.TelemetryHost
 */ 
int carma::util::Program::main()
{
  SharedMemory *_bimaShm;
  try
  {
    Telemetry *telemetry;

    Category& log = getLogger();

    int boardId = getIntParameter( "board" );
    int canBusId = getIntParameter( "canbus" );

    AntennaNameResolver *anr;

    if ( parameterWasSpecified( "antenna" ) )
      anr = new AntennaNameResolver( getStringParameter( "antenna" ).c_str() );
    else
      anr = new AntennaNameResolver();

    Configuration config( anr->getAntennaName(), Program::getConfDir() );

    setInstanceLogname( string("carma.") + config.getAntenna() + string(".TelemetryHost") );

    config.setEmulate( getBoolParameter( "emulate" ) );

    CPTRACE( Trace::TRACE1,
	"Instantiating Telemetry device with following parameters" << endl
	<< "  boardId:    " << boardId << endl
	<< "  canBusId:   " << canBusId << endl
	<< "  emulate:    " << (boolalpha) << config.isEmulating() << endl
	<< "  Logger:     Program::getLogger()" );

    CPTRACE( Trace::TRACE3, "Attaching to shared memory" );
    int startattempts = 0;
    while ( true )
    {
      try{
	_bimaShm = new SharedMemory( config.getAntenna().c_str() );
	_bimaShm->init();
	break;
      }
      catch ( ... )
      {
	if ( startattempts++ < 30 )
	{
	  CPTRACE(Trace::TRACE2,"Unable to open shared memory file, retrying in 1 second");
	  log << Priority::WARN << "Unable to open shared memory file, retrying in 1 second";
	  sleep(3);
	}
	else
	  throw CARMA_ERROR( "Unable to open shared memory file after 29 attempts!" );
      }
    }


    telemetry = new Telemetry( boardId, canBusId, config, _bimaShm );

    // Start watching the telemetry
    pthread_t readThread;
    pthread_t writeThread;

    CPTRACE( Trace::TRACE2, "Launching threads" );

    if ( config.isEmulating() )
    {
      if ( pthread_create( &readThread, (pthread_attr_t *)NULL,
	    Telemetry::startEmulateReaderThread, (void *)telemetry ) != 0 )
	throw CARMA_ERROR( strerror(errno) );
    }
    else
      if ( pthread_create( &readThread, (pthread_attr_t *)NULL,
	    Telemetry::startReaderThread, (void *)telemetry ) != 0 )
	throw CARMA_ERROR( strerror(errno) );

    if ( config.isEmulating() )
    {
      if ( pthread_create( &writeThread, (pthread_attr_t *)NULL,
	    Telemetry::startEmulateWriterThread, (void *)telemetry ) != 0 )
	throw CARMA_ERROR( strerror(errno) );
    }
    else
      if ( pthread_create( &writeThread, (pthread_attr_t *)NULL,
	    Telemetry::startWriterThread, (void *)telemetry ) != 0 )
	throw CARMA_ERROR( strerror(errno) );
    CPTRACE(Trace::TRACE2, "Threads up");
    while (true){
      double time = Time::MJD();
      _bimaShm->putData( "TELEMUP", &time);
      sleep(5);
    }
  }
  catch ( carma::util::ErrorException& err )
  {
    cerr << err.what() << endl;
    _bimaShm->putData( "TELEMUP", &STOPPED);
    return EXIT_FAILURE;
  }
  catch ( ... )
  {
    cerr << "UNCAUGHT EXCEPTION" << endl;
    _bimaShm->putData( "TELEMUP", &STOPPED);
    return EXIT_FAILURE;
  }
  _bimaShm->putData( "TELEMUP", &STOPPED);
  return EXIT_SUCCESS;
};

