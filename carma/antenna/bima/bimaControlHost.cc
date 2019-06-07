/** @file
 * Program::main implementation for the CARMA BIMA control host
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.15 $
 * $Date: 2012/08/01 14:13:04 $
 * $Id: bimaControlHost.cc,v 1.15 2012/08/01 14:13:04 friedel Exp $
 */

#include "carma/antenna/bima/bimaControlHost.h"
#include "carma/antenna/bima/ProcessMonitor.h"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::antenna::common;
using namespace carma::antenna::bima;

/** 
 * @version $Revision: 1.15 $
 *
 * @usage Usage: bimaControlHost [emulate=false]
 * Example: bimaControlHost emulate=true
 *
 * @description
 * This program is the main control DO and monitor conduit for
 * a BIMA antputer.
 *
 * @key emulate false b Emulate (true for emulation). 
 * @key antenna @noDefault s To uniquely identify which antenna.
 *                           Defaults to gethostname().
 * @logger DEFAULT_FACILITY carma.bima.ControlHost
 */ 
int Program::main()
{
  CPTRACE( Trace::TRACE1, "Starting bimaControlHost, Tracing..." );

  ControlServer *cs;
  Category &log = Program::getLogger();
  SharedMemory *bimaShm;

  try
  {
    AntennaNameResolver *anr;

    if ( parameterWasSpecified( "antenna" ) )
      anr = new AntennaNameResolver( getStringParameter( "antenna" ).c_str() );
    else
      anr = new AntennaNameResolver();

    Configuration config( anr->getAntennaName(), Program::getConfDir() );
    config.setEmulate( getBoolParameter( "emulate" ) );

    setInstanceLogname( string( string( "carma." )
	  + config.getAntenna()
	  + string( ".ControlHost" ) ) );

    int startattempts = 0;
    CPTRACE(Trace::TRACE2,"SHM1");
    while ( true )
    {
      try
      {
	CPTRACE(Trace::TRACE2,"CHM");
	bimaShm = new SharedMemory( config.getAntenna().c_str() );
	CPTRACE(Trace::TRACE2,"DONE");
	break;
      }
      catch ( ... )
      {
	CPTRACE(Trace::TRACE2,"CATCH");
	if ( startattempts++ < 10 ) 
	{
	  log << Priority::WARN << "Unable to open shared memory file, retrying in 1 second";
	  sleep(1);
	}
	else
	  throw CARMA_ERROR( "Unable to open shared memory file after 9 attempts!" );
      }
    } 

    CPTRACE(Trace::TRACE2,"GET SERVER");
    carma::corba::Server & server = getCorbaServer();
    CPTRACE( Trace::TRACE2, "Creating ControlServer Object" );
    cs = new ControlServer( config, server );

    // cs destructor automagically takes care of remove_refs...
    CPTRACE( Trace::TRACE5, "Running server (runORB)..." );

    cs->runServer();

  }
  catch ( ErrorException& err )
  {
    log << Priority::ERROR << err.what();
    bimaShm->putData("CONTRLUP", &STOPPED);
    return EXIT_FAILURE;
  }

  bimaShm->putData("CONTRLUP", &STOPPED);
  return EXIT_SUCCESS;
};


