/** @file
 * Program::main implementation for the CARMA Monitor Stream
 * publishing.
 *
 * @author Colby Gutierrez-Kraybill
 * $Revision: 1.26 $
 * $Date: 2012/12/14 01:30:53 $
 * $Id: bimaMonitorHost.cc,v 1.26 2012/12/14 01:30:53 abeard Exp $
 */

// CARMA/CORBA
#include "carma/corba/corba.h"

// System includes
#include <unistd.h>
#include <pthread.h>
#include <signal.h>
#include <errno.h>

#include "carma/antenna/bima/MonitorUpdater.h"
#include "carma/antenna/bima/SharedMemory.h"
#include "carma/antenna/bima/AntennaNameResolver.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/Logger.h"


using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::antenna::bima;

/** 
 * @version $Revision: 1.26 $
 *
 * @usage Usage: bimaMonitorHost
 * Example: bimaMonitorHost
 *
 * @description
 * This program provides monitor stream information, based on
 * the contents of the BIMA shared memory.
 *
 * @key antenna     @noDefault  s Used to uniquely identify this instance, by antenna. (e.g. bima1)
 * @key emulate     false       b Run daemon in emulate mode.
 * @key awdelay     0.200       d Monitor system autowrite delay in seconds.
 * @logger DEFAULT_FACILITY carma.bima.MonitorHost
 */ 
int Program::main()
{
  AntennaNameResolver *anr;

  if ( parameterWasSpecified( "antenna" ) )
    anr = new AntennaNameResolver( getStringParameter( "antenna" ).c_str() );
  else
    anr = new AntennaNameResolver();

  // Hard sleep to ensure that the FSP has started before this process gets rolling
  sleep(20);

  Configuration config( anr->getAntennaName(), Program::getConfDir() );
  config.setEmulate( getBoolParameter( "emulate" ) );

  Category& log = getLogger();

  setInstanceLogname( string( string( "carma." )
	+ config.getAntenna()
	+ string( ".MonitorHost" ) ) );

  try
  {
    SharedMemory *bimaShm;
    int startattempts = 0;
    while ( true )
    {
      try
      {
	bimaShm = new SharedMemory( config.getAntenna().c_str() );
	break;
      }
      catch ( ... )
      {
	if ( startattempts++ < 30 )
	{
	  log << Priority::WARN << "Unable to open shared memory file, retrying in 1 second";
	  sleep(3);
	}
	else
	  throw CARMA_ERROR( "Unable to open shared memory file after 29 attempts!" );
      }
    }

    CPTRACE( Trace::TRACE2, "Creating monitor handle for: " << config.getAntenna() );

    const double awdelay = getDoubleParameter( "awdelay" );
    MonitorUpdater *monitorUpdater = new MonitorUpdater( config, log, awdelay );

    pthread_t monitorThread;

    if ( pthread_create( &monitorThread, (pthread_attr_t *)NULL,
	  MonitorUpdater::startThread, (void *)monitorUpdater ) != 0 )
      throw CARMA_ERROR( strerror(errno) );

    monitorUpdater->deadManCheck();
    CPTRACE(Trace::TRACE2,"sleeping");
    sleep(7); // Make sure the system has had enough time to init

    int restarts = 0;
    bimaShm->putData( "MONRESTARTS" , &restarts );

    while ( true )
    {
      if ( monitorUpdater->deadManCheck() == true )
      {
	CPTRACE( Trace::TRACE2, "deadManCheck() == true " );
	sleep(2);
      }
      else
      {
	CPTRACE( Trace::TRACE1, "DEAD MAN CHECK FAILED!" );
	pthread_kill( monitorThread, SIGSTOP );
	delete monitorUpdater;
	monitorUpdater = new MonitorUpdater( config, log, awdelay );
	if ( pthread_create( &monitorThread, (pthread_attr_t *)NULL,
	      MonitorUpdater::startThread, (void *)monitorUpdater ) != 0 )
	  throw CARMA_ERROR( strerror(errno) );
	sleep(5); // Make sure the system has had enough time to init
	restarts++;
	bimaShm->putData( "MONRESTARTS" , &restarts );
      }
    }
  }
  catch ( carma::util::ErrorException& err )
  {
    cerr << err.what() << endl;
    return EXIT_FAILURE;
  }
  catch ( std::exception &stdex )
  {
    cerr << stdex.what() << endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
};


