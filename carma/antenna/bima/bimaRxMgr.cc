/** @file
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.12 $
 * $Date: 2012/08/01 14:13:04 $
 * $Id: bimaRxMgr.cc,v 1.12 2012/08/01 14:13:04 friedel Exp $
 */

// C++ includes
#include <string>

// System includes
#include <unistd.h>
#include <errno.h>
#include <signal.h>

// CARMA includes
#include "carma/util/StartPthread.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

#include "carma/antenna/bima/RxMgrThread.h"
#include "carma/antenna/bima/AntennaNameResolver.h"
#include "carma/antenna/bima/ProcessMonitor.h"

#include "log4cpp/Category.hh"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::antenna::bima;

/** 
 * @version $Revision: 1.12 $
 *
 * @usage Usage: bimaRxMgr [antenna=bima1]
 * Example: bimaRxMgr antenna=bima1
 *
 * @description BIMA antenna receiver manager
 *
 * @key antenna     @noDefault s Used to uniquely identify this instance,
 *                               by antenna.  If not specified, defaults
 *                               to gethostname();
 * @key emulate     false      b Put server into emulate mode.
 * @logger RX_FACILITY carma.bima.RxMgr
 */ 
int Program::main()
{
  Category &logger = Program::getLogger();
  AntennaNameResolver *anr;
  SharedMemory *bimaShm;

  try
  {
    if ( parameterWasSpecified( "antenna" ) )
      anr = new AntennaNameResolver( getStringParameter( "antenna" ).c_str() );
    else
      anr = new AntennaNameResolver();
  
    Configuration config( anr->getAntennaName(), Program::getConfDir() );
    config.setEmulate( getBoolParameter( "emulate" ) );

    setInstanceLogname( string( string( "carma." ) + config.getAntenna()
                                + string( ".RxManager" ) ) );
    CPTRACE( Trace::TRACE2,
             "Launching Rx Manager with the following parameter(s)," << endl
             << "  antenna: " << config.getAntenna() );
              
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
	  logger << Priority::WARN << "Unable to open shared memory file, retrying in 3 second";
	  sleep(3);
	}
	else
	  throw CARMA_ERROR( "Unable to open shared memory file after 29 attempts!" );
      }
    }

    Rx rx( config );
    LO lo( config );
    Secondary secondary( config );
    CalWheel calwheel( config );
    RxMgrThread rxThread( config, rx, lo, secondary, calwheel );

    pthread_t rxMgrThreadId = StartPthreadWithRef( RxMgrThread::thread, rxThread );

    while ( true )
    {
      double time = Time::MJD();
      bimaShm->putData("RXUP", &time);
      if ( rxThread.isOk() )
	sleep( 5 );
      else
      {
	logger << Priority::WARN << "Rx Control Thread locked!  Removing "
	  << "old thread and creating new one";
	pthread_kill( rxMgrThreadId, 9 );
	rxMgrThreadId = StartPthreadWithRef( RxMgrThread::thread, rxThread );
      }
    }
  }
  catch ( std::exception &ex )
  {
    logger << Priority::ERROR << "Caught exception: " << ex.what();
    cerr << ex.what() << endl;
    bimaShm->putData("RXUP", &STOPPED);
    return EXIT_FAILURE;
  }
  catch ( ... )
  {
    logger << Priority::ERROR << "Caught unknown exception!";
    cerr << "Caught unknown exception!" << endl;
    bimaShm->putData("RXUP", &STOPPED);
    return EXIT_FAILURE;
  }

  bimaShm->putData("RXUP", &STOPPED);
  return EXIT_SUCCESS;
}

