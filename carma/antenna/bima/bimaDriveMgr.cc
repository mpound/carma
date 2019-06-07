/** @file
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.13 $
 * $Date: 2012/12/12 18:53:13 $
 * $Id: bimaDriveMgr.cc,v 1.13 2012/12/12 18:53:13 plambeck Exp $
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

#include "carma/antenna/bima/Drives.h"
#include "carma/antenna/bima/DriveMgrThread.h"
#include "carma/antenna/bima/Configuration.h"
#include "carma/antenna/bima/AntennaNameResolver.h"
#include "carma/antenna/bima/ProcessMonitor.h"
 
#include "log4cpp/Category.hh"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::antenna::bima;

/** 
 * @version $Revision: 1.13 $
 *
 * @usage Usage: bimaDriveMgr [antenna=bima1]
 * Example: bimaDriveMgr antenna=bima1
 *
 * @description Manages the drive for a BIMA antenna
 *
 * @key antenna     @noDefault s Used to uniquely identify this instance, by antenna.
 *                               If not specified, uses gethostname(2), see man page for details.
 * @key emulate	    false      b Turn emulation on/off.
 * @logger DEFAULT_FACILITY carma.bima.DriveMgr
 */ 
int Program::main()
{
  Category &logger = Program::getLogger();
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

    setInstanceLogname( string( string( "carma." ) + config.getAntenna()
	  + string( ".DriveManager" ) ) );
    CPTRACE( Trace::TRACE2,
	"Launching Drive Manager with the following parameter(s)," << endl
	<< "  antenna: " << config.getAntenna() );

    int startattempts = 0;
    while ( true )
    {
      try
      {
	CPTRACE(Trace::TRACE2,"SHM");
	bimaShm = new SharedMemory( config.getAntenna().c_str() );
	CPTRACE(Trace::TRACE2,"DONE");
	break;
      }
      catch ( ... )
      {
	if ( startattempts++ < 30 )
	{
	  logger << Priority::WARN << "Unable to open shared memory file, retrying in 3 seconds";
	  sleep(3);
	}
	else
	  throw CARMA_ERROR( "Unable to open shared memory file after 29 attempts!" );
      }
    }
    Drives drives( config );
    DriveMgrThread driveThread( config, drives );

    // default is simple, non-iterative calculation in Drives.cc
    int iterate = 0;
    bimaShm->putData("ITERATE", &iterate);

    // Set up default for CLOSE/TRACK tolerance, 3 arc sec
    drives.setTolerance( 3 );

    pthread_t driveMgrThreadId = StartPthreadWithRef( DriveMgrThread::thread, driveThread );

    while ( true )
    {
      if ( driveThread.isOk() ){
	double time = Time::MJD();
	bimaShm->putData("DRIVEUP", &time);
	sleep( 5 );
      }
      else
      {
	CPTRACE(Trace::TRACE2,"RESPAWN");
	logger << Priority::WARN << "Drive Control Thread locked!  Removing "
	  << "old thread and creating new one";
	pthread_kill( driveMgrThreadId, 9 );
	driveMgrThreadId = StartPthreadWithRef( DriveMgrThread::thread, driveThread );
      }
    }
  }
  catch ( std::exception &ex )
  {
    logger << Priority::ERROR << "Caught exception: " << ex.what();
    cerr << ex.what() << endl;
    bimaShm->putData("DRIVEUP", &STOPPED);
    return EXIT_FAILURE;
  }
  catch ( ... )
  {
    logger << Priority::ERROR << "Caught unknown exception!";
    cerr << "Caught unknown exception!" << endl;
    bimaShm->putData("DRIVEUP", &STOPPED);
    return EXIT_FAILURE;
  }
  bimaShm->putData("DRIVEUP", &STOPPED);
  return EXIT_SUCCESS;
}

