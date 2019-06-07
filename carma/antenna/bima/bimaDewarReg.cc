/** @file
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.11 $
 * $Date: 2012/02/21 21:06:56 $
 * $Id: bimaDewarReg.cc,v 1.11 2012/02/21 21:06:56 abeard Exp $
 */

// C++ includes
#include <string>

// System includes
#include <unistd.h>
#include <pthread.h>
#include <errno.h>

// CARMA includes
#include "carma/antenna/bima/TelemetryClient.h"
#include "carma/antenna/bima/Dewar.h"
#include "carma/antenna/bima/DewarRegulation.h"
#include "carma/antenna/bima/control/BimaDewarRegulationImpl.h"
#include "carma/antenna/bima/Configuration.h"
#include "carma/antenna/bima/AntennaNameResolver.h"
#include "carma/util/Program.h"
#include "carma/util/StartPthread.h"


#include "carma/services/Global.h"

#include "log4cpp/Category.hh"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::antenna::bima;

/** 
 * @version $Revision: 1.11 $
 *
 * @usage Usage: bimaDewarReg [antenna=bima1]
 * Example: bimaDewarReg antenna=bima1 
 *
 * @description bimaDewarRx
 *
 * @key antenna     @noDefault s Used to uniquely identify this instance, by antenna.
 *                               Uses gethostname() if not specified.
 * @key emulate     false   b Emulate operation.
 * @key stage       4       i Which stage sensor to use.
 * @key reg         false   b Run as server to regulate temperature
 * @key set         3.5     d Set point for temperature regulation.
 * @key defrost     false   b Set defrost on/off
 * @key diag        false   b Tell daemon to write to /tmp/dereg.log
 *                            (target K, stage3 K, cycle min 2s, cycle max 2s, heater3 mW)
 * @key gaini       @noDefault d Override what's found in dewars.tab
 * @key gainp       @noDefault d Override what's found in dewars.tab
 * @logger RX_FACILITY carma.bima.DewarRegulation
 */ 
int Program::main()
{
  Category &log = Program::getLogger();

  try
  {
    AntennaNameResolver *anr;

    if ( parameterWasSpecified( "antenna" ) )
      anr = new AntennaNameResolver( getStringParameter( "antenna" ).c_str() );
    else
      anr = new AntennaNameResolver();

    Configuration config( anr->getAntennaName(), Program::getConfDir() );
    bool server = getBoolParameter( "reg" );
    bool diag = getBoolParameter( "diag" );
    bool emulate = getBoolParameter( "emulate" );
    double targetTemp = getDoubleParameter( "set" );

    setInstanceLogname( string( string( "carma." ) + config.getAntenna()
	  + string( ".DewarRegulation" ) ) );

    // Removed check to see if already running
    // the imr already checks this for us!
    DewarRegulation dereg( config, server, emulate );

    if ( parameterWasSpecified( "defrost" ) )
    {
      if ( getBoolParameter( "defrost" ) == true )
	dereg.defrost();
      else
	dereg.cancelDefrost();

      exit(EXIT_SUCCESS);
    }

    if ( parameterWasSpecified( "gaini" ) )
    {
	dereg.getDewar().setGainI( getDoubleParameter( "gaini" ) );
    }

    if ( parameterWasSpecified( "gainp" ) )
    {
	dereg.getDewar().setGainP( getDoubleParameter( "gainp" ) );
    }

    dereg.setStage( getIntParameter( "stage" ) );

    if ( server == true )
    {
      CPTRACE( Trace::TRACE2,
	  "Launching dewar regulation with the following parameters," << endl
	  << "  antenna: " << config.getAntenna() << endl
	  << "  dewar: " << config.getDewarName() );

      dereg.setPoint( targetTemp );

      // Handles actual monitoring and control of heater/defrost
      CPTRACE( Trace::TRACE2, "Starting up regulation thread..." );
      pthread_t dewarRegulator = StartPthreadWithRef( DewarRegulation::thread,
	  dereg );

      // BimaDewarRegulation::thread does a runORB()...
      CPTRACE( Trace::TRACE2, "Starting up regulation control thread..." );
      BimaDewarRegulationImpl bdri( dereg, emulate );
      pthread_t dewarRegulatorControl = StartPthreadWithRef(
	  BimaDewarRegulationImpl::thread, bdri );

      while ( true )
      {
	// no-op/avoid compiler warnings
	if ( dewarRegulatorControl == 0 )
	  throw CARMA_ERROR(
	      string( "dewarRegulatorControl == 0 This should not happen!") );
	else if ( dewarRegulator == 0 )
	  throw CARMA_ERROR(
	      string( "dewarRegulator == 0 This should not happen!") );
	else
	{
          CPTRACE( Trace::TRACE2, "Threads started, going to sleep in program thread" );
	  sleep( 3600 );
	}
      }
    }
    else
    {
      CPTRACE( Trace::TRACE2,
	  "Modifying dewar regulation set point," << endl
	  << "  antenna: " << config.getAntenna() << endl
	  << "  dewar: " << config.getDewarName() << endl );

      cout << "  Current set point: " << dereg.getPoint() << endl;
      cout << "  Setting new point: " << targetTemp << endl;

      if ( dereg.alreadyRunning() )
      {
	if ( diag )
	  cout << "  Turning diagnostic logging on, see /tmp/dereg.log" << endl;
	else
	  cout << "  Turning diagnostic logging off, see /tmp/dereg.log" << endl;

	dereg.startDiagLog( diag );

	dereg.setPoint( targetTemp );

      }
      else
	cout << "bimaDewarReg daemon not running! Restart using the imr." << endl;
    }

    CPTRACE( Trace::TRACE5, "Creating DewarRegulation Object" );
  }
  catch ( ErrorException& err )
  {
    log << Priority::ERROR << "Caught ErrorException: " << err.what();
    cerr << err.what() << endl;
    return EXIT_FAILURE;
  }
  catch ( ... )
  {
    log << Priority::ERROR << "Unexpected exception";
    cerr << "Unexpected exception, unknown type!" << endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

