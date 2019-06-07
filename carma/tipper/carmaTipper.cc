/* 
 * $Id: carmaTipper.cc,v 1.8 2011/09/01 18:19:13 abeard Exp $
 *
 * @usage carmaTipper
 * @key update 300 i Seconds between tipper data gathering and Tau computation.
 * @key device "/dev/ttyUSB3" s Default serial port to use.
 * @key dir "/misc/array/rt/TipperData" s Directory to write data files to.
 * @key awdelay  0.175  d Monitor system autowrite delay in seconds.
 * @key emulate false b Emulation mode.
 * @logger ENVIRONMENT_FACILITY carma.tipper.Tipper
 * 
 */

#include <iostream>
#include <errno.h>

// CORBA
#include "carma/corba/corba.h"

// CARMA/CORBA
#include "carma/corba/Server.h"

#include "log4cpp/Category.hh"

#include "carma/util/Program.h"
#include "carma/util/IPQwriter.h"
#include "carma/util/programLogging.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/loggingUtils.h"
#include "carma/util/StartPthread.h"
#include "carma/util/Trace.h"

#include "carma/tipper/TipperControl.h"
#include "carma/tipper/TipperControl_skel.h"
#include "carma/tipper/TipperControl_skel_tie.h"
#include "carma/tipper/TipperControlImpl.h"

#include "carma/tipper/TipperAutoUpdateThread.h"

using namespace ::std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::tipper;

int Program::main()
{
  CPTRACE( Trace::TRACE1, "Program::main() - carmaTipper entry" ); 

  Category &log = Program::getLogger();
  CPTRACE( Trace::TRACE7, "Acquired logging handle: " << (hex) << &log );
  
  try
  {
    int update = getIntParameter( "update" );
    string device = getStringParameter( "device" );
    string dataDir = getStringParameter( "dir" );
    const double autoWriteDelayInS = getDoubleParameter( "awdelay" );
    bool emulate = getBoolParameter( "emulate" );


    // This is waiting for an idl spec for Tipper...
    CPTRACE( Trace::TRACE1, " Launching control DO thread" );

    // Everything from here on is CORBA related
    TipperControlThread * tControl = new TipperControlThread( device.c_str(),
	dataDir.c_str(), autoWriteDelayInS, emulate );

    // Create a servant 
    TipperControlImpl * impl = new TipperControlImpl( *tControl );

    carma::corba::Server & server = getCorbaServer( );
    server.addServant< POA_carma::tipper::TipperControl_tie >( *impl, 
                                                               TIPPER_NAME );

    // Servant is incarnated and published at this stage.
    log << log4cpp::Priority::INFO
      << "Device IOR successfully published on nameserver as '"
      << TIPPER_NAME << "'";

    log << log4cpp::Priority::INFO
      << "Launching auto update thread";
    CPTRACE( Trace::TRACE2, " StartPthreadWithRef()" );

    TipperAutoUpdateThread autoUpdate( *tControl, update );
    pthread_t pht = StartPthreadWithRef( TipperAutoUpdateThread::thread,
	autoUpdate, "TipperAutoUpdaterThread" );
    CPTRACE( Trace::TRACE3, "  TipperAutoUpdaterThread pthreadid: " << pht );

    // Block on orb forever.  The runOrb command takes care of POA
    // activation.
    server.run( );

    // If we get here, the ORB was shutdown via the IMR (not ^C).  

  }
  catch ( const char *msg )
  {
    cerr << "error: " << msg << endl;
    CPTRACE( Trace::TRACE1, msg );
    programLogErrorIfPossible( msg );
  }
  catch ( runtime_error &rte )
  {
    cerr << "runtime_error: " << rte.what() << endl;
    CPTRACE( Trace::TRACE1, rte.what() );
    programLogErrorIfPossible( rte.what() );
  }
  catch ( ... )
  {
    string msg;
    {
      ostringstream oss;

      oss << "Caught exception in carmaTipper Program::main: "
	<< getStringForCaught() << "\n"
	<< getCaughtBacktraceAsString( );

      msg = oss.str();
    }

    cerr << msg << endl;

    logMultipleLines( log, Priority::ERROR, msg );

    CPTRACE( Trace::TRACE1, msg );

    return EXIT_FAILURE;
  }

  CPTRACE( Trace::TRACE1, "exiting main..." );
  return EXIT_SUCCESS;
}
