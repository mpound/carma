/*
 * $Id: carmaPhaseMonitor.cc,v 1.25 2013/02/06 20:07:29 abeard Exp $
 *
 * @usage carmaPhaseMonitor
 * @key output "/home/obs/phasemon" s Data output directory to write raw data.
 *                                    Data is still written to DBMS as well.
 * @key parameters @noDefault s Use different parameters file than conf/phasemonitor/CedarFlat.tab
 * @key device "/dev/ttyUSB2" s Device file
 * @key emulate false b emulation
 * @key record @noDefault s Record raw voltages to specified file.  See replay.
 * @key replay @noDefault s Replay voltages from recorded file (implies emulate). See record.
 * @key testBadVolts false b testBadVolts handling
 * @key readTemp false b Read temperature and exit
 * @key awdelay  0.275  d Monitor system autowrite delay in seconds.
 *
 * @logger ENVIRONMENT_FACILITY carma.environment.PhaseMonitor
 */

#include "log4cpp/Category.hh"

#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/loggingUtils.h"

#include "carma/util/Trace.h"

#include "carma/monitor/PhaseMonitorSubsystem.h"

#include "carma/phasemonitor/AntennaParameters.h"
#include "carma/phasemonitor/PhaseMonitorDevice.h"
#include "carma/phasemonitor/PhaseMonitorWorker.h"
#include "carma/phasemonitor/PhaseMonitorSamples.h"

#include <boost/thread.hpp>

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::monitor;
using namespace carma::phasemonitor;

int Program::main()
{
  CPTRACE( Trace::TRACE1, "Program::main() - carmaPhaseMonitor entry" );

  Category &log = Program::getLogger();
  CPTRACE( Trace::TRACE7, "Acquired logging handle: " << (hex) << &log );

  try
  {
    bool emulate = getBoolParameter( "emulate" );
    bool testBadVolts = getBoolParameter( "testBadVolts" );
    bool readTemp = getBoolParameter( "readTemp" );
    string outDir = getStringParameter( "output" );
    string device = getStringParameter( "device" );
    string params;

    if ( parameterWasSpecified( "parameters" ) )
      params = getStringParameter( "parameters" );
    else
      params = string( Program::getConfDir()
          + string( "phasemonitor/CedarFlat.tab" ) );

    string replay;
    if ( parameterWasSpecified( "replay" ) )
      replay = getStringParameter( "replay" );
    else
      replay = "";

    string record;
    if ( parameterWasSpecified( "record" ) )
      record = getStringParameter( "record" );
    else
      record = "";

    CPTRACE( Trace::TRACE1, "Options:" );
    CPTRACE( Trace::TRACE1, " Data dir: " << outDir );
    CPTRACE( Trace::TRACE1, " Parameters file: " << params );
    CPTRACE( Trace::TRACE1, " Device: " << device );
    CPTRACE( Trace::TRACE1, " testBadVolts: " << (boolalpha) << testBadVolts );
    CPTRACE( Trace::TRACE1, " emulate: " << (boolalpha) << emulate );

    if ( readTemp == true )
    {
      PhaseMonitorDevice phmd( device, emulate, record );
      cout << "Box temp (C): " << phmd.queryTemperatureC() << endl;
      return EXIT_SUCCESS;
    }

    log << Priority::INFO << "Starting up";

    AntennaParameters phparams( params );
    PhaseMonitorDevice pmd( device, emulate, record, testBadVolts, replay ); 
    PhaseMonitorSamples pms( outDir + "/" + "sample.dat" );

    if ( !replay.empty() && !emulate ) {
        // Replaying while emulate==false provides a way to produce
        // calibrated samples from raw.dat files (or subsets thereof).
        // It doesn't need a monitor system, imr or anything fancy.
        PhaseMonitorWorker pmw( outDir, 0, pmd, phparams, pms );
        pmw.replay();
        return EXIT_SUCCESS;
    }

    const double autoWriteDelayInS = getDoubleParameter("awdelay");
    PhaseMonitorSubsystem mon;
    mon.startAutoWriter( autoWriteDelayInS );

    PhaseMonitorWorker pmw( outDir, &mon, pmd, phparams, pms );
        
    boost::thread thread( boost::ref( pmw ) );
    
    bool done = false;
    while ( !done ) {
        // If the IMR is trying to shut us down, terminate our thread.
        // If not, check if thread has terminated on it's own.
        if ( imrTerminationRequested() ) { 
            thread.interrupt( );
            thread.join( );
            done = true;
        } else { 
            done = thread.timed_join( boost::posix_time::milliseconds( 100 ) );
        }
    }

  } catch ( ... ) {
    string msg;
    {
        ostringstream oss;

        oss << "Caught exception in carmaPhaseMonitor Program::main: "
            << getStringForCaught() << "\n";

        msg = oss.str();
    }

    cerr << msg << endl;

    logMultipleLines( log, Priority::ERROR, msg );

    CPTRACE( Trace::TRACE1, msg );

    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
