/* $Id: weatherStation.cc,v 1.49 2012/01/23 22:29:31 mpound Exp $ */
/* vim: set ts=4 sts=4 sw=4 et: */

#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>

#include <carma/corba/corba.h>
#include <carma/util/ErrorException.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/Program.h>
#include <carma/util/Logger.h>
#include <carma/util/PthreadMutex.h>
#include <carma/util/programLogging.h>
#include <carma/util/Trace.h>
#include <carma/util/AutoPthreadQuitAndJoinGroup.h>
#include <carma/util/ScopedPthreadMutexLock.h>
#include <carma/util/StartPthread.h>
#include <carma/util/FrameAlignedTimer.h>
#include <carma/util/ThreadQuit.h>
#include <carma/monitor/WeatherSubsystem.h>
#include <carma/environment/Weather.h>

using namespace carma::monitor;
using namespace carma::environment;
using namespace carma::util;
using namespace std;


/** @mainpage
 *
 * The primary purpose of the WeatherStation subsystem is to monitor
 * our weather station hardware, and pass weather variables into the
 * monitor stream. We currently use two weatherstations, named WS
 * (the Weather Station) and the DPS (Dew Point Sensor).
 *
 * Related Documentation:
 * - <A HREF="http://www.mmarray.org/project/WP/WeatherStation/sw/current.pdf">
 *   WeatherStation Software Design</A>
 *
 * @author Peter Teuben
 */

//
// @version     $Revision: 1.49 $
//
// @usage       grabs weather station values and dumps them into the monitor stream
//
// @logger ENVIRONMENT_FACILITY carma.environment.weatherStation
//
// @description
// This program serves the WeatherStation DO. It can use two instruments
// to obtain the weather:
// WS (Weather Station)  or DPS (Dew Point Sensor).
// WS contains most weather info, DPS only measures Ta and Td, and if selected
// it will override those values from WS.
// The WeatherStation DO can run with either, or both, WS and DPS activated, and
// of course run in a simple emulatation mode for (off-site) testing.
// It is probably not a good idea to not emulate and have both the WS and DPS
// disabled.
//
// @key WSdev    /dev/ttyUSB0 s  Device name for WS, if used
// @key DPSdev   /dev/ttyUSB1 s  Device name for DPS, if used
// @key emulate  true         b  Emulation mode.
// @key ws       true         b  Should the WS be used
// @key dps      true         b  Should the DPS be used (if so, it uses Ta/Tb instead of WS)
// @key delay    0            i  Time in seconds to delay opening WS
// @key reopen   false        b  Close and Reopen device for each call?
// @key publish  2            i  publish MP? 0=nothing  1=shared mem, but don't write   2=write
//

#define CPTRACE1(args...) CARMA_CPTRACE(::carma::util::Trace::TRACE1, args)

/*----------------------------------------------------------------------------*/
/* Main Weather Station Program                                               */
/*----------------------------------------------------------------------------*/

/*
 *   setSigAction(): if needed to bypass carma signal handler.
 */

static void setSigAction()
{
    struct sigaction action;
    // action.sa_flags = SA_SIGINFO;
    sigfillset (&(action.sa_mask));
    action.sa_handler = SIG_DFL; // Specify Default action - abort().
    action.sa_restorer = NULL;
    sigaction (SIGINT, &action, NULL);
}

int Program::main()
{
    int status  = EXIT_SUCCESS;

    setSigAction();

    carma::monitor::WeatherSubsystem wss;
    wss.timestamp().setValue(Time::MJD());
    wss.write();

    try {
        // first get the command line parameters
        const int publish  = getIntParameter("publish");
        const unsigned int delay = static_cast<unsigned int>(getIntParameter("delay"));

        const std::string WSdev = getStringParameter("WSdev");
        const std::string DPSdev = getStringParameter("DPSdev");
        const bool useWS = getBoolParameter("ws");
        const bool useDPS = getBoolParameter("dps");
        const bool reopen = getBoolParameter("reopen");
        const bool emulate = getBoolParameter("emulate");

        /* start the weather station reader */
        Weather weather(WSdev, DPSdev, useWS, useDPS, reopen, emulate);
        weather.startThreads();

        /*
         * delay execution to give the weather station reader time to start
         * before we start writing the monitor system data
         */
        if (delay > 0)
            sleep(delay);

        /* frame aligned timer */
        const unsigned int delay_nsec = 200 * 1000 * 1000;
        carma::util::FrameAlignedTimer timer(delay_nsec, 1);

        /* loop forever */
        while (true) {

            // check for quit, wait for the next frame time to write
            ThreadQuitTestSelf();
            timer.ResetNextFireTimeAndWait();
            ThreadQuitTestSelf();

            // write the weather data to our internal shared memory
            if (publish >= 1) {
                double timestamp = ::carma::util::Time::MJD();

                // the main weather everybody should use
                wss.timestamp().setValue(timestamp);
                wss.online().setValue(true);

                weather.copyToMonitorSystem(wss);
            }

            // write the monitor points
            if (publish >= 2) {
                CPTRACE1("Write data to monitor system");
                wss.write();
            }
        }

    } catch ( CORBA::Exception &ex ) {
        std::ostringstream oss;

        oss << "CORBA exception: " << getStringForCaught();
        programLogCriticalIfPossible(oss.str());
        std::cerr << oss.str() << std::endl;

        status = EXIT_FAILURE;
    } catch ( carma::util::ErrorException &eex ) {
        std::ostringstream oss;

        oss << "Error exception: " << getStringForCaught();
        programLogCriticalIfPossible(oss.str());
        std::cerr << oss.str() << std::endl;

        status = EXIT_FAILURE;
    } catch(...) {
        std::ostringstream oss;

        oss << "Uncaught exception: " << getStringForCaught();
        programLogCriticalIfPossible(oss.str());
        std::cerr << oss.str() << std::endl;

        status = EXIT_FAILURE;
    }

    return status;
}
