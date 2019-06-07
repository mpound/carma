/**
 * Accumulates average monitor data by type, and writes the averages
 * to specified output streams.
 *
 * @author: Amar Amarnath
 * @version $Id: monitorAverageWriter.cc,v 1.137 2014/07/23 20:01:16 scott Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <memory>

#include "carma/dbms/DBConfigurator.h"
#include "carma/dbms/dbFileManagers.h"
#include "carma/dbms/TagIDAuthority.h"
#include "carma/monitor/AverageAccumulator.h"
#include "carma/monitor/MonitorPoint.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorSystemSelector.h"
#include "carma/monitor/SlPipelineSubsystemExt.h"
#include "carma/monitor/WbPipelineSubsystemExt.h"
#include "carma/util/CommonExceptions.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/FileUtils.h"
#include "carma/util/StopWatch.h"
#include "carma/util/Logger.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

using namespace ::std;
using namespace carma;
using namespace carma::dbms;
using namespace carma::monitor;
using namespace carma::util;
using carma::util::StopWatch;
using carma::monitor::MonitorPoint;

namespace {

typedef MonitorPoint MP;

// we get a frame every half-second
const long kFramesPerSecond = 2L;
const long kSecondsPerMinute = 60L;
const long kFramesPerMinute = (kFramesPerSecond * kSecondsPerMinute);
const bool kDoDbload = true;

monitor::MonitorPoint::ARCHIVE_PRIORITY 
str2priority( const string & priorityStr )
{ monitor::MonitorPoint::ARCHIVE_PRIORITY priority;
  if(priorityStr == "verbose") {
    priority =     monitor::MonitorPoint::VERBOSE;
  } else if(priorityStr == "debug") {
    priority =     monitor::MonitorPoint::DEBUG;
  } else if(priorityStr == "normal") {
    priority =     monitor::MonitorPoint::NORMAL;
  } else if(priorityStr == "useful") {
    priority =     monitor::MonitorPoint::USEFUL;
  } else if(priorityStr == "vital") {
    priority =     monitor::MonitorPoint::VITAL;
  } else {
    throw CARMA_ERROR("Unknown priority " + priorityStr);
  }
  return priority;
}

bool
IsRunning( const string & processName ) {
    // FIXME linux system command dependency
    const string command = string( "/bin/ps -C " ) + processName;

    FILE * ptable = popen( command.c_str( ), "r" );

    const int kBufferCount = 300;
    char buffer[ kBufferCount ];

    int lineCount = 0;

    while ( fgets( buffer, kBufferCount, ptable ) ) {
        ++lineCount;

        // one line is the header, one line is for this instance of the
        // app, a third line indicates another instance of this app
        if ( lineCount > 2 )
            break;
    }

    pclose( ptable );

    return (lineCount > 2);
}

void
resetStopWatchesAndLogIfSpecified( 
    StopWatch & swTotal, StopWatch & swWallTotal,
    StopWatch & swReadMPs, StopWatch & swWallReadMPs,
    StopWatch & swAccumulation, StopWatch & swWallAccumulation,
    StopWatch & swWriteFrame, StopWatch & swWallWriteFrame,
    StopWatch & swWriteMinute, StopWatch & swWallWriteMinute, 
    StopWatch & swWriteAstro, StopWatch & swWallWriteAstro,
    const bool log )
{
    swWallTotal.stop();
    swTotal.stop();

    const double totalCpuTime = swTotal.getCumulativeElapsedTime(true);
    const double totalWallTime = swWallTotal.getCumulativeElapsedTime(true);

    const double readMPCpuTime = swReadMPs.getCumulativeElapsedTime(true);
    const double readMPWallTime = swWallReadMPs.getCumulativeElapsedTime(true); 

    const double accumCpuTime = swAccumulation.getCumulativeElapsedTime(true);
    const double accumWallTime = 
        swWallAccumulation.getCumulativeElapsedTime(true);

    const double writeFrameCpuTime = 
        swWriteFrame.getCumulativeElapsedTime(true);
    const double writeFrameWallTime = 
        swWallWriteFrame.getCumulativeElapsedTime(true);

    const double writeMinuteCpuTime = 
        swWriteMinute.getCumulativeElapsedTime(true);
    const double writeMinuteWallTime = 
        swWallWriteMinute.getCumulativeElapsedTime(true);

    const double writeAstroCpuTime = 
        swWriteAstro.getCumulativeElapsedTime(true);
    const double writeAstroWallTime = 
        swWallWriteAstro.getCumulativeElapsedTime(true);

    if ( totalCpuTime > 0.0 && log ) {
        std::ostringstream msg;
        msg << "CPU/Wall times: "
            << "Read MP (blocks) = " << readMPCpuTime 
            << "s/" << readMPWallTime << "s, " 
            << "Accumulate Averages = " << accumCpuTime 
            << "s/" << accumWallTime << "s, "
            << "Write Frame Data = " << writeFrameCpuTime 
            << "s/" << writeFrameWallTime << "s, "
            << "Write Minute Data = " << writeMinuteCpuTime 
            << "s/" << writeMinuteWallTime << "s, "
            << "Write Astro Data = " << writeAstroWallTime 
            << "s/" << writeAstroCpuTime << "s, "
            << "Total CPU Time = " << totalCpuTime << "s, "
            << "Total Wall Clock Time = " << totalWallTime << "s.";
        programLogErrorIfPossible( msg.str( ) );
    }

    swWallTotal.start();
    swTotal.start();

} // resetStopWatchesAndLogIfSpecified()

}  // anonymous namespace


//
// @description accumulate monitor sample values and write averages to
//              specified output streams.
//
// @usage monitorAverageWriter [--keywords] [--help] [--usage] 
//        [inputCMS=[final]] [dbconffile=[dbms/dbms.conf]] [frames=[0]]
//
// @key inputCMS "final" string
//               Monitor system to use for input.
//               One of { raw, final, intermediate }.
//
// @key dbconffile dbms/dbms.conf string
//      file from which to get directory info on where to put files and where
//      to create symlinks, with the file location interpreted by
//      Program::getConfFile()
//
// @key frames 0 int
//      number of frames before exiting, 0=>run forever
//
// @key framepriority useful string
//      archive priority of frame points to write, points with priorities at or
//      higher than this will be archived, valid values are vital, useful,
//      normal, debug, verbose
//
// @key minutepriority normal string
//      archive priority of minute points to write, points with priorities at or
//      higher than this will be archived, valid values are vital, useful,
//      normal, debug, verbose
//
// @key wbpriority normal string
//      archive priority of wb points to write, points with priorities at or
//      higher than this will be archived, valid values are vital, useful,
//      normal, debug, verbose
//
// @key slpriority normal string
//      archive priority of sl points to write, points with priorities at or
//      higher than this will be archived, valid values are vital, useful,
//      normal, debug, verbose
//
// @key dosdp true bool
//      Create sdp links
//
// @key dotransfer true bool
//      Create transfer links
//
// @key dotiming false bool
//        Log timing information once a minute.
//
// @key noframe false bool
//        Disable processing of frame data for performance reasons.
//
// @key nominute false bool
//        Disable processing of minute data for performance reasons.
//       
// @key frameoffset 10 int
//        Number of frames to offset frame mpdat writes from minute boundary.
// 
// @key stopfile /tmp/monitorAverageWriter.stop string
//      if this file is found while in the main loop, the program will finish
//      up what its doing and exit gracefully
//
// @logger MONITOR_FACILITY carma.dbms.monitorAverageWriter
//

int
util::Program::main( ) 
try {

    // Define CPU time stop watches for internal instrumentation:
    // i) total time; ii) time reading MP stream; iii) time accumulating
    // data; iv) time writing frame, minute, and astro integration data.
    const StopWatch::ClockType cpuClock = StopWatch::CPU_TIME; 
    StopWatch swTotal(cpuClock), swReadMP(cpuClock), 
              swAccum(cpuClock), swWriteFrame(cpuClock), 
              swWriteMin(cpuClock), swWriteAstro(cpuClock);  
    const StopWatch::ClockType wallClock = StopWatch::WALL_CLOCK; 
    StopWatch swWallTotal(wallClock), swWallReadMP(wallClock),
              swWallAccum(wallClock), swWallWriteFrame(wallClock), 
              swWallWriteMin(wallClock), swWallWriteAstro(wallClock);  

    swTotal.start( );
    swWallTotal.start( );
    {
        const string myArg0 = getArg0( );

        const string myBasename = basename( myArg0.c_str( ) );

        if ( IsRunning( myBasename )) {
            getLogger() << ::log4cpp::Priority::CRIT
                << myBasename << " is already running";
            return 0;
        }
    }

    // Pick up our command line parameters
    const string inputCMS = getStringParameter( "inputCMS" );
    const string confFile = getConfFile( getStringParameter("dbconffile") );
    const string framePriorityStr = getStringParameter( "framepriority" );
    const string minutePriorityStr = getStringParameter( "minutepriority" );
    const string slPriorityStr = getStringParameter( "slpriority" );
    const string wbPriorityStr = getStringParameter( "wbpriority" );
    const int maxFrames = getIntParameter( "frames" );
    const string stopFile = getStringParameter("stopfile");
    const bool dosdp = getBoolParameter( "dosdp" );
    const bool dotransfer = getBoolParameter( "dotransfer" );
    const bool dotiming = getBoolParameter("dotiming");
    const bool noframe = getBoolParameter( "noframe" );
    const bool nominute = getBoolParameter( "nominute" );
    const int frameoffset = getIntParameter( "frameoffset" );
    const bool useDBMS = getUseDBMS( );

    // Always use database and allow use of config file from cmd line.
    carma::dbms::TagIDAuthority::configureAuthority(useDBMS, confFile);

    const MP::ARCHIVE_PRIORITY framePriority  = str2priority(framePriorityStr);
    const MP::ARCHIVE_PRIORITY minutePriority = str2priority(minutePriorityStr);
    const MP::ARCHIVE_PRIORITY wbPriority     = str2priority(wbPriorityStr);
    const MP::ARCHIVE_PRIORITY slPriority     = str2priority(slPriorityStr);

    const monitor::CmsSelector inputCmsSelector = 
        monitor::convertStringToCmsSelector( inputCMS );

    string inCmsName;
    monitor::CmsAP cms = monitor::makeCms( inputCmsSelector, inCmsName );
    monitor::MonitorSystem & monitorSystem = *( cms.get( ) ); 

    // Dave, I'm resetting the queue here to ensure that you don't 
    // have a problem with two succesive frames in the queue having 
    // inconsistent monitor point definitions.
    monitorSystem.resetQueue();

    AverageAccumulator instAccumulator( monitorSystem, framePriority );
    AverageAccumulator minuteAccumulator( monitorSystem, minutePriority );
    AverageAccumulator wbAccumulator( monitorSystem, wbPriority );
    AverageAccumulator slAccumulator( monitorSystem, slPriority );

    // Create file & link manager instances: For transfer (arch), we want
    // all timescales except frame data, for sdp, we just want corr data,
    // all subject to change by the dotransfer and dosdp flags.
    auto_ptr< dbms::DBConfigurator > dbConf;
    try {
        dbConf = auto_ptr< dbms::DBConfigurator >( 
                new dbms::DBConfigurator( confFile ) );
    } catch ( const NotFoundException & exc ) {
        getLogger() << ::log4cpp::Priority::CRIT
            << "Unable to read configuration file " << confFile
            << " because " << exc.what();
        return EXIT_FAILURE;
    }

    DbFFIOFileManager frameDbFFIOFileManager( dbms::FRAME_AVG, *dbConf.get() );

    DbFileManager minuteDbFileManager( dbms::MINUTE_AVG,
                                       dotransfer,
                                       false,
                                       *dbConf.get() );

    DbFileManager wbDbFileManager( dbms::WBCORREL_AVG, 
                                   dotransfer,
                                   dosdp,
                                   *dbConf.get() );

    DbFileManager slDbFileManager( dbms::SLCORREL_AVG, 
                                   dotransfer,
                                   dosdp,
                                   *dbConf.get() );

    dbConf.reset( ); // Delete dbConf;

    // Read monitor point data; instrumented for CPU time usage.
    swReadMP.start();
    swWallReadMP.start();
    monitorSystem.read();
    double timeOfRead  = Time::MJD();
    swReadMP.stop();
    swWallReadMP.stop();


    const int appBeginFrameCount = monitorSystem.getFrameCount();

    long frameCount = appBeginFrameCount;
    long endFrameCount = frameCount + (40 - ( (frameCount + frameoffset) % 40));

    ostringstream endName;

    endName << frameCount << "-" << endFrameCount << ".mpdat";

    frameDbFFIOFileManager.OpenNewFiles( endName.str(), 
                                         monitorSystem.getFrameCount() );

    int oldFrameCount;
    bool done = false;
    int loopCount = 0;

    // Determine wideband integration number
    // TODO ADB: Check for validity prior to setting. 
    int wbIntegrationCount = monitorSystem.wbPipeline().
        integratorStageContainer().integratorStage().integrationNumber().
        getValue();

    int oldWbIntegrationCount = wbIntegrationCount;

    // Determine spectral line integration number
    // TODO ADB: Check for validity prior to setting. 
    int slIntegrationCount = monitorSystem.slPipeline().
        integratorStageContainer().integratorStage().integrationNumber().
        getValue();

    int oldSlIntegrationCount = slIntegrationCount;

    while ( !done )  {

        // Accumulate data; instrumented for CPU time usage.
        swAccum.start();
        swWallAccum.start();
        if ( !nominute ) minuteAccumulator.accumulate();
        if ( !noframe ) instAccumulator.accumulate();
        swAccum.stop();
        swWallAccum.stop();

        // Write out frame data; instrumented for CPU time usage.
        swWriteFrame.start();
        swWallWriteFrame.start();
        if ( !noframe ) 
            frameDbFFIOFileManager.writeInstAveragesToFile( instAccumulator,
                                                            frameCount );
        swWriteFrame.stop();
        swWallWriteFrame.stop();

        //////////////////////////////////////////////////
        // Frame Averages
        //////////////////////////////////////////////////

        if ( ( ( frameCount + frameoffset ) % 40) == 0 && !noframe ) { 
            // finish up with the current frame files and open new ones
            frameDbFFIOFileManager.DoneWithFiles( kDoDbload );

            endName.str("");
            endName << (frameCount+1) << "-" << (frameCount+40) << ".mpdat";

            frameDbFFIOFileManager.OpenNewFiles( 
                endName.str(), 
                monitorSystem.getFrameCount() );

        } // End if ( frameCount % 40 ) == 0

        if ( !noframe ) instAccumulator.resetAccumulator();

        //////////////////////////////////////////////////
        // Minute Averages
        //////////////////////////////////////////////////

        if ( (frameCount % 120) == 0 && !nominute ) {

            endName.str("");
            endName << frameCount << ".mpdat";

            // Open and write minute-averaged files; instrumented for
            // CPU time usage
            swWriteMin.start();
            swWallWriteMin.start();
            minuteDbFileManager.OpenNewFiles( endName.str(), 
                                              monitorSystem.getFrameCount() );

            minuteDbFileManager.writeLongAveragesToFile( minuteAccumulator,
                                                         frameCount );
            swWriteMin.stop();
            swWallWriteMin.stop();

            resetStopWatchesAndLogIfSpecified( swTotal, swWallTotal,
                                               swReadMP, swWallReadMP,
                                               swAccum, swWallAccum,
                                               swWriteFrame, swWallWriteFrame,
                                               swWriteMin, swWallWriteMin,
                                               swWriteAstro, swWallWriteAstro,
                                               dotiming );

            minuteAccumulator.resetAccumulator(); // reset for next go

            minuteDbFileManager.DoneWithFiles( kDoDbload );

        } // End if ( (frameCount % 120) == 0 ) (minute data)

        //////////////////////////////////////////////////
        // Wideband Integration Averages
        //////////////////////////////////////////////////

        // Check wideband integration number
        int wbIC = monitorSystem.wbPipeline().integratorStageContainer().
            integratorStage().integrationNumber().getValue();
        bool scienceData = monitorSystem.wbPipeline().visBrickStageContainer().
            visBrickStage().scienceData().getValue();    

        bool wbicIsValid = monitorSystem.wbPipeline().integratorStageContainer()
                 .integratorStage().integrationNumber().getValidity() >=
                 MonitorPoint::VALID;
        bool scienceDataIsValid = monitorSystem.wbPipeline().
                visBrickStageContainer().visBrickStage().
                scienceData().getValidity() >=
                 MonitorPoint::VALID;
                 
        if ( ( wbIC >= 0 ) && wbicIsValid && scienceDataIsValid) {
            wbIntegrationCount = wbIC;
        }

        if (wbIntegrationCount != oldWbIntegrationCount ) {
            if ( (oldWbIntegrationCount > 0) ) {

                endName.str("");
                endName << oldWbIntegrationCount << ".mpdat";

                // Open and write wideband averaged files; instrumented
                // for CPU time usage
                swWriteAstro.start();
                swWallWriteAstro.start();

                if (scienceData) {
                    wbDbFileManager.OpenNewFiles( endName.str(), 
                                              wbIntegrationCount );
                    wbDbFileManager.writeLongAveragesToFile( wbAccumulator, 
                        oldWbIntegrationCount);
                }

                swWriteAstro.stop();
                swWallWriteAstro.stop();

                // reset for next accumulation
                wbAccumulator.resetAccumulator();
                if (scienceData) {
                    wbDbFileManager.DoneWithFiles( kDoDbload );
                }
            }
            oldWbIntegrationCount = wbIntegrationCount;
        }

        // Accumulate, but only AFTER cleaning up outstanding integrations
        if ( wbIntegrationCount > 0 ) {
            swAccum.start();
            swWallAccum.start();
            wbAccumulator.accumulate();
            swAccum.stop();
            swWallAccum.stop();
        }

        //////////////////////////////////////////////////
        // Spectral Line Integration Averages
        //////////////////////////////////////////////////

        // Check spectral line integration number
        int slIC = monitorSystem.slPipeline().integratorStageContainer().
            integratorStage().integrationNumber().getValue();
        scienceData = monitorSystem.slPipeline().visBrickStageContainer().
            visBrickStage().scienceData().getValue();    

        bool slicIsValid = monitorSystem.slPipeline().integratorStageContainer()
                 .integratorStage().integrationNumber().getValidity() >=
                        MonitorPoint::VALID;
        scienceDataIsValid = monitorSystem.slPipeline().
                visBrickStageContainer().visBrickStage().
                scienceData().getValidity() >=
                 MonitorPoint::VALID;
            
            
        if ( ( slIC >= 0 ) && slicIsValid && scienceDataIsValid) {
            slIntegrationCount = slIC;
        }

        if (slIntegrationCount != oldSlIntegrationCount ) {
            if ((oldSlIntegrationCount > 0)) {

                // The prior integration has completed - open new files for the
                // data, write the accumulated averages to these files and then 
                // reset the accumulator.
                endName.str("");
                endName << oldSlIntegrationCount << ".mpdat";

                // Open and write spectral line averaged files; instrumented
                // for CPU time usage.
                swWriteAstro.start();
                swWallWriteAstro.start();

                if (scienceData) {
                    slDbFileManager.OpenNewFiles(endName.str(), 
                                                 oldSlIntegrationCount);

                    slDbFileManager.writeLongAveragesToFile(slAccumulator,
                                                         oldSlIntegrationCount);
                }
                swWriteAstro.stop();
                swWallWriteAstro.stop();

                // reset for next accumulation
                slAccumulator.resetAccumulator();
                if (scienceData) {
                    slDbFileManager.DoneWithFiles( kDoDbload );
                }
            }

            oldSlIntegrationCount = slIntegrationCount;
        }

        // Accumulate, but only AFTER cleaning up outstanding integrations
        if ( slIntegrationCount > 0 ) {
            swAccum.start();
            swWallAccum.start();
            slAccumulator.accumulate();
            swAccum.stop();
            swWallAccum.stop();
        }

        //////////////////////////////////////////////

        oldFrameCount = frameCount;
        double previousTimeOfRead  = timeOfRead;

        // Read monitor point data; instrumented for CPU time usage.
        swReadMP.start();
        swWallReadMP.start();
        int ipqSkips = monitorSystem.read( );
        timeOfRead = Time::MJD();
        swReadMP.stop();
        swWallReadMP.stop();

        frameCount = monitorSystem.getFrameCount();

        while ( frameCount == oldFrameCount ) {
            ostringstream oss;
            oss << "frameCount and previous frameCount are the same "
                << frameCount;
            CARMA_CPTRACE( util::Trace::TRACE1, oss.str());
            programLogErrorIfPossible( oss.str( ) );

            // Read monitor point data; instrumented for CPU time usage.
            swReadMP.start();
            swWallReadMP.start();
            previousTimeOfRead = timeOfRead;
            ipqSkips = monitorSystem.read( );
            timeOfRead = Time::MJD();
            swReadMP.stop();
            swWallReadMP.stop();

            frameCount = monitorSystem.getFrameCount();
        }

        const unsigned int skipped = frameCount - oldFrameCount - 1; 
        if ( (loopCount > 1) && (skipped > 0)) {
            ostringstream oss;
            oss.setf(ios::fixed);
            oss << "frameCount skip detected, previous frameCount "
                << oldFrameCount << " current frameCount " << frameCount
                << ", " << setw(2) << skipped << " frame";
            if (skipped > 1) oss << "s";
            oss << " skipped.  " 
                << "IPQskip=" << ipqSkips 
                << " clockTimeDiffBtwReads=" << setw(6) << setprecision(3)
                << (Time::SECONDS_PER_DAY*(timeOfRead-previousTimeOfRead)) 
                << "secs";
            CARMA_CPTRACE( util::Trace::TRACE1, oss.str());
            programLogErrorIfPossible( oss.str( ) );

            resetStopWatchesAndLogIfSpecified( swTotal, swWallTotal,
                                               swReadMP, swWallReadMP,
                                               swAccum, swWallAccum,
                                               swWriteFrame, swWallWriteFrame,
                                               swWriteMin, swWallWriteMin,
                                               swWriteAstro, swWallWriteAstro,
                                               true );
        }

        if ( carma::util::FileUtils::exists( stopFile ) ) {
            const string msg( stopFile + " found, exiting gracefully." );
            programLogInfo( msg );
            done = true;
        }

        loopCount++;
        done = ( done || ((maxFrames > 0)
                    && ((frameCount - appBeginFrameCount) > maxFrames)));

    } // End while ( !done ) 

    frameDbFFIOFileManager.DoneWithFiles( kDoDbload ); //finish up with currently files 

    return EXIT_SUCCESS;

} catch ( ... ) {
    logCaught( log4cpp::Priority::CRIT );
    return EXIT_FAILURE;
}
