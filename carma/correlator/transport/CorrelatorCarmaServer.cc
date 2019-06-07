
#include "carma/corba/corba.h"
#include "carma/corba/Server.h"
#include "carma/correlator/lib/CorrelatorConfigChecker.h"
#include "carma/correlator/obsRecord2/aceUtils.h"
#include "carma/correlator/transport/CorrDataUpdater.h"
#include "carma/correlator/transport/CorrMonUpdater.h"
#include "carma/util/AutoPthreadQuitAndJoinGroup.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/posixErrors.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadCond.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/StartPthread.h"
#include "carma/util/ThreadQuit.h"

#include <cobra/CobraVersion.h>

using namespace ::std;
using namespace carma;
using namespace carma::correlator;
using namespace carma::correlator::lib;
using namespace carma::correlator::obsRecord2;
using namespace carma::correlator::transport;
using namespace carma::util;


namespace {


::pthread_mutex_t gMutex = PTHREAD_MUTEX_INITIALIZER;

bool              gProducerShutdown = false;
::pthread_cond_t  gProducerShutdownCond = PTHREAD_COND_INITIALIZER;

bool              gMonAndDataThreadsDone = false;
::pthread_cond_t  gMonAndDataThreadsDoneCond = PTHREAD_COND_INITIALIZER;


typedef ScopedLock< ::pthread_mutex_t > ScopedGMutexLock;


void
producerShutdownCallback( void * )
try {
    {
        const ScopedGMutexLock lock( gMutex );

        gProducerShutdown = true;
    }

    broadcastCond( gProducerShutdownCond );

    struct ::timespec absTimeout;
    {
        struct ::timeval now;

        ::gettimeofday( &now, 0 );

        absTimeout.tv_sec = now.tv_sec + 2;
        absTimeout.tv_nsec = 1000 * now.tv_usec;
    }

    programLogInfoIfPossible( "Producer shutdown broadcast" );

    {
        const ScopedGMutexLock lock( gMutex );

        while ( gMonAndDataThreadsDone == false )
            timedWaitCond( gMonAndDataThreadsDoneCond, gMutex, absTimeout );
    }
} catch ( ... ) {
    programLogErrorIfPossible(
        "Stifling exception in producerShutdownCallback - " +
        getStringForCaught() );

    return;
}


struct MonUpdateThreadArgs {
    int                            bandNo;
    lib::CorrelatorConfigChecker * ccc;
    unsigned short                 controlPort;
    string                         controlHost;
    int                            portOffset;
    string                         hwType;
};


void
monUpdateThreadEntrypoint( const MonUpdateThreadArgs & args )
try {

    ScopedLogNdc ndc("monUpdateThreadEntrypoint");
    CARMA_CPTRACE(Trace::TRACE6, "ENTRY");

    {
        ostringstream oss;

        oss << "Starting monitor update loop using "
            << args.controlHost << ":" << args.controlPort << "...";

        CARMA_CPTRACE(Trace::TRACE6, oss.str() );
    }

    CARMA_CPTRACE(Trace::TRACE6, "instantiate corrMonUpdater");
    CorrMonUpdater corrMonUpdater;

    CARMA_CPTRACE(Trace::TRACE6, "runUpdateLoop");
    corrMonUpdater.runUpdateLoop( args.bandNo,
                                 *(args.ccc),
                                  args.controlPort,
                                  args.controlHost,
                                  args.portOffset,
                                  args.hwType );
} catch ( ... ) {
    if ( CaughtExceptionIsThreadQuitRequestedError() )
        programLogInfoIfPossible( "Thread quitting as requested" );
    else {
        programLogCriticalIfPossible(
            "Thread dying (and exiting the process) due to an exception - " +
            getStringForCaught() );

        // Draconian I know but I don't want the server to keep running
        // without a monitor thread.
        // Better to just die a screaming death I think.
        // I will fix this better later.
        ::exit( EXIT_FAILURE );
    }

    throw;
}


struct DataUpdateThreadArgs {
    string         doName;
    string         ecName;
    unsigned short controlPort;
    string         controlHost;
    int            portOffset;
    int            bandNo;
    string         hwType;
    bool           spectralLineMode;
    int            maxCorrDataAgeFrames;
};


void
dataUpdateThreadEntrypoint( const DataUpdateThreadArgs & args )
try {
    CorrDataUpdater corrDataUpdater;

    {
        ostringstream oss;

        oss << "Starting data update loop using "
            << args.controlHost << ":" << args.controlPort << "...";

        programLogInfoIfPossible( oss.str() );
    }

    corrDataUpdater.runUpdateLoop( args.doName,
                                   args.ecName,
                                   args.controlPort,
                                   args.controlHost,
                                   args.portOffset,
                                   args.bandNo,
                                   producerShutdownCallback,
                                   0,
                                   args.hwType,
                                   args.spectralLineMode,
                                   args.maxCorrDataAgeFrames );

    programLogInfoIfPossible( "Data update loop returned" );
} catch ( ... ) {
    if ( CaughtExceptionIsThreadQuitRequestedError() )
        programLogInfoIfPossible( "Thread quitting as requested" );
    else {
        programLogCriticalIfPossible(
            "Thread dying (and exiting the process) due to an exception - " +
            getStringForCaught() );

        // Draconian I know but I don't want the server to keep running
        // without a monitor thread.
        // Better to just die a screaming death I think.
        // I will fix this better later.
        ::exit( EXIT_FAILURE );
    }

    throw;
}


}  // namespace < anonymous >


//
//  @description
//      Creates and registers one remote CORBA object for
//      controlling a correlator band, and one CORBA event
//      channel containing data read from the correlator
//      and republished as a CorrelatorBand object.
//      It runs the data and monitor update loops which
//      continually read correlation and monitor data
//      from ACE Messages published by the correlator software
//
//  @usage Use it to publish a correlator band to the carma world
//
//  @key b @mandatory int
//       Band Number ( zero for CARMA3G )
//
//  @key f correlator/correlator.conf string
//       Correlator config filename
//       (install or build conf dir path will be prepended)
//
//  @key r localhost string
//       Remote control server name
//
//  @key p @mandatory int
//       Control server port number
//
//  @key d 0 int
//       Data server port offset (zero queries server for port)
//
//  @key m 0 int
//       Monitor server port offset (zero queries server for port)
//
//  @key redirectAceLogging false bool
//       Whether or not to redirect ACE logging
//
// @key duration @noDefault int
//      If given then it is the number of seconds to run for before quitting.
//      Otherwise run forever.
//      Only usefeul for debugging, profiling, and coverage.
//      Value gets pinned to a minimum of 10 seconds.
//
// @key hwtype cobra string
//      Indicates the band hardware type for this band, either cobra, carma,c3gmax8, or c3gmax23
//
// @key maxCorrDataAgeFrames 3 int
//      The max age of data in frames after the sample frame, that we will still
//      send data to the notification server.  If data arrives from the
//      CorrelatorBandServer any later than this, it is discarded.
//
//  @logger DEFAULT_FACILITY carma.correlator.carmaserver
//
//  @author Tom Costa
//  @version $Revision: 1.52 $
//
int
Program::main( )
try {
    struct ::timeval startTime;
    ::gettimeofday( &startTime, 0 );

    ScopedLogNdc ndc("main thread");

    const int bandNo = getIntParameter( "b" );
    const int dataPortOffset = getIntParameter( "d" );
    const int monitorPortOffset = getIntParameter( "m" );
    const int maxCorrDataAgeFrames = getIntParameter( "maxCorrDataAgeFrames" );
    const string prefix("carma.correlator");

    ostringstream dataObjectName;    // event channel for data
    ostringstream controlObjectName; // DefaultCorrControl object
    string logpostfix;
    string hwStr = getStringParameter("hwtype");
    bool spectralLineMode; // keep this for now, though it is redundant
    ostringstream oss;
    // i bet all this could be done with a clever std::map.
    if ( hwStr.compare("cobra") == 0 ) {
        logpostfix = ".wbcBand";
        dataObjectName << prefix << ".wbcData" ;
        controlObjectName << prefix << ".wbcControl" << bandNo-8;
        oss << getLogname() << logpostfix << bandNo;
        setInstanceLogname( oss.str() );
        spectralLineMode=false;
    } else {
        if ( hwStr.compare("carma") == 0 ) {
            logpostfix = ".slcBand";
            dataObjectName << prefix << ".slcData" ;
            controlObjectName << prefix << ".slcControl" << bandNo;
            oss << getLogname() << logpostfix << bandNo;
            setInstanceLogname( oss.str() );
            spectralLineMode=true;
        } else {
            if ( hwStr.compare("c3gmax8") == 0 ||
                 hwStr.compare("c3gmax23") == 0 )
            {
                if ( (COBRA_COMPILETIME_VERSION_MINOR < 37) && (COBRA_COMPILETIME_VERSION_PATCH < 7) ) {
                   oss << "For CARMA3G, you must have cobra library version 2.37.7 or above."
                       << " Your version is 2." << COBRA_COMPILETIME_VERSION_MINOR
                       << "." << COBRA_COMPILETIME_VERSION_PATCH;
                   throw CARMA_EXCEPTION( util::ErrorException, oss.str() );
                }
                if (bandNo != 0 ) {
                   oss << "For CARMA3G hardware bandNo parameter must be zero";
                   throw CARMA_EXCEPTION( util::IllegalArgumentException, oss.str() );
                }
                logpostfix = hwStr;
                dataObjectName    << prefix << "." << hwStr << ".Data";
                controlObjectName << prefix << "." << hwStr << ".Control";
                oss << getLogname() << logpostfix;
                setInstanceLogname( oss.str() );
                spectralLineMode=true;
            } else {
                 ostringstream errss;
                 errss << "Unrecognized hwType : "
                       << hwStr
                       << ".  Valid values are cobra, carma, c3gmax8, c3gmax23";
                 throw CARMA_EXCEPTION( util::IllegalArgumentException, errss.str() );
            }
        }
    }

    string controlObject = controlObjectName.str();
    string eventChannelBase  = dataObjectName.str(); 
    /*
    // debug -- monitor system shows this now.
    {
        ostringstream os;
        os << "For Band " << bandNo << ", control object is " << controlObject
           << " , event channel basename is " << eventChannelBase
            ;
        programLogInfoIfPossible( os.str() );
    }
    */

    if ( getBoolParameter( "redirectAceLogging" ) )
        installAceLoggingBackend();

    const bool haveDuration = parameterWasSpecified( "duration" );
    const int duration =
        (haveDuration ?
            (::std::max( 10, getIntParameter( "duration" ) )) :
            0);

    const unsigned short controlPort = getIntParameter( "p" );
    const string controlHost         = getStringParameter( "r" );


    // filename path must start in the conf directory. getConfFile will
    // prepend the correct path for the build directories and install
    // directories
    CorrelatorConfigChecker * const ccc =
        CorrelatorConfigChecker::getInstance(
            getConfFile( getStringParameter("f") ) );

    ccc->start();


    {
        AutoPthreadQuitAndJoinGroup autoQuitJoinData;
        AutoPthreadQuitAndJoinGroup autoQuitJoinMon;

        {
            programLogInfoIfPossible( "Starting mon update thread..." );

            MonUpdateThreadArgs args;

            args.bandNo = bandNo;
            args.ccc = ccc;
            args.controlPort = controlPort;
            args.controlHost = controlHost;
            args.portOffset  = monitorPortOffset;
            args.hwType  = hwStr;

            const pthread_t monUpdateThread =
                StartPthreadWithCopy( monUpdateThreadEntrypoint,
                                      args,
                                      "Mon Update Thread" );

            autoQuitJoinMon.insert( monUpdateThread );

            programLogInfoIfPossible( "Mon update thread started" );
        }

        {
            programLogInfoIfPossible( "Starting data update thread..." );

            DataUpdateThreadArgs args;

            args.doName = controlObject;
            args.ecName = eventChannelBase;
            args.controlPort = controlPort;
            args.controlHost = controlHost;
            args.portOffset  = dataPortOffset;
            args.bandNo = bandNo;
            args.hwType  = hwStr;
            args.spectralLineMode = spectralLineMode;
            args.maxCorrDataAgeFrames = maxCorrDataAgeFrames;

            const pthread_t dataUpdateThread =
                StartPthreadWithCopy( dataUpdateThreadEntrypoint,
                                      args,
                                      "Data Update Thread" );

            autoQuitJoinData.insert( dataUpdateThread );

            programLogInfoIfPossible( "Data update thread started" );
       }

        if ( haveDuration ) {
            programLogInfoIfPossible(
                "Waiting until shutdown time has arrived..." );

            struct ::timespec quitTime;

            quitTime.tv_sec = startTime.tv_sec + duration;
            quitTime.tv_nsec = 1000 * startTime.tv_usec;

            bool quitTimeArrived = false;
            {
                const ScopedGMutexLock lock( gMutex );

                while ( (quitTimeArrived == false) &&
                        (gProducerShutdown == false) ) {
                    quitTimeArrived =
                        (timedWaitCond( gProducerShutdownCond,
                                        gMutex,
                                        quitTime ) == false);
                }
            }

            if ( quitTimeArrived || imrTerminationRequested() ) {
                programLogInfoIfPossible( "Shutting down the orb" );

                getCorbaServer().stop();
            }
        }

        {
            programLogInfoIfPossible( "Waiting for producer shutdown..." );

            const ScopedGMutexLock lock( gMutex );

            while ( gProducerShutdown == false )
                waitCond( gProducerShutdownCond, gMutex );
        }

        autoQuitJoinMon.requestQuitsNoThrow();
        autoQuitJoinData.requestQuitsNoThrow();
    }

    {
        const ScopedGMutexLock lock( gMutex );

        gMonAndDataThreadsDone = true;
    }

    broadcastCond( gMonAndDataThreadsDoneCond );

    programLogInfoIfPossible(
        "Mon and data threads are done and that fact has been broadcast" );

    sleep(1);

    return EXIT_SUCCESS;
} catch ( ... ) {
    programLogCriticalIfPossible( "Exiting on an exception - " +
                                getStringForCaught() );

    throw;
}
