/*
 * This is the master server program for the java display servers.
 *
 * @author  Steve Scott
 * $Id: MasterServer.cc,v 1.30 2013/11/19 00:07:37 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <fstream>
#include <sstream>
#include <vector>

#include <carma/ui/rtd/common/MasterServer.h>
#include <carma/ui/rtd/common/SenderAddress.h>
#include <carma/ui/rtd/common/Connections.h>
#include <carma/ui/rtd/common/ReaderWithTimeout.h>
#include <carma/ui/rtd/common/WindowList.h>
#include <carma/ui/rtd/common/Version.h>
#include <carma/ui/rtd/auth/Authenticate.h>

#include <carma/util/Trace.h>
#include <carma/util/ErrorException.h>
#include <carma/util/Program.h>
#include <carma/util/programLogging.h>

#include <boost/shared_ptr.hpp>

using namespace ::std;
using namespace carma::util;
using namespace carma::ui::rtd;

namespace {

const size_t kSpawnedArgMaxLen = 128;
const size_t kSpawnedArgsMaxCount = 11;

char gSpawnedArgStorage[ kSpawnedArgsMaxCount ][ kSpawnedArgMaxLen + 1 ];

char * gSpawnedArgv[ kSpawnedArgsMaxCount + 1 ];

void
initSpawnedArgv( )
{
    for ( size_t i = 0; i <= kSpawnedArgsMaxCount; ++i )
        gSpawnedArgv[ i ] = 0;
}

void
setSpawnedArg( const size_t   argIndex,
               const string & argValue )
{
    if ( (argIndex >= 0) && (argIndex < kSpawnedArgsMaxCount) ) {
        strncpy( gSpawnedArgStorage[ argIndex ],
                 argValue.c_str(),
                 kSpawnedArgMaxLen );

        gSpawnedArgStorage[ argIndex ][ kSpawnedArgMaxLen ] = '\0';

        gSpawnedArgv[ argIndex ] = gSpawnedArgStorage[ argIndex ];
    }
}

}  // namespace < anonymous >

MasterServer::MasterServer( )
{
}

int
MasterServer::run(
    const string &     pgpDir,
    const string &     inRootPathServerPath,
    const string &     inServerPathOverride,
    const string &     programName,
    const string &     mmapPath,
    const string &     connectionFilename,
    const int          traceLevel,
    const bool         useDbms,
    const int          niceLevel,
    const WindowList&  windowList,
    const bool         checkDomain )
{
    const string rootPathServerPath = inRootPathServerPath;
    const string serverPathOverride = inServerPathOverride;
    const std::string connectionFullFilename = mmapPath + "/" + connectionFilename;

    // Get the client's internet address
    SenderAddress *sa = new SenderAddress(STDIN_FILENO);

    /// Full host name, including domain
    const std::string senderHostName = sa->getHostName();
    const std::string senderDomainName = sa->getDomainName();

    /// A host name without any domain information
    std::string hostNameOnly;
    {
        size_t found = senderHostName.find_first_of(".");
        if (found != std::string::npos) {
            hostNameOnly = senderHostName.substr(0, found);
        }
    }

    CARMA_CPTRACE(Trace::TRACE7, "ServerHostName: " << sa->getServerHostName());
    CARMA_CPTRACE(Trace::TRACE7, "ServerDomainName: " << sa->getServerDomainName());
    CARMA_CPTRACE(Trace::TRACE7, "serverProgramName: " << programName);

    // Open a log and send an entry for starting up
    {
        std::ostringstream oss;
        oss << "Starting rtd " << programName << ", client=" << senderHostName;
        programLogInfoIfPossible(oss.str());
    }

    const std::string s = ((serverPathOverride.empty()) ?
        ( Program::getConfFile("rtd/version.tab") ) :
        ( serverPathOverride + "conf/rtd/version.tab" ));
    Version version( Program::getConfFile("rtd/version.tab") );

    // Get user short/full name and client version
    Authenticate auth;
    const enum AuthenticationStatus authResult = auth.authenticate(version, windowList);
    if (authResult == AUTH_EXIT) {
        CARMA_CPTRACE(Trace::TRACE7, "client requested exit during auth");
        return EXIT_SUCCESS;
    }

    if (authResult == AUTH_FAILURE) {
        CARMA_CPTRACE(Trace::TRACE7, "client authentication failed");
        return EXIT_SUCCESS;
    }

    // double check to catch programming mistakes
    if (authResult != AUTH_SUCCESS) {
        std::ostringstream oss;
        oss << "received an unknown return code from auth->authenticate(): " << authResult;
        programLogErrorIfPossible(oss.str());
        return EXIT_FAILURE;
    }

    // authentication succeeded
    const Window *window = auth.getWindow();

    // the requested window name is valid, start it
    const std::string serviceProgramName = window->getProgramName();
    const std::string windowName = window->getWindowName();
    CARMA_CPTRACE(Trace::TRACE6, "Find worked..." << serviceProgramName << " " << windowName);

    // Now try to run the service program with exec()
    // Start with directory location

    // Full pathname for the server program
    const std::string fullPath =
        ((serverPathOverride.empty()) ?
            (rootPathServerPath + serviceProgramName) :
            (serverPathOverride + serviceProgramName));

    const std::string spawnedArg0 =
        ((serverPathOverride.empty()) ? serviceProgramName : fullPath);

    std::ostringstream traceLevelString;
    traceLevelString << "traceLevel=" << traceLevel;

    std::ostringstream useDbmsString;
    useDbmsString << "useDBMS=" << (useDbms ? "true" : "false");

    std::ostringstream niceLevelString;
    niceLevelString << "nice=" << niceLevel;

    std::ostringstream fullNameString;
    fullNameString << "fullname=\"" << auth.getFullName() << "\"";

    initSpawnedArgv();

    setSpawnedArg( 0,  spawnedArg0 );
    setSpawnedArg( 1,  traceLevelString.str() );
    setSpawnedArg( 2,  "traceFile=syslog" );
    setSpawnedArg( 3,  useDbmsString.str() );
    setSpawnedArg( 4,  niceLevelString.str() );
    setSpawnedArg( 5,  fullNameString.str() );
    setSpawnedArg( 6,  ("string1=" + window->getString1()) );
    setSpawnedArg( 7,  ("string2=" + window->getString2()) );
    setSpawnedArg( 8,  ("string3=" + window->getString3()) );
    setSpawnedArg( 9,  ("string4=" + window->getString4()) );
    setSpawnedArg( 10, ("integer1=" + window->getInteger1()) );

    if (false) {
        std::cerr << "dbg:" << fullPath;
        std::cerr << " " << gSpawnedArgv[0]
             << " " << gSpawnedArgv[1]
             << " " << gSpawnedArgv[2] << "\n"
             << " " << gSpawnedArgv[3] << " " << gSpawnedArgv[4] << "\n"
             << " " << gSpawnedArgv[5] << " " << gSpawnedArgv[6] << "\n"
             << " " << gSpawnedArgv[7] << " " << gSpawnedArgv[8] << "\n"
             << " " << gSpawnedArgv[9] << " " << gSpawnedArgv[10]
             << std::endl;
    }

    // Now fork and exec the process that will feed the Java display
    int pid = fork();

    if (pid == -1) {
        // this is an error
        programLogCriticalIfPossible("Fork failed, exiting.");
        return EXIT_FAILURE;
    } else if (pid == 0) {  // child
        // And away we go...
        execv( fullPath.c_str(), gSpawnedArgv );
        // If we get here the exec failed
        {
            std::ostringstream oss;
            oss << "exec(" << fullPath << ") failed";
            programLogCriticalIfPossible(oss.str());

            std::cout << oss.str() << std::endl;
            return EXIT_FAILURE;
        }
    } else {  // parent
        {
            std::ostringstream oss;
            oss << "registering \"" << windowName << "\" with pid=" << pid;
            programLogDebugIfPossible(oss.str());
        }

        // Register the connected display window
        boost::shared_ptr<ConnectionRW> con(new ConnectionRW(pid, connectionFullFilename));
        con->registerWindow(windowName.c_str(),
                            auth.getShortName().c_str(),
                            auth.getFullName().c_str(),
                            hostNameOnly.c_str(),
                            senderHostName.c_str(),
                            false );

        // go to sleep and wait for the service to call exit(2)
        int status;
        wait(&status);
        if (!WIFEXITED(status)) {
            // The reason that this program hangs around rather than
            // exiting immediately is to catch crashes of the server program.
            std::ostringstream oss;
            oss << "Service failed(crash?): " << serviceProgramName << "(" << pid << ")";
            programLogErrorIfPossible(oss.str());
        } else {
            // the service exited cleanly with success,
            // so there is nothing to log
        }

        // Unregister the window
        con->unregisterWindow(pid);

        // And log the exit of the server
        programLogNoticeIfPossible("Done, exiting.");
        return EXIT_SUCCESS;
    }

    // Should never get here
    programLogCriticalIfPossible("Should never get here, exiting.");
    return EXIT_FAILURE;
}
