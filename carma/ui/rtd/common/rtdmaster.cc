/**
 * @file
 *
 * The master cma internet service that sends socket data to the
 * Java client for display data. This main routine is used to customize
 * a MasterServer object, which then does all of the work. See the source
 * for MasterServer.h for more details on how it works.
 *
 * This program should be started by xinetd as an internet service. To set
 * this up, the file /etc/services must be edited and a service file
 * added to /etc/xinetd.d.
 *
 *
 * For testing, it is sometimes convenient to interact with mjsrv via telnet.
 * When this program starts up, it asks for authentication. Note that from
 * telnet, you can only do a guest login, as a real login comes in an
 * encrypted packet. The login dialog must be done even if authentication
 * has been disabled in the makefile (by not including -D AUTH).
 * If authentication has been disabled, the results of the login dialog
 * are ignored and everyone has privileged access.
 * Telnet is a good debugging tool for services. An example of telnet
 * program startup:
 *
 * tioga>telnet labacc 3100
 * Trying 192.100.16.7...
 * Connected to labacc.
 * Escape character is '^]'.
 * GUEST                     !you type this to the authorization routine
 * x                         !you type this id (any random string)
 * Onsite. User unknown. No restrictions apply.
 * demo                      !you specify the codename of the window server pgm
 * 1.7 1.7.1 Display         !The numbers are server and client version numbers,
 *                           !and will change with time.
 *
 *
 * @author Steve Scott
 * $Id: rtdmaster.cc,v 1.24 2013/11/19 00:07:37 iws Exp $
 *
 * $CarmaCopyright$
 *
 *
 */

//
// @key serverPath @noDefault string
//      Override path to use for spawning subprograms. If not given then the
//      default is to use < root >/bin/
//
// @logger DEFAULT_FACILITY carma.rtd.master
//

#include <iostream>
#include <string>

#include <carma/util/Program.h>
#include <carma/util/programLogging.h>
#include <carma/util/RuntimeDirs.h>
#include <carma/util/Trace.h>

#include <carma/ui/rtd/common/MasterServer.h>
#include <carma/ui/rtd/common/CarmaDisplay.h>

using namespace ::std;
using namespace carma;
using namespace carma::util;
using namespace carma::ui;
using namespace carma::ui::rtd;


namespace {


bool
trimFromEnd( string &       source,
             const string & suffix )
{
    const string::size_type sourceSize = source.size();
    const string::size_type suffixSize = suffix.size();

    if ( sourceSize < suffixSize )
        return false;

    const string::size_type suffixStartPos = sourceSize - suffixSize;

    if ( source.substr( suffixStartPos ) != suffix )
        return false;

    source.erase( suffixStartPos );

    return true;
}


}  // namespace < anonymous >


// Command line arguments:
//  1) program name (rtdmaster)
int
Program::main()
{
    CARMA_CPTRACE(Trace::TRACE7, "Starting rtdmaster");

    const string programName = getArg0();

    const string rootPathServerPath = getRootDir() + "bin/";

    bool rootPathOverridden = false;
    string serverPathOverride;
    string pgpDir;
    {
        if ( parameterWasSpecified( "serverPath" ) ) {
            string rootPathOverride = getStringParameter( "serverPath" );

            if ( rootPathOverride.empty() == false ) {
                if ( trimFromEnd( rootPathOverride, "/bin/" ) )
                    rootPathOverride.push_back( '/' );
                else if ( trimFromEnd( rootPathOverride, "/bin" ) )
                    rootPathOverride.push_back( '/' );
                else {
                    trimFromEnd( rootPathOverride, "/" );

                    rootPathOverride.push_back( '/' );
                }

                rootPathOverridden = true;
                serverPathOverride = rootPathOverride + "bin/";
                pgpDir = rootPathOverride + "conf/pgp/";
            }
        }

        if ( rootPathOverridden == false )
            pgpDir = getConfFile( "pgp/" );
    }

    const int traceLevel = getTraceLevel();
    const bool useDbms = getUseDBMS();
    const int niceLevel = getNiceLevel();

    CarmaWindows carmaWindows;

    carmaWindows.load();


    {
        ostringstream oss;

        oss << "Creating MasterServer with ";

        if ( rootPathOverridden == false ) {
            oss << "server path=" << rootPathServerPath
                << ", PGP path=" << pgpDir;
        } else {
            oss << "server path override=" << serverPathOverride
                << ", PGP path override=" << pgpDir;
        }

        oss << ", trace level=" << traceLevel
            << ", use dbms=" << (useDbms ? "true" : "false")
            << ", nice level=" << niceLevel;

        programLogInfoIfPossible( oss.str() );
    }

    MasterServer masterServer;

    return masterServer.run( pgpDir,
                             rootPathServerPath,
                             serverPathOverride,
                             programName,
                             "/tmp",
                             "connections.shmem",
                             traceLevel,
                             useDbms,
                             niceLevel,
                             carmaWindows,
                             false );
}
