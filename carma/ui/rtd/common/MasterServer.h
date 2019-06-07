#ifndef CARMA_UI_RTD_MASTERSERVER_H
#define CARMA_UI_RTD_MASTERSERVER_H

/*
 * @file
 *
 * Master process for the realtime display data servers.
 *
 * @author Steve Scott
 * $id: $
 *
 * $CarmaCopyright$
 *
 */

#include <sys/wait.h>
#include <string>

namespace carma {
namespace ui {
namespace rtd {

// forward declarations
class WindowList;

/**
 *
 * This object is the master data server for java monitor and control programs
 * for the Carma. The parameters for the constructor provide
 * for customization of the system. The contructor does all of the work,
 * concluding by launching a separate server program to actually send
 * data to the java display client.
 *
 * The program that invokes this object is usually executed by the Unix inetd
 * daemon when a tcp connect request comes in on the port specified in
 * /etc/inet/inetd.conf & /etc/services.
 * There are two ports used, one for running production code and one for
 * development. The difference in production and development is simply the
 * location of the server programs that are run by this program. This program
 * will run the development version if its first argument (normally program name)
 * starts with a 'd'. The program name that is passed in is part of the port
 * specific setup of inetd.conf. The directory location for the production and
 * development services are hardcoded as string constants at the start of this
 * program. They may need to be changed based on the installation. There is
 * also a data file containing version information ("latestClientVersion.dat")
 * that should be in the development and production configuration directories.
 *
 * Java initiates the connection and inetd starts this program with the
 * connection open on STDIN (fd=0). This program then does a read and does
 * string compares looking for known requests for specific data - this is a
 * code word that relates directly to program names that serve data for specific
 * windows. If it gets a known code word, it does a fork and exec of a
 * server program (specific to each code word) that will supply the response
 * to the initial request.  The new server program will then do all the chit-chat
 * with the requester (data updates). The original parent (this program)
 * hangs around to catch and log child crashes. It exits after the child exits.
 * The child process is passed 3 arguments:
 *  1) The window server executable filename
 *  2) An integer parameter
 *  3) The username string
 * There is an optional prefix ('*') that can be included in front of the
 * input code word. This prefix is passed to the exec'ed program as a prefix
 * to argv[0] (the program name).
 *
 *
 * For testing, it is sometimes convenient to interact with this object via telnet.
 * When the object is created by the main() of the internet service program,
 * it asks for authentication. Note that from telnet, you can only do a guest login,
 * as a real login comes in an  encrypted packet. The login dialog must be done even
 * if authentication has been disabled in the makefile (by not including -D AUTH).
 * If authentication has been disabled, the results of the login dialog are ignored
 * and everyone has privileged access. An example of telnet program startup:
 *
 * <PRE>
 * tioga>telnet acc 3101
 * Trying 192.100.16.7...
 * Connected to inyo.
 * Escape character is '^]'.
 * GUEST                     !you type this to the authorization routine
 * x                         !you type this id (any random string)
 * Onsite. User unknown. No restrictions apply.
 * demo                      !you specify the codename of the window server pgm
 * 1.7 1.7.1 Display         !The numbers are server and client version numbers,
 *                           !and will change with time.
 * </PRE>
 *
 */
class MasterServer {
public:
    MasterServer( );

    /**
    * Constructor
    * @param pgpDir  The full directory for PGP private and public keys
    * @param rootPathServerPath Root path based full directory name of the
    *                           location of the executable program.
    * @param serverPathOverride Override full directory name of the
    *                           location of the executable program.
    * @param programName The filename of the server program to be run.
    *                    Just the program name, not the full path.
    * @param connectionFilename The filename (without path) of the memory
    *                    mapped file used to log the connections. This cannot
    *                    be a remotely mounted filesystem.
    * @param traceLevel The trace level (command line input)
    * @param useDbms Use dbms? (command line input)
    * @param niceLevel The nice level (command line input)
    * @param windowList A list of code words associated with filenames of
    *                   executables that are invoked for the code word.
    * @param checkDomain Flag to control checking of domain of the client.
    *                    If this is true, then any client outside of the
    *                    privileged domain will be required to supply user/pwd.
    */
    int run( const std::string & pgpDir,
             const std::string & rootPathServerPath,
             const std::string & serverPathOverride,
             const std::string & programName,
             const std::string & mmapPath,
             const std::string & connectionFilename,
             int                 traceLevel,
             bool                useDbms,
             int                 niceLevel,
             const WindowList &  windowList,
             bool                checkDomain );

private:
    // No copying
    MasterServer( const MasterServer & rhs );
    MasterServer & operator=( const MasterServer & rhs );
};

} // namespace carma::ui::rtd
} // namespace carma::ui
} // namespace carma

#endif  // end conditional include guard
