/**
 * $Id: ImrClient.h,v 1.11 2013/04/09 22:48:02 abeard Exp $
 *
 * ImrClient - An IMR client for managing servers via the IMR.
 *
 * cgwon: Original implementation
 *
 * abeard Aug 2010: Generalized interface to facilitate easier use with 
 * both TAO and ORBacus implementations.
 *
 * @author Chul Gwon
 * @author Andrew Beard
 */
#ifndef CARMA_UTIL_IMRCLIENT_H
#define CARMA_UTIL_IMRCLIENT_H

#include <string>
#include <utility>
#include <memory>

namespace carma {

namespace corba {
    class Client;
}

namespace util {

  class ImrClient {
  public:

    /**
     * Server status enumeration.
     * Note that not all implementations report back all states, rather this
     * is a superset of possible states.  The only states which can be
     * used reliably for program logic are RUNNING, STOPPED and UNKNOWN.
     */
    enum ServerStatus {
        UNKNOWN,   /*< Server state could not be determined from the IMR.     */
        FORKED,    /*< Server has been spawned by IMR but not yet initialized
                       it's POAs.                                             */
        STARTING,  /*< Server has created POAs and is proceeding to startup.  */
        RUNNING,   /*< Server has activated POAs and is running normally.     */
        STOPPING,  /*< Server has deactivated POAs and is shutting down.      */
        STOPPED    /*< Server is not running and there are no outstanding 
                       startup requests.                                      */
    };

    /**
     * constructor
     * @param client Reference to corba::Client instance.
     * @param imrHost name of the host machine running the IMR,
     * defaulted to "imr"
     */
    explicit ImrClient( carma::corba::Client & client, 
                        const std::string & imrHost="imr" );

    virtual ~ImrClient();

    /**
     * add OAD to IMR
     * @param oadHost OAD host name
     * @return true if OAD is successfully added, false otherwise
     */
    bool addOad( const std::string & oadHost );
    
    /**
     * remove OAD from IMR
     * @param oadHost OAD host name
     * @return true if OAD is removed, and false otherwise
     */
    bool removeOad( const std::string & oadHost );

    /**
     * Add a server to the OAD.
     * @param oadHost OAD hostname server will live on
     * @param serverName name of the server
     * @param execPath path of the executable for the server on OAD machine
     * @param args Command line arguments to the executable
     * @param optargs 'Optional' command line arguments to the executable. These
     *             are options which proceed the standard carma keywords 
     *             following the '--'.
     * @param runDir Directory executable will be ran from.
     * @return true if server added, false otherwise
     */
    bool addServer( const std::string & serverName,
                    const std::string & oadHost,
                    const std::string & execPath,
                    const std::string & args,
                    const std::string & optArgs,
                    const std::string & runDir );

    /**
     * remove server from OAD/IMR
     * @param serverName name of server
     * @return true if server removed, and false otherwise
     */
    bool removeServer( const std::string & serverName );

    /**
     * start a server through the IMR
     * @param serverName name of server
     * @return true if server started, false otherwise
     */
    bool startServer( const std::string & serverName );

    /**
     * stop a server through the IMR
     * @param serverName name of server
     * @return true if server stopped, and false otherwise
     */
    bool stopServer( const std::string & serverName );

    /**
     * reset the server through the IMR
     * @param serverName name of server
     * @return true if server restarted, and false otherwise
     */
    bool resetServer( const std::string & serverName );

    /**
     * Get server status. 
     * See caveats regarding returned ServerStatus enumeration.  In particular
     * note that not all reported states can be relied upon for anything 
     * other than passive monitoring.
     * @param serverName name of server
     * @return server status enumeration.
     */
    ServerStatus getServerStatus( const std::string & serverName );

  private:

    class Pimpl;
    std::auto_ptr<Pimpl> pimpl_; // Hide the implementation 

  }; // end class ImrClient
} // end namespace util
} // end namespace carma

#endif
