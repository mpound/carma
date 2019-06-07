/**@file
 * canOverIpServer main implementation.
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.5 $
 * $Date: 2012/11/07 17:10:45 $
 * $Id: canOverIpServer.cc,v 1.5 2012/11/07 17:10:45 abeard Exp $
 */

#include <cerrno>
#include <cstdio>
#include <iostream>
#include <memory>

// System includes
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/socket.h>
#include <unistd.h>

// Carma includes
#include "carma/canbus/Session.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ErrorException.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace carma::util;
using namespace carma::canbus::canoverip;
using namespace log4cpp;
using namespace std;

namespace {
    
    // Useful constants
    const uint16_t READ_PORT = 15000; 
    const uint16_t WRITE_PORT = 15001; 
    const size_t MAXHOSTNAME = 255;
    const time_t CONNECTION_TIMEOUT = 5; // Seconds
    const time_t ACCEPT_TIMEOUT_NS = 200000000; // 200 nano-seconds

    typedef union {
        struct sockaddr_in sa_in; // Server addresses
        struct sockaddr sa; 
    } SockAddressUnion;

} // End anonymous namespace

// ----------
int createSocket() 
{
    int sock;
    sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (sock < 0) {
        throw CARMA_ERROR ("Error creating socket: "
            + (string)strerror(errno));
    }

    /*
     * Allow local address re-use...
     */
    int optval = 1;           /* Boolean true for SO_REUSEADDR option */
    int optlen = sizeof(int);
    
    if( setsockopt(
        sock, SOL_SOCKET, SO_REUSEADDR, (char *) &optval, optlen) <0 ) 
    {
        close(sock);
        throw CARMA_ERROR ("setsockopt(): " + (string)strerror(errno));
    };

    return sock;
}

// ----------
void closeSocket(int sock) {
    // TODO: Check these return values and retry if necessary.
    shutdown(sock, SHUT_RDWR);
    close(sock);
}

// ----------
void bindReadWrite(int readSock, int writeSock, string host) 
{
    SockAddressUnion sau;

    struct hostent *hostEntry = gethostbyname(host.c_str());
    
    if (hostEntry == NULL)
        throw CARMA_ERROR("Unable to bind to " + host + ": " 
            + (string)hstrerror(h_errno));
    
    // Preparations for binding local address
    memset(&sau.sa_in, 0, sizeof(sau));
    sau.sa_in.sin_family = AF_INET;  // TCP/IP Sockets
    sau.sa_in.sin_port = htons(READ_PORT);   
    memcpy(&sau.sa_in.sin_addr.s_addr, hostEntry->h_addr, 4); // Bind to specific host

    // Bind read socket to the local network interface
    if (bind(readSock, &sau.sa, sizeof(sau.sa)) < 0) {
        throw CARMA_ERROR ("Error binding to socket: "
            + (string)strerror(errno));
    }

    sau.sa_in.sin_port = htons(WRITE_PORT);
    // Bind write socket to the local network interface
    if (bind(writeSock, &sau.sa, sizeof(sau.sa)) < 0) {
        throw CARMA_ERROR ("Error binding to socket: "
            + (string)strerror(errno));
    }

    Program::getLogger() << Priority::NOTICE 
         << "CanOverIp bound to read/write ports " 
         << READ_PORT << "/" << WRITE_PORT << " on interface " << host 
         << ".";
}

// ----------
void listenReadWrite(int readSock, int writeSock) 
{
    // Listen one at a time please...
    if (listen(readSock, 1) != 0) {
        throw CARMA_ERROR ("Error listening on read "
            "socket: " + (string)strerror(errno));
    }
    
    if (listen(writeSock, 1) != 0) {
        throw CARMA_ERROR ("Error listening on read "
            "socket: " + (string)strerror(errno));
    }
}

// ----------
bool waitForClientReadConnect(int readSock, int writeSock)
{
    fd_set rfds;
    int nMatchedFd;  // Number of matching file descriptors returned by pselect
    int maxFd = (readSock > writeSock ? readSock + 1 : writeSock + 1);
    int asd;        // Accepted socket descriptor
    SockAddressUnion sau;
    socklen_t sockLength = sizeof( sau.sa );
    
    FD_ZERO(&rfds);
    FD_SET(readSock, &rfds);
    FD_SET(writeSock, &rfds);
    struct timespec timeout;
    timeout.tv_sec = 0;
    timeout.tv_nsec = ACCEPT_TIMEOUT_NS;

    // Block on both the read and write sockets.  Will return when one or both
    // is available for accept.
    nMatchedFd = pselect(maxFd, &rfds, NULL, NULL, &timeout, NULL);

    if (nMatchedFd == 1) {
        // Check to make sure a client didn't try to initiated a write first
        if (FD_ISSET(writeSock, &rfds)) {
            // It did, accept it then drop it...
            asd = accept(writeSock, &sau.sa, &sockLength );
            closeSocket(asd);
            return false;
        } else if (FD_ISSET(readSock, &rfds)) {
            return true;
        } else {
            // False reading or misformed fdset.
            return false;
        }
    } else if (nMatchedFd == 2 && FD_ISSET(readSock, &rfds)) {
        return true;
    } else {
        // False reading or timeout
        return false;
    }
}
            
// ----------
Session* acceptConnections(int readSock, int writeSock) 
{
    int rsd, wsd; // Read/Write socket descriptors.

    SockAddressUnion rau, wau;
    socklen_t len = sizeof( rau );
    
    fd_set rfds;
    int nMatchedFd;  // Number of matching descriptor returned from pselect.
    struct timespec timeout;

    // Wait for a client to properly initiate a read connection
    if ( !waitForClientReadConnect(readSock, writeSock) )
        return 0;

    // Accept the read connection - should be properly confirmed. 
    rsd = accept(readSock, &rau.sa, &len);
    if (rsd <= 0) {
        Program::getLogger() << Priority::ERROR 
            << "acceptConnections() - Unable to accept read.";
        closeSocket(rsd);
        return NULL;
    } 
    
    // Block on pselect for a write connection but time this out if it
    // doesn't occur within timeout...
    FD_ZERO(&rfds);
    FD_SET(writeSock, &rfds);
    timeout.tv_sec = CONNECTION_TIMEOUT;
    timeout.tv_nsec = 0;

    // Block until a write connection is made or timeout occurs.
    nMatchedFd = pselect(writeSock + 1, &rfds, NULL, NULL, &timeout, NULL);
    
    // nMatchedFd should never be greater than one.
    if (nMatchedFd == 1 && FD_ISSET(writeSock, &rfds)) {
        // This is our boy - request is properly formed, accept it and
        // spawn a Session.
        wsd =  accept(writeSock, &wau.sa, &len);
        // verify that read and write request come from same client.
        if (wau.sa_in.sin_addr.s_addr == rau.sa_in.sin_addr.s_addr) {
            // Yep they match spawn a session and get out of dodge.
            try {
                return new Session(rsd, wsd);
            } catch (carma::util::ErrorException &ex) {
                ostringstream err;
                err << "acceptConnections() - " << ex.what();
                programLogErrorIfPossible( err.str() );
                cerr << err.str();
                closeSocket(rsd);
                closeSocket(wsd);
                return NULL;
            }
        } else {
            // Oops something went wrong - force client to retry.
            Program::getLogger() << Priority::ERROR <<
                "acceptConnections() - read and write "
                "socket addresses did not match!";
            closeSocket(rsd);
            closeSocket(wsd);
            return NULL;
        }
    } else {
        // pselect timed out or screwed up - request is misformed.
        Program::getLogger() << Priority::ERROR << 
            "acceptConnections() - Attempt to initiate connection timedout. "
            "Client connected with read socket but didn't initiate a write "
            "socket connection.";
        closeSocket(rsd); // Close read socket only
        return NULL;
    }
}


/**
 * @version $Revision: 1.5 $
 *
 * @description
 * CAN over IP server. No arguments needed.  A 'Can Master' application 
 * must be running on the same system as this server as this server
 * makes use of the carma::canbus::DirectCan interface.
 *
 * @usage canOverIp 
 *
 * @key host @noDefault s Specific host to bind to, otherwise, use gethostname.
 * @logger DEFAULT_FACILITY carma.canbus.canoverip
 */
int Program::main() 
{
    string hostname;
    int rsd, wsd;          // Server R/W-Client R/W socket descriptors
    Session * sess = 0;     
    try {

        // Create read/write sockets.
        rsd = createSocket();
        wsd = createSocket();

        if (parameterWasSpecified("host")) {
            hostname = getStringParameter("host");
         } else {
            ::std::auto_ptr< char > ushostname( new char[ MAXHOSTNAME ] );
            if (gethostname( ushostname.get(), MAXHOSTNAME ))
                throw CARMA_ERROR("Unable to retrieve hostname.");
            hostname = ushostname.get();
         }
        
        // Bind and listen on read and write sockets.
        bindReadWrite(rsd, wsd, hostname);
        listenReadWrite(rsd, wsd);
        Program::getLogger() 
            << Priority::INFO << "Waiting for clients to connect...";

        while ( !imrTerminationRequested() ) {
            // Try to accept connections
            sess = acceptConnections(rsd, wsd);
            // DON'T do anything with this pointer (Session deletes itself).
            if ( sess != 0 ) 
                sleep( 1 );
        }

        closeSocket(rsd);
        closeSocket(wsd);

    } catch (...) {
        ostringstream errorMsg;
        errorMsg << "Exception caught in Program::main() - Exiting. ";
        try {
            throw; // Rethrow
        } catch (const carma::util::ErrorException  &err) {
            errorMsg << err.what();
        } catch (const std::exception &ex) {
            errorMsg << ex.what();
        } catch (...) {
            errorMsg << "Exception unkown.";
        }
        // Report to console and log
        cerr << errorMsg.str() << endl;
        Program::getLogger() << Priority::CRIT << errorMsg.str();
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
