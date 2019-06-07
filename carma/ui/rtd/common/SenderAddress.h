#ifndef CARMA_UI_RTD_SENDERADDRESS_H
#define CARMA_UI_RTD_SENDERADDRESS_H

/**
 * @file
 *
 * SenderAddress.h - object that will return information about a peer
 * at the other end of a connected socket.  Works by examining a
 * hostent structure returned by gethostbyaddr(3N).
 *
 * Requires linking with libnsl and libresolv.
 *
 * @author Scott Brumbaugh
 * $id: $
 *
 * $CarmaCopyright$
 */

#include <iostream>
#include <unistd.h>
#include <netdb.h>
#include <netinet/in.h>
#include <errno.h>

#include <string>

namespace carma {
namespace ui {
namespace rtd {

/**
 * SenderAddress stores and makes available peer information
 * for the opposite (client) end of a connected socket.
 * Information about this send, the server end, is also exposed.
 */
class SenderAddress {

 public:

  /**
   * There is no default constructor.  A SenderAddress
   * constructor should be passed a connected socket descriptor.
   * as its only argument.
   *
   * If the integer argument is not a connected socket descriptor,
   * then a SenderAddress object will still be constructed.  In this
   * case, SenderAddress.getHostname() and SenderAddress.getDomainName()
   * will return valid 'char *' values initialized to a default value.
   * All other member functions will return 0.
   */
    SenderAddress(int fd);

    /// Allow copy
    SenderAddress(const SenderAddress &s);

    /// Allow initialize by assignment.
    SenderAddress& operator = (const SenderAddress &s);

    /// Full host name, including domain
    std::string getHostName() const;

    /// Get the domain name
    std::string getDomainName() const;

    /**
     * Return a character string representation of the IP address in
     * the standard 'dot' format - i.e. 192.100.16.1
     */
    std::string getNetAddress() const;

    /// Return the port
    int getPortAddress() const;

    /// Return the local address
    int getLocalAddress() const;

    /**
     * Get server's address. The server is  the machine running
     * the program that creates this object.
     */
    std::string getServerHostName() const;

    /// Get server's domain name
    std::string getServerDomainName() const;

private:
    std::string netAddress; // dotted-quad IP address
    std::string hostName;   // fully qualified host name
    std::string domainName;
    std::string serverHostName_;  // Fully qualified
    std::string serverDomainName_;
    union {
        struct sockaddr_in sin;
        struct sockaddr    sa;
    } address;
};

} // namespace carma::ui::rtd
} // namespace carma::ui
} // namespace carma

#endif // CARMA_UI_RTD_SENDERADDRESS_H
