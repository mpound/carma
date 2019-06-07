/**
 * SenderAddress retrieves peer information from a socket descriptor.
 *
 * @author  Steve Scott
 * $Id: SenderAddress.cc,v 1.9 2014/04/02 23:11:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <iostream>
#include <strings.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/param.h>

#include <carma/ui/rtd/common/SenderAddress.h>

using namespace std;
using namespace carma::ui::rtd;

/*
 * Constructor - uses getpeername(3N) and gethostbyaddr(3N) to build a object
 * describing a peer on a socket descriptor.  If the interger passed as
 * an argument into the constructor is not a socket descriptor, construct
 * a SenderAddress object with hostName set to DefaultHostName and all
 * all other member variables will be 0.
 */
SenderAddress::SenderAddress(int fd)
{
    char*           name;
    struct hostent* hostent;

    // on a 64bit system size_t is 64bits but
    // socklen_t is 32bits. This will cause a
    // compile error in the getpeername call below if
    // one tries to static_cast<socklen_t *>(a size_t *).
    // Therefore, force addrlen to be 32bits wide.
    uint32_t        addrlen = static_cast<uint32_t>(sizeof(address));

    /**
     * If for any reason, getpeername fails, zero out the address member
     * variable, ultimately this will cause DefaultHostName to be
     * copied to hostName.
     */

    if (getpeername(fd, &address.sa, static_cast<socklen_t*>(&addrlen))) {
        bzero (&address.sin, addrlen);
    }

    char *naddr = inet_ntoa(address.sin.sin_addr);
    if (naddr) {
        this->netAddress = naddr;
    }

    // Get the host name
    // Sometimes the hostname returned does not have the domainname appended,
    // so you have to search the alias list. A name containing a dot ('.')
    // is assumed to be what we are looking for.
    hostent = gethostbyaddr((char*)&address.sin.sin_addr,sizeof(struct in_addr), address.sin.sin_family);
    if ( hostent != 0 ) {
        name = hostent->h_name;
        for (int i=0; i<3; i++) {
            if (strchr(name, '.'))break;
            if (hostent->h_aliases[i] != 0)name = hostent->h_aliases[i];
            else break;
        }

        this->hostName = name;

        // Here domainName is set to point to the first character
        // past the first '.' in the fully qualified hostname.
        size_t found = this->hostName.find_first_of(".");
        if (found != std::string::npos) {
            this->domainName = this->hostName.substr(found + 1);
        } else {
            this->domainName = "";
        }
    } else {
        this->hostName = this->netAddress;
        this->domainName = "";
    }

    // Get the server host and domain name
    char servername[MAXHOSTNAMELEN];
    if (gethostname(servername, MAXHOSTNAMELEN) != -1) {
        servername[MAXHOSTNAMELEN-1] = '\0';
    } else {
        // Failure
        this->serverHostName_   = "gethostnameFailed";
        this->serverDomainName_ = "gethostnameFailed";
        return;
    }

    hostent = gethostbyname(servername);
    if ((hostent == 0) || (hostent->h_name == 0)) {
        // Failure
        this->serverHostName_   = "gethostbynameFailed";
        this->serverDomainName_ = "gethostbynameFailed";
        return;
    } else {
        name = hostent->h_name;
    }

    // Sometimes the hostname does not have the domainname appended,
    // so you have to search the alias list. A name containing a dot ('.')
    // is assumed to be what we want.
    for (int i=0; i<3; i++) {
        if (strchr(name, '.'))break;
        if (hostent->h_aliases[i] != 0)name = hostent->h_aliases[i];
        else break;
    }

    this->serverHostName_ = name;

    {
        size_t found = this->serverHostName_.find_first_of(".");
        if (found != std::string::npos) {
            this->serverDomainName_ = this->serverHostName_.substr(found + 1);
        } else {
            this->serverDomainName_ = "NoDomainName";
        }
    }
}

SenderAddress::SenderAddress(const SenderAddress &s)
{
    this->hostName = s.hostName;
    this->domainName = s.domainName;
    this->address = s.address;
}

SenderAddress& SenderAddress::operator = (const SenderAddress &s)
{
    if (this != &s) {
        this->netAddress = s.netAddress;
        this->hostName = s.hostName;
        this->domainName = s.domainName;
        this->address = s.address;
    }

    return *this;
}

std::string SenderAddress::getHostName() const
{
    return hostName;
}

std::string SenderAddress::getDomainName() const
{
    return domainName;
}

std::string SenderAddress::getNetAddress() const
{
    return netAddress;
}

int SenderAddress::getPortAddress () const
{
  return address.sin.sin_port;
}

int SenderAddress::getLocalAddress () const
{
  return inet_lnaof(address.sin.sin_addr);
}

std::string SenderAddress::getServerDomainName() const
{
    return serverDomainName_;
}

std::string SenderAddress::getServerHostName() const
{
    return serverHostName_;
}
