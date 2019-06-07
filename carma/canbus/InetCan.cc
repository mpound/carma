/** @file
 * Definition of carma::canbus::InetCan class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.11 $
 * $Date: 2012/07/25 17:51:26 $
 * $Id: InetCan.cc,v 1.11 2012/07/25 17:51:26 abeard Exp $
 */

// Carma includes
#include "carma/canbus/InetCan.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Trace.h"

#include <cerrno>

// System includes
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>

using namespace carma::canbus;
using namespace carma::util;
using namespace std;

const unsigned int InetCan::READ_PORT;
const unsigned int InetCan::WRITE_PORT;

// -----------------------------------------------------------------------------
InetCan::InetCan(
    const std::string hostname,
    unsigned int ac,
    unsigned int am,
    unsigned short busac) : 
    hostname_(hostname)
{
    // Connect expects a sockaddr wereas everybody else deals with sockaddr_in.
    // The structures are guaranteed to be compatible with each other.  A way
    // to do this is to define a union between the two...
    union sock {
        struct sockaddr s;
        struct sockaddr_in i;
    };

    union sock readAddress;
    union sock writeAddress;
    struct hostent *she; // Network host entry for canOverIp server.
    int cStatus;  // Connection status.

    // Create the sockets...
    readSock_ = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (readSock_ == -1)
        throw CARMA_EXCEPTION(carma::util::ErrorException,
                "InetCan::InetCan() - Error creating read socket - " +
                static_cast<string>(strerror(errno)));

    writeSock_ = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (writeSock_ == -1)
        throw CARMA_EXCEPTION(carma::util::ErrorException,
                "InetCan - Error creating client socket - " +
                static_cast<string>(strerror(errno)));

    // Retrieve server host entry. gethostbyname may return a pointer to
    // static data which in turn may be overwritten by subsequent calls.  Thus
    // care is taken to do a deep copy of the h_addr into the s_addr....
    she = gethostbyname(hostname_.c_str());

    if (!she) 
        throw CARMA_ERROR("Unable to bind to " + hostname + ": "
            + (string)hstrerror(h_errno));

    // Create the read and write addresses...
    readAddress.i.sin_family = AF_INET;  // TCP/IP Sockets
    readAddress.i.sin_port = htons(READ_PORT);
    if (!memcpy(&readAddress.i.sin_addr.s_addr, she->h_addr, she->h_length))
        throw CARMA_EXCEPTION(carma::util::ErrorException,
                "InetCan::InetCan() - Error memcpying server host address to "
                "readAddress structure.");

    writeAddress.i.sin_family = AF_INET; // TCP/IP Sockets
    writeAddress.i.sin_port = htons(WRITE_PORT);
    if (!memcpy(&writeAddress.i.sin_addr.s_addr, she->h_addr, she->h_length))
        throw CARMA_EXCEPTION(carma::util::ErrorException,
                "InetCan::InetCan() - Error memcpying server host address to "
                "writeAddress structure.");

    // Connect to can over ip server:
    // Protocol says first connect to port 15000 (READ_PORT) then
    // connect to port 15001 (WRITE_PORT)...
    cStatus = connect(readSock_, &(readAddress.s), sizeof(readAddress));
    if (cStatus != 0)
        throw CARMA_EXCEPTION(carma::util::ErrorException,
                "InetCan::InetCan() - Error connecting to read socket on "
                "server " + hostname_ + ": " + strerror(errno));

    // Sleep one second to allow server to accept...1
    sleep(1);

    cStatus = connect(writeSock_, &(writeAddress.s), sizeof(writeAddress));
    if (cStatus != 0)
        throw CARMA_EXCEPTION(carma::util::ErrorException,
                "InetCan::InetCan() - Error connecting to write socket on "
                "server " + hostname_ + ": " + strerror(errno));

    CPTRACE(Trace::TRACEALL, "InetCan::InetCan() - Successfully connected to "
            << hostname_);

    // Set the initial acceptance filter.
    addFilter(ac, am, busac);

}

// -----------------------------------------------------------------------------
InetCan::~InetCan()
{
    shutdown(readSock_, SHUT_RDWR);
    shutdown(writeSock_, SHUT_RDWR);
    // close(readSock_);
    // close(writeSock_);
}

// -----------------------------------------------------------------------------
void InetCan::postMessage(
    const carma::canbus::Message& msg,
    carma::canbus::txPriorityType prio)
{
    MsgToServer serverMsg;
    vector<byteType> data = msg.getData();
    
    // Form up server message while making sure that
    // multi-byte types are in Network Byte Order.
    serverMsg.mode = static_cast<unsigned char>(WRITE_CAN_MSG);
    serverMsg.id = htonl(msg.getId());
    serverMsg.busId = htons(msg.getBusId());
    serverMsg.size = static_cast<unsigned char>(msg.getData().size());
    memcpy(serverMsg.data, &data[0], serverMsg.size);
    
    // Send it out...
    sendToServer(serverMsg); 
}

// -----------------------------------------------------------------------------
void InetCan::addFilter(unsigned int ac, unsigned int am, unsigned short busac)
{
    MsgToServer msg;
    msg.mode = static_cast<unsigned char>(SET_READ_FILTER);
    msg.ac = htonl(ac);
    msg.busFilter = htons(busac);
    msg.am = htonl(am);

    sendToServer(msg);
}

// -----------------------------------------------------------------------------
void InetCan::clearFilters()
{
    MsgToServer msg;
    msg.mode = static_cast<unsigned char>(CLEAR_READ_FILTERS);

    sendToServer(msg);
}
    
// -----------------------------------------------------------------------------
void InetCan::sendToServer(const MsgToServer &msg)
{
    // Convert the message into a raw buffer...
    unsigned char buff[16];   // Raw buffer for our bytes.
    unsigned short bi = 0;    // Buffer index
    size_t bs = sizeof(buff); // Size of buffer.
    int status;               // Status of the send

    // Set mode.
    memcpy(&buff[bi], &msg.mode, sizeof(unsigned char));
    
    if (msg.mode == SET_READ_FILTER) {
        bi += sizeof(unsigned char);
        memcpy(&buff[bi], &msg.ac, sizeof(unsigned int));
        bi += sizeof(unsigned int); 
        memcpy(&buff[bi], &msg.busFilter, sizeof(unsigned short));
        bi += sizeof(unsigned short) + 1; // Skip the size.
        memcpy(&buff[bi], &msg.am, sizeof(unsigned int));
    } else if (msg.mode == WRITE_CAN_MSG) {
        bi += sizeof(unsigned char); 
        // Id
        memcpy(&buff[bi], &msg.id, sizeof(unsigned int));
        bi += sizeof(unsigned int);
        // Bus Id
        memcpy(&buff[bi], &msg.busId, sizeof(unsigned short));
        bi += sizeof(unsigned short);
        memcpy(&buff[bi], &msg.size, sizeof(unsigned char));
        bi += sizeof(unsigned char);
        memcpy(&buff[bi], msg.data, static_cast<size_t>(msg.size));
    } else if (msg.mode != CLEAR_READ_FILTERS) {
        // TODO: Raise hell.
    }
    
    status = ::send(writeSock_, static_cast<void *>(buff), bs, MSG_NOSIGNAL);

    if ( status < 0 ) {
        throw CARMA_ERROR("Send failed: " 
            + static_cast<string>(strerror(errno)));
    } 
}

// -----------------------------------------------------------------------------
carma::canbus::Message InetCan::getMessage()
{
    // Waring: this is all pretty crude...  TODO: Cleanup.
    carma::canbus::Message msg; // Where the data is going
    
    unsigned int id; 
    unsigned short busId;
    unsigned char size;
    vector<byteType> data(8);
    
    int nBytesRx, status;   // Number of bytes received.
    unsigned char buff[15];
    size_t buffSize = sizeof(buff);


    nBytesRx = 0;
    // Read a message and keep reading until we receive the whole message
    while ( nBytesRx < static_cast<int>( buffSize ) ) {
        status = recv(readSock_, &buff[nBytesRx], buffSize-nBytesRx,
                MSG_NOSIGNAL);
        if ( status == 0 ) {       // Clean termination by server.
            throw CARMA_ERROR("Connection terminated by server.");
        } else if ( status < 0 ) { // Other badness
            throw CARMA_ERROR( strerror(errno) );
        } else {                   // Success
            nBytesRx += status;
        }
    }
    
    // Unpack the raw data and stick it into a CAN message.
    memcpy(&id, &buff[0], sizeof(unsigned int));
    memcpy(&busId, &buff[4], sizeof(unsigned short));
    memcpy(&size, &buff[6], sizeof(unsigned char));
    memcpy(&data[0], &buff[7], static_cast<size_t>(size));

    // Set the data
    msg.setId(ntohl(id));
    msg.setBusId(ntohs(busId));
    msg.setData(data);

    return msg;
}
