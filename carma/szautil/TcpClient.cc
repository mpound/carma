#include <sys/socket.h>
#include <unistd.h>

#include "carma/szaarrayutils/tcpip.h"

#include "carma/szautil/TcpClient.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
TcpClient::TcpClient()
{
  fd_ = -1;
  connected_   = false;

  hostInitialized_ = false;
  portInitialized_ = false;
};

/**.......................................................................
 * Constructor.
 */
TcpClient::TcpClient(std::string host, unsigned port) 
{
  fd_ = -1;
  connected_   = false;

  setHost(host);
  setPort(port);
};

/**.......................................................................
 * Destructor.
 */
TcpClient::~TcpClient() 
{
  disconnect();
};

/**.......................................................................
 * Constructor.
 */
int TcpClient::connectToServer(std::string host, unsigned port, bool doWait) 
{
  setHost(host);
  setPort(port);

  return privateConnectToServer(doWait);
};

/**.......................................................................
 * Connect to the server
 */
int TcpClient::connectToServer(bool doWait)
{
  DBPRINT(true, Debug::DEBUG9, "host_ = " << host_);
  DBPRINT(true, Debug::DEBUG9, "port_ = " << port_);

  return privateConnectToServer(doWait);
};

/**.......................................................................
 * Connect to the server
 */
int TcpClient::privateConnectToServer(bool doWait)
{
  if(!(hostInitialized_ && portInitialized_)) {
    ThrowError("Host and port number have not been set");
  }

  // Terminate any existing connection.
  
  disconnect();

  // Now attempt to connect.

  fd_ = tcp_connect((char *)host_.c_str(), port_, doWait);
  
  if(fd_ < 0) {
    LogMessage(true, "Error in tcp_connect().");
    return -1;
  }
  
  // If tcp_connect() successfully returned an fd mark us as connected.
  
  connected_ = true;

  return fd_;
};

/**.......................................................................
 * Disconnect the connection to the control-program control port.
 */
void TcpClient::disconnect()
{
  if(fd_ >= 0) {
    shutdown(fd_, 2);
    close(fd_);
    fd_ = -1;
  };

  connected_ = false;
}

/**.......................................................................
 * Set the host
 */
void TcpClient::setHost(std::string host)
{
  host_ = host;
  hostInitialized_ = true;
}

/**.......................................................................
 * Set the port
 */
void TcpClient::setPort(unsigned port)
{
  port_ = port;
  portInitialized_ = true;
}

/**.......................................................................
 * Return true if we are connected
 */
bool TcpClient::isConnected()
{
  return connected_;
}

/**.......................................................................
 * Configure the socket managed by this class for
 * blocking/non-blocking I/O
 */
void TcpClient::setBlocking(bool doWait)
{
  LogStream errStr;

  if(tcp_set_blocking(fd_, doWait)) {
    errStr.appendMessage(true, "Unable to reconfigure socket");
    throw  Error(errStr);
  }
}
