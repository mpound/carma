#include <iostream>

#include <sys/types.h>
#include <sys/socket.h>

#include "carma/szaarrayutils/szacontrol.h"

#include "carma/szautil/Program.h"

#include "carma/szautil/NetMonitorFrameClient.h"

using namespace std;
using namespace sza::util;

PROGRAM_KEYWORDS = {
  { "host",       "localhost", "s", USAGE "Server host"},
  { "clientPort", "5666",      "i", USAGE "Client port to connect to on host"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

using namespace carma::util;

int Program::main(void)
{
  NetMonitorFrame nmf;

  int fdPair[2];
  int fdRead;
  int fdWrite;
  
  if(::socketpair(AF_LOCAL, SOCK_STREAM, 0, fdPair)) {
    ThrowSysError("socketpair()");
  }
  
  fdRead  = fdPair[0];
  fdWrite = fdPair[1];

  try {

    // Spawn a client to retrieve the data from the server

    NetMonitorFrameClient client(true, Program::getParameter("host"), Program::getIntParameter("clientPort"), &nmf, fdWrite);
    client.spawn();

    // And create a server that will service viewer connection requests

    sza::array::ViewerServer server(false, 6443, &nmf, fdRead);
    server.run();

  } catch(Exception& err) {
    COUT(err.what());
  }

  return 0;
}
