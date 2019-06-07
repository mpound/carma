#include <iostream>

#include <sys/types.h>
#include <sys/socket.h>

#include "carma/szaarrayutils/szacontrol.h"

#include "carma/szautil/Program.h"

#include "carma/szautil/RtdMonitorFrameClient.h"
#include "carma/szautil/RtdServerTask.h"

using namespace std;
using namespace sza::util;

PROGRAM_KEYWORDS = {
  { "host",       "localhost", "s", USAGE "Server host"},
  { "clientPort", "5667",      "i", USAGE "Client port to connect to on host"},
  { "serverPort", "5668",      "i", USAGE "Server port on which to service rtd clients"},
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

    RtdMonitorFrameClient client(true, Program::getParameter("host"), Program::getIntParameter("clientPort"), &nmf, fdWrite);
    client.spawn();

    RtdServerTask server(false, Program::getIntParameter("serverPort"), &nmf, fdRead);
    server.run();

  } catch(Exception& err) {
    COUT(err.what());
  }

  return 0;
}
