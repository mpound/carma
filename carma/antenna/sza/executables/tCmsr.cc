#include <iostream>
#include <signal.h>

#include <sys/types.h>
#include <sys/socket.h>

#include "carma/szautil/Program.h"

#include "carma/szautil/NetMonitorFrame.h"
#include "carma/szautil/NetMonitorFrameClient.h"
#include "carma/szautil/NetMonitorFrameServer.h"

#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"

using namespace std;
using namespace sza::util;

PROGRAM_KEYWORDS = {
  { "host",       "acc.carma.pvt", "s", USAGE "Server host to connect to"},
  { "clientPort", "5665",          "i", USAGE "Client port to connect to on host"},
  { "serverPort", "5666",          "i", USAGE "Port on which we will re-serve data"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

using namespace sza::util;

//=======================================================================
// Entry point of this program.  We instantiate a monitor system
// reader, to acquire data from the CARMA monitor system, and a
// server, to republish data in mpstore format.
//=======================================================================

int Program::main(void)
{
  sigset_t allSignals;
  sigfillset(&allSignals);
  pthread_sigmask(SIG_BLOCK, &allSignals, NULL);

  NetMonitorFrameServer* server = 0;
  NetMonitorFrameClient* client = 0;

  bool stop = false;

  while(!stop) {

    try {
      NetMonitorFrame nmf;

      int fdPair[2];
      int fdRead;
      int fdWrite;
      
      if(::socketpair(AF_LOCAL, SOCK_STREAM, 0, fdPair)) {
	ThrowSysError("socketpair()");
      }
      
      fdRead  = fdPair[0];
      fdWrite = fdPair[1];
      
      server = new NetMonitorFrameServer(false, Program::getIntParameter("serverPort"), &nmf, fdRead);
      client = new NetMonitorFrameClient(true, Program::getParameter("host"), Program::getIntParameter("clientPort"), &nmf, fdWrite);
      
      server->spawn();
      client->spawn();
      
      // And block in server run method

      server->run();
      stop = true;

    } catch(carma::util::ErrorException& err) {
      CARMALOGINFO("Caught an error: " << err.what());
      COUT("Caught an error: " << err.what());
    } catch(sza::util::Exception& err) {
      CARMALOGINFO("Caught an error: " << err.what());
      COUT("Caught an error: " << err.what());
    } catch(...) {
      CARMALOGINFO("Caught an unknown error");
      COUT("Caught an unknown error");
    }

    // Explicitly call the destructors to make sure any socket
    // connections are properly shut down

    if(server) {
      delete server;
      server = 0;
    }

    if(client) {
      delete client;
      client = 0;
    }

    // Sleep 10 seconds and try again

    if(!stop)
      sleep(10);
  }

  return 0;
}
