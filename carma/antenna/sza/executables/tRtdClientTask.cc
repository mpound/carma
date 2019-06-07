#include <iostream>

#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>

#include "carma/szaarrayutils/szacontrol.h"

#include "carma/szautil/Program.h"

#include "carma/szautil/RtdClientTask.h"
#include "carma/szautil/RtdServer.h"
#include "carma/szautil/NetVar.h"
#include "carma/szautil/TcpClient.h"

using namespace std;
using namespace sza::util;

PROGRAM_KEYWORDS = {
  { "host",       "localhost", "s", USAGE "Server host"},
  { "clientPort", "5668",      "i", USAGE "Client port to connect to on host"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

using namespace carma::util;

int Program::main(void)
{
#if 1
  RtdClientTask client(true, Program::getParameter("host"), Program::getIntParameter("clientPort"));
  client.spawn();


  sleep(5);

  client.sendAddRegMsg(124771);
  client.sendAddRegMsg(124780);
  client.sendAddRegMsg(64203);

  sleep(2);

  client.sendRemRegMsg(124780);
  client.sendRemRegMsg(64203);

  sleep(2);

  client.blockForever();

#else
  TcpClient tcp(Program::getParameter("host"), Program::getIntParameter("clientPort"));
  tcp.connectToServer(true);

  sleep(5);
  Vector<unsigned char> buffer(16);
  unsigned int* iptr = (unsigned int*)&buffer[0];
  iptr[0] = htonl(0);
  iptr[1] = htonl(16);
  iptr[2] = htonl(3);
  iptr[3] = htonl(1);
  tcp.writeBytes(buffer);

  sleep(10);
#endif

  return 0;
}
