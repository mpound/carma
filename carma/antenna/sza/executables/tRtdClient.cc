#include <iostream>

#include <sys/types.h>
#include <sys/socket.h>

#include "carma/szaarrayutils/szacontrol.h"

#include "carma/szautil/Program.h"

#include "carma/szautil/RtdClient.h"
#include "carma/szautil/RtdServer.h"
#include "carma/szautil/NetVar.h"

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
#if 0
  RtdClientData data1, data2;
  RtdServer::ClientData cd;

  data1.addReg_ = 1234;
  data1.setTo(RtdClientData::MEM_ADDREG);

  NetVar* var = (NetVar*)data1.getMember(RtdClientData::MEM_ADDREG);
  unsigned int* iptr = (unsigned int*)(var->vPtr_);

  COUT("addreg is now: " << *iptr);

  std::vector<unsigned char>& bytes = data1.getSerializedData();

  data2.deserialize(bytes);
  COUT("data2 = " << data2);

  cd.clientData_.deserialize(bytes);
  COUT("cd = " << cd.clientData_);
  
#else
  RtdClient client(true, Program::getParameter("host"), Program::getIntParameter("clientPort"));
  client.spawn();
  client.blockForever();
#endif

  return 0;
}
