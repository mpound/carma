#include <iostream>

#include <sys/types.h>
#include <sys/socket.h>

#include "carma/szaarrayutils/szacontrol.h"

#include "carma/szautil/Program.h"

#include "carma/szautil/FastPdbClientTask.h"
#include "carma/szautil/CondVar.h"
using namespace std;
using namespace sza::util;

PROGRAM_KEYWORDS = {
  { "src",        "MARS",      "s", USAGE "Source to list"},
  { "project",    "c1176",     "s", USAGE "Project to list"},
  { "matchsrc",   "t",         "b", USAGE "True to match sources.  False to list projects"},
  { "host",       "localhost", "s", USAGE "Server host"},
  { "clientPort", "5669",      "i", USAGE "Client port to connect to on host"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

using namespace carma::util;

static CondVar condVar1_;
static CondVar condVar2_;

static FASTPDB_CALLBACK_FN(commandCallback)
{
  COUT(s);
  condVar2_.broadcast();
}

int Program::main(void)
{
  condVar1_.lock();
  FastPdbClientTask client(true, Program::getParameter("host"), Program::getIntParameter("clientPort"), &condVar1_);
  client.spawn();
  condVar1_.waitNoLock();

  condVar2_.lock();

  if(Program::getBoolParameter("matchsrc")) 
    client.sendListSourceMsg(Program::getParameter("src"), &commandCallback);
  else
    client.sendListProjectMsg(Program::getParameter("project"), &commandCallback);

  condVar2_.waitNoLock();

  return 0;
}
