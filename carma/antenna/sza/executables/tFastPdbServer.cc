#include <iostream>
#include <iomanip>
#include <fstream>
#include <map>

#include <time.h>

#include "carma/szautil/Program.h"

#include "carma/szautil/Angle.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/FastPdb.h"
#include "carma/szautil/HourAngle.h"
#include "carma/szautil/Server.h"
#include "carma/szautil/Sort.h"
#include "carma/szautil/String.h"
#include "carma/szautil/FastPdbData.h"
#include "carma/szautil/FastPdbServer.h"

using namespace std;
using namespace carma::util;
using namespace sza::util;

PROGRAM_KEYWORDS = {
  { "file",      "/home/eml/projects/carma/database/allprojs.txt", "s", USAGE "database file"},
  { "src",       "MARS",         "s", USAGE "Source to match"},
  { "list",      "t",            "b", USAGE "True to list projects"},
  { "project",   "",             "s", USAGE "If non-empty, the project to list"},
  { "matchsrc",  "t",            "b", USAGE "True to match source"},
  { "matchfreq", "f",            "b", USAGE "True to match frequency"},
  { "freq",      "35",           "d", USAGE "Frequency to match"},
  { "serverPort", "5669",        "i", USAGE "Server port on which to service pdb clients"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

int Program::main(void)
{
  std::string file = Program::getStringParameter("file");

  COUT("Here 0");

  try {
    FastPdbServer server(false, Program::getIntParameter("serverPort"), file);
    server.run();
  } catch(Exception& err) {
    COUT("Caught an error: " << err.what());
  }

  COUT("Here 1");
  return 0;
}
