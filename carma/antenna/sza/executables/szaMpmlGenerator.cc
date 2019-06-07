#include <iostream>

#include "carma/szautil/Program.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/MpmlGen.h"

using namespace std;
using namespace sza::util;

PROGRAM_KEYWORDS = {
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

int Program::main(void)
{
  MpmlGen gen;

  gen.setOutputDirectory(".");
  gen.writeSzaMpmlTemplate();
  gen.writeSzaMonitorPointMapping();

  return 0;
}
