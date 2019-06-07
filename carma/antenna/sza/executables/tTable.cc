#include <iostream>
#include <iomanip>
#include <vector>

#include <time.h>

#include "carma/corba/Client.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/Program.h"

#include "carma/szautil/Angle.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/HourAngle.h"
#include "carma/szautil/String.h"
#include "carma/szautil/Sort.h"

#include "carma/util/Time.h"
#include "carma/observertools/ProjectDatabaseManager_skel.h"

#include "carma/services/Table.h"

using namespace std;
using namespace sza::util;
using namespace carma::util;
using namespace carma::services;

PROGRAM_KEYWORDS = {
  { "file",  "",  "s", USAGE "Name Server host:port"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

using namespace carma::observertools;


int Program::main(void)
{
  std::ostringstream os;
  os << Program::getConfDir() << "sza/rx/cmDewars.tab";

  Table tab(Program::getStringParameter("file"));

  std::vector<string> dewars = tab.getColumn( "name" );
  std::vector<double> vd1    = tab.getDoubleColumn( "Vd1" );

  COUT("conf dir = " << Program::getConfDir() << " conffile = " << os.str());

  for(unsigned i=0; i < dewars.size(); i++) {
    COUT("Found rx = " << dewars[i] << " " << vd1[i]);
  }


  return 0;
}
