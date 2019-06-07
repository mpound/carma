#include <iostream>
#include <iomanip>

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

using namespace std;
using namespace sza::util;
using namespace carma::util;

PROGRAM_KEYWORDS = {
  { "nameserver",  "corba.carma.pvt:9001",  "s", USAGE "Name Server host:port"},
  { "eventserver", "corba.carma.pvt:8001",  "s", USAGE "Event Server host:port"},
  { "notifyserver","corba.carma.pvt:10001", "s", USAGE "Notification Server host:port"},
  { "project",     "c0481",                 "s", USAGE "Project to list"},
  { "obsblock",    "1SL_95NGC133",          "s", USAGE "Obsblock to modify"},
  { "addobsblock", "f",                     "b", USAGE "True to add an obsblock"},
  { "addobsblocktime", "f",                     "b", USAGE "True to add an obsblock time"},
  { "addtrial",    "f",                     "b", USAGE "True to add a trial"},
  { "remtrial",    "f",                     "b", USAGE "True to remove a trial"},
  { "edittrial",   "f",                     "b", USAGE "True to edit a trial"},
  { "hours",       "10.0",                  "d", USAGE "min allocated time"},
  { "startlst",    "01:00:00",              "s", USAGE "Start LST of the latest trial"},
  { "stoplst",     "03:00:00",              "s", USAGE "Stop LST of the latest trial"},
  { "comment",     "",                      "s", USAGE "Comment"},
  { "list",        "f",                     "b", USAGE "True to list project, otherwise modify the latest trial"},
  { "qsrc",        "t",                     "b", USAGE "True to list projects matching srcname"},
  { "source",      "MARS",                  "s", USAGE "True to list projects matching srcname"},
  { "startdate",   "2013-01-23",            "s", USAGE "Start date for source search"},
  { "stopdate",    "2013-01-24",            "s", USAGE "Start date for source search"},
  { "ntrial",      "f",                     "b", USAGE "True to list number of trials in a project"},
  { "itrial",      "0",                     "i", USAGE "trial to modify, otherwise most recent"},
  { "grade",       "95",                    "d", USAGE "grade to assign to the trial"},

  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

using namespace carma::observertools;


int Program::main(void)
{
  std::vector<string> testStr;
  std::vector<string> testStrSorted;
  testStr.push_back("bee");
  testStr.push_back("tree");
  testStr.push_back("tart");

  testStrSorted = Sort::sort(testStr);

  for(unsigned i=0; i < testStrSorted.size(); i++) {
    COUT(testStrSorted[i]);
  }

  String str("this is a test of a really long string \nthat needs to be wrapped and so Ill try to wrap it");
  str.wrapTo(10, 20);

  COUT(str.str());

  return 0;
}
