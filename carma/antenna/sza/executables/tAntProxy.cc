#include <iostream>

#include "carma/corba/Client.h"

#include "carma/szautil/Angle.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/Program.h"

#include "carma/antenna/sza/antenna/corba/DriveProxy.h"
#include "carma/antenna/sza/antenna/corba/RxProxy.h"
#include "carma/antenna/sza/antenna/corba/RxSelector.h"

#include "carma/antenna/common/RxControl.h"

using namespace std;
using namespace sza::util;

PROGRAM_KEYWORDS = {
  { "szaPrefix", "sza1",  "s", USAGE "CARMA Antenna prefix to control"},
  { "mode",      "zero",  "s", USAGE "Wrap mode"},
  { "az",        "0.0",   "s", USAGE "Commanded AZ"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

using namespace carma::antenna::common;

int Program::main(void)
{
  std::string mode = Program::getParameter("mode");
  std::ostringstream os;
  os << "carma." << Program::getParameter("szaPrefix") << ".Drive";

  try {

    COUT("Attempting to resolve: " << os.str());

    carma::antenna::sza::control::DriveControl_var drive = 0;
    drive = getCorbaClient().resolveName<carma::antenna::sza::control::DriveControl>(os.str());

    COUT("Here with mode = " << mode);

    if(mode == "add")
      drive->setWrapMode(carma::antenna::common::DriveControl::ADD);
    else if(mode == "sub")
      drive->setWrapMode(carma::antenna::common::DriveControl::SUB);
    else
      drive->setWrapMode(carma::antenna::common::DriveControl::ZERO);

    sleep(1);

    sza::util::Angle az;

    az.setDegrees(Program::getParameter("az"));
    drive->setAzel(az.degrees(), 45.0, 0);
    sleep(1);

  } catch(carma::util::UserException& err) {
    COUT(err.errorMsg);
  } catch(...) {
    COUT("Caught an unknown error");
  }

  return 0;
}
