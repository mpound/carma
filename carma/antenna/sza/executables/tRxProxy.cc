#include <iostream>

#include "carma/corba/Client.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/Program.h"

#include "carma/antenna/sza/antenna/corba/DriveProxy.h"
#include "carma/antenna/sza/antenna/corba/RxProxy.h"
#include "carma/antenna/sza/antenna/corba/RxSelector.h"

#include "carma/antenna/common/RxControl.h"

using namespace std;
using namespace sza::util;

PROGRAM_KEYWORDS = {
  { "szaPrefix",     "sza1",  "s", USAGE "CARMA Antenna prefix to control"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

using namespace carma::antenna::common;

int Program::main(void)
{
  std::ostringstream os;
  os << "carma." << Program::getParameter("szaPrefix") << ".RxSelector";

  try {

    carma::antenna::common::RxSelector_var rxSelector = 0;
    rxSelector = getCorbaClient().resolveName<carma::antenna::common::RxSelector>(os.str());

    rxSelector->Rx(carma::antenna::common::RxControl::RX1CM)->
            setFrequency(30.0,0,false,false,false,19);

    sleep(1);
  } catch(carma::util::UserException& err) {
    COUT(err.errorMsg);
  } catch(...) {
    COUT("Caught an unknown error");
  }

  return 0;
}
