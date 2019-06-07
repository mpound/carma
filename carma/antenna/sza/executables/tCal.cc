#include <iostream>

#include "carma/corba/Client.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedPthreadMutexLock.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/Program.h"
#include "carma/szautil/Thread.h"

#include "carma/antenna/sza/antenna/corba/DriveProxy.h"
#include "carma/antenna/sza/antenna/corba/RxProxy.h"
#include "carma/antenna/sza/antenna/corba/RxSelector.h"

#include "carma/antenna/common/RxControl.h"
#include "carma/antenna/sza/control/szaCalibratorControl.h"

using namespace std;
using namespace carma::util;
using namespace sza::util;

PROGRAM_KEYWORDS = {
  { "szaPrefix",     "sza1",  "s", USAGE "CARMA Antenna prefix to control"},
  { "thome",         "f",     "b", USAGE "True to test tertiary home command"},
  { "ttertposangle", "f",     "b", USAGE "True to test tertiary position angle command"},
  { "ttertposrx",    "f",     "b", USAGE "True to test tertiary position rx command"},
  { "tcal",          "f",     "b", USAGE "True to test cal position command"},
  { "calpos",        "sky",   "s", USAGE "Calibrator position to move to"},
  { "angle",         "0",     "d", USAGE "Angle (in degrees) to move to"},
  { "rx",            "rx1cm", "s", USAGE "Rx to move to "},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

using namespace carma::antenna::common;

carma::antenna::common::RxControl::Type rxStrToRxType(std::string rxStr);

static THREAD_START(testFn)
{
  carma::util::PthreadMutex* guard = (carma::util::PthreadMutex*)arg;

  CTOUT("About to lock mutex (1)");

  ScopedPthreadMutexLock spml(*guard);

  CTOUT("Blocking forever...");

  while(true) {
  }

  CTOUT("Exiting thread function");
}

int Program::main(void)
{

  try {
#if 1

    carma::util::PthreadMutex guard;

    pthread_t id;
    pthread_create(&id, NULL, &testFn, (void*)&guard);

    sleep(5);

    CTOUT("About to lock mutex (2)");
    ScopedPthreadMutexLock spml(guard);

    sleep(5);
#else
  
  //------------------------------------------------------------
  // Now do something with it
  //------------------------------------------------------------

  std::ostringstream os;
  os << "carma." << Program::getParameter("szaPrefix") << ".Calibrator";

  try {

    carma::antenna::sza::control::CalibratorControl_var calibrator = 0;

    calibrator = 
      getCorbaClient().
      resolveName<carma::antenna::sza::control::CalibratorControl>(os.str());

    //------------------------------------------------------------
    // Test the home command
    //------------------------------------------------------------

    if(Program::getBoolParameter("thome")) {
      calibrator->homeTertiary();
    }

    //------------------------------------------------------------
    // Test the tertiary position angle command
    //------------------------------------------------------------

    if(Program::getBoolParameter("ttertposangle")) {
      double degrees = Program::getDoubleParameter("angle");
      calibrator->positionTertiaryAngle(degrees);
    }

    //------------------------------------------------------------
    // Test the tertiary position rx command
    //------------------------------------------------------------

    if(Program::getBoolParameter("ttertposrx")) {
      std::string rxStr = Program::getParameter("rx");
      carma::antenna::common::RxControl::Type rxType = rxStrToRxType(rxStr);
      calibrator->positionTertiaryRx(rxType);
    }

    //------------------------------------------------------------
    // Test the calibrator position command
    //------------------------------------------------------------

    if(Program::getBoolParameter("tcal")) {

      std::string posStr = Program::getParameter("calpos");

      carma::antenna::common::CalibratorControl::Position pos;

      if(posStr == "sky")
	pos = carma::antenna::common::CalibratorControl::SKY;
      else if(posStr == "load")
	pos = carma::antenna::common::CalibratorControl::AMBIENT;
      else {
	ThrowError("Unrecognized calibrator position  (should be sky or load): " 
		   << posStr)
	  }
      
      calibrator->setPos(pos, 19);
    }

    sleep(1);
  } catch(carma::util::UserException& err) {
    COUT(err.errorMsg);
  } catch(...) {
    COUT("Caught an unknown error");
  }

#endif
  } catch(carma::util::UserException& err) {
    COUT("User: " << err.errorMsg);
  } catch(carma::util::ErrorException& err) {
    COUT("Error: " << err.what());
  } catch(sza::util::Exception& err) {
    COUT("Sza: " << err.what());
  } catch(...) {
    COUT("Caught an error");
  }
  return 0;
}

/**.......................................................................
 * Convert from rx string to rx type
 */
carma::antenna::common::RxControl::Type rxStrToRxType(std::string rxStr)
{
  if(rxStr == "rx1cm") {
    return carma::antenna::common::RxControl::RX1CM;
  } else if(rxStr == "rx3mm") {
    return carma::antenna::common::RxControl::RX3MM;
  } else if(rxStr == "rx1mm") {
    return carma::antenna::common::RxControl::RX1MM;
  } else {
    ThrowError("Unrecognized rx string: " << rxStr << " (should be rx1cm|rx3mm|rx1mm)");
  }
}
