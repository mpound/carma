#include "carma/szautil/HpDcPowerSupply.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructors
 */
HpDcPowerSupply::HpDcPowerSupply(bool doSpawn) : GpibUsbDevice(doSpawn) {}
HpDcPowerSupply::HpDcPowerSupply(std::string port, bool doSpawn) : GpibUsbDevice(port, doSpawn) {}

/**.......................................................................
 * Destructor.
 */
HpDcPowerSupply::~HpDcPowerSupply() {}

/**.......................................................................
 * Set the voltage
 */
double HpDcPowerSupply::setVoltage(double volts)
{
  double retVal;
  std::ostringstream os;
  os << "VOLT " << volts << ";*WAI;VOLT?";
  sendDeviceCommand(os.str(), true, checkVoltage, true, (void*)&retVal);
  return retVal;
}

/**.......................................................................
 * Get the voltage
 */
double HpDcPowerSupply::getVoltage()
{
  double retVal;
  sendDeviceCommand("VOLT?", true, checkVoltage, true, (void*)&retVal);
  return retVal;
}

GPIB_RESPONSE_HANDLER(HpDcPowerSupply::checkVoltage)
{
  GpibUsbController* gpib = (GpibUsbController*)arg;
  
  double* retVal = (double*)gpib->retVal_;

  if(retVal) {
    *retVal = atof(gpib->response_.str().c_str());
  }

  return true;
}
