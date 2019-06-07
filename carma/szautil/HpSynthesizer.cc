#include "carma/szautil/HpSynthesizer.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructors
 */
HpSynthesizer::HpSynthesizer(bool doSpawn) : GpibUsbDevice(doSpawn) {}
HpSynthesizer::HpSynthesizer(std::string port, bool doSpawn) : GpibUsbDevice(port, doSpawn) {}
HpSynthesizer::HpSynthesizer(GpibUsbController& controller) : GpibUsbDevice(controller) {}

/**.......................................................................
 * Destructor.
 */
HpSynthesizer::~HpSynthesizer() {}

/**.......................................................................
 * Set the power
 */
Power HpSynthesizer::setOutputPower(Power pow)
{
  Power retVal;

  std::ostringstream os;
  os << ":POW " << pow.getdBm() << "dBm" << ";*WAI;:POW?";
  sendDeviceCommand(os.str(), true, checkPower, true, (void*)&retVal);

  return retVal;
}

/**.......................................................................
 * Get the power
 */
Power HpSynthesizer::getOutputPower()
{
  Power retVal;
  sendDeviceCommand(":POW?", true, checkPower, true, (void*)&retVal);
  COUT("Got retVal = " << retVal.getdBm());
  return retVal;
}

GPIB_RESPONSE_HANDLER(HpSynthesizer::checkPower)
{
  GpibUsbController* gpib = (GpibUsbController*)arg;
  
  Power* retVal = (Power*)gpib->retVal_;

  if(retVal) {
    retVal->setdBm(atof(gpib->response_.str().c_str()));
  }

  return true;
}

/**.......................................................................
 * Set the frequency
 */
Frequency HpSynthesizer::setFrequency(Frequency freq)
{
  Frequency retVal;

  std::ostringstream os;
  os << ":FREQ:CW " << freq.MHz() << "MHz"  << ";*WAI;:FREQ:CW?";
  sendDeviceCommand(os.str(), true, checkFrequency, true, (void*)&retVal);

  return retVal;
}

/**.......................................................................
 * Get the frequency
 */
Frequency HpSynthesizer::getFrequency()
{
  Frequency retVal;
  sendDeviceCommand(":FREQ:CW?", true, checkFrequency, true, (void*)&retVal);
  COUT("Got retVal = " << retVal.MHz());
  return retVal;
}

GPIB_RESPONSE_HANDLER(HpSynthesizer::checkFrequency)
{
  GpibUsbController* gpib = (GpibUsbController*)arg;
  
  Frequency* retVal = (Frequency*)gpib->retVal_;

  if(retVal) {
    retVal->setHz(atof(gpib->response_.str().c_str()));
  }

  return true;
}

/**.......................................................................
 * Enable RF output from the synthesizer
 */
bool HpSynthesizer::enableRfOutput(bool enable)
{
  bool retVal;

  if(enable) 
    sendDeviceCommand(":OUTP ON;*WAI;:OUTP:STAT?", true, checkRfOutput, true, &retVal); 
  else
    sendDeviceCommand(":OUTP OFF;*WAI;:OUTP:STAT?", true, checkRfOutput, true, &retVal); 

  return retVal;
}

/**.......................................................................
 * Query if RF output from the synthesizer is enabled
 */
bool HpSynthesizer::queryRfOutputEnabled()
{
  bool retVal;
  sendDeviceCommand(":OUTP:STAT?", true, checkRfOutput, true, &retVal); 
  return retVal;
}

/**.......................................................................
 * Enable modulation of the RF output from the synthesizer
 */
bool HpSynthesizer::enableOutputMod(bool enable)
{
  bool retVal;

  if(enable) 
    sendDeviceCommand(":OUTP:MOD ON;*WAI;:OUTP:MOD:STAT?", true, checkOutputMod, true, &retVal); 
  else
    sendDeviceCommand(":OUTP:MOD OFF;*WAI;:OUTP:MOD:STAT?", true, checkOutputMod, true, &retVal); 

  return retVal;
}

/**.......................................................................
 * Query if modulation of the RF output from the synthesizer is
 * enabled
 */
bool HpSynthesizer::queryOutputModEnabled()
{
  bool retVal;
  sendDeviceCommand(":OUTP:MOD:STAT?", true, checkOutputMod, true, &retVal); 
  return retVal;
}

GPIB_RESPONSE_HANDLER(HpSynthesizer::checkRfOutput)
{
  GpibUsbController* gpib = (GpibUsbController*)arg;
  
  bool* retVal = (bool*)gpib->retVal_;

  if(retVal) {
    *retVal = atoi(gpib->response_.str().c_str());
  }

  return true;
}

GPIB_RESPONSE_HANDLER(HpSynthesizer::checkOutputMod)
{
  GpibUsbController* gpib = (GpibUsbController*)arg;
  
  bool* retVal = (bool*)gpib->retVal_;

  if(retVal) {
    *retVal = atoi(gpib->response_.str().c_str());
  }

  return true;
}
