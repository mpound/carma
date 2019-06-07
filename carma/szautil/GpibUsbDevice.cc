#include "carma/szautil/GpibUsbDevice.h"

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructors
 */
GpibUsbDevice::GpibUsbDevice(bool doSpawn)
{
  address_       = 0;
  controller_    = new GpibUsbController(doSpawn);
  ownController_ = true;
}

GpibUsbDevice::GpibUsbDevice(std::string port, bool doSpawn)
{
  address_       = 0;
  controller_    = new GpibUsbController(port, doSpawn);
  ownController_ = true;
}

GpibUsbDevice::GpibUsbDevice(GpibUsbController& controller)
{
  address_       = 0;
  controller_    = &controller;
  ownController_ = false;
}

/**.......................................................................
 * Destructor.
 */
GpibUsbDevice::~GpibUsbDevice() 
{
  if(controller_ && ownController_) {
    delete controller_;
    controller_ = 0;
  }
}

/**.......................................................................
 * Convenience accessor method to send a device command
 */
void GpibUsbDevice::sendDeviceCommand(std::string cmd, bool expectsResponse, 
				      GPIB_RESPONSE_HANDLER(*handler), bool block, 
				      void* retVal)
{
  controller_->setAddress(address_);

  controller_->sendDeviceCommand(cmd, expectsResponse, handler, block, retVal);

}

/**.......................................................................
 * Convenience accessor method to send a controller command
 */
void GpibUsbDevice::sendControllerCommand(std::string cmd, bool expectsResponse, 
					  GPIB_RESPONSE_HANDLER(*handler), bool block, void* retVal)
{
  controller_->sendControllerCommand(cmd, expectsResponse, handler, block, retVal);
}

/**.......................................................................
 * Just store the address -- don't send to the controller.  This will
 * be used in the overloaded sendDeivceCommand() method
 */
void GpibUsbDevice::setAddress(unsigned address)
{
  address_ = address;
}

/**.......................................................................
 * Return the GPIB network address associated with this device
 */
unsigned GpibUsbDevice::getAddress()
{
  return address_;
}

std::string GpibUsbDevice::getDevice()
{
  std::string retVal;
  sendDeviceCommand("*IDN?", true, GpibUsbController::checkString, true, (void*)&retVal);
  return retVal;
}
