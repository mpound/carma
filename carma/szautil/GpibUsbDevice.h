// $Id: GpibUsbDevice.h,v 1.1 2010/12/13 21:06:30 eml Exp $

#ifndef SZA_UTIL_GPIBUSBDEVICE_H
#define SZA_UTIL_GPIBUSBDEVICE_H

/**
 * @file GpibUsbDevice.h
 * 
 * Tagged: Tue Aug 11 14:04:19 PDT 2009
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:30 $
 * 
 * @author tcsh: username: Command not found.
 */
#include "carma/szautil/GpibUsbController.h"

#include <string>

namespace sza {
  namespace util {

    class GpibUsbDevice {
    public:

      // Constructor.

      GpibUsbDevice(bool doSpawn=false);
      GpibUsbDevice(std::string port, bool doSpawn=false);
      GpibUsbDevice(GpibUsbController& controller);

      // Destructor.

      virtual ~GpibUsbDevice();

      // Set the address for this device

      void setAddress(unsigned address);
      unsigned  getAddress();

      // Convenience accessor methods to the controller

      void sendDeviceCommand(std::string cmd, bool expectsResponse=false, 
			     GPIB_RESPONSE_HANDLER(*handler)=0, bool block=false, 
			     void* retVal=0);

      void sendControllerCommand(std::string cmd, bool expectsResponse=false, 
				 GPIB_RESPONSE_HANDLER(*handler)=0, bool block=false, void* retVal=0);

      std::string getDevice();

      GpibUsbController* controller() {
	return controller_;
      }

    private:

      unsigned address_;
      GpibUsbController* controller_;
      bool ownController_;

    }; // End class GpibUsbDevice

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_GPIBUSBDEVICE_H
