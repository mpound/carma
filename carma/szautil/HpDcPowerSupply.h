#ifndef SZA_UTIL_HPDCPOWERSUPPLY_H
#define SZA_UTIL_HPDCPOWERSUPPLY_H

/**
 * @file HpDcPowerSupply.h
 * 
 * Tagged: Thu Oct 18 15:49:12 PDT 2007
 * 
 * @author SZA data acquisition
 */
#include "carma/szautil/Frequency.h"
#include "carma/szautil/GpibUsbDevice.h"
#include "carma/szautil/Power.h"

namespace sza {
  namespace util {
    
    class HpDcPowerSupply : public GpibUsbDevice {
    public:
      
      /**
       * Constructor.
       */
      HpDcPowerSupply(bool doSpawn=false);
      HpDcPowerSupply(std::string port, bool doSpawn=false);
      
      /**
       * Destructor.
       */
      virtual ~HpDcPowerSupply();

      //------------------------------------------------------------
      // Device commands
      //------------------------------------------------------------

      double setVoltage(double voltage);
      double getVoltage();

    private:
      
      static GPIB_RESPONSE_HANDLER(checkVoltage);

    }; // End class HpDcPowerSupply
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_HPDCPOWERSUPPLY_H
