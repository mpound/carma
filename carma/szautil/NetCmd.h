#ifndef SZA_UTIL_NETCMD_H
#define SZA_UTIL_NETCMD_H

/**
 * @file NetCmd.h
 * 
 * Tagged: Wed Mar 17 19:42:02 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szaarrayutils/rtcnetcoms.h"

#include "carma/szautil/AntNum.h"
#include "carma/szautil/Percent.h"
#include "carma/szautil/Pressure.h"
#include "carma/szautil/Temperature.h"

namespace sza {
  namespace util {
    
    class NetCmd {
    public:
      
      /**
       * Constructor.
       */
      NetCmd(sza::array::RtcNetCmd rtc, sza::array::NetCmdId opcode);
      NetCmd();
      
      /**
       * Destructor.
       */
      virtual ~NetCmd();

      sza::array::RtcNetCmd rtc_;

      sza::array::NetCmdId opcode_;

      // True if this is an initialization command

      bool init_;

      void packAtmosCmd(Temperature& airTemp, double humidity, double pressure,
			AntNum::Id antennas = AntNum::ANTALL);
 
      void packAtmosCmd(Temperature& airTemp, Percent& humidity, Pressure& pressure,
			AntNum::Id antennas = AntNum::ANTALL);

   }; // End class NetCmd
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_NETCMD_H
