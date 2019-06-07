#ifndef SZA_UTIL_STRIPNETCMDFORWARDER_H
#define SZA_UTIL_STRIPNETCMDFORWARDER_H

/**
 * @file StripNetCmdForwarder.h
 * 
 * Tagged: Wed Jul 21 04:04:30 UTC 2004
 * 
 * @author 
 */
#include "carma/szautil/NetCmdForwarder.h"

namespace sza {
  namespace util {
    
    class StripNetCmdForwarder : public NetCmdForwarder {
    public:
      
      /**
       * Constructor.
       */
      StripNetCmdForwarder() {};
      
      /**
       * Destructor.
       */
      virtual ~StripNetCmdForwarder() {};
      
      //------------------------------------------------------------
      // Overwrite the base-class method by which all rtc commands for
      // the antennas are processed
      //------------------------------------------------------------
      
      /**
       * A virtual method to forward a command received from the ACC.
       * Make this virtual so that inheritors can completely redefine
       * what happens with a received command, if they wish.
       */
      virtual void forwardNetCmd(sza::util::NetCmd* netCmd) {};

    private:
    }; // End class StripNetCmdForwarder
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_STRIPNETCMDFORWARDER_H
