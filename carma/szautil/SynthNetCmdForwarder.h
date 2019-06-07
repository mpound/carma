#ifndef SZA_UTIL_SYNTHNETCMDFORWARDER_H
#define SZA_UTIL_SYNTHNETCMDFORWARDER_H

/**
 * @file SynthNetCmdForwarder.h
 * 
 * Tagged: Wed Jul 21 04:04:30 UTC 2004
 * 
 * @author 
 */
#include "carma/szautil/NetCmdForwarder.h"

namespace sza {
  namespace util {
    
    class SynthNetCmdForwarder : public NetCmdForwarder {
    public:
      
      /**
       * Constructor.
       */
      SynthNetCmdForwarder() {};
      
      /**
       * Destructor.
       */
      virtual ~SynthNetCmdForwarder() {};
      
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
    }; // End class SynthNetCmdForwarder
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_SYNTHNETCMDFORWARDER_H
