#ifndef SZA_UTIL_DELAYNETCMDFORWARDER_H
#define SZA_UTIL_DELAYNETCMDFORWARDER_H

/**
 * @file DelayNetCmdForwarder.h
 * 
 * Tagged: Sun May 16 12:53:30 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/NetCmdForwarder.h"

namespace sza {
  namespace util {
    
    class DelayNetCmdForwarder : public NetCmdForwarder {
    public:
      
      /**
       * Constructor.
       */
      DelayNetCmdForwarder();
      
      /**
       * Destructor.
       */
      virtual ~DelayNetCmdForwarder();
      
      //------------------------------------------------------------
      // Overwrite the base-class method by which all rtc commands are
      // processed
      //------------------------------------------------------------
      
      /**
       * A virtual method to forward a command received from the ACC.
       * Make this virtual so that inheritors can completely redefine
       * what happens with a received command, if they wish.
       */
      virtual void forwardNetCmd(sza::util::NetCmd* netCmd);
      
    }; // End class DelayNetCmdForwarder
    
  } // End namespace util
} // End namespace sza

#endif // End #ifndef SZA_UTIL_DELAYNETCMDFORWARDER_H
