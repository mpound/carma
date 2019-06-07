#ifndef SZA_UTIL_GRABBERNETCMDFORWARDER_H
#define SZA_UTIL_GRABBERNETCMDFORWARDER_H

/**
 * @file GrabberNetCmdForwarder.h
 * 
 * Tagged: Thu Jul  8 18:21:37 UTC 2004
 * 
 * @author 
 */
#include "carma/szautil/NetCmdForwarder.h"

namespace sza {
  namespace util {
    
    class GrabberNetCmdForwarder : public NetCmdForwarder {
    public:
      
      /**
       * Constructor.
       */
      GrabberNetCmdForwarder() {};
      
      /**
       * Destructor.
       */
      virtual ~GrabberNetCmdForwarder() {};
      
      //------------------------------------------------------------
      // Overwrite the base-class method by which all rtc commands are
      // processed
      //------------------------------------------------------------
      
      /**
       * A virtual method to forward a command received from the ACC.
       * Make this virtual so that inheritors can completely redefine
       * what happens with a received command, if they wish.
       */
      virtual void forwardNetCmd(sza::util::NetCmd* netCmd) {};
      
    }; // End class GrabberNetCmdForwarder
    
  } // End namespace util
} // End namespace szza



#endif // End #ifndef SZA_UTIL_GRABBERNETCMDFORWARDER_H
