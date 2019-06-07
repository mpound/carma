#ifndef SZA_UTIL_DCNETCMDFORWARDER_H
#define SZA_UTIL_DCNETCMDFORWARDER_H

/**
 * @file DcNetCmdForwarder.h
 * 
 * Tagged: Sun May 16 12:55:56 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/NetCmdForwarder.h"

namespace sza {
  namespace util {
    
    class DcNetCmdForwarder : public NetCmdForwarder {
    public:
      
      /**
       * Constructor.
       */
      DcNetCmdForwarder();
      
      /**
       * Destructor.
       */
      virtual ~DcNetCmdForwarder();
      
      //------------------------------------------------------------
      // Overwrite the base-class method by which all rtc commands for
      // the antennas are processed
      //------------------------------------------------------------
      
      /**
       * A virtual method to forward a command received from the ACC.
       * Make this virtual so that inheritors can completely redefine
       * what happens with a received command, if they wish.
       */
      virtual void forwardNetCmd(sza::util::NetCmd* netCmd);

    }; // End class DcNetCmdForwarder
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_DCNETCMDFORWARDER_H
