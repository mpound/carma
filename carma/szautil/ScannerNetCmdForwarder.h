#ifndef SZA_UTIL_SCANNERNETCMDFORWARDER_H
#define SZA_UTIL_SCANNERNETCMDFORWARDER_H

/**
 * @file ScannerNetCmdForwarder.h
 * 
 * Tagged: Thu Jul  8 17:47:42 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/NetCmdForwarder.h"

namespace sza {
  namespace util {
    
    class ScannerNetCmdForwarder : public NetCmdForwarder {
    public:
      
      /**
       * Constructor.
       */
      ScannerNetCmdForwarder() {};
      
      /**
       * Destructor.
       */
      virtual ~ScannerNetCmdForwarder() {};
      
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
    }; // End class ScannerNetCmdForwarder
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_SCANNERNETCMDFORWARDER_H
