#ifndef SZA_UTIL_ARRAYNETCMDFORWARDER_H
#define SZA_UTIL_ARRAYNETCMDFORWARDER_H

/**
 * @file ArrayNetCmdForwarder.h
 * 
 * Tagged: Wed Mar 17 19:02:51 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/NetCmdForwarder.h"

#include "carma/szaarrayutils/rtcnetcoms.h"
#include "carma/szautil/NetCmd.h"

namespace sza {
  namespace util {
    
    /**
     * Master class for forwarding message intended for different array
     * subsystems
     */
    class ArrayNetCmdForwarder : public NetCmdForwarder {
    public:
      
      /**
       * Constructor.
       */
      ArrayNetCmdForwarder();
      
      /**
       * Destructor.
       */
      virtual ~ArrayNetCmdForwarder();
          
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

    protected:
      
      //------------------------------------------------------------
      // Objects which will handle message forwarding for each
      // subsystem.  We make these pointers so that they can point to
      // inheritors of subsystem base classes if we wish.  For example,
      // we might have two different types of antenna command
      // forwarders, one for CORBA communications, and one for TCP/IP
      //------------------------------------------------------------

      /**
       * Antenna subsystem
       */
      NetCmdForwarder* antennaForwarder_;

      /** 
       * Control command intended for the translator itself
       */
      NetCmdForwarder* controlForwarder_;

      /** 
       * Downconverter subsystem
       */
      NetCmdForwarder* dcForwarder_;

      /** 
       * Delay subsystem
       */
      NetCmdForwarder* delayForwarder_;

      /** 
       * Frame Grabber subsystem
       */
      NetCmdForwarder* grabberForwarder_;

      /** 
       * Scanner command intended for the translator itself
       */
      NetCmdForwarder* scannerForwarder_;

      /** 
       * Scanner command intended for the antenna power strips
       */
      NetCmdForwarder* stripForwarder_;

      /** 
       * Scanner command intended for the synthesizer
       */
      NetCmdForwarder* synthForwarder_;

    private:

      /**
       * Forward a command to the antenna subsystem
       */
      void forwardAntennaNetCmd(sza::util::NetCmd* netCmd);

      /**
       * Forward a control command
       */
      void forwardControlNetCmd(sza::util::NetCmd* netCmd);

      /**
       * Forward a command to the dc subsystem
       */
      void forwardDcNetCmd(sza::util::NetCmd* netCmd);

      /**
       * Forward a command to the delay subsystem
       */
      void forwardDelayNetCmd(sza::util::NetCmd* netCmd);

      /**
       * Forward a command to the frame grabber subsystem
       */
      void forwardGrabberNetCmd(sza::util::NetCmd* netCmd);

      /**
       * Forward a scanner command
       */
      void forwardScannerNetCmd(sza::util::NetCmd* netCmd);

      /**
       * Forward a strip command
       */
      void forwardStripNetCmd(sza::util::NetCmd* netCmd);

      /**
       * Forward a synth command
       */
      void forwardSynthNetCmd(sza::util::NetCmd* netCmd);

    }; // End class ArrayNetCmdForwarder
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_ARRAYNETCMDFORWARDER_H
