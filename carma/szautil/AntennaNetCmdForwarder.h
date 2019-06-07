#ifndef SZA_UTIL_ANTENNANETCMDFORWARDER_H
#define SZA_UTIL_ANTENNANETCMDFORWARDER_H

/**
 * @file AntennaNetCmdForwarder.h
 * 
 * Tagged: Sun May 16 12:27:04 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/NetCmdForwarder.h"

namespace sza {
  namespace util {
    
    class AntennaNetCmdForwarder : public NetCmdForwarder {
    public:
      
      /**
       * Constructor.
       */
      AntennaNetCmdForwarder();
      
      /**
       * Destructor.
       */
      virtual ~AntennaNetCmdForwarder();
      
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

    protected:

      /**
       * Forward a control command 
       */
      virtual void forwardControlNetCmd(sza::util::NetCmd* netCmd);

      /**
       * Optical Camera commands
       */
      virtual void forwardOpticalCameraNetCmd(sza::util::NetCmd* netCmd);

      /**
       * Thermometry commands
       */
      virtual void forwardThermoNetCmd(sza::util::NetCmd* netCmd);

      /**
       * Channelizer commands
       */
      virtual void forwardChannelizerNetCmd(sza::util::NetCmd* netCmd);

      /**
       * RxSim commands
       */
      virtual void forwardRxSimulatorNetCmd(sza::util::NetCmd* netCmd);

      /**
       * GPIB commands
       */
      virtual void forwardGpibNetCmd(sza::util::NetCmd* netCmd);

      /**
       * Noise cal commands
       */
      virtual void forwardNoiseCalNetCmd(sza::util::NetCmd* netCmd);

      /**
       * Forward a command for the weather station
       */
      virtual void forwardWeatherNetCmd(sza::util::NetCmd* netCmd) ;

      /**
       * Forward a tracker net command
       */
      virtual void forwardTrackerNetCmd(sza::util::NetCmd* netCmd);

      /**
       * Receiver commands
       */
      virtual void forwardRxNetCmd(sza::util::NetCmd* netCmd);

      /**
       * Scanner task commands
       */
      virtual void forwardScannerNetCmd(sza::util::NetCmd* netCmd);

      /**
       * Probe commands
       */
      virtual void forwardProbeNetCmd(sza::util::NetCmd* netCmd);

      /**
       * Board flagging commands
       */
      virtual void forwardBoardNetCmd(sza::util::NetCmd* netCmd);
  
      /**
       * StripControl commands
       */
      virtual void forwardStripControlNetCmd(sza::util::NetCmd* netCmd);
    
    }; // End class AntennaNetCmdForwarder
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_ANTENNANETCMDFORWARDER_H
