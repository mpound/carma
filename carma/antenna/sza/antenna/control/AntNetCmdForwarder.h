#ifndef SZA_ANTENNA_CONTROL_ANTNETCMDFORWARDER_H
#define SZA_ANTENNA_CONTROL_ANTNETCMDFORWARDER_H

/**
 * @file AntNetCmdForwarder.h
 * 
 * Tagged: Wed Mar 17 19:23:17 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szaarrayutils/rtcnetcoms.h"
#include "carma/szautil/AntennaNetCmdForwarder.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      class AntennaMaster;

      class AntNetCmdForwarder : public sza::util::AntennaNetCmdForwarder {
      public:
	
	/**
	 * Constructor.
	 */
	AntNetCmdForwarder(AntennaMaster* parent);
	
	/**
	 * Destructor.
	 */
	virtual ~AntNetCmdForwarder();

    private:

      /**
       * The parent resources.
       */
	AntennaMaster* parent_;

      //------------------------------------------------------------
      // Methods by which individual rtc commands are forwarded
      //------------------------------------------------------------

      /**
       * Forward a Tracker command.
       */
      void forwardTrackerNetCmd(sza::util::NetCmd* netCmd);
      
      /**
       * Forward a Rx command.
       */
      void forwardRxNetCmd(sza::util::NetCmd* netCmd);

      /**
       * Forward a umac control net command
       */
      void forwardUmacControlNetCmd(sza::util::NetCmd* netCmd);

      }; // End class AntNetCmdForwarder
      
    } // End namespace control
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CONTROL_ANTNETCMDFORWARDER_H
