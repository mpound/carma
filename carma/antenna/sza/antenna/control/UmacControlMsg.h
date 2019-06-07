#ifndef SZA_ANTENNA_CONTROL_UMACCONTROLMSG_H
#define SZA_ANTENNA_CONTROL_UMACCONTROLMSG_H

/**
 * @file UmacControlMsg.h
 * 
 * Tagged: Mon Jul 19 11:41:59 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/GenericTaskMsg.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      class UmacControlMsg :
	public sza::util::GenericTaskMsg {
	
	public:
	
	/**
	 * Enumerate supported UmacControl messages
	 */
	enum MsgType {
	  POWER,  // Cycle power to the named device/circuit breaker
	  RESET   // Reset the RPC module
	};
	
	/**
	 * The type of this message
	 */
	MsgType type;
	
	/**
	 * Define a Message container
	 */
	union {
	  
	  /**------------------------------------------------------------
	   * Flag a board.
	   */
	  struct {
	    unsigned breaker; // The register map index of the
	    bool power;             // True to turn power on, false to
				    // turn power off.
	  } power;
	} body;
	
	inline void packPowerMsg(unsigned breaker, bool power)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type = POWER;
	    body.power.breaker = breaker;
	    body.power.power = power;
	  }
	
      }; // End class UmacControlMsg
      
    } // End namespace control
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CONTROL_UMACCONTROLMSG_H
