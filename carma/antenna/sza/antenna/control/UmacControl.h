#ifndef SZA_ANTENNA_CONTROL_UMACCONTROL_H
#define SZA_ANTENNA_CONTROL_UMACCONTROL_H

/**
 * @file UmacControl.h
 * 
 * Tagged: Mon Jul 19 11:34:40 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <string>
#include <list>

#include "carma/antenna/sza/antenna/control/UmacControlMsg.h"
#include "carma/antenna/sza/antenna/control/SzaTask.h"

#include "carma/szautil/GenericTask.h"
#include "carma/szautil/SerialClient.h"
#include "carma/szautil/TimeVal.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      /**
       * Incomplete type specification for AntennaMaster lets us
       * declare it as a friend below without defining it
       */
      class AntennaMaster;
      
      /**
       * AntennaRx class will handle all receiver functions.  This class
       * instantiates proxy objects for all receiver subsystems, which
       * can be served as CORBA distributed objects.
       */
      class UmacControl :
	public SzaTask,
	public sza::util::GenericTask<UmacControlMsg> {
	
	public:
	
	/**
	 * Constructor.
	 */
	UmacControl(AntennaMaster* parent);
	
	/**
	 * Destructor.
	 */
	virtual ~UmacControl();
	
	private:
	
	friend class AntennaMaster;

	AntennaMaster* parent_;

	sza::util::SerialClient* client_;
	sza::util::TimeVal timer_;
	struct timeval* timeOut_;
	
	/**
	 * A stack of command/responses for communicating with the
	 * strip
	 */
	std::list<std::string> sentStrings_;
	std::list<std::string> rcvdStrings_;
	
	/**
	 * A pointer to the current element of the above lists
	 */
	std::list<std::string>::iterator sentStringIter_;
	std::list<std::string>::iterator rcvdStringIter_;
	
	/**
	 * Set up the client to attach to the power strip telnet port
	 * for this antenna.
	 */
	void initSerialConnection() ;
	
	/**
	 * Initiate sending commands to the power strip.
	 */
	void initiateCommSequence(unsigned breaker, bool power);
	
	/**
	 * Terminate a command sequence to the power strip.
	 */
	void terminateCommSequence();
	
	/**
	 * Compile the state machine we will use for communicating with the
	 * strip
	 */
	void compileCommandStateMachine(unsigned breaker,
					bool power);
	
	/**
	 * Set a timeout for waiting for a response from the strip.
	 */
	void enableTimeOut(bool enable);
	
	/**
	 * Read a line from the power strip and determine what to do
	 */
	void processClientMessage();
	
	/**
	 * React to a failure on the part of the power strip to reply
	 */
	void registerTimeOut();

	/**
	 * Overwrite the base-class method.
	 */
	void serviceMsgQ();
	
	/**
	 * Overwrite the base-class method.
	 */
	void processMsg(UmacControlMsg* msg);
	
	
      }; // End class UmacControl
      
    } // End namespace control
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CONTROL_UMACCONTROL_H
