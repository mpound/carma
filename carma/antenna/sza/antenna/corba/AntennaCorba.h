#ifndef SZA_ANTENNA_CORBA_ANTENNACORBA_H
#define SZA_ANTENNA_CORBA_ANTENNACORBA_H
/**
 * @file AntennaCorba.h
 * 
 * Tagged: Wed Mar 17 22:52:52 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Directives.h"
#include "carma/szautil/GenericTask.h"

#if DIR_USE_ANT_CORBA
#include "carma/antenna/sza/antenna/corba/Corba.h"
#include <orbsvcs/CosNotifyChannelAdminC.h>
#endif

#include "carma/antenna/sza/antenna/control/AntennaControlMsg.h"
#include "carma/antenna/sza/antenna/control/SzaTask.h"

#include "carma/antenna/sza/antenna/corba/CarmaMonitorPointHandler.h"

namespace carma {
  namespace corba {
    class Server;
  }
}

namespace sza {
  namespace antenna {
    namespace control {
      class AntennaControl;
    }
  }
}

namespace sza {
  namespace antenna {
    namespace corba {

      class AntennaProxy;
      class SzaShareCorba;

      class AntennaCorba :
      public sza::antenna::control::SzaTask,
	public sza::util::
	GenericTask<sza::antenna::control::AntennaControlMsg> {
	  
      public:
	  
	  /**
	   * Constructor.
	   */
	  AntennaCorba(sza::antenna::control::AntennaControl* parent);
	  
	  /**
	   * Destructor.
	   */
	  virtual ~AntennaCorba();
	
#if DIR_USE_ANT_CORBA
	  /**
	   * This object's run method.
	   */
	  void run();
#endif

	  void writeCarmaSeqNo(unsigned long seq, 
			       sza::util::GenericTaskMsg::CarmaSeqNoType type, 
			       bool success);

	  void writeCarmaMonitorPoints();

	  SzaShareCorba* getShare();

	  unsigned int getCarmaAntennaIndex();

	  sza::util::AntNum* getAnt();

	  void initialize();

	  void initializeAntenna();

	  inline sza::antenna::control::AntennaControl* parent() {
	    return parent_;
	  };

      private:

	  friend class sza::antenna::control::AntennaControl;

	  sza::antenna::control::AntennaControl* parent_;

#if DIR_USE_ANT_CORBA
	  /**
	   * A pointer to the CORBA server started by this task.
	   */
	  carma::corba::Server & server_;

	  /**
	   * An object which will serve the CORBA Antenna DO
	   */
	  AntennaProxy* antennaProxy_;

	  /**
	   * An object which will serve as our entry point into the
	   * antenna monitor system
	   */
	  CarmaMonitorPointHandler* monitorPointHandler_;

	  /**
	   * Initialize the ORB, but don't activate the POA
	   *
	   * @throws Exception
	   */
	  void initOrb();
	
	  /**
	   * Shutdown this server process ORB
	   */
	  void shutdownORB(bool deactivate, bool etherealize, bool wait);
#endif
	}; // End class AntennaCorba
      
    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_ANTENNACORBA_H
