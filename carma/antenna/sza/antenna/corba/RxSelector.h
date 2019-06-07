// $Id: RxSelector.h,v 1.5 2013/08/20 21:56:45 eml Exp $

#ifndef SZA_ANTENNA_CORBA_RXSELECTOR_H
#define SZA_ANTENNA_CORBA_RXSELECTOR_H

/**
 * @file RxSelector.h
 *
 * Tagged: Fri Jul 24 10:57:18 PDT 2009
 *
 * @version: $Revision: 1.5 $, $Date: 2013/08/20 21:56:45 $
 *
 * @author username: Command not found.
 */
#include "carma/antenna/sza/antenna/corba/Corba.h"

#include "carma/antenna/common/RxSelector.h"

#include "carma/antenna/sza/control/szaRxControl.h"

namespace carma {
  namespace corba {
    class Server;
  }
}

namespace sza {
  namespace antenna {
    namespace control {
      class AntennaMaster;
    };
  };
};

#include "carma/szautil/Rx.h"

#include "carma/antenna/sza/antenna/corba/Proxy.h"

namespace sza {
  namespace antenna {
    namespace corba {

      /**
       * Incomplete specifications lets us use classes without
       * defining them
       */
      class RxProxy;
      class Rx1cmProxy;
      class Rx3mmProxy;
      class Rx1mmProxy;

      class RxSelector : public Proxy {

      public:

	/**
	 * Constructor.
	 */
	RxSelector(sza::antenna::control::AntennaMaster* parent);

	/**
	 * Destructor.
	 */
	virtual ~RxSelector();

	/**
	 * Rx bits that will be served up by the Antenna object via
	 * CORBA.
	 */
	carma::antenna::common::RxControl_ptr
	  Rx(carma::antenna::common::RxControl::Type type);

	// Register this object with the corba::Server class

	void registerObject(const std::string& name, 
			    carma::corba::Server& server);

	sza::antenna::corba::RxProxy* getRxProxy(sza::util::Rx::Id rxId = sza::util::Rx::RXALL);

	void selectRx(sza::util::Rx::Id rxId);

      private:

	sza::antenna::control::AntennaMaster* parent_;

	/**
	 * Receiver subsystem served up by this object.
	 */
	Rx1cmProxy* rx1cm_;
	Rx3mmProxy* rx3mm_;
	Rx1mmProxy* rx1mm_;
      
	/**
	 * Receiver DO pointers.  
	 */
	carma::antenna::sza::control::RxControl_ptr rx1cmPtr_;
	carma::antenna::sza::control::RxControl_ptr rx1mmPtr_;
	carma::antenna::sza::control::RxControl_ptr rx3mmPtr_;

      }; // End class RxSelector

    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_RXSELECTOR_H
