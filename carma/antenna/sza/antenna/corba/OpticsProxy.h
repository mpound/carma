#ifndef SZA_ANTENNA_CORBA_OPTICSPROXY_H
#define SZA_ANTENNA_CORBA_OPTICSPROXY_H

/**
 * @file OpticsProxy.h
 *
 * Tagged: Thu Nov 13 16:53:42 UTC 2003
 *
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/corba/Proxy.h"

#include "carma/antenna/common/OpticsControl.h"

namespace sza {
  namespace antenna {
    namespace corba {

      /**
       * A class which will be served as the CORBA Optics DO, whose
       * methods will send messages to the AntennaMaster message
       * queue.
       */
      class OpticsProxy : public Proxy {

      public:

	  /**
	   * Constructor with a pointer to the parent AntennaMaster
	   */
	  OpticsProxy(sza::antenna::control::AntennaMaster* parent);

	  /**
	   * Destructor.
	   */
	  ~OpticsProxy();

	  virtual void selectRx();

	}; // End class OpticsProxy

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif


