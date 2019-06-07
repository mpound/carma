#ifndef SZA_ANTENNA_CORBA_POLARIZATIONPROXY_H
#define SZA_ANTENNA_CORBA_POLARIZATIONPROXY_H

/**
 * @file PolarizationProxy.h
 *
 * Tagged: Thu Nov 13 16:53:48 UTC 2003
 *
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/corba/Proxy.h"

#include "carma/antenna/common/PolarizationControl.h"

namespace sza {
  namespace antenna {
    namespace corba {

      /**
       * A class which will be served as the CORBA Polarization DO,
       * whose methods will send messages to the AntennaMaster message
       * queue.
       */
      class PolarizationProxy : public Proxy {

	public:

	/**
	 * Constructor with a pointer to the parent AntennaMaster
	 */
	PolarizationProxy(sza::antenna::control::AntennaMaster* parent);

	/**
	 * Destructor.
	 */
	~PolarizationProxy();

	virtual void
	  setState(carma::antenna::common::PolarizationControl::State pol,
		   CORBA::ULong seq);

      }; // End class PolarizationProxy

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif


