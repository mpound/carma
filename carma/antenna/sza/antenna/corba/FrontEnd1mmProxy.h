#ifndef SZA_ANTENNA_CORBA_FRONTEND1MMPROXY_H
#define SZA_ANTENNA_CORBA_FRONTEND1MMPROXY_H

/**
 * @file FrontEnd1mmProxy.h
 *
 * Tagged: Thu Nov 11 16:51:19 UTC 2001
 *
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/corba/FrontEndProxy.h"

namespace sza {
  namespace antenna {
    namespace corba {

      /**
       * A class which will be served as the CORBA FrontEnd DO, whose
       * methods will send messages to the AntennaMaster message queue.
       */
      class FrontEnd1mmProxy : public FrontEndProxy {

      public:

	/**
	 * Constructor with a pointer to the parent AntennaMaster
	 */
	FrontEnd1mmProxy(sza::antenna::control::AntennaMaster* parent);

	/**
	 * Destructor.
	 */
	~FrontEnd1mmProxy();

	void setVG(carma::antenna::common::FrontEndControl::Amp amp,
		   carma::antenna::common::FrontEndControl::Stage stage,
		   float voltage);

	void setVD(carma::antenna::common::FrontEndControl::Amp amp,
		   carma::antenna::common::FrontEndControl::Stage stage,
		   float voltage);

	void setID(carma::antenna::common::FrontEndControl::Amp amp,
		   carma::antenna::common::FrontEndControl::Stage stage,
		   float current);

      }; // End class FrontEnd1mmProxy

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif


