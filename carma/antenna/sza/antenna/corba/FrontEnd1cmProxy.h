#ifndef SZA_ANTENNA_CORBA_FRONTEND1CMPROXY_H
#define SZA_ANTENNA_CORBA_FRONTEND1CMPROXY_H

/**
 * @file FrontEnd1cmProxy.h
 *
 * Tagged: Thu Nov 13 16:53:39 UTC 2003
 *
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/corba/FrontEndProxy.h"

namespace sza {
  namespace antenna {
    namespace corba {

      /**
       * A class which will be served as the CORBA FrontEnd DO, whose
       * methods will send messages to the AntennaMaster message
       * queue.
       */
      class FrontEnd1cmProxy : public FrontEndProxy {

      public:

	/**
	 * Constructor with a pointer to the parent AntennaMaster
	 */
	FrontEnd1cmProxy(sza::antenna::control::AntennaMaster* parent);

	/**
	 * Destructor.
	 */
	~FrontEnd1cmProxy();

	//-----------------------------------------------------------------------
	// CORBA methods
	//-----------------------------------------------------------------------

	void setVG(carma::antenna::common::FrontEndControl::Amp amp,
		   carma::antenna::common::FrontEndControl::Stage stage,
		   float voltage);

	void setVD(carma::antenna::common::FrontEndControl::Amp amp,
		   carma::antenna::common::FrontEndControl::Stage stage,
		   float voltage);

	void setID(carma::antenna::common::FrontEndControl::Amp amp,
		   carma::antenna::common::FrontEndControl::Stage stage,
		   float current);

	//-----------------------------------------------------------------------
	// Local methods
	//-----------------------------------------------------------------------

	// Return the appropriate bias number for the requested gate
	// voltage of this receiver.

	sza::util::Rx::Stage getVgStage(carma::antenna::common::FrontEndControl::Amp amp,
					carma::antenna::common::FrontEndControl::Stage stage);

	// Return the appropriate bias number for the requested drain
	// current of this receiver.

	sza::util::Rx::Stage getIdStage(carma::antenna::common::FrontEndControl::Amp amp,
					carma::antenna::common::FrontEndControl::Stage stage);

      }; // End class FrontEnd1cmProxy

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif


