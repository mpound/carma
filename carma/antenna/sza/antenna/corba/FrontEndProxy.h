#ifndef SZA_ANTENNA_CORBA_FRONTENDPROXY_H
#define SZA_ANTENNA_CORBA_FRONTENDPROXY_H

/**
 * @file FrontEndProxy.h
 *
 * Tagged: Thu Nov 13 16:53:39 UTC 2003
 *
 * @author Erik Leitch
 */
#include "carma/szautil/Amp.h"

#include "carma/antenna/sza/antenna/corba/Proxy.h"

#include "carma/antenna/common/FrontEndControl.h"

namespace sza {
  namespace antenna {
    namespace corba {

      /**
       * A class which will be served as the CORBA FrontEnd DO, whose
       * methods will send messages to the AntennaMaster message queue.
       */
      class FrontEndProxy : public Proxy {

      public:

	/**
	 * Constructor with a pointer to the parent AntennaMaster
	 */
	FrontEndProxy(sza::antenna::control::AntennaMaster* parent);

	/**
	 * Destructor.
	 */
	~FrontEndProxy();

	//-----------------------------------------------------------------------
	// CORBA methods
	//-----------------------------------------------------------------------

	virtual void setFrequency(double freq);

	virtual void setSISVj(float voltage);

	virtual void setSISIj(float current);

	virtual void doIVcurve();

	virtual void setVG(carma::antenna::common::FrontEndControl::
			   Amp amp,
			   carma::antenna::common::FrontEndControl::
			   Stage stage,
			   float voltage);

	virtual void setVD(carma::antenna::common::FrontEndControl::
			   Amp amp,
			   carma::antenna::common::FrontEndControl::
			   Stage stage,
			   float voltage);

	virtual void setID(carma::antenna::common::FrontEndControl::Amp amp,
			   carma::antenna::common::FrontEndControl::Stage stage,
			   float current);

	virtual void setMixer(float voltage);

	virtual void doIVcurve(float startVjInMv,
			       float stopVjInMv,
			       float stepVjInMv,
			       unsigned short deltaInMs,
			       bool doPower,
			       CORBA::ULong seqNo);

    virtual carma::antenna::common::IVCurve* getIVCurve();



	//-----------------------------------------------------------------------
	// Local methods
	//-----------------------------------------------------------------------

	// This is the main method by which we will set all biases

	void setBias(sza::util::Rx::Id rxId,
		     carma::antenna::common::FrontEndControl::Amp amp,
		     carma::antenna::common::FrontEndControl::Stage stage,
		     sza::util::Rx::Stage szaStage,
		     short bias,
		     unsigned long seq,
		     bool isDefault);

	// This is the main method by which we will set default values for biases

	void setDefaultBias(unsigned iBias, short bias);

	// Return the appropriate bias number for the requested gate
	// voltage of this receiver.

	sza::util::Rx::Stage getVgStage(carma::antenna::common::FrontEndControl::Amp amp,
					carma::antenna::common::FrontEndControl::Stage stage);

	// Return the appropriate bias number for the requested drain
	// voltage of this receiver.

	sza::util::Rx::Stage getVdStage(carma::antenna::common::FrontEndControl::Amp amp,
					carma::antenna::common::FrontEndControl::Stage stage);

	// Return the appropriate bias number for the requested drain
	// current of this receiver.

	sza::util::Rx::Stage getIdStage(carma::antenna::common::FrontEndControl::Amp amp,
					carma::antenna::common::FrontEndControl::Stage stage);

      }; // End class FrontEndProxy

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif


