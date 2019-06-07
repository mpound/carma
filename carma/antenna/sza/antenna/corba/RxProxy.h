#ifndef SZA_ANTENNA_CORBA_RXPROXY_H
#define SZA_ANTENNA_CORBA_RXPROXY_H

/**
 * @file RxProxy.h
 *
 * Tagged: Thu Nov 13 16:53:50 UTC 2003
 *
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/corba/Proxy.h"

#include "carma/antenna/sza/control/szaRxControl.h"
#include "carma/antenna/sza/control/szaLOControl.h"

#include "carma/szautil/Frequency.h"
#include "carma/szautil/Rx.h"

namespace sza {
  namespace antenna {
    namespace corba {

      /**
       * Incomplete specifications lets us use classes without
       * defining them yet.
       */
      class LOProxy;
      class IFProxy;
      class FrontEndProxy;
      class OpticsProxy;
      class PolarizationProxy;

      /**
       * A class which will be served as the CORBA Rx DO, whose
       * methods will send messages to the AntennaMaster message
       * queue.
       */
      class RxProxy : public Proxy {

      public:

	/**
	 * Constructor with a pointer to the parent.
	 */
	RxProxy(sza::antenna::control::AntennaMaster* parent);

	/**
	 * Destructor.
	 */
	~RxProxy();

	// Return the DO for the named LO subsystem.

	virtual carma::antenna::common::LOControl_ptr
	  LO();

	virtual carma::antenna::common::IFControl_ptr
	  IF( carma::antenna::common::RxControl::IF_Type ifoutpu );

	virtual carma::antenna::common::FrontEndControl_ptr
	  FrontEnd( carma::antenna::common::RxControl::Pol_Type pol=carma::antenna::common::RxControl::LEFTCIRCULAR);

	virtual carma::antenna::common::OpticsControl_ptr Optics();

	virtual carma::antenna::common::PolarizationControl_ptr Polarization();

	// Set up frequencies for this receiver?

	virtual void setFrequency(double yigFreq, double LOFreq,
				  bool endWithAbsorberInBeam,
				  bool optimizeReceiver,
				  CORBA::ULong seq);

	void setFrequency(sza::util::Rx::Id rxId, CORBA::ULong seq);

	virtual void setObservingFrequency(double obsFreq, CORBA::ULong seq);

	virtual void measureTotalPower(carma::antenna::common::CalibratorControl::Position position, CORBA::ULong seq);

	virtual void measureTotalPower(carma::antenna::common::
				       CalibratorControl::Position position);

	virtual void setOffset(double az, double el);

	virtual void toggleFastSampling(CORBA::ULong channel,
					bool start);

	virtual void setIFPresetPower();

	virtual void setIFAtten(CORBA::Float atten,
				carma::antenna::common::RxControl::IF_Type ifType);

	virtual void setIFPower(CORBA::Float power);

	// SZA-specific methods

	virtual void resetCanModule(const char* moduleName);
	virtual void resetCanBus();

	// End IDL interface

	sza::antenna::corba::LOProxy* getLOProxy();
	sza::antenna::corba::IFProxy* getIFProxy();
	sza::antenna::corba::FrontEndProxy* getFrontEndProxy();

	void storeEncoderPosition(sza::util::Rx::Id rxId, short position);

	void setDefaultYigFrequency(sza::util::Rx::Id rxId, sza::util::Frequency freq);
	void setDefaultGunnFrequency(sza::util::Rx::Id rxId, sza::util::Frequency freq);

      protected:

	/**
	 * Object which we will serve as the CORBA IF subsystem DO.
	 */
	IFProxy* ifSys_;
	carma::antenna::common::IFControl_ptr ifPtr_; // To return via IF() accessor

	/**
	 * Object which we will serve as the CORBA LO subsystem DO.
	 */
	LOProxy* lo_;
	carma::antenna::sza::control::LOControl_ptr loPtr_; // To return via LO() accessor

	/**
	 * Object which we will serve as the CORBA FrontEnd subsystem DO.
	 */
	FrontEndProxy* frontEnd_;
	carma::antenna::common::FrontEndControl_ptr frontEndPtr_; // For frontEnd()

	/**
	 * Object which we will serve as the CORBA Optics subsystem DO.
	 */
	OpticsProxy* optics_;
	carma::antenna::common::OpticsControl_ptr opticsPtr_; // For optics()

	/**
	 * Object which we will serve as the CORBA Polarization
	 * subsystem DO.
	 */
	PolarizationProxy* polarization_;
	carma::antenna::common::PolarizationControl_ptr polarizationPtr_; 

      }; // End class RxProxy

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif


