#ifndef SZA_ANTENNA_CORBA_POINTINGMODELPROXY_H
#define SZA_ANTENNA_CORBA_POINTINGMODELPROXY_H

/**
 * @file PointingModelProxy.h
 *
 * Tagged: Thu Nov 13 16:53:47 UTC 2003
 *
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/corba/PointingModel.h"

// Must undef macro SystemException in carma::util::BaseException.h,
// since this causes references to CORBA::SystemException below to be
// misinterpreted

#ifdef SystemException
#undef SystemException
#endif

namespace sza {
  namespace antenna {
    namespace control {
      class AntennaMaster;
    };
  };
};

namespace sza {
  namespace antenna {
    namespace corba {

      /**
       * A class which will be served as the CORBA PointingModel DO,
       * whose methods will send messages to the AntennaMaster message
       * queue.
       */
      class PointingModelProxy : public sza::antenna::corba::PointingModel {

      public:

	/**
	 * Constructor with a pointer to the parent AntennaMaster
	 */
	PointingModelProxy(sza::antenna::control::AntennaMaster* parent);

	/**
	 * Destructor.
	 */
	~PointingModelProxy();

	// CORBA methods

	/**
	 * Set the value of the sequence number to be associated with
	 * the next command.
	 */
	void scheduleNextSequenceNo(unsigned long seq);

	void setTilts(double azTilt1, double azTilt2,
		      double elTilt1, double elTilt2);

	void setFlexure(carma::antenna::common::
			DriveControl::Aperture model, double flexure);

	void setCollimation(carma::antenna::common::
			    DriveControl::Aperture model, double magnitude,
			    double direction);

      private:

	/**
	 * Pointer to the parent task, whose send() methods we will
	 * call.
	 */
	sza::antenna::control::AntennaMaster* parent_;

	/**
	 * The sequence number to be associated with the next command
	 * that requires a sequence number.
	 */
	int seq_;

      }; // End class PointingModelProxy

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif


