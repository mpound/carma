/**
 * @file
 * BIMA CryoControl implementation.
 *
 * @author Colby Gutierrez-Kraybill
 *
 * $Revision: 1.15 $
 * $Id: FrontEndControlImpl.h,v 1.15 2012/02/21 21:06:58 abeard Exp $
 */


#ifndef CARMA_ANTENNA_BIMA_FRONTENDCONTROLIMPL_H
#define CARMA_ANTENNA_BIMA_FRONTENDCONTROLIMPL_H

#include "carma/corba/corba.h"
#include "carma/antenna/common/FrontEndControl.h"
#include "carma/antenna/bima/RxClient.h"

namespace log4cpp {
  class Category;
}

namespace carma
{
  namespace antenna
  {
    namespace bima
    {

    /**
     * CORBA control implementation for the FrontEndControl interface.
     */
    class FrontEndControlImpl :
      public RxClient
    {
    public:

        /**
         * Constructor.
         */
        FrontEndControlImpl
	  (
	   Configuration &config
	  );

        /**
         * Destructor.
         */
        ~FrontEndControlImpl();

        // CORBA control methods are declared private to assure that they
        // are only called by CORBA.
        void setFrequency(double freq);

        void setSISVj(float voltage);

        void setSISIj(float current);

        void doIVcurve(
            ::CORBA::Float startVjInMv,
            ::CORBA::Float stopVjInMv,
            ::CORBA::Float stepVjInMv,
            ::CORBA::UShort deltaInMs,
            ::CORBA::Boolean doPower );

        carma::antenna::common::IVCurve * getIVCurve( );

        void doIVcurve(
            ::CORBA::Float startVjInMv,
            ::CORBA::Float stopVjInMv,
            ::CORBA::Float stepVjInMv,
            ::CORBA::UShort deltaInMs,
            ::CORBA::Boolean doPower,
            ::CORBA::ULong seqNo );


	void setVG(carma::antenna::common::FrontEndControl::Amp amp,
	    carma::antenna::common::FrontEndControl::Stage stage,
	    float voltage);

	void setVD(carma::antenna::common::FrontEndControl::Amp amp,
	    carma::antenna::common::FrontEndControl::Stage stage,
	    float voltage);

	void setID(carma::antenna::common::FrontEndControl::Amp amp,
	    carma::antenna::common::FrontEndControl::Stage stage,
	    float voltage);

	void setMixer(float voltage);

  private:

	log4cpp::Category &log_;
	Configuration &_config;

    }; // End class FrontEndControlImpl
    } // End namespace bima
  } // End namespace antenna
} // End namespace carma

#endif
