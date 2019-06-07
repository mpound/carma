/**
 * @file
 * BIMA CryoControl implementation.
 *
 * @author Colby Gutierrez-Kraybill
 *
 * $Revision: 1.9 $
 * $Id: PolarizationControlImpl.h,v 1.9 2012/02/21 21:06:58 abeard Exp $
 */

#ifndef CARMA_ANTENNA_BIMA_POLARIZATIONCONTROLIMPL_H
#define CARMA_ANTENNA_BIMA_POLARIZATIONCONTROLIMPL_H


// Carma includes
#include "carma/antenna/bima/RxClient.h"
#include "carma/antenna/common/PolarizationControl.h"
#include "carma/corba/corba.h"

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
     * PolarizationControlImpl Corba control class.
     */
    class PolarizationControlImpl :
      public RxClient
    {
    public:

        /**
         * Constructor
         */
        PolarizationControlImpl
	  (
	   Configuration &config
	  );

        /**
         * Destructor
         */
        ~PolarizationControlImpl();

        /**
         * Set observing frequency.
         * Note that this is static and updates static information
         * due to the fact that there is only a single observing
         * frequency despite the several different receiver types.
         * @param freq in GHz
         */
        static void setObservingFreq(float freq);

        void setState(
            carma::antenna::common::PolarizationControl::State poltype);


        void setState(
            carma::antenna::common::PolarizationControl::State poltype,
            ::CORBA::ULong seqNo );

    private:

        log4cpp::Category &log_;
	Configuration &_config;
        static float observingFreq_; // I'm afraid this is needed.


    };
}}} // End namespace carma::antenna::bima
#endif
