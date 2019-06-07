/**
 * @file
 * BIMA CryoControl implementation.
 *
 * @author Colby Gutierrez-Kraybill
 *
 * $Revision: 1.7 $
 * $Id: OpticsControlImpl.h,v 1.7 2012/02/21 21:06:58 abeard Exp $
 */

#ifndef CARMA_ANTENNA_BIMA_OPTICSCONTROLIMPL_H
#define CARMA_ANTENNA_BIMA_OPTICSCONTROLIMPL_H

#include "carma/antenna/bima/RxClient.h"

namespace log4cpp {
  class Category;
}

namespace carma {
namespace antenna {
namespace bima {

    /**
     * OpticsControlImpl Corba control class.
     * This class uses delegation to communicate with appropriate logical
     * and canbus device classes.  This delegation allows us to more easily
     * switch the underlying communications mechanism if and when this is
     * found necessary.
     */
    class OpticsControlImpl :
      public RxClient
    {
    public:

        /**
         * Constructor
         */
        OpticsControlImpl
	  (
	   Configuration &config
	  );

        /**
         * Destructor
         */
        ~OpticsControlImpl();

        void selectRx();

    private:

        log4cpp::Category &log_;
	Configuration &_config;

    }; // End class OpticsControlImpl
}}} // Namespace carma::antenna::bima
#endif
