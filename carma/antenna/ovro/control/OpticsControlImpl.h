/**
 * @file
 * OpticsControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.14 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: OpticsControlImpl.h,v 1.14 2012/02/15 21:05:00 abeard Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_OPTICSCONTROLIMPL_H
#define CARMA_ANTENNA_OVRO_OPTICSCONTROLIMPL_H

// Corba includes
#include "carma/corba/corba.h"

// Carma includes
#include "carma/antenna/common/RxControl.h"
#include "carma/antenna/common/RxTypeInfo.h"

namespace carma {
namespace antenna {
namespace ovro {

    // Forward declaration
    class Optics;

    /**
     * OpticsControlImpl Corba control class.
     * This class uses delegation to communicate with appropriate logical
     * and canbus device classes.  This delegation allows us to more easily
     * switch the underlying communications mechanism if and when this is
     * found necessary. Another interesting thing about this class is that
     * there is only a single class with a Rx type attribute, rather than
     * multiple derived classes (one for each type).  The main reason for
     * this is that the receiver specific code is so minimal, I can't
     * justify reproduction of all the derived class boilerplate.
     */
    class OpticsControlImpl { 
    public:

        /**
         * Constructor
         */
        OpticsControlImpl(
            carma::antenna::ovro::Optics& optics,
            carma::antenna::common::RxControl::Type type );

        ~OpticsControlImpl(); 

        void selectRx();

    private:

        carma::antenna::ovro::Optics& optics_;
        const carma::antenna::common::RxTypeInfo rxTypeInfo_;

    }; // End class OpticsControlImpl
}}} // Namespace carma::antenna::ovro
#endif
