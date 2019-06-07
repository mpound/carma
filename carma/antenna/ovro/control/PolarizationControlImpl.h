/**
 * @file
 * PolarizationControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.17 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: PolarizationControlImpl.h,v 1.17 2012/02/15 21:05:00 abeard Exp $
 *
 * $CarmaCopyright$
 */
#ifndef CARMA_ANTENNA_OVRO_POLARIZATIONCONTROLIMPL_H
#define CARMA_ANTENNA_OVRO_POLARIZATIONCONTROLIMPL_H

// Corba includes
#include "carma/corba/corba.h"

// Carma includes
#include "carma/antenna/common/RxControl.h"

namespace log4cpp {
   // Forward declaration
   class Category;
} // End namespace log4cpp

namespace carma {
namespace antenna {
namespace ovro {

    // Forward declaration
    class Optics;

    /**
     * PolarizationControlImpl Corba control class.
     * This class makes use of delegation to dispatch CORBA control commands
     * to appropriate canbus::Device derived and logical classes.  This allows
     * us to more easily change underying control mechanisms if and when
     * the need ever arises.
     */
    class PolarizationControlImpl {
    public:

        /**
         * Constructor
         * @param optics Pointer to underlying ovro optics CAN device.
         * @param type The type of receiver this particular object will control.
         * @param poa Pointer to encompassing poa.
         */
        PolarizationControlImpl(
            carma::antenna::ovro::Optics& optics,
            carma::antenna::common::RxControl::Type type );

        /**
         * Set observing frequency.
         * Note that this is static and updates static information
         * due to the fact that there is only a single observing
         * frequency despite the several different receiver types.
         * @param freq in GHz
         */
        static void setObservingFreq(float freq);

        ~PolarizationControlImpl(); 

        // Only callable via CORBA
        void setState(
                carma::antenna::common::PolarizationControl::State poltype,
                ::CORBA::ULong seqNo );

        void setParameters(float gridAngle, float backshortPos);

    private:

        carma::antenna::ovro::Optics& optics_;
        log4cpp::Category &log_;
        static float observingFreq_; // I'm afraid this is needed.
        const carma::antenna::common::RxControl::Type type_;

    };
}}} // End namespace carma::antenna::ovro
#endif
