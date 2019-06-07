/**
 * @file
 * IFControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.17 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: IFControlImpl.h,v 1.17 2012/02/15 21:05:00 abeard Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_IFCONTROLIMPL_H
#define CARMA_ANTENNA_OVRO_IFCONTROLIMPL_H

// Corba includes
#include "carma/corba/corba.h"

// Carma includes
#include "carma/antenna/ovro/canbus/AntennaIF.h"
#include "carma/antenna/common/RxControl.h"
#include "carma/antenna/common/RxTypeInfo.h"

namespace log4cpp {
    // Forward declaration
    class Category;
} // End namespace log4cpp

namespace carma {
namespace antenna {
namespace ovro {

    // Forward declaration
    class AntennaIF;

    /**
     * IFControlImpl Corba control delegate class.
     * This class delegates CORBA commands to canbus Device derived classes.
     * The delegation allows a level of indirection if and when we ever
     * change the underlying control implementation.
     */
    class IFControlImpl {
    public:

        /**
         * Constructor.
         * Creates an instance of the IF Control DO.
         * @param antennaIf Reference to AntennaIF CAN module.
         * @param type The type of receiver this instance applies to.
         */
        IFControlImpl(
            carma::antenna::ovro::AntennaIF& antennaIf,
            carma::antenna::common::RxControl::Type type,
            carma::antenna::common::RxControl::IF_Type ifType );

        ~IFControlImpl(); 

        // Control commands
        /**
         * Based on which receiver this object belongs to, select the
         * appropriate IF switch.
         */
        void selectRx();

        /**
         * Retrieve IF total powers from the Antenna IF (PAM) module.
         * This is generally done as part of an IV curve if requested.
         */
        IFTotalPowerVec getIFTotalPower( ) const;

        void reset( );
        void selectBand( ::CORBA::UShort band );
        void setAtten( ::CORBA::Float atten );
        void setPower( ::CORBA::Float power );
        void saveCurrentPower();
        void setPresetPower();

    private:

        // Disallow assignment and copy construction
        IFControlImpl(const IFControlImpl &);
        IFControlImpl &operator=(const IFControlImpl &);

        AntennaIF & antennaIf_;
        log4cpp::Category & log_;
        std::string typeString_;
        const carma::antenna::common::RxTypeInfo rxType_;
        const carma::antenna::common::RxControl::IF_Type ifType_;

    }; // End class IFControlImpl
}}} // End namespace carma::antenna::ovro
#endif
