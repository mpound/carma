/**
 * @file
 * EnvironmentalMonitorImplCorba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.8 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: EnvironmentalControlImpl.h,v 1.8 2012/02/15 21:05:00 abeard Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_ENVIRONMENTALCONTROLIMPL_H
#define CARMA_ANTENNA_OVRO_ENVIRONMENTALCONTROLIMPL_H

// Corba include
#include "carma/corba/corba.h"

namespace log4cpp {
    // Forward declaration
    class Category;
} // End namespace log4cpp

namespace carma {
namespace antenna {
namespace ovro {

    class EnvironmentalMonitor;

    /**
     * CORBA control implementation for the EnvironmentalControl interface.
     * This class delegates CORBA control commands to associated canbus
     * classes. By using delegation we introduce a level of indirection
     * which will make it easier to switch the underlying communications
     * mechanism (CORBA) should the need ever arise.
     */
    class EnvironmentalControlImpl {
    public:

        /**
         * Constructor.
         * @param env Reference to EnvironmentalMonitor CAN Device class.
         */
        EnvironmentalControlImpl( EnvironmentalMonitor & env );

        ~EnvironmentalControlImpl(); 

        void enableCamera( ::CORBA::Boolean on );

        void turnSidecabPowerOff();  // !!!

        void enable24vPs( ::CORBA::UShort supplyNo, ::CORBA::Boolean on);

    private:

        EnvironmentalMonitor &env_;
        log4cpp::Category &log_;

    }; // End class EnvironmentalControlImpl
}}} // End namespace carma::antenna::ovro
#endif
