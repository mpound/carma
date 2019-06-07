/**
 * @file
 * Ovro CryoControl implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.15 $
 * $Id: CryoControlImpl.h,v 1.15 2012/02/15 21:05:00 abeard Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_CRYOCONTROLIMPL_H
#define CARMA_ANTENNA_OVRO_CRYOCONTROLIMPL_H

// Corba includes - for some reason, this must proceed the below CORBA
// generated includes.
#include "carma/corba/corba.h"

// Carma includes
#include "carma/antenna/common/SwitchState.h"

namespace log4cpp {
    // Forward declarations
    class Category;
} // End namespace log4cpp

namespace carma {
namespace antenna {
namespace ovro {

    class CryoCompressor;

    /**
     * CryoControlImpl Corba control implementation.
     * This class implements the CryoControl interface defined in
     * carma::antenna::common. The class simply delegates work to
     * the carma::antenna::ovro::CryoControl canbus::Device derivative.
     * Delegation is a simple way to decouple the canbus side of things from
     * the control communications layer.  It's not a general solution but is
     * simple and easy enough to change when and if a more general solution is
     * found.
     */
    class CryoControlImpl {
    public:

        /**
         * Constructor
         * Inputs a CryoCompressor reference to allow delegation of control
         * commands to it.
         */
        CryoControlImpl( CryoCompressor & compressor );

        ~CryoControlImpl(); // Prevent explicit destruction

        // Only callable via CORBA
        void turnCompressor(carma::antenna::common::SwitchState state);

        void resetCompressor();

        void fillCompressor();

        void purgeCompressor();

        void reset();

        void turnTempServoLoop(carma::antenna::common::SwitchState state);

        void setInletLouverPosition(float volts);

        void setOutletLouverPosition(float volts);

    private:

        carma::antenna::ovro::CryoCompressor & compressor_;
        log4cpp::Category &log_;

    }; // End class CryoControlImpl
}}}  // End namespace carma::antenna::ovro
#endif
