/**
 * @file
 * LOControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.7 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: CmLOControlImpl.h,v 1.7 2012/02/15 21:05:00 abeard Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_CMLOCONTROLIMPL_H
#define CARMA_ANTENNA_OVRO_CMLOCONTROLIMPL_H

#include "carma/corba/corba.h"

#include "carma/antenna/common/RxTypeInfo.h"
#include "carma/antenna/common/SwitchState.h"

namespace log4cpp {
    // Forward declaration
    class Category;
} // End namespace log4cpp

namespace carma {
namespace antenna {

namespace common {
    class LOReferenceMonitor;
    class Varactor;
} // End namespace common

namespace ovro {

    class GunnPll;
    class YigPll;

    /**
     * CmLOControlImpl Corba control class.
     * This class delegates Corba control commands to Ovro antenna CANbus
     * device classes.  The delegation allows for a level of indirection which
     * will make changing the underlying communications mechanism much
     * easier if and when we determine it is necessary.
     */
    class CmLOControlImpl {
    public:

        /**
         * Constructor.
         * @param yig Reference to YigPll CAN Device class.
         * @param gunn Reference to Varactor CAN Device class.
         * @param type Receiver type we are instantiating.
         */
        CmLOControlImpl(
            YigPll & yig,
            GunnPll & gunn,
            carma::antenna::common::Varactor & varactor,
            carma::antenna::common::LOReferenceMonitor & loref );

        ~CmLOControlImpl(); 

        /**
         * Verify that yig frequency is in range.
         * @param yigFreq Frequency to verify.
         * @return true if valid, false if out of range.
         */
        bool yigFreqOutOfRange( ::CORBA::Double yigFreq );

        /**
         * Verify that lo frequency is in range.
         * @param loFreq Frequency to verify.
         * @return true if valid, false if out of range.
         */
        bool loFreqOutOfRange( ::CORBA::Double loFreq );

        /**
         * Set the gunn LO frequency independent of the YIG.
         * @param Frequency LO frequency in GHz.
         * @throw carma::util::UserException
         */
        void setLoFrequency( ::CORBA::Double Frequency );

        /**
         * Set the yig frequency and lock.
         * This method does NOT block while waiting for lock.
         * @param yigFreq Frequency in GHz.
         * @throw carma::util::UserException
         */
        void setYigFrequency( ::CORBA::Double yigFreq );

        /**
         * Set the LO terminator power level to preset value.
         */
        void setLoTerminatorPowerToPreset( );

        /**
         * Set the LO terminator power level to requested value.
         */
        void setLoTerminatorPowerLevel( ::CORBA::Double power );

        // Yig and LO frequencies are in GHz!

        // Control commands from antenna::common::LOControl interface.
        void toggleSweep( ::CORBA::Boolean on );

        void toggleYigSweep( ::CORBA::Boolean on );

        void setLoTerminatorAttenuation( ::CORBA::UShort atten );

        // Control commands specific to OVRO YigPll.
        void extractTuneTable( );

        void setDampingResistance( ::CORBA::UShort resistance );

        void setYigOutputFrequency( ::CORBA::Double freq );

        void resetYigPll( );

        void setGunnLoopGainResistance( ::CORBA::UShort resistanceInOhms );

        void turnGunn( carma::antenna::common::SwitchState state );

        void turnIfMonitor( carma::antenna::common::SwitchState state );

        void resetGunn( );

    private:

        carma::antenna::ovro::YigPll & yig_;
        carma::antenna::ovro::GunnPll & gunn_;
        carma::antenna::common::Varactor & varactor_;
        carma::antenna::common::LOReferenceMonitor & loref_;
        log4cpp::Category & log_;
        const carma::antenna::common::RxTypeInfo typeInfo_;

   }; // End class LOControlImpl
}}} // End namespace carma::antenna::ovro
#endif
