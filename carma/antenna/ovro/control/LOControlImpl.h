/**
 * @file
 * LOControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.32 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: LOControlImpl.h,v 1.32 2012/02/15 21:05:00 abeard Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_LOCONTROLIMPL_H
#define CARMA_ANTENNA_OVRO_LOCONTROLIMPL_H

// Carma includes
#include "carma/antenna/common/RxControl.h"
#include "carma/antenna/common/RxTypeInfo.h"
#include "carma/antenna/common/SwitchState.h"
#include "carma/antenna/ovro/canbus/YigPll.h"

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

    // Forward declaration
    class GunnPll;

    /**
     * LOControlImpl Corba control class.
     * This class delegates Corba control commands to Ovro antenna CANbus
     * device classes.  The delegation allows for a level of indirection which
     * will make changing the underlying communications mechanism much
     * easier if and when we determine it is necessary.
     */
    class LOControlImpl {
    public:

        /**
         * Constructor.
         * @param yig Reference to YigPll CAN Device class.
         * @param gunn Reference to GunnPll CAN Device class.
         * @param type Receiver type we are instantiating.
         */
        LOControlImpl(
            YigPll & yig,
            GunnPll & gunn,
            carma::antenna::common::Varactor & varactor,
            carma::antenna::common::LOReferenceMonitor & loref,
            carma::antenna::common::RxControl::Type type );
        
        ~LOControlImpl(); 

        /**
         * Verify that yig frequency is in range.
         * @param yigFreq Frequency to verify.
         * @return true if valid, false if out of range.
         */
        static bool yigFreqOutOfRange( ::CORBA::Double yigFreq );

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
         * Set Yig frequency and wait for either lock or timeout.
         * @param yigFreq Requested YIG frequency in GHz.
         * @return Lock result.
         */
        YigPll::LockResultType
        setYigFrequencyAndWaitForLockOrTimeout( ::CORBA::Double yigFreq );

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

        // Control commands specific to OVRO Bias Tuned Gunn.
        void setGunnVoltage( ::CORBA::Float volts );

        void setGunnLoopGain( ::CORBA::Float percent );

        void turnGunn( carma::antenna::common::SwitchState state );

        void turnIfMonitor( carma::antenna::common::SwitchState state );

        void setTuner( ::CORBA::ULong position );

        void setBackshort( ::CORBA::ULong position );

        void setAttenuator( ::CORBA::ULong position );

        void jogTuner( ::CORBA::Short microsteps );

        void jogBackshort( ::CORBA::Short microsteps );

        void jogAttenuator( ::CORBA::Short microsteps );

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
