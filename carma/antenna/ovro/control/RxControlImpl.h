/**
 * @file
 * RxControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.41 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: RxControlImpl.h,v 1.41 2012/02/15 21:05:00 abeard Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_RXCONTROLIMPL_H
#define CARMA_ANTENNA_OVRO_RXCONTROLIMPL_H

// Carma includes
#include "carma/antenna/ovro/control/LOControlImpl.h"
#include "carma/antenna/ovro/control/OpticsControlImpl.h"
#include "carma/antenna/ovro/control/ovroFrontEndControl.h"
#include "carma/antenna/ovro/control/ovroLOControl.h"
#include "carma/antenna/ovro/control/ovroPolarizationControl.h"
#include "carma/antenna/ovro/control/PolarizationControlImpl.h"
#include "carma/antenna/common/IFControl.h"
#include "carma/antenna/common/RxControl.h"
#include "carma/antenna/common/RxTypeInfo.h"
#include "carma/antenna/common/SwitchState.h"

// Stl includes
#include <map>

namespace log4cpp {
  // Forward declaration
  class Category;
}

namespace carma {

    namespace corba {
        class Server;
    }

    namespace monitor {
        class AntennaCommon;
        class OvroSubsystem;
    } // End namespace monitor

namespace antenna {
namespace ovro {

    // Forward declarations
    class CalibratorControlImpl;
    class FrontEndControlImpl;
    class IFControlImpl;
    class OvroMaster;
    class PolarizationControlImpl;

    /**
     * RxControlImpl CORBA control class.
     * The RxControl interface is a logical interface which was designed
     * to simplify and hide some of the complicated tasks of the receivers.
     * This implementation uses delegation to dispatch CORBA control commands
     * to logical interfaces which likewise control canbus receiver hardware.
     */
    class RxControlImpl {
	public:

        /**
         * Constructor
         */
        RxControlImpl(
                carma::antenna::ovro::OvroMaster & master,
                carma::antenna::ovro::CalibratorControlImpl & cal,
                carma::antenna::common::RxControl::Type type,
                carma::monitor::OvroSubsystem & ovroSubsys,
                carma::corba::Server & server );

        ~RxControlImpl( ); // Prohibit explicit destruction

        carma::antenna::common::LOControl_ptr LO( );

        carma::antenna::common::IFControl_ptr IF(
	    carma::antenna::common::RxControl::IF_Type polarization );

        carma::antenna::common::FrontEndControl_ptr FrontEnd(
                carma::antenna::common::RxControl::Pol_Type pol );

        carma::antenna::common::OpticsControl_ptr Optics( );

        carma::antenna::common::PolarizationControl_ptr Polarization( );

        void setFrequency( ::CORBA::Double yigFreq,
                           ::CORBA::Double LOfreq,
                           ::CORBA::Boolean endWithAbsorberInBeam,
                           ::CORBA::Boolean optimizeReceiver,
                           ::CORBA::Boolean forceRelock,
                           ::CORBA::ULong seqNo );

        void setObservingFrequency( ::CORBA::Double freq,
                                    ::CORBA::ULong seqNo );

        void measureTotalPower(
                carma::antenna::common::CalibratorControl::Position position,
                ::CORBA::ULong seqNo );

        void toggleFastSampling( CORBA::ULong channel,
                                 CORBA::Boolean start );

        void setIFPresetPower( );

        void setIFAtten(
            CORBA::Float atten,
	        carma::antenna::common::RxControl::IF_Type ifType );

        void setIFPower( CORBA::Float power );
    
    private:

        typedef struct {
            RxControlImpl & This;
            ::CORBA::Double yigFreq;
            ::CORBA::Double loFreq;
            ::CORBA::ULong seqNo;
            bool ignoreSeqNo;
            ::CORBA::Boolean endWithAbsorberInBeam;
        } SetFreqArgType;

        // The setFrequency command dispatches it's work to the below thread.
        static void setFrequencyEntryPoint( const SetFreqArgType & args );

        // Since the Rx is the logical container for all of it's
        // subsystems, this class is responsible for creating and
        // maintaining those subsystems.  These are ref counted CORBA
        // servants and as such need to be created on the heap.
        typedef ::std::map< enum carma::antenna::common::RxControl::Pol_Type,
                            FrontEndControlImpl * > FrontEndMap;
        typedef ::std::map< enum carma::antenna::common::RxControl::IF_Type,
                            IFControlImpl * > IFMap;
        typedef ::std::map< 
            enum carma::antenna::common::RxControl::Pol_Type,
            carma::antenna::ovro::FrontEndControl_ptr > FrontEndPtrMap;
        typedef ::std::map< 
            enum carma::antenna::common::RxControl::IF_Type,
            carma::antenna::common::IFControl_ptr > IFPtrMap;

        FrontEndMap frontEndMap_;
        IFMap ifMap_;
        LOControlImpl lo_;
        OpticsControlImpl optics_;
        PolarizationControlImpl polarization_;
        CalibratorControlImpl & cal_;


        FrontEndPtrMap frontEndPtrMap_;
        IFPtrMap ifPtrMap_;
        carma::antenna::ovro::LOControl_ptr loPtr_;
        carma::antenna::common::OpticsControl_ptr opticsPtr_;
        carma::antenna::ovro::PolarizationControl_ptr polPtr_;
        
        log4cpp::Category & log_;
        carma::monitor::OvroSubsystem & mon_;
        carma::monitor::AntennaCommon & comMon_;
        const carma::antenna::common::RxTypeInfo rxType_;

    };
} // namespace ovro
} // namespace antenna
} // namespace carma
#endif
