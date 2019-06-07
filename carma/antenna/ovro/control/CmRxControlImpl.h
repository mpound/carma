/**
 * @file
 * CmRxControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.9 $
 * $Date: 2013/01/18 01:00:35 $
 * $Id: CmRxControlImpl.h,v 1.9 2013/01/18 01:00:35 abeard Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_CMRXCONTROLIMPL_H
#define CARMA_ANTENNA_OVRO_CMRXCONTROLIMPL_H

// Carma includes
#include "carma/antenna/common/CMFrontEndControlImpl.h"
#include "carma/antenna/common/RxControl.h"
#include "carma/antenna/common/RxTypeInfo.h"
#include "carma/antenna/ovro/control/CmLOControlImpl.h"
#include "carma/antenna/ovro/control/IFControlImpl.h"
#include "carma/antenna/ovro/control/OpticsControlImpl.h"
#include "carma/antenna/ovro/control/ovroCmLOControl.h"
#include "carma/antenna/ovro/control/ovroPolarizationControl.h"
#include "carma/antenna/ovro/control/PolarizationControlImpl.h"

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
    class OvroMaster;

    /**
     * CmRxControlImpl CORBA control class.
     * The RxControl interface is a logical interface which was designed
     * to simplify and hide some of the complicated tasks of the receivers.
     */
    class CmRxControlImpl {
	public:

        /**
         * Constructor
         */
        explicit CmRxControlImpl(
                carma::antenna::ovro::OvroMaster & master,
                carma::antenna::ovro::CalibratorControlImpl & cal,
                carma::monitor::OvroSubsystem & ovroSubsys,
                carma::corba::Server & server,
                unsigned short antennaId,
                const std::string & confDir );

        ~CmRxControlImpl( ); 

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
                           ::CORBA::Boolean forceRelock, // Not used by ovro
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

        void biasRxUsingConfFile( );

        typedef struct {
            CmRxControlImpl & This;
            ::CORBA::Double yigFreq;
            ::CORBA::Double loFreq;
            ::CORBA::ULong seqNo;
            bool ignoreSeqNo;
            ::CORBA::Boolean endWithAbsorberInBeam;
        } SetFreqArgType;

        // The setFrequency command dispatches it's work to the below thread.
        static void setFrequencyEntryPoint( const SetFreqArgType & args );

        carma::antenna::common::CMFrontEndControlImpl frontEnd_; 
        IFControlImpl if_;
        CmLOControlImpl lo_;
        OpticsControlImpl optics_;
        PolarizationControlImpl polarization_;
        CalibratorControlImpl & cal_;
        
        carma::antenna::common::FrontEndControl_ptr frontEndPtr_;
        carma::antenna::common::IFControl_ptr ifPtr_;
        carma::antenna::ovro::CmLOControl_ptr loPtr_;
        carma::antenna::common::OpticsControl_ptr opticsPtr_;
        carma::antenna::ovro::PolarizationControl_ptr polPtr_;

        log4cpp::Category & log_;
        carma::monitor::OvroSubsystem & mon_;
        carma::monitor::AntennaCommon & comMon_;
        const carma::antenna::common::RxTypeInfo rxType_;
                
        const unsigned short antennaId_;
        const std::string confDir_;

    };
} // namespace ovro
} // namespace antenna
} // namespace carma
#endif
