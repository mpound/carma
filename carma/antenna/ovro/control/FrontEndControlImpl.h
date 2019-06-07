/**
 * @file
 * FrontEndControlImplCorba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.25 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: FrontEndControlImpl.h,v 1.25 2012/02/15 21:05:00 abeard Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_FRONTENDCONTROLIMPL_H
#define CARMA_ANTENNA_OVRO_FRONTENDCONTROLIMPL_H

// Corba include
#include "carma/corba/corba.h"

// Carma includes
#include "carma/antenna/common/RxControl.h"
#include "carma/antenna/common/RxTypeInfo.h"
#include "carma/antenna/ovro/control/ovroFrontEndControl.h"

namespace log4cpp {
    // Forard declaration
    class Category;
} // End namespace log4cpp

namespace carma {
namespace antenna {

namespace common {
    class SisReceiver;
} // namespace common

namespace ovro {

    class AntennaIF;

    /**
     * CORBA control implementation for the FrontEndControl interface.
     * This class delegates CORBA control commands to associated canbus
     * classes. By using delegation we introduce a level of indirection
     * which will make it easier to switch the underlying communications
     * mechanism (CORBA) should the need ever arise.
     */
    class FrontEndControlImpl {
    public:

        /**
         * Constructor.
         * @param sisRx Reference to SIS Receiver module.
         * @param type Receiver type.
         */
        FrontEndControlImpl(
            carma::antenna::common::SisReceiver & sisRx,
            carma::antenna::common::RxControl::Type type,
            carma::antenna::common::RxControl::Pol_Type polType,
            carma::antenna::ovro::AntennaIF & antIF );

        ~FrontEndControlImpl(); // Prohibit explicit destruction

        // Public control commands
        /**
         * Set the frequency of the first stage controller.
         * @see carma::antenna::common::FrontEndControl::setFrequency
         */
        void setFrequency(::CORBA::Double freq);

        // Only callable via CORBA.
        void setSISVj(::CORBA::Float voltage);

        void setSISIj(::CORBA::Float current);

	void doIVcurve(
        ::CORBA::Float startVjInMv,
        ::CORBA::Float stopVjInMv,
        ::CORBA::Float stepVjInMv,
        ::CORBA::UShort deltaInMs,
        ::CORBA::Boolean doPower,
	    ::CORBA::ULong seqNo );

    carma::antenna::common::IVCurve *
    getIVCurve( );

	void setVG(carma::antenna::common::FrontEndControl::Amp amp,
	    carma::antenna::common::FrontEndControl::Stage stage,
	    ::CORBA::Float voltage);

	void setVD(carma::antenna::common::FrontEndControl::Amp amp,
	    carma::antenna::common::FrontEndControl::Stage stage,
	    ::CORBA::Float voltage);

	void setID(carma::antenna::common::FrontEndControl::Amp amp,
	    carma::antenna::common::FrontEndControl::Stage stage,
	    ::CORBA::Float current);

	void setMixer(::CORBA::Float voltage);

	void setLoAttenuation(::CORBA::Float atten);

	void getVgap(carma::antenna::ovro::FrontEndControl::CurrentMode mode,
	    ::CORBA::Float current);

	void setIgap(::CORBA::Float current);

	void setVjLoopMode(
	    carma::antenna::ovro::FrontEndControl::VjLoopMode mode);

	void setIjLoopMode(
	    carma::antenna::ovro::FrontEndControl::IjLoopMode mode);

    private:

	carma::antenna::common::SisReceiver &sisRx_;
    carma::antenna::ovro::AntennaIF & antIF_;
	log4cpp::Category &log_;
	std::string typeString_;
	const carma::antenna::common::RxTypeInfo rxType_;

    }; // End class FrontEndControlImpl
}}} // End namespace carma::antenna::ovro
#endif
