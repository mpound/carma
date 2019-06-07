/**
 * @file
 * OpticalTelControl Corba interface implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.18 $
 * $Date: 2012/02/29 16:23:01 $
 * $Id: OpticalTelControlImpl.h,v 1.18 2012/02/29 16:23:01 abeard Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_OPTICALTELCONTROLIMPL_H
#define CARMA_ANTENNA_OVRO_OPTICALTELCONTROLIMPL_H

// Carma includes
#include "carma/antenna/common/OpticalTelCommon.h"
#include "carma/antenna/ovro/control/EnvironmentalControl.h"

// C++ Standard Library includes
#include <string>

namespace log4cpp {
    // Forward declaration
    class Category;
} // End namespace log4cpp

namespace carma {

namespace corba {
    class Client;
}

namespace monitor {
    class OvroSubsystem;
}

namespace antenna {
namespace ovro {

    /**
     * OpticalTelControl implementation class.
     */
    class OpticalTelControlImpl : 
        public carma::antenna::common::OpticalTelCommon
    {
    public:

        /**
         * Constructor.
         * @param antenna String antenna name (e.g. ovro1).
         * @param fg Reference to FrameGrabber instance.
         * @param poa Poa responsible for creating this DO.
         * @param activate Activate this servant or let someone else do it.
         */
        OpticalTelControlImpl(
            ::std::string antenna,
            carma::antenna::common::FrameGrabber& fg,
            bool activate,
            carma::monitor::AntennaCommon::OpticalTel & opticalTel,
            float azFieldOfViewInArcminutes,
            float elFieldOfViewInArcminutes,
            float rotationInDegrees,
            bool simulate,
            carma::corba::Client & client );

        void turn( carma::antenna::common::SwitchState state );

    private:

        ~OpticalTelControlImpl( ); // Don't allow explicit destruction

        ::std::string enviroName_; // Hierarchical DO name in nameserver.
        carma::antenna::ovro::EnvironmentalControl_var enviroControl_;

        carma::corba::Client & client_; // Needed to resolve environment control

        // These are inherited from OpticalTelCommon, as protected members
        //log4cpp::Category& log_;

   }; // End class OpticalTelControlImpl
}}} // End namespace carma::antenna::ovro

#endif

