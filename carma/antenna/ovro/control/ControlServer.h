/** @file
 * carma::antenna::ovro::ControlServer class declaration.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.26 $
 * $Id: ControlServer.h,v 1.26 2013/01/18 01:00:35 abeard Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_CONTROLSERVER_H
#define CARMA_ANTENNA_OVRO_CONTROLSERVER_H

#include "carma/antenna/ovro/control/AntennaControlImpl.h"
#include "carma/antenna/ovro/control/CalibratorControlImpl.h"
#include "carma/antenna/ovro/control/CryoControlImpl.h"
#include "carma/antenna/ovro/control/DriveControlImpl.h"
#include "carma/antenna/ovro/control/EnvironmentalControlImpl.h"
#include "carma/antenna/ovro/control/FocusControlImpl.h"
#include "carma/antenna/ovro/control/RxSelectorImpl.h"
#include "carma/antenna/ovro/control/RxTemperatureControlImpl.h"
#include "carma/antenna/common/TiltmeterControlImpl.h"

#include <string>

namespace log4cpp {
    class Category;
}

namespace carma {

namespace monitor {
    class OvroSubsystem;
}

namespace antenna {

namespace common {
    class TiltmeterControlImpl;
}

/**
 * Contains names specific to OVRO antennas.
 */
namespace ovro {

    class OvroMaster;
    
    /**
     * ControlServer class encapsulates most control details.
     * This class is mainly responsible for encapsulating internal CORBA
     * control details by incarnating all canbus related control DOs which
     * in turn delegate requests to canbus::Device class derivatives or
     * related classes.  The idea behind the class is to confine most details
     * to a single class so that they can be more modularly changed in the 
     * future if need be.
     */
    class ControlServer {
    public:
        
        /**
         * Constructor.
         * The constructor is responsible for creating all CORBA DOs and 
         * publishing them.  Note this object must be created on the stack.
         * The main reason for this is that this object will need to be 
         * destroyed prior to destruction of the master class.  That said,
         * make sure this object is destroyed prior to destroying an 
         * OvroMaster instance.
         * @param master Pointer to OvroMaster object.
         * @param antennaId Unique ID for antenna.
         * @param mon Reference to ovro subsystem instance.
         * @param confDir Location of the conf directory.
         * @param simulate True if we are simulating, false if RTS.
         */
        ControlServer (
            carma::antenna::ovro::OvroMaster& master,
            unsigned short antennaId,
            carma::monitor::OvroSubsystem & mon,
            carma::corba::Server & server,
            const std::string & confDir,
            bool simulate = false );

        /**
         * Destructor
         */
        virtual ~ControlServer(); 

        /**
         * Run control server.
         * This method blocks on the orb!
         */
        void runServer();

    private:
        
        log4cpp::Category &                log_; 
        carma::antenna::ovro::OvroMaster & master_;

        carma::antenna::ovro::AntennaControlImpl ant_;
        carma::antenna::ovro::CalibratorControlImpl cal_;
        carma::antenna::ovro::CryoControlImpl cryo_;
        carma::antenna::ovro::DriveControlImpl drive_;
        carma::antenna::ovro::EnvironmentalControlImpl env_;
        carma::antenna::ovro::FocusControlImpl focus_;
        carma::antenna::ovro::RxSelectorImpl rxSelector_;
        carma::antenna::ovro::RxTemperatureControlImpl rxtemp_;
        carma::antenna::common::TiltmeterControlImpl tiltmeter_;

        const unsigned short antennaId_;

        const ::std::string namingPrefix_;

        carma::corba::Server & server_;

    }; // End class ControlServer
}}} // End namespace carma::antenna::ovro
#endif
