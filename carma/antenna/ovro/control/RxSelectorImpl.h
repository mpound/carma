/**
 * @file
 * RxSelectorImpl CORBA implementation class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.13 $
 * $Date: 2013/01/18 01:00:35 $
 * $Id: RxSelectorImpl.h,v 1.13 2013/01/18 01:00:35 abeard Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_RXSELECTORIMPL_H
#define CARMA_ANTENNA_OVRO_RXSELECTORIMPL_H

#include "carma/antenna/common/RxSelector.h"
#include "carma/antenna/ovro/control/CmRxControlImpl.h"
#include "carma/antenna/ovro/control/RxControlImpl.h"

namespace log4cpp {
    class Category;
}

namespace carma {

namespace corba {
    class Server;
}

namespace monitor {
    class OvroSubsystem;
}

namespace antenna {
namespace ovro {

    // Forward decs
    class CalibratorControlImpl;
    class CmRxControlImpl;
    class OvroMaster;
    class RxControlImpl;

    /**
     * RxSelectorImpl CORBA implementation class.
     */
    class RxSelectorImpl { 
    public:

        /**
         * Constructor
         */
        RxSelectorImpl(
            carma::antenna::ovro::OvroMaster& master,
            carma::antenna::ovro::CalibratorControlImpl & cal,
            carma::monitor::OvroSubsystem & ovroSubsys,
            carma::corba::Server & server,
            unsigned short antennaId,
            const std::string & confDir );

        ~RxSelectorImpl(); 

        carma::antenna::common::RxControl_ptr 
        Rx( carma::antenna::common::RxControl::Type type );
    
    private:

        log4cpp::Category& log_;

        carma::antenna::ovro::CmRxControlImpl rx1cm_;
        carma::antenna::ovro::RxControlImpl rx1mm_;
        carma::antenna::ovro::RxControlImpl rx3mm_;

        carma::antenna::common::RxControl_ptr rx1cmPtr_;
        carma::antenna::common::RxControl_ptr rx1mmPtr_;
        carma::antenna::common::RxControl_ptr rx3mmPtr_;

    }; // End class RxSelectorImpl
}}} // End namespace carma::antenna::ovro
#endif
