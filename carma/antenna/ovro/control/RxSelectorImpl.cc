/**
 * @file
 * RxSelectorImpl CORBA class definition.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.12 $
 * $Date: 2013/01/18 01:00:35 $
 * $Id: RxSelectorImpl.cc,v 1.12 2013/01/18 01:00:35 abeard Exp $
 */

#include "carma/antenna/ovro/control/RxSelectorImpl.h"

#include "carma/antenna/common/RxControl.h"
#include "carma/antenna/common/RxControl_skel.h"
#include "carma/antenna/common/RxControl_skel_tie.h"
#include "carma/antenna/ovro/canbus/OvroMaster.h"
#include "carma/antenna/ovro/control/CalibratorControlImpl.h"
#include "carma/antenna/ovro/control/CmRxControlImpl.h"
#include "carma/antenna/ovro/control/RxControlImpl.h"
#include "carma/corba/Server.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

// STL includes
#include <string>

using namespace carma::antenna::common;
using namespace carma::antenna::ovro;
using namespace carma::corba;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

// -----------------------------------------------------------------------------
RxSelectorImpl::RxSelectorImpl(
    OvroMaster & master,
    CalibratorControlImpl & cal,
    carma::monitor::OvroSubsystem & ovroSubsys,
    carma::corba::Server & server,
    const unsigned short antennaId,
    const std::string & confDir )
    :
    log_( Program::getLogger() ),
    rx1cm_( master, cal, ovroSubsys, server, antennaId, confDir ),
    rx1mm_( master, cal, RxControl::RX1MM, ovroSubsys, server ),
    rx3mm_( master, cal, RxControl::RX3MM, ovroSubsys, server ) 
{
    CARMA_CPTRACE( Trace::TRACE6, "RxSelectorImpl() - Creating receiver "
        "selector factory RxSelectorImpl.");

    server.addServant< POA_carma::antenna::common::RxControl_tie >( rx1cm_, 
                                                                    rx1cmPtr_ );
    server.addServant< POA_carma::antenna::common::RxControl_tie >( rx1mm_,
                                                                    rx1mmPtr_ );
    server.addServant< POA_carma::antenna::common::RxControl_tie >( rx3mm_,
                                                                    rx3mmPtr_ );
}


// -----------------------------------------------------------------------------
RxSelectorImpl::~RxSelectorImpl()
{
    CARMA_CPTRACE( Trace::TRACE6, "~RxSelectorImpl() - Destroying receiver "
        "selector factory RxSelectorImpl.");
}

// -----------------------------------------------------------------------------
carma::antenna::common::RxControl_ptr RxSelectorImpl::Rx( RxControl::Type type )
try {
        string typeString =
            (type == common::RxControl::RX1CM ? "RX1CM" :
            (type == common::RxControl::RX1MM ? "RX1MM" :
            (type == common::RxControl::RX3MM ? "RX3MM" : "RXANY")));

        log_ << Priority::INFO << "RxSelectorImpl::Rx() - "
            << "Retrieving Rx for receiver type " << typeString << ".";

        switch (type) {
            case RxControl::RX1CM:
                return RxControl::_duplicate( rx1cmPtr_ );
                break;
            case RxControl::RX1MM:
                return RxControl::_duplicate( rx1mmPtr_ );
                break;
            case RxControl::RX3MM:
                return RxControl::_duplicate( rx3mmPtr_ );
                break;
            default:
                return RxControl::_nil();
                break;
        };
    
        // We should never get here.
        return RxControl::_nil();

} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
    return RxControl::_nil(); // squash warning
}
