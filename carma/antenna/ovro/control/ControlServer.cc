/**
 * @file 
 * ControlServer class definition.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.34 $
 * $Id: ControlServer.cc,v 1.34 2013/01/18 01:00:35 abeard Exp $
 */

#include "carma/antenna/ovro/control/ControlServer.h"

// Must include all generated IDL stubs, client and server.
#include "carma/antenna/ovro/control/ovroAntennaControl.h"
#include "carma/antenna/ovro/control/ovroAntennaControl_skel.h"
#include "carma/antenna/ovro/control/ovroAntennaControl_skel_tie.h"
#include "carma/antenna/common/CalibratorControl.h"
#include "carma/antenna/common/CalibratorControl_skel.h"
#include "carma/antenna/common/CalibratorControl_skel_tie.h"
#include "carma/antenna/ovro/control/ovroCryoControl.h"
#include "carma/antenna/ovro/control/ovroCryoControl_skel.h"
#include "carma/antenna/ovro/control/ovroCryoControl_skel_tie.h"
#include "carma/antenna/ovro/control/ovroDriveControl.h"
#include "carma/antenna/ovro/control/ovroDriveControl_skel.h"
#include "carma/antenna/ovro/control/ovroDriveControl_skel_tie.h"
#include "carma/antenna/ovro/control/EnvironmentalControl.h"
#include "carma/antenna/ovro/control/EnvironmentalControl_skel.h"
#include "carma/antenna/ovro/control/EnvironmentalControl_skel_tie.h"
#include "carma/antenna/ovro/control/ovroFocusControl.h"
#include "carma/antenna/ovro/control/ovroFocusControl_skel.h"
#include "carma/antenna/ovro/control/ovroFocusControl_skel_tie.h"
#include "carma/antenna/common/RxSelector.h"
#include "carma/antenna/common/RxSelector_skel.h"
#include "carma/antenna/common/RxSelector_skel_tie.h"
#include "carma/antenna/ovro/control/RxTemperatureControl.h"
#include "carma/antenna/ovro/control/RxTemperatureControl_skel.h"
#include "carma/antenna/ovro/control/RxTemperatureControl_skel_tie.h"
#include "carma/antenna/common/TiltmeterControl.h"
#include "carma/antenna/common/TiltmeterControl_skel.h"
#include "carma/antenna/common/TiltmeterControl_skel_tie.h"

#include "carma/antenna/ovro/canbus/OvroMaster.h"

#include "carma/corba/corba.h"
#include "carma/corba/Server.h"

#include "carma/monitor/OvroSubsystem.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"

// Carma tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

// STL includes
#include <iostream>
#include <sstream>

using namespace carma::antenna::common;
using namespace carma::antenna::ovro;
using namespace carma::monitor;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

namespace {

    ::std::string
    createNamingPrefix( const unsigned short antennaId )
    {
        ostringstream prefix;

        prefix << "carma.ovro" << antennaId << ".";

        return prefix.str( );
    }

} // End anonymous namespace

// -----------------------------------------------------------------------------
ControlServer::ControlServer( OvroMaster& master, 
                              const unsigned short antennaId,
                              OvroSubsystem & mon,
                              carma::corba::Server & server,
                              const string & confDir,
                              const bool simulate ) : 
    log_( Program::getLogger( ) ),
    master_(master), 
    ant_( master_, server ),
    cal_( master_.getOptics() ),
    cryo_( master_.getCryoCompressor() ),
    drive_( master_.getDriveEngine(), 
            createNamingPrefix( antennaId ) + OPTICAL_TEL_NAME, 
            simulate ),
    env_( master_.getEnvironmentalMonitor() ),
    focus_( master_.getSecondary() ),
    rxSelector_( master_, cal_, mon, server, antennaId, confDir ),
    rxtemp_( master_.getRxTemperatureController() ),
    tiltmeter_( master_.getTiltmeter() ),
    antennaId_(antennaId),
    namingPrefix_( createNamingPrefix( antennaId ) ),
    server_( server )
{
    try {
        CARMA_CPTRACE( Trace::TRACE5, 
            "ControlServer::ControlServer() - C'tor." );

        programLogInfoIfPossible( "ControlServer::ControlServer - Creating control server." );

        // Add servants to corba::Server and publish them
        programLogInfoIfPossible( "ControlServer::ControlServer - Adding servants." );
        namespace POA_cac = POA_carma::antenna::common;
        namespace POA_cao = POA_carma::antenna::ovro;
        server.addServant< POA_cao::AntennaControl_tie >( 
            ant_, namingPrefix_ + ANTENNA_NAME );
        server.addServant< POA_cac::CalibratorControl_tie >(
            cal_, namingPrefix_ + CALIBRATOR_NAME );
        server.addServant< POA_cao::CryoControl_tie >(
            cryo_, namingPrefix_ + CRYO_NAME );
        server.addServant< POA_cao::DriveControl_tie >(
            drive_, namingPrefix_ + DRIVE_NAME );
        server.addServant< POA_cao::EnvironmentalControl_tie >(
            env_, namingPrefix_ + ENVIRONMENT_NAME );
        server.addServant< POA_cao::FocusControl_tie >(
            focus_, namingPrefix_ + FOCUS_NAME );
        server.addServant< POA_cac::RxSelector_tie >(
            rxSelector_, namingPrefix_ + RXSELECTOR_NAME );
        server.addServant< POA_cao::RxTemperatureControl_tie >(
            rxtemp_, namingPrefix_ + RX_TEMPERATURE_CONTROL_NAME );
        server.addServant< POA_cac::TiltmeterControl_tie >(
            tiltmeter_, namingPrefix_ + TILTMETER_NAME );

        programLogInfoIfPossible( "ControlServer::ControlServer - Creation complete." );
    } catch ( ... ) {
        try {
            throw;
        } catch ( const CORBA::Exception & ex ) {
            log_ << Priority::ERROR << "ControlServer::ControlServer() "
                << "- CORBA::Exception thrown from c'tor: " << ex;
        } catch ( ... ) {
            log_ << Priority::ERROR << "ControlServer::ControlServer() "
                << "- Exception thrown from within C'tor - rethrowing." ;
        }
        throw; // Rethrow for now.
    }
}

// -----------------------------------------------------------------------------
void ControlServer::runServer()
{
    const ScopedLogNdc ndc( "ControlServer::runServer" );

    server_.run( );

}

// -----------------------------------------------------------------------------
ControlServer::~ControlServer()
{
    CARMA_CPTRACE( Trace::TRACE5, "~ControlServer Dtor" );
}
