#include "carma/control/AntennaHandle.h"

#include "carma/control/antennaHandleUtils.h"
#include "carma/util/Time.h"

#include <iostream>
#include <sstream>
#include <string>

using namespace std;
using namespace carma::control;
using namespace carma::util;

AntennaHandle::AntennaHandle( const unsigned short                     carmaAntNo, 
                              monitor::MonitorSystem &                 monitorSystem,
                              monitor::ControlSubsystemBase::Antenna & antenna ) :
AntennaRemoteObjHandle( makeAntennaDoName( carmaAntNo, carma::antenna::common::ANTENNA_NAME ),
                        &(antenna.antennaReachable( ).antenna( )),
                        &(getAntennaSubsystem( carmaAntNo, monitorSystem )),
                        &monitorSystem,
                        true,
                        true ) 
{
    // Nothing
}

AntennaHandle::~AntennaHandle( )
{
    // Nothing
}

void 
AntennaHandle::setInitialization( const CORBA::Boolean state )
{
    if ( isObjReachable( ) ) {
        string remoteCallString;
        {
            ostringstream oss;

            oss << "AntennaControl::setInitialization( "
                << "state=" << ( state ? "true" : "false" )
                << " )";
            remoteCallString = oss.str( );
        }

        try {
            const double sendTime = Time::MJD( );

            remoteObj( )->setInitialization( state );

            logSentCommandIfNeeded( remoteCallString, sendTime );
        } catch ( const CORBA::Exception & ex ) {
            processException( remoteCallString, ex );
        }
    } // if isObjReachable
}
