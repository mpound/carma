/** @file
 * Implementation of Antenna IF CAN Device.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.23 $
 * $Date: 2011/09/26 17:48:16 $
 * $Id: AntennaIF.cc,v 1.23 2011/09/26 17:48:16 iws Exp $
 *
 * $CarmaCopyright$
 */

// Carma includes
#include "carma/antenna/ovro/canbus/AntennaIF.h"
#include "carma/canbus/Utilities.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ScopedPthreadMutexLock.h"

using namespace carma;
using namespace carma::antenna;
using namespace carma::antenna::ovro;
using namespace carma::canbus;
using namespace carma::util;

namespace {

    namespace CM = carma::monitor;

    nodeType checkNode( const nodeType node ) 
    {
        if ( node == common::AntennaIF::IF_LEFT_POL_NODE_ID ||
             node == common::AntennaIF::IF_RIGHT_POL_NODE_ID )
            return node;
        else
            throw CARMA_ERROR( "Invalid node id." );
    }

} // namespace < unnamed >

// -----------------------------------------------------------------------------
AntennaIF::AntennaIF( nodeType node, 
                      CanOutput & io,  
                      carma::monitor::OvroSubsystem & mon ) :
    carma::antenna::common::AntennaIF( 
        node, 
        io, 
        mon.antennaIfContainer( checkNode( node ) - 1 ).state(),
        mon.antennaIfContainer( node - 1 ).antennaIF(),
        mon.antennaIfContainer( node - 1 ).xac() )
{
    // Nothing here
}

// -----------------------------------------------------------------------------
AntennaIF::~AntennaIF()
{
    // Nothing here
}

// -----------------------------------------------------------------------------
IFTotalPowerVec AntennaIF::getIFTotalPower( ) const
{
    ScopedPthreadMutexLock scopelock( totalPowerMutex_ );
    return totalPower;
}

// -----------------------------------------------------------------------------
void AntennaIF::simTotalPower( const unsigned int nsamps ) 
{
    ScopedPthreadMutexLock scopelock( totalPowerMutex_ );

    totalPower.clear();
    const float ifTotalPower = 0.024; // Mw

    for ( unsigned samp = 0; samp < nsamps; ++samp )
        totalPower.push_back( ifTotalPower );
}

// -----------------------------------------------------------------------------
void AntennaIF::processTotalPowerResponse(DataVector &data) 
{

}

// -----------------------------------------------------------------------------
void AntennaIF::processFastTotalPowerPacket(carma::canbus::DataVector & data)
{
    const unsigned short seqNo = dataToUshort( data );
    const float totPower = dataToFloat( data );

    ScopedPthreadMutexLock scopelock( totalPowerMutex_ );
    if ( seqNo == 1 )
        totalPower.clear();

    totalPower.push_back( totPower );
}
    
// -----------------------------------------------------------------------------
void AntennaIF::processSwitchStatusOnChange(DataVector &data)
{
    // No-op
}

// -----------------------------------------------------------------------------
void AntennaIF::processPAMStatusOnChange(DataVector &data)
{
    // No-op
}

// -----------------------------------------------------------------------------
void AntennaIF::processFastChannel1Packet(DataVector &data)
{
    // No-op
}

// -----------------------------------------------------------------------------
void AntennaIF::processFastChannel2Packet(DataVector &data)
{
    // No-op
}



