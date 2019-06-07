/**
 * @file
 * Class to manage details of Correlator structure and types of correlators
 * within CARMA. Ties together monitor information and runtime needs.
 * Monitor information is primary source of structural information.
 *
 * @author: Amar Amarnath
 *
 * $CarmaCopyright$
 */

#include "carma/monitor/Correlator.h"

#include <string>

#include "carma/monitor/BimaSubsystem.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/monitor/SzaSubsystem.h"
#include "carma/monitor/WbcBandSubsystem.h"
#include "carma/util/ErrorException.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


struct Correlator::Info {
    util::CorrelatorType type;
    string         name;
};


namespace {


const Correlator::Info kInfoTab[ ] = {
    {
        CORR_NONE,      // type
        "None",         // name
    },
    {
        CORR_SPECTRAL,  // type
        "SpectralLine", // name
    },
    {
        CORR_WIDEBAND,  // type
        "WideBand",     // name
    },
    {
        CORR_C3GMAX8,   // type
        "C3GMAX8",      // name
    },
    {
        CORR_C3GMAX23,  // type
        "C3GMAX23",     // name
    },
};


const ::size_t kInfoTabCount = (sizeof( kInfoTab ) / sizeof( kInfoTab[ 0 ] ));


const Correlator::Info &
findInfoTabEntry( const util::CorrelatorType type ) {
    for ( ::size_t i = 0; i < kInfoTabCount; ++i ) {
        if ( kInfoTab[ i ].type == type )
            return kInfoTab[ i ];
    }
    
    throw CARMA_ERROR( "Unknown correlator type" );
}


}  // namespace < anonymous >


Correlator::Correlator( const util::CorrelatorType type ) :
info_( findInfoTabEntry( type ) )
{
}


Correlator::~Correlator( ) {
}


::size_t
Correlator::numCorrelatorTypes( ) {
    return kInfoTabCount;
}


const string
Correlator::correlatorTypeName( ) const {
    return info_.name;
}


const string
Correlator::correlatorTypeName( util::CorrelatorType corrType ) {
    return findInfoTabEntry( corrType ).name;
}


util::CorrelatorType
Correlator::correlatorType( ) const {
    return info_.type;
}
