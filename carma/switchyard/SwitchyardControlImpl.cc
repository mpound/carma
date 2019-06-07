#include "carma/switchyard/SwitchyardControlImpl.h"

#include "carma/switchyard/Switchyard.h"
#include "carma/util/corbaSequenceUtils.h"
#include "carma/util/ExceptionUtils.h"

#include <boost/foreach.hpp>
#include <vector>

using namespace carma::switchyard;
using namespace carma::util;
using namespace std;

SwitchyardControlImpl::SwitchyardControlImpl( Switchyard & switchyard ) :
    switchyard_( switchyard )
{

}
    
SwitchyardControlImpl::~SwitchyardControlImpl( )
{

}

void 
SwitchyardControlImpl::setSwitches( 
    const carma::switchyard::SwitchPositionSeq & pos )
try {
    
    vector< SwitchPosition > posVec;
    
    assignSequenceToVector( pos, posVec );

    SwitchMap switchMap;
    BOOST_FOREACH( const SwitchPosition switchPos, posVec ) {
        switchMap[ switchPos.switchNo ] = switchPos.switchPos;
    }

    switchyard_.setSwitches( switchMap );

} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}
