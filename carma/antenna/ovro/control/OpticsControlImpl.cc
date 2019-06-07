/**
 * @file
 * Class definition of OpticsControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.17 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: OpticsControlImpl.cc,v 1.17 2012/02/15 21:05:00 abeard Exp $
 */

#include "carma/antenna/ovro/control/OpticsControlImpl.h"

#include "carma/antenna/common/RxControl.h"
#include "carma/antenna/ovro/canbus/Optics.h"
#include "carma/util/BaseException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Trace.h"


using namespace carma::antenna::ovro;
using namespace carma::antenna::common;
using namespace carma::util;
using namespace std;

namespace {

    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE5;
    const Trace::TraceLevel TRACE_CONTROL_COMMANDS = Trace::TRACE5;

    Optics::RECEIVER
    getOpticsRxForControlRxType( const RxControl::Type rxType )
    {
        switch ( rxType ) {
            case RxControl::RX1CM: return Optics::RX_CM;
            case RxControl::RX1MM: return Optics::RX_1MM;
            case RxControl::RX3MM: return Optics::RX_3MM;
            case RxControl::RXANY: return Optics::RX_GRID;
        }
        return Optics::RX_GRID; // Squash compiler warnings
    } // getOpticsRxForControlRxType

} // namespace < unnamed >

// -----------------------------------------------------------------------------
OpticsControlImpl::OpticsControlImpl(
    Optics & optics,
    carma::antenna::common::RxControl::Type type ) :
optics_( optics ),
rxTypeInfo_( type )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "OpticsControlImpl() - "
                   + rxTypeInfo_.rxAsString( ) + " C'tor." );
}

// -----------------------------------------------------------------------------
OpticsControlImpl::~OpticsControlImpl()
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "~OpticsControlImpl() - D'tor." );
}

// -----------------------------------------------------------------------------
void OpticsControlImpl::selectRx( )
try {
    const ScopedLogNdc ndc( "OpticsControlImpl::selectRx( )" );

    const Optics::RECEIVER rx =
        getOpticsRxForControlRxType( rxTypeInfo_.rxAsRxControlType( ) );

    const string msg = "Placing " + rxTypeInfo_.rxAsString( ) + " in beam.";

    programLogInfoIfPossible( msg );

    CARMA_CPTRACE( TRACE_CONTROL_COMMANDS, msg );

    optics_.selectReceiver( rx );

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}
