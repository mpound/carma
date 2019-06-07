#include "carma/control/RemoteObjHandleT.h"

#include "carma/monitor/monitorPointSpecializations.h"
#include "carma/monitor/MonitorSubsystem.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/util/programLogging.h"
#include "carma/util/UserException.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Time.h"

using namespace ::std;
using namespace ::CORBA;
using namespace log4cpp;
using namespace carma;
using namespace carma::control;
using namespace carma::monitor;
using namespace carma::util;


RemoteObjHandleBase::RemoteObjHandleBase(
    const string &                 doName,
    MonitorPointBool * const       mpReachable,
    const MonitorSubsystem * const subsystem,
    MonitorSystem    * const       system,
    const bool                     defaultLogIfNotReachable,
    const bool                     defaultLogSentCommands ) :
doName_( doName ),
defaultLogIfNotReachable_( defaultLogIfNotReachable ),
defaultLogSentCommands_( defaultLogSentCommands ),
mpReachable_( mpReachable ),
monitorSubsystem_( subsystem ),
monitorSystem_( system ),
objRefValidity_( OBJ_REF_VALIDITY_NEVER_SET )
{
    const ScopedLogNdc ndc( "ROHB::RemoteObjHandleBase " + doName_ );

    CARMA_CPTRACE( Trace::TRACE6, "Entered" );

    setReachableMpValue( true );
}


RemoteObjHandleBase::~RemoteObjHandleBase( )
try {
    // setReachableMpValue( false );
} catch ( ... ) {
    // just stifle any exception

    return;
}


void
RemoteObjHandleBase::setReachableMpValue( const bool value )
{
    const ScopedLogNdc ndc( "ROHB::setReachableMpValue " + doName_ );

    CARMA_CPTRACE( Trace::TRACE7, "value=" << (boolalpha) << value );

    if ( mpReachable_ != 0 )
        mpReachable_->setValue( value );

    CARMA_CPTRACE( Trace::TRACE7, "mpReachable set" );
}


void
RemoteObjHandleBase::markObjRefValid( )
{
    const ObjRefValidity oldValidity = objRefValidity_;

    objRefValidity_ = OBJ_REF_VALIDITY_GOOD;

    setReachableMpValue( true );

    if ( oldValidity != OBJ_REF_VALIDITY_GOOD )
        programLogInfoIfPossible( "Marking " + doName_ + " as connected" );
}


void
RemoteObjHandleBase::invalidateObjRef( )
{
    const ObjRefValidity oldValidity = objRefValidity_;

    objRefValidity_ = OBJ_REF_VALIDITY_BAD;

    setReachableMpValue( false );

    if ( oldValidity != OBJ_REF_VALIDITY_BAD )
        programLogErrorIfPossible( "Marking " + doName_ + " as disconnected" );
}


void
RemoteObjHandleBase::throwIfObjRefIsNotValid( ) const
{
    if ( objRefValidity_ != OBJ_REF_VALIDITY_GOOD )
        throwBadRemoteObjAccess();
}


bool
RemoteObjHandleBase::isObjReachable( const bool logIfNotReachable )
{
    // const ScopedLogNdc ndc( "ROHB::isObjReachable " + doName_ );

    if ( objRefValidity_ == OBJ_REF_VALIDITY_GOOD )
        return true;

    if ( monitorSystem_ == 0 ) {
        CARMA_CPTRACE( Trace::TRACE7, "monitorSystem_ is null" );
    } else {
        CARMA_CPTRACE( Trace::TRACE7,
                       "Calling monitorSystem_->readNewestConditionalCopy" );

        monitorSystem_->readNewestConditionalCopy();
    }

    enum {
        UNKNOWN_REASON,
        SUBSYSTEM_PTR_NULL_REASON,
        SUBSYSTEM_NOT_CURRENT_REASON,
        DID_NOT_RESOLVE_SYSTEM_REASON
    } reason = UNKNOWN_REASON;

    if ( monitorSubsystem_ == 0 )
        reason = SUBSYSTEM_PTR_NULL_REASON;
    else if ( (objRefValidity_ != OBJ_REF_VALIDITY_NEVER_SET) &&
              (monitorSubsystem_->isCurrent() != true) )
        reason = SUBSYSTEM_NOT_CURRENT_REASON;
    else {
        reason = DID_NOT_RESOLVE_SYSTEM_REASON;
        
        resolveObjRef();
    }
    
    const bool result = (objRefValidity_ == OBJ_REF_VALIDITY_GOOD);
    
    if ( result != true ) {
        string msg = doName_ + " is not reachable because ";
        
        switch ( reason ) {
            case UNKNOWN_REASON:
                msg += "of an unknown reason";
                break;

            case SUBSYSTEM_PTR_NULL_REASON:
                msg += "it's associated monitor subsystem pointer is NULL";
                break;

            case SUBSYSTEM_NOT_CURRENT_REASON:
                msg += "it's associated monitor subsystem is not current";
                break;

            case DID_NOT_RESOLVE_SYSTEM_REASON:
                msg += "the DO object ref did not resolve";
                break;
        }
        
        CARMA_CPTRACE( Trace::TRACE5, msg );

        if ( logIfNotReachable )
            programLogErrorIfPossible( msg );
    }
    
    return result;
}


bool
RemoteObjHandleBase::isObjReachable( )
{
    return isObjReachable( defaultLogIfNotReachable_ );
}


void
RemoteObjHandleBase::forceFullReconnect( )
{
    resolveObjRef();
}


bool
RemoteObjHandleBase::attemptToReconnectIfNeeded( )
{
    if (objRefValidity_ != OBJ_REF_VALIDITY_GOOD) {
        return resolveObjRef();
    }
    return true;
}


void
RemoteObjHandleBase::throwBadRemoteObjAccess( ) const
{
    string msg;

    msg += "Bad remote object access attempted on ";
    msg += doName_;

    programLogError( msg );

    throw CARMA_ERROR( msg );
}


void
RemoteObjHandleBase::logException( const string & callString,
                                   const string & exString ) const
{
    programLogError( callString + " - " + exString );
}


void
RemoteObjHandleBase::logSentCommand( const string & callString,
                                     const double   mjd,
                                     const string & subDoName ) const
{
    // digits after decimal for seconds (3 = millisecond level precision)
    const int kSecondsPrecision = 3;

    string msg;

    msg += callString;
    msg += " invoked on ";
    msg += doName_;
    if ( subDoName.empty() == false ) {
        msg += ":";
        msg += subDoName;
    }
    msg += " at ";
    msg += Time::getTimeString( mjd, kSecondsPrecision );

    programLogInfo( msg );
}


void
RemoteObjHandleBase::logSentCommand( const string & callString,
                                     const double   mjd ) const
{
    logSentCommand( callString, mjd, string() );
}


void
RemoteObjHandleBase::logSentCommandIfNeeded( const string & callString,
                                             const double   mjd,
                                             const string & subDoName ) const
{
    if ( defaultLogSentCommands_ )
        logSentCommand( callString, mjd, subDoName );
}


void
RemoteObjHandleBase::logSentCommandIfNeeded( const string & callString,
                                             const double   mjd ) const
{
    if ( defaultLogSentCommands_ )
        logSentCommand( callString, mjd );
}


void
RemoteObjHandleBase::processException(const string &    callString,
                                      const Exception& corbaException) 
{
    ostringstream oss;

    const util::UserException * const carmaUserException =
        dynamic_cast< const util::UserException * >( &corbaException );

    const CORBA::UserException * const corbaUserException =
        dynamic_cast< const CORBA::UserException * >( &corbaException );

    if ( carmaUserException != 0 ) {
        oss << "carma::util::UserException "
            << carmaUserException->_name() << " raised."
            << " Message - " << carmaUserException->errorMsg << "."
            << " File - " << carmaUserException->fileName << "."
            << " Line - " << carmaUserException->lineNo << ".";
    } else if ( corbaUserException != 0 ) {
        oss << "CORBA::UserException "
            << corbaUserException->_name() << " raised."
            << " Reason - "
            << corbaUserException->_info().c_str(); // Tao specific
    } else {
        invalidateObjRef();

        const CORBA::SystemException * const corbaSystemException =
            dynamic_cast< const CORBA::SystemException * >( &corbaException );

        if ( corbaSystemException != 0 ) {
            oss << "CORBA::SystemException "
                << corbaSystemException->_name() << " raised."
                << " Reason - "
                << corbaSystemException->_info().c_str(); // Tao specific
        } else {
            oss << "CORBA::Exception "
                << corbaException._name() << " raised."
                << " Reason - "
                << corbaException._info().c_str(); // Tao specific
        }
    }

    logException( callString, oss.str() );
}


void
RemoteObjHandleBase::invalidateObjRefIfNeededForCaught( )
{
    try {
        throw;
    } catch ( const util::UserException & ) {
        // No need to invalidate
    } catch ( const CORBA::UserException & ) {
        // No need to invalidate
    } catch ( const CORBA::Exception & ) {
        invalidateObjRef();
    } catch ( ... ) {
        // No need to invalidate
    }
}

