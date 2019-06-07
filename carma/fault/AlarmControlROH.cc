#include <carma/fault/AlarmControlROH.h>
using namespace carma::fault;

#include <carma/corba/Client.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/Program.h>
#include <carma/util/programLogging.h>
#include <carma/util/ScopedLogNdc.h>
#include <carma/util/Time.h>
#include <carma/util/Trace.h>
using namespace carma::util;

using namespace ::std;
using namespace ::CORBA;
using namespace carma;
using namespace carma::alarm;


AlarmControlROH::AlarmControlROH( ) :
doName_( ALARM_NAME ),
objRefState_( OBJ_REF_STATE_NEVER_SET ),
objRef_(),
haveGoodPrevCalls_( false ),
prevCallsCallString_(),
prevCallsUnloggedMjdVec_()
{
    prevCallsUnloggedMjdVec_.reserve( 8 );
}


AlarmControlROH::~AlarmControlROH( )
try {
    flushPrevCallsLogging( true );
} catch ( ... ) {
    // just stifle any exception

    return;
}


void
AlarmControlROH::setObjRefStateToResolved( )
{
    const ObjRefState oldObjRefState = objRefState_;

    objRefState_ = OBJ_REF_STATE_RESOLVED;

    if ( oldObjRefState != OBJ_REF_STATE_RESOLVED )
        programLogInfoIfPossible( "Marking " + doName_ + " as resolved" );
}


void
AlarmControlROH::setObjRefStateToConnected( )
{
    const ObjRefState oldObjRefState = objRefState_;

    objRefState_ = OBJ_REF_STATE_CONNECTED;

    if ( oldObjRefState != OBJ_REF_STATE_CONNECTED )
        programLogInfoIfPossible( "Marking " + doName_ + " as connected" );
}


void
AlarmControlROH::setObjRefStateToDisconnected( )
{
    const ObjRefState oldObjRefState = objRefState_;

    objRefState_ = OBJ_REF_STATE_DISCONNECTED;

    if ( oldObjRefState != OBJ_REF_STATE_DISCONNECTED )
        programLogErrorIfPossible( "Marking " + doName_ + " as disconnected" );
}


bool
AlarmControlROH::resolveObjRef( )
try {
    // const ScopedLogNdc ndc( "AlarmControlROH::resolveObjRef" );

    bool done = false;
    bool validatedObjRef = false;

    const int maxAttempts = 1;
    int attemptNo = 0;

    while ( (done == false) && (attemptNo < maxAttempts) ) {
        ++attemptNo;

        try {
            carma::corba::Client & client 
                = Program::getProgram().getCorbaClient();
            const AlarmControl_var newObjRef =
               client.resolveName< AlarmControl >( doName_ );

            if ( CORBA::is_nil( newObjRef ) == false ) {
                done = true;

                objRef_ = newObjRef;

                if ( CORBA::is_nil( objRef_ ) == false ) {
                    validatedObjRef = true;
                    setObjRefStateToResolved();
                }
            }
        } catch ( ... ) {
            CARMA_CPTRACE( Trace::TRACE1,
                           "Resolve attempt #" << attemptNo << " failed" );

            // Just stifle any exception
        }
    }  // End while loop

    if ( validatedObjRef == false )
        setObjRefStateToDisconnected();

    return validatedObjRef;
} catch ( ... ) {
    setObjRefStateToDisconnected();

    throw;
}


bool
AlarmControlROH::isObjReachable( const bool alarmSubsysCurrent )
{
    // const ScopedLogNdc ndc( "ROHB::isObjReachable " + doName_ );

    if ( (objRefState_ == OBJ_REF_STATE_CONNECTED) ||
         (objRefState_ == OBJ_REF_STATE_RESOLVED) )
        return true;

    enum {
        UNKNOWN_REASON,
        SUBSYSTEM_NOT_CURRENT_REASON,
        DID_NOT_RESOLVE_SYSTEM_REASON
    } reason = UNKNOWN_REASON;

    if ( (objRefState_ != OBJ_REF_STATE_NEVER_SET) &&
         (alarmSubsysCurrent != true) )
        reason = SUBSYSTEM_NOT_CURRENT_REASON;
    else {
        reason = DID_NOT_RESOLVE_SYSTEM_REASON;

        resolveObjRef();
    }

    const bool result = ((objRefState_ == OBJ_REF_STATE_CONNECTED) ||
                         (objRefState_ == OBJ_REF_STATE_RESOLVED));

    if ( result != true ) {
        string msg = doName_ + " is not reachable because ";

        switch ( reason ) {
            case UNKNOWN_REASON:
                msg += "of an unknown reason";
                break;

            case SUBSYSTEM_NOT_CURRENT_REASON:
                msg += "it's associated monitor subsystem is not current";
                break;

            case DID_NOT_RESOLVE_SYSTEM_REASON:
                msg += "the DO object ref did not resolve";
                break;
        }

        flushPrevCallsLogging( true );
        programLogErrorIfPossible( msg );
    }

    return result;
}


AlarmControl_var
AlarmControlROH::remoteObj( )
{
    if ( (objRefState_ != OBJ_REF_STATE_CONNECTED) &&
         (objRefState_ != OBJ_REF_STATE_RESOLVED) ) {
        string msg;
        {
            ostringstream oss;

            oss << "Bad remote object access attempted on " << doName_
                << " with a objRefState_ of ";

            switch ( objRefState_ ) {
                case OBJ_REF_STATE_NEVER_SET:
                    oss << "OBJ_REF_STATE_NEVER_SET";
                    break;

                case OBJ_REF_STATE_DISCONNECTED:
                    oss << "OBJ_REF_STATE_DISCONNECTED";
                    break;

                default:
                    oss << objRefState_;
                    break;
            }

            msg = oss.str();
        }

        flushPrevCallsLogging( true );
        programLogError( msg );

        throw CARMA_ERROR( msg );
    }

    if ( CORBA::is_nil( objRef_ ) ) {
        const string msg =
            doName_ + " objRef_ is NIL in AlarmControlROH::remoteObj()";

        flushPrevCallsLogging( true );
        programLogError( msg );

        throw CARMA_ERROR( msg );
    }

    return objRef_;
}


void
AlarmControlROH::flushPrevCallsLogging( const bool forceTransition )
{
    if ( haveGoodPrevCalls_ &&
         (prevCallsUnloggedMjdVec_.empty() == false) ) {
        ostringstream oss;

        oss << prevCallsCallString_ << " invoked on " + doName_;

        const vector< double >::const_iterator iBegin =
            prevCallsUnloggedMjdVec_.begin();

        const size_t numUnloggedCalls = prevCallsUnloggedMjdVec_.size();

        if ( numUnloggedCalls == 1 ) {
            oss << " at " << Time::getTimeString( *iBegin, 3 );
        } else {
            oss << " " << numUnloggedCalls << " times. Calls at: ";

            const vector< double >::const_iterator iEnd =
                prevCallsUnloggedMjdVec_.end();

            vector< double >::const_iterator i = iBegin;

            for ( ; i != iEnd; ++i ) {
                if ( i != iBegin )
                    oss << ", ";

                // digits after decimal for seconds
                // (3 = millisecond level precision)
                oss << Time::getTimeString( *i, 3 );
            }
        }

        programLogInfoIfPossible( oss.str() );
    }

    prevCallsUnloggedMjdVec_.clear();
    if ( forceTransition )
        haveGoodPrevCalls_ = false;
}


void
AlarmControlROH::logGoodCall( const string & callString,
                              const double   callMjd,
                              const size_t   maxConsecPerLog )
{
    const bool transition =
        ((haveGoodPrevCalls_ == false) ||
         (prevCallsCallString_ != callString));

    if ( transition ) {
        flushPrevCallsLogging( true );
        prevCallsCallString_ = callString;
    }

    haveGoodPrevCalls_ = true;
    prevCallsUnloggedMjdVec_.push_back( callMjd );

    // Always immediately log the first call after a transition
    if ( transition || (prevCallsUnloggedMjdVec_.size() >= maxConsecPerLog) )
        flushPrevCallsLogging( false );
}


void
AlarmControlROH::processCaught( const string & callString )
{
    ostringstream oss;

    oss << callString << " exception - ";

    try {
        throw;
    } catch ( const util::UserException & carmaUserException ) {
        setObjRefStateToConnected();

        oss << "carma::util::UserException "
            << carmaUserException._name() << " raised."
            << " Message - " << carmaUserException.errorMsg << "."
            << " File - " << carmaUserException.fileName << "."
            << " Line - " << carmaUserException.lineNo << ".";
    } catch ( const CORBA::UserException & corbaUserException ) {
        setObjRefStateToConnected();

        oss << "CORBA::UserException "
            << corbaUserException._name() << " raised."
            << " Reason - "
            << corbaUserException._info().c_str(); // Tao specific
    } catch ( const CORBA::SystemException & corbaSystemException ) {
        setObjRefStateToDisconnected();

        oss << "CORBA::SystemException "
            << corbaSystemException._name() << " raised."
            << " Reason - "
            << corbaSystemException._info().c_str(); // Tao specific 
    } catch ( const CORBA::Exception & corbaException ) {
        setObjRefStateToDisconnected();

        oss << "CORBA::Exception "
            << corbaException._name() << " raised."
            << " Reason - "
            << corbaException._info().c_str(); // Tao specific
    } catch ( ... ) {
        setObjRefStateToConnected();

        oss << getStringForCaught();
    }

    flushPrevCallsLogging( true );
    programLogErrorIfPossible( oss.str() );
}


bool AlarmControlROH::setState(const bool alarmOn,
                               const std::string &sound,
                               const std::string &mpName)
{
    const std::string reason = "fault system alarm";
    const double callMjd = Time::MJD();
    bool result = false;

    if (alarmOn) {
        if (isObjReachable(true)) {
            std::ostringstream oss;
            oss << "AlarmControl::turnOn(\"" << sound << "\", "
                << "\"" << mpName << "\", "
                << "\"" << reason << "\", True)";

            const std::string callString = oss.str();

            try {
                remoteObj()->turnOn(sound.c_str(), mpName.c_str(), reason.c_str(), true);
                setObjRefStateToConnected();
                result = true;
            } catch (...) {
                flushPrevCallsLogging(true);
                processCaught(callString);
                result = false;
            }

            if (result)
                logGoodCall(callString, callMjd, 1);
        } else {
            flushPrevCallsLogging(true);
        }
    } else {
        if (isObjReachable(true)) {
            const std::string callString = "AlarmControl::turnOff()";

            try {
                remoteObj()->turnOff();
                setObjRefStateToConnected();
                result = true;
            } catch (...) {
                flushPrevCallsLogging(true);
                processCaught(callString);
                result = false;
            }

            if (result)
                logGoodCall(callString, callMjd, 8);
        } else {
            flushPrevCallsLogging(true);
        }
    }

    return result;
}

/* vim: set ts=4 sts=4 sw=4 et: */
