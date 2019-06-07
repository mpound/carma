//!
//! @file
//!
//! Manages connection to remote DO.
//!
//! Author: N. S. Amarnath
//!

#ifndef CARMA_REMOTE_OBJ_HANDLE_T_H
#define CARMA_REMOTE_OBJ_HANDLE_T_H

#include <string>

#include "carma/corba/corba.h"
#include "carma/corba/Client.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"


namespace carma  {

namespace monitor {

class MonitorPointBool;
class MonitorSubsystem;
class MonitorSystem;

}  // namespace carma::monitor


namespace control  {


template < typename T >
class RemoteObjMethodFunctorBase;


//! @brief Common base class for managing connections to DOs.
class RemoteObjHandleBase {
    public:
        /**
         * Constructor
         * @param doName name of the Direct Object that is the object 
                  of affection of this handle
         * @param mpReachable a monitor point for reachability of this DO
                  Must be part of a Control.SubarrayX.Reachable monitor
                  point subsystem that is routinely written by the control
                  system. This MP is both read and write.
         * @param subsystem The monitor subsystem, often running on a remote
                  machine, that is used to determine whether to try and remake
                  the connect on the next IO attempt. Usually, if the monitor
                  subsystem returns false from its isCurrent() method, no
                  attempt is made to remake the connection
                  (covers the use case of a disconnected machine, 
                  like an antenna during a move).
         * @param system A pointer to a monitor system which is used to do
                  a readNewest() to update both the isCurrent state of the
                  subsystem and the reachable MP.
         * @param defaultLogIfNotReachable Controls logging if the DO is not
                  reachable.
         * @param defaultLogSentCommands controls logging of commands
         */
        RemoteObjHandleBase(
            const ::std::string &             doName,
            monitor::MonitorPointBool *       mpReachable,
            const monitor::MonitorSubsystem * subsystem,
            monitor::MonitorSystem    *       system,
            bool                              defaultLogIfNotReachable,
            bool                              defaultLogSentCommands );
            
        //! @brief Destructor
        virtual ~RemoteObjHandleBase( );

        //! @brief get the DO name
        ::std::string doName( ) const;

        //! @brief Force a full re-lookup of the DO by name
        void forceFullReconnect( );

        //! @brief Try reconnect to the DO if needed
        //!
        //! The minimum work necessary to reestablish a broken connection will
        //! be done. Hence nothing will be done if the connection did not have
        //! a problem the last time it was used.
        //! Returns true if a connection is established (or was previously
        //! established) and false if it could not connect. The subsystem
        //! monitor system is not checked.
        bool attemptToReconnectIfNeeded( );

        /**
         * If state is 'not reachable' and monitor system is current,
         * tries to reconnect.
         */
        bool isObjReachable( );
        bool isObjReachable( bool logIfNotReachable );

    protected:
        bool getDefaultLogIfNotReachable( ) const;

        bool getDefaultLogSentCommands( ) const;
    
        void markObjRefValid( );
        void invalidateObjRef( );
        
        void throwIfObjRefIsNotValid( ) const;

        virtual bool resolveObjRef( ) = 0;
        
        void throwBadRemoteObjAccess( ) const;

        void logException( const ::std::string & callString,
                           const ::std::string & exString ) const;

        void logSentCommand( const ::std::string & callString,
                             const double          mjd ) const;
        void logSentCommand( const ::std::string & callString,
                             const double          mjd,
                             const ::std::string & subDoName ) const;

        void logSentCommandIfNeeded( const ::std::string & callString,
                                     const double          mjd ) const;
        void logSentCommandIfNeeded( const ::std::string & callString,
                                     const double          mjd,
                                     const ::std::string & subDoName ) const;

        void processException( const ::std::string &      callString,
                               const ::CORBA::Exception & corbaException ) ;

        void invalidateObjRefIfNeededForCaught( );
        
    private:
        void setReachableMpValue( bool value );

        const ::std::string doName_;
        
        const bool defaultLogIfNotReachable_;
        const bool defaultLogSentCommands_;
        
        monitor::MonitorPointBool * const mpReachable_;

        const monitor::MonitorSubsystem * const monitorSubsystem_;
        monitor::MonitorSystem * const monitorSystem_;

        typedef enum {
            OBJ_REF_VALIDITY_NEVER_SET,
            OBJ_REF_VALIDITY_BAD,
            OBJ_REF_VALIDITY_GOOD
        } ObjRefValidity;
        
        ObjRefValidity objRefValidity_;
};


//! @brief Typed class for managing connections to DOs.
template < typename T >
class RemoteObjHandleT : public RemoteObjHandleBase {
    friend class RemoteObjMethodFunctorBase< T >;
    
    public:
        /**
         * Constructor
         * @brief Constructs reference to a DO using the name
         * Resolves name using NameService and sets handle.
         *
         * @param doName, name bound to DO's IOR in NameService
         * @param monitor::MonitorPointBool * const, pointer to
         *        boolean monitor point which indicates whether DO
         *        is currently contactable.
         * @param monitor::MonitorSubsystem * const, pointer to
         *        monitor subsystem associated with this handle - used
         *        as an indicator of subsystem health.
         */
        explicit RemoteObjHandleT(
            const ::std::string &             doName,
            monitor::MonitorPointBool *       mpReachable,
            const monitor::MonitorSubsystem * subsystem,
            monitor::MonitorSystem    *       system,
            bool                              defaultLogIfNotReachable,
            bool                              defaultLogSentCommands );

        //! @brief Destructor
        //!
        //! Releases all DO references.
        virtual ~RemoteObjHandleT( );


        /**
         * Returns remote object handle as var.
         *
         * @return IOR of DO as ObjVar specialized to type T.
         */
        typename T::_var_type remoteObj( ) const;

        /**
         * Returns remote object handle as var.
         *
         * @return IOR of DO as ObjVar specialized to type S.
         */
        template < typename S >
        typename S::_var_type
        narrowedRemoteObj( ) const
        {
            return S::_narrow( objRef_ );
        }

    protected:
        virtual bool resolveObjRef( );

    private:
        typename T::_var_type objRef_;
};


}  // namespace carma::control
}  // namespace carma


inline ::std::string
carma::control::RemoteObjHandleBase::doName( ) const
{
    return doName_;
}


inline bool
carma::control::RemoteObjHandleBase::getDefaultLogIfNotReachable( ) const
{
    return defaultLogIfNotReachable_;
}


inline bool
carma::control::RemoteObjHandleBase::getDefaultLogSentCommands( ) const
{
    return defaultLogSentCommands_;
}

/*
inline carma::monitor::MonitorSystem const * 
carma::control::RemoteObjHandleBase::getMonitorSystem( ) const
{
    return monitorSystem_;
}
*/


template < typename T >
carma::control::RemoteObjHandleT< T >::RemoteObjHandleT(
    const ::std::string &                   doName,
    monitor::MonitorPointBool * const       mpReachable,
    const monitor::MonitorSubsystem * const subsystem,
    monitor::MonitorSystem    * const       system,
    const bool                              defaultLogIfNotReachable,
    const bool                              defaultLogSentCommands ) :
RemoteObjHandleBase( doName,
                     mpReachable,
                     subsystem,
                     system,
                     defaultLogIfNotReachable,
                     defaultLogSentCommands ),
objRef_()
{
}


template < typename T >
carma::control::RemoteObjHandleT< T >::~RemoteObjHandleT( )
try {
} catch ( ... ) {
    // just stifle any exception

    return;
}


template < typename T >
typename T::_var_type
carma::control::RemoteObjHandleT< T >::remoteObj( ) const
{
    throwIfObjRefIsNotValid();
    
    if ( CORBA::is_nil( objRef_ ) )
        throwBadRemoteObjAccess();
        
    return objRef_;
}


// Can retry many times before giving up; only logging the final failure.
// But now setup to try only once. 
// There is no sleep in the loop.
// Crude, but gets around some of the startup sync issues for now.
template < typename T >
bool
carma::control::RemoteObjHandleT< T >::resolveObjRef( )
try {
    bool done = false;
    bool validatedObjRef = false;
    
    const int maxAttempts = 1;
    int attemptNo = 0;

    CPTRACE( util::Trace::TRACE1, " resolveObjRef(), doName()=" << doName() );

    while ( (done == false) && (attemptNo < maxAttempts) ) {
        ++attemptNo;

        try {
            const typename T::_var_type newObjRef =
                carma::util::Program::getProgram().getCorbaClient().resolveName< T >( doName() );
                
            if ( CORBA::is_nil( newObjRef ) == false ) {
                done = true;
                
                objRef_ = newObjRef;
                
                if ( CORBA::is_nil( objRef_ ) == false ) {
                    validatedObjRef = true;
                    markObjRefValid();
                }
            }
        } catch ( ... ) {
            CARMA_CPTRACE( util::Trace::TRACE1,
                           "Resolve attempt #" << attemptNo << " failed" );

            // Just stifle any exception
        }
    }  // End while loop

    if ( validatedObjRef == false )
        invalidateObjRef();
        
    return validatedObjRef;
} catch ( ... ) {
    invalidateObjRef();
    
    throw;
}


#endif
