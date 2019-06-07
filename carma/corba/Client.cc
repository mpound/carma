#include "carma/corba/Client.h"
#include "carma/corba/corba.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Trace.h"

#include <iostream>
#include <tao/corba.h>
#include <tao/Messaging/Messaging.h>
#include <vector>

using namespace carma::corba;
using namespace carma::util;
using namespace std;

namespace { 

const carma::util::Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE2;

CORBA::Object_ptr
resolveInitialReference( CORBA::ORB_ptr orb, const ::std::string & name ) 
{
    CORBA::Object_var objectVar;

    try { 

        objectVar = orb->resolve_initial_references( name.c_str() );

    } catch ( const CORBA::ORB::InvalidName & ) {
        // This is thrown when an invalid name is pased to resolve_init...
        // For example resolve_initial_references( "NamingService" ) will 
        // throw this because it only understands "NameService". 
        const string nameStr( name );
        throw CARMA_EXCEPTION( IllegalArgumentException, 
                               nameStr + " is invalid." );
    } 

    if ( CORBA::is_nil( objectVar ) ) {
        ostringstream err;
        err << "carma::corba::detail::resolveInit( orb, name=" << name 
            << " ) - Unable to resolve initial reference.";
        throw CARMA_EXCEPTION( ErrorException, err.str() );
    }

    return objectVar._retn( );  // Relinquish ownership of objectVar
}

} // namespace < unnamed >

bool
detail::isNil( CORBA::Object_ptr objPtr ) 
{
    return CORBA::is_nil( objPtr );
}

struct Client::Private {

    std::vector< std::string > lArgv;
    CORBA::ORB_var orb;

}; // struct Client::Private

Client::PrivateInline::PrivateInline( Client::Private & privInst ) :
    private_( privInst )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "Client::PrivateInline - Ctor." );
}

CORBA::Object_ptr
Client::PrivateInline::resolveInit( const ::std::string & name ) 
{
    return resolveInitialReference( private_.orb, name ); 
}

void
Client::applyTimeoutPolicies( CORBA::ORB_ptr orb, const int rrtt )
{
    // Connection timeout refers to the amount of time allowed for a client
    // to connect to a corba server including location forwarding. 
    // In practice I do not know of a situation where this would trigger
    // as a network layer failure will trigger an exception from the runtime. 
    TimeBase::TimeT connection_timeout = 15 * 1000 * 1000 * 10; // 15 seconds.
    CORBA::Any connection_timeout_as_any;
    connection_timeout_as_any <<= connection_timeout;

    // Relative round trip timeout refers to the total amount of time it 
    // takes to make a corba call.  This is useful for cleaning up stale
    // resources at the very least (think Control System thread pool threads
    // stuck on dead connections).
    TimeBase::TimeT relative_rt_timeout = rrtt * 10000000UL; 
    CORBA::Any relative_rt_timeout_as_any;
    relative_rt_timeout_as_any <<= relative_rt_timeout;

    CORBA::PolicyList policy_list;
    policy_list.length( 2 );
    policy_list[0] = orb->create_policy( 
        TAO::CONNECTION_TIMEOUT_POLICY_TYPE,
        connection_timeout_as_any );
    policy_list[1] = orb->create_policy( 
        Messaging::RELATIVE_RT_TIMEOUT_POLICY_TYPE,
        relative_rt_timeout_as_any );

    CORBA::PolicyManager_var policy_manager = 
        detail::getNarrowedVar< CORBA::PolicyManager >( 
            resolveInitialReference( orb, "ORBPolicyManager" ) );

    policy_manager->set_policy_overrides( policy_list, CORBA::ADD_OVERRIDE );

    for ( CORBA::ULong i = 0; i < policy_list.length(); ++i ) 
        policy_list[i]->destroy();

    policy_list.length( 0 );
}

Client::Client( int argc, char * argv[], const int rrtt ) :
    private_( new Private ),
    privateInline_( new PrivateInline( *(private_.get()) ) )
{
    // Initialize CORBA runtime...

    // First make a copy of our command line args - the ORB mucks with these
    // and there's no guarantee that the passed in argc and argv aren't going 
    // to be mucked with externally - so I keep my own copy to be safe.
    for ( int arg = 0; arg < argc; ++arg )
        private_->lArgv.push_back( argv[ arg ] );

    // Process the imr keyword and tack it on as the -ORBDefaultInitRef
    // option to CORBA.  If -ORBDefaultInitRef is already specified we throw.
    if ( Program::getProgram().haveImrHostname() ) {

        for ( vector<string>::iterator i = private_->lArgv.begin();
                i != private_->lArgv.end(); ++i ) {
            if ( *i == "-ORBDefaultInitRef" )
                programLogWarnIfPossible( 
                    "Both imr=<hostname> and -ORBDefaultInitRef "
                    "have been specified on the command line - "
                    "using former, ignoring latter." );
        }

        string imr = Program::getProgram().getImrHostname( );

        if ( imr.find(":") == string::npos )
            imr += ":20000";

        private_->lArgv.push_back( "-ORBDefaultInitRef" );
        private_->lArgv.push_back( "corbaloc::" + imr );
    }

    // Form up an argc and argv which can be trusted to outlive the orb...
    int orbArgc = private_->lArgv.size();
    char * orbArgv[ orbArgc ];
    for ( int arg = 0; arg < orbArgc; ++arg ) {
        orbArgv[arg] = const_cast< char * >( private_->lArgv[ arg ].c_str() );
    }

    // Implement a separate orb per instance of this class
    static int orbInstanceCount = 0; 
    ostringstream orbIdStr;
    orbIdStr << "ClientORB" << orbInstanceCount++;

    // Initialize the orb
    private_->orb = CORBA::ORB_init( orbArgc, orbArgv, orbIdStr.str().c_str() );

    if ( CORBA::is_nil( private_->orb ) )
        throw CARMA_EXCEPTION( ErrorException, 
                               "Unable to initialize orb." );

    CosNaming::NamingContext_var rootNamingContext = 
        privateInline_->resolveInitRef< CosNaming::NamingContext >( "NameService" );

    if ( !detail::isNil( rootNamingContext ) ) {
        naming_ = std::auto_ptr< Client::Naming >( 
            new Client::Naming( rootNamingContext ) );

        try {
            CosNotifyChannelAdmin::EventChannelFactory_var ncf = 
                resolveName< CosNotifyChannelAdmin::EventChannelFactory >(
                    "NotifyEventChannelFactory" );
            if ( detail::isNil( ncf ) ) 
                programLogWarnIfPossible( "corba::Client() - "
                    "NotifyEventChannelFactory didn't resolve so client is "
                    "NOT using notification service!" );
            else
                notify_ = std::auto_ptr< Client::Notify >(
                    new Client::Notify( *(naming_.get()), ncf ) );
        } catch (...) {
            const string exMsg( getStringForCaught() );
            programLogWarnIfPossible( "corba::Client() - "
                "NotifyEventChannelFactory didn't resolve on exception so "
                "client is NOT using NOTIFICATION service!  Exception is: "
                + exMsg );
        }
    } else {
        programLogWarnIfPossible( "corba::Client() - Naming service didn't "
            "resolve so client is NOT using naming nor notification service!" );
    }
    
    applyTimeoutPolicies( private_->orb, rrtt );

}

Client::Client( CORBA::ORB_ptr orb, const int rrtt ) :
    private_( new Private ),
    privateInline_( new PrivateInline( *(private_.get()) ) )
{
    if ( CORBA::is_nil( orb ) )
        throw CARMA_EXCEPTION( ErrorException, "Input orb is nil." );

    private_->orb = CORBA::ORB::_duplicate( orb );
    
    CosNaming::NamingContext_var rootNamingContext = 
        privateInline_->resolveInitRef< CosNaming::NamingContext >( "NameService" );
    
    if ( !detail::isNil( rootNamingContext ) ) {
        naming_ = std::auto_ptr< Client::Naming >( 
            new Client::Naming( rootNamingContext ) );

        try {
            CosNotifyChannelAdmin::EventChannelFactory_var eventChannelFactory =
                resolveName< CosNotifyChannelAdmin::EventChannelFactory >(
                    "NotifyEventChannelFactory" );
            if ( detail::isNil( eventChannelFactory ) ) 
                programLogWarnIfPossible( "corba::Client() - "
                    "NotifyEventChannelFactory didn't resolve so client is "
                    "NOT using notification service!" );
            else
                notify_ = std::auto_ptr< Client::Notify >(
                    new Client::Notify( *(naming_.get()), eventChannelFactory));
        } catch (...) {
            const string exMsg( getStringForCaught() );
            programLogWarnIfPossible( "corba::Client() - "
                "NotifyEventChannelFactory didn't resolve on exception so "
                "client is NOT using NOTIFICATION service!  Exception is: "
                + exMsg );
        }
    } else {
        programLogWarnIfPossible( "corba::Client() - Naming service didn't "
            "resolve so client is NOT using naming nor notification service!" );
    }

    applyTimeoutPolicies( private_->orb, rrtt );
}

Client::~Client( ) 
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "Client::~Client() - Dtor." );
}

