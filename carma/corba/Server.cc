#include "carma/corba/Server.h"

#include "carma/corba/Client.h"
#include "carma/corba/corba.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedPthreadMutexLock.h"

#include <boost/thread.hpp>
#include <orbsvcs/CosNotifyChannelAdminC.h>

using namespace carma::util;
using namespace std;

/* 

Notes on corba::Server shutdown.

Shutdown is particularly onerous with this class and because of this I outline
some of my design decisions here.  There are 8 known shutdown scenarios which 
I tried to accomodate:
1) Server::run() w/ DO implemented call to Server::stop()
2) Server::work() w/ DO implemented call to Server::stop()
3) Server::run() w/ IMR termination.
4) Server::work() w/ IMR termination.
5) Server::run() w/ separate thread calling Server::stop() (non-DO)
6) Server::work() w/ separate thread calling Server::stop() (non-DO)
7) Server::work() w/ same thread calling Server::stop() (non-DO)
8) Server::stop() when neither Server::work nor Server::run has been invoked.

Scenarios 1 & 2 led to use of the PTHREAD_RECURSIVE_MUTEX as well as the need
to call shutdown( wait=false ).  In addition asynchronous shutdown via any
scenario outlined in 1-4 necessitated the Server::terminate() method.  This 
allows a user to poll as to whether or not the server is still running 
(Server::run() will simply exit).  

The main guarantee I make is that when Server::stop() returns, in all but cases
1 and 5, terminate is guaranteed to return true.  In cases 1 and 5 a user must
wait until terminate returns true before being guaranteed that the server is 
idle.  Even then case 1 doesn't really apply here since it is invoked remotely.

*/

namespace {
    
const Trace::TraceLevel TRACE_SERVER( Trace::TRACE5 );
const Trace::TraceLevel TRACE_THREADING( Trace::TRACE2 );

string 
parseORBOption( const int argc, const char * const argv[], const string & option )
{
    string orbOption = "";
    for ( int arg = 0; arg < argc; ++arg )
    {
        if ( string( argv[arg] ) == option &&
                arg + 1 < argc ) {
            orbOption = string( argv[arg + 1 ] );
            break;
        }
    }

    return orbOption;
}

typedef std::pair< string, string > HostPortPair;

HostPortPair
parseImrHostAndPort( const string & imr )
{
    // Host
    string imrhost( imr );

    const string corbaloc( "corbaloc::" );
    if ( imrhost.find( corbaloc ) != string::npos ) 
        imrhost = imrhost.substr( corbaloc.size() );

    const string::size_type colonIdx( imrhost.find( ":" ) );
    if ( colonIdx != string::npos ) 
        imrhost = imrhost.substr( 0, colonIdx );  

    // Port 
    string port;

    const string::size_type lastColonIdx( imr.rfind( ":" ) );
    if ( lastColonIdx != string::npos && lastColonIdx + 1 < imr.size() ) 
        port = imr.substr( lastColonIdx + 1 );

    return std::make_pair( imrhost, port );
}

enum ServerStateEnum {
    INITIALIZED, // Orb initialized but run or work not yet called.
    RUNNING,     // Orb::perform_work() or Orb::run() invoked or invoking.
    STOPPING,    // Stop() called but run() or perform_work must finish.
    STOPPED      // Definitively stopped.
};

} // namespace < unnamed >

struct carma::corba::Server::PrivateImplementation
{
    CORBA::ORB_var          orb;
    PortableServer::POA_var poa;
    PortableServer::POAManager_var manager;

    std::vector< std::string > lArgv;  // Local argv.
    unsigned int objectCount;
    mutable PthreadMutex objectCountMutex;
    auto_ptr< carma::corba::Client > client;

    enum ServerStateEnum serverState;
    mutable PthreadMutex serverStateMutex;

    boost::thread_group serverThreads;
    static void serverThread( Server & server );

    PrivateImplementation( ) : 
        objectCount( 0 ),
        objectCountMutex( ),
        serverState( INITIALIZED ),
        serverStateMutex( ::PTHREAD_MUTEX_RECURSIVE ),
        serverThreads( )
    { };

};

carma::corba::Server::Server( int argc, char * argv[], const int rrtt ) :
    privateImpl_( new carma::corba::Server::PrivateImplementation() ),
    privateInl_( new carma::corba::Server::PrivateInline( *privateImpl_ ) )
{
    CARMA_CPTRACE( TRACE_SERVER, "Server::Server( ) - Inside constructor." );
    
    static int orbInstanceCount = 0; 
    static PthreadMutex orbInstanceCountMutex;

    const ScopedPthreadMutexLock orbInstanceCountGuard( orbInstanceCountMutex );

    // First make a copy of our command line args - the ORB mucks with these
    // and there's no guarantee that the passed in argc and argv aren't going 
    // to be mucked with externally - so I keep my own copy to be safe.
    for ( int arg = 0; arg < argc; ++arg )
        privateImpl_->lArgv.push_back( argv[ arg ] );

    // Only one Server instance (orb) can register with the IMR.
    // For now I define this as the first one and change all subsequent
    // instances to -ORBUseImr 0.
    if ( orbInstanceCount > 0 ) {
        const string orbUseImr( "-ORBUseImr" );
        vector< string >::iterator i = find( privateImpl_->lArgv.begin(),
                                             privateImpl_->lArgv.end(),
                                             orbUseImr );
        if ( i != privateImpl_->lArgv.end() ) {
            i++;
            if ( i != privateImpl_->lArgv.end() )
                *i = "0";
        }
    }

    // Parse and modify command line options as needed. 
    // WARNING: ORB_init mangles argc and argv so you should not
    // use them after you pass them to ORB_init.
    const string defaultInitRef( parseORBOption(argc, argv, "-ORBDefaultInitRef") );
    const string serverId( parseORBOption(argc, argv, "-ORBServerId") );

    // Process the imr keyword and tack it on as the -ORBDefaultInitRef
    // option to CORBA.  If -ORBDefaultInitRef is already specified we throw.
    if ( Program::getProgram().haveImrHostname() ) { 

        const string imrhostcl( Program::getProgram().getImrHostname( ) );
        const HostPortPair imrHostAndPort( parseImrHostAndPort(imrhostcl) );
        const string imrhostname( imrHostAndPort.first );
        const string imrport( imrHostAndPort.second );

        // First see if -ORBDefaultInitRef is already specified.
        if ( defaultInitRef != "" ) { // It is, but...

            const HostPortPair initRefImrHostAndPort( 
                parseImrHostAndPort( defaultInitRef ) );
            // Do the hostnames match?
            const string defaultInitRefHostname( initRefImrHostAndPort.first );
            if ( defaultInitRefHostname != imrhostname ) {
                ostringstream err;
                err << "Server::Server( ) - Both the imr keyword and "
                    << "-ORBDefaultInitRef option have been specified with "
                    << "different hostnames - " << imrhostname << " and " 
                    << defaultInitRefHostname << " respectively.";
                throw CARMA_ERROR( err.str() );
            }

            // Is a port specified?
            const string defaultInitRefPort( initRefImrHostAndPort.second );
            if ( defaultInitRefPort == "" ) {
                throw CARMA_ERROR( "Server::Server() - ORBDefaultInitRef "
                    "specified without a port number." );
            }

            // Do the specified ports match?
            if ( imrport != ""  && defaultInitRefPort != imrport ) {
                ostringstream err;
                err << "Server::Server( ) - Both the imr keyword and "
                    << "-ORBDefaultInitRef option have been specified with "
                    << "different ports - " << imrport << " and " 
                    << defaultInitRefPort << " respectively.";
                throw CARMA_ERROR( err.str() );
            }

        } else { // -ORBDefaultInitRef is not specified

            // Tack it onto command line.
            privateImpl_->lArgv.push_back( "-ORBDefaultInitRef" );
            privateImpl_->lArgv.push_back( "corbaloc::" + imrhostname + ":" +
                ( imrport == "" ? "20000" : imrport ) );

        }
    }

    // Form up local argc and argv which can be trusted to outlive the orb and
    // can be mucked with by ORB_init...
    int orbArgc = privateImpl_->lArgv.size();
    char * orbArgv[ orbArgc + 1 ]; // One element for null termination (see execve)
    for ( int arg = 0; arg < orbArgc; ++arg )  
        orbArgv[arg] = const_cast< char * >( privateImpl_->lArgv[ arg ].c_str() );
    orbArgv[ orbArgc ] = 0;

    // Implement a separate orb per instance of this class
    ostringstream orbIdString;
    orbIdString << "ServerORB" << orbInstanceCount++;

    // Initialize the orb
    privateImpl_->orb = CORBA::ORB_init( orbArgc, orbArgv, orbIdString.str().c_str() );

    CARMA_CPTRACE( TRACE_SERVER, "Server::Server() - Creating client." );
    privateImpl_->client = auto_ptr< Client >( 
        new Client( privateImpl_->orb, rrtt ) );

    // Ok, that was fun - let's create our POA now.
    CARMA_CPTRACE( TRACE_SERVER, "Server::Server() - Creating POA." );

    // If the user has specified the -ORBServerId option on the command line,
    // use that as our persistent POA name.  If not, create a generic POA name. 
    // Note that the IMR registers it's objects based on POA name, not process
    // name so setting the POA name to the server id is a way to guarantee it's
    // uniqueness systemwide.  Note also that the IMR won't complain if you
    // register multiple identical POA/Server names - last one wins.  Lame.
    string poaString;
    if ( serverId == "" || orbInstanceCount > 1 ) {  
        ostringstream oss;
        oss << "ServerPOA" << orbInstanceCount;
        poaString = oss.str();
    } else {
        poaString = serverId;
    }
    
    PortableServer::POA_var rootPOA = 
        privateImpl_->client->privateInline_->resolveInitRef< 
            PortableServer::POA >( "RootPOA" );

    privateImpl_->manager = rootPOA->the_POAManager();

    // define policy list - these are the standard CARMA 'persistent' options
    CORBA::PolicyList pl;
    pl.length(3);
    pl[0] = rootPOA->create_lifespan_policy( PortableServer::PERSISTENT );
    pl[1] = rootPOA->create_id_assignment_policy( PortableServer::USER_ID );
    pl[2] = rootPOA->create_implicit_activation_policy( 
        PortableServer::NO_IMPLICIT_ACTIVATION );

    privateImpl_->poa = rootPOA->create_POA( poaString.c_str(), 
                                             privateImpl_->manager, pl );

    // destroy the policies - they came from factories and aren't needed 
    for ( CORBA::ULong p = 0; p < pl.length(); ++p ) pl[p]->destroy();

    // Activate manager - this will start queueing requests even before
    // we run to process them.
    CARMA_CPTRACE( TRACE_SERVER, "Server::Server() - Activating POA Manager.");
    privateImpl_->manager->activate();

    // Prime 
    work();

    CARMA_CPTRACE( TRACE_SERVER, "Server::Server() - Done with C-tor." );
}

carma::corba::Server::~Server( )
try {
    
    CARMA_CPTRACE( TRACE_SERVER, "~Server() - Dtor." );

    { 
        ScopedPthreadMutexLock scopelock( privateImpl_->serverStateMutex );
        if ( privateImpl_->serverState != STOPPED ) {
            CARMA_CPTRACE( TRACE_SERVER, "~Server() - Stopping server." );
            stop();
        }
    }

    // Note I don't interrupt the thread group - stop is sufficient we just
    // need to wait for them to complete.
    CARMA_CPTRACE( TRACE_THREADING, "~Server( ) - joining all threads." );
    privateImpl_->serverThreads.join_all( );
        
    CARMA_CPTRACE( TRACE_SERVER, "~Server() - Deactivating POA manager." );
    const bool etherealize = false; // Only used if using servant activators
    const bool wait = true; // Wait for completion
    privateImpl_->manager->deactivate( etherealize, wait );

    CARMA_CPTRACE( TRACE_SERVER, "~Server() - Destroying POA." );
    privateImpl_->poa->destroy( etherealize, wait );

    CARMA_CPTRACE( TRACE_SERVER, "~Server() - Destroying ORB." );
    privateImpl_->orb->destroy();
} catch (...) {
    programLogErrorIfPossible( "~Server(): " + getStringForCaught() );
    return;
}

void
carma::corba::Server::work( )
{
    // See notes on shutdown at the top of this file.
    CARMA_CPTRACE( TRACE_SERVER, "Server::work()." );

    // Notes regarding perform_work.  Neither TAO, nor CORBA in general, 
    // provides an atomic way to to check for a unit of work and simultaneously
    // perform the work - rather it provides two calls: work_pending and 
    // perform_work.  Subsequently there is a race where work_pending can
    // return true, but another thread (including a CORBA client only thread) 
    // could come along and service the work
    // unit prior to entering into perform_work in this thread.  Since the
    // default behaviour of perform_work is to block if there is no unit of 
    // work, this effectively causes work to become a non-polling blocking call
    // which can lead to deadlocks.  To get around this, we pass in a 
    // timeout value of 0 to force perform_work to not block in this case.
    ACE_Time_Value zero(0,0);

    // Note that I purposely lock this for the duration of the call.  
    ScopedPthreadMutexLock scopelock( privateImpl_->serverStateMutex );

    if ( privateImpl_->serverState == INITIALIZED ) {
        privateImpl_->serverState = RUNNING;
        CARMA_CPTRACE( TRACE_SERVER, "Server::work() - RUNNING." );
    }

    if ( privateImpl_->serverState == STOPPED ) 
        throw CARMA_ERROR( "Server::work() - Server already stopped." );

    try {

        if ( privateImpl_->orb->work_pending() ) 
            privateImpl_->orb->perform_work( zero );

    } catch (...) {
        CARMA_CPTRACE( TRACE_SERVER, getStringForCaught() );
        privateImpl_->serverState = STOPPED;
        CARMA_CPTRACE( TRACE_SERVER, "Server::work() - STOPPED." );
    }

}

void
carma::corba::Server::run( const bool inSeparateThread )
{
    // See notes on shutdown at the top of this file.
    CARMA_CPTRACE( TRACE_SERVER, "Server::run()." );

    { 
        ScopedPthreadMutexLock scopelock( privateImpl_->serverStateMutex );

        if ( privateImpl_->serverState == INITIALIZED ) {
            privateImpl_->serverState = RUNNING;
            CARMA_CPTRACE( TRACE_SERVER, "Server::run() - RUNNING." );
        }

        if ( privateImpl_->serverState == STOPPED ) 
            throw CARMA_ERROR( "Server::run() - Server already stopped." );
    }

    if ( inSeparateThread ) {
        CARMA_CPTRACE( TRACE_THREADING, 
            "Server::run( ) - Adding new thread to thread group..." );
        boost::thread * newThread = new boost::thread( 
            &Server::PrivateImplementation::serverThread, boost::ref( *this ) );
        privateImpl_->serverThreads.add_thread( newThread ); // takes ownership
        CARMA_CPTRACE( TRACE_THREADING, 
            "Server::run( ) - Thread group now contains " 
            << privateImpl_->serverThreads.size() << " threads." );
    } else {

        try {
            privateImpl_->orb->run();
            CARMA_CPTRACE( TRACE_SERVER, "Server::run() - exiting cleanly." );
        } catch (...) {
            CARMA_CPTRACE( TRACE_SERVER, getStringForCaught() );
        }

        { 
            ScopedPthreadMutexLock scopelock( privateImpl_->serverStateMutex );
            privateImpl_->serverState = STOPPED;
            CARMA_CPTRACE( TRACE_SERVER, "Server::run() - STOPPED." );
        }

    }
    
    CARMA_CPTRACE( TRACE_THREADING, "Server::run( ) - exiting." );
}

void
carma::corba::Server::stop( )
{
    // See notes on shutdown at the top of this file.
    CARMA_CPTRACE( TRACE_SERVER, "Server::stop()." );

    // Note that I purposely lock this for the duration of the call.
    ScopedPthreadMutexLock scopelock( privateImpl_->serverStateMutex );

    if ( privateImpl_->serverState == INITIALIZED ) {
        const string warn( "Server::stop() - Never started, returning." );
        CARMA_CPTRACE( TRACE_SERVER, warn );
        privateImpl_->serverState = STOPPED;
        return;
    }

    if ( privateImpl_->serverState >= STOPPING ) { 
        const string warn( "Server::stop() - Already stopped, returning." );
        CARMA_CPTRACE( TRACE_SERVER, warn );
        programLogWarnIfPossible( warn );
        return;
    }

    try {
        const bool wait = false;
        privateImpl_->orb->shutdown( wait );
    } catch ( ... ) {
        const string warnMsg = getStringForCaught();  
        CARMA_CPTRACE( TRACE_SERVER, "Server::stop() - " + warnMsg );
        programLogWarnIfPossible( "Server::stop() - " + warnMsg ); 
    }
        
    if ( privateImpl_->serverState == RUNNING ) {
        privateImpl_->serverState = STOPPING;
        CARMA_CPTRACE( TRACE_SERVER, "Server::stop() - STOPPING." );
    }

    work(); // Trigger shutdown on running servers.
}

bool
carma::corba::Server::terminated( ) const
{
    ScopedPthreadMutexLock scopelock( privateImpl_->serverStateMutex );
    return privateImpl_->serverState == STOPPED;
}

carma::corba::Server::PrivateInline::PrivateInline( 
    Server::PrivateImplementation & pimpl ) : pimpl_( pimpl ) 
{
    // Nothing
}

PortableServer::POA_ptr 
carma::corba::Server::PrivateInline::poa()
{
    return pimpl_.poa;
}

void
carma::corba::Server::PrivateInline::activate( PortableServer::Servant servant )
{
    ostringstream objectName;
    { 
        const ScopedPthreadMutexLock objectCountGuard(pimpl_.objectCountMutex);
        objectName << "ServerObject" << ++( pimpl_.objectCount );
    }

    PortableServer::ObjectId_var oid = 
        PortableServer::string_to_ObjectId( objectName.str().c_str() );

    CARMA_CPTRACE( TRACE_SERVER, "Activating object." );

    // Adds servant to active object map.  refCount++.
    pimpl_.poa->activate_object_with_id( oid, servant );
}

void
carma::corba::Server::PrivateInline::publish( const string & name,    
                                              CORBA::Object_ptr objectPtr )
{
    if ( pimpl_.client->naming_.get() == 0 )
        throw CARMA_ERROR( "Server instructed to publish an object to the "
            "nameserver but it's not available.  Check that it is running "
            "and/or command line options to initialize it are correct." );
        
    pimpl_.client->naming_->addObject( name, objectPtr );
}

void 
carma::corba::Server::PrivateInline::connectConsumerToProxySupplier(
    const std::string & channelName,
    const std::string & proxyName,
    CosNotifyComm::StructuredPushConsumer_var spc )
{
    if ( pimpl_.client->notify_.get() == 0 )
        throw CARMA_ERROR( "Server instructed to connect to the notification "
                "service but it's not available.  Check that it is running "
                "and/or command line options to initialize it are correct." );

    try {
        // 'Connect' the channel to the notification service.
        CosNotifyChannelAdmin::StructuredProxyPushSupplier_var proxySupplier_;
        proxySupplier_ = pimpl_.client->notify_->getNotifyProxyPushSupplier(
                channelName, 
                proxyName );

        proxySupplier_->connect_structured_push_consumer( spc );

    } catch ( const CosEventChannelAdmin::AlreadyConnected & ) {
        const string errMsg( "Server::addNotificationServantFunctor( ) - "
                "A servant already exists for notification channel " 
                + channelName + " and proxy " + proxyName + ".  If you wish "
                "to receive multiple notifications on a single channel, you "
                "must use a distinct proxy name for each." );
        throw CARMA_ERROR( errMsg );
    } 
}

void
carma::corba::Server::PrivateImplementation::serverThread( Server & server ) 
try {

    if ( !server.terminated() ) 
        server.run( false );

} catch (...) {
    CARMA_CPTRACE( TRACE_SERVER, "serverThread exiting on exception: " 
                                  << getStringForCaught() );
}

carma::corba::Client &
carma::corba::Server::client( ) 
{
    return *privateInl_->pimpl_.client;
}
