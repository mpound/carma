/**
 * $Id: ImrClient.cc,v 1.22 2013/04/09 22:48:02 abeard Exp $
 * ImrClient - a client for connecting to the IMR and calling admin
 * functions
 *
 */

#include "carma/util/ImrClient.h"

#include "carma/corba/Client.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "carma/util/Logger.h"
#include "carma/util/ErrorException.h"

#include <tao/ImR_Client/ImplRepoC.h>

using namespace ::std;
using namespace carma;
using namespace carma::util;

class ImrClient::Pimpl { 
public:

    explicit Pimpl( corba::Client & client, const std::string & imrHost );

    /* virtual */ ~Pimpl( );
    
    bool addOad(std::string oadHost);
    bool removeOad(std::string oadHost );
    bool addServer(std::string serverName,
                   std::string oadHost,
                   std::string execPath,
                   std::string args,
                   std::string optArgs,
                   std::string runDir );
    bool removeServer(std::string serverName);
    bool startServer(std::string serverName);
    bool stopServer(std::string serverName);
    bool resetServer(std::string serverName);
    ServerStatus getServerStatus(std::string serverName);

private:

    const std::string imrHostname_;

    const ::CORBA::Long maxRespawns_;

    ImplementationRepository::Administration_var imrDomain_;
    
}; // Class ImrClient::Pimpl 

// -----------------------------------------------------------------------------
// TAO implementation

ImrClient::Pimpl::Pimpl( corba::Client & client, const string & imrHost ) : 
    imrHostname_( imrHost ),
    maxRespawns_( 1 )
{
    imrDomain_ = client.resolveName< ImplementationRepository::Administration>(
        "ImplRepoService", true );
}

ImrClient::Pimpl::~Pimpl( )
{
    // Nothing
}

bool 
ImrClient::Pimpl::addOad( string oadHost )
{
    // No-op - TAO doesn't have IMR controlled OADs.
    return true;
}

bool 
ImrClient::Pimpl::removeOad( std::string oadHost )
{
    // No-op - TAO doesn't have IMR controlled OADs.
    return true;
}

bool 
ImrClient::Pimpl::addServer( std::string serverName,
                             std::string oadHost,
                             std::string execPath,
                             std::string args,
                             std::string optArgs,
                             std::string runDir )
try {

    CARMA_CPTRACE( Trace::TRACE7, 
                   "ImrClient::Pimpl::addServer( " << serverName << " )" );

    ImplementationRepository::StartupOptions opts;
    
    string imrHostAndPort = imrHostname_;
    if ( imrHostname_.find( ':' ) == string::npos ) 
        imrHostAndPort += ":20000";
    
    // For TAO we need both imr= and -- -ORBDefaultInitRef on the command line.
    const string cl( execPath + " " + args + " imr=" + imrHostAndPort 
        + " -- -ORBDefaultInitRef corbaloc::" 
        + imrHostAndPort + " -ORBUseImr 1 -ORBServerId " 
        + serverName + " " + optArgs );
    opts.command_line = cl.c_str();
    opts.working_directory = runDir.c_str();
    opts.activation = ImplementationRepository::MANUAL;
    opts.activator = oadHost.c_str();
    opts.start_limit = maxRespawns_;

    imrDomain_->add_or_update_server( serverName.c_str(), opts );

    return true;
} catch (...) {
    // Stifle
    return false;
}
                             
bool 
ImrClient::Pimpl::removeServer( std::string serverName )
try {
    imrDomain_->remove_server( serverName.c_str() );
    return true;
} catch (...) {
    // Stifle
    return false;
}

bool 
ImrClient::Pimpl::startServer(std::string serverName)
try {
    imrDomain_->activate_server( serverName.c_str() );
    return true;
} catch (...) {
    // Stifle
    return false;
}

bool 
ImrClient::Pimpl::stopServer(std::string serverName)
try {
    imrDomain_->shutdown_server( serverName.c_str() );
    return true;
} catch (...) {
    // Stifle
    return false;
}

bool 
ImrClient::Pimpl::resetServer(std::string serverName)
try {
    // No-op for now, not sure if TAO let's you reset the spawn count
    // or if I'd be required to re-register the server.
    return true;
} catch (...) {
    // Stifle
    return false;
}

ImrClient::ServerStatus 
ImrClient::Pimpl::getServerStatus(std::string serverName)
try {
    // This is strange but this is how the TAO ops handle it, I'd much
    // rather have an explicit state coming from TAO but this seems to be
    // all they give.
    ImplementationRepository::ServerInformation_var info;
    imrDomain_->find( serverName.c_str(), info );

    if ( string( info->partial_ior ).size() > 0 ) 
        return ImrClient::RUNNING;
    else
        return ImrClient::STOPPED;
} catch (...) {
    return ImrClient::UNKNOWN;
}

// End of TAO implementation
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// One-shots through to pimpl

ImrClient::ImrClient( corba::Client & client, const std::string & imrHost ) :
    pimpl_( new Pimpl( client, imrHost ) )
{
    // Nothing
}

ImrClient::~ImrClient( )
{
    // Nothing
}

bool 
ImrClient::addOad( const std::string & oadHost )
{
    return pimpl_->addOad( oadHost );
}

bool 
ImrClient::removeOad( const std::string & oadHost )
{
    return pimpl_->removeOad( oadHost );
}

bool 
ImrClient::addServer( const std::string & serverName,
                      const std::string & oadHost,
                      const std::string & execPath,
                      const std::string & args,
                      const std::string & optArgs,
                      const std::string & runDir )
{
    return pimpl_->addServer( serverName, oadHost, execPath, 
                              args, optArgs, runDir );
}

bool 
ImrClient::removeServer( const std::string & serverName )
{
    return pimpl_->removeServer( serverName );
}

bool
ImrClient::startServer( const std::string & serverName )
{
    return pimpl_->startServer( serverName );
}

bool 
ImrClient::stopServer( const std::string & serverName )
{
    return pimpl_->stopServer( serverName );
}

bool 
ImrClient::resetServer( const std::string & serverName )
{
    return pimpl_->resetServer( serverName );
}

ImrClient::ServerStatus 
ImrClient::getServerStatus( const std::string & serverName )
{
    return pimpl_->getServerStatus( serverName );
}
