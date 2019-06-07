/** 
 * @file
 * 
 * Process to collect status of carma processes from the IMR.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.23 $
 * $Date: 2012/03/13 05:18:24 $
 * $Id: processMonitor.cc,v 1.23 2012/03/13 05:18:24 abeard Exp $
 */

#include "carma/monitor/ImrSubsystem.h"
#include "carma/corba/Server.h"
#include "carma/util/FileUtils.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/ImrClient.h"
#include "carma/util/ImrConfigHandlers.h"
#include "carma/util/ProcessMonitor_skel.h"
#include "carma/util/ProcessMonitor_skel_tie.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/StartPthread.h"

#include <boost/foreach.hpp>
#include <iostream>
#include <string>
#include <vector>

using namespace carma;
using namespace carma::monitor;
using namespace carma::util;
using namespace std;

namespace { // Anonymous namespace for local riffraff.

    // There's lots of boilerplate here so most implementations follow main.
    
    class ServerInfo;

    typedef ::std::map< string, ServerInfo > ServerInfoMap;
    typedef ImrSubsystem::StateMonitorPointEnum ImrServerStateEnum;
    typedef ImrSubsystem::ServerTypeMonitorPointEnum ServerTypeEnum;
    typedef carma::util::ScopedLock< PthreadMutex > PthreadScopedLock;

    const long UPDATE_TIMER_OFFSET = 250000000; // 250 ms in ns
    const double SERVER_ASSUMED_DEAD_SECS = 25; 

    ImrSubsystem::StateMonitorPointEnum::STATE
    serverStatusToMonitorStateEnum( const enum ImrClient::ServerStatus stat,
                                    const bool critical ); 

    ServerTypeEnum::SERVERTYPE
    systemTypeToServerType( const SystemType sysType );

    /**
     * Class to facilitate updating server information.
     * Implementation follows main.
     */
    class ServerInfo {
    public:

        ServerInfo( ImrSubsystem::Server & monitorSys,
                    ImrClient & imrClient,
                    const ServerConfig & serverConfig );

        ServerInfo( const ServerInfo & other ); 

        ~ServerInfo( );

        ServerInfo & operator=( const ServerInfo & other ); 

        ImrServerStateEnum::STATE update( );

        void writeToMonitorSystem( ); 

        bool isCritical( ); 
        
        void alive( );

    private:

        PthreadMutex                                stateMutex_;
        ImrServerStateEnum::STATE                   state_;

        PthreadMutex                                lastHeardMutex_;
        double                                      lastHeardMjd_;

        ImrSubsystem::Server &                      monitorSys_;
        ImrClient &                                 imrClient_;
        const ServerConfig &                        serverConfig_;

    }; // End class ServerInfo

    /**
     * Bookkeeping class for summary stats.
     * Implementation follows main.
     */
    class ServerSummaryInfo { 
    public:
    
        explicit ServerSummaryInfo( int numServers,
                                    int numCriticalServers,
                                    string absConfFilename,
                                    ImrSubsystem & monitorSys );

        ~ServerSummaryInfo( );

        void setServerCounts( int runningServers, 
                              int notRunningServers,
                              int notRunningCriticalServers );

        void writeInfoToMonitorSystem( );

    private:

        PthreadMutex mutex_;
        int runningServers_; 
        int notRunningServers_;
        int notRunningCriticalServers_;
        const int numServers_;
        const int numCriticalServers_;
        const string absConfFilename_; 
        ImrSubsystem & monitorSys_;

    }; // End class ServerSummaryInfo

    class ProcessMonitorImpl {
    public:

        explicit ProcessMonitorImpl( ServerInfoMap & serverInfo );

        ~ProcessMonitorImpl( );

        void alive( const char * serverId );

    private:

        ServerInfoMap & serverInfo_;

    }; // ProcessMonitorImpl


    // Helper functions

    ServerInfoMap createServerInfo( const domainType & domain,
                                    ImrClient & imrClient,
                                    ImrSubsystem & monSubsys ); 

    void logErrorAndReport( const ::std::string & msg ); 

    ::std::string getAbsoluteConfFilename( const ::std::string & input ); 

    void updateServerInfo( ServerInfoMap & serverInfo,
                           ServerSummaryInfo & summaryInfo );

    struct WriteMonitorDataThreadArgs {
        ServerInfoMap & serverInfo;
        ServerSummaryInfo & summaryInfo;
    };
        
    void writeMonitorDataEntryPoint( WriteMonitorDataThreadArgs & args ); 

} // End namespace < unnamed >

/**
 * @description Collects status of all processes running on the input imr.  
 *
 * @usage processMonitor imr=host [autowrite=true] [writedelay=0.200]
 *
 * @key autowrite  true bool   Enable autowriter - requires an FSP.
 * @key writedelay 0.20 double Autowriter delay in seconds. 
 * @key poll         10 int    Polling period in frames.
 * @key file @mandatory string XML configuration file.
 * @logger DEFAULT_FACILITY carma.util.processMonitor
 */
int Program::main() {
    
    const bool autowrite = getBoolParameter( "autowrite" );
    const double awdelay = getDoubleParameter( "writedelay" );
    const int pollInterval = getIntParameter( "poll" );
    const ::std::string config = getStringParameter( "file" );

    // Make sure poll interval is > 0
    if ( pollInterval <= 0 ) {
        logErrorAndReport( "Poll interval must be > 0 - exiting." );
        return 1;
    } 

    // Make sure IMR hostname was specified..
    if ( !haveImrHostname( ) ) {
        logErrorAndReport( "Imr must be specified on the command line." );
        return 1;
    }

    ImrClient imr( getCorbaClient(), 
                   Program::getProgram( ).getImrHostname( ) );  

    const string conffile = getAbsoluteConfFilename( config );

    if ( !FileUtils::exists( conffile ) ) {
        logErrorAndReport( conffile + " doesn't exist - exiting." );
        return 1;
    }

    domainType domain = parseXmlConfig( config, false, false );
    
    // Initialize the monitor system and autowriter.
    carma::monitor::ImrSubsystem imrMonSubsys;
    
    if ( autowrite )
        imrMonSubsys.startAutoWriter( awdelay );
    
    // Make sure we've got enough monitor system...
    if ( static_cast<int>( domain.nServers ) > imrMonSubsys.serverCount() ) { 
        logErrorAndReport( "Too many IMR servers - exiting." );
        return 1;
    }

    ServerSummaryInfo summaryInfo( domain.nServers, 
                                   domain.nCriticalServers,
                                   conffile,
                                   imrMonSubsys );
    
    ServerInfoMap serverInfo = createServerInfo( domain, 
                                                    imr, 
                                                    imrMonSubsys );
    
    WriteMonitorDataThreadArgs args = { serverInfo, summaryInfo };
    StartPthreadWithRef( writeMonitorDataEntryPoint, args );
    
    ProcessMonitorImpl procImpl( serverInfo );
    corba::Server & corbaServer = getCorbaServer( );
    corbaServer.addServant< POA_carma::util::ProcessMonitor_tie >( 
        procImpl,
        string( PROCESS_MONITOR_NAME ) );
    corbaServer.run( true );

    carma::util::FrameAlignedTimer timer( UPDATE_TIMER_OFFSET, pollInterval ); 
    timer.ResetNextFireTime( );
    while ( !corbaServer.terminated() ) {
        timer.WaitForNextFireTime( ); // fires at the beginning of every frame
        updateServerInfo( serverInfo, summaryInfo );
    } // End main work loop

    return 0; // Success

} // End Program::main

// -----------------------------------------------------------------------------
// Implementation only
// -----------------------------------------------------------------------------
namespace {

    // ServerInfo class implementation.

    ServerInfo::ServerInfo( ImrSubsystem::Server & monitorSys,
                            ImrClient & imrClient,
                            const ServerConfig & serverConfig ) :
        state_( ImrServerStateEnum::UNKNOWN ),
        lastHeardMjd_( 0. ),
        monitorSys_( monitorSys ),
        imrClient_( imrClient ),
        serverConfig_( serverConfig )
    { 
        // Nothing
    }

    ServerInfo::ServerInfo( const ServerInfo & other ) :
        state_( other.state_ ),
        lastHeardMjd_( other.lastHeardMjd_ ),
        monitorSys_( other.monitorSys_ ),
        imrClient_( other.imrClient_ ),
        serverConfig_( other.serverConfig_ ) 
    { 
        // Nothing
    } 

    ServerInfo::~ServerInfo( ) 
    { 
        // Nothing
    }

    ServerInfo & 
    ServerInfo::operator=( const ServerInfo & other ) 
    {
        return *this; // Return by value to invoke copy constructor.
    }

    ImrServerStateEnum::STATE 
    ServerInfo::update( ) 
    {
        double lastHeardMjd;
        {
            PthreadScopedLock scopelock( lastHeardMutex_ );
            lastHeardMjd = lastHeardMjd_;
        }
        
        const double nowMjd = Time::MJD();
        const double ageS = ( nowMjd - lastHeardMjd ) * Time::SECONDS_PER_DAY;

        bool serverAssumedDead = false;
        if ( ageS > SERVER_ASSUMED_DEAD_SECS ) 
            serverAssumedDead = true;

        PthreadScopedLock scopelock( stateMutex_ );
        try {
            if ( serverAssumedDead ) {
                state_ = ( serverConfig_.critical ? 
                            ImrServerStateEnum::STOPPEDc :
                            ImrServerStateEnum::STOPPED );
            } else {
                state_ = 
                    serverStatusToMonitorStateEnum( 
                        imrClient_.getServerStatus( serverConfig_.name ),
                        serverConfig_.critical );
            }
        } catch (...) {
            if ( serverConfig_.critical ) {
                state_ = ImrServerStateEnum::UNKNOWNc;
            } else {
                state_ = ImrServerStateEnum::UNKNOWN;
            }
        }
        return state_;
    }

    void 
    ServerInfo::writeToMonitorSystem( ) 
    {

        monitorSys_.name().setValue( serverConfig_.name );
        monitorSys_.serverType().setValue( 
                systemTypeToServerType( serverConfig_.system ) );
        monitorSys_.state().setValue( state_ );
        
    }

    bool 
    ServerInfo::isCritical( ) 
    {
        return serverConfig_.critical;
    }

    void
    ServerInfo::alive( )
    {
        PthreadScopedLock lock( lastHeardMutex_ );
        lastHeardMjd_ = Time::MJD();
    }
    
    // ServerSummaryInfo class implementation

    ServerSummaryInfo::ServerSummaryInfo( int numServers,
                                          int numCriticalServers,
                                          string absConfFilename,
                                          ImrSubsystem & monitorSys ) :
        numServers_( numServers ),
        numCriticalServers_( numCriticalServers ),
        absConfFilename_( absConfFilename ),
        monitorSys_( monitorSys )
    {
        // Nothing
    }
    
    ServerSummaryInfo::~ServerSummaryInfo( ) 
    {
        // Nothing
    }
        
    void 
    ServerSummaryInfo::setServerCounts( int runningServers, 
                                        int notRunningServers,
                                        int notRunningCriticalServers )
    {
        PthreadScopedLock scopelock( mutex_ );
        runningServers_ = runningServers;
        notRunningServers_ = notRunningServers;
        notRunningCriticalServers_ = notRunningCriticalServers;
    }

    void
    ServerSummaryInfo::writeInfoToMonitorSystem( )
    {
        PthreadScopedLock scopelock( mutex_ );
        monitorSys_.numServers( ).setValue( numServers_ );
        monitorSys_.numCriticalServers( ).setValue( numCriticalServers_ );
        monitorSys_.numRunningServers( ).setValue( runningServers_ );
        monitorSys_.numNotRunningServers( ).setValue( notRunningServers_ );
        monitorSys_.numNotRunningCriticalServers( ).setValue(
            notRunningCriticalServers_ );
        monitorSys_.configFilename( ).setValue( absConfFilename_ );
    }

    ProcessMonitorImpl::ProcessMonitorImpl( ServerInfoMap & serverInfo ) :
        serverInfo_( serverInfo )
    {
        // Nothing
    }

    ProcessMonitorImpl::~ProcessMonitorImpl( )
    {
        // Nothing
    }

    void 
    ProcessMonitorImpl::alive( const char * serverId )
    {
        const string serverIdString( serverId );

        const ServerInfoMap::iterator si = serverInfo_.find( serverId );
        if ( si != serverInfo_.end() ) 
            si->second.alive();
    }

    // Helper function implementations
    ImrSubsystem::StateMonitorPointEnum::STATE
    serverStatusToMonitorStateEnum( const enum ImrClient::ServerStatus stat,
                                    const bool critical ) 
    {
        switch ( stat ) {
            case ImrClient::FORKED: 
                if ( critical ) {
                    return ImrServerStateEnum::FORKEDc;
                } else {
                    return ImrServerStateEnum::FORKED;
                }
            case ImrClient::STARTING:
                if ( critical ) {
                    return ImrServerStateEnum::STARTINGc;
                } else {
                    return ImrServerStateEnum::STARTING;
                }
            case ImrClient::RUNNING:      
                return ImrServerStateEnum::RUNNING;
            case ImrClient::STOPPING:
                if ( critical ) {
                    return ImrServerStateEnum::STOPPINGc;
                } else {
                    return ImrServerStateEnum::STOPPING;
                }
            case ImrClient::STOPPED:      
                if ( critical ) {
                    return ImrServerStateEnum::STOPPEDc; 
                } else {
                    return ImrServerStateEnum::STOPPED; 
                }
            default:
                if ( critical ) {
                    return ImrServerStateEnum::UNKNOWNc;
                } else {
                    return ImrServerStateEnum::UNKNOWN;
                }
        }
    }

    ServerTypeEnum::SERVERTYPE
    systemTypeToServerType( const SystemType sysType ) {
        switch ( sysType ) {
            case DATA:           return ServerTypeEnum::DATA;
            case MONITOR:        return ServerTypeEnum::MONITOR;
            case CONTROL:        return ServerTypeEnum::CONTROL;
            case INTERFEROMETRY: return ServerTypeEnum::INTERFEROMETRY;
            case CORRELATOR:     return ServerTypeEnum::CORRELATOR;
            case OVRO:           return ServerTypeEnum::OVRO;
            case BIMA:           return ServerTypeEnum::BIMA;
            case SZA:            return ServerTypeEnum::SZA;
            case MISC:           return ServerTypeEnum::MISC;
        }
        return ServerTypeEnum::UNKNOWN;
    } 

    ServerInfoMap
    createServerInfo( const domainType & domain,
                      ImrClient & imrClient,
                      ImrSubsystem & monSubsys ) 
    {
        unsigned serverCount = 0;
        ServerInfoMap servers;

        vector<OADConfig>::const_iterator oad = domain.oads.begin();
        for ( ; oad != domain.oads.end( ); ++oad ) {

            vector<ServerConfig>::const_iterator server = oad->servers.begin();
            for( ; server != oad->servers.end( ); ++server ) {

                ServerInfo serverInfo( monSubsys.server( serverCount ),
                                       imrClient,
                                       *server );

                ServerInfoMap::value_type val = std::make_pair( server->name ,
                                                                serverInfo );
                servers.insert( val );
                ++serverCount;
            } // End loop over servers
        } // End loop over oads

        return servers;
    }
    
    ::std::string
    getAbsoluteConfFilename( const ::std::string & input ) 
    {
        string result;
        // If input file is an absolute path, use it, otherwise assume
        // a relative path beginning with either a ./ or nothing.
        if ( input.at( 0 ) == '/' ) {
            result = input;
        } else if ( input.at( 0 ) == '.' ) {
            result = Program::getProgram( ).getCwd( ) + input.substr( 1 ); 
        } else {
            // Assume a relative path
            result = Program::getProgram( ).getCwd( ) + '/' + input;
        }
        return result;
    }

    void 
    logErrorAndReport( const ::std::string & msg ) 
    {
        cerr << msg << endl;
        carma::util::programLogErrorIfPossible( msg );
    }

    void
    updateServerInfo( ServerInfoMap & serverInfo,
                      ServerSummaryInfo & summaryInfo ) 
    {
        int runningServers = 0;
        int notRunningServers = 0;
        int notRunningCriticalServers = 0;

        BOOST_FOREACH( ServerInfoMap::value_type & val, serverInfo ) {

            ServerInfo & server = val.second;

            ImrServerStateEnum::STATE newState = server.update();
            if ( newState == ImrServerStateEnum::RUNNING ) {
                ++runningServers;
            } else if ( server.isCritical( ) ) {
                ++notRunningCriticalServers;
                ++notRunningServers;
            } else {
                ++notRunningServers;
            }

        } // End loop over servers

        summaryInfo.setServerCounts( runningServers,
                                     notRunningServers,
                                     notRunningCriticalServers );

    } // End updateServerInfo

    void
    writeMonitorDataEntryPoint( WriteMonitorDataThreadArgs & args ) 
    {
        // Thread to write data every half second on the half second.
        ServerInfoMap & serverInfo = args.serverInfo;
        ServerSummaryInfo & summaryInfo = args.summaryInfo;
        carma::util::FrameAlignedTimer timer( 0 );  // Sync to frame

        timer.ResetNextFireTime( );

        while ( true ) {

            timer.WaitForNextFireTime( ); 
            
            BOOST_FOREACH( ServerInfoMap::value_type & val, serverInfo ) {
                ServerInfo & server = val.second;
                server.writeToMonitorSystem( );
            }

            summaryInfo.writeInfoToMonitorSystem( );

        }
    } // End writeMonitorDataEntryPoint

}
