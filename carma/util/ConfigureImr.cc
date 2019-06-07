/**@file
 * Implementation of ConfigureImr class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.36 $
 * $Date: 2013/04/09 22:48:02 $
 * $Id: ConfigureImr.cc,v 1.36 2013/04/09 22:48:02 abeard Exp $
 */

#include "carma/corba/corba.h"

#include "carma/util/ConfigureImr.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Logger.h"
#include "carma/util/PeriodicTimer.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <unistd.h> // needed for gethostname
#include <algorithm>
#include <iomanip>
#include <iostream>

using namespace std;
using namespace carma::util;
using namespace log4cpp;

namespace { // Anonymous namespace for local constants and typedefs

    pair<int, int> parsePrioritySpec( const string & prioritySpec )
    {
        int firstpriority = 0;
        int secondpriority = 0; 

        // Check for a '-' and assume range if found
        const string::size_type dashpos = prioritySpec.find_first_of( '-' );
        if ( dashpos == string::npos ) {
            // Assume a single priority level
            firstpriority = ::atoi( prioritySpec.c_str() ); 
            secondpriority=firstpriority;
        } else {
            // Assume a range 
            firstpriority = ::atoi( prioritySpec.substr(0, dashpos).c_str() );
            secondpriority = ::atoi( 
                prioritySpec.substr( dashpos + 1, 
                                     prioritySpec.size() - dashpos).c_str() );
        }
        return std::make_pair( firstpriority, secondpriority );
    }

    string serverStatusToString( ImrClient::ServerStatus status ) {
        switch ( status ) {
            case ImrClient::FORKED: return "FORKED";
            case ImrClient::STARTING: return "STARTING";
            case ImrClient::RUNNING: return "RUNNING";
            case ImrClient::STOPPING: return "STOPPING";
            case ImrClient::STOPPED: return "STOPPED";
            default: return "<invalid>";
        }
    }


    ::timespec IMR_POLL_PERIOD = { 0, 150 * 1000L * 1000L };

    const Trace::TraceLevel TRACE_SERVER_REG = Trace::TRACE5;
    const Trace::TraceLevel TRACE_SERVER_START = Trace::TRACE4;
    const Trace::TraceLevel TRACE_SERVER_STOP = Trace::TRACE6;

} // End namespace <unnamed>
    
// -----------------------------------------------------------------------------
ConfigureImr::ConfigureImr(corba::Client & client,
                           std::string imrHost,
                           domainType domain) 
    : imr_(client, imrHost),
      imrHost_(imrHost),
      domain_(domain)
{
    CARMA_CPTRACE( Trace::TRACE7, "ConfigureImr::ConfigureImr() = ctor." );
}

// -----------------------------------------------------------------------------
ConfigureImr::~ConfigureImr()
{
    // Intentionally empty
}

// -----------------------------------------------------------------------------
vector<OADConfig>::iterator ConfigureImr::findOAD(const string &oad) 
{
    vector<OADConfig>::iterator i;
    for (i = domain_.oads.begin(); i < domain_.oads.end(); i++) {
        string lowercaseHostname;
        lowercaseHostname.resize(i->hostname.size());
        for (string::size_type ii=0; ii<i->hostname.size(); ii++) {
             lowercaseHostname[ii] = tolower(i->hostname[ii]);
        }
        string hostname = lowercaseHostname; 
        if (lowercaseHostname == "localhost") {
            const int len = 255; // Posix std length
            char localhostname[len+1];
            gethostname(localhostname, len);
            hostname = localhostname;
        }
        //cout << i->hostname << "/" << hostname << "/" << oad << endl;
        if (hostname == oad) return i;
    }
    return domain_.oads.end();
}

// -----------------------------------------------------------------------------
void ConfigureImr::addServers( const std::string &oad, 
                               const std::string prioritySpec,
                               const unsigned long sleepyTimeMs )
{
    CARMA_CPTRACE( TRACE_SERVER_REG, "ConfigureImr::addServers() - Adding "
        "servers for OAD " + oad + "." );
    
    vector<OADConfig>::iterator i = findOAD(oad);
    vector<ServerConfig>::iterator j;
    if ( i < domain_.oads.end() ) {
        pair<int, int> range = parsePrioritySpec( prioritySpec );
        for ( int pri = range.first; pri <= range.second; ++pri ) {
            for (j = i->servers.begin(); j < i->servers.end(); j++) {
                if ( j->startup_priority == pri ) {
                    imr_.addServer( j->name,
                                    j->hostname,
                                    j->path, 
                                    j->args,
                                    j->optargs,
                                    j->directory );
                    ::usleep( sleepyTimeMs * 1000L ); // Ignore return value.
                }
            }
        }
    }
}

// -----------------------------------------------------------------------------
void ConfigureImr::startServers( const std::string &oad, 
                                 const std::string prioritySpec,
                                 const unsigned int waitForRunningMs,
                                 const unsigned long sleepyTimeMs,
                                 const bool waitForEachPriorityStage,
                                 const unsigned long waitAfterEachStageMs,
                                 const bool verbose )
{
    vector<OADConfig>::iterator i = findOAD( oad );
    if ( i == domain_.oads.end() ) { 
        throw CARMA_ERROR( "OAD " + oad + " not found." );
    }

    CARMA_CPTRACE( TRACE_SERVER_REG, "ConfigureImr::startServers() - Starting "
        "servers for OAD " + oad + "." );

    pair<int, int> range = parsePrioritySpec( prioritySpec );

    // Always start highest priority first
    if ( range.first > range.second ) swap( range.first, range.second ); 

    const double t0 = Time::MJD();
    for ( int pri = range.first; pri <= range.second; ++pri ) {

        bool serversInThisPriority = false;

        vector<ServerConfig>::iterator j;
        for (j = i->servers.begin(); j < i->servers.end(); j++) {
            if ( j->startup_priority == pri ) {
                CARMA_CPTRACE( TRACE_SERVER_START, "ConfigureImr::startServers("
                    ") - Starting server " << j->name << "." );
                
                serversInThisPriority = true;
                const double startMJD = Time::MJD();
                // Start it up...
                imr_.startServer(j->name);
                const double startS = 
                        ( Time::MJD() - startMJD ) * Time::SECONDS_PER_DAY;
                ostringstream o, o2;
                o2 << left << setw(17) << j->name << " on " << setw(21) << oad;
                o << "Start server " << o2.str() << " in " 
                         << right << fixed << setw(6) << setprecision(3) 
                         << startS << " secs" ;
                Program::getLogger() << Priority::INFO << o.str();
                if ( verbose ) {
                    cout << Time::getTimeString() << " " << o.str() << endl;
                }
                ::usleep( sleepyTimeMs * 1000L ); // Ignore return value.
            }

        } 
            
        if ( serversInThisPriority && waitAfterEachStageMs > 0 ) 
                ::usleep( waitAfterEachStageMs * 1000L );

        if ( waitForEachPriorityStage ) 
            waitForServers( oad, ImrClient::RUNNING, 
                            pair<int,int>(pri, pri), 
                            waitForRunningMs );
    }
    
    // Now wait if instructed to do so.
    if ( !waitForEachPriorityStage ) { 
        waitForServers( oad, ImrClient::RUNNING, range, waitForRunningMs );
    }
    ostringstream o ;
    const double startS = (Time::MJD() - t0)* Time::SECONDS_PER_DAY;    
    o << "Starting all servers on " << left << setw(21) << oad
      << " took " << right << fixed << setw(6) << setprecision(3) 
      << startS << " secs";
    Program::getLogger() << Priority::INFO << o.str();
    if ( verbose ) {
        cout << Time::getTimeString() << " " << o.str() << endl;
    }
}

// -----------------------------------------------------------------------------
void ConfigureImr::stopServers( const std::string &oad,
                                const std::string prioritySpec,
                                const unsigned int waitForStoppedMs, 
                                const bool waitForEachPriorityStage )
{
    vector<OADConfig>::iterator i = findOAD( oad );
    if ( i == domain_.oads.end() ) { 
        throw CARMA_ERROR( "OAD " + oad + " not found." );
    }

    pair<int, int> range = parsePrioritySpec( prioritySpec );

    // Always stop lowest priority first
    if ( range.first < range.second ) swap( range.first, range.second ); 

    // Loop over priorities from lowest to highest 
    for ( int pri = range.first; pri >= range.second; --pri ) {
        vector<ServerConfig>::iterator j;
        for (j = i->servers.begin(); j < i->servers.end(); j++) {
            if ( j->startup_priority == pri )
                imr_.stopServer(j->name);
        } 
        if ( waitForEachPriorityStage ) 
            waitForServers( oad, ImrClient::STOPPED, 
                            pair<int,int>(pri, pri), 
                            waitForStoppedMs );
    }

    // Now wait if instructed to do so.
    if ( !waitForEachPriorityStage ) 
        waitForServers( oad, ImrClient::STOPPED, range, waitForStoppedMs );
}

// -----------------------------------------------------------------------------
void ConfigureImr::resetServers(const std::string &oad)
{
    vector<OADConfig>::iterator i = findOAD(oad);
    vector<ServerConfig>::iterator j;
    if ( i < domain_.oads.end() ) { 
        for (j = i->servers.begin(); j < i->servers.end(); j++)
            imr_.resetServer(j->name);
    }
}

// -----------------------------------------------------------------------------
void ConfigureImr::addOads()
{
    vector<OADConfig>::iterator i;
    for (i = domain_.oads.begin(); i < domain_.oads.end(); i++) {
        imr_.addOad(i->hostname);
    }
}

// -----------------------------------------------------------------------------
void ConfigureImr::clean(const std::string &oad)
{
    vector<OADConfig>::iterator oadIt = findOAD(oad);

    const ServerVector::const_iterator sBegin = oadIt->servers.begin( );
    const ServerVector::const_iterator sEnd = oadIt->servers.end( );
    for ( ServerVector::const_iterator s = sBegin; s != sEnd; ++s ) {

        // Whatever we do, don't wipe out the NotifyService and the
        // naming service - this sucks as it is hard-coding the names
        // of servers which we don't want to get rid of. <shrug>
        if ( s->name == "NamingService" || s->name == "NotifyService" ) 
            continue;

        // Stop and remove the server...
        imr_.resetServer( s->name );
        imr_.stopServer( s->name );
        imr_.removeServer( s->name );
    } // loop over servers
    
    // Remove the OAD record as well - only removes OAD records which aren't 
    // running.
    imr_.removeOad( oadIt->hostname );
}

// -----------------------------------------------------------------------------
void ConfigureImr::waitForServers( const std::string & oad, 
                                   const ImrClient::ServerStatus state, 
                                   const std::pair<int, int> & inPriorityRange,
                                   const long waitMaxMs )
{
    const std::pair<int, int> range = inPriorityRange;

    if ( waitMaxMs <= 0 ) return;

    vector<OADConfig>::iterator oadIt = findOAD(oad);
    if ( oadIt == domain_.oads.end() ) return; 

    const int lowpri = std::max( range.first, range.second );
    const int highpri = std::min( range.first, range.second );

    typedef vector<ServerConfig> ServerVec;
    typedef ServerVec::iterator ServerVecIt;

    // Form up vector of servers we're waiting for.
    ServerVec waiting;
    const vector<ServerConfig>::const_iterator sBegin = oadIt->servers.begin();
    const vector<ServerConfig>::const_iterator sEnd = oadIt->servers.end();
    vector<ServerConfig>::const_iterator s;
    for ( s = sBegin; s != sEnd; ++s ) {
        const int priority = s->startup_priority;
        if ( priority <= lowpri && priority >= highpri ) 
             waiting.push_back( *s );
    }
        
    // Periodically poll IMR for server status and remove from waiting vec
    // if it matches desired state.
    PeriodicTimer timer( IMR_POLL_PERIOD );

    long msWaited = 0;
    ::timespec startTimespec;
    const long result = ::clock_gettime( CLOCK_REALTIME, &startTimespec );

    if ( result != 0 ) 
        throw CARMA_ERROR( "clock_gettime failed." );

    const double startMjd = Time::timespec2MJD( startTimespec );

    timer.ResetNextFireAbsoluteTimeAndWait( startTimespec );

    while ( !waiting.empty() && msWaited < waitMaxMs ) {
        
        timer.WaitForNextFireTime( );

        const vector<ServerConfig>::iterator wsBegin = waiting.begin();
        vector<ServerConfig>::iterator wsEnd = waiting.end();
        vector<ServerConfig>::iterator ws = wsBegin;
        while ( ws != wsEnd ) {
            ImrClient::ServerStatus srvrStat = imr_.getServerStatus( ws->name );
            if ( srvrStat == state ) { 
                ws = waiting.erase( ws ); // Invalidates iterator and end.
                wsEnd = waiting.end();
            } else {
                ++ws;
            }
        }
        
        msWaited = static_cast< long >( 
            ( Time::MJD() - startMjd ) * Time::MILLISECONDS_PER_DAY );

    } 

    if ( !waiting.empty() ) {
        ostringstream warn;
        warn << "ConfigureImr::waitForServers() timed out after "
            << msWaited << "ms while waiting for the following servers "
            << "at priority(s) " << highpri << "-" << lowpri << " "
            << "to go to " << serverStatusToString( state ) << " state: ";
        
        const vector<ServerConfig>::const_iterator wsBegin = waiting.begin();
        const vector<ServerConfig>::const_iterator wsEnd = waiting.end();
        vector<ServerConfig>::const_iterator ws;
        for ( ws = wsBegin; ws != wsEnd; ++ws ) 
            warn << " " << ws->name;
        warn << ".";
        programLogWarnIfPossible( warn.str() );
    } else {
        ostringstream info;
        info << "ConfigureImr::waitForServers() - Servers transitioned to "
            << serverStatusToString( state ) << " state after " 
            << msWaited << "ms at priority(s) " << highpri << "-" << lowpri 
            << ".";
        programLogInfoIfPossible( info.str() );
    }

}
