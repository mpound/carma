#include "carma/corba/Server.h"
#include "carma/corba/Test/Notification.h"
#include "carma/util/Program.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/SimpleStatisticsAccumulators.h"
#include "carma/util/Time.h"

#include <boost/foreach.hpp>
#include <boost/thread.hpp>
#include <vector>
#include <iostream>

using namespace boost;
using namespace boost::accumulators;
using namespace boost::posix_time;
using namespace carma;
using namespace carma::corba;
using namespace carma::util;
using namespace std;

namespace {

class Consumer {
private:

    const int channelNumber_;
    const int proxyNumber_;

    unsigned last_;
    unsigned dups_;
    unsigned drops_;

    util::DoubleStatAccumulator stats_;

    PthreadMutex mutex_;
    bool done_;

public:

    Consumer( const int channelNumber,
              const int proxyNumber ) : 
        channelNumber_( channelNumber ), proxyNumber_( proxyNumber ),
        last_( 0 ), dups_( 0 ), drops_( 0 ), done_( false ) { };

    ~Consumer( ) { };

    bool done( ) { 
        ScopedPthreadMutexLock lock( mutex_ );
        return done_; 
    };

    void operator( )( const corba::test::Notification & n );

    void printStats( );

}; // class Consumer

void 
Consumer::operator( )( const corba::test::Notification & n )
try {

    const unsigned expectedCount = last_ + 1;
    if ( n.count_ <= last_ ) 
        ++dups_;
    else if ( n.count_ > expectedCount ) 
        drops_ += n.count_ - expectedCount;
    else 
        last_ = n.count_;
        
    if ( n.count_ % 100 == 0 ) 
        cout << "." << flush;

    const double rxMJD( util::Time::MJD() );
    stats_( ( rxMJD - n.sendMJD ) * Time::MILLISECONDS_PER_DAY );

    if ( n.stop ) {
        ScopedPthreadMutexLock lock( mutex_ );
        done_ = true;
    } 

} catch (...) {
    cout << "Exception" << endl;
}

void
Consumer::printStats( )
{
    if ( drops_ || dups_ ) { 
        cout << "Chan " << channelNumber_ << " proxy " << proxyNumber_ << 
            ": " << drops_ << " dropped, " << dups_ << " duplicates." << endl;
    }
        
    cout << "Chan " << channelNumber_ << " proxy " << proxyNumber_ 
        << " end-to-end avg=" << setprecision( 3 )
        << mean( stats_ ) << "+/-" << ::sqrt( variance( stats_ ) ) << "ms, "
        << "max=" << accumulators::max( stats_ ) << "ms, min="
        << accumulators::min( stats_ ) << "ms." << endl;
}

} // namespace < unnamed >

/**
 * @version $Revision: 1.1 $
 *
 * @description Stress tests notification consumer functionality of 
 *    corba::Server class in many different configurable ways.  
 *    Requires that tNotificationSuppliers be started with the same
 *    number of channels (defaults match).
 *
 * @usage tNotificationConsumers imr=<imr host> [keywords]
 *
 * @key channels 100 integer Number of separate channels to create.
 * @key proxies 1 integer Number of proxies per channel.
 * @key mode "ServerPerChannel" string Mode string indicating one of:
 *   "ServerPerChannel" Use a single corba::Server instance per channel.
 *   "ServerPerProxy" Use a single corba::Server instance per proxy.
 *   "ServerPerProcess" Use a single corba::Server instance for the process.
 *
 * @author Andy Beard 
 *
 * @logger TEST_FACILITY carma.test.corba.tNotificationConsumers
 */
int
carma::util::Program::main( ) 
{
    const int channels( getIntParameter( "channels" ) );
    const int proxies( getIntParameter( "proxies" ) );
    const string mode( getStringParameter( "mode" ) );

    bool serverPerChannel( false );
    bool serverPerProxy( false );
    bool serverPerProcess( false );
    
    if ( mode == "ServerPerChannel" )
        serverPerChannel = true;
    else if ( mode == "ServerPerProxy" )
        serverPerProxy = true;
    else if ( mode == "ServerPerProcess" )
        serverPerProcess = true;
    else {
        cerr << "Invalid mode" << endl;
        return 1;
    }
        
    vector< Consumer * > consumers;
    vector< Server * > servers;

    cout << "Creating consumers, proxies and servers..." << endl;

    if ( serverPerProcess ) 
        servers.push_back( new Server( getExtraArgc(), getExtraArgv() ) );

    for ( int c = 1; c <= channels; ++c ) { 

        ostringstream channelName;
        channelName << test::TEST_CHANNEL_NAME << c;

        if ( serverPerChannel ) 
            servers.push_back( new Server( getExtraArgc(), getExtraArgv() ) );
            
        for ( int p = 1; p <= proxies; ++p ) {

            ostringstream proxyName;
            proxyName << "proxy" << p;

            if ( serverPerProxy ) 
                servers.push_back( new Server( getExtraArgc(), getExtraArgv()));

            consumers.push_back( new Consumer( c, p ) );

            servers.back( )->addNotificationServantFunctor< 
                Consumer, test::Notification >( 
                    *consumers.back( ),
                    channelName.str(),
                    proxyName.str() );

        } // End loop over proxies

    } // End loop over channels

    cout << "Running servers in a separate thread per server..." << endl;
    BOOST_FOREACH( Server * server, servers )
        server->run( true );
    
    // Poll every second for all consumers to be 'done' (receive stop command).
    bool done = false; 
    while ( !done ) {
        boost::this_thread::sleep( seconds( 1 ) );
        bool alldone = true;
        BOOST_FOREACH( Consumer * consumer, consumers ) {
            if ( !consumer->done() ) {
                alldone = false;
                break;
            }
        }
        done = alldone;
    }

    cout << endl << "All consumers have finished, stopping servers..." << endl;
    BOOST_FOREACH( Server * server, servers )
        server->stop( );

    cout << "Tallying statistics..." << endl;
    BOOST_FOREACH( Consumer * consumer, consumers ) 
        consumer->printStats();

    cout << "Cleaning up..." << endl;
    BOOST_FOREACH( Consumer * consumer, consumers ) delete consumer;
    BOOST_FOREACH( Server * server, servers ) delete server;
                
    return 0;

}
