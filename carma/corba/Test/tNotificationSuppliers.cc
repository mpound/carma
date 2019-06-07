#include "carma/corba/Client.h"
#include "carma/corba/Test/Notification.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

#include <boost/thread.hpp>
#include <string>

using namespace boost;
using namespace carma::corba;
using namespace carma::corba::test;
using namespace carma::util;
using namespace std;

namespace {

class Sender {
public:

    Sender( Client & client, 
            const string & channel, 
            const string & proxy,
            unsigned int events );

    void operator( )( );

private:

    Client & client_;
    const string channel_;
    const string proxy_;
    unsigned int nEvents_;

}; 

Sender::Sender( Client & client, 
                const string & channel, 
                const string & proxy,
                unsigned int events ) :
    client_( client ),
    channel_( channel ),
    proxy_( proxy ),
    nEvents_( events )
{

}

void 
Sender::operator( ) ( )
{
    Notification n;

    for ( unsigned i = 1; i < nEvents_; ++i ) {
        n.sendMJD = Time::MJD();
        n.count_ = i;
        n.stop = false;
        client_.sendNotification( channel_, proxy_, n );
    }

}

} // namespace < unnamed >

/**
 * @version $Revision: 1.1 $
 *
 * @description Stress test notification supplier functionality of the
 *    corba::Client class. Requires that tNotificationConsumers be started
 *    with the same number of channels (defaults match).
 *
 * @usage tNotificationSuppliers imr=<imr host> [keywords]
 *
 * @key channels 100 integer Number of separate channels to create.
 * @key proxies 1 integer Number of proxies per channel - note that multiple
 *   proxies result in duplicate messages being sent to consumers.
 * @key events 1000 integer Number of events to send to each proxy.
 * @key fixProxyName true bool Use the same proxy name even if proxies > 1.
 *
 * @author Andy Beard (Based on original tests by Chul Gwon)
 *
 * @logger TEST_FACILITY carma.test.corba.tNotificationSuppliers
 */
int
carma::util::Program::main( ) 
{
    const int channels( getIntParameter( "channels" ) );
    const int proxies( getIntParameter( "proxies" ) );
    const int events( getIntParameter( "events" ) );
    const bool fixProxyName( getBoolParameter( "fixProxyName" ) );
    
    Client & client = getCorbaClient();

    boost::thread_group group;

    for ( int c = 1; c <= channels; ++c ) {

        ostringstream channelName;
        channelName << TEST_CHANNEL_NAME << c;

        for ( int p = 1; p <= proxies; ++p ) {

            ostringstream proxyName;
            proxyName << "proxy" << ( fixProxyName ? 1 : p ); 

            group.create_thread( Sender( client, 
                                         channelName.str(),
                                         proxyName.str(),
                                         events ) );
        }
    }

    group.join_all();
    
    // Send one stop message per channel.  Note that
    // if we sent one per (client) proxy, then each consumer would
    // receive a stop message per *client* proxy.
    for ( int c = 1; c <= channels; ++c ) {
        ostringstream channelName;
        channelName << TEST_CHANNEL_NAME << c;
        Notification stopNotification;
        stopNotification.sendMJD = Time::MJD();
        stopNotification.count_ = events;
        stopNotification.stop = true;
        client.sendNotification( channelName.str(), 
                                 string( "<unused>" ), 
                                 stopNotification );
    }

    return 0;
}
