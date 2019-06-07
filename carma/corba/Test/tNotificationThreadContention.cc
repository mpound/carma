#include "carma/corba/Client.h"
#include "carma/corba/Server.h"
#include "carma/corba/Test/Notification.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedPthreadMutexLock.h"

#include <boost/thread/barrier.hpp>
#include <boost/thread.hpp>
#include <iostream>

using namespace boost;
using namespace boost::posix_time;
using namespace carma::corba;
using namespace carma::util;
using namespace std;

namespace { 

const string gNotifyChannel( "carma.test.tNotificationThreadContention" );
const string gNotifyProxy( "tNotificationThreadContention" );

struct NotificationServant {

    Server & server_;
    boost::barrier & barrier_;
    static int instances;
    static PthreadMutex instancesMutex;

    NotificationServant( Server & server, boost::barrier & barrier ) : 
        server_( server ),
        barrier_( barrier ) { };

    void operator() ( const test::Notification & n )
    {
        cout << "." << flush;
    };

    void operator()() 
    try {
        ostringstream oss;
        {
            ScopedPthreadMutexLock instancesGuard( instancesMutex );
            oss << ++instances;
        }

        barrier_.wait(); 

        cout << ".";
        server_.addNotificationServantFunctor< NotificationServant, 
                                               test::Notification > 
            ( *this, gNotifyChannel, oss.str() ); 

        server_.work();
        barrier_.wait();
        server_.run( false );

        cout << "." << flush;

    } catch (...) {
        cout << getStringForCaught() << endl;
    };

}; // NotificationServant
    
int NotificationServant::instances = 0;
carma::util::PthreadMutex NotificationServant::instancesMutex;

}  // namespace < unnamed >

// @version $Revision: 1.3 $ $Date: 2013/02/26 19:26:24 $ 
//
// @usage tNotificationThreadContention imr=<imr hostname>
//
// @logger TEST_FACILITY carma.test.corba.tNotificationThreadContention
// 
// @key channel @noDefault string Specify a specific channel name to use.
// @key proxy @noDefault string Specify a specific proxy name to use. 
//
// @description Stress test corba::Server NotificationServant processing by
//           creating lots of servants in separate threads.  The focus is on 
//           creation of servants and this test puts most contention there. 
//           This test uncovered a startup race in corba::Server which it would
//           trigger roughly 1/3 of the time.
//
int Program::main( )
try {
    Server & server( Program::getCorbaServer() );
    Client & client( Program::getCorbaClient() );

    const int nThreads( 200 );
    boost::barrier barrier( nThreads + 1 ); // Main waits on it too
    boost::thread_group threads;

    cout << "Creating " << nThreads << " threads to contentiously add "
        << "notification servants..." << flush;

    for ( int i = 0; i < nThreads; ++i ) 
        threads.create_thread( NotificationServant( server, barrier ) ); 

    barrier.wait(); // Wait for threads to start.

    // This is the point where there is a ton of contention when creating
    // and running servants.
     
    barrier.wait(); // Wait for threads to start servers.

    // Might as well send them a notification...
    cout << endl << "Sending 'em all the same notification..." << flush;
    test::Notification n;
    client.sendNotification< test::Notification >( 
        parameterWasSpecified( "channel" ) ? 
            getStringParameter( "channel" ) : gNotifyChannel, 
        parameterWasSpecified( "proxy" ) ?
            getStringParameter( "proxy" ) : gNotifyProxy, 
        n ); 
    
    boost::this_thread::sleep( seconds( 1 ) );

    cout << "Stopping the servers..." << endl;
    server.stop();
    
    cout << "Waiting for all those threads to exit..." << flush;
    threads.join_all();

    cout << endl << "SUCCESS!" << endl;
    return 0;
} catch (...) {
    cout << "FAILURE!" << endl;
    return 1;
}
