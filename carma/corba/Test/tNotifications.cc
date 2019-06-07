#include "carma/corba/Client.h"
#include "carma/corba/Server.h"
#include "carma/corba/Test/Notification.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"

#include <iostream>

using namespace carma::corba;
using namespace carma::corba::test;
using namespace carma::util;
using namespace std;

namespace {

// Notification data itself is defined in Test/Notification.idl

class NotificationServant {
private:

    unsigned long last_;

public:

    NotificationServant( ) : last_( 0 ) { };

    void operator()( const Notification & n ) 
    {
        if ( last_ + 1 != n.count_ ) {
            cout << "!" << flush;
        }

        last_ = n.count_;

        if ( n.count_ % 1000 == 0 ) 
            cout << "." << flush;
    };

};

} // namespace < unnamed >


// @version $Revision: 1.4 $ $Date: 2013/03/21 17:32:00 $ 
//
// @usage tNotifications imr=<imr hostname>
//
// @logger TEST_FACILITY carma.test.corba.tNotifications
// 
// @noKeys
//
// @description Simple test of corba::Server & Client notification functionality
//
int Program::main( )
try {
    Server & server( Program::getCorbaServer() );
    Client & client( Program::getCorbaClient() );

    NotificationServant ns;

    const string channelName( "tNotifications" );
    const string proxyName( "dummy?" ); 
    server.addNotificationServantFunctor< NotificationServant, Notification >( 
        ns, channelName, proxyName );

    server.run( true ); // Process notifications in a separate thread.

    const int nNotifications( 80000 );
    cout << "Sending " << nNotifications << " from main thread." << endl;
    Notification notification;
    notification.count_ = 0;
    for ( int i = 0; i < nNotifications; ++i ) {
        notification.count_++;
        client.sendNotification( channelName, proxyName, notification );
    }

    cout << "Success!" << endl;
    return 0;

} catch (...) {
    cerr << carma::util::getStringForCaught() << endl;
    return 1;
}
