#include "carma/corba/Client.h"
#include "carma/corba/Server.h"
#include "carma/corba/Test/Hello.h"
#include "carma/corba/Test/HelloImpl.h"
#include "carma/corba/Test/Hello_skel.h"
#include "carma/corba/Test/Hello_skel_tie.h"
#include "carma/corba/Test/Terminator.h"
#include "carma/corba/Test/TerminatorImpl.h"
#include "carma/corba/Test/Terminator_skel.h"
#include "carma/corba/Test/Terminator_skel_tie.h"
#include "carma/corba/Test/Wait.h"
#include "carma/corba/Test/Wait_skel.h"
#include "carma/corba/Test/Wait_skel_tie.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/PthreadCond.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedLock.h"

#include <boost/thread/barrier.hpp>
#include <boost/thread.hpp>
#include <iostream>

using namespace carma;
using namespace boost::posix_time;
using namespace carma::util;
using namespace carma::corba;
using namespace std;

/**
 * Class to lock-step client and server for testing purposes (Hijacks Wait).
 */
class TestSync {
private:

    boost::barrier barrier_;

public:

    TestSync( ) : barrier_( 2 ) { };

    ~TestSync( ) { };

    // Called by client - ignore args
    void pause( CORBA::ULong ms = 0 ) 
    {
        barrier_.wait();
    };

};

class HiNowDie {
private:

    Server & server_;
    const bool stop_;

public:

    HiNowDie( Server & server, bool stop ) :
        server_( server ), stop_( stop ) 
    { };

    ~HiNowDie( ) 
    { };

    void hi( ) 
    try { 

        cout << " SERVER (" << pthread_self() << ", " 
             << boost::this_thread::get_id() << "): Hi!" << flush;
       
        if ( stop_ ) { 
            cout << " Now die!" << flush; 
            server_.stop(); 
        }

        cout << endl;
    } catch (...) {
        cout << " SERVER: " << getStringForCaught() << endl;
    };

}; // class HiNowDie 

// @version $Revision: 1.1 $ $Date: 2012/11/28 18:33:56 $ 
//
// @usage tServerExhaustive
//
// @logger TEST_FACILITY carma.test.corba.tServerExhaustive
// 
// @noKeys 
// 
// @description Detailed test of corba::Server class remotely.
int Program::main( ) 
try {

    namespace POA_cct = POA_carma::corba::test; 
        
    TestSync testSync;

    Server & programServer = Program::getCorbaServer();
    programServer.addServant< POA_cct::Wait_tie >( testSync, 
                                                   "carma.corba.testSync" );
    programServer.run( true );

    cout << "Please start tServerExhaustiveClient to begin testing..." << endl;
    testSync.pause(); // 1

    {
        cout << "Testing corba::Server ctor and dtor only." << endl;
        carma::corba::Server server( getExtraArgc(), getExtraArgv() );
    }

    cout << "Success" << endl << endl;

    {
        cout << "Testing corba::Server ctor and dtor with run( true )." << endl;
        carma::corba::Server server( getExtraArgc(), getExtraArgv() );
        server.run( true );
    }
        
    cout << "Success" << endl << endl;

    {
        cout << "Testing corba::Server ctor and dtor with work()." << endl;
        carma::corba::Server server( getExtraArgc(), getExtraArgv() );
        server.work( );
    }
        
    cout << "Success" << endl << endl;
    
    {
        cout << "Testing Server::run() in separate thread & DO stop." << endl;

        carma::corba::Server server( getExtraArgc(), getExtraArgv() );
        
        HiNowDie servant( server, true );
        server.addServant< POA_cct::Hello_tie >( servant, "test1.hello" );
        server.run( true );
        testSync.pause(); // 2
        cout << " Waiting for remote invocation and termination." << endl;
        testSync.pause(); // 3
    } 

    cout << "Success" << endl << endl;
    
    {
        cout << "Testing run() in separate thread w/ explicit stop." << endl;

        carma::corba::Server server( getExtraArgc(), getExtraArgv() );
        
        HiNowDie servant( server, false );
        server.addServant< POA_cct::Hello_tie >( servant, "test2.hello" );
        server.run( true );
        testSync.pause( ); // 4
        cout << " Waiting for remote invocation." << endl;
        testSync.pause( ); // 5
        cout << " Explicitly terminating." << endl;
        server.stop( );
    } 

    cout << "Success" << endl << endl;
    
    {
        cout << "Testing Server::run() in same thread." << endl;
        
        carma::corba::Server server( getExtraArgc(), getExtraArgv() );

        HiNowDie servant( server, true );
        server.addServant< POA_cct::Hello_tie >( servant, "test3.hello" );
        testSync.pause( ); // 6 
        cout << " Waiting for remote invocation and termination." << endl;
        server.run( false );
        cout << " Verifying that terminated() returns true..." << flush; 
        if ( server.terminated() ) { 
            cout << " it does." << endl;
        } else {
            cout << " it doesn't." << endl;
            return 1;
        }
        testSync.pause( ); // 7 
    } 

    cout << "Success" << endl << endl;
    
    {
        cout << "Testing Server::work() with DO stop." << endl;
        
        carma::corba::Server server( getExtraArgc(), getExtraArgv() );

        HiNowDie servant( server, true );
        server.addServant< POA_cct::Hello_tie >( servant, "test4.hello" );
        testSync.pause( ); // 8 
        cout << " Waiting for remote invocation and termination." << endl;
        while ( !server.terminated() ) {
            boost::this_thread::sleep( milliseconds( 1 ) );
            server.work( );
        }
        testSync.pause( ); // 9 
    } 

    cout << "Success" << endl << endl;

    {
        cout << "Testing Server::addServant() after server is running." << endl;

        carma::corba::Server server( getExtraArgc(), getExtraArgv() );
        
        HiNowDie servant( server, true );
        server.run( true );
        server.addServant< POA_cct::Hello_tie >( servant, "test5.hello" );
        testSync.pause( ); // 10 
        cout << " Waiting for remote invocation and termination." << endl;
        testSync.pause( ); // 11 
    } 

    cout << "Success" << endl << endl;
        
    {
        cout << "Testing Server::run() with destructor triggered stop." << endl;

        carma::corba::Server server( getExtraArgc(), getExtraArgv() );
        
        HiNowDie servant( server, false );
        server.run( true );
        server.addServant< POA_cct::Hello_tie >( servant, "test6.hello" );
        testSync.pause( ); // 12
        cout << " Waiting for remote invocation." << endl;
        testSync.pause( ); // 13 
        cout << " Terminating via destructor." << endl;
    } 

    cout << "Success" << endl << endl;

    {
        cout << "Testing multiple simultaneous Server::run() calls." << endl;
        
        carma::corba::Server serverA( getExtraArgc(), getExtraArgv() );
        
        HiNowDie servantA( serverA, false );

        serverA.addServant< POA_cct::Hello_tie >( servantA, "test7.helloA" );

        serverA.run( true );
        serverA.run( true );

        testSync.pause( ); // 14
        cout << " Waiting for several remote invocations." << endl;
        testSync.pause( ); // 15
    }
    
    cout << "Success" << endl << endl;

    {
        cout << "Testing multiple simultaneous Server instances." << endl;
        
        carma::corba::Server serverA( getExtraArgc(), getExtraArgv() );
        carma::corba::Server serverB( getExtraArgc(), getExtraArgv() );
        
        HiNowDie servantA( serverA, false );
        HiNowDie servantB( serverB, false );

        serverA.addServant< POA_cct::Hello_tie >( servantA, "test8.helloA" );
        serverB.addServant< POA_cct::Hello_tie >( servantB, "test8.helloB" );

        serverA.run( true );
        serverB.run( true );

        testSync.pause( ); // 16 
        cout << " Waiting for remote invocation." << endl;
        testSync.pause( ); // 17 
    }
    
    cout << "Success" << endl << endl;
    return 0;

} catch (...) {
    cerr << "Failure: " << carma::util::getStringForCaught() << endl;
    return 1;
}
    
