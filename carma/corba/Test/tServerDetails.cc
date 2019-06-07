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
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"

#include <boost/thread.hpp>
#include <iostream>

using namespace boost::posix_time;
using namespace carma::util;
using namespace carma::corba;
using namespace std;

struct ClientWorker 
{
    test::Hello_ptr hello_;
    Server & server_;

    ClientWorker( test::Hello_ptr & helloClient,
                  Server & server ) : 
        hello_( helloClient ),
        server_( server ) { };

    void operator( ) ( ) {
        boost::this_thread::sleep( seconds( 2 ) );
        hello_->hi();
        server_.stop();
    }

};

struct ClientKiller 
{
    test::Terminator_ptr terminator_;

    ClientKiller( test::Terminator_ptr & terminator ) : 
        terminator_( terminator ) { };

    void operator( ) ( ) try {
        boost::this_thread::sleep( seconds( 1 ) );
        terminator_->kill();
    } catch (...) {
        cerr << "ClientKiller thread: " << getStringForCaught() << endl;
    }

};
    
void
waitForServerToTerminate( Server & server ) 
{
    cout << " Waiting for server to terminate." << flush;
    while ( !server.terminated() ) {
        boost::this_thread::sleep( milliseconds( 100 ) );
        cout << "." << flush;
    }
    cout << endl;
}

// @version $Revision: 1.4 $ $Date: 2012/11/14 19:42:45 $ 
//
// @usage tServerDetails
//
// @logger TEST_FACILITY carma.test.corba.tServerDetails
// 
// @noKeys 
// 
// @description Detailed test of corba::Server & corba::Client classes. 
int Program::main( ) 
try {

    namespace POA_cct = POA_carma::corba::test; 
        
    corba::test::Hello_ptr helloPtr;

    {
        cout << "Testing corba::Server constructor and destructor." << endl;
        carma::corba::Server server( getExtraArgc(), getExtraArgv() );
        server.run( true );
    }
    
    cout << "Success" << endl << endl;

    {
        cout << "Testing Server::run() in separate thread w/ stop." << endl;

        carma::corba::test::HelloImpl hello;

        carma::corba::Server server( getExtraArgc(), getExtraArgv() );
        server.addServant< POA_cct::Hello_tie >( hello, helloPtr );
        server.run( true );
        helloPtr->hi();
        server.stop();
        waitForServerToTerminate( server );
    } 

    cout << "Success" << endl << endl;
    
    {
        cout << "Testing Server::run() in separate thread sans stop." << endl;

        carma::corba::test::HelloImpl hello;

        carma::corba::Server server( getExtraArgc(), getExtraArgv() );
        server.addServant< POA_cct::Hello_tie >( hello, helloPtr );
        server.run( true );
        helloPtr->hi();
        // Let the destructor shut it down.
    } 

    cout << "Success" << endl << endl;

    {
        cout << "Testing sensible behaviour of out-of-order client/run "
                "invocation." << endl;

        carma::corba::test::HelloImpl hello;

        carma::corba::Server server( getExtraArgc(), getExtraArgv() );
        server.addServant< POA_cct::Hello_tie >( hello, helloPtr );
        helloPtr->hi();  // purposely out-of-order
        server.run( true );
        // Let the destructor shut it down.
    } 

    cout << "Success!" << endl << endl;

    { 
        cout << "Testing corba::Server::run in same thread." << endl; 

        carma::corba::test::HelloImpl hello;

        carma::corba::Server server( getExtraArgc(), getExtraArgv() );
        server.addServant< POA_cct::Hello_tie >( hello, helloPtr );
        ClientWorker clientWorker( helloPtr, server );
        boost::thread workerThread( clientWorker );
        server.run( false );
        workerThread.join();
        waitForServerToTerminate( server );
    }

    cout << "Success" << endl << endl;

    {
        cout << "Testing corba::Server::work in same thread." << endl;
        
        carma::corba::Server server( getExtraArgc(), getExtraArgv() );

        carma::corba::test::HelloImpl hello;
        carma::corba::test::Hello_ptr helloPtr;
        server.addServant< POA_cct::Hello_tie >( hello, helloPtr );

        helloPtr->hi();
        cout << " Terminating server..." << flush;
        while ( !server.terminated() ) {
            server.work();
            server.stop();
            cout << "." << flush;
        }
        cout << " terminated." << endl; 
    }

    cout << "Success" << endl << endl;

    { 
        cout << "Testing corba::Server stop() and terminated()." << endl; 

        carma::corba::test::HelloImpl hello;
        carma::corba::Server server( getExtraArgc(), getExtraArgv() );
        server.addServant< POA_cct::Hello_tie >( hello, helloPtr );

        cout << " Checking default terminated() is false..." << flush;
        if ( server.terminated() ) { 
            cout << " it isn't." << endl;
            return 1;
        } else {
            cout << " it is." << endl;
        }

        server.stop();

        cout << " Checking that stop sans run or work terminates..." << flush; 
        if ( server.terminated() ) { 
            cout << " it does." << endl;
        } else {
            cout << " it doesn't." << endl;
            return 1;
        }
        
        cout << " Checking that subsequent work call throws..." << flush;
        try {
            server.work();
            cout << " it doesn't." << endl;
            return 1;
        } catch ( ... ) {
            cout << " it does." << endl;
        }
        
        cout << " Checking that subsequent run call throws..." << flush;
        try {
            server.run();
            cout << " it doesn't." << endl;
            return 1;
        } catch ( ... ) {
            cout << " it does." << endl;
        }
        
    }
    
    cout << "Success" << endl << endl;

    {
        cout << "Testing stop() invocation from DO call in run()." << endl;
        
        carma::corba::Server server( getExtraArgc(), getExtraArgv() );
        
        carma::corba::test::HelloImpl hello;
        carma::corba::test::Hello_ptr helloPtr;

        test::TerminatorImpl terminator( server );
        carma::corba::test::Terminator_ptr terminatorPtr;

        server.addServant< POA_cct::Hello_tie >( hello, helloPtr );
        server.addServant< POA_cct::Terminator_tie >(terminator, terminatorPtr);
        server.run( true );

        ClientKiller killer( terminatorPtr );
        boost::thread killerThread( killer );

        waitForServerToTerminate( server );
    }

    cout << "Success" << endl << endl;

    {
        cout << "Testing stop() invocation from DO call in work()." << endl;
        
        carma::corba::Server server( getExtraArgc(), getExtraArgv() );
        
        carma::corba::test::HelloImpl hello;
        carma::corba::test::Hello_ptr helloPtr;

        test::TerminatorImpl terminator( server );
        carma::corba::test::Terminator_ptr terminatorPtr;

        server.addServant< POA_cct::Hello_tie >( hello, helloPtr );
        server.addServant< POA_cct::Terminator_tie >(terminator, terminatorPtr);

        ClientKiller killer( terminatorPtr );
        boost::thread killerThread( killer );

        cout << " Calling Server::work() from main thread" << flush;
        while ( !server.terminated() ) {
            cout << "." << flush;
            server.work();
            boost::this_thread::sleep( milliseconds( 100 ) );
        }
    }

    cout << "Success" << endl << endl;

    {
        cout << "Testing multiple simultaneous Server instances." << endl;
        
        carma::corba::Server serverA( getExtraArgc(), getExtraArgv() );
        carma::corba::Server serverB( getExtraArgc(), getExtraArgv() );
        
        carma::corba::test::HelloImpl hello;
        carma::corba::test::Hello_ptr helloPtrA;
        carma::corba::test::Hello_ptr helloPtrB;

        cout << " Adding shared HelloImpl servant to servers." << endl;
        serverA.addServant< POA_cct::Hello_tie >( hello, helloPtrA );
        serverB.addServant< POA_cct::Hello_tie >( hello, helloPtrB );

        cout << " Running server A in separate thread." << endl;
        serverA.run( true );
        cout << " Running server B in separate thread." << endl;
        serverB.run( true );

        cout << " Calling hello on both." << endl;
        helloPtrA->hi();
        helloPtrB->hi();
    }
    
    cout << "Success" << endl << endl;

    return 0;

} catch (...) {
    cerr << "Failure: " << carma::util::getStringForCaught() << endl;
    return 1;
}
    
