#include "carma/corba/Client.h"
#include "carma/corba/Test/Hello.h"
#include "carma/corba/Test/Terminator.h"
#include "carma/corba/Test/Wait.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"

#include <boost/thread.hpp>
#include <iostream>

using namespace boost::posix_time;
using namespace carma::corba;
using namespace carma::corba::test;
using namespace carma::util;
using namespace std;

// @version $Revision: 1.1 $ $Date: 2012/11/28 18:33:56 $ 
//
// @usage With tServerExhaustive running: tServerExhaustiveClient imr=<imrhost>
//
// @logger TEST_FACILITY carma.test.corba.tServerExhaustiveClient
// 
// @noKeys
//
// @description Client component to tServerExhaustive.
// 
int Program::main( ) 
try {

    Client client( Program::getProgram().getExtraArgc(),
                   Program::getProgram().getExtraArgv() );

    // Use program client to retrieve test sync option (choreographs 
    // client and server interaction during test).
    test::Wait_var testSync = client.resolveName< test::Wait >( 
        "carma.corba.testSync" );

    testSync->pause( 0 ); // 1

    {
        testSync->pause( 0 ); // 2

        cout << "Retrieving hello DO for test1." << flush;
        Hello_var hello = client.resolveName< Hello >( "test1.hello" );

        cout << " Invoking hi." << flush;
        hello->hi();
        cout << " Success." << endl;

        testSync->pause( 0 ); // 3
    }

    { 
        testSync->pause( 0 ); // 4

        cout << "Retrieving hello DO for test2." << flush;
        Hello_var hello2 = client.resolveName< Hello >( "test2.hello" );

        cout << " Invoking hi." << flush;
        hello2->hi();
        cout << " Success." << endl;

        testSync->pause( 0 ); // 5
    }

    { 
        testSync->pause( 0 ); // 6

        cout << "Retrieving hello DO for test3." << flush;
        Hello_var hello3 = client.resolveName< Hello >( "test3.hello" );

        cout << " Invoking hi." << flush;
        hello3->hi();
        cout << " Success." << endl;

        testSync->pause( 0 ); // 7
    }

    { 
        testSync->pause( 0 ); // 8 

        cout << "Retrieving hello DO for test4." << flush;
        Hello_var hello3 = client.resolveName< Hello >( "test4.hello" );

        cout << " Invoking hi." << flush;
        hello3->hi();
        cout << " Success." << endl;

        testSync->pause( 0 ); // 9 
    }
    
    { 
        testSync->pause( 0 ); // 10 

        cout << "Retrieving hello DO for test5." << flush;
        Hello_var hello5 = client.resolveName< Hello >( "test5.hello" );

        cout << " Invoking hi." << flush;
        hello5->hi();
        cout << " Success." << endl;

        testSync->pause( 0 ); // 11
    }
    
    { 
        testSync->pause( 0 ); // 12

        cout << "Retrieving hello DO for test6." << flush;
        Hello_var hello6 = client.resolveName< Hello >( "test6.hello" );

        cout << " Invoking hi." << flush;
        hello6->hi();
        cout << " Success." << endl;

        testSync->pause( 0 ); // 13
    }
    
    { 
        testSync->pause( 0 ); // 14

        cout << "Retrieving hello DO for test7." << flush;
        Hello_var hello7A = client.resolveName< Hello >( "test7.helloA" );

        const int numCalls( 4 );
        cout << " Invoking hi " << numCalls << " times." << flush;
        for ( int i = 0; i < numCalls; ++i ) {
            hello7A->hi();
        }
            
        cout << " Success." << endl;

        testSync->pause( 0 ); // 15
    }

    { 
        testSync->pause( 0 ); // 16

        cout << "Retrieving hello DO for test8." << flush;
        Hello_var hello7A = client.resolveName< Hello >( "test8.helloA" );
        Hello_var hello7B = client.resolveName< Hello >( "test8.helloB" );

        const int numCalls( 4 );
        cout << " Invoking hi " << numCalls << " times." << flush;
        for ( int i = 0; i < numCalls; ++i ) {
            hello7A->hi();
            hello7B->hi();
        }
            
        cout << " Success." << endl;

        testSync->pause( 0 ); // 17
    }

    return 0;

} catch (...) {
    cerr << getStringForCaught() << endl;
    return 1;
}
