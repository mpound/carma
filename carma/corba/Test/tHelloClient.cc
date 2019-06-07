#include "carma/corba/Client.h"
#include "carma/corba/Test/Hello.h"
#include "carma/corba/Test/Terminator.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"

#include <iostream>

using namespace carma::corba;
using namespace carma::corba::test;
using namespace carma::util;
using namespace std;

// @version $Revision: 1.1 $ $Date: 2012/02/11 02:41:02 $ 
//
// @usage With tHelloServer running: tHelloClient imr=<imr hostname>
//
// @logger TEST_FACILITY carma.test.corba.tHelloClient
// 
// @noKeys
//
// @description Simple corba client application.  Use with tHelloServer.
// 
int Program::main( ) 
try {

    Client client( Program::getProgram().getExtraArgc(),
                   Program::getProgram().getExtraArgv() );

    cout << "CLIENT: Retrieving " << HELLO_NAME << " DO." << endl;
    Hello_var hello = client.resolveName< Hello >( HELLO_NAME );

    cout << "CLIENT: Retrieving " << TERMINATOR_NAME << "DO." << endl;
    Terminator_var term = client.resolveName< Terminator >( TERMINATOR_NAME );

    cout << "CLIENT: Invoking 'hi' remotely." << endl;
    hello->hi();

    cout << "CLIENT: Invoking 'kill' remotely." << endl;
    term->kill();

    cout << "CLIENT: Success!" << endl;
    return 0;
} catch (...) {
    cerr << "CLIENT Error: " << getStringForCaught() << endl;
    return 1;
}
