#include "carma/corba/Server.h"

// Note the inclusion pattern to pick up tie classes
#include "carma/corba/Test/Hello.h"
#include "carma/corba/Test/Hello_skel.h"
#include "carma/corba/Test/Hello_skel_tie.h"
#include "carma/corba/Test/HelloImpl.h"
#include "carma/corba/Test/Terminator.h"
#include "carma/corba/Test/Terminator_skel.h"
#include "carma/corba/Test/Terminator_skel_tie.h"
#include "carma/corba/Test/TerminatorImpl.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"

#include <iostream>

using namespace carma::util;
using namespace carma::corba;
using namespace carma::corba::test;
using namespace std;


// @version $Revision: 1.2 $ $Date: 2012/02/11 02:41:02 $ 
//
// @usage tHelloServer imr=<imr hostname>
//
// @logger TEST_FACILITY carma.test.corba.tHelloServer
// 
// @noKeys
//
// @description Simple corba server example. Use with tHelloClient.
//
int Program::main( ) 
try {

    namespace POA_cct = POA_carma::corba::test;

    cout << "SERVER: Creating corba server." << endl;
    // carma::corba::Server corbaServer( getExtraArgc(), getExtraArgv() );
    carma::corba::Server & corbaServer = getCorbaServer();

    carma::corba::test::HelloImpl hello;

    cout << "SERVER: Adding Hello servant as '" << HELLO_NAME << "'." << endl;
    corbaServer.addServant< POA_cct::Hello_tie >( hello, HELLO_NAME );

    // Add a 'Terminator' servant so we can remotely terminate this app.
    carma::corba::test::TerminatorImpl terminator( corbaServer );
    cout << "SERVER: Adding Terminator servant as '" << TERMINATOR_NAME << "'.";
    cout << endl;
    corbaServer.addServant< POA_cct::Terminator_tie >( terminator, 
                                                       TERMINATOR_NAME );

    cout << "SERVER: Running server from main thread." << endl;
    corbaServer.run( false ); 

    cout << "SERVER: Success!" << endl;

    return 0;

} catch (...) {
    cerr << "SERVER: " << carma::util::getStringForCaught() << endl;
    return 1;
}
