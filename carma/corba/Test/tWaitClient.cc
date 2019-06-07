#include "carma/corba/Client.h"
#include "carma/corba/Test/Wait.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"

#include <iostream>
#include <string>

using namespace carma;
using namespace carma::util;
using namespace carma::corba;
using namespace std;

// @version $Revision: 1.2 $ $Date: 2012/02/11 02:41:02 $ 
//
// @usage tWait 
//
// @logger TEST_FACILITY carma.test.corba.tWait
// 
// @noKeys
//
// @description Simple server to test simultaneous access. 
int Program::main ( ) 
try {
    Client client( getExtraArgc(), getExtraArgv() );

    test::Wait_var waiter = client.resolveName< test::Wait >( test::WAIT_NAME );

    while ( true ) {
        cout << "Calling wait for 650 s" << endl;
        waiter->pause( 650000 );
        // cout << "Sleeping for 5ms" << endl;
        // sleep( 5 );
    }
    return 0;
} catch (...) {
    cerr << getStringForCaught() << endl;
    return 1;
}
