
#include "carma/corba/Server.h"
#include "carma/corba/Test/Wait.h"
#include "carma/corba/Test/Wait_skel.h"
#include "carma/corba/Test/Wait_skel_tie.h"
#include "carma/corba/Test/Waiter.h"
#include "carma/util/Program.h"

#include <iostream>

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
int Program::main ( ) {
    test::Waiter waiter;

    const int argc = getExtraArgc();
    char * const * argv = getExtraArgv();
    int localArgc = 0;
    char * localArgv[argc];
    for ( int argIdx = 0; argIdx < argc; ++argIdx ) {
        if ( strcmp( argv[ argIdx ], "-ORBServerId" ) == 0 ) {
            cout << "!!! ";
            argIdx += 2;
            if ( argIdx >= argc ) break;
        }
        ++localArgc;
        localArgv[argIdx] = argv[argIdx];
        cout << localArgv[argIdx] << " ";
    }
    cout << endl;
            
    Server server( localArgc, localArgv );

    // Server & server = Program::getProgram().getCorbaServer( );

    server.addServant< POA_carma::corba::test::Wait_tie >( waiter, 
                                                           test::WAIT_NAME );
    server.run( false );
    return 0;
}
