#include "carma/corba/Client.h"
#include "carma/corba/Server.h"
#include "carma/corba/Test/ObjectManager.h"
#include "carma/corba/Test/ObjectManager_skel.h"
#include "carma/corba/Test/ObjectManager_skel_tie.h"
#include "carma/corba/Test/ObjectManagerImpl.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"

#include <iostream>
#include <unistd.h>
        
using namespace carma::util;
using namespace carma::corba;
using namespace carma::corba::test;
using namespace std;

namespace POA_cct = POA_carma::corba::test;

int 
invokeClient( )
try {
    Client client( Program::getProgram().getExtraArgc(), 
                   Program::getProgram().getExtraArgv() );

    cout << "CLIENT: Retrieving " << OBJECT_MANAGER_NAME 
         << " from name server." << endl;
    test::ObjectManager_var clientManager = 
        client.resolveName< test::ObjectManager >( OBJECT_MANAGER_NAME );

    cout << "CLIENT: Invoking 'hi()' remote DO call through manager." << endl;
    clientManager->Hello()->hi();
    
    cout << "CLIENT: Invoking 'wait()' remote DO call through manager." << endl;
    clientManager->Wait()->pause(100);

    cout << "CLIENT: Invoking 'kill()' remote DO call through manager." << endl;
    clientManager->Terminator()->kill();

    cout << "CLIENT: Success!" << endl;

    return 0;

} catch (...) {
    cerr << "CLIENT: " << getStringForCaught() << endl;
    return 1;
}
    

// @version $Revision: 1.1 $ $Date: 2012/02/11 02:41:02 $ 
//
// @usage tObjectManagerServer imr=< imr hostname >
//
// @logger TEST_FACILITY carma.test.corba.tObjectManagerServer
// 
// @key server false bool Run as server only (default is both server & client).
// @key client false bool Run as client only (default is both server & client).
//
// @description Example program demonstrating corba::Server class usage 
//              for more complicated IDL and C++ servants.
int Program::main( ) 
try {

    bool client = getBoolParameter( "client" );
    bool server = getBoolParameter( "server" );
    if ( !client && !server ) { // Force both client and server if neither.
        client = true;
        server = true;
    }

    if ( client && !server ) 
        return invokeClient();

    cout << "SERVER: Creating Server objects." << endl;
    // carma::corba::Server & corbaServer = Program::getCorbaServer(); 
    carma::corba::Server corbaServer( getExtraArgc(), getExtraArgv() );
    
    const string name = OBJECT_MANAGER_NAME;

    { 
        cout << "SERVER: Creating ObjectManager instance." << endl;
        carma::corba::test::ObjectManagerImpl objectManager( corbaServer );

        cout << "SERVER: Adding manager as '" << name << "'." << endl;
        corbaServer.addServant< POA_cct::ObjectManager_tie >( objectManager, 
                                                              name );

        const bool runInSeparateThread = server && client;
        cout << "SERVER: Starting server in " 
             << ( runInSeparateThread ? "separate" : "main" ) 
             << " thread." << endl;
        corbaServer.run( runInSeparateThread );

        if ( client ) {
            cout << "SERVER: Invoking client from main thread." << endl;
            invokeClient();
        }

        cout << "SERVER: Destroying ObjectManager instance." << endl;
    }

    cout << "SERVER: Success!" << endl;

    return 0;

} catch (...) {

    cerr << "SERVER: " << carma::util::getStringForCaught() << endl;
    return 1;

}
    
