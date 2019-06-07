#include "carma/corba/Client.h"
#include "carma/corba/Server.h"
#include "carma/corba/Test/HelloImpl.h"
#include "carma/corba/Test/Hello.h"
#include "carma/corba/Test/Hello_skel.h"
#include "carma/corba/Test/Hello_skel_tie.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"

#include <boost/foreach.hpp>
#include <iostream>

using namespace carma::corba;
using namespace carma::corba::test;
using namespace carma::util;
using namespace std;

// @version $Revision: 1.5 $ $Date: 2013/02/09 00:02:17 $ 
//
// @usage tNameService imr=< imrhostname<:port> >
//
// @noKeys
//
// @logger TEST_FACILITY  carma.test.corba.tNameService
//
// @description Test program for carma::corba::Client class.
int Program::main( ) 
try {
    const string seperator( "." );

    carma::corba::test::HelloImpl impl;

    vector< string > contextNames;
    contextNames.push_back( "world.northamerica.usa" );
    contextNames.push_back( "world.europe.france" );
    contextNames.push_back( "world.europe.spain" );
    contextNames.push_back( "" );

    vector< string > objectNames;
    objectNames.push_back( "hello" );
    objectNames.push_back( "ola" );
    objectNames.push_back( "bonjour" );
    objectNames.push_back( "" );
    
    cout << "Testing basic functionality by creating and destroying some "
        << "bindings and contexts." << endl;

    BOOST_FOREACH( const string context, contextNames ) {
    
        cout << "Creating Server object." << endl;
        Server server( getExtraArgc(), getExtraArgv() );

        cout << "Creating Client object." << endl;
        carma::corba::Client client( getExtraArgc(), getExtraArgv() );

        BOOST_FOREACH( const string object, objectNames ) {
            const string name = context + seperator + object; 

            cout << " Adding obj \"" << name << "\" to nameservice." << endl;
            server.addServant< POA_carma::corba::test::Hello_tie,
                               carma::corba::test::HelloImpl >( impl, name );

            cout << " Retrieving " << name << " from name service." << endl;
            test::Hello_var remoteDO = client.resolveName<Hello>( name );

            cout << " Invoking" << endl;
            remoteDO->hi();

        }
       
        cout << "Deleting Client instance." << endl << endl;
    }

    cout << "Success!" << endl;
    return 0;
} catch (...) {
    cerr << "Failure!  Error msg is: " << getStringForCaught() << endl;
    return 1;
}
