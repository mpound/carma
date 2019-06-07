#include <stdexcept>
#include <iostream>
#include <exception>

#include "carma/util/Program.h"
#include "carma/util/StartPthread.h"
#include "carma/util/ExceptionUtils.h"

using namespace ::std;
using namespace carma::util;


namespace {


void
myUnexpectedHandler( ) try {
    cerr << "We are fubar #1." << endl;
    
    try {
        throw;
    } catch ( const ::std::exception & e ) {
        cerr << "Type was ::std::exception derived." << endl;
    } catch ( ... ) {
        cerr << "Type was not ::std::exception derived." << endl;
    }
    
    terminate( );
} catch ( ... ) {
    terminate( );
}


class MyError {
    public:
        MyError( ) { }
};


class MyOtherError {
    public:
        MyOtherError( ) { }
};


void
violateThrowSpec( ) throw ( MyError ) {
    throw MyOtherError( );
}


void
threadEntry( const int & wait ) {
    set_unexpected( myUnexpectedHandler );

    sleep( wait );
    
    violateThrowSpec( );
}


}  // namespace < anonymous >


//
// @version $Revision: 1.6 $
//
// @usage Use it
//
// @description
// Does stuff
//
// @key wait  7  integer Time to wait in seconds
//
// @logger TEST_FACILITY carma.test.util.tUnexpected
//


int
Program::main( ) {
    const int wait = getIntParameter( "wait" );
    
    sleep( 1 );
    
    StartPthreadWithCopy( threadEntry, wait );

    sleep( wait + 60 );
    
    return 0;
}
