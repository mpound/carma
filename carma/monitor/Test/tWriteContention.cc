#include "carma/monitor/MonitorPointSet.h"
#include "carma/monitor/TestSubsystem.h"
#include "carma/util/Program.h"

#include <boost/thread.hpp>
#include <iostream>

using namespace boost;
using namespace carma::util;
using namespace carma::monitor;
using namespace std;

namespace {

struct Autowriter {

    TestSubsystem & testSubsystem_;
    const string term_;
    const bool autowrite_;
    
    Autowriter( TestSubsystem & testSs, string term, bool autowrite ) : 
        testSubsystem_( testSs ),
        term_( term ),
        autowrite_( autowrite ) { };

    Autowriter( const Autowriter & a ) : 
        testSubsystem_( a.testSubsystem_ ),
        term_( a.term_ ),
        autowrite_( a.autowrite_ ) { };

    ~Autowriter( ) { };
    
    void operator( ) ( ) { 
        while ( true ) { 
            testSubsystem_.box().mpstring().setValue(term_);
            if ( autowrite_ ) testSubsystem_.monitorPointSet().write( true ); 
            cout << "." << flush;
        } 
    };
}; 

} // namespace < unnamed >

/**
 * @author Andy Beard 
 *
 * @version $Id: tWriteContention.cc,v 1.1 2013/05/15 15:20:32 abeard Exp $
 * 
 * @usage tWriteContention
 * 
 * @description
 * Test sychronization between monitor subsystem write() and getValue() methods.
 *
 * @noKeys
 *
 * @logger TEST_FACILITY carma.test.monitor.tWriteContention
 */
int Program::main( ) 
{
    TestSubsystem testSubsys;

    const string phrase1( "asfasrwer" ), phrase2( "fgegewqrqp" );
    Autowriter awBlah( testSubsys, phrase1, false );
    Autowriter awBah( testSubsys, phrase2, false );

    // boost::thread autowriterBlahThread( boost::ref( awBlah ) ); 
    boost::thread autowriterBahThread( boost::ref( awBah ) ); 

    // Now set and get values until the cows come home.  If they ever
    // don't match, there's a monitor system synch issue.
    bool match( true );
    string got;
    TestSubsystem::Box & box = testSubsys.box();
    box.mpstring().setValue( phrase1 );
    while ( match ) {
        cout << "-" << flush;
        got = box.mpstring().getValue();
        match = ( got == phrase1 || got == phrase2 );
    }

    return 0;
}
