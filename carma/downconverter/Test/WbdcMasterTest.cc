/**@file
 * CppUnit Test class definition for WbdcMaster class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.18 $
 * $Date: 2012/02/15 21:40:20 $
 * $Id: WbdcMasterTest.cc,v 1.18 2012/02/15 21:40:20 abeard Exp $
 */

#include "carma/downconverter/Test/WbdcMasterTest.h"
#include "carma/downconverter/common/downconverterSystem.h"
#include "carma/downconverter/common/downconverterSystem_skel.h"
#include "carma/downconverter/common/downconverterSystem_skel_tie.h"
#include "carma/monitor/WbdcSubsystem.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

#include <unistd.h>

using namespace carma;
using namespace carma::canbus;
using namespace carma::downconverter;
using namespace carma::util;
using namespace std;

namespace {

    const Trace::TraceLevel TRACE_TESTS = Trace::TRACE1;

} // namespace < unnamed >

WbdcMasterTest::WbdcMasterTest( ) :
    server_( Program::getProgram().getCorbaServer() )
{

}

// -----------------------------------------------------------------------------
void WbdcMasterTest::setUp()
{
   try {

        monitor::WbdcSubsystem wbdcSubsys;
   

        master_ = new WbdcMaster( server_, wbdcSubsys );

        carma::downconverter::System_ptr systemPtr;

        server_.addServant< POA_carma::downconverter::System_tie >(
                    *master_,
                    systemPtr );
        
        client_ = new ClientTest( systemPtr );
        
     } catch (exception& ex) {
        cerr << ex.what() << endl;
     } catch (CORBA::Exception &cex) {
        cerr << cex << endl;
     }
}

// -----------------------------------------------------------------------------
void WbdcMasterTest::tearDown() 
{
    delete client_;
    delete master_;
}
    
// -----------------------------------------------------------------------------
void WbdcMasterTest::testDeviceInternals()
{
    CARMA_CPTRACE( TRACE_TESTS, "Testing device internals..." );

    // Just let things run for a while...  This assures that the Master 
    // simulator thread starts up which calls simulateMsg and processMsg for
    // every CAN message for each device.  
    // This performs the majority of coverage.
    sleep(5);
}

// -----------------------------------------------------------------------------
void WbdcMasterTest::testSystemControls()
{
    CARMA_CPTRACE( TRACE_TESTS, "Testing system controls..." );
    
    const int result = client_->runSystemControlsTest();
    while (!master_->isDone()) 
        server_.work();
    
    CPPUNIT_ASSERT(result == EXIT_SUCCESS);
}

// -----------------------------------------------------------------------------
void WbdcMasterTest::testDownconverterControls()
{
    CARMA_CPTRACE( TRACE_TESTS, "Testing downconverter controls..." );

    const int result = client_->runDownconverterControlsTest(); // Queue 'em up.
    while (!master_->isDone()) 
        server_.work();

    CPPUNIT_ASSERT(result == EXIT_SUCCESS);
}

// -----------------------------------------------------------------------------
void WbdcMasterTest::testQuadModControls()
{
    CARMA_CPTRACE( TRACE_TESTS, "Testing quad mod controls..." );
    
    const int result = client_->runQuadModControlsTest();
    while (!master_->isDone()) 
        server_.work();

    CPPUNIT_ASSERT(result == EXIT_SUCCESS);
}

// -----------------------------------------------------------------------------
void WbdcMasterTest::testNoiseSourceControls()
{
    CARMA_CPTRACE( TRACE_TESTS, "Testing noise source controls..." );
    
    const int result = client_->runNoiseSourceControlsTest();
    while (!master_->isDone()) 
        server_.work();

    CPPUNIT_ASSERT(result == EXIT_SUCCESS);
}

// -----------------------------------------------------------------------------
void WbdcMasterTest::testLoMonitorControls()
{
    CARMA_CPTRACE( TRACE_TESTS, "Testing LO monitor controls..." );
    
    const int result = client_->runLoMonitorControlsTest();
    while (!master_->isDone()) 
        server_.work();

    CPPUNIT_ASSERT(result == EXIT_SUCCESS);
}
