
/**
 *
 * test if IPQ destructor gets call when a pointer
 * to a monsys is deleted.
 *
 * @author: Marc Pound
 *
 * $Id: monitorDestructorTest.cc,v 1.6 2012/01/18 18:48:43 mpound Exp $
 * $CarmaCopyright$
 *
 */

// @usage       monitorDestructorTest traceLevel=4 traceFile=/dev/stdout useDBMS=false
// @description
//      Test program for monitor/IPQ destructors, using the test subsystem. 
//      Run this with traceLevel=4 traceFile=/dev/stdout useDBMS=false to 
//      see IPQbuffer messages about destructors/file descriptors.
//      If you see lots of opens with closes, there is a problem
//
// @noKeys
// @logger TEST_FACILITY carma.test.monitor.monitorDestructorTest

#include <iostream>

#include "carma/util/Program.h"
#include "carma/monitor/TestSubsystem.h"
#include "carma/monitor/WbdcSubsystem.h"

using namespace std;
using namespace carma::util;
using namespace carma::monitor;


int Program::main() 
{
    try {
        for (int i = 0 ; i < 1024; i++ ) {
            if(i%50 == 0) cout << i << endl;
            TestSubsystem* a= new TestSubsystem();
            //WbdcSubsystem* a= new WbdcSubsystem();
            delete a;
        }

        return EXIT_SUCCESS;
    } catch (BaseException & e) {
        cout << "caught exception: " << e.getMessage() << endl;
        return EXIT_FAILURE;
    } catch ( ... ) {
        cout << "caught unknown error" << endl;
        return EXIT_FAILURE;
    }
}












