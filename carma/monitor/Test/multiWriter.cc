/*
 * Test multiple instances of a program writing to the same monitor frame.
 *
 * @author: Marc Pound
 *
 * $Id: multiWriter.cc,v 1.8 2012/01/18 18:48:43 mpound Exp $
 * $CarmaCopyright$
 *
 */

// Keyword setup...
// @version     $Revision: 1.8 $ $Date: 2012/01/18 18:48:43 $
// @usage    testMPwriterReader inst
// @description
// instance 1 will write mpchar, mpbyte, bpshort
// instance 2 will write mpint, mpbool, mpfloat
// instance 3 will write mpdouble, mpcomplex, 
// instance 4 will write mpmjd, mpstring
// instance 5 will write serialno, color
//
// @key iterations  0          integer  number of writes - 0 means write forever
// @key instance    @mandatory integer A number between 1 and 5 indicating 
// which instance of the program this is. Each instance writes a few 
// different monitor points in the TestSubsystem and invokes 
// MonitorFrame.write().  Each instance sleeps for 5 seconds between
// iterations.   All monitor points must be set as persistent in the MPML
// file, so that testFSP knows they are persistent.
//
// @logger MONITOR_FACILITY carma.monitor.multiWriter


#include <iomanip>
#include <iostream>
#include <values.h>

#include "carma/util/Program.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/Time.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/monitorPointSpecializations.h"
#include "carma/monitor/TestSubsystemExt.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


namespace {


void
writeSampleValues( TestSubsystem &    testSubsystem,
                   const unsigned int i,
                   const unsigned int instanceNo )
try {
    const string riddleArray[ ] = {
        "Why", "did", "the", "chicken",  "cross", "the", "road?"
    };
    
    const ::size_t riddleArrayCount =
        (sizeof( riddleArray ) / sizeof( riddleArray[ 0 ] ));
    
    int ival = 23 + i;

    testSubsystem.timestamp().setValue (Time::MJD());
    TestSubsystem::Box& mc = testSubsystem.box();
    // have  to initialize these here or 
    // compiler complains about initialization crosses
    //
    MonitorPointChar& mpc  = mc.mpchar();
    MonitorPointByte& mpb  = mc.mpbyte();
    MonitorPointShort& mps = mc.mpshort();
    MonitorPointBool& mybool = mc.mpbool();
    MonitorPointInt& mpi   = mc.mpint();
    MonitorPointFloat& mpf     =  mc.mpfloat();
    MonitorPointDouble& mpd    =  mc.mpdouble();
    MonitorPointComplex& mpcx  =  mc.mpcomplex();
    MonitorPointAbstime& mptime   =  mc.mpmjd();
    MonitorPointString& mpstr = mc.mpstring();
    MonitorPointSerialNo& mpsn = mc.mpserialno();

    switch ( instanceNo ) {
        case 1:
            {
                mpb.setValidity(MonitorPoint::VALID_GOOD, 0);
                mpb.setNumSamples(1);
                // set the average value (sampleNo = 0) since rtd shows that.
                mpb.setValue(ival,0);
                //mpb.setValidity(MonitorPoint::INVALID_NO_HW,  0);
    
                mps.setNumSamples(1);
                mps.setValue(ival*2, 0);
    
                // count from A to Z
                const char cval = static_cast< char >( 'A' + (i % 26) );
                mpc.setValue(cval);
            }
            break;
        case 2:
            {
                mpi.setNumSamples(1);
                mpi.setValue(ival);
                mpi.setValidity(MonitorPoint::VALID_ERROR_LOW,    0);
                
                //alternate between true and false.
                const bool myvalue = ((i % 2) == 0);
                mybool.setValue(myvalue);
    
                const double fval = 52.12345678 + static_cast< double >( i );
                mpf.setValue(fval);
            }
            break;
            
        case 3:
            {
                const double dval = 100.98765432 - static_cast< double >( i );
                mpd.setNumSamples(1);
                mpd.setValue(dval,0); 
                complex<float> cxval(43.12345678+0.1*i, -2.54321 - 0.1*i);
                mpcx.setValue(cxval);
            }
            break;
            
        case 4:
            {
                mptime.setValue(Time::MJD());
                const ::size_t index = (i % riddleArrayCount);
                mpstr.setValue(riddleArray[index]);
            }
            break;
            
        case 5:
            {
                const int snval = 8675309 + i;
                mpsn.setValue(snval);
                switch ( i % 3 ) {
                    default:
                    case 0: mc.color().setValue( 
                        TestSubsystem::ColorMonitorPointEnum::RED);
                        break;
                    case 1: mc.color().setValue( 
                        TestSubsystem::ColorMonitorPointEnum::GREEN);
                        break;
                    case 2: mc.color().setValue( 
                        TestSubsystem::ColorMonitorPointEnum::BLUE);
                        break;
                }
            }
            break;
            
        default:
            {
                // should never get here.
                cerr << " bad instance value! (" << instanceNo << ")" << endl;
                
                exit(-1);
            }
            break;
    }

    testSubsystem.write();
} catch ( const carma::util::BaseException & be ) {
    cerr << "writeSamples caught exception: " << be.getMessage() << endl;

    exit(-1);
} catch ( const ::std::exception & e ) {
    cerr << "writeSamples caught exception: " << e.what() << endl;

    exit(-1);
} catch ( ... ) {
    cerr << "writeSamples caught unknown exception"<< endl;

    exit(-1);
}


}  // namespace < anonymous >


int
Program::main() {
    const int numIterationsParam = getIntParameter( "iterations" );
    const int instanceNoParam = getIntParameter( "instance" );

    if ( numIterationsParam < 0 ) {
        cerr << "ERROR: Iterations number must be non-negative." << endl;

        return EXIT_FAILURE;
    }

    if ( (instanceNoParam < 1) || (instanceNoParam > 5) ) {
        cerr << "ERROR: Instance number must be between 1 and 5." << endl;

        return EXIT_FAILURE;
    }

    const unsigned int numIterations =
        ((numIterationsParam == 0) ? 
            static_cast< unsigned int >( MAXINT ) :
            static_cast< unsigned int >( numIterationsParam ));

    const unsigned int instanceNo =
        static_cast< unsigned int >( instanceNoParam );

    // Connection to test subsystem frame
    TestSubsystem       testWriter;

    // Connection to carma system frame
    /**
      MonitorSystem & carma =
          (rawMode ? (*(new RawCarmaMonitorSystem)) : 
                     (*(new CarmaMonitorSystem));

      TestSubsystem & testReader = carma.test();
    */


    //testWriter.startAutoWriter();     

    FrameAlignedTimer fat;
    fat.ResetNextFireTime();
    
    for ( unsigned int  i = 0;  i < numIterations; ++i )  {
        fat.WaitForNextFireTime();
        cout << "Fired!!" << endl;
        writeSampleValues(testWriter, i, instanceNo);            
        sleep(5);
    }

    return EXIT_SUCCESS;
}
