#include <iostream>
#include <sstream>

#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/loggingUtils.h"
#include "carma/util/Program.h"
#include "carma/util/programExtras.h"
#include "carma/util/programLogging.h"

using namespace ::std;
using namespace ::log4cpp;
using namespace carma;
using namespace carma::util;


namespace {


class ExpectedExceptionNotThrownError : public ErrorException {
    public:
        ExpectedExceptionNotThrownError( const string & msg,
                                         const char *   filename,
                                         int            lineNo );
};


ExpectedExceptionNotThrownError::ExpectedExceptionNotThrownError(
    const string &     msg,
    const char * const filename,
    const int          lineNo ) :
ErrorException( ("Expected exception was not thrown for " + msg),
                filename,
                lineNo )
{
}


}  // namespace < anonymous >


//
// @noKeys
//
// @logger TEST_FACILITY carma.test.util.frameAlignedTimerTest
//

int
Program::main( )
try {
    cout << "Starting frame aligned timer tests" << endl;

    const long kNanosPerSecond = 1000L * 1000L * 1000L;

    {
        cout << "    Testing constructor parameter values" << flush;

        for ( long offsetNanos = -2 * kNanosPerSecond;
              offsetNanos <= 2 * kNanosPerSecond;
              offsetNanos += (kNanosPerSecond / 10) ) {
            for ( long periodFrames = -5; periodFrames <= 5; ++periodFrames ) {
                const bool exceptionExpected =
                    ((offsetNanos < 0) ||
                     (offsetNanos >= (kNanosPerSecond / 2)) ||
                     (periodFrames <= 0));

                bool exceptionThrown = false;

                try {
                    FrameAlignedTimer timer( offsetNanos, periodFrames );
                } catch ( ... ) {
                    exceptionThrown = true;

                    if ( exceptionExpected == false )
                        throw;
                }

                if ( exceptionExpected && (exceptionThrown == false) ) {
                    ostringstream oss;
                    
                    oss << "bad c'tor params (offsetNanos="
                        << offsetNanos << ", periodFrames="
                        << periodFrames << ")";
                        
                    throw CARMA_EXCEPTION( ExpectedExceptionNotThrownError,
                                           oss.str() );
                }
            }

            cout << '.' << flush;
        }

        cout << endl;
    }


    {
        cout << "    Testing wait without a reset" << endl;

        bool exceptionThrown = false;

        try {
            FrameAlignedTimer timer;

            timer.WaitForNextFireTime();
        } catch ( ... ) {
            exceptionThrown = true;
        }

        if ( exceptionThrown == false ) {
            throw CARMA_EXCEPTION( ExpectedExceptionNotThrownError,
                                   "bad WaitForNextFireTime state" );
        }
    }


    {
        cout << "    Testing reset delay parameter values" << flush;

        long delayFrames = -200;

        do {
            const bool exceptionExpected = (delayFrames < 0);
            bool exceptionThrown = false;

            try {
                FrameAlignedTimer timer;

                timer.ResetNextFireTime( delayFrames );
            } catch ( ... ) {
                exceptionThrown = true;

                if ( exceptionExpected == false )
                    throw;
            }

            if ( exceptionExpected && (exceptionThrown == false) ) {
                ostringstream oss;
                
                oss << "bad ResetNextFireTime param (delayFrames="
                    << delayFrames << ")";

                throw CARMA_EXCEPTION( ExpectedExceptionNotThrownError,
                                       oss.str() );
            }

            cout << '.' << flush;

            delayFrames += 11;
        } while ( delayFrames <= 200 );

        cout << endl;
    }

    {
        cout << "    Testing reset-and-wait delay parameter values"
             << flush;

        long delayFrames = -200;

        do {
            const bool exceptionExpected = (delayFrames < 0);
            bool exceptionThrown = false;

            try {
                FrameAlignedTimer timer;

                timer.ResetNextFireTimeAndWait( delayFrames );
            } catch ( ... ) {
                exceptionThrown = true;

                if ( exceptionExpected == false )
                    throw;
            }

            if ( exceptionExpected && (exceptionThrown == false) ) {
                ostringstream oss;
                
                oss << "bad ResetNextFireTimeAndWait param (delayFrames="
                    << delayFrames << ")";

                throw CARMA_EXCEPTION( ExpectedExceptionNotThrownError,
                                       oss.str() );
            }

            cout << '.' << flush;

            if ( delayFrames < 0 ) {
                delayFrames += 11;

                if ( delayFrames >= 0 )
                    delayFrames = 0;
            } else
                delayFrames += 1;
        } while ( delayFrames <= 5 );

        cout << endl;
    }

    {
        cout << "    Testing reset followed by wait" << endl;

        FrameAlignedTimer timer;

        timer.ResetNextFireTime();

        timer.WaitForNextFireTime();
    }

    {
        cout << "    Testing reset-and-wait" << endl;

        FrameAlignedTimer timer;

        timer.ResetNextFireTimeAndWait();
    }

    {
        cout << "    Testing reset followed by multiple waits" << flush;

        FrameAlignedTimer timer;

        timer.ResetNextFireTime();

        for ( int i = 0; i <= 10; ++i ) {
            timer.WaitForNextFireTime();

            cout << '.' << flush;
        }

        cout << endl;
    }

    {
        cout << "    Testing reset-and-wait followed by multiple waits"
             << flush;

        FrameAlignedTimer timer;

        for ( int i = 0; i <= 10; ++i ) {
            if ( i == 0 )
                timer.ResetNextFireTimeAndWait();
            else
                timer.WaitForNextFireTime();

            cout << '.' << flush;
        }

        cout << endl;
    }

    cout << "Done with frame aligned timer tests" << endl;

    return 0;
} catch ( ... ) {
    const string msg =  "Caught exception coming out of Program::main - " +
                        getStringForCaught();

    cerr << endl << "ERROR: " << msg << endl;
    
    programLogErrorIfPossible( msg );
    
    if ( caughtBacktraceCaptured() ) {
        const string btText =
            getCaughtBacktraceAsString( "Exception bt ", "\n" );
            
        cerr << btText << endl;
        
        Category * const logger = getProgramLoggerIfAvailable();
    
        if ( logger != 0 )
            logMultipleLines( *logger, Priority::ERROR, btText );
    }

    return -2;
}
