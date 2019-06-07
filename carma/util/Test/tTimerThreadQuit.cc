#include <iostream>
#include <stdexcept>

#include <pthread.h>
#include <unistd.h>

#include "carma/util/Program.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ThreadQuit.h"
#include "carma/util/FrameAlignedTimer.h"


using namespace ::std;
using namespace carma::util;

namespace {


::pthread_mutex_t gOneTimeBarrier = PTHREAD_MUTEX_INITIALIZER;
::pthread_mutex_t gSerializedOutputGuard = PTHREAD_MUTEX_INITIALIZER;


struct WorkInfo {
    string   prefix;
    long     periodFrames;
    ::size_t firingsToDeferQuits;
    ::size_t totalFirings;
};


void
WorkLoop( const WorkInfo & workInfo ) {
    // Hold up until we get the all clear
    {
        ScopedLock< ::pthread_mutex_t > lock( gOneTimeBarrier );
    }

    {
        ScopedLock< ::pthread_mutex_t > sl( gSerializedOutputGuard );

        cout << workInfo.prefix << " begins" << endl;
    }

    FrameAlignedTimer timer( 0, workInfo.periodFrames );

    ::size_t firings = 0;

    if ( workInfo.firingsToDeferQuits > 0 ) {
        ScopedThreadQuitDeferSelf quitDefer;

        timer.ResetNextFireTime( );

        while ( ((workInfo.totalFirings == 0) || (firings < workInfo.totalFirings)) && (firings < workInfo.firingsToDeferQuits) ) {
            timer.WaitForNextFireTime( );

            ++firings;

            {
                ScopedLock< ::pthread_mutex_t > sl( gSerializedOutputGuard );

                cout << workInfo.prefix << " F" << firings << " QD" << endl;
            }
        }
    } else
        timer.ResetNextFireTime( );

    while ( (workInfo.totalFirings == 0) || (firings < workInfo.totalFirings) ) {
        timer.WaitForNextFireTime( );

        ++firings;

        {
            ScopedLock< ::pthread_mutex_t > sl( gSerializedOutputGuard );

            cout << workInfo.prefix << " F" << firings << endl;
        }
    }
}


void *
QuittableEntryPoint( void * arg ) {
    try {
        ScopedThreadQuitRegisterSelf quitReg;

        const WorkInfo * workInfo = static_cast< WorkInfo * >( arg );

        if ( workInfo == 0 )
            throw logic_error( "NULL workInfo pointer" );

        WorkLoop( *workInfo );
    } catch ( ... ) {
        try {
            if ( MarkCaughtExceptionOkayToDestructIfThreadQuitRequestedError( ) ) {
                ScopedLock< ::pthread_mutex_t > lock( gSerializedOutputGuard );

                cout << "thread quit by request" << endl;
            } else {
                ScopedLock< ::pthread_mutex_t > lock( gSerializedOutputGuard );

                cerr << "exception caught trying escape main" << endl;

                try {
                    throw;
                } catch ( const ::std::exception & e ) {
                    cerr << "    " << e.what( ) << endl;
                    // stifle our little throw
                } catch ( ... ) {
                    // stifle our little throw
                }
            }
        } catch ( ... ) {
            // stifle any exception
        }
    }

    return 0;
}


void *
NonquittableEntryPoint( void * arg ) {
    try {
        const WorkInfo * workInfo = static_cast< WorkInfo * >( arg );

        if ( workInfo == 0 )
            throw logic_error( "NULL workInfo pointer" );

        WorkLoop( *workInfo );
    } catch ( ... ) {
        try {
            if ( MarkCaughtExceptionOkayToDestructIfThreadQuitRequestedError( ) ) {
                ScopedLock< ::pthread_mutex_t > lock( gSerializedOutputGuard );

                cout << "thread quit by request" << endl;
            } else {
                ScopedLock< ::pthread_mutex_t > lock( gSerializedOutputGuard );

                cerr << "exception caught trying escape main" << endl;

                try {
                    throw;
                } catch ( const ::std::exception & e ) {
                    cerr << "    " << e.what( ) << endl;
                    // stifle our little throw
                } catch ( ... ) {
                    // stifle our little throw
                }
            }
        } catch ( ... ) {
            // stifle any exception
        }
    }

    return 0;
}


void
TestIt( ) {
    lockMutex( gOneTimeBarrier );

    WorkInfo workInfoP1;

    workInfoP1.prefix = "P1";
    workInfoP1.periodFrames = 1;
    workInfoP1.firingsToDeferQuits = 0;
    workInfoP1.totalFirings = 0;

    pthread_t threadP1;

    ::pthread_create( &threadP1, 0, QuittableEntryPoint, &workInfoP1 );


    WorkInfo workInfoP11;

    workInfoP11.prefix = "              P11";
    workInfoP11.periodFrames = 11;
    workInfoP11.firingsToDeferQuits = 6;
    workInfoP11.totalFirings = 0;

    pthread_t threadP11;

    ::pthread_create( &threadP11, 0, QuittableEntryPoint, &workInfoP11 );


    WorkInfo workInfoNQP7;

    workInfoNQP7.prefix = "                            NQP7";
    workInfoNQP7.periodFrames = 7;
    workInfoNQP7.firingsToDeferQuits = 6;
    workInfoNQP7.totalFirings = 11;

    pthread_t threadNQP7;

    ::pthread_create( &threadNQP7, 0, NonquittableEntryPoint, &workInfoNQP7 );


    WorkInfo workInfoP269;

    workInfoP269.prefix = "                                          P269";
    workInfoP269.periodFrames = 269;
    workInfoP269.firingsToDeferQuits = 0;
    workInfoP269.totalFirings = 0;

    pthread_t threadP269;

    ::pthread_create( &threadP269, 0, QuittableEntryPoint, &workInfoP269 );

    // Hold up a second to let them get ready
    ::sleep( 1 );

    // let them all loose
    unlockMutex( gOneTimeBarrier );

    // Let them run for a little while
    ::sleep( 6 );

    {
        RequestThreadQuit( threadP1 );
        {
            ScopedLock< ::pthread_mutex_t > lock( gSerializedOutputGuard );

            cout << "P1 quit requested" << endl;
        }

        RequestThreadQuit( threadP11 );
        {
            ScopedLock< ::pthread_mutex_t > lock( gSerializedOutputGuard );

            cout << "P11 quit requested" << endl;
        }

        // RequestThreadQuit( threadNQP7 );
        // {
        //     ScopedLock< ::pthread_mutex_t > lock( gSerializedOutputGuard );
        //
        //     cout << "NQP7 quit requested" << endl;
        // }

        RequestThreadQuit( threadP269 );
        {
            ScopedLock< ::pthread_mutex_t > lock( gSerializedOutputGuard );

            cout << "P269 quit requested" << endl;
        }
    }


    {
        void * threadResult = 0;

        ::pthread_join( threadP1, &threadResult );
        {
            ScopedLock< ::pthread_mutex_t > lock( gSerializedOutputGuard );

            cout << "P1 joined" << endl;
        }

        ::pthread_join( threadP11, &threadResult );
        {
            ScopedLock< ::pthread_mutex_t > lock( gSerializedOutputGuard );

            cout << "P11 joined" << endl;
        }

        ::pthread_join( threadNQP7, &threadResult );
        {
            ScopedLock< ::pthread_mutex_t > lock( gSerializedOutputGuard );

            cout << "NQP7 joined" << endl;
        }

        ::pthread_join( threadP269, &threadResult );
        {
            ScopedLock< ::pthread_mutex_t > lock( gSerializedOutputGuard );

            cout << "P269 joined" << endl;
        }
    }
}


} // anonymous namespace


//
// @noKeys
//
// @logger TEST_FACILITY carma.test.util.tTimerThreadQuit
//

int
Program::main( ) {
    int result = -1;

    try {
        TestIt( );

        result = 0;
    } catch ( const ::std::exception & e ) {
        cerr << "exception caught trying escape main. what:" << endl;
        cerr << e.what( ) << endl;

        result = -2;
    } catch ( ... ) {
        cerr << "unknown class of exception caught trying escape main" << endl;

        result = -2;
    }

    return result;
}
