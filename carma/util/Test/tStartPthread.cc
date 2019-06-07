#include <stdexcept>
#include <iostream>
#include <vector>
#include <cstdlib>

#include <pthread.h>
#include <unistd.h>

#include "carma/util/Program.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLockManager.h"
#include "carma/util/StartPthread.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/PthreadAttr.h"


using namespace ::std;


//#define TEST_XFAIL 1


namespace {


void
Bail( const string & message )
{
    try {
        cerr << message << endl;
    } catch ( ... ) {
        // just stifle any exception
    }

    ::exit( 1 );
}


class X {
    public:
        explicit X( ::size_t failStartingAtCopyN = 128 );
        X( const X & x );

        ~X( );

        X & operator=( const X & x );

        ::size_t CtorCopyCount( ) const;
        ::size_t AssignmentCopyCount( ) const;

        void Modify( );

        bool HasBeenModified( ) const;

        bool ParanoidIsAlive( ) const;

        static ::size_t Num( );

    private:
        unsigned long paranoidCheck_;
        ::size_t      ctorCopyCount_;
        ::size_t      assignmentCopyCount_;
        ::size_t      failStartingAtCopyN_;
        bool          modified_;

        static ::pthread_mutex_t numGuard_;
        static ::size_t          num_;
};


::pthread_mutex_t X::numGuard_ = PTHREAD_MUTEX_INITIALIZER;
::size_t X::num_ = 0;

const unsigned long kAliveParanoidValue = 0xFEEDCAFE;
const unsigned long kDeadParanoidValue = 0xDEADBEEF;

X::X( const ::size_t failStartingAtCopyN ) :
paranoidCheck_( kDeadParanoidValue ),
ctorCopyCount_( 0 ),
assignmentCopyCount_( 0 ),
failStartingAtCopyN_( failStartingAtCopyN ),
modified_( false )
{
    try {
        const carma::util::ScopedLock< ::pthread_mutex_t > lock( numGuard_ );

        ++num_;
    } catch ( ... ) {
        Bail( "ERROR: mutex locking error" );
    }

    paranoidCheck_ = kAliveParanoidValue;
}


X::X( const X & x ) :
paranoidCheck_( kDeadParanoidValue ),
ctorCopyCount_( x.ctorCopyCount_ + 1 ),
assignmentCopyCount_( x.assignmentCopyCount_ ),
failStartingAtCopyN_( x.failStartingAtCopyN_ ),
modified_( x.modified_ )
{
    if ( x.ParanoidIsAlive() == false )
        Bail( "source is dead" );

    if ( (ctorCopyCount_ + assignmentCopyCount_) >= failStartingAtCopyN_ )
        throw runtime_error( "Nth or later copy" );

    try {
        const carma::util::ScopedLock< ::pthread_mutex_t > lock( numGuard_ );

        ++num_;
    } catch ( ... ) {
        Bail( "ERROR: mutex locking error" );
    }

    paranoidCheck_ = kAliveParanoidValue;
}


X::~X( )
{
    paranoidCheck_ = kDeadParanoidValue;

    try {
        const carma::util::ScopedLock< ::pthread_mutex_t > lock( numGuard_ );

        if ( num_ < 1 )
            Bail( "ERROR: invalid X's alive counter" );

        --num_;
    } catch ( ... ) {
        Bail( "ERROR: mutex locking error" );
    }
}


X &
X::operator=( const X & x )
{
    if ( ParanoidIsAlive() == false )
        Bail( "not alive" );

    if ( x.ParanoidIsAlive() == false )
        Bail( "source is not alive" );

    if ( (x.ctorCopyCount_ + x.assignmentCopyCount_ + 1) >= x.failStartingAtCopyN_ )
        throw runtime_error( "Nth copy or later" );

    ctorCopyCount_ = x.ctorCopyCount_;
    assignmentCopyCount_ = x.assignmentCopyCount_ + 1;
    failStartingAtCopyN_ = x.failStartingAtCopyN_;
    modified_ = x.modified_;

    return *this;
}


::size_t
X::CtorCopyCount( ) const
{
    if ( ParanoidIsAlive() == false )
        Bail( "not alive" );

    return ctorCopyCount_;
}


::size_t
X::AssignmentCopyCount( ) const
{
    if ( ParanoidIsAlive() == false )
        Bail( "not alive" );

    return assignmentCopyCount_;
}


void
X::Modify( )
{
    if ( ParanoidIsAlive() == false )
        Bail( "not alive" );

    modified_ = true;
}


bool
X::HasBeenModified( ) const
{
    if ( ParanoidIsAlive() == false )
        Bail( "not alive" );

    return modified_;
}


bool
X::ParanoidIsAlive( ) const
{
    return (paranoidCheck_ == kAliveParanoidValue);
}


::size_t
X::Num( )
{
    ::size_t result = 0;

    try {
        const carma::util::ScopedLock< ::pthread_mutex_t > lock( numGuard_ );

        result = num_;
    } catch ( ... ) {
        Bail( "ERROR: mutex locking error" );
    }

    return result;
}


void
VerifySingleCtorCopy( const X & x )
{
    if ( (x.CtorCopyCount() != 1) || (x.AssignmentCopyCount() != 0) )
        Bail( "ERROR: not a single c'tor copy" );
}


void
VerifyNotACopy( const X & x )
{
    if ( (x.CtorCopyCount() != 0) || (x.AssignmentCopyCount() != 0) )
        Bail( "ERROR: is a copy" );
}


void
VerifyNotModified( const X & x )
{
    if ( x.HasBeenModified() )
        Bail( "ERROR: has been modified" );
}


void
VerifyModified( const X & x )
{
    if ( x.HasBeenModified() == false )
        Bail( "ERROR: has not been modified" );
}


void
VerifyParanoidIsAlive( const X & x )
{
    if ( x.ParanoidIsAlive() == false )
        Bail( "ERROR: paranoid live check has failed" );
}


void
VerifyNumberOfXs( const ::size_t num )
{
    if ( X::Num() != num )
        Bail( "ERROR: wrong number of X's alive" );
}


void
VerifyPointerNotChanged( const X * const p,
                         const X * const originalP )
{
    if ( p != originalP )
        Bail( "ERROR: pointer has changed" );
}


void
VerifyPointerChanged( const X * const p,
                      const X * const originalP )
{
    if ( p == originalP )
        Bail( "ERROR: pointer has not changed" );
}


::size_t gEntryPointsEntered = 0;


void
EntryPointX( X & x )
{
    ++gEntryPointsEntered;

    VerifyParanoidIsAlive( x );

    VerifyNumberOfXs( 2 );
    VerifySingleCtorCopy( x );

    VerifyNotModified( x );
    x.Modify();
    VerifyModified( x );

    throw runtime_error( "EntryPointX deliberate exception" );
}


void
EntryPointCX( const X & cx )
{
    ++gEntryPointsEntered;

    VerifyParanoidIsAlive( cx );

    VerifyNumberOfXs( 2 );
    VerifySingleCtorCopy( cx );

    VerifyNotModified( cx );

    throw runtime_error( "EntryPointCX deliberate exception" );
}


void
EntryPointRX( X & x )
{
    ++gEntryPointsEntered;

    VerifyParanoidIsAlive( x );

    VerifyNumberOfXs( 1 );
    VerifyNotACopy( x );

    VerifyNotModified( x );
    x.Modify();
    VerifyModified( x );

    throw runtime_error( "EntryPointRX deliberate exception" );
}


void
EntryPointRCX( const X & cx )
{
    ++gEntryPointsEntered;

    VerifyParanoidIsAlive( cx );

    VerifyNumberOfXs( 1 );
    VerifyNotACopy( cx );

    VerifyNotModified( cx );

    throw runtime_error( "EntryPointRCX deliberate exception" );
}


void
EntryPointPX( X * & px )
{
    ++gEntryPointsEntered;

    VerifyParanoidIsAlive( *px );

    VerifyNumberOfXs( 1 );
    VerifyNotACopy( *px );

    VerifyNotModified( *px );
    px->Modify();
    VerifyModified( *px );

    px = 0;

    throw runtime_error( "EntryPointPX deliberate exception" );
}


void
EntryPointPCX( const X * & pcx )
{
    ++gEntryPointsEntered;

    VerifyParanoidIsAlive( *pcx );

    VerifyNumberOfXs( 1 );
    VerifyNotACopy( *pcx );

    VerifyNotModified( *pcx );

    pcx = 0;

    throw runtime_error( "EntryPointPCX deliberate exception" );
}


::size_t gPthreadsStarted = 0;


void
TestPassByCopyOfValue( )
{
    void * threadResult = 0;

    // copy of non-const variable -> non-const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            X x;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( x );

            ::pthread_join( carma::util::StartPthreadWithCopy( EntryPointX, x ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( x );
        }

        VerifyNumberOfXs( 0 );
    }

    // copy of const variable -> non-const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            const X cx;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );

            ::pthread_join( carma::util::StartPthreadWithCopy( EntryPointX, cx ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );
        }

        VerifyNumberOfXs( 0 );
    }

    // copy of anonymous temp -> non-const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        ::pthread_join( carma::util::StartPthreadWithCopy( EntryPointX, X() ), &threadResult );

        ++gPthreadsStarted;

        VerifyNumberOfXs( 0 );
    }

    // copy of non-const variable -> const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            X x;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( x );

            ::pthread_join( carma::util::StartPthreadWithCopy( EntryPointCX, x ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( x );
        }

        VerifyNumberOfXs( 0 );
    }

    // copy of const variable -> const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            const X cx;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );

            ::pthread_join( carma::util::StartPthreadWithCopy( EntryPointCX, cx ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );
        }

        VerifyNumberOfXs( 0 );
    }

    // copy of anonymous temp -> const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        ::pthread_join( carma::util::StartPthreadWithCopy( EntryPointCX, X() ), &threadResult );

        ++gPthreadsStarted;

        VerifyNumberOfXs( 0 );
    }
}


void
TestPassByRefToValue( )
{
    void * threadResult = 0;

    // ref to non-const variable -> non-const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            X x;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( x );

            ::pthread_join( carma::util::StartPthreadWithRef( EntryPointRX, x ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyModified( x );
        }

        VerifyNumberOfXs( 0 );
    }

#if defined( TEST_XFAIL ) && (TEST_XFAIL)
    // XFAIL - trying to hand off a const ref as non-const ref parameter
    // ref to const variable -> non-const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            const X cx;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );

            ::pthread_join( carma::util::StartPthreadWithRef( EntryPointRX, cx ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );
        }

        VerifyNumberOfXs( 0 );
    }
#endif

#if defined( TEST_XFAIL ) && (TEST_XFAIL)
    // XFAIL - trying to hand off a ref to a temp as a non-const ref parameter
    // ref to anonymous temp -> non-const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        ::pthread_join( carma::util::StartPthreadWithRef( EntryPointRX, X() ), &threadResult );

        ++gPthreadsStarted;

        VerifyNumberOfXs( 0 );
    }
#endif

    // ref to non-const variable -> const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            X x;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( x );

            ::pthread_join( carma::util::StartPthreadWithRef( EntryPointRCX, x ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( x );
        }

        VerifyNumberOfXs( 0 );
    }

    // ref to const variable -> const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            const X cx;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );

            ::pthread_join( carma::util::StartPthreadWithRef<X>( EntryPointRCX, cx ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );
        }

        VerifyNumberOfXs( 0 );
    }

    // ref to anonymous temp -> const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        ::pthread_join( carma::util::StartPthreadWithRef( EntryPointRCX, X() ), &threadResult );

        ++gPthreadsStarted;

        VerifyNumberOfXs( 0 );
    }
}


void
TestPassByCopyOfPointerToValue( )
{
    void * threadResult = 0;

    // copy of anonymous temp pointer to non-const variable -> non-const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            X x;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( x );

            ::pthread_join( carma::util::StartPthreadWithCopy( EntryPointPX, &x ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyModified( x );
        }

        VerifyNumberOfXs( 0 );
    }

    // copy of pointer to non-const variable -> non-const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            X x;
            X * px = &x;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( x );

            ::pthread_join( carma::util::StartPthreadWithCopy( EntryPointPX, px ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyModified( x );

            VerifyPointerNotChanged( px, &x );
        }

        VerifyNumberOfXs( 0 );
    }

#if defined( TEST_XFAIL ) && (TEST_XFAIL)
    // XFAIL - trying to hand off a (const X *) as a (X *) parameter
    // copy of anonymous temp pointer to const variable -> non-const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            const X cx;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );

            ::pthread_join( carma::util::StartPthreadWithCopy( EntryPointPX, &cx ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );
        }

        VerifyNumberOfXs( 0 );
    }
#endif

#if defined( TEST_XFAIL ) && (TEST_XFAIL)
    // XFAIL - trying to hand off a (const X *) as a (X *) parameter
    // copy of pointer to const variable -> non-const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            const X cx;
            const X * pcx = &cx;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );

            ::pthread_join( carma::util::StartPthreadWithCopy( EntryPointPX, pcx ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );

            VerifyPointerNotChanged( pcx, &cx );
        }

        VerifyNumberOfXs( 0 );
    }
#endif

#if 0
    // copy of anonymous temp pointer to anonymous temp -> non-const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        // XWARN
        // The warning from the compiler about the line below is intentional
        // I am deliberately trying to test a corner case.
        ::pthread_join( carma::util::StartPthreadWithCopy( EntryPointPX, &X() ), &threadResult );

        ++gPthreadsStarted;

        VerifyNumberOfXs( 0 );
    }
#endif

    // copy of anonymous temp pointer to non-const variable -> const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            X x;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( x );

            ::pthread_join( carma::util::StartPthreadWithCopy( EntryPointPCX, &x ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( x );
        }

        VerifyNumberOfXs( 0 );
    }

    // copy of anonymous temp pointer to const variable -> const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            const X cx;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );

            ::pthread_join( carma::util::StartPthreadWithCopy( EntryPointPCX, &cx ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );
        }

        VerifyNumberOfXs( 0 );
    }

    // copy of pointer to const variable -> const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            const X cx;
            const X * pcx = &cx;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );

            ::pthread_join( carma::util::StartPthreadWithCopy( EntryPointPCX, pcx ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );

            VerifyPointerNotChanged( pcx, &cx );
        }

        VerifyNumberOfXs( 0 );
    }

#if 0
    // copy of anonymous temp pointer to anonymous temp -> const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        // XWARN
        // The warning from the compiler about the line below is intentional
        // I am deliberately trying to test a corner case.
        ::pthread_join( carma::util::StartPthreadWithCopy( EntryPointPCX, &X() ), &threadResult );

        ++gPthreadsStarted;

        VerifyNumberOfXs( 0 );
    }
#endif
}


void
TestPassByRefToPointerToValue( )
{
    void * threadResult = 0;

#if defined( TEST_XFAIL ) && (TEST_XFAIL)
    // XFAIL - non-const refs to anonymous temps are not allowed
    // ref to anonymous temp pointer to non-const variable -> non-const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            X x;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( x );

            ::pthread_join( carma::util::StartPthreadWithRef( EntryPointPX, &x ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyModified( x );
        }

        VerifyNumberOfXs( 0 );
    }
#endif

    // ref to pointer to non-const variable -> non-const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            X x;
            X * px = &x;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( x );

            ::pthread_join( carma::util::StartPthreadWithRef( EntryPointPX, px ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyModified( x );

            VerifyPointerChanged( px, &x );
        }

        VerifyNumberOfXs( 0 );
    }

#if defined( TEST_XFAIL ) && (TEST_XFAIL)
    // XFAIL - trying to hand off a (const X *) as a (X *) parameter
    // ref to anonymous temp pointer to const variable -> non-const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            const X cx;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );

            ::pthread_join( carma::util::StartPthreadWithRef( EntryPointPX, &cx ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );
        }

        VerifyNumberOfXs( 0 );
    }
#endif

#if defined( TEST_XFAIL ) && (TEST_XFAIL)
    // XFAIL - trying to hand off a (const X *) as a (X *) parameter
    // ref to pointer to const variable -> non-const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            const X cx;
            const X * pcx = &cx;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );

            ::pthread_join( carma::util::StartPthreadWithRef( EntryPointPX, pcx ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );

            VerifyPointerChanged( pcx, &cx );
        }

        VerifyNumberOfXs( 0 );
    }
#endif

#if defined( TEST_XFAIL ) && (TEST_XFAIL)
    // XFAIL - non-const refs to anonymous temps are not allowed
    // ref to anonymous temp pointer to anonymous temp -> non-const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        // XWARN
        // The warning from the compiler about the line below is intentional
        // I am deliberately trying to test a corner case.
        ::pthread_join( carma::util::StartPthreadWithRef( EntryPointPX, &X() ), &threadResult );

        ++gPthreadsStarted;

        VerifyNumberOfXs( 0 );
    }
#endif

#if defined( TEST_XFAIL ) && (TEST_XFAIL)
    // XFAIL - non-const refs to anonymous temps are not allowed
    // ref to anonymous temp pointer to non-const variable -> const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            X x;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( x );

            ::pthread_join( carma::util::StartPthreadWithRef( EntryPointPCX, &x ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( x );
        }

        VerifyNumberOfXs( 0 );
    }
#endif

#if defined( TEST_XFAIL ) && (TEST_XFAIL)
    // XFAIL - non-const refs to anonymous temps are not allowed
    // ref to anonymous temp pointer to const variable -> const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            const X cx;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );

            ::pthread_join( carma::util::StartPthreadWithRef( EntryPointPCX, &cx ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );
        }

        VerifyNumberOfXs( 0 );
    }
#endif

    // ref to pointer to const variable -> const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        {
            const X cx;
            const X * pcx = &cx;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );

            ::pthread_join( carma::util::StartPthreadWithRef( EntryPointPCX, pcx ), &threadResult );

            ++gPthreadsStarted;

            VerifyNumberOfXs( 1 );

            VerifyNotModified( cx );

            VerifyPointerChanged( pcx, &cx );
        }

        VerifyNumberOfXs( 0 );
    }

#if defined( TEST_XFAIL ) && (TEST_XFAIL)
    // XFAIL - non-const refs to anonymous temps are not allowed
    // ref to anonymous temp pointer to anonymous temp -> const parameter entry point
    {
        VerifyNumberOfXs( 0 );

        // XWARN
        // The warning from the compiler about the line below is intentional
        // I am deliberately trying to test a corner case.
        ::pthread_join( carma::util::StartPthreadWithRef( EntryPointPCX, &X() ), &threadResult );

        ++gPthreadsStarted;

        VerifyNumberOfXs( 0 );
    }
#endif
}


void
TestPassByCopyOfValueExceptions( )
{
    void * threadResult = 0;

    VerifyNumberOfXs( 0 );

    {
        const X cx( 1 );

        VerifyNotModified( cx );

        try {
            carma::util::StartPthreadWithCopy( EntryPointX, cx );

            ++gPthreadsStarted;

            Bail( "ERROR: expected exception was NOT thrown" );
        } catch ( ... ) {
            // stifle the expected exception
        }
    }

    VerifyNumberOfXs( 0 );

    {
        const X cx( 2 );

        VerifyNotModified( cx );

        ::pthread_join( carma::util::StartPthreadWithCopy( EntryPointX, cx ), &threadResult );

        ++gPthreadsStarted;
    }

    VerifyNumberOfXs( 0 );
}


struct MyInfo {
    int threadId;
    int periodInFrames;
    int firings;
};


::pthread_mutex_t gOutputSerializer = PTHREAD_MUTEX_INITIALIZER;


void
MyEP( const MyInfo & info )
{
    ++gEntryPointsEntered;

    {
        const carma::util::ScopedLock< ::pthread_mutex_t >
            lock( gOutputSerializer );

        cout << "(#" << info.threadId
             << " : P" << info.periodInFrames
             << " : F" << info.firings
             << "): thread started"
             << endl;
    }

    try {
        if ( info.firings > 0 ) {
            carma::util::FrameAlignedTimer timer( 0, info.periodInFrames );

            timer.ResetNextFireTime();

            for ( int i = 1; i <= info.firings; ++i ) {
                timer.WaitForNextFireTime();

                {
                    const carma::util::ScopedLock< ::pthread_mutex_t >
                        lock( gOutputSerializer );

                    cout << "(#" << info.threadId
                         << " : P" << info.periodInFrames
                         << " : F" << info.firings
                         << "): "
                         << i << "/" << info.firings
                         << " t=" << (info.periodInFrames * (i - 1))
                         << endl;
                }
            }
        }
    } catch ( ... ) {
        try {
            const carma::util::ScopedLock< ::pthread_mutex_t >
                lock( gOutputSerializer );

            cout << "(#" << info.threadId
                 << " : P" << info.periodInFrames
                 << " : F" << info.firings
                 << "): thread ";

            if ( carma::util::CaughtExceptionIsThreadQuitRequestedError() )
                cout << "quitting as requested";
            else
                cout << "ending via exception";

            cout << endl;
        } catch ( ... ) {
            // just stifle any exception
        }

        throw;
    }

    {
        const carma::util::ScopedLock< ::pthread_mutex_t >
            lock( gOutputSerializer );

        cout << "(#" << info.threadId
             << " : P" << info.periodInFrames
             << " : F" << info.firings
             << "): thread ending normally"
             << endl;
    }
}


void
TestMultipleWithQuitting( const int num )
{
    vector< ::pthread_t > threads;

    threads.reserve( num );

    MyInfo info;

    for ( int id = 1; id <= num; ++id ) {
        info.threadId = id;
        info.periodInFrames = num - id;
        info.firings = id - 1;

        threads.push_back( carma::util::StartPthreadWithCopy( MyEP, info ) );

        ++gPthreadsStarted;
    }

    ::sleep( 10 );

    for ( int i = 0; i < num; ++i ) {
        try {
            carma::util::RequestThreadQuit( threads[ i ] );
        } catch ( ... ) {
            // just stifle any exception
        }
    }

    void * threadResult = 0;

    for ( int i = 0; i < num; ++i )
        ::pthread_join( threads[ i ], &threadResult );
}


void
MyDetachedEP( carma::util::PthreadMutex & m )
{
    using namespace carma::util;

    ++gEntryPointsEntered;

    cout << "MyDetachedEP:  Waiting for the lock" << endl;

    const ScopedLock< PthreadMutex > lock( m );

    cout << "MyDetachedEP:  Acquired the lock" << endl;

    ::sleep( 5 );

    cout << "MyDetachedEP:  Releasing the lock" << endl;
}


void
TestDetached( const bool useRawPthreadAttr )
{
    const string prefix =
        "TestDetached(" +
        string( useRawPthreadAttr ? "true" : "false" ) +
        "):  ";

    bool error = false;

    using namespace carma::util;

    PthreadMutex m;

    ScopedLockManager< PthreadMutex > lm( m );

    lm.lock();

    cout << prefix << "Creating the detached thread" << endl;

    ::pthread_t myPthread;
    {
        const PthreadAttr attr( PTHREAD_CREATE_DETACHED );

        if ( useRawPthreadAttr ) {
            myPthread =
                StartPthreadWithRef(
                    MyDetachedEP,
                    m,
                    "Detached pthread",
                    &(attr.InternalPthreadAttr()) );
        } else {
            myPthread =
                StartPthreadWithRef(
                    MyDetachedEP,
                    m,
                    "Detached pthread",
                    attr );
        }

        ++gPthreadsStarted;
    }

    {
        void * threadResult = 0;
        const int joinResult = ::pthread_join( myPthread, &threadResult );

        cout << prefix << "Attempt to join to detached thread returned "
             << joinResult;

        if ( joinResult == EINVAL )
            cout << " < EINVAL >";

        if ( joinResult == ESRCH )
            cout << " < ESRCH >";

        if ( joinResult == EDEADLK )
            cout << " < EDEADLK >";

        cout << endl;

        if ( joinResult != EINVAL )
            error = true;
    }

    cout << prefix << "Releasing the lock" << endl;

    lm.unlock();

    cout << prefix << "Released the lock" << endl;

    ::sleep( 1 );

    cout << prefix << "Reacquiring the lock" << endl;

    lm.lock();

    cout << prefix << "Reacquired the lock" << endl;

    lm.unlock();

    if ( error )
        throw runtime_error( prefix + "pthread_join error" );
}


void
TestIt( )
{
    const carma::util::ScopedThreadQuitRegisterSelf regQuittable;

    TestPassByCopyOfValue();

    TestPassByRefToValue();

    TestPassByCopyOfPointerToValue();

    TestPassByRefToPointerToValue();

    TestPassByCopyOfValueExceptions();

    TestMultipleWithQuitting( 20 );

    TestDetached( true );
    TestDetached( false );

    cout << gPthreadsStarted << " pthreads started successfully." << endl;
    cout << gEntryPointsEntered << " entry points entered." << endl;
}


}  // anonymous namespace


//
// @noKeys
//
// @logger TEST_FACILITY carma.test.util.tStartPthread
//

int
carma::util::Program::main( )
{
    try {
        const carma::util::ScopedThreadQuitRegisterSelf regQuittable;

        TestIt();
    } catch ( const exception & e ) {
        Bail( e.what() );
    } catch ( ... ) {
        Bail( "ERROR: unknown exception caught in main" );
    }

    return 0;
}
