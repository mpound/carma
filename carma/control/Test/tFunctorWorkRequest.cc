#include "carma/util/Program.h"

#include <string>
#include <map>
#include <cmath>
#include <stdexcept>

#include <unistd.h>

#include <log4cpp/NDC.hh>

#include "carma/control/FunctorWorkRequest.h"
#include "carma/control/MethodFunctors.h"
#include "carma/control/WorkerPool.h"
#include "carma/util/Trace.h"
#include "carma/util/BaseException.h"
#include "carma/util/programLogging.h"
#include "carma/util/WorkResult.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;
using namespace carma::control;


namespace {


template < typename T >
MethodFunctor0< T >
makeMethodFunctor( T * t,
                   void (T::*methodPtr)( ) ) {
    return MethodFunctor0< T >( t,
                                methodPtr );
}


template < typename T, typename S0 >
MethodFunctor1< T, S0 >
makeMethodFunctor( T * const  t,
                   void  (T::*methodPtr)( S0 ),
                   const S0 & s0 ) {
    return MethodFunctor1< T, S0 >( t,
                                    methodPtr,
                                    s0 );
}


template < typename T, typename S0, typename S1 >
MethodFunctor2< T, S0, S1 >
makeMethodFunctor( T * const  t,
                   void  (T::*methodPtr)( S0, S1 ),
                   const S0 & s0,
                   const S1 & s1 ) {
    return MethodFunctor2< T, S0, S1 >( t,
                                        methodPtr,
                                        s0,
                                        s1 );
}


template < typename T,
           typename S0,
           typename S1,
           typename S2 >
MethodFunctor3< T, S0, S1, S2 >
makeMethodFunctor( T * const  t,
                   void  (T::*methodPtr)( S0, S1, S2 ),
                   const S0 & s0,
                   const S1 & s1,
                   const S2 & s2 ) {
    return MethodFunctor3< T, S0, S1, S2 >( t,
                                            methodPtr,
                                            s0,
                                            s1,
                                            s2 );
}


template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3 >
MethodFunctor4< T, S0, S1, S2, S3 >
makeMethodFunctor( T * const  t,
                   void  (T::*methodPtr)( S0, S1, S2, S3 ),
                   const S0 & s0,
                   const S1 & s1,
                   const S2 & s2,
                   const S3 & s3 ) {
    return MethodFunctor4< T, S0, S1, S2, S3 >( t,
                                                methodPtr,
                                                s0,
                                                s1,
                                                s2,
                                                s3 );
}


template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4  >
MethodFunctor5< T, S0, S1, S2, S3, S4 >
makeMethodFunctor( T * const  t,
                   void  (T::*methodPtr)( S0, S1, S2, S3, S4 ),
                   const S0 & s0,
                   const S1 & s1,
                   const S2 & s2,
                   const S3 & s3,
                   const S4 & s4 ) {
    return MethodFunctor5< T, S0, S1, S2, S3, S4 >( t,
                                                    methodPtr,
                                                    s0,
                                                    s1,
                                                    s2,
                                                    s3,
                                                    s4 );
}


template < typename F >
void
queueFunctorWorkRequest( const string &  requestKey,
                         const F         functor,
                         WorkResultSet & resultSet,
                         WorkerPool &    workerPool ) {
    map< string, F > resultKeyFunctorMap;
    
    resultKeyFunctorMap.insert( make_pair( requestKey, functor ) );
    
    queueFunctorWorkRequestGroup( "",
                                  resultKeyFunctorMap,
                                  resultSet,
                                  workerPool );
}


template < typename K, typename T >
map< K, MethodFunctor0< T > >
makeMethodFunctorGroup(
    const map< K, T * > & tGroup,
    void             (T::*methodPtr)( ) ) {
    typedef map< K, T * > TGroup;
    typedef map< K, MethodFunctor0< T > > FunctorGroup;
            
    FunctorGroup result;
    
    typename TGroup::const_iterator i = tGroup.begin( );    
    const typename TGroup::const_iterator iEnd = tGroup.end( );

    while ( i != iEnd ) {
        T * const t = i->second;
        
        if ( t != 0 ) {
            result.insert(
                make_pair(
                    i->first, 
                    MethodFunctor0< T >( t,
                                         methodPtr ) ) );
        }
        
        ++i;
    }
    
    return result;
}


template < typename K, typename T, typename A0 >
map< K, MethodFunctor1< T, A0 > >
makeMethodFunctorGroup(
    const map< K, T * > & tGroup,
    void             (T::*methodPtr)( A0 ),
    A0                    arg0 ) {
    typedef map< K, T * > TGroup;
    typedef map< K, MethodFunctor1< T, A0 > > FunctorGroup;
            
    FunctorGroup result;
    
    typename TGroup::const_iterator i = tGroup.begin( );
    const typename TGroup::const_iterator iEnd = tGroup.end( );

    while ( i != iEnd ) {
        T * const t = i->second;
        
        if ( t != 0 ) {
            result.insert(
                make_pair(
                    i->first, 
                    MethodFunctor1< T, A0 >( t,
                                             methodPtr,
                                             arg0 ) ) );
        }
        
        ++i;
    }
    
    return result;
}


template < typename K, typename T, typename A0, typename A1 >
map< K, MethodFunctor2< T, A0, A1 > >
makeMethodFunctorGroup(
    const map< K, T * > & tGroup,
    void             (T::*methodPtr)( A0, A1 ),
    A0                    arg0,
    A1                    arg1 ) {
    typedef map< K, T * > TGroup;
    typedef map< K, MethodFunctor2< T, A0, A1 > > FunctorGroup;
            
    FunctorGroup result;
    
    typename TGroup::const_iterator i = tGroup.begin( );
    const typename TGroup::const_iterator iEnd = tGroup.end( );

    while ( i != iEnd ) {
        T * const t = i->second;
        
        if ( t != 0 ) {
            result.insert(
                make_pair(
                    i->first, 
                    MethodFunctor2< T, A0, A1 >( t,
                                                 methodPtr,
                                                 arg0,
                                                 arg1 ) ) );
        }
        
        ++i;
    }
    
    return result;
}


template < typename K, typename T, typename A0, typename A1, typename A2>
map< K, MethodFunctor3< T, A0, A1, A2 > >
makeMethodFunctorGroup(
    const map< K, T * > & tGroup,
    void             (T::*methodPtr)( A0, A1, A2 ),
    A0                    arg0,
    A1                    arg1,
    A2                    arg2 ) {
    typedef map< K, T * > TGroup;
    typedef map< K, MethodFunctor3< T, A0, A1, A2 > > FunctorGroup;
            
    FunctorGroup result;
    
    typename TGroup::const_iterator i = tGroup.begin( );
    const typename TGroup::const_iterator iEnd = tGroup.end( );

    while ( i != iEnd ) {
        T * const t = i->second;
        
        if ( t != 0 ) {
            result.insert(
                make_pair(
                    i->first, 
                    MethodFunctor3< T, A0, A1, A2 >( t,
                                                 methodPtr,
                                                 arg0,
                                                 arg1, arg2 ) ) );
        }
        
        ++i;
    }
    
    return result;
}


void
myFunc1( void ) {
    CARMA_CPTRACE( Trace::TRACE2, "myFunc1() called." );
}


class Obj {
    public:
        explicit Obj( const string & id );

        void methodA( );

        void methodB( double x );

        void methodC( int y, float z );

    private:
        Obj( const Obj & rhs );
        Obj & operator=( const Obj & rhs );

        const string id_;
};


Obj::Obj( const string & id ) :
id_( id ) {
}


void
Obj::methodA( ) {
    CARMA_CPTRACE( Trace::TRACE2, "methodA() invoked on " << id_ );
}


void
Obj::methodB( const double x ) {
    CARMA_CPTRACE( Trace::TRACE2, "methodB(" << x << ") invoked on " << id_ );
}


void
Obj::methodC( int y, float z ) {
    CARMA_CPTRACE( Trace::TRACE2,
                   "methodC(" << y << ", " << z << ") invoked on " << id_ );
}


}  // namespace < anonymous >


//
// @version 0.3
//
// @usage use it
//
// @description
//   fuggetaboutit.
//
// @key threads 23 int
//      Number of threads to use in the worker pool. Values less than 1 will
//      result in 1 thread being used.
//
// @key objects 6 int
//      Number of objects to fork across for each test. Values less than 1 will
//      result in no objects being used.
//
// @key timeout 30.0 double
//      Timeout value in seconds to use for the waits with a timeout. Values
//      less than 0.0 will result in 0.0 being used.
//
// @logger TEST_FACILITY carma.test.control.tFunctorWorkRequest
//

int
Program::main( )
try {
    log4cpp::NDC::push( "main thread" );

    const int numThreadsParam = getIntParameter( "threads" );
    const int numObjectsParam = getIntParameter( "objects" );
    const double timeoutParam = getDoubleParameter( "timeout" );

    const ::size_t numThreads = ((numThreadsParam < 1) ? 1 : numThreadsParam);
    const ::size_t numObjects = ((numObjectsParam < 0) ? 0 : numObjectsParam);
    const double timeout = ((timeoutParam < 0.0) ? 0.0 : timeoutParam);

    const unsigned long timeoutMillis =
        static_cast< unsigned long >( floor( timeout * 1000.0 ) );

    typedef map< string, Obj * > StringToObjMap;

    StringToObjMap stringToObjMap;

    for ( ::size_t i = 0; i < numObjects; ++i ) {
        string s;

        {
            ostringstream oss;

            oss << "Obj #" << (i + 1);

            s = oss.str( );
        }

        if ( stringToObjMap.insert( make_pair( s, new Obj( s ) ) ).second != true )
            throw runtime_error( s + " was already mapped in stringToObjMap" );
    }

    {
        CARMA_CPTRACE( Trace::TRACE1, "Constructing a worker pool with " <<
                                      numThreads << " threads." );

        WorkerPool wp( "worker pool", numThreads, false );

        WorkResultSet wrs( "result set" );

        CARMA_CPTRACE( Trace::TRACE1, "Queueing myFunc1() request" );

        {
            const string requestKey = "myFunc1()";
    
            queueFunctorWorkRequest( requestKey,
                                     myFunc1,
                                     wrs,
                                     wp );
        }

        CARMA_CPTRACE( Trace::TRACE1, "myFunc1 request queued" );
        
        CARMA_CPTRACE( Trace::TRACE1, "Queueing Obj::methodA() requests" );

        {
            StringToObjMap::const_iterator i = stringToObjMap.begin( );
            const StringToObjMap::const_iterator iEnd = stringToObjMap.end( );

            while ( i != iEnd ) {
                const string requestKey = i->first + " Obj::methodA()";

                queueFunctorWorkRequest( requestKey,
                                         makeMethodFunctor( i->second, &Obj::methodA ),
                                         wrs,
                                         wp );

                ++i;
            }
        }


        CARMA_CPTRACE( Trace::TRACE1, "Queueing Obj::methodB(12.3) requests" );

        {
            StringToObjMap::const_iterator i = stringToObjMap.begin( );
            const StringToObjMap::const_iterator iEnd = stringToObjMap.end( );

            while ( i != iEnd ) {
                const string requestKey = i->first + " Obj::methodB(12.3)";
                Obj * const constPointerToObj = i->second;

                queueFunctorWorkRequest( requestKey,
                                         makeMethodFunctor( 
                                            constPointerToObj, 
                                            &Obj::methodB, 
                                            12.3 ),
                                         wrs,
                                         wp );

                ++i;
            }
        }

        CARMA_CPTRACE( Trace::TRACE1, "Obj::methodB(12.3) requests queued" );

        CARMA_CPTRACE( Trace::TRACE1, "Queueing Obj::methodC(42, 12.3f) requests" );

        {
            StringToObjMap::const_iterator i = stringToObjMap.begin( );
            const StringToObjMap::const_iterator iEnd = stringToObjMap.end( );

            while ( i != iEnd ) {
                const string requestKey = i->first + " Obj::methodC(42, 12.3f)";

                queueFunctorWorkRequest( requestKey,
                                         makeMethodFunctor( i->second, &Obj::methodC, 42, 12.3f ),
                                         wrs,
                                         wp );

                ++i;
            }
        }

        CARMA_CPTRACE( Trace::TRACE1, "Obj::methodC(42, 12.3f) requests queued" );

        CARMA_CPTRACE( Trace::TRACE1, "Waiting for all results." );

        wrs.waitForAll( timeoutMillis,
                        true,
                        WorkResultSet::LATE_DROPPED_POST_STATE );

        CARMA_CPTRACE( Trace::TRACE1, "Results ready." );

        CARMA_CPTRACE( Trace::TRACE1,
                       "Work result set and worker pool going out of scope." );
    }

    CARMA_CPTRACE( Trace::TRACE1, "Out of scope." );

    {
        CARMA_CPTRACE( Trace::TRACE1, "Constructing a worker pool with " <<
                                      numThreads << " threads." );

        WorkerPool wp( "worker pool #2", numThreads, false );

        WorkResultSet wrs( "Obj::methodC(19, 2.71f) result set" );

        CARMA_CPTRACE( Trace::TRACE1, "Queueing a single Obj::methodC(19, 2.71f) request group" );

        queueFunctorWorkRequestGroup(
            "Obj::methodC(19, 2.71f)",
            makeMethodFunctorGroup( stringToObjMap, &Obj::methodC, 19, 2.71f ),
            wrs,
            wp );

        CARMA_CPTRACE( Trace::TRACE1, "Obj::methodC(19, 2.71f) request group queued" );

        CARMA_CPTRACE( Trace::TRACE1, "Waiting for all results." );

        wrs.waitForAll( timeoutMillis,
                        true,
                        WorkResultSet::LATE_DROPPED_POST_STATE );

        CARMA_CPTRACE( Trace::TRACE1, "Results ready." );

        CARMA_CPTRACE( Trace::TRACE1,
                       "Work result set and worker pool going out of scope." );
    }

    CARMA_CPTRACE( Trace::TRACE1, "Out of scope." );

    programLogInfoIfPossible( "All tests done" );

    return EXIT_SUCCESS;
} catch ( const BaseException & e ) {
    CARMA_CPTRACE( Trace::TRACE1,
                   "Coming out on a BaseException (" <<
                   e.getLogString( ) << ")" );

    throw;
} catch ( const ::std::exception & e ) {
    CARMA_CPTRACE( Trace::TRACE1,
                   "Coming out on a ::std::exception (" << e.what( ) << ")" );

    throw;
} catch ( ... ) {
    CARMA_CPTRACE( Trace::TRACE1, "Coming out on an unknown exception" );

    throw;
}

