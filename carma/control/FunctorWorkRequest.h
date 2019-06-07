#ifndef CARMA_CONTROL_FUNCTOR_WORK_REQUEST_H
#define CARMA_CONTROL_FUNCTOR_WORK_REQUEST_H


#include <string>
#include <map>
#include <stdexcept>
#include <set>

#include "carma/control/WorkerPool.h"
#include "carma/util/WorkRequest.h"


namespace carma {
namespace control {

template < typename F >
void
queueFunctorWorkRequestGroup(
    const ::std::string &                  requestIdCallString,
    const ::std::map< ::std::string, F > & keyToFunctorMap,
    util::WorkResultSet &                  wrs,
    WorkerPool &                           workerPool );


template < typename F >
void
queueFunctorWorkRequestGroup(
    const ::std::string &                  requestIdCallString,
    const ::std::map< ::std::string, F > & keyToFunctorMap,
    util::WorkResultSet &                  wrs,
    WorkerPool &                           workerPool,
    const bool                             loggingOverride );


template < typename F >
class FunctorWorkRequestImpl : public util::WorkRequest::Impl {
    public:
        static util::WorkRequest make( const ::std::string &    requestId,
                                       const util::WorkResult & wr,
                                       const F &                functor );
        
    private:
        FunctorWorkRequestImpl( const ::std::string &    requestId,
                                const util::WorkResult & wr,
                                const F &                functor );
        
        virtual void serviceImpl( );
        
        F functor_;
};


template < typename F >
FunctorWorkRequestImpl< F >::FunctorWorkRequestImpl(
    const ::std::string &    requestId,
    const util::WorkResult & wr,
    const F &                functor ) :
util::WorkRequest::Impl( requestId, wr ),
functor_( functor )
{
}


template < typename F >
util::WorkRequest
FunctorWorkRequestImpl< F >::make( const ::std::string &    requestId,
                                   const util::WorkResult & wr,
                                   const F &                functor )
{
    return util::WorkRequest( new FunctorWorkRequestImpl( requestId,
                                                          wr,
                                                          functor ) );
}


template < typename F >
void
FunctorWorkRequestImpl< F >::serviceImpl( )
{
    functor_( );
}


template < typename F >
void
queueFunctorWorkRequestGroupImpl(
    const ::std::string &                  requestIdCallString,
    const ::std::map< ::std::string, F > & keyToFunctorMap,
    util::WorkResultSet &                  wrs,
    WorkerPool &                           workerPool,
    const bool * const                     pLoggingOverride )
{
    ::std::set< ::std::string > keysToRemove;
    
    try {
        typedef typename ::std::map< ::std::string, F >::const_iterator
            KeyToFunctorMapIter;
            
        typedef ::std::map< ::std::string, util::WorkResult > KeyToWrMap;
        
        const KeyToFunctorMapIter keyToFunctorMapEnd = keyToFunctorMap.end( );
    
        KeyToWrMap keyToWrMap;
        {
            ::std::set< ::std::string > keys;
            
            KeyToFunctorMapIter i = keyToFunctorMap.begin( );
            
            for ( ; i != keyToFunctorMapEnd; ++i )
                keys.insert( i->first );
            
            // add the keys to the wrs to get work results
            keyToWrMap = wrs.addKeys( keys );
            
            keysToRemove.swap( keys );
        }
        
        ::std::set< util::WorkRequest > requestGroup;
        {
            KeyToWrMap::const_iterator i = keyToWrMap.begin( );
            const KeyToWrMap::const_iterator iEnd = keyToWrMap.end( );
        
            while ( i != iEnd ) {
                const ::std::string key = i->first;
                const util::WorkResult wr = i->second;
                
                const KeyToFunctorMapIter j = keyToFunctorMap.find( key );
                
                if ( j == keyToFunctorMapEnd )
                    throw ::std::logic_error( "Two maps don't agree" );
                    
                ::std::string requestId = key;
                
                if ( requestIdCallString.empty( ) == false ) {
                    if ( requestIdCallString[ 0 ] != ' ' )
                        requestId.append( 1, ' ' );
                        
                    requestId.append( requestIdCallString );
                }
        
                requestId.append( " request" );
    
                const util::WorkRequest request =
                    FunctorWorkRequestImpl< F >::make( requestId,
                                                       wr,
                                                       j->second );
                
                if ( requestGroup.insert( request ).second == false )
                    throw ::std::logic_error( "request is not unique" );
                
                ++i;
            }
        }
    
        ::std::set< ::std::string > temp;
        
        if ( pLoggingOverride != 0 )
            workerPool.queueRequestGroup( requestGroup, *pLoggingOverride );
        else
            workerPool.queueRequestGroup( requestGroup );
        
        keysToRemove.swap( temp ); // This will not throw
    } catch ( ... ) {
        wrs.removeKeys( keysToRemove );
        
        throw;
    }
}


template < typename F >
void
queueFunctorWorkRequestGroup(
    const ::std::string &                  requestIdCallString,
    const ::std::map< ::std::string, F > & keyToFunctorMap,
    util::WorkResultSet &                  wrs,
    WorkerPool &                           workerPool )
{
    queueFunctorWorkRequestGroupImpl( requestIdCallString,
                                      keyToFunctorMap,
                                      wrs,
                                      workerPool,
                                      0 );
}


template < typename F >
void
queueFunctorWorkRequestGroup(
    const ::std::string &                  requestIdCallString,
    const ::std::map< ::std::string, F > & keyToFunctorMap,
    util::WorkResultSet &                  wrs,
    WorkerPool &                           workerPool,
    const bool                             loggingOverride )
{
    queueFunctorWorkRequestGroupImpl( requestIdCallString,
                                      keyToFunctorMap,
                                      wrs,
                                      workerPool,
                                      &loggingOverride );
}


}  // namespace carma::control
}  // namespace carma


#endif
