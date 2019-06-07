#include "carma/util/AutoPthreadQuitAndJoinGroup.h"

#include <iostream>
#include <sstream>
#include <cstring>

#include "carma/util/ThreadQuit.h"
#include "carma/util/programLogging.h"
#include "carma/util/ErrorException.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


AutoPthreadQuitAndJoinGroup::~AutoPthreadQuitAndJoinGroup( )
try {
    try {
        requestQuitsNoThrow();
    } catch ( ... ) {
        // just stifle the exception
    }
    
    ThreadsMap::const_iterator i = threads_.begin();
    const ThreadsMap::const_iterator iEnd = threads_.end();
    
    for ( ; i != iEnd; ++i ) {
        try {
            if ( i->second == QUIT_ONLY_ACTION )
                continue;

            void * threadResult = 0;
        
            const int err = pthread_join( i->first, &threadResult );
            
            if ( err != 0 ) {
                ostringstream oss;
                
                size_t buflen=256;
                char buf[buflen];
                oss << "pthread_join error " << err
                    << " (" << strerror_r( err, buf, buflen ) << ")";
                    
                programLogErrorIfPossible( oss.str() );
            }
        } catch ( ... ) {
            // Just stifle any exception
        }
    }
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


void
AutoPthreadQuitAndJoinGroup::requestQuitsNoThrow( ) const
try {
    ThreadsMap::const_iterator i = threads_.begin();
    const ThreadsMap::const_iterator iEnd = threads_.end();
    
    for ( ; i != iEnd; ++i ) {
        try {
            if ( i->second != JOIN_ONLY_ACTION )
                RequestThreadQuit( i->first );
        } catch ( ... ) {
            // just stifle the exception
        }
    }
} catch ( ... ) {
    // just stifle the exception
}


void
AutoPthreadQuitAndJoinGroup::swap( AutoPthreadQuitAndJoinGroup & rhs )
{
    threads_.swap( rhs.threads_ );
}


void
AutoPthreadQuitAndJoinGroup::insert( const pthread_t thread,
                                     const Action    action )
{
    if ( threads_.insert( make_pair( thread, action ) ).second != true )
        throw CARMA_ERROR( "thread already in AutoPthreadQuitAndJoinGroup" );
}


void
AutoPthreadQuitAndJoinGroup::insert( const pthread_t thread )
{
    insert( thread, QUIT_AND_JOIN_ACTION );
}


void
AutoPthreadQuitAndJoinGroup::remove( const pthread_t thread )
{
    const size_t numErased = threads_.erase( thread );
    
    if ( numErased == 0 )
        throw CARMA_ERROR( "thread not in AutoPthreadQuitAndJoinGroup" );
    
    if ( numErased != 1 ) {
        ostringstream oss;
        
        oss << "thread erased from AutoPthreadQuitAndJoinGroup " << numErased
            << " times which is quite unexpected";

        programLogErrorIfPossible( oss.str() );
    }
}
