#include "carma/util/WorkResult.h"

#include <stdexcept>
#include <vector>

#include <sys/time.h>

#include "carma/util/WorkResultSetPostError.h"
#include "carma/util/WorkResultSetWaitError.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/PthreadRWLock.h"
#include "carma/util/PthreadCond.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedExclusiveLock.h"
#include "carma/util/ScopedSharedLock.h"
#include "carma/util/stlContainerUtils.h"
#include "carma/util/Trace.h"


using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


const Trace::TraceLevel kProblemTraceLevel = Trace::TRACE1;
const Trace::TraceLevel kLifecycleTraceLevel = Trace::TRACE5;
const Trace::TraceLevel kRefCountTraceLevel = Trace::TRACE6;


struct ::timespec
convertMillisToAbsTimeout( const unsigned long ms )
{
    struct ::timespec result;

    struct ::timeval now;

    ::gettimeofday( &now, 0 );

    const unsigned long seconds = (ms / 1000UL);
    const unsigned long nanoseconds = (ms % 1000UL) * 1000UL * 1000UL;

    result.tv_sec = now.tv_sec + seconds;
    result.tv_nsec = (now.tv_usec * 1000UL) + nanoseconds;

    if ( result.tv_nsec >= (1000L * 1000L * 1000L) ) {
        result.tv_sec += 1UL;
        result.tv_nsec -= (1000L * 1000L * 1000L);
    }

    return result;
}


struct ::timespec
convertTimevalToAbsTimeout( const struct ::timeval & tvAbsTimeout )
{
    struct ::timespec result;

    result.tv_sec = tvAbsTimeout.tv_sec;
    result.tv_nsec = 1000L * tvAbsTimeout.tv_usec;

    return result;
}


string
escapeAndQuoteStringAsNeeded( const string & s )
{
    string result;
    
    if ( s.find_first_of( " ," ) != string::npos )
        result = string( "\"" ) + s + string( "\"" );
    else
        result = s;

    return result;
}

string
keySetToString( const set< string > & keys,
                const bool            bracketSingletons = false )
{
    if ( (bracketSingletons == false) && sizeIsExactlyOne( keys ) )
        return escapeAndQuoteStringAsNeeded( *(keys.begin()) );

    string s = "{";

    bool noneYet = true;

    set< string >::const_iterator i = keys.begin();
    const set< string >::const_iterator iEnd = keys.end();

    for ( ; i != iEnd; ++i ) {
        if ( noneYet )
            noneYet = false;
        else
            s += ",";

        s += string( " " ) + escapeAndQuoteStringAsNeeded( *i );
    }

    s += " }";

    return s;
}


}  // namespace < anonymous >


class WorkResultSet::Impl {
    public:
        struct Info {
            bool   normal;
            string errorText;
            
            explicit Info( );
            
            explicit Info( const string & inErrorText );
        };

        static Impl * make( const string & id );

        static Impl * addSetRef( Impl * impl );

        static Impl * addKeyRef( Impl *         impl,
                                 const string & key );

        static void removeSetRef( const Impl * impl );

        static void removeKeyRef( const Impl *   impl,
                                  const string & key );

        string getId( ) const;

        void addKeys( const set< string > & keys );

        void removeKeys( const set< string > & keys );

        void wait( const struct ::timespec & absTimeout,
                   bool                      requireNormal,
                   PostState                 postStateAfterTimeout ) const;

        void post( const string & key,
                   const Info &   info );

        // Testing and debugging utility methods

        void verifyKeys( const set< string > & keys ) const;

    private:
        explicit Impl( const string & id );

        virtual ~Impl( );

        typedef ScopedLock< PthreadMutex > RefCountsAndIdGuardLock;

        typedef ScopedSharedLock< PthreadRWLock > KeysGuardReadLock;
        typedef ScopedExclusiveLock< PthreadRWLock > KeysGuardWriteLock;

        typedef ScopedLock< PthreadMutex > PostedInfosGuardLock;

        typedef map< string, Info > KeyToInfoMap;

        const string id_;

        mutable PthreadMutex refCountsGuard_;
        mutable ::size_t     setRefCount_;
        mutable ::size_t     keyRefCount_;

        mutable PthreadRWLock keysGuard_;
        set< string >         keys_;

        mutable PthreadCond  infosPostedCond_;
        mutable PthreadMutex postedInfosGuard_;
        KeyToInfoMap         postedInfos_;
        mutable PostState    postState_;
};


WorkResultSet::Impl::Info::Info( ) :
normal( true ),
errorText()
{
}


WorkResultSet::Impl::Info::Info( const string & inErrorText ) :
normal( false ),
errorText( inErrorText )
{
}


WorkResultSet::Impl::Impl( const string & id ) :
id_( id ),

refCountsGuard_(),
setRefCount_( 1 ),
keyRefCount_( 0 ),

keysGuard_(),
keys_(),

infosPostedCond_(),
postedInfosGuard_(),
postedInfos_(),
postState_( GOOD_POST_STATE )
{
    // okay to use id_ because we are the only ones with a ref to this object
    // AND because we know the object should still
    // exist and the id_ member is well and truly immutable const
    
    CARMA_CPTRACE( kLifecycleTraceLevel, "constructing " << id_ );
}


WorkResultSet::Impl::~Impl( )
try {
    // It's okay to use id_ because we know the object should still
    // exist and the id_ member is well and truly immutable const
    
    CARMA_CPTRACE( kLifecycleTraceLevel, "destructing " << id_ );
} catch ( ... ) {
    try {
        const string msg =
            "Stifling exception in WorkResultSet::Impl::~Impl: " +
            getStringForCaught();
                           
        programLogErrorIfPossible( msg );
    } catch ( ... ) {
    }
    
    // just stifle any exception

    return;
}


WorkResultSet::Impl *
WorkResultSet::Impl::make( const string & id )
{
    return new Impl( id );
}


WorkResultSet::Impl *
WorkResultSet::Impl::addSetRef( Impl * const impl )
{
    if ( impl == 0 ) {
        CARMA_CPTRACE( kProblemTraceLevel, "null pointer" );
    } else {
        ::size_t newSetRefCount;

        {
            const RefCountsAndIdGuardLock lock( impl->refCountsGuard_ );

            newSetRefCount = impl->setRefCount_ + 1;

            if ( newSetRefCount <= impl->setRefCount_ ) {
                // It's okay to use id_ because we know the object should still
                // exist and the id_ member is well and truly immutable const

                const string msg = escapeAndQuoteStringAsNeeded( impl->id_ ) +
                                   " set ref count overflow";

                CARMA_CPTRACE( kProblemTraceLevel, msg );

                throw CARMA_ERROR( msg );
            }

            impl->setRefCount_ = newSetRefCount;
        }

        // It's okay to use id_ because we know the object should still
        // exist and the id_ member is well and truly immutable const

        CARMA_CPTRACE( kRefCountTraceLevel,
                       "Incremented set ref count for " <<
                       escapeAndQuoteStringAsNeeded( impl->id_ ) << " is " <<
                       newSetRefCount );
    }

    return impl;
}


WorkResultSet::Impl *
WorkResultSet::Impl::addKeyRef( Impl * const   impl,
                                const string & key )
{
    if ( impl == 0 ) {
        CARMA_CPTRACE( kProblemTraceLevel, "null pointer" );
    } else {
        {
            const KeysGuardReadLock readLock( impl->keysGuard_ );

            if ( impl->keys_.find( key ) == impl->keys_.end() ) {
                // It's okay to use id_ because we know the object should still
                // exist and the id_ member is well and truly immutable const

                string msg;

                msg += "WorkResultSet ";
                msg += escapeAndQuoteStringAsNeeded( impl->id_ );
                msg += " does not have ";
                msg += escapeAndQuoteStringAsNeeded( key );
                msg += " as a key.";

                throw CARMA_ERROR( msg );
            }
        }

        ::size_t newKeyRefCount;

        {
            const RefCountsAndIdGuardLock lock( impl->refCountsGuard_ );

            newKeyRefCount = impl->keyRefCount_ + 1;

            if ( newKeyRefCount <= impl->keyRefCount_ ) {
                // It's okay to use id_ because we know the object should still
                // exist and the id_ member is well and truly immutable const

                const string msg = escapeAndQuoteStringAsNeeded( impl->id_ ) +
                                   " key ref count overflow";

                CARMA_CPTRACE( kProblemTraceLevel, msg );

                throw CARMA_ERROR( msg );
            }

            impl->keyRefCount_ = newKeyRefCount;
        }

        // It's okay to use id_ because we know the object should still
        // exist and the id_ member is well and truly immutable const

        CARMA_CPTRACE( kRefCountTraceLevel,
                       "Incremented key ref count for " <<
                       escapeAndQuoteStringAsNeeded( impl->id_ ) << " is " <<
                       newKeyRefCount );
    }

    return impl;
}


void
WorkResultSet::Impl::removeSetRef( const Impl * const impl )
{
    if ( impl == 0 ) {
        CARMA_CPTRACE( kProblemTraceLevel, "null pointer" );
    } else {
        // It's okay to use id_ because we know the object should still
        // exist and the id_ member is well and truly immutable const
        
        const string quotedId = escapeAndQuoteStringAsNeeded( impl->id_ );

        ::size_t newSetRefCount;
        ::size_t oldKeyRefCount;

        {
            const RefCountsAndIdGuardLock lock( impl->refCountsGuard_ );

            newSetRefCount = (impl->setRefCount_ - 1);
            oldKeyRefCount = impl->keyRefCount_;
            
            if ( newSetRefCount >= impl->setRefCount_ ) {
                const string msg = quotedId + " set ref count underflow";

                CARMA_CPTRACE( kProblemTraceLevel, msg );

                throw CARMA_ERROR( msg );
            }

            impl->setRefCount_ = newSetRefCount;
            
            if ( (newSetRefCount < 1) && (oldKeyRefCount > 0) ) {
                const PostedInfosGuardLock lock( impl->postedInfosGuard_ );
                
                switch ( impl->postState_ ) {
                    case GOOD_POST_STATE:
                        impl->postState_ = NO_WAITERS_DROPPED_POST_STATE;
                        break;
                        
                    case LATE_POST_STATE:
                        impl->postState_ = LATE_DROPPED_POST_STATE;
                        break;
                        
                    case LATE_DROPPED_POST_STATE:
                        // We're good here
                        break;
                        
                    case NO_WAITERS_DROPPED_POST_STATE:
                        // Uh, what's this all about?
                        break;
                }
            }
        }

        // By this point id_ may indeed have been deleted by another thread
        // along with the rest of the impl instance

        CARMA_CPTRACE( kRefCountTraceLevel,
                       "Decremented set ref count for " << quotedId <<
                       " is " << newSetRefCount );

        if ( (newSetRefCount < 1) && (oldKeyRefCount < 1) )
            delete impl;
    }
}


void
WorkResultSet::Impl::removeKeyRef( const Impl * const impl,
                                   const string &     key )
{
    if ( impl == 0 ) {
        CARMA_CPTRACE( kProblemTraceLevel, "null pointer" );
    } else {
        // It's okay to use id_ because we know the object should still
        // exist and the id_ member is well and truly immutable const
        
        const string quotedId = escapeAndQuoteStringAsNeeded( impl->id_ );

        {
            const KeysGuardReadLock readLock( impl->keysGuard_ );

            if ( impl->keys_.find( key ) == impl->keys_.end() ) {
                string msg;

                msg += "WorkResultSet ";
                msg += quotedId;
                msg += " does not have ";
                msg += escapeAndQuoteStringAsNeeded( key );
                msg += " as a key.";

                throw CARMA_ERROR( msg );
            }
        }

        ::size_t oldSetRefCount;
        ::size_t newKeyRefCount;

        {
            const RefCountsAndIdGuardLock lock( impl->refCountsGuard_ );

            oldSetRefCount = impl->setRefCount_;
            newKeyRefCount = (impl->keyRefCount_ - 1);

            if ( newKeyRefCount >= impl->keyRefCount_ ) {
                const string msg = quotedId + " key ref count underflow";

                CARMA_CPTRACE( kProblemTraceLevel, msg );

                throw CARMA_ERROR( msg );
            }

            impl->keyRefCount_ = newKeyRefCount;
        }

        // By this point id_ may indeed have been deleted by another thread
        // along with the rest of the impl instance

        CARMA_CPTRACE( kRefCountTraceLevel,
                       "Decremented key ref count for " << quotedId <<
                       " is " << newKeyRefCount );

        if ( (oldSetRefCount < 1) && (newKeyRefCount < 1) )
            delete impl;
    }
}


string
WorkResultSet::Impl::getId( ) const
{
    // It's okay to use id_ because we know the object should still
    // exist and the id_ member is well and truly immutable const
    
    return id_;
}


void
WorkResultSet::Impl::addKeys( const set< string > & keysToAdd )
{
    const KeysGuardWriteLock writeLock( keysGuard_ );

    set< string > badKeys;

    set_intersection( keysToAdd.begin(), keysToAdd.end(),
                      keys_.begin(), keys_.end(),
                      inserter( badKeys, badKeys.begin() ) );

    if ( badKeys.empty() == false ) {
        // It's okay to use id_ because we know the object should still
        // exist and the id_ member is well and truly immutable const
    
        string msg;

        msg += "WorkResultSet ";
        msg += escapeAndQuoteStringAsNeeded( id_ );
        msg += " already has ";
        msg += keySetToString( badKeys );
        if ( sizeIsExactlyOne( badKeys ) )
            msg += " as a key.";
        else
            msg += " as keys.";

        throw CARMA_ERROR( msg );
    }

    set< string > newKeys;

    set_union( keys_.begin(), keys_.end(),
               keysToAdd.begin(), keysToAdd.end(),
               inserter( newKeys, newKeys.begin() ) );

    keys_.swap( newKeys );
}


void
WorkResultSet::Impl::removeKeys( const set< string > & keysToRemove )
{
    const KeysGuardWriteLock writeLock( keysGuard_ );

    if ( includes( keys_.begin(), keys_.end(),
                   keysToRemove.begin(), keysToRemove.end() ) == false ) {
        set< string > badKeys;

        set_difference( keysToRemove.begin(), keysToRemove.end(),
                        keys_.begin(), keys_.end(),
                        inserter( badKeys, badKeys.begin() ) );

        // It's okay to use id_ because we know the object should still
        // exist and the id_ member is well and truly immutable const

        string msg;

        msg += "WorkResultSet ";
        msg += escapeAndQuoteStringAsNeeded( id_ );
        msg += " does not have ";
        msg += keySetToString( badKeys );
        if ( sizeIsExactlyOne( badKeys ) )
            msg += " as a key.";
        else
            msg += " as keys.";

        throw CARMA_ERROR( msg );
    }

    set< string > newKeys;

    set_difference( keys_.begin(), keys_.end(),
                    keysToRemove.begin(), keysToRemove.end(),
                    inserter( newKeys, newKeys.begin() ) );

    keys_.swap( newKeys );
}


void
WorkResultSet::Impl::wait(
    const struct ::timespec & absTimeout,
    const bool                requireNormal,
    const PostState           postStateAfterTimeout ) const
{
    if ( postStateAfterTimeout == NO_WAITERS_DROPPED_POST_STATE ) {
        throw CARMA_ERROR( "NO_WAITERS_DROPPED_POST_STATE is for internal"
                           " use only" );
    }
    
    set< string > waitedKeys;
    {
        const KeysGuardReadLock readLock( keysGuard_ );

        waitedKeys = keys_;
    }

    if ( waitedKeys.empty() )
        return;

    set< string > keysLeft = waitedKeys;

    map< string, string > abnormals;

    {
        bool timedOutAlready = false;

        const PostedInfosGuardLock lock( postedInfosGuard_ );

        while ( true ) {
            vector< string > keysReady;

            {
                keysReady.reserve( postedInfos_.size() );

                KeyToInfoMap::const_iterator i = postedInfos_.begin();
                const KeyToInfoMap::const_iterator iEnd = postedInfos_.end();

                for ( ; i != iEnd; ++i ) {
                    if ( (keysLeft.find( i->first ) != keysLeft.end()) &&
                         (i->second.normal == false) )
                        abnormals.insert( make_pair( i->first,
                                                     i->second.errorText ) );

                    keysReady.push_back( i->first );
                }
            }

            vector< string > newKeysLeft;

            set_difference( keysLeft.begin(), keysLeft.end(),
                            keysReady.begin(), keysReady.end(),
                            back_inserter( newKeysLeft ) );

            {
                set< string > temp( newKeysLeft.begin(), newKeysLeft.end() );

                keysLeft.swap( temp );
            }

            if ( keysLeft.empty() )
                break;  // We're done
                
            if ( timedOutAlready ) {
                postState_ = postStateAfterTimeout;

                break;
            }

            if ( infosPostedCond_.TimedWait( postedInfosGuard_,
                                             absTimeout ) == false )
                timedOutAlready = true;
        }
    }

    if ( (keysLeft.empty() == false) ||
         (requireNormal && (abnormals.empty() == false)) ) {
        // It's okay to use id_ because we know the object should still
        // exist and the id_ member is well and truly immutable const
        
        const string id = id_;
        
        if ( requireNormal ) {
            throw WaitError( __FILE__,
                             __LINE__,
                             id,
                             waitedKeys,
                             keysLeft,
                             abnormals );
        } else {
            throw WaitError( __FILE__,
                             __LINE__,
                             id,
                             waitedKeys,
                             keysLeft,
                             map< string, string >() );
        }
    }
}


void
WorkResultSet::Impl::post( const string & key,
                           const Info &   info )
{
    {
        const KeysGuardReadLock readLock( keysGuard_ );

        if ( keys_.find( key ) == keys_.end() ) {
            // It's okay to use id_ because we know the object should still
            // exist and the id_ member is well and truly immutable const

            string msg;

            msg += "WorkResultSet ";
            msg += escapeAndQuoteStringAsNeeded( id_ );
            msg += " does not have ";
            msg += escapeAndQuoteStringAsNeeded( key );
            msg += " as a key.";

            throw CARMA_ERROR( msg );
        }
    }

    
    PostState postState = NO_WAITERS_DROPPED_POST_STATE;
    {
        const PostedInfosGuardLock lock( postedInfosGuard_ );

        const bool insertedOkay =
            postedInfos_.insert( make_pair( key, info ) ).second;

        if ( insertedOkay == false ) {
            // It's okay to use id_ because we know the object should still
            // exist and the id_ member is well and truly immutable const

            string msg;

            msg += "WorkResultSet ";
            msg += escapeAndQuoteStringAsNeeded( id_ );
            msg += " already has a posted result for ";
            msg += escapeAndQuoteStringAsNeeded( key );
            msg += ".";

            throw CARMA_ERROR( msg );
        }
        
        postState = postState_;
    }

    infosPostedCond_.Broadcast();

    if ( postState != GOOD_POST_STATE ) {
        // It's okay to use id_ because we know the object should still
        // exist and the id_ member is well and truly immutable const

        const string id = id_;

        throw PostError( __FILE__,
                         __LINE__,
                         id,
                         key,
                         info.normal,
                         postState );
    }
}


void
WorkResultSet::Impl::verifyKeys( const set< string > & keys ) const
{
    const KeysGuardWriteLock readLock( keysGuard_ );

    if ( keys != keys_ ) {
        // It's okay to use id_ because we know the object should still
        // exist and the id_ member is well and truly immutable const

        string msg;

        msg += "WorkResultSet ";
        msg += escapeAndQuoteStringAsNeeded( id_ );
        msg += " key set was not ";
        msg += keySetToString( keys, true );
        msg += " but rather was ";
        msg += keySetToString( keys_, true );

        throw CARMA_ERROR( msg );
    }
}


namespace {


string
buildPostErrorMessage( const string &                 wrsId,
                       const string &                 key,
                       const bool                     resultWasNormal,
                       const WorkResultSet::PostState postState )
{
    string msg;

    msg += "WorkResultSet ";
    msg += escapeAndQuoteStringAsNeeded( wrsId );
    msg += " ";
    msg += (resultWasNormal ? "normal" : "abnormal" );
    msg += " result for ";
    msg += escapeAndQuoteStringAsNeeded( key );
    msg += " was posted ";
    
    bool okay = false;
    switch ( postState ) {
        case WorkResultSet::GOOD_POST_STATE:
            okay = true;
            msg += "just fine which makes this error a bit confusing";
            break;
            
        case WorkResultSet::LATE_POST_STATE:
            okay = true;
            msg += "late";
            break;

        case WorkResultSet::LATE_DROPPED_POST_STATE:
            okay = true;
            msg += "so late that all waiters had given up waiting";
            break;

        case WorkResultSet::NO_WAITERS_DROPPED_POST_STATE:
            okay = true;
            msg += "but all waiters had disappeared";
            break;
    }
    
    if ( okay == false ) {
        ostringstream oss;
        
        oss << "with an unknown post state of " << postState;
        
        msg += oss.str();
    }
    
    msg += ".";

    return msg;
}


string
stringForAbnormalsMap( const map< string, string > & abnormals )
{
    typedef map< string, set< string > > ErrorTextToKeysMap;
    
    ErrorTextToKeysMap errorTextToKeysMap;
    {
        map< string, string >::const_iterator i =
            abnormals.begin();
            
        const map< string, string >::const_iterator iEnd =
            abnormals.end();
            
        for ( ; i != iEnd; ++i )
            errorTextToKeysMap[ i->second ].insert( i->first );
    }
    
    string result;
    {
        ErrorTextToKeysMap::const_iterator i =
            errorTextToKeysMap.begin();
            
        const ErrorTextToKeysMap::const_iterator iEnd =
            errorTextToKeysMap.end();
            
        for ( ; i != iEnd; ++i ) {
            if ( result.empty() == false )
                result += ", ";
                
            if ( sizeIsExactlyOne( i->second ) )
                result += "key ";
            else
                result += "keys ";
            
            result += keySetToString( i->second );

            result += " had error \"";
            result += i->first;
            result += "\"";
        }
    }
    
    return result;
}


string
buildWaitErrorMessage( const string &                wrsId,
                       const set< string > &         waitedKeys,
                       const set< string > &         keysLeft,
                       const map< string, string > & abnormals )
{
    ostringstream oss;

    oss << "wait for " << escapeAndQuoteStringAsNeeded( wrsId )
        << " work result set ";

    if ( keysLeft.empty() ) {
        oss << "completed";
        
        if ( sizeIsExactlyOne( waitedKeys ) == false )
            oss << " with all " << waitedKeys.size() << " keys ready";

        if ( abnormals.empty() == false )
            oss << " but " << stringForAbnormalsMap( abnormals );
    } else {
        oss << "timed out with ";
        if ( sizeIsExactlyOne( keysLeft ) )
            oss << "key ";
        else
            oss << "keys ";
        oss << keySetToString( keysLeft ) << " still not ready";

        if ( abnormals.empty() == false )
            oss << " and " << stringForAbnormalsMap( abnormals );
    }

    oss << ".";

    return oss.str();
}


}  // namespace < anonymous >


WorkResultSet::PostError::PostError( const char * const fileName,
                                     const int          lineNo,
                                     const string &     wrsId,
                                     const string &     key,
                                     const bool         resultWasNormal,
                                     const PostState    postState ) :
ErrorException( buildPostErrorMessage( wrsId,
                                       key,
                                       resultWasNormal,
                                       postState ),
                fileName,
                lineNo ),
key_( key ),
resultWasNormal_( resultWasNormal ),
postState_( postState )
{
}


WorkResultSet::PostError::~PostError( ) throw()
try {
} catch ( ... ) {
    try {
        const string msg =
            "Stifling exception in WorkResultSet::PostError::~PostError: " +
            getStringForCaught();
                           
        programLogErrorIfPossible( msg );
    } catch ( ... ) {
    }

    // Just stifle any exception

    return;
}


string
WorkResultSet::PostError::getKey( ) const
{
    return key_;
}


bool
WorkResultSet::PostError::getResultWasNormal( ) const
{
    return resultWasNormal_;
}


WorkResultSet::PostState
WorkResultSet::PostError::getPostState( ) const
{
    return postState_;
}


WorkResultSet::WaitError::WaitError(
    const char * const            fileName,
    const int                     lineNo,
    const string &                wrsId,
    const set< string > &         waitedKeys,
    const set< string > &         keysLeft,
    const map< string, string > & abnormals ) :
ErrorException( buildWaitErrorMessage( wrsId,
                                       waitedKeys,
                                       keysLeft,
                                       abnormals ),
                fileName,
                lineNo ),
waitedKeys_( waitedKeys ),
keysLeft_( keysLeft ),
abnormals_( abnormals )
{
}


WorkResultSet::WaitError::~WaitError( ) throw()
try {
} catch ( ... ) {
    try {
        const string msg =
            "Stifling exception in WorkResultSet::WaitError::~WaitError: " +
            getStringForCaught();
                           
        programLogErrorIfPossible( msg );
    } catch ( ... ) {
    }

    // Just stifle any exception

    return;
}


set< string >
WorkResultSet::WaitError::getWaitedKeys( ) const
{
    return waitedKeys_;
}


string
WorkResultSet::WaitError::getStringForWaitedKeys( ) const
{
    return keySetToString( waitedKeys_ );
}


bool
WorkResultSet::WaitError::hadUnfinishedKeys( ) const
{
    return (keysLeft_.empty() == false);
}


bool
WorkResultSet::WaitError::singleUnfinishedKey( ) const
{
    return sizeIsExactlyOne( keysLeft_ );
}


set< string >
WorkResultSet::WaitError::getUnfinishedKeys( ) const
{
    return keysLeft_;
}


string
WorkResultSet::WaitError::getStringForUnfinishedKeys( ) const
{
    return keySetToString( keysLeft_ );
}


bool
WorkResultSet::WaitError::hadAbnormals( ) const
{
    return (abnormals_.empty() == false);
}


map< string, string >
WorkResultSet::WaitError::getAbnormals( ) const
{
    return abnormals_;
}


string
WorkResultSet::WaitError::getStringForAbnormals( ) const
{
    return stringForAbnormalsMap( abnormals_ );
}


WorkResultSet::WorkResultSet( const string & id ) :
impl_( 0 )
{
    impl_ = Impl::make( id );
}


WorkResultSet::WorkResultSet( const WorkResultSet & rhs ) :
impl_( 0 )
{
    impl_ = Impl::addSetRef( rhs.impl_ );
}


WorkResultSet::~WorkResultSet( )
try {
    Impl::removeSetRef( impl_ );
} catch ( ... ) {
    try {
        const string msg =
            "Stifling exception in WorkResultSet::~WorkResultSet: " +
            getStringForCaught();
                           
        programLogErrorIfPossible( msg );
    } catch ( ... ) {
    }

    // just stifle any exception

    return;
}


string
WorkResultSet::getId( ) const
{
    return impl_->getId();
}


WorkResult
WorkResultSet::addKey( const string & key )
{
    set< string > keys;

    keys.insert( key );

    impl_->addKeys( keys );

    return WorkResult( impl_, key );
}


map< string, WorkResult >
WorkResultSet::addKeys( const set< string > & keys )
{
    impl_->addKeys( keys );

    try {
        map< string, WorkResult > result;

        set< string >::const_iterator i = keys.begin();
        const set< string >::const_iterator iEnd = keys.end();

        for ( ; i != iEnd; ++i ) {
            const string key = *i;

            const WorkResult wr( impl_, key );

            result.insert( make_pair( key, wr ) );
        }

        return result;
    } catch ( ... ) {
        impl_->removeKeys( keys );

        throw;
    }
}


void
WorkResultSet::removeKeys( const set< string > & keys )
{
    impl_->removeKeys( keys );
}


void
WorkResultSet::waitForAll(
    const unsigned long milliseconds,
    const bool          requireNormal,
    const PostState     postStateAfterTimeout ) const
{
    const ::timespec absTimeout = convertMillisToAbsTimeout( milliseconds );

    impl_->wait( absTimeout, requireNormal, postStateAfterTimeout );
}


void
WorkResultSet::waitForAll(
    const struct ::timeval & tvAbsTimeout,
    const bool               requireNormal,
    const PostState          postStateAfterTimeout ) const
{
    const ::timespec absTimeout = convertTimevalToAbsTimeout( tvAbsTimeout );

    impl_->wait( absTimeout, requireNormal, postStateAfterTimeout );
}


void
WorkResultSet::waitForAll(
    const struct ::timespec & absTimeout,
    const bool                requireNormal,
    const PostState           postStateAfterTimeout ) const
{
    impl_->wait( absTimeout, requireNormal, postStateAfterTimeout );
}


void
WorkResultSet::verifyKeys( const set< string > & keys ) const
{
    impl_->verifyKeys( keys );
}


WorkResult::WorkResult( WorkResultSet::Impl * const setImpl,
                        const string &              key ) :
setImpl_( 0 ),
key_( key )
{
    setImpl_ = WorkResultSet::Impl::addKeyRef( setImpl, key );
}


WorkResult::WorkResult( const WorkResult & rhs ) :
setImpl_( 0 ),
key_( rhs.key_ )
{
    setImpl_ = WorkResultSet::Impl::addKeyRef( rhs.setImpl_, key_ );
}


WorkResult::~WorkResult( )
try {
    WorkResultSet::Impl::removeKeyRef( setImpl_, key_ );
} catch ( ... ) {
    try {
        const string msg =
            "Stifling exception in WorkResult::~WorkResult: " +
            getStringForCaught();
                           
        programLogErrorIfPossible( msg );
    } catch ( ... ) {
    }

    // just stifle any exception

    return;
}


void
WorkResult::postNormal( )
{
    setImpl_->post( key_, WorkResultSet::Impl::Info() );
}


void
WorkResult::postAbnormal( const string & errorText )
{
    setImpl_->post( key_, WorkResultSet::Impl::Info( errorText ) );
}
