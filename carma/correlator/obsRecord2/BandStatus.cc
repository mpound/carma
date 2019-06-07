// $Id: BandStatus.cc,v 1.9 2007/05/08 20:44:51 tcosta Exp $

#include "carma/correlator/obsRecord2/BandStatus.h"

#include "carma/util/ErrorException.h"
#include "carma/util/ScopedExclusiveLock.h"
#include "carma/util/ScopedSharedLock.h"

#include <sstream>

using namespace ::std;
using namespace carma;
using namespace carma::util;
using namespace carma::correlator;
using namespace carma::correlator::obsRecord2;


namespace {


typedef ScopedSharedLock< PthreadRWLock > ScopedGuardReadLock;
typedef ScopedExclusiveLock< PthreadRWLock > ScopedGuardWriteLock;


::pthread_rwlock_t gSingletonGuard = PTHREAD_RWLOCK_INITIALIZER;
BandStatus * gSingleton = 0;


}  // namespace < anonymous >


BandStatus::BandStatus( ) :
guard_(),
bands_()
{
}


BandStatus::~BandStatus( )
try {
} catch ( ... ) {
    // Just stifle an exceptions
    
    return;
}


BandStatus *
BandStatus::getInstance( )
{
    // Let's be optimistic and see if it's already allocated
    {
        const ScopedSharedLock< ::pthread_rwlock_t >
            readLock( gSingletonGuard );
    
        if ( gSingleton != 0 )
            return gSingleton;
    }
    
    // Okay, our optimism was not rewarded so we'll try again and allocate it
    // if still needed
    
    const ScopedExclusiveLock< ::pthread_rwlock_t >
        writeLock( gSingletonGuard );

    if ( gSingleton == 0 )
        gSingleton = new BandStatus;

    return gSingleton;
}


void 
BandStatus::addCorrelatorDO( const string &   name,
                             Correlator_I_var doObject )
{
    bool insertedOkay = false;
    
    {
        const ScopedGuardWriteLock writeLock( guard_ );

        insertedOkay = bands_.insert( make_pair( name, doObject ) ).second;
    }

    if ( insertedOkay != true )
        throw CARMA_ERROR( "DO name already exists, delete it first" );
}


void
BandStatus::removeCorrelatorDO( const string & name )
{
    bool erasedOkay = false;

    {
        const ScopedGuardWriteLock writeLock( guard_ );

        const DOmap::iterator i = bands_.find( name );
        
        if ( i != bands_.end() ) {
            bands_.erase( i );
            erasedOkay = true;
        }
    }

    if ( erasedOkay != true )
        throw CARMA_ERROR( "No such DO name" );
}


Correlator_I_var
BandStatus::getCorrelatorDO( const string & name ) const
{
    bool foundOkay = false;
    Correlator_I_var result;
    
    {
        const ScopedGuardReadLock readLock( guard_ );

        const DOmap::const_iterator i = bands_.find( name );
    
        if ( i != bands_.end() ) {
            result = i->second;
            foundOkay = true;
        }
    }
    
    if ( foundOkay != true ) {
        ostringstream oss;

        oss << "No such DO name: " << name;

        throw CARMA_ERROR( oss.str() );
    }

    return result;
}
