// $Id: CorrelatorDataPool.cc,v 1.5 2007/09/04 19:53:52 tcosta Exp $

#include "carma/correlator/lib/CorrelatorDataPool.h"

#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Trace.h"

using namespace ::std;
using namespace carma;
using namespace carma::correlator;
using namespace carma::correlator::lib;
using namespace carma::util;


CorrelatorDataPool::CorrelatorDataPool( )
{
}


CorrelatorDataPool::~CorrelatorDataPool( )
try {
    const ScopedPthreadMutexLock scopelock( mutex_ );
  
    const size_t poolSize = pool_.size();
    
    for ( size_t i = 0; i < poolSize; ++i )
        delete pool_[i];
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


CorrelatorData *
CorrelatorDataPool::getCorrelatorData( )
{
    const ScopedPthreadMutexLock scopelock( mutex_ );
    
    {
        const size_t poolSize = pool_.size();

        for ( size_t i = 0; i < poolSize; ++i ) {
            CorrelatorData * const cd = pool_[i];
            
            if ( cd->incrementRefCountIfZero() )
                return cd;
        }
    }
    
    // all objects are in use, so create a new one.
    CARMA_CPTRACE( Trace::TRACE5, "Creating correlator data object." );
    
    CorrelatorData * const cd = new CorrelatorData; // ref count already set to 1

    pool_.push_back( cd );

    return cd;
}


int
CorrelatorDataPool::getPoolSize( )
{
    size_t poolSize;
    {
        const ScopedPthreadMutexLock scopelock( mutex_ );
        
        poolSize =  pool_.size();
    }
    
    return poolSize;
}


int
CorrelatorDataPool::getInUseCount( )
{
    int count = 0;
    {
        const ScopedPthreadMutexLock scopelock( mutex_ );

        const size_t poolSize = pool_.size();
        for ( size_t i = 0; i < poolSize; ++i ) {
            if ( pool_[i]->getRefCount() != 0 )
                ++count;
        }
    }

    return count;
}


int
CorrelatorDataPool::getAvailableCount( )
{
    int count = 0;
    {
        const ScopedPthreadMutexLock scopelock( mutex_ );
        
        const size_t poolSize = pool_.size();
        for ( size_t i = 0; i < poolSize; ++i ) {
            if ( pool_[i]->getRefCount() == 0 )
                ++count;
        }
    }
    
    return count;
}
