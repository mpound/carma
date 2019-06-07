#include "carma/correlator/obsRecord2/SeqNoInfo.h"

#include "carma/util/ScopedExclusiveLock.h"
#include "carma/util/ScopedSharedLock.h"

using namespace ::std;
using namespace carma;
using namespace carma::correlator;
using namespace carma::correlator::obsRecord2;
using namespace carma::util;


namespace {

typedef ScopedSharedLock< PthreadRWLock > ScopedReadLock;
typedef ScopedExclusiveLock< PthreadRWLock > ScopedWriteLock;

}  // namespace < anonymous >


SeqNoInfo::SeqNoInfo( const int  bwSeqNo,
                      const bool bwSeqNoSuccess ) :
guard_(),
bwSeqNo_( bwSeqNo ),
bwSeqNoSuccess_( bwSeqNoSuccess )
{
}


SeqNoInfo::~SeqNoInfo( )
try {
} catch ( ... ) {
    // Just stfile any exception
    
    return;
}


void
SeqNoInfo::getInfo(
    int * const  bwSeqNo,
    bool * const bwSeqNoSuccess ) const
{
    const ScopedReadLock readLock( guard_ );

    if ( bwSeqNo != 0 )
        *bwSeqNo = bwSeqNo_;

    if ( bwSeqNoSuccess != 0 )
        *bwSeqNoSuccess = bwSeqNoSuccess_;
}


void
SeqNoInfo::setInfo(
    const int * const  bwSeqNo,
    const bool * const bwSeqNoSuccess )
{
    const ScopedWriteLock writeLock( guard_ );

    if ( bwSeqNo != 0 )
        bwSeqNo_ = *bwSeqNo;

    if ( bwSeqNoSuccess != 0 )
        bwSeqNoSuccess_ = *bwSeqNoSuccess;
}
