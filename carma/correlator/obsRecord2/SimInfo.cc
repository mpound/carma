#include "carma/correlator/obsRecord2/SimInfo.h"

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


SimInfo::SimInfo( const string &                               inifile,
                  const string &                               bwText,
                  const string &                               sourceName,
                  const cobra::CorrelatorInterpolatorSamples & samps,
                  const float                                  dcFreq,
                  const bool                                   dcSbIsUpper,
                  const bool                                   bdcEnabled ) :
inifile_( inifile ),
guard_(),
bwText_( bwText ),
sourceName_( sourceName ),
samps_( samps ),
dcFreq_( dcFreq ),
dcSbIsUpper_( dcSbIsUpper ),
bdcEnabled_(bdcEnabled)
                           
{
}


SimInfo::~SimInfo( )
try {
} catch ( ... ) {
    // Just stfile any exception
    
    return;
}


string
SimInfo::getInifile( ) const
{
    return inifile_;
}


void
SimInfo::getInfo(
    string * const                               bwText,
    string * const                               sourceName,
    cobra::CorrelatorInterpolatorSamples * const samps,
    float * const                                dcFreq,
    bool * const                                 dcSbIsUpper,
    bool * const                                 bdcEnabled ) const
{
    const ScopedReadLock readLock( guard_ );

    if ( bwText != 0 )
        *bwText = bwText_;

    if ( sourceName != 0 )
        *sourceName = sourceName_;

    if ( samps != 0 )
        *samps = samps_;

    if ( dcFreq != 0 )
        *dcFreq = dcFreq_;

    if ( dcSbIsUpper != 0 )
        *dcSbIsUpper = dcSbIsUpper_;

    if ( bdcEnabled != 0 )
        *bdcEnabled= bdcEnabled_;
}


void
SimInfo::setInfo(
    const string * const                               bwText,
    const string * const                               sourceName,
    const cobra::CorrelatorInterpolatorSamples * const samps,
    const float * const                                dcFreq,
    const bool * const                                 dcSbIsUpper ,
    const bool * const                                 bdcEnabled )
{
    const ScopedWriteLock writeLock( guard_ );

    if ( bwText != 0 )
        bwText_ = *bwText;

    if ( sourceName != 0 )
        sourceName_ = *sourceName;

    if ( samps != 0 )
        samps_ = *samps;

    if ( dcFreq != 0 )
        dcFreq_ = *dcFreq;

    if ( dcSbIsUpper != 0 )
        dcSbIsUpper_ = *dcSbIsUpper;

    if ( bdcEnabled != 0 )
        bdcEnabled_  = *bdcEnabled;
}
