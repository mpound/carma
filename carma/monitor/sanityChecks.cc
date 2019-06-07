#include "carma/monitor/sanityChecks.h"

#include <sstream>

#include "carma/dbms/TagIDAuthority.h"
#include "carma/util/programLogging.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


namespace {


void
logBad( const int                  vf,
        const MonitorPoint * const mp,
        const bool                 fix,
        const char * const         context )
{
    ostringstream oss;
    
    oss << "Bad validity flags (" << vf << ")";
    
    if ( mp != 0 )
        oss << " for " << mp->getCanonicalName();
    
    if ( context != 0 )
        oss << " in " << context;
        
    if ( fix )
        oss << " are being fixed";
    else
        oss << " will not be fixed";
        
    programLogErrorIfPossible( oss.str() );
}


void
logBad( const int                  vf,
        const tagIDType            tagId,
        const int                  sampleIndex,
        const bool                 fix,
        const char * const         context )
{
    ostringstream oss;
    
    oss << "Bad validity flags (" << vf << ")";
    
    string mpCanonName;
    try {
        mpCanonName = dbms::TagIDAuthority::getAuthority().lookupName( tagId );
    } catch ( ... ) {
        mpCanonName = "< UNKNOWN TAGID >";
    }
    
    oss << " for " << mpCanonName << " sample index " << sampleIndex;
    
    if ( context != 0 )
        oss << " in " << context;
        
    if ( fix )
        oss << " are being fixed";
    else
        oss << " will not be fixed";
        
    programLogErrorIfPossible( oss.str() );
}


}  // namespace < anonymous >


void
carma::monitor::logBadValidityFlags(
    const uchar                vf,
    const MonitorPoint * const mp,
    const bool                 fix,
    const char * const         context )
{
    logBad( static_cast< int >( vf ), mp, fix, context );
}


void
carma::monitor::logBadValidityFlags(
    const uchar                vf,
    const tagIDType            tagId,
    const int                  sampleIndex,
    const bool                 fix,
    const char * const         context )
{
    logBad( static_cast< int >( vf ), tagId, sampleIndex, fix, context );
}


void
carma::monitor::logBadValidity(
    const MonitorPoint::VALIDITY validity,
    const MonitorPoint * const   mp,
    const bool                   fix,
    const char * const           context )
{
    logBad( static_cast< int >( validity ), mp, fix, context );
}
