#ifndef CARMA_MONITOR_SANITYCHECKS_H
#define CARMA_MONITOR_SANITYCHECKS_H

#include "carma/monitor/MonitorPoint.h"
#include "carma/monitor/types.h"


namespace carma {
namespace monitor {


void logBadValidityFlags( uchar                vf,
                          const MonitorPoint * mp,
                          bool                 fix,
                          const char *         context );

void logBadValidityFlags( uchar                vf,
                          tagIDType            tagId,
                          int                  sampleIndex,
                          bool                 fix,
                          const char *         context );

void logBadValidity( MonitorPoint::VALIDITY validity,
                     const MonitorPoint *   mp,
                     bool                   fix,
                     const char *           context );


uchar sanityCheckValidityFlags( uchar                vf,
                                const MonitorPoint * mp,
                                bool                 fix,
                                const char *         context );

uchar sanityCheckValidityFlags( uchar                vf,
                                tagIDType            tagId,
                                int                  sampleIndex,
                                bool                 fix,
                                const char *         context );

MonitorPoint::VALIDITY
sanityCheckValidity( MonitorPoint::VALIDITY validity,
                     const MonitorPoint *   mp,
                     bool                   fix,
                     const char *           context );


}  // namespace carma::monitor
}  // namespace carma


inline carma::monitor::uchar
carma::monitor::sanityCheckValidityFlags(
    const uchar                vf,
    const MonitorPoint * const mp,
    const bool                 fix,
    const char * const         context )
{
    if ( vf >= MonitorPoint::MAX_VALIDITY ) {
        logBadValidityFlags( vf, mp, fix, context );
        
        if ( fix )
            return MonitorPoint::INVALID_NO_DATA;
        else
            return vf;
    } else
        return vf;
}


inline carma::monitor::uchar
carma::monitor::sanityCheckValidityFlags(
    const uchar                vf,
    const tagIDType            tagId,
    const int                  sampleIndex,
    const bool                 fix,
    const char * const         context )
{
    if ( vf >= MonitorPoint::MAX_VALIDITY ) {
        logBadValidityFlags( vf, tagId, sampleIndex, fix, context );
        
        if ( fix )
            return MonitorPoint::INVALID_NO_DATA;
        else
            return vf;
    } else
        return vf;
}


inline carma::monitor::MonitorPoint::VALIDITY
carma::monitor::sanityCheckValidity(
    const MonitorPoint::VALIDITY validity,
    const MonitorPoint * const   mp,
    const bool                   fix,
    const char * const           context )
{
    if ( static_cast< uchar >( validity ) >= MonitorPoint::MAX_VALIDITY ) {
        logBadValidity( validity, mp, fix, context );
        
        if ( fix )
            return MonitorPoint::INVALID_NO_DATA;
        else
            return validity;
    } else
        return validity;
}


#endif
