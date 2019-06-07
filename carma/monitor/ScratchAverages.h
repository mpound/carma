#ifndef CARMA_MONIOR_SCRATCHAVERAGES_H
#define CARMA_MONIOR_SCRATCHAVERAGES_H

#include "carma/monitor/MonitorPointNumeric.h"
#include "carma/monitor/monitorPointSpecializations.h"


namespace carma {
namespace monitor {


struct ScratchAverages {
    MonitorPointAverageNumeric numericAccum;
    MonitorPointAverageBool    boolAccum;
    MonitorPointAverageComplex complexAccum;
    MonitorPointAverageEnum    enumAccum;
    
    ScratchAverages( );
};


}  // namespace carma::util
}  // namespace carma


inline
carma::monitor::ScratchAverages::ScratchAverages( ) :
numericAccum( 0.0, -(HUGE_VAL), (HUGE_VAL) ),
boolAccum( 1, 0, 1 ),
complexAccum( ::std::complex< float >( 0.0, 0.0 ),
              ::std::complex< float >( NAN, NAN ),
              ::std::complex< float >( 0.0, 0.0 ) ),
enumAccum( 0, 0, 0 )
{
}


#endif
