#ifndef CARMA_MONITOR_CORRDESIGNATION_H
#define CARMA_MONITOR_CORRDESIGNATION_H

#include "carma/monitor/ControlCorrelEnum.h"
#include "carma/monitor/Correlator.h"
//#include "carma/util/ErrorException.h"

typedef carma::monitor::CorrelatorDesignationMonitorPointEnum CorrDesignation;
typedef enum CorrDesignation::CORRELATORDESIGNATION MonitorCorrelatorDesignation;
#define CORRELATOR_DESIGNATION_MP correlatorDesignation


namespace carma {
namespace monitor {

inline
carma::util::CorrelatorType
corrDesToCorrType( const MonitorCorrelatorDesignation cde ) 
{
  return static_cast<carma::util::CorrelatorType>(cde);
};

inline
MonitorCorrelatorDesignation
corrTypeToCorrDes( const carma::util::CorrelatorType ct ) 
{
  return static_cast<MonitorCorrelatorDesignation>(ct);
};

#if 0
inline 
int
corrTypeToSubarrayNo( const carma::util::CorrelatorType ct ) 
{
  ThrowCarmaError("corrTypeToSubarrayNo() is an erroneous construct -- subarrays don't imply any particular correlator");
  return 0;
};

inline 
int
corrDesToSubarrayNo( const MonitorCorrelatorDesignation cd ) 
{
  ThrowCarmaError("corrDesToSubarrayNo() is an erroneous construct -- subarrays don't imply any particular correlator");
  return 0;
}
#endif

}  // namespace carma::monitor
}  // namespace carma


#endif
