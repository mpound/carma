#ifndef CARMA_UTIL_FACILITY_TYPE_H
#define CARMA_UTIL_FACILITY_TYPE_H

#include <syslog.h>


namespace carma {
namespace util {


//! @brief A type for syslog facilities.
//!
//! Syslog facility codes are #define'd in syslog.h
//! so we defined CARMA names for them here.
typedef enum {
    RX_FACILITY             = LOG_LOCAL1,
    DEFAULT_FACILITY        = LOG_LOCAL2,
    MONITOR_FACILITY        = LOG_LOCAL3,
    CONTROL_FACILITY        = LOG_LOCAL4,
    INTERFEROMETRY_FACILITY = LOG_LOCAL5,
    ENVIRONMENT_FACILITY    = LOG_LOCAL6

    // FUTURE0_FACILITY        = LOG_LOCAL0  for future use
    // FUTURE7_FACILITY        = LOG_LOCAL7  for future use
    // Boot messages go on local7... - colby
} facilityType;


}  // namespace carma::util
}  // namespace carma


#endif // CARMA_UTIL_FACILITY_TYPE_H
