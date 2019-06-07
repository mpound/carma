#ifndef CARMA_MONITOR_MONITOR_SYSTEM_SELECTOR_H
#define CARMA_MONITOR_MONITOR_SYSTEM_SELECTOR_H


#include <memory>
#include <string>

namespace carma {
namespace monitor {


typedef enum {
    RAW_CMS_SELECTOR,
    INTERMEDIATE_CMS_SELECTOR,
    FINAL_CMS_SELECTOR
} CmsSelector;


CmsSelector
convertStringToCmsSelector( const ::std::string & s );

class CarmaMonitorSystem;

typedef ::std::auto_ptr< CarmaMonitorSystem > CmsAP;

CmsAP
makeCms( const CmsSelector selector,
         ::std::string &   outName );


}  // namespace carma::monitor
}  // namespace carma


#endif
