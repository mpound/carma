
/*
 *
 * Implementation of base class for all carma windows that use the monitor
 * system.
 *
 * @author Steve Scott 
 * $id: $
 *
 * $CarmaCopyright$
 *
 */

  

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace log4cpp;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;

void MonitorDisplay::preInternalUpdate() 
{
    carma_.readNewest();
}



MonitorDisplay::MonitorDisplay(const string& subtitle, 
        const char* ut, const char* lst, 
        bool visibleTimePanel) :
        CarmaDisplay(subtitle, ut, lst, visibleTimePanel),
        carma_(*new CarmaMonitorSystem())
{
    CARMA_CPTRACE(Trace::TRACE6, "MonitorDisplay constructor");    
}
        
MonitorDisplay::MonitorDisplay(const string& subtitle, 
        bool visibleTimePanel) :
        CarmaDisplay(subtitle, visibleTimePanel), 
        carma_(*new CarmaMonitorSystem())
{
    CARMA_CPTRACE(Trace::TRACE6, "MonitorDisplay constructor");    
}
 

CarmaMonitorSystem& MonitorDisplay::cms() const
{
    return carma_;
}


