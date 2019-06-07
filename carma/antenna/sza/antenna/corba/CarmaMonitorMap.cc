#include "carma/antenna/sza/antenna/corba/CarmaMonitorMap.h"

using namespace std;

using namespace sza::antenna::corba;

/**.......................................................................
 * Constructor.
 */
CarmaMonitorMap::CarmaMonitorMap() {}

/**.......................................................................
 * Destructor.
 */
CarmaMonitorMap::~CarmaMonitorMap() {}

#if DIR_USE_ANT_CORBA
carma::monitor::MonitorPoint* 
CarmaMonitorMap::getMonitorPoint(carma::monitor::SzaSubsystem* antennaMonitor, 
				 std::string board, std::string block)
{
  return 0;
}
#endif

