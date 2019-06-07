#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/OpticsProxy.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;

/**.......................................................................
 * Constructor with a pointer to the parent AntennaMaster
 */
OpticsProxy::OpticsProxy(AntennaMaster* parent) : Proxy(parent) {}

/**.......................................................................
 * Destructor function
 */
OpticsProxy::~OpticsProxy() {};

void OpticsProxy::selectRx()
{
  COUT("Inside OpticsProxy::selectRx() stub");
};
