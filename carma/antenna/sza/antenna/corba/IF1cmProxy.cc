#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/IF1cmProxy.h"

using namespace std;

using namespace sza::antenna::corba;
using namespace sza::antenna::control;

/**.......................................................................
 * Constructor.
 */
IF1cmProxy::IF1cmProxy(sza::antenna::control::AntennaMaster* parent) : IFProxy(parent) {}

/**.......................................................................
 * Destructor.
 */
IF1cmProxy::~IF1cmProxy() {}

void IF1cmProxy::selectRx()
{
  std::cout << "IF: selectRx() stub" << std::endl;
};


