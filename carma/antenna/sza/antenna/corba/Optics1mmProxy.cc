#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/Optics1mmProxy.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;

/**.......................................................................
 * Constructor.
 */
Optics1mmProxy::Optics1mmProxy(AntennaMaster* parent) : OpticsProxy(parent) {}

/**.......................................................................
 * Destructor.
 */
Optics1mmProxy::~Optics1mmProxy() {}

void Optics1mmProxy::selectRx()
{
  std::cout << "Optics1mmProxy: selectRx() stub" << std::endl;
};
