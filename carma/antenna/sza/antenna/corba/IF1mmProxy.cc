#include "carma/antenna/sza/antenna/corba/IF1mmProxy.h"

using namespace std;

using namespace sza::antenna::corba;

/**.......................................................................
 * Constructor.
 */
IF1mmProxy::IF1mmProxy(sza::antenna::control::AntennaMaster* parent) : IFProxy(parent) {}

/**.......................................................................
 * Destructor.
 */
IF1mmProxy::~IF1mmProxy() {}

void IF1mmProxy::selectRx()
{
  std::cout << "IF: selectRx() stub" << std::endl;
};


