#include "carma/antenna/sza/antenna/corba/IF3mmProxy.h"

using namespace std;

using namespace sza::antenna::corba;

/**.......................................................................
 * Constructor.
 */
IF3mmProxy::IF3mmProxy(sza::antenna::control::AntennaMaster* parent) : IFProxy(parent) {}

/**.......................................................................
 * Destructor.
 */
IF3mmProxy::~IF3mmProxy() {}

void IF3mmProxy::selectRx()
{
  std::cout << "IF: selectRx() stub" << std::endl;
};


