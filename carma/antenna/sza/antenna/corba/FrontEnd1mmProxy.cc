#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/FrontEnd1mmProxy.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;

/**.......................................................................
 * Constructor with a pointer to the parent AntennaMaster
 */
FrontEnd1mmProxy::FrontEnd1mmProxy(AntennaMaster* parent) :
  FrontEndProxy(parent) {}

/**.......................................................................
 * Destructor function
 */
FrontEnd1mmProxy::~FrontEnd1mmProxy() {}

/**.......................................................................
 * Set a gate voltage.
 */
void FrontEnd1mmProxy::setVG(carma::antenna::common::FrontEndControl::Amp amp,
			     carma::antenna::common::FrontEndControl::
			     Stage stage,
			     float voltage)
{
  // Do nothing
}

/**.......................................................................
 * Set a drain voltage.
 */
void FrontEnd1mmProxy::setVD(carma::antenna::common::FrontEndControl::Amp amp,
			     carma::antenna::common::FrontEndControl::
			     Stage stage,
			     float voltage)
{
  // Do nothing
}

/**.......................................................................
 * Set a drain current
 */
void FrontEnd1mmProxy::setID(carma::antenna::common::FrontEndControl::Amp amp,
			     carma::antenna::common::FrontEndControl::
			     Stage stage,
			     float current)
{
  // Do nothing
}
