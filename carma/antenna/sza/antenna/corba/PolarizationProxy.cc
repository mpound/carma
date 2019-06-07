#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/PolarizationProxy.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
PolarizationProxy::PolarizationProxy(AntennaMaster* parent) : Proxy(parent) {}

/**.......................................................................
 * Destructor function
 */
PolarizationProxy::~PolarizationProxy() {};

/**.......................................................................
 * Set the polarizationization mode
 */
void PolarizationProxy::
setState(carma::antenna::common::PolarizationControl::State pol,
	 CORBA::ULong seq)
{
  std::cout << "Polarization::setState() stub" << std::endl;
}
