#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/Optics1cmProxy.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;

/**.......................................................................
 * Constructor.
 */
Optics1cmProxy::Optics1cmProxy(AntennaMaster* parent) : OpticsProxy(parent) {}

/**.......................................................................
 * Destructor.
 */
Optics1cmProxy::~Optics1cmProxy() {}

void Optics1cmProxy::selectRx()
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packCalTertMsg(sza::array::CALTERT_POSITION_TERT,
				 sza::util::Rx::RX1CM,
				 sza::array::TERTPOS_RX30GHZ,
				 sza::util::CalPos::NONE,
				 true,
				 sza::util::CalTertTypes::MODULE,
				 sza::util::CalTertTypes::READ,
				 sequenceNumber());
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
};
