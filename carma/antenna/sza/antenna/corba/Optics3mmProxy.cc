#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/Optics3mmProxy.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;

/**.......................................................................
 * Constructor.
 */
Optics3mmProxy::Optics3mmProxy(AntennaMaster* parent)  : OpticsProxy(parent) {}

/**.......................................................................
 * Destructor.
 */
Optics3mmProxy::~Optics3mmProxy() {}

void Optics3mmProxy::selectRx()
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packCalTertMsg(sza::array::CALTERT_POSITION_TERT,
				 sza::util::Rx::RX3MM,
				 sza::array::TERTPOS_RX90GHZ,
				 sza::util::CalPos::NONE,
				 true,
				 sza::util::CalTertTypes::MODULE,
				 sza::util::CalTertTypes::READ,
				 sequenceNumber());
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
};
