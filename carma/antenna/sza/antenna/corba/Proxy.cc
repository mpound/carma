#include "carma/szautil/Exception.h"

#include "carma/antenna/sza/antenna/corba/Proxy.h"
#include "carma/antenna/sza/antenna/corba/SzaShareCorba.h"

#include "carma/antenna/sza/antenna/control/AntennaMaster.h"
#include "carma/szautil/OffsetMsg.h"

using namespace std;
using namespace sza::antenna::corba;
using namespace sza::antenna::control;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
Proxy::Proxy(sza::antenna::control::AntennaMaster* parent) 
{
  parent_ = 0;
  share_  = 0;

  // Sanity check

  if(parent == 0)
    ThrowError("Null parent received.\n");

  parent_  = parent;
  seq_     = 0;
  share_   = (SzaShareCorba*)parent_->getShare();
}

/**.......................................................................
 * Destructor.
 */
Proxy::~Proxy() {}

unsigned long Proxy::sequenceNumber()
{
  unsigned long seq;

  seqGuard_.lock();
  seq = ++seq_;
  seqGuard_.unlock();

  return seq;
}

/**.......................................................................
 * Base-class function to set the sky offsets (since it is used by
 * different inheritors)
 */
void Proxy::setSkyOffsets(unsigned long seq)
{
  AntennaMasterMsg msg;
  OffsetMsg offset;

  Angle x = share_->getTotalXCollimationOffset();
  Angle y = share_->getTotalYCollimationOffset();

  COUT("Packing sky offsets: x = " << x << " y = " << y);

  unsigned axisMask = (unsigned)OffsetMsg::X | (unsigned)OffsetMsg::Y;
  offset.packSkyOffsets(OffsetMsg::SET, (OffsetMsg::Axis)axisMask, 
			x.radians(), y.radians());

  msg.getDriveMsg()->getTrackerMsg()->packOffsetMsg(sequenceNumber(), offset);

  if(seq==0)
    msg.getDriveMsg()->getTrackerMsg()->setCarmaSequenceNumber();
  else
    msg.getDriveMsg()->getTrackerMsg()->setCarmaDriveSequenceNumber(seq);

  parent_->fwdTaskMsg(&msg);
}

/**.......................................................................
 * Set the flexure for the currently selected receiver
 */
void Proxy::setFlexure() 
{
  Angle flexureSin;
  Angle flexureCos;

  // Query the correct flexure terms for the current receiver

  share_->getFlexure(flexureSin, flexureCos);

  // Now assert the flexure terms in the mount model

  setFlexure(sza::util::PointingMode::RADIO, flexureSin, flexureCos);
}

void Proxy::setFlexure(sza::util::PointingMode::Type model, sza::util::Angle& sFlex, sza::util::Angle& cFlex)
{
  COUT("DEBUG Inside setFlexure: model = " << model << " sFlex = " << sFlex << " cFlex = " << cFlex);

  AntennaMasterMsg msg;
  msg.getDriveMsg()->getTrackerMsg()->packFlexureMsg(sequenceNumber(),
						     model,
						     sFlex.radians(),
						     cFlex.radians());
  msg.getDriveMsg()->getTrackerMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}
