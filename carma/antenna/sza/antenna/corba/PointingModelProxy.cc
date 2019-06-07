#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/PointingModelProxy.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;

#define CHECK_SEQ() \
  {\
    LogStream errStr;\
    if(seq_ < 0) {\
      errStr.appendMessage(true, "Sequence number is uninitialized");\
      throw Error(errStr);\
    }\
  }

#define RESET_SEQ() seq_ = -1

#define SEQ (unsigned int) seq_

/**.......................................................................
 * Constructor with a pointer to the parent AntennaMaster
 */
PointingModelProxy::PointingModelProxy(AntennaMaster* parent)
{
  seq_    = -1;
  parent_ =  parent;
};

/**.......................................................................
 * Destructor.
 */
PointingModelProxy::~PointingModelProxy() {};

/**.......................................................................
 * Set the value of the sequence number to be associated with the next
 * command.
 */
void PointingModelProxy::scheduleNextSequenceNo(unsigned long seq)
{
  seq_ = (long)seq;
}

/**.......................................................................
 * Set up azimuth tilts.
 */
void PointingModelProxy::setTilts(double azTilt1, double azTilt2,
				  double elTilt1, double elTilt2)
{
  AntennaMasterMsg msg;

  CHECK_SEQ();
  msg.getDriveMsg()->getTrackerMsg()->
    packTiltsMsg(SEQ, azTilt1, azTilt2, elTilt1);
  RESET_SEQ();

  parent_->fwdTaskMsg(&msg);
}

/**.......................................................................
 * Set the flexure
 */
void PointingModelProxy::setFlexure(carma::antenna::common::
				    DriveControl::Aperture model,
				    double flexure)
{
  AntennaMasterMsg msg;

  PointingMode::Type mode =
    (model==carma::antenna::common::DriveControl::OPTICAL) ?
    PointingMode::OPTICAL : PointingMode::RADIO;

  CHECK_SEQ();
  msg.getDriveMsg()->getTrackerMsg()->packFlexureMsg(SEQ, mode, flexure, 0.0);
  RESET_SEQ();

  parent_->fwdTaskMsg(&msg);
}

/**.......................................................................
 * Set the collimation
 */
void PointingModelProxy::setCollimation(carma::antenna::common::
					DriveControl::Aperture model,
					double magnitude, double direction)
{
  AntennaMasterMsg msg;

  PointingMode::Type mode =
    (model==carma::antenna::common::DriveControl::OPTICAL) ?
    PointingMode::OPTICAL : PointingMode::RADIO;

  CHECK_SEQ();
  msg.getDriveMsg()->getTrackerMsg()->packCollimationMsg(SEQ, mode,
							 magnitude, direction,
							 sza::util::OffsetMsg::SET);
  RESET_SEQ();

  parent_->fwdTaskMsg(&msg);
}


