#include <stdlib.h>
#include <iostream>
#include <string>

#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/CalibratorProxy.h"

#include "carma/szautil/Exception.h"

#include "carma/util/ErrorException.h"

using namespace carma::antenna::common;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;

/**.......................................................................
 * Set the Calibrator to the requested position
 */
void CalibratorProxy::
setPos(carma::antenna::common::CalibratorControl::Position carmaPos,
       const CORBA::ULong seq)
{
  COUT("Got a setPos command with carmaPos = " << carmaPos << " seq = " << seq);

  // First convert and check the requested position

  sza::util::CalPos::Pos szaPos = carmaCalPosToSzaCalPos(carmaPos);

  if(szaPos == sza::util::CalPos::NONE) {
    std::ostringstream os;

    os << "pos = " << carmaPos << " is an invalid position for SZA";

    throw CARMA_EXCEPTION(carma::util::UserException, os.str().c_str());
  }

  // Now send the message

  AntennaMasterMsg msg;
  msg.getRxMsg()->packCalTertMsg(sza::array::CALTERT_POSITION_CAL,
				 sza::util::Rx::RXNONE, // Not used
				 0, // Not used
				 szaPos,
				 false, // Not used
				 CalTertTypes::DEV_NONE, // Not used
				 CalTertTypes::CMD_NONE, // Not used
				 sequenceNumber());

  msg.getRxMsg()->setCarmaCalSequenceNumber(seq);
  parent_->fwdTaskMsg(&msg);
};

/**.......................................................................
 * Home tertiary
 */
void CalibratorProxy::
homeTertiary()
{
  COUT("Got a home tertiary command");

  // Now send the message

  AntennaMasterMsg msg;
  msg.getRxMsg()->packCalTertMsg(sza::array::CALTERT_HOME_TERT,
				 sza::util::Rx::RXNONE,   // Not used
				 0,                       // Not used
				 sza::util::CalPos::NONE, // Not used
				 false,                   // Not used
				 CalTertTypes::DEV_NONE,  // Not used
				 CalTertTypes::CMD_NONE,  // Not used
				 sequenceNumber());

  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

/**.......................................................................
 * Position the tertiary by angle
 */
void CalibratorProxy::
positionTertiaryAngle(double angleDegrees)
{
  COUT("Got a positionTertiaryAngle command with angle = " << angleDegrees);

  // Now send the message

  AntennaMasterMsg msg;
  msg.getRxMsg()->packNewCalTertMsg(sza::array::CALTERT_NEW_TERT_POS_ANGLE,
				    sza::util::CalPos::NONE, // Not used
				    sza::util::Rx::RXNONE,   // Not used
				    angleDegrees,
				    sequenceNumber());

  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

/**.......................................................................
 * Position the tertiary by receiver mark
 */
void CalibratorProxy::
positionTertiaryRx(carma::antenna::common::RxControl::Type rx)
{
  COUT("Got a positionTertiaryRx command with rx = " << rx);

  sza::util::Rx::Id rxId = carmaRxToSzaRxId(rx);

  // Now send the message

  AntennaMasterMsg msg;
  msg.getRxMsg()->packNewCalTertMsg(sza::array::CALTERT_NEW_TERT_POS_RX,
				    sza::util::CalPos::NONE, // Not used
				    rxId,
				    0.0,                     // Not used
				    sequenceNumber());

  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

/**.......................................................................
 * Constructor with pointer to the parent AntennaMaster
 */
CalibratorProxy::CalibratorProxy(AntennaMaster* parent) : Proxy(parent) {};

CalibratorProxy::~CalibratorProxy() {};

sza::util::CalPos::Pos CalibratorProxy::carmaCalPosToSzaCalPos(carma::antenna::common::CalibratorControl::Position pos)
{
  switch (pos) {
  case carma::antenna::common::CalibratorControl::SKY:
    return sza::util::CalPos::SKY;
    break;
  case carma::antenna::common::CalibratorControl::AMBIENT:
    return sza::util::CalPos::AMBIENT;
    break;
  default:
    return sza::util::CalPos::NONE;
    break;
  }
}

sza::util::Rx::Id CalibratorProxy::carmaRxToSzaRxId(carma::antenna::common::RxControl::Type type)
{
  switch(type) {
  case carma::antenna::common::RxControl::RX1CM:
    return sza::util::Rx::RX1CM;
    break;
  case carma::antenna::common::RxControl::RX3MM:
    return sza::util::Rx::RX3MM;
    break;
  case carma::antenna::common::RxControl::RX1MM:
    return sza::util::Rx::RX1MM;
    break;
  default:
    ThrowError("Invalid receiver type: " << type);
    break;
  }
}
