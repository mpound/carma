#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/IFProxy.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;

/**.......................................................................
 * Constructor with a pointer to the parent AntennaMaster
 */
IFProxy::IFProxy(AntennaMaster* parent) : Proxy(parent) {}

/**.......................................................................
 * Destructor function
 */
IFProxy::~IFProxy() {}

void IFProxy::selectRx()
{
  std::cout << "IF: selectRx() stub" << std::endl;
};

void IFProxy::selectBand(unsigned short pos)
{
  COUT("IF: selectBand() called with switch pos = " << pos);

  AntennaMasterMsg msg;

  msg.getRxMsg()->packIFModMsg(sza::array::IFMOD_POSITION_SWITCH,
			       Rx::switchPosToRx((unsigned char)pos),
			       0.0,
			       sza::util::IFAtten::ATTEN_TOTAL,
			       0.0,
			       0.0,
			       0.0,
			       sequenceNumber(),
			       sza::util::CalPos::NONE);

  msg.getRxMsg()->setCarmaSequenceNumber();

  parent_->fwdTaskMsg(&msg);
};

/**.......................................................................
 * Pack a message to set the IF attenuation
 */
void IFProxy::setAtten(float atten)
{
  COUT("IF: setAtten() called with atten = " << atten);

  AntennaMasterMsg msg;

  msg.getRxMsg()->packIFModMsg(sza::array::IFMOD_SET_ATTEN,
			       sza::util::Rx::RXNONE,
			       0.0,
			       sza::util::IFAtten::ATTEN_TOTAL,
			       atten,
			       0.0,
			       0.0,
			       sequenceNumber(),
			       sza::util::CalPos::NONE);

  msg.getRxMsg()->setCarmaSequenceNumber();

  parent_->fwdTaskMsg(&msg);
};

/**.......................................................................
 * Pack a message to set the IF power
 */
void IFProxy::setPower(float power)
{
  COUT("IF: setPower() called with power = " << power);

  AntennaMasterMsg msg;

  msg.getRxMsg()->packIFModMsg(sza::array::IFMOD_SET_LEVEL,
			       sza::util::Rx::RXNONE,
			       power,
			       sza::util::IFAtten::ATTEN_TOTAL,
			       0.0,
			       0.0,
			       0.0,
			       sequenceNumber(),
			       sza::util::CalPos::NONE);

  msg.getRxMsg()->setCarmaSequenceNumber();

  parent_->fwdTaskMsg(&msg);
};

void IFProxy::setPresetPower()
{
  std::cout << "IF: setPresetPower() stub" << std::endl;
};

void IFProxy::reset()
{
  std::cout << "IF: reset() stub" << std::endl;
};

void IFProxy::setDefaultAtten(sza::util::Attenuation& atten, sza::util::Rx::Id rxId, sza::util::CalPos::Pos pos)
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packIFModMsg(sza::array::IFMOD_SET_DEFAULT_ATTEN,
			       rxId,
			       0.0,
			       sza::util::IFAtten::ATTEN_TOTAL,
			       atten.dB(),
			       0,
			       0,
			       sequenceNumber(),
			       pos);
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);

}
