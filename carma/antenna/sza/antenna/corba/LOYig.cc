#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/LOYig.h"

#include "carma/szautil/Debug.h"
#include "carma/szautil/LoBand.h"
#include "carma/szautil/LoStage.h"

#include "carma/szaarrayutils/rtcnetcoms.h"

using namespace std;
using namespace carma::antenna::common;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;

LOYig::LOYig(AntennaMaster* parent)
{
  parent_ = parent;
}

/**.......................................................................
 * Destructor.
 */
LOYig::~LOYig() {};

/**.......................................................................
 * Toggle the sweep
 */
void LOYig::toggleSweep(bool on)
{
  AntennaMasterMsg msg;

  msg.getRxMsg()->packLoMsg(sza::array::LO_TOGGLE, LoOsc::YIG, LoStage::LO_SWEEP, Rx::RXALL,
			    true,
			    0, 0, 0, 0,
			    0, 0, 0, 0, 0,
			    0.0,
			    0, 0, 0, 0);
  DBPRINT(true, Debug::DEBUG8, "Inside toggleSweep");
  parent_->fwdTaskMsg(&msg);
}

/**.......................................................................
 * Set the LO frequency
 */
void LOYig::setLoFrequency(double GHz)
{
  AntennaMasterMsg msg;
  Frequency freq;
  freq.setGHz(GHz);

  msg.getRxMsg()->packLoMsg(sza::array::LO_LO_FREQ, LoOsc::YIG, LoStage::LO_ALL, Rx::RXALL,
			    true,
			    0, 0, 0, 0,
			    0, 0, 0, 0, 0,
			    freq.yigUnits(),
			    0, 0, 0, 0);
  DBPRINT(true, Debug::DEBUG8, "Inside toggleSweep");
  parent_->fwdTaskMsg(&msg);
}

/**
 * Command to set the frequency.
 */
void LOYig::setFrequency(double yigFreq, double LOfreq)
{
}

