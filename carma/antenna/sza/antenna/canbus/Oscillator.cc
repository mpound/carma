#include <sstream>

#include "carma/canbus/Utilities.h"

#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

#include "carma/antenna/sza/antenna/canbus/Oscillator.h"

using namespace std;
using namespace sza::antenna::canbus;
using namespace carma::canbus;

/**.......................................................................
 * Constructor.
 */
Oscillator::Oscillator(sza::antenna::control::SzaShare* share, 
		       string boardName,
		       carma::canbus::apiType api,
		       carma::canbus::nodeType node, 
		       carma::canbus::CanOutput& io) :
  CanDevice(share, boardName, api, node, io) 
{
  // All oscillators will have these monitor points
  
  monitor_.addMonitorPoint("boardTemperature");
  monitor_.addMonitorPoint("errorVoltage");
  monitor_.addMonitorPoint("ifLevel");
  monitor_.addMonitorPoint("maxChnl");
  monitor_.addMonitorPoint("noiseMeterVoltage");
}

/**.......................................................................
 * Destructor.
 */
Oscillator::~Oscillator() {}

