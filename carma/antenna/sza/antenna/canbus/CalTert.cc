#include "carma/canbus/Utilities.h"

#include "carma/szautil/CalTertFlags.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

#include "carma/antenna/sza/antenna/canbus/CalTert.h"
#include "carma/antenna/sza/antenna/control/AntennaRx.h"

using namespace std;
using namespace carma::canbus;
using namespace sza::antenna::canbus;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
CalTert::CalTert(sza::antenna::control::AntennaRx* parent,
		 sza::antenna::control::SzaShare* share, 
		 std::string boardName,
		 carma::canbus::nodeType node, 
		 carma::canbus::CanOutput& io) :
  CanDevice(share, boardName, sza::util::CanModule::caltertApiNo_, node, io) 
{
  parent_  = parent;
}

/**.......................................................................
 * Destructor.
 */
CalTert::~CalTert() {};

/**.......................................................................
 * Interface with a 1-wire device
 */
std::vector<carma::canbus::Message>
CalTert::oneWireInterface(CalTertTypes::OwDevice device, 
			  CalTertTypes::OwCommand command, bool send)
{
  vector<carma::canbus::Message> msgs;
  return msgs;
}

/**.......................................................................
 * Request a calibrator position
 */
std::vector<carma::canbus::Message>
CalTert::positionCalibrator(sza::util::CalPos::Pos position, bool send)
{
  vector<carma::canbus::Message> msgs;
  return msgs;
}

/**.......................................................................
 * Request a calibrator position
 */
std::vector<carma::canbus::Message>
CalTert::positionCalibrator(sza::util::CalPos::Pos position, unsigned seq, bool send)
{
  vector<carma::canbus::Message> msgs;
  return msgs;
}

/**.......................................................................
 * Home the tertiary
 */
std::vector<carma::canbus::Message>
CalTert::homeTertiary(bool send)
{
  vector<carma::canbus::Message> msgs;
  return msgs;
}

/**.......................................................................
 * Move the tertiary
 */
std::vector<carma::canbus::Message>
CalTert::positionTertiary(unsigned short position, bool send)
{
  vector<carma::canbus::Message> msgs;
  return msgs;
}

std::vector<carma::canbus::Message>
CalTert::positionTertiary(sza::util::Angle position, unsigned seq, bool send)
{
  vector<carma::canbus::Message> msgs;
  return msgs;
}

/**.......................................................................
 * Move the tertiary
 */
std::vector<carma::canbus::Message>
CalTert::positionTertiary(sza::util::Rx::Id id, bool send)
{
  vector<carma::canbus::Message> msgs;
  return msgs;
}

std::vector<carma::canbus::Message>
CalTert::positionTertiary(sza::util::Rx::Id id, unsigned seq, bool send)
{
  vector<carma::canbus::Message> msgs;
  return msgs;
}

/**.......................................................................
 * Enable the tertiary
 */
std::vector<carma::canbus::Message>
CalTert::enableTertiary(bool enable, bool send)
{
  vector<carma::canbus::Message> msgs;
  return msgs;
}

/**.......................................................................
 * Reset the stepper driver
 */
std::vector<carma::canbus::Message>
CalTert::resetStepper(bool send)
{
  vector<carma::canbus::Message> msgs;
  return msgs;
}

/**.......................................................................
 * Write the current encoder position to one of the positions
 * indexed as 30GHz, 90GHz or 230GHz
 */
std::vector<carma::canbus::Message>
CalTert::indexCurrentEncoderPosition(sza::util::Rx::Id id, bool send)
{
  vector<carma::canbus::Message> msgs;
  return msgs;
}

/**.......................................................................
 * Write the current encoder position to one of the positions
 * indexed as 30GHz, 90GHz or 230GHz
 */
std::vector<carma::canbus::Message>
CalTert::setEncoderPositionIndex(sza::util::Rx::Id id,
				 unsigned short position, bool send)
{
  vector<carma::canbus::Message> msgs;
  return msgs;
}

/**.......................................................................
 * Write the current encoder position to one of the positions
 * indexed as 30GHz, 90GHz or 230GHz
 */
std::vector<carma::canbus::Message>
CalTert::setDefaultEncoderPositionIndex(sza::util::Rx::Id id, bool send)
{
  vector<carma::canbus::Message> msgs;
  return msgs;
}

void CalTert::registerRequest(unsigned seq)
{
}

void CalTert::storeEncoderPositionIndex(sza::util::Rx::Id id, 
					unsigned short index)
{
}
