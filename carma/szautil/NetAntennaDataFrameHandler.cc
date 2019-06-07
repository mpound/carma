#include "carma/szautil/NetAntennaDataFrameHandler.h"

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
NetAntennaDataFrameHandler::NetAntennaDataFrameHandler() 
{
  // Install the data frame manager's buffer as the read buffer.

  nrs_->setBuffer(frame_.frame()->data(), frame_.frame()->nByte());
}

/**.......................................................................
 * Destructor.
 */
NetAntennaDataFrameHandler::~NetAntennaDataFrameHandler() {}

/**.......................................................................
 * Set the antenna number associated with this dataframe.
 * @throws Exception
 */
void NetAntennaDataFrameHandler::setAnt(AntNum::Id antennaId)
{
  frame_.setAnt(antennaId);
}

/**.......................................................................
 * Return the antenna number associated with this dataframe.
 * @throws Exception
 */
unsigned int NetAntennaDataFrameHandler::getAnt()
{
  return frame_.getAntIntId();
}

/**.......................................................................
 * Return a pointer to our frame manager
 */
AntennaDataFrameManager* NetAntennaDataFrameHandler::getFrame()
{
  return &frame_;
}
