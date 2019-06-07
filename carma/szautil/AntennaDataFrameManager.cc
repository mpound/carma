#include "carma/szautil/Debug.h"
#include "carma/szautil/Directives.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/RegDate.h"
#include "carma/szautil/TimeVal.h"

#include "carma/szautil/AntennaDataFrameManager.h"
#include "carma/szautil/AntennaDataFrameNormal.h"

#include "carma/szaarrayutils/scanner.h"

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor with no resizing of the initially zero-length DataFrame
 * buffer. This constructor doesn't initialize the antenna number
 * associated with this manager object.
 */
void AntennaDataFrameManager::initialize(bool archivedOnly) 
{
  if((regMap_ = new_SzaAntRegMap())==0)
    throw Error("AntennaDataFrameManager::AntennaDataFrameManager: "
		" Unable to allocate regMap_.\n");

  antFrame_ = 0;

  // Set the flag if this frame only contains archived registers

  DBPRINT(true, Debug::DEBUG10, "archivedOnly = " << archivedOnly);

  archivedOnly_ = archivedOnly;

  // If we are only recording archived registers, frame size is the
  // size of the archived register map

  unsigned frameSize = SCAN_BUFF_BYTE_SIZE(regMap_->nByte(archivedOnly));

  DBPRINT(true, Debug::DEBUG10, "byteSize = " << frameSize);

  frame_ = new AntennaDataFrameNormal(frameSize);

  // Initialize the nBuffer variable

  nBuffer_ = frameSize;

  // And recast the base-class pointer to the correct type of frame
  // for this manager.

  antFrame_ = dynamic_cast<AntennaDataFrame*>(frame_);
}

/**.......................................................................
 * This constructor doesn't intialize the antenna number
 * associated with this manager object.
 */
AntennaDataFrameManager::AntennaDataFrameManager(bool archivedOnly)
{
  initialize(archivedOnly);
  DBPRINT(true, Debug::DEBUG10, "archivedOnly = " << archivedOnly);
}

/**.......................................................................
 * This constructor does intialize the antenna number associated with
 * this manager object.
 */
AntennaDataFrameManager::
AntennaDataFrameManager(sza::util::AntNum* antNum, bool archivedOnly)
{
  initialize(archivedOnly);
  antFrame_->setAnt(antNum->getId());
}

/**.......................................................................
 * This constructor does intialize the antenna number associated with
 * this manager object.
 */
AntennaDataFrameManager::
AntennaDataFrameManager(const sza::util::AntNum& antNum, bool archivedOnly)
{
  initialize(archivedOnly);
  antFrame_->setAnt(antNum);
}

/**.......................................................................
 * Copy constructor
 */
AntennaDataFrameManager::AntennaDataFrameManager(AntennaDataFrameManager& fm)
{
  initialize(fm.archivedOnly_);
  operator=(fm);
}

/**.......................................................................
 * Destructor.
 */
AntennaDataFrameManager::~AntennaDataFrameManager() 
{
  if(regMap_ != 0) {
    regMap_ = del_SzaAntRegMap(regMap_);
    regMap_ = 0;
  }
}

/**.......................................................................
 * Set the antenna number associated with this dataframe.
 * @throws Exception
 */
void AntennaDataFrameManager::setAnt(AntNum::Id antennaId)
{
  antFrame_->setAnt(antennaId);
}

/**.......................................................................
 * Return the antenna number associated with this dataframe.
 * @throws Exception
 */
AntNum AntennaDataFrameManager::getAnt()
{
  return antFrame_->getAnt();
}

/**.......................................................................
 * Return the antenna number associated with this dataframe.
 * @throws Exception
 */
unsigned AntennaDataFrameManager::getAntIntId()
{
  return antFrame_->getAntIntId();
}

/**.......................................................................
 * Overload the base-class assignment operator for this type
 */
void AntennaDataFrameManager::operator=(RegMapDataFrameManager& fm)
{
  operator=((AntennaDataFrameManager&) fm);
}

/**.......................................................................
 * Define an assignment operator for this type
 */
void AntennaDataFrameManager::operator=(AntennaDataFrameManager& fm)
{
  // Assign members specific to the antenna frame
  
  lock();
  fm.lock();

  try {

    *antFrame_ = *fm.antFrame_;

    // Unlock the frames if an error was thrown

  } catch (const Exception& err) {

    unlock();
    fm.unlock();

    throw(err);
  }

  // And call the base-class assignment operator
  
  RegMapDataFrameManager::operator=((RegMapDataFrameManager&) fm);
}

