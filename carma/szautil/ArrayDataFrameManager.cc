#include "carma/szautil/ArrayDataFrameManager.h"
#include "carma/szautil/DataFrameNormal.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/TimeVal.h"

#include "carma/szaarrayutils/scanner.h"

using namespace sza::util;
using namespace std;

/**.......................................................................
 * Constructor with no resizing of the initially zero-length DataFrame
 * buffer.  This constructor doesn't intialize the antenna number
 * associated with this manager object.
 */
void ArrayDataFrameManager::initialize(ArrayMap* arrayMap, bool archivedOnly)
{
  unsigned frameSize;

  if(arrayMap == 0) {
    if((arrayMap_ = new_SzaArrayMap())==0)
      throw Error("ArrayDataFrameManager::ArrayDataFrameManager: "
		  " Unable to allocate arrmap_.\n");
  } else
    arrayMap_ = alias_ArrayMap(arrayMap);

  // If we are only recording archived registers, frame size is the
  // size of the archived register map

  frameSize = SCAN_BUFF_BYTE_SIZE(arrayMap_->nByte(archivedOnly));

  //  COUT("Araymap size = " << arrayMap_->nByte(false));
  //  COUT("Araymap size = " << arrayMap_->nByte(true));
  //  COUT("Frame size   = " << frameSize);

  // Make the frame large enough to accomodate the register map

  if(frame_) {
    //    COUT("Resizing " << frame_ << " to frameSize = " << frameSize);
    frame_->resize(frameSize);
  } else {
    frame_ = new sza::util::DataFrameNormal(frameSize);
    //    COUT("Allocated new with frameSize = " << frameSize << " frame = " << frame_);
  }

#if 0
  std::vector<unsigned char>& dataVec = frame()->dataVec();
  COUT("Frame data size = " << dataVec.size());
#endif

  // Initialize the nBuffer variable

  nBuffer_ = frameSize;
}

/**.......................................................................
 * This constructor doesn't intialize the antenna number
 * associated with this manager object.
 */
ArrayDataFrameManager::
ArrayDataFrameManager(bool archivedOnly, ArrayMap* arrayMap) :
  ArrayMapDataFrameManager(archivedOnly)
{
  frame_ = 0;
  initialize(arrayMap, archivedOnly);
}

/**.......................................................................
 * Copy constructor
 */
ArrayDataFrameManager::ArrayDataFrameManager(ArrayDataFrameManager& fm) :
  ArrayMapDataFrameManager(fm.archivedOnly_)
{
  initialize(fm.arrayMap_, fm.archivedOnly_);
  operator=(fm);
}

/**.......................................................................
 * Destructor.
 */
ArrayDataFrameManager::~ArrayDataFrameManager() 
{
  if(arrayMap_ != 0) {
    arrayMap_ = del_SzaArrayMap(arrayMap_);
    arrayMap_ = 0;
  }
}

/**.......................................................................
 * Assignment operators.
 */
void ArrayDataFrameManager::operator=(ArrayDataFrameManager& fm)
{
  ArrayMapDataFrameManager::operator=((ArrayMapDataFrameManager&) fm);
}

/**.......................................................................
 * Find the register map corresponding to an antenna data frame
 */
ArrRegMap* ArrayDataFrameManager::findAntennaRegMap(AntennaDataFrameManager& fm)
{
  return arrayMap_->findArrRegMap((char*)fm.getAnt().getAntennaName().c_str());
}

/**.......................................................................
 * Write an antenna data frame into our array frame
 */
void ArrayDataFrameManager::writeAntennaRegMap(AntennaDataFrameManager& fm,
					       bool lockFrame)
{
  ArrRegMap* aregmap = findAntennaRegMap(fm);

  if(aregmap==0) 
    ThrowError("No register map: " << fm.getAnt().getAntennaName() 
	       << " found in array map");

  // Call the base-class method to write the register map

  writeRegMap(aregmap, fm, lockFrame);
}

