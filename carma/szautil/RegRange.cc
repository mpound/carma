#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/RegRange.h"

using namespace sza::util;
using namespace std;

/**.......................................................................
 * Constructor.
 */
RegRange::RegRange(unsigned iByteStart, unsigned iByteStop, 
		   bool archivedOnly, ArrayMap* arrayMap)
{
  if(arrayMap != 0)
    arrayMap_ = alias_ArrayMap(arrayMap);
  else
    arrayMap_ = arrayMapBase_.arrayMap();
    
  iByteStart_   =  iByteStart;
  iByteStop_    =  iByteStop;
  iByteCurrent_ =  iByteStart_;
  archivedOnly_ =  archivedOnly;
  iEl_          = -1;
  
  if(iByteStop < iByteStart)
    ThrowError("Range doesn't make sense: " 
	       << iByteStart << "-" << iByteStop);
  
  // And reset to the start
  
  reset();
}

/**.......................................................................
 * Reset internal iterators to correspond to the starting byte of this
 * range.
 */
void RegRange::reset() 
{
  iByteCurrent_ = iByteStart_;
  
  // Iterate through the array map, looking for the register element
  // corresponding to the start byte of this range.
  
  // First we need to find the register map containing the start byte
  
  for(iregmap_ = arrayMap_->regmaps.begin(); 
      iregmap_ != arrayMap_->regmaps.end(); iregmap_++) {
    
    int byteOffsetOfRegMapInArrayMap = 
      (*iregmap_)->byteOffsetInArrayMap(archivedOnly_);
    
    // If this register map is not present in the array map, skip it
    
    if(byteOffsetOfRegMapInArrayMap < 0)
      continue;
    
    if(iByteStart_ >= byteOffsetOfRegMapInArrayMap &&
       iByteStart_ < byteOffsetOfRegMapInArrayMap + 
       (*iregmap_)->nByte(archivedOnly_)) {
      
      RegMap* regmap = (*iregmap_)->regmap;
      
      // Find the board containing the start byte 
      
      for(iboard_=regmap->boards_.begin(); iboard_ != regmap->boards_.end(); 
	  iboard_++) {
	
	int byteOffsetOfBoardInRegMap = 
	  (*iboard_)->byteOffsetInRegMap(archivedOnly_);
	
	// If this board is not present in the register map, skip it
	
	if(byteOffsetOfBoardInRegMap < 0)
	  continue;
	
	int byteOffsetOfBoardInArrayMap = 
	  byteOffsetOfRegMapInArrayMap + byteOffsetOfBoardInRegMap;
	
	if(iByteStart_ >= byteOffsetOfBoardInArrayMap &&
	   iByteStart_ < byteOffsetOfBoardInArrayMap + 
	   (*iboard_)->nByte(archivedOnly_)) {
	  
	  // Find the block containing the start byte
	  
	  for(iblock_=(*iboard_)->blocks.begin(); 
	      iblock_ != (*iboard_)->blocks.end(); iblock_++) {
	    
	    // Find the block containing the start byte
	    
	    int byteOffsetOfBlockInRegMap = 
	      (*iblock_)->byteOffsetInRegMapOf(archivedOnly_);
	    
	    // If this block is not present in the register map, skip it
	    
	    if(byteOffsetOfBlockInRegMap < 0)
	      continue;
	    
	    int byteOffsetOfBlockInArrayMap = 
	      byteOffsetOfRegMapInArrayMap + byteOffsetOfBlockInRegMap;
	    
	    if(iByteStart_ >= byteOffsetOfBlockInArrayMap &&
	       iByteStart_ < byteOffsetOfBlockInArrayMap + 
	       (*iblock_)->nByte()) {
	      
	      // Find the element corresponding to the start byte
	      
	      unsigned bytesIntoRegister =  
		(iByteStart_ - byteOffsetOfBlockInArrayMap);
	      
	      // If the byte offset does not correspond to any
	      // element, throw an error
	      
	      if(bytesIntoRegister % (*iblock_)->nBytePerEl() != 0) 
		ThrowError("Start byte: " << iByteStart_ 
			   << " does not correspond to an integral "
			   << "element boundary");
	      
	      iEl_ = bytesIntoRegister/(*iblock_)->nBytePerEl();

	      // The current slot is the slot offset of the register
	      // map, plus the slot offset of the block in the
	      // register map, plus the number of elements into the
	      // register

	      iSlotCurrent_ = 
		(*iregmap_)->slotOffsetInArrayMap(archivedOnly_) + 
		(*iblock_)->slotOffsetInRegMapOf(archivedOnly_) + iEl_;
	      return;
	    }
	  }
	}
      }
    }
  }
  
  // The fact that we are here means no valid element was found
  
  LogStream errStr;
  errStr.initMessage(true);
  errStr << "No valid register index corresponds to byte offset: " 
	 << iByteStart_ << endl;
  throw Error(errStr);
}

/**.......................................................................
 * Destructor.
 */
RegRange::~RegRange() {}

/**.......................................................................
 * Return true if the current byte is the last byte.
 */
bool RegRange::isEnd() 
{
  return iByteCurrent_ > iByteStop_;
}

/**.......................................................................
 * Prefix increment operator
 */
const RegRange& RegRange::operator++()
{
  // Increment until we find the next register compatible with this
  // array map
  
  do {
    increment();
  } while(!currentRegisterIsValid() && !isEnd());
  
  return *this;
}

/**.......................................................................
 * Increment this iterator.
 */
void RegRange::increment()
{
  // If the last register was present in this array map, increment
  // the byte counter by the number of bytes per element of the
  // last block, and increment the slot counter by 1
  
  if(currentRegisterIsValid()) {
    iByteCurrent_ += (*iblock_)->nBytePerEl();
    iSlotCurrent_++;
  }

  // If we are done, just return silently
  
  if(!isEnd()) {
    
    // If we just did the last element of this block, increment to the
    // next block.
    
    if(iEl_ == (*iblock_)->nEl()-1) {
      
      // If we are on the last block of this board, increment to the
      // next board
      
      if(iblock_ == (*iboard_)->blocks.end()-1) {
	
	// If we are on the last board of this register map, increment
	// to the next register map
	
	if(iboard_ == (*iregmap_)->regmap->boards_.end()-1) {
	  
	  // If we are on the last register map of the array map,
	  // something's wrong!
	  
	  if(iregmap_ == arrayMap_->regmaps.end()) {
	    ThrowError("Prematurely reached the end of the"
		       << " register map array.\n");
	  } else { // Else increment to the next register map
	    iregmap_++;
	    
	    // And set the board pointing to the first board of this
	    // register map
	    
	    iboard_ = (*iregmap_)->regmap->boards_.begin();
	  }
	  
	} else // Else just increment to the next board of the current map
	  iboard_++;
	
	// Set the block iterator pointing to the first block of this board
	
	iblock_ = (*iboard_)->blocks.begin();
	
      } else  // Else just increment to the next block of the current board
	iblock_++;
      
      // And set the element iterator pointing to the first element of
      // this block
      
      iEl_ = 0;
      
    } else // Else just increment to the next element of the current block
      iEl_++;
  }
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
ostream& 
sza::util::operator<<(ostream& os, RegRange& range)
{
  if(range.iEl_ < 0)
    os << "Not initialized";
  else {
    RegMapBlock* blk = *range.iblock_;
    Coord coord = blk->axes_->coordOf(range.iEl_);
    
    os << (*range.iregmap_)->name << "." << (*range.iboard_)->name << "." 
       << blk->name_ << coord;
  }
  
  return os;
}

/**.......................................................................
 * Check the validity of the current element.
 */
void RegRange::checkValidity()
{
  if(iEl_ < 0) 
    ThrowError("Current element is not set");
}

/**.......................................................................
 * Check the validity of the current register
 */
bool RegRange::currentRegisterIsValid()
{
  return archivedOnly_ ? (*iblock_)->isArchived() : true;
}

/**.......................................................................
 * Get a pointer to the current register map
 */
ArrRegMap* RegRange::currentArrRegMap()
{
  checkValidity();
  return (*iregmap_);
}

/**.......................................................................
 * Get a pointer to the current block
 */
RegMapBlock* RegRange::currentBlock()
{
  checkValidity();
  return (*iblock_);
}

/**.......................................................................
 * Get the current element
 */
unsigned RegRange::currentEl()
{
  checkValidity();
  return (unsigned)iEl_;
}

/**.......................................................................
 * Get the current byte Offset
 */
unsigned RegRange::currentByteOffset()
{
  checkValidity();
  return iByteCurrent_;
}

/**.......................................................................
 * Get the current slot index
 */
int RegRange::currentSlot()
{
  checkValidity();
  return iSlotCurrent_;
}

