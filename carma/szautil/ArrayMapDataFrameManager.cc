#include "carma/szautil/ArrayMapDataFrameManager.h"
#include "carma/szautil/AxisRange.h"
#include "carma/szautil/CoordRange.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/RegDescription.h"

#include <cstring>

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
ArrayMapDataFrameManager::ArrayMapDataFrameManager(bool archivedOnly) 
{
  arrayMap_ = 0;
  
  archivedOnly_ = archivedOnly;
}

/**.......................................................................
 * Destructor.
 */
ArrayMapDataFrameManager::~ArrayMapDataFrameManager() {}

/**.......................................................................
 * Overloaded assignment operator from the base class
 */
void ArrayMapDataFrameManager::operator=(DataFrameManager& fm)
{
  // Just call our assignment operator
  
  operator=((ArrayMapDataFrameManager&) fm);
}

/**.......................................................................
 * Assignment operator
 */
void ArrayMapDataFrameManager::operator=(ArrayMapDataFrameManager& fm)
{
  // If both frames have the same internal specifications, the copy is
  // trivial
  
  if(fm.archivedOnly_ == archivedOnly_) {
    
    // Check that these frames are the same size
    
    if(fm.frame_->nByte() == frame_->nByte()) {
      
      // If they are, just call the base-class assignment operator
      
      DataFrameManager::operator=((DataFrameManager&) fm);
      
    } else {
      LogStream errStr;
      errStr.appendMessage(true, 
			   "Frames are not from equivalent array maps.\n");
      throw Error(errStr);
    }
    
    // Else we have to iterate over registers, checking which ones
    // from the source frame should be copied to the destination frame
    
  } else {
    
    // Lock the frames before copying so that no-one else can modify
    // source or destination
    
    lock();
    fm.lock();
    
    try {
      
      // Iterate over all register maps 
      
      for(unsigned int iregmap=0; iregmap < arrayMap_->nregmap; iregmap++) {
	
	ArrRegMap* aregmap = arrayMap_->regmaps[iregmap];
	RegMap* regmap = aregmap->regmap;
	
	if(regMapIsPresent(regmap)) {
	  
	  // Iterate over all boards in this register map.
	  
	  for(unsigned int iboard=0; iboard < regmap->nboard_; iboard++) {
	    RegMapBoard* brd = regmap->boards_[iboard];
	    
	    // Only copy if this board is present in both source and
	    // destination register maps.
	    
	    if(boardIsPresent(brd) && fm.boardIsPresent(brd)) {
	      
	      // If the board is marked as reachable, attempt to copy the
	      // contents of the board's registers into the frame buffer.
	      
	      if(!boardIsFlagged(aregmap, brd)) {
		
		// Iterate over all register blocks of this board
		
		for(unsigned int iblock=0; iblock < brd->nblock; iblock++) {
		  RegMapBlock* blk = brd->blocks[iblock];
		  
		  // If this block is present in the register maps of both
		  // frames, copy it
		  
		  if(blockIsPresent(blk) && fm.blockIsPresent(blk)) {
		    
		    unsigned iStartDest = byteOffsetInFrameOf(aregmap, blk);
		    unsigned iStartSrc  = fm.byteOffsetInFrameOf(aregmap, blk);
		    
		    // Pack the frame without locking, since the
		    // frames will already be locked
		    
		    frame_->pack(fm.frame_->getPtr(iStartSrc, DataType::UCHAR), 
				 blk->nByte(), DataType::UCHAR, 
				 iStartDest, false);
		    
		  }; // End if(blockIsPresent(blk) && fm.blockIsPresent(blk))
		}; // End iteration over blocks
	      }; // End if !boardIsFlagged(brd)
	    }; // End if boardIsPresent(brd)...
	  }; // End iteration over boards
	}; // End if(regMapIsPresent(regmap))
      }; // End iteration over regmaps
      
      // If an exception was thrown, unlock the frames, and rethrow it
      
    } catch(const Exception& err) {
      
      unlock();
      fm.lock();
      
      throw(err);
    } catch(...) {
      unlock();
      fm.lock();
    }

    // And reset the running average counter

    frame_->resetRunningAvgCounter();

  } // End else {
}

/**.......................................................................
 * Increment operator
 */
ArrayMapDataFrameManager& ArrayMapDataFrameManager::operator+=(ArrayMapDataFrameManager& fm)
{
  // Lock the frames before copying so that no-one else can modify
  // source or destination
  
  lock();
  fm.lock();
  
  try {
    
    // Iterate over all register maps 
    
    for(unsigned int iregmap=0; iregmap < arrayMap_->nregmap; iregmap++) {
      
      ArrRegMap* aregmap = arrayMap_->regmaps[iregmap];
      RegMap* regmap     = aregmap->regmap;

      if(regMapIsPresent(regmap)) {
	
	// Iterate over all boards in this register map.
	
	for(unsigned int iboard=0; iboard < regmap->nboard_; iboard++) {
	  RegMapBoard* brd = regmap->boards_[iboard];
	  
	  // Only increment if this board is present in both source and
	  // destination register maps.
	  
	  if(boardIsPresent(brd) && fm.boardIsPresent(brd)) {
	    
	    // If the board is marked as reachable, attempt to copy the
	    // contents of the board's registers into the frame buffer.
	    
	    if(!boardIsFlagged(aregmap, brd)) {

	      // Iterate over all register blocks of this board
	      
	      for(unsigned int iblock=0; iblock < brd->nblock; iblock++) {
		RegMapBlock* blk = brd->blocks[iblock];
		
		// If this block is present in the register maps of both
		// frames, sum them
		
		if(blockIsPresent(blk) && fm.blockIsPresent(blk)) {

		  unsigned iStartDest =    byteOffsetInFrameOf(aregmap, blk);
		  unsigned iStartSrc  = fm.byteOffsetInFrameOf(aregmap, blk);
		  
		  if(strcmp(aregmap->name, "Alarm")==0) {
		    //		    COUT("Block array.frame.features: " 
		    // << "summed - " << blk->isSummed()
		    // << "postAveraged - " << blk->isPostAveraged()
		    // << "preAveraged - " << blk->isPreAveraged()
		    // << "isunioned - " << blk->isUnioned());
		  }

		  // Pack the frame without locking, since the
		  // frames will already be locked
		  
		  // If this block is summed, add it

		  if(blk->isSummed() || blk->isPostAveraged())
		    frame_->addSum(fm.frame_->getPtr(iStartSrc, DataType::typeOf(blk)),
				   blk->nEl(), DataType::typeOf(blk), iStartDest, false);
		  

		  else if(blk->isPreAveraged())
		    frame_->addRunningAverage(fm.frame_->getPtr(iStartSrc, DataType::typeOf(blk)),
					      blk->nEl(), DataType::typeOf(blk), iStartDest, false);

		  // Else if this block is a union, OR it

		  else if(blk->isUnioned())
		    frame_->addUnion(fm.frame_->getPtr(iStartSrc, DataType::typeOf(blk)),
				     blk->nEl(), DataType::typeOf(blk), iStartDest, false);

		  // Else just pack the current value

		  else
		    frame_->pack(fm.frame_->getPtr(iStartSrc, DataType::typeOf(blk)),
				 blk->nEl(), DataType::typeOf(blk), iStartDest, false);

		}; // End if(blockIsPresent(blk) && fm.blockIsPresent(blk))
	      }; // End iteration over blocks
	    }; // End if !boardIsFlagged(brd)
	  }; // End if boardIsPresent(brd)...
	}; // End iteration over boards
      }; // End if(regMapIsPresent(regmap))
    }; // End iteration over regmaps
    
    // Increment our running average counter

    frame_->incrementRunningAvgCounter();

    // If an exception was thrown, unlock the frames, and rethrow it

  } catch(const Exception& err) {
    
    unlock();
    fm.lock();
    
    throw(err);
  } catch(...) {
    unlock();
    fm.lock();
  }

}

//-----------------------------------------------------------------------
// Methods to get the byte offsets in the register map of a named
// regmap, board and block
//-----------------------------------------------------------------------

/**.......................................................................
 * Return the offset in bytes of the data for the requested
 * register map, from the beginning of the frame buffer.
 */
int ArrayMapDataFrameManager::
byteOffsetInFrameOf(ArrRegMap* aregmap)
{
  int regOffset = archivedOnly_ ? aregmap->byteOffsetInArcArrayMap() :
    aregmap->byteOffsetInWholeArrayMap();
  
  if(regOffset < 0)
    return -1;
  
  return byteOffsetInFrameOfData() + regOffset;
}

/**.......................................................................
 * Return the offset in bytes of the data for the requested
 * and register, from the beginning of the frame buffer.
 */
int ArrayMapDataFrameManager::
byteOffsetInFrameOf(ArrRegMap* aregmap, RegMapBlock* blk, Coord* coord)
{
  int regOffset = arrayMap_->byteOffsetOf(archivedOnly_, aregmap, blk, coord);
  
  if(regOffset < 0)
    return -1;

  return byteOffsetInFrameOfData() + regOffset;
}

int ArrayMapDataFrameManager::
byteOffsetInFrameOf(string regmap, string board, string block, Coord* coord)
{
  int regOffset = 
    arrayMap_->byteOffsetOf(archivedOnly_, regmap, board, block, coord);
  
  if(regOffset < 0)
    return -1;
  
  return byteOffsetInFrameOfData() + regOffset;
}

/**.......................................................................
 * Pack data of an arbitrary type into the underlying frame
 */
void ArrayMapDataFrameManager::
packData(ArrRegMap* aregmap, RegMapBlock* blk, void* data, CoordRange* range, 
	 DataType::Type type, bool lockFrame)
{
  // Calculate the byte offset of the start element of this register
  // from the head of the frame.
  
  int byteOffset = byteOffsetInFrameOf(aregmap, blk);
  
  // Do nothing if this register isn't archived
  
  if(byteOffset < 0)
    return;
  
  // If the number of bytes was passed as 0, use the default from the
  // block descriptor
  
  AxisRange axisRange(blk->axes_, range);
  frame_->pack(data, axisRange, type, byteOffset, lockFrame);
}

void ArrayMapDataFrameManager::
packData(string regmap, string board, string block, void* data, 
	 CoordRange* range, DataType::Type type, bool lockFrame)
{
  RegMapBlock* blk   = getReg(regmap, board, block);
  ArrRegMap* aregmap = getArrReg(regmap);
  
  return packData(aregmap, blk, data, range, type, lockFrame);
}

/**.......................................................................
 * Unpack data of an arbitrary type from the underlying frame
 */
void ArrayMapDataFrameManager::
unpackData(ArrRegMap* aregmap, RegMapBlock* blk, void* data, CoordRange* range, 
	   DataType::Type type, bool lockFrame)
{
  CoordRange tmpRange;
  
  // Calculate the byte offset of the start element of this register
  // from the head of the frame.
  
  int byteOffset = byteOffsetInFrameOf(aregmap, blk);
  
  // Do nothing if this register isn't archived
  
  if(byteOffset < 0) {
    LogStream errStr;
    errStr.initMessage(true);
    errStr << "Register " << aregmap->name << "."
	   << blk->brd_->name << "." 
	   << blk->name_ << " is not archived" << endl;
    throw Error(errStr);
  }
  
  // If the number of bytes was passed as 0, use the default from the
  // block descriptor
  
  AxisRange axisRange(blk->axes_, range);
  
  frame_->unpack(data, axisRange, type, byteOffset, lockFrame);
}

void ArrayMapDataFrameManager::
unpackData(string regmap, string board, string block, void* data, 
	   CoordRange* range, DataType::Type type, bool lockFrame)
{
  RegMapBlock* blk = getReg(regmap, board, block);
  ArrRegMap* aregmap = getArrReg(regmap);
  return unpackData(aregmap, blk, data, range, type, lockFrame);
}

/**.......................................................................
 * Get the descriptor for this reg map block
 */
RegMapBlock* ArrayMapDataFrameManager::
findReg(string regmap, string board, string block)
{
  return getReg(regmap, board, block);
}
/**.......................................................................
 * Get the descriptor for this reg map block
 */
RegMapBlock* ArrayMapDataFrameManager::getReg(string regmap, string board, string block)
{
  RegMapBlock* blk=0; 
  LogStream errStr;
  
  // Look up the requested block
  
  blk = arrayMap_->findArrayMapBlock(regmap, board, block, true);
  
  // Check that the block requested was a valid one
  
  if(blk==0) {
    errStr.initMessage(true);
    errStr << "Register " << regmap << "." << board << "." << block 
	   << " not found in the array map" << endl;
    throw Error(errStr);
  }
  
  return blk;
}

/**.......................................................................
 * Get the descriptor for this register map
 */
ArrRegMap* ArrayMapDataFrameManager::getArrReg(string regmap)
{
  return arrayMap_->findArrRegMap(regmap);
}

/**.......................................................................
 * Check the type and element number of a regmap block
 */
void ArrayMapDataFrameManager::
checkType(ArrRegMap* aregmap, RegMapBlock* blk, DataType::Type type,
	  CoordRange* range)
{
  bool match=false;
  LogStream errStr;
  
  switch (type) {
  case DataType::BOOL:
    match = blk->isBool();
    break;
  case DataType::UCHAR:
    match = blk->isUchar();
    break;
  case DataType::CHAR:
    match = blk->isChar();
    break;
  case DataType::USHORT:
    match = blk->isUshort();
    break;
  case DataType::SHORT:
    match = blk->isShort();
    break;
  case DataType::UINT:
    match = blk->isUint();
    break;
  case DataType::INT:
    match = blk->isInt();
    break;
  case DataType::FLOAT:
    match = (blk->isFloat() && !blk->isComplex());
    break;
  case DataType::DOUBLE:
    match = blk->isDouble();
    break;
  case DataType::DATE:
    match = blk->isUtc();
    break;
  case DataType::COMPLEX_FLOAT:
    match = (blk->isFloat() && blk->isComplex());
    break;
  default:
    match = false;
  }
  
  // If the register type doesn't match the requested, throw an error
  
  if(!match) {
    errStr.initMessage(true);
    errStr << "Register " << aregmap->name << "." << blk->brd_->name << "." << blk->name_
	   << "(" << DataType::typeOf(blk) << ")"
	   << " does not match the requested type. (" << type << ")" << endl;
    throw Error(errStr);
  }
  
  if(range != 0 && !blk->axes_->rangeIsValid(range)) {
    errStr.initMessage(true);
    errStr << "Invalid range: " 
	   << aregmap->name << "." << blk->brd_->name << "." << blk->name_ 
	   << *range << endl;
    throw Error(errStr);
  }
}

void ArrayMapDataFrameManager::
checkType(string regmap, string board, string block, DataType::Type type,
	  CoordRange* range)
{
  RegMapBlock* blk=getReg(regmap, board, block);
  ArrRegMap* aregmap=getArrReg(regmap);
  
  return checkType(aregmap, blk, type, range);
}

//-----------------------------------------------------------------------
// Methods to write to a named register
//-----------------------------------------------------------------------

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 bool* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::BOOL, range);
  
  // And pack the data 
  
  packData(regmap, board, block, (void*) data, range, DataType::BOOL);
}

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 unsigned char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::UCHAR, range);

  // And pack the data 
  
  packData(regmap, board, block, (void*) data, range, DataType::UCHAR);
}

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 signed char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::CHAR, range);
  
  // And pack the data 
  
  packData(regmap, board, block, (void*) data, range, DataType::CHAR);
}

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 unsigned short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::USHORT, range);
  
  // And pack the data 
  
  packData(regmap, board, block, (void*) data, range, DataType::USHORT);
}

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 signed short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::SHORT, range);
  
  // And pack the data 
  
  packData(regmap, board, block, (void*) data, range, DataType::SHORT);
}

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 unsigned int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::UINT, range);
  
  // And pack the data 
  
  packData(regmap, board, block, (void*) data, range, DataType::UINT);
}

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 signed int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::INT, range);
  
  // And pack the data 
  
  packData(regmap, board, block, (void*) data, range, DataType::INT);
}

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 float* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::FLOAT, range);
  
  // And pack the data 

  packData(regmap, board, block, (void*) data, range, DataType::FLOAT);
}

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 double* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::DOUBLE, range);
  
  // And pack the data 
  
  packData(regmap, board, block, (void*) data, range, DataType::DOUBLE);
}

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 RegDate::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::DATE, range);
  
  // And pack the data 
  
  packData(regmap, board, block, (void*) data, range, DataType::DATE);
}

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 Complex<float>::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::COMPLEX_FLOAT, range);
  
  // And pack the data 
  
  packData(regmap, board, block, (void*) data, range, DataType::COMPLEX_FLOAT);
}

//-----------------------------------------------------------------------
// Methods to write to single-valued registers
//-----------------------------------------------------------------------

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 bool data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::BOOL, range);
  
  // And pack the data 
  
  packData(regmap, board, block, (void*) &data, range, DataType::BOOL);
}

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 unsigned char data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::UCHAR, range);
  
  // And pack the data 
  
  packData(regmap, board, block, (void*) &data, range, DataType::UCHAR);
}

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 signed char data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::CHAR, range);
  
  // And pack the data 
  
  packData(regmap, board, block, (void*) &data, range, DataType::CHAR);
}

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 unsigned short data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::USHORT, range);
  
  // And pack the data 
  
  packData(regmap, board, block, (void*) &data, range, DataType::USHORT);
}

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 signed short data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::SHORT, range);
  
  // And pack the data 
  
  packData(regmap, board, block, (void*) &data, range, DataType::SHORT);
}

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 unsigned int data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::UINT, range);
  
  // And pack the data 
  
  packData(regmap, board, block, (void*) &data, range, DataType::UINT);
}

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 signed int data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::INT, range);
  
  // And pack the data 
  
  packData(regmap, board, block, (void*) &data, range, DataType::INT);
}

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 float data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::FLOAT, range);
  
  // And pack the data 
  
  packData(regmap, board, block, (void*) &data, range, DataType::FLOAT);
}

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 double data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::DOUBLE, range);
  
  // And pack the data 
  
  packData(regmap, board, block, (void*) &data, range, DataType::DOUBLE);
}

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 RegDate::Data data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::DATE, range);
  
  // And pack the data 
  
  packData(regmap, board, block, (void*) &data, range, DataType::DATE);
}

void ArrayMapDataFrameManager::
writeReg(string regmap, string board, string block, 
	 Complex<float>::Data data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::COMPLEX_FLOAT, range);
  
  // And pack the data 
  
  packData(regmap, board, block, (void*) &data, range, DataType::COMPLEX_FLOAT);
}

//-----------------------------------------------------------------------
// Methods to write to a named register
//-----------------------------------------------------------------------

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 bool* data, CoordRange* range, bool lockFrame)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::BOOL, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) data, range, DataType::BOOL, lockFrame);
}

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 unsigned char* data, CoordRange* range, bool lockFrame)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::UCHAR, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) data, range, DataType::UCHAR, lockFrame);
}

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 signed char* data, CoordRange* range, bool lockFrame)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::CHAR, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) data, range, DataType::CHAR, lockFrame);
}

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 unsigned short* data, CoordRange* range, bool lockFrame)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::USHORT, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) data, range, DataType::USHORT, lockFrame);
}

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 signed short* data, CoordRange* range, bool lockFrame)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::SHORT, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) data, range, DataType::SHORT, lockFrame);
}

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 unsigned int* data, CoordRange* range, bool lockFrame)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::UINT, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) data, range, DataType::UINT, lockFrame);
}

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 signed int* data, CoordRange* range, bool lockFrame)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::INT, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) data, range, DataType::INT, lockFrame);
}

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 float* data, CoordRange* range, bool lockFrame)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::FLOAT, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) data, range, DataType::FLOAT, lockFrame);
}

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 double* data, CoordRange* range, bool lockFrame)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::DOUBLE, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) data, range, DataType::DOUBLE, lockFrame);
}

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 RegDate::Data* data, CoordRange* range, bool lockFrame)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::DATE, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) data, range, DataType::DATE, lockFrame);
}

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 Complex<float>::Data* data, CoordRange* range, bool lockFrame)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::COMPLEX_FLOAT, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) data, range, DataType::COMPLEX_FLOAT, lockFrame);
}

//-----------------------------------------------------------------------
// Methods to write to a single-valued register
//-----------------------------------------------------------------------

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 bool data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::BOOL, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) &data, range, DataType::BOOL);
}

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 unsigned char data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::UCHAR, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) &data, range, DataType::UCHAR);
}

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 signed char data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::CHAR, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) &data, range, DataType::CHAR);
}

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 unsigned short data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::USHORT, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) &data, range, DataType::USHORT);
}

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 signed short data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::SHORT, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) &data, range, DataType::SHORT);
}

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 unsigned int data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::UINT, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) &data, range, DataType::UINT);
}

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 signed int data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::INT, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) &data, range, DataType::INT);
}

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 float data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::FLOAT, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) &data, range, DataType::FLOAT);
}

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 double data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::DOUBLE, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) &data, range, DataType::DOUBLE);
}

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 RegDate::Data data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::DATE, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) &data, range, DataType::DATE);
}

void ArrayMapDataFrameManager::
writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
	 Complex<float>::Data data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::COMPLEX_FLOAT, range);
  
  // And pack the data 
  
  packData(aregmap, blk, (void*) &data, range, DataType::COMPLEX_FLOAT);
}

//------------------------------------------------------------
// Methods to read a named register
//------------------------------------------------------------

void ArrayMapDataFrameManager::
readReg(string regmap, string board, string block, 
	bool* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::BOOL, range);
  
  // And pack the data 
  
  unpackData(regmap, board, block, (void*) data, range, DataType::BOOL);
}

void ArrayMapDataFrameManager::
readReg(string regmap, string board, string block, 
	unsigned char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::UCHAR, range);
  
  // And pack the data 
  
  unpackData(regmap, board, block, (void*) data, range, DataType::UCHAR);
}

void ArrayMapDataFrameManager::
readReg(string regmap, string board, string block, 
	signed char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::CHAR, range);
  
  // And pack the data 
  
  unpackData(regmap, board, block, (void*) data, range, DataType::CHAR);
}

void ArrayMapDataFrameManager::
readReg(string regmap, string board, string block, 
	unsigned short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::USHORT, range);
  
  // And pack the data 
  
  unpackData(regmap, board, block, (void*) data, range, DataType::USHORT);
}

void ArrayMapDataFrameManager::
readReg(string regmap, string board, string block, 
	signed short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::SHORT, range);
  
  // And pack the data 
  
  unpackData(regmap, board, block, (void*) data, range, DataType::SHORT);
}

void ArrayMapDataFrameManager::
readReg(string regmap, string board, string block, 
	unsigned int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::UINT, range);
  
  // And pack the data 
  
  unpackData(regmap, board, block, (void*) data, range, DataType::UINT);
}

void ArrayMapDataFrameManager::
readReg(string regmap, string board, string block, 
	signed int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::INT, range);
  
  // And pack the data 
  
  unpackData(regmap, board, block, (void*) data, range, DataType::INT);
}

void ArrayMapDataFrameManager::
readReg(string regmap, string board, string block, 
	float* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::FLOAT, range);
  
  // And pack the data 
  
  unpackData(regmap, board, block, (void*) data, range, DataType::FLOAT);
}

void ArrayMapDataFrameManager::
readReg(string regmap, string board, string block, 
	double* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::DOUBLE, range);
  
  // And pack the data 
  
  unpackData(regmap, board, block, (void*) data, range, DataType::DOUBLE);
}

void ArrayMapDataFrameManager::
readReg(string regmap, string board, string block, 
	RegDate::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::DATE, range);
  
  // And pack the data 
  
  unpackData(regmap, board, block, (void*) data, range, DataType::DATE);
}

void ArrayMapDataFrameManager::
readReg(string regmap, string board, string block, 
	Complex<float>::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(regmap, board, block, DataType::COMPLEX_FLOAT, range);
  
  // And pack the data 
  
  unpackData(regmap, board, block, (void*) data, range, DataType::COMPLEX_FLOAT);
}

//------------------------------------------------------------
// Methods to read a named register
//------------------------------------------------------------

void ArrayMapDataFrameManager::
readReg(ArrRegMap* aregmap, RegMapBlock* blk,
	bool* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::BOOL, range);
  
  // And pack the data 
  
  unpackData(aregmap, blk, (void*) data, range, DataType::BOOL);
}

void ArrayMapDataFrameManager::
readReg(ArrRegMap* aregmap, RegMapBlock* blk,
	unsigned char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::UCHAR, range);
  
  // And pack the data 
  
  unpackData(aregmap, blk, (void*) data, range, DataType::UCHAR);
}

void ArrayMapDataFrameManager::
readReg(ArrRegMap* aregmap, RegMapBlock* blk,
	signed char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::CHAR, range);
  
  // And pack the data 
  
  unpackData(aregmap, blk, (void*) data, range, DataType::CHAR);
}

void ArrayMapDataFrameManager::
readReg(ArrRegMap* aregmap, RegMapBlock* blk,
	unsigned short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::USHORT, range);
  
  // And pack the data 
  
  unpackData(aregmap, blk, (void*) data, range, DataType::USHORT);
}

void ArrayMapDataFrameManager::
readReg(ArrRegMap* aregmap, RegMapBlock* blk,
	signed short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::SHORT, range);
  
  // And pack the data 
  
  unpackData(aregmap, blk, (void*) data, range, DataType::SHORT);
}

void ArrayMapDataFrameManager::
readReg(ArrRegMap* aregmap, RegMapBlock* blk,
	unsigned int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::UINT, range);
  
  // And pack the data 
  
  unpackData(aregmap, blk, (void*) data, range, DataType::UINT);
}

void ArrayMapDataFrameManager::
readReg(ArrRegMap* aregmap, RegMapBlock* blk,
	signed int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::INT, range);
  
  // And pack the data 
  
  unpackData(aregmap, blk, (void*) data, range, DataType::INT);
}

void ArrayMapDataFrameManager::
readReg(ArrRegMap* aregmap, RegMapBlock* blk,
	float* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::FLOAT, range);
  
  // And pack the data 
  
  unpackData(aregmap, blk, (void*) data, range, DataType::FLOAT);
}

void ArrayMapDataFrameManager::
readReg(ArrRegMap* aregmap, RegMapBlock* blk,
	double* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::DOUBLE, range);
  
  // And pack the data 
  
  unpackData(aregmap, blk, (void*) data, range, DataType::DOUBLE);
}

void ArrayMapDataFrameManager::
readReg(ArrRegMap* aregmap, RegMapBlock* blk,
	RegDate::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::DATE, range);
  
  // And pack the data 
  
  unpackData(aregmap, blk, (void*) data, range, DataType::DATE);
}

void ArrayMapDataFrameManager::
readReg(ArrRegMap* aregmap, RegMapBlock* blk,
	Complex<float>::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(aregmap, blk, DataType::COMPLEX_FLOAT, range);
  
  // And pack the data 
  
  unpackData(aregmap, blk, (void*) data, range, DataType::COMPLEX_FLOAT);
}

/**.......................................................................
 * Return true if this board is flagged.
 */
bool ArrayMapDataFrameManager::boardIsFlagged(ArrRegMap* aregmap, 
					      RegMapBoard* brd)
{
  try {
    // The first register of each board is a scalar "status" register.
    // This has the value 0 when the board is ok, or non-zero when
    // broken.
    
    unsigned status;
    readReg(aregmap, brd->blocks[0], &status);
    return status==1;
  } catch(...) {
    return false;
  }
}

/**.......................................................................
 * Return true if the passed board is present in the register map.  It
 * is assumed that the register map has already been verified to
 * belong to this array map.
 */
bool ArrayMapDataFrameManager::regMapIsPresent(RegMap* regmap)
{
  // This regmap will be present if this frame encompasses all
  // registers, or if the board contains any archived registers
  
  return !archivedOnly_ || regmap->nArcByte_ > 0;
}

/**.......................................................................
 * Return true if the passed board is present in the register map.  It
 * is assumed that the board has already been verified to belong to
 * this array map.
 */
bool ArrayMapDataFrameManager::boardIsPresent(RegMapBoard* brd)
{
  // Board will be present if this frame encompasses all registers, or
  // if the board contains any archived registers
  
  return !archivedOnly_ || brd->nArcByte_ > 0;
}

/**.......................................................................
 * Return true if the block is present in the register map.  It is
 * assumed that this block has already been verified to belong to this
 * array map.
 */
bool ArrayMapDataFrameManager::blockIsPresent(RegMapBlock* blk)
{
  // Block is present if this frame encompasses all registers, or
  // if the register is archived
  
  return !archivedOnly_ || blk->isArchived();
}

/**.......................................................................
 * Copy the contents of a RegMapDataFrameManager to the appropriate
 * place in the array map.
 */
void ArrayMapDataFrameManager::writeRegMap(string regmap, 
					   RegMapDataFrameManager& fm, 
					   bool lockFrame)
{
  ArrRegMap* aregmap = getArrReg(regmap);
  
  if(aregmap == 0) 
    ThrowError("Register map: " << regmap << " not found in the array map");
  
  writeRegMap(aregmap, fm, lockFrame);
}

/**.......................................................................
 * Copy the contents of a RegMapDataFrameManager to the appropriate
 * place in the array map.
 */
void ArrayMapDataFrameManager::readRegMap(ArrRegMap* aregmap,
					  RegMapDataFrameManager& fm)
{
  // Do nothing if the register map is not present in our frame
  
  if(!regMapIsPresent(aregmap->regmap))
    return;
  
  // If both frames have the same internal specifications, just copy
  // the whole block at once
  
  lock();
  unlock();
  
  try {
    
    if(fm.archivedOnly() == archivedOnly_) {
      
      // Starting byte index from which we will read will be the offset
      // of this register map in the frame
      
      unsigned iStartSrc = byteOffsetInFrameOf(aregmap);
      
      // Starting byte in the destination frame will just be the start of the
      // data
      
      unsigned iStartDest  = fm.byteOffsetInFrameOfData();
      
      // Pack the array into our frame
      
      fm.frame_->pack(frame_->getPtr(iStartSrc, DataType::UCHAR), 
		      archivedOnly_ ? aregmap->nArcByte() : aregmap->nByte(),
		      DataType::UCHAR, iStartDest, false);
      
      // Else we have to iterate over registers, checking which ones
      // from the source frame should be copied to the destination frame
      
    } else {
      
      RegMap* regmap = aregmap->regmap;
      
      // Iterate over all boards in this register map.
      
      for(unsigned int iboard=0; iboard < regmap->nboard_; iboard++) {
	RegMapBoard* brd = regmap->boards_[iboard];
	
	// Only copy if this board is present in both source and
	// destination register maps.
	
	if(boardIsPresent(brd) && fm.boardIsPresent(brd)) {
	  
	  // If the board is marked as reachable, attempt to copy the
	  // contents of the board's registers into the frame buffer.
	  
	  if(!boardIsFlagged(aregmap, brd)) {
	    
	    // Iterate over all register blocks of this board
	    
	    for(unsigned int iblock=0; iblock < brd->nblock; iblock++) {
	      RegMapBlock* blk = brd->blocks[iblock];
	      
	      // If this block is present in the register maps of both
	      // frames, copy it
	      
	      if(blockIsPresent(blk) && fm.blockIsPresent(blk)) {
		
		unsigned iStartDest = fm.byteOffsetInFrameOf(blk);
		unsigned iStartSrc  = byteOffsetInFrameOf(aregmap, blk);
		
		fm.frame_->pack(frame_->getPtr(iStartSrc, DataType::UCHAR), 
				blk->nByte(), DataType::UCHAR, iStartDest, false);
		
	      }; // End if(blockIsPresent(blk) && fm.blockIsPresent(blk))
	    }; // End iteration over blocks
	  }; // End if !boardIsFlagged(brd)
	}; // End if boardIsPresent(brd)...
      }; // End iteration over boards
    }; // End if(regMapIsPresent(regmap))
  } catch(const Exception& err) {
    unlock();
    fm.unlock();
    throw(err);
  } catch (...) {
    unlock();
    fm.unlock();
  }
}

/**.......................................................................
 * Copy the contents of a RegMapDataFrameManager to the appropriate
 * place in the array map.
 */
void ArrayMapDataFrameManager::writeRegMap(ArrRegMap* aregmap,
					   RegMapDataFrameManager& fm, 
					   bool lockFrame)
{
  // Do nothing if the register map is not present in our frame
  
  if(!regMapIsPresent(aregmap->regmap))
    return;

  DBPRINT(true, Debug::DEBUG14, "About to lock ARMDFM frame: " 
	  << this << " by " << pthread_self());

  if(lockFrame) {
    lock();
    fm.lock();
  }

  DBPRINT(true, Debug::DEBUG14, "Locked ARMDFM frame: " 
	  << this << " by " << pthread_self());

  try {
    
    // If both frames have the same internal specifications, just copy
    // the whole block at once
    
    if(fm.archivedOnly() == archivedOnly_) {
      
      DBPRINT(true, Debug::DEBUG10, "About to pack(0)");

      // Starting byte index to which we will write will be the offset
      // of this register map in the frame
      
      unsigned iStartDest = byteOffsetInFrameOf(aregmap);
      
      // Starting byte in the source frame will just be the start of the
      // data
      
      unsigned iStartSrc  = fm.byteOffsetInFrameOfData();
      
      // Pack the array into our frame
      
      DBPRINT(true, Debug::DEBUG10, "About to pack(0): startByte = " 
	      << iStartDest << " nByte = " << aregmap->nByte(archivedOnly_));

      frame_->pack(fm.frame_->getPtr(iStartSrc, DataType::UCHAR), 
		   aregmap->nByte(archivedOnly_),
		   DataType::UCHAR, iStartDest, false);

      DBPRINT(true, Debug::DEBUG10, "Done packing(0)");

      // Else we have to iterate over registers, checking which ones
      // from the source frame should be copied to the destination frame
      
    } else {
      
      DBPRINT(true, Debug::DEBUG10, "About to pack(1)");

      RegMap* regmap = aregmap->regmap;
      
      // Iterate over all boards in this register map.
      
      for(unsigned int iboard=0; iboard < regmap->nboard_; iboard++) {
	RegMapBoard* brd = regmap->boards_[iboard];
	
	// Only copy if this board is present in both source and
	// destination register maps.
	
	if(boardIsPresent(brd) && fm.boardIsPresent(brd)) {
	  
	  // If the board is marked as reachable, attempt to copy the
	  // contents of the board's registers into the frame buffer.
	  
	  if(!boardIsFlagged(aregmap, brd)) {
	    
	    // Iterate over all register blocks of this board
	    
	    for(unsigned int iblock=0; iblock < brd->nblock; iblock++) {
	      RegMapBlock* blk = brd->blocks[iblock];
	      
	      // If this block is present in the register maps of both
	      // frames, copy it
	      
	      if(blockIsPresent(blk) && fm.blockIsPresent(blk)) {
		
		unsigned iStartSrc  = fm.byteOffsetInFrameOf(blk);
		unsigned iStartDest = byteOffsetInFrameOf(aregmap, blk);


		frame_->pack(fm.frame_->getPtr(iStartSrc, DataType::UCHAR), 
			     blk->nByte(), DataType::UCHAR, iStartDest, false);
		
	      }; // End if(blockIsPresent(blk) && fm.blockIsPresent(blk))
	    }; // End iteration over blocks
	  }; // End if !boardIsFlagged(brd)
	}; // End if boardIsPresent(brd)...
      }; // End iteration over boards

      DBPRINT(true, Debug::DEBUG10, "Done packing(1)");

    }; // End if(regMapIsPresent(regmap))
  } catch(const Exception& err) {
    if(lockFrame) {
      unlock();
      fm.unlock();
    }
    throw(err);
  } catch(...) {
    if(lockFrame) {
      unlock();
      fm.unlock();
    }
  }
}

/**.......................................................................
 * Return the value of the current register in a range, cast to
 * a double.
 */
double ArrayMapDataFrameManager::getRegVal(RegRange& range, bool lockFrame)
{
  RegMapBlock* blk    = range.currentBlock();
  unsigned byteOffset = range.currentByteOffset() + byteOffsetInFrameOfData();

  if(blk->isBool()) {
    bool val;
    frame_->unpack(&val, 1, DataType::BOOL, byteOffset, lockFrame);
    return (double)val;
  } 

  else if(blk->isChar()) {
    char val;
    frame_->unpack(&val, 1, DataType::CHAR,   byteOffset, lockFrame);
    return (double)val;
  } 

  else if(blk->isUchar()) {
    unsigned char val;
    frame_->unpack(&val, 1, DataType::UCHAR,  byteOffset, lockFrame);
    return (double)val;
  } 

  else if(blk->isShort()) {
    short val;
    frame_->unpack(&val, 1, DataType::SHORT,  byteOffset, lockFrame);
    return (double)val;
  } 

  else if(blk->isUshort()) {
    unsigned short val;
    frame_->unpack(&val, 1, DataType::USHORT, byteOffset, lockFrame);
    return (double)val;
  } 

  else if(blk->isInt()) {
    int val;
    frame_->unpack(&val, 1, DataType::INT,    byteOffset, lockFrame);
    return (double)val;
  } 

  else if(blk->isUint()) {
    unsigned int val;
    frame_->unpack(&val, 1, DataType::UINT,   byteOffset, lockFrame);
    return (double)val;
  } 

  else if(blk->isFloat()) {
    if(blk->isComplex()) {
      Complex<float>::Data val;
      frame_->unpack(&val, 1, DataType::COMPLEX_FLOAT, byteOffset, lockFrame);
      return *((double*)&val);
    } else {
      float val;
      frame_->unpack(&val, 1, DataType::FLOAT,  byteOffset, lockFrame);
      return (double)val;
    } 
  }

  else if(blk->isDouble()) {
    double val;
    frame_->unpack(&val, 1, DataType::DOUBLE, byteOffset, lockFrame);
    return (double)val;
  }

  else if(blk->isUtc()) {
    RegDate::Data val;
    frame_->unpack(&val, 1, DataType::DATE, byteOffset, lockFrame);
    return *((double*)&val);
  }
}

/**.......................................................................
 * Return the value of the requested register index
 */
double ArrayMapDataFrameManager::getRegVal(RegDescription& desc)
{
  RegMapBlock* blk = desc.block();
  void* ptr        = getPtr(desc);

  if(blk->isBool()) {
    return (double) *((bool*)ptr);
  } 

  else if(blk->isChar()) {
    return (double) *((char*)ptr);
  } 

  else if(blk->isUchar()) {
    return (double) *((unsigned char*)ptr);
  } 

  else if(blk->isShort()) {
    return (double) *((short*)ptr);
  } 

  else if(blk->isUshort()) {
    return (double) *((unsigned short*)ptr);
  } 

  else if(blk->isInt()) {
    return (double) *((int*)ptr);
  } 

  else if(blk->isUint()) {
    return (double) *((unsigned int*)ptr);
  } 

  else if(blk->isFloat()) {
    if(blk->isComplex()) {
      Complex<float>::Data val;
      val = *((Complex<float>::Data*)ptr);
      return *((double*)&val); 
    } else {
      return (double) *((float*)ptr);
    } 
  }

  else if(blk->isDouble()) {
    return *((double*)ptr);
  }

  else if(blk->isUtc()) {
    RegDate date;
    *date.data() = *((RegDate::Data*)ptr); 
    return (double) date.mjd();
  }
}

/**.......................................................................
 * Get a unique frame id based on integral MJD half-seconds.
 */
unsigned int ArrayMapDataFrameManager::getId(unsigned nanoSecondInterval)
{
  RegDate date;

  readReg("array", "frame", "utc", date.data());

  DBPRINT(true, Debug::DEBUG10, "Read date: " << date);

  return date.timeVal().getMjdId(nanoSecondInterval);
}

/**.......................................................................
 * Get a pointer to the start of the register dscribed in a
 * RegDescription, in our data frame.
 */
void* ArrayMapDataFrameManager::getPtr(RegDescription& desc)
{
  Coord c = desc.getRangePtr()->startCoord();
  return frame_->getPtr(byteOffsetInFrameOf(desc.regMap(), desc.block(), &c),
			DataType::typeOf(desc.block()));
}
