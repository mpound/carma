#include "carma/szautil/RegAxisRange.h"
#include "carma/szautil/RegMapDataFrameManager.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"

using namespace sza::util;
using namespace std;


/**.......................................................................
 * Constructor.
 */
RegMapDataFrameManager::RegMapDataFrameManager(bool archivedOnly) 
{
  regMap_ = 0; // Just initialize the regmap pointer to null --
	       // inheritors should allocate this according to which
	       // regmap is appropriate for them.
  
  archivedOnly_ = archivedOnly;

  DBPRINT(true, Debug::DEBUG10, "archivedOnly = " << archivedOnly);
}

/**.......................................................................
 * Destructor.
 */
RegMapDataFrameManager::~RegMapDataFrameManager() {}

/**.......................................................................
 * Overloaded assignment operator from the base class
 */
void RegMapDataFrameManager::operator=(DataFrameManager& fm)
{
  // Just call our assignment operator
  
  operator=((RegMapDataFrameManager&) fm);
}

/**.......................................................................
 * Assignment operator
 */
void RegMapDataFrameManager::operator=(RegMapDataFrameManager& fm)
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
      errStr.appendMessage(true, "Frames are not from equivalent register maps.\n");
      throw Error(errStr);
    }
    
    // Else we have to iterate over registers, checking which ones
    // from the source frame should be copied to the destination frame
    
  } else {
    
    lock();
    fm.lock();
    
    try {
      
      // Iterate over all boards in this register map.
      
      for(unsigned int iboard=0; iboard < regMap_->nboard_; iboard++) {
	RegMapBoard* brd = regMap_->boards_[iboard];
	
	// Only copy if this board is present in both source and
	// destination register maps.
	
	if(boardIsPresent(brd) && fm.boardIsPresent(brd)) {
	  
	  // If the board is marked as reachable, attempt to copy the
	  // contents of the board's registers into the frame buffer.
	  
	  if(!boardIsFlagged(brd)) {
	    
	    // Iterate over all register blocks of this board
	    
	    for(unsigned int iblock=0; iblock < brd->nblock; iblock++) {
	      RegMapBlock* blk = brd->blocks[iblock];
	      
	      // If this block is present in the register maps of both
	      // frames, copy it
	      
	      if(blockIsPresent(blk) && fm.blockIsPresent(blk)) {
		
		unsigned iStartDest = byteOffsetInFrameOf(blk);
		unsigned iStartSrc  = fm.byteOffsetInFrameOf(blk);
		
		// Pack without locking the frames, since they will
		// already be locked

		frame_->pack(fm.frame_->getPtr(iStartSrc, DataType::UCHAR), 
			     blk->nByte(), DataType::UCHAR, iStartDest, false);
	      }
	    }
	  }
	}
      }

      // If an exception was thrown, unlock the frames, and rethrow
      
    } catch (const Exception& err) {
      
      unlock();
      fm.unlock();
      
      throw(err);
      
    } catch (...) {
      unlock();
      fm.unlock();
    }
  }
}

//-----------------------------------------------------------------------
// Methods to get the byte offsets in the register map of a named board
// and block
//-----------------------------------------------------------------------

/**.......................................................................
 * Return the offset in a register map, of the data for this
 * register.
 */
int RegMapDataFrameManager::
byteOffsetInRegMapOf(RegMapBlock* blk, Coord* coord)
{
  if(regMap_ != 0) {
    return regMap_->byteOffsetInRegMapOf(archivedOnly_, blk, coord);
  } else {
    LogStream errStr;
    errStr.appendMessage(true, "Register map is NULL\n");
    throw Error(errStr);
  }
}

int RegMapDataFrameManager::
byteOffsetInRegMapOf(string board, string block, Coord* coord)
{
  RegMapBlock* blk = getReg(board, block);
  return byteOffsetInRegMapOf(blk, coord);
}

/**.......................................................................
 * Return the offset in a register map, of the data for this
 * register.
 */
int RegMapDataFrameManager::
byteOffsetInRegMapOf(RegMapBlock* blk, CoordRange* range)
{
  Coord coord, *coordPtr=0;

  if(range != 0) {
    coord = range->startCoord();
    coordPtr = &coord;
  }
  
  return regMap_->byteOffsetInRegMapOf(archivedOnly_, blk, coordPtr);
}

int RegMapDataFrameManager::
byteOffsetInRegMapOf(string board, string block, CoordRange* range)
{
  RegMapBlock* blk = getReg(board, block);
  return byteOffsetInRegMapOf(blk, range);
}

/**.......................................................................
 * Return the offset in bytes of the data for the requested
 * and register, from the beginning of the frame buffer.
 */
int RegMapDataFrameManager::
byteOffsetInFrameOf(RegMapBlock* blk, Coord* coord)
{
  int regOffset = byteOffsetInRegMapOf(blk, coord);
  
  if(regOffset < 0)
    return -1;
  
  return byteOffsetInFrameOfData() + regOffset;
}

int RegMapDataFrameManager::
byteOffsetInFrameOf(string board, string block, Coord* coord)
{
  int regOffset = byteOffsetInRegMapOf(board, block, coord);
  
  if(regOffset < 0)
    return -1;
  
  return byteOffsetInFrameOfData() + regOffset;
}

/**.......................................................................
 * Return the offset in bytes of the data for the requested
 * and register, from the beginning of the frame buffer.
 */
int RegMapDataFrameManager::
byteOffsetInFrameOf(RegMapBlock* blk, CoordRange* range)
{
  Coord coord, *coordPtr=0;

  if(range != 0) {
    coord = range->startCoord();
    coordPtr = &coord;
  }

  return byteOffsetInFrameOf(blk, coordPtr);
}

int RegMapDataFrameManager::
byteOffsetInFrameOf(string board, string block, CoordRange* range)
{
  Coord coord, *coordPtr=0;

  if(range != 0) {
    coord = range->startCoord();
    coordPtr = &coord;
  }
  return byteOffsetInFrameOf(board, block, coordPtr);
}

/**.......................................................................
 * Pack data of an arbitrary type into the underlying frame
 */
void RegMapDataFrameManager::
packData(RegMapBlock* blk, void* data, CoordRange* range, 
	 DataType::Type type, bool lockFrame)
{
  // Calculate the byte offset of the start element of this register
  // from the head of the frame.
  
  int byteOffset = byteOffsetInFrameOf(blk);
  
  // Do nothing if this register isn't archived
  
  if(byteOffset < 0)
    return;
  
  // If the number of bytes was passed as 0, use the default from the
  // block descriptor
  
  AxisRange axisRange(blk->axes_, range);

  //axisRange_.setTo(blk->axes_, range);

  frame_->pack(data, axisRange, type, byteOffset, lockFrame);
}

void RegMapDataFrameManager::
packData(string board, string block, void* data, CoordRange* range, 
	 DataType::Type type, bool lockFrame)
{
  RegMapBlock* blk = getReg(board, block);
  return packData(blk, data, range, type, lockFrame);
}

/**.......................................................................
 * Pack the same value for all elements of a range
 */
void RegMapDataFrameManager::
packValue(RegMapBlock* blk, void* data, CoordRange* range, 
	  DataType::Type type, bool lockFrame)
{
  // Calculate the byte offset of the start element of this register
  // from the head of the frame.
  
  int byteOffset = byteOffsetInFrameOf(blk);
  
  // Do nothing if this register isn't archived
  
  if(byteOffset < 0)
    return;
  
  // If the number of bytes was passed as 0, use the default from the
  // block descriptor
  
  AxisRange axisRange(blk->axes_, range);

  //axisRange_.setTo(blk->axes_, range);
 
  frame_->packValue(data, axisRange, type, byteOffset, lockFrame);
}

void RegMapDataFrameManager::
packValue(string board, string block, void* data, CoordRange* range, 
	  DataType::Type type, bool lockFrame)
{
  RegMapBlock* blk = getReg(board, block);
  return packValue(blk, data, range, type, lockFrame);
}

/**.......................................................................
 * Unpack data of an arbitrary type into the underlying frame
 */
void RegMapDataFrameManager::
unpackData(RegMapBlock* blk, void* data, CoordRange* range, 
	   DataType::Type type, bool lockFrame)
{
  // Calculate the byte offset of the start element of this register
  // from the head of the frame.
  
  int byteOffset = byteOffsetInFrameOf(blk);
  
  // Do nothing if this register isn't archived
  
  if(byteOffset < 0)
    return;
  
  // If the number of bytes was passed as 0, use the default from the
  // block descriptor
  
  AxisRange axisRange(blk->axes_, range);

  //axisRange_.setTo(blk->axes_, range);

  frame_->unpack(data, axisRange, type, byteOffset, lockFrame);
}

void RegMapDataFrameManager::
unpackData(string board, string block, void* data, CoordRange* range, 
	   DataType::Type type, bool lockFrame)
{
  RegMapBlock* blk = getReg(board, block);
  return unpackData(blk, data, range, type, lockFrame);
}

/**.......................................................................
 * Get the descriptor for this reg map block
 */
RegMapBlock* RegMapDataFrameManager::getReg(string board, string block)
{
  RegMapBlock* blk=0; 
  LogStream errStr;
  
  // Look up the requested block

  if(regMap_ != 0)
    blk = regMap_->findRegMapBlock(board, block, true);
  else {
    errStr.appendMessage(true, "Register map is NULL\n");
    throw Error(errStr);
  }
  
  // Check that the block requested was a valid one
  
  if(blk==0) {
    errStr.initMessage(true);
    errStr << "Register " << board << "." << block 
	   << " not found in the register map" << endl;
    throw Error(errStr);
  }
  
  return blk;
}

/**.......................................................................
 * Check the type and element number of a regmap block
 */
void RegMapDataFrameManager::
checkType(RegMapBlock* blk, DataType::Type type,
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
  
  if(!match) {
    errStr.initMessage(true);
    errStr << "Register " << blk->brd_->name << "." << blk->name_
	   << " does not match the requested type." << endl;
    throw Error(errStr);
  }
  
  if(range !=0 && !blk->axes_->rangeIsValid(range)) {
    errStr.initMessage(true);
    errStr << "Invalid range: " << blk->brd_->name << "." << blk->name_
	   << range << endl;
    throw Error(errStr);
  }
}

void RegMapDataFrameManager::
checkType(string board, string block, DataType::Type type,
	  CoordRange* range) 
{
  bool match;
  LogStream errStr;
  RegMapBlock* blk=getReg(board, block);
  
  return checkType(blk, type, range);
}

//-----------------------------------------------------------------------
// Methods to write to a named register
//-----------------------------------------------------------------------

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 bool* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::BOOL, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::BOOL);
}

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 unsigned char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::UCHAR, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::UCHAR);
}

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 signed char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::CHAR, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::CHAR);
}

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 unsigned short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::USHORT, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::USHORT);
}

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 signed short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::SHORT, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::SHORT);
}

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 unsigned int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::UINT, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::UINT);
}

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 signed int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::INT, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::INT);
}

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 float* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::FLOAT, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::FLOAT);
}

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 double* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::DOUBLE, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::DOUBLE);
}

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 RegDate::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::DATE, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::DATE);
}

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 Complex<float>::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::COMPLEX_FLOAT, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::COMPLEX_FLOAT);
}

//------------------------------------------------------------
// Versions for writing single register values
//------------------------------------------------------------

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 bool data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::BOOL, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::BOOL);
}

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 unsigned char data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::UCHAR, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::UCHAR);
}

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 signed char data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::CHAR, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::CHAR);
}

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 unsigned short data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::USHORT, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::USHORT);
}

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 signed short data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::SHORT, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::SHORT);
}

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 unsigned int data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::UINT, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::UINT);
}

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 signed int data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::INT, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::INT);
}

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 float data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::FLOAT, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::FLOAT);
}

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 double data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::DOUBLE, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::DOUBLE);
}

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 RegDate::Data data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::DATE, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::DATE);
}

void RegMapDataFrameManager::
writeReg(string board, string block, 
	 Complex<float>::Data data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::COMPLEX_FLOAT, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::COMPLEX_FLOAT);
}

//------------------------------------------------------------
// Versions which don't lock
//------------------------------------------------------------

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       bool* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::BOOL, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::BOOL, false);
}

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       unsigned char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::UCHAR, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::UCHAR, false);
}

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       signed char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::CHAR, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::CHAR, false);
}

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       unsigned short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::USHORT, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::USHORT, false);
}

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       signed short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::SHORT, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::SHORT, false);
}

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       unsigned int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::UINT, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::UINT, false);
}

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       signed int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::INT, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::INT, false);
}

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       float* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::FLOAT, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::FLOAT, false);
}

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       double* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::DOUBLE, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::DOUBLE, false);
}

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       RegDate::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::DATE, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::DATE, false);
}

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       Complex<float>::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::COMPLEX_FLOAT, range);
  
  // And pack the data 
  
  packData(board, block, (void*) data, range, DataType::COMPLEX_FLOAT, false);
}

//------------------------------------------------------------
// Versions for writing single register values
//------------------------------------------------------------

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       bool data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::BOOL, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::BOOL, false);
}

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       unsigned char data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::UCHAR, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::UCHAR, false);
}

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       signed char data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::CHAR, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::CHAR, false);
}

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       unsigned short data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::USHORT, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::USHORT, false);
}

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       signed short data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::SHORT, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::SHORT, false);
}

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       unsigned int data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::UINT, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::UINT, false);
}

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       signed int data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::INT, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::INT, false);
}

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       float data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::FLOAT, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::FLOAT, false);
}

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       double data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::DOUBLE, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::DOUBLE, false);
}

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       RegDate::Data data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::DATE, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::DATE, false);
}

void RegMapDataFrameManager::
writeRegNoLock(string board, string block, 
	       Complex<float>::Data data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::COMPLEX_FLOAT, range);
  
  // And pack the data 
  
  packValue(board, block, (void*) &data, range, DataType::COMPLEX_FLOAT, false);
}

//-----------------------------------------------------------------------
// Methods to write to a named register
//-----------------------------------------------------------------------

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 bool* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::BOOL, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::BOOL);
}

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 unsigned char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::UCHAR, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::UCHAR);
}

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 signed char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::CHAR, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::CHAR);
}

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 unsigned short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::USHORT, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::USHORT);
}

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 signed short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::SHORT, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::SHORT);
}

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 unsigned int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::UINT, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::UINT);
}

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 signed int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::INT, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::INT);
}

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 float* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::FLOAT, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::FLOAT);
}

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 double* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::DOUBLE, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::DOUBLE);
}

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 RegDate::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::DATE, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::DATE);
}

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 Complex<float>::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::COMPLEX_FLOAT, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::COMPLEX_FLOAT);
}

//------------------------------------------------------------
// Versions for writing single register values
//------------------------------------------------------------

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 bool data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::BOOL, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::BOOL);
}

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 unsigned char data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::UCHAR, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::UCHAR);
}

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 signed char data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::CHAR, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::CHAR);
}

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 unsigned short data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::USHORT, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::USHORT);
}

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 signed short data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::SHORT, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::SHORT);
}

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 unsigned int data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::UINT, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::UINT);
}

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 signed int data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::INT, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::INT);
}

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 float data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::FLOAT, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::FLOAT);
}

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 double data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::DOUBLE, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::DOUBLE);
}

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 RegDate::Data data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::DATE, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::DATE);
}

void RegMapDataFrameManager::
writeReg(RegMapBlock* blk,
	 Complex<float>::Data data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::COMPLEX_FLOAT, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::COMPLEX_FLOAT);
}

//------------------------------------------------------------
// Versions which don't lock
//------------------------------------------------------------

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       bool* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::BOOL, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::BOOL, false);
}

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       unsigned char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::UCHAR, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::UCHAR, false);
}

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       signed char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::CHAR, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::CHAR, false);
}

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       unsigned short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::USHORT, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::USHORT, false);
}

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       signed short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::SHORT, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::SHORT, false);
}

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       unsigned int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::UINT, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::UINT, false);
}

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       signed int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::INT, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::INT, false);
}

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       float* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::FLOAT, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::FLOAT, false);
}

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       double* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::DOUBLE, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::DOUBLE, false);
}

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       RegDate::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::DATE, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::DATE, false);
}

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       Complex<float>::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::COMPLEX_FLOAT, range);
  
  // And pack the data 
  
  packData(blk, (void*) data, range, DataType::COMPLEX_FLOAT, false);
}

//------------------------------------------------------------
// Versions for writing single register values
//------------------------------------------------------------

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       bool data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::BOOL, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::BOOL, false);
}

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       unsigned char data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::UCHAR, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::UCHAR, false);
}

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       signed char data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::CHAR, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::CHAR, false);
}

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       unsigned short data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::USHORT, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::USHORT, false);
}

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       signed short data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::SHORT, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::SHORT, false);
}

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       unsigned int data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::UINT, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::UINT, false);
}

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       signed int data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::INT, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::INT, false);
}

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       float data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::FLOAT, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::FLOAT, false);
}

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       double data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::DOUBLE, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::DOUBLE, false);
}

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       RegDate::Data data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::DATE, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::DATE, false);
}

void RegMapDataFrameManager::
writeRegNoLock(RegMapBlock* blk,
	       Complex<float>::Data data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::COMPLEX_FLOAT, range);
  
  // And pack the data 
  
  packValue(blk, (void*) &data, range, DataType::COMPLEX_FLOAT, false);
}


//------------------------------------------------------------
// Methods to read a named register
//------------------------------------------------------------

void RegMapDataFrameManager::
readReg(string board, string block, 
	bool* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::BOOL, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::BOOL);
}

void RegMapDataFrameManager::
readReg(string board, string block, 
	unsigned char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::UCHAR, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::UCHAR);
}

void RegMapDataFrameManager::
readReg(string board, string block, 
	signed char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::CHAR, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::CHAR);
}

void RegMapDataFrameManager::
readReg(string board, string block, 
	unsigned short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::USHORT, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::USHORT);
}

void RegMapDataFrameManager::
readReg(string board, string block, 
	signed short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::SHORT, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::SHORT);
}

void RegMapDataFrameManager::
readReg(string board, string block, 
	unsigned int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::UINT, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::UINT);
}

void RegMapDataFrameManager::
readReg(string board, string block, 
	signed int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::INT, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::INT);
}

void RegMapDataFrameManager::
readReg(string board, string block, 
	float* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::FLOAT, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::FLOAT);
}

void RegMapDataFrameManager::
readReg(string board, string block, 
	double* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::DOUBLE, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::DOUBLE);
}

void RegMapDataFrameManager::
readReg(string board, string block, 
	RegDate::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::DATE, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::DATE);
}

void RegMapDataFrameManager::
readReg(string board, string block, 
	Complex<float>::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::COMPLEX_FLOAT, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::COMPLEX_FLOAT);
}

//------------------------------------------------------------
// Versions which don't lock
//------------------------------------------------------------

void RegMapDataFrameManager::
readRegNoLock(string board, string block, 
	      bool* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::BOOL, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::BOOL, false);
}

void RegMapDataFrameManager::
readRegNoLock(string board, string block, 
	      unsigned char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::UCHAR, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::UCHAR, false);
}

void RegMapDataFrameManager::
readRegNoLock(string board, string block, 
	      signed char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::CHAR, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::CHAR, false);
}

void RegMapDataFrameManager::
readRegNoLock(string board, string block, 
	      unsigned short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::USHORT, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::USHORT, false);
}

void RegMapDataFrameManager::
readRegNoLock(string board, string block, 
	      signed short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::SHORT, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::SHORT, false);
}

void RegMapDataFrameManager::
readRegNoLock(string board, string block, 
	      unsigned int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::UINT, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::UINT, false);
}

void RegMapDataFrameManager::
readRegNoLock(string board, string block, 
	      signed int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::INT, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::INT, false);
}

void RegMapDataFrameManager::
readRegNoLock(string board, string block, 
	      float* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::FLOAT, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::FLOAT, false);
}

void RegMapDataFrameManager::
readRegNoLock(string board, string block, 
	      double* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::DOUBLE, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::DOUBLE, false);
}

void RegMapDataFrameManager::
readRegNoLock(string board, string block, 
	      RegDate::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::DATE, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::DATE, false);
}

void RegMapDataFrameManager::
readRegNoLock(string board, string block, 
	      Complex<float>::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(board, block, DataType::COMPLEX_FLOAT, range);
  
  // And pack the data 
  
  unpackData(board, block, (void*) data, range, DataType::COMPLEX_FLOAT, false);
}


//------------------------------------------------------------
// Methods to read a named register
//------------------------------------------------------------

void RegMapDataFrameManager::
readReg(RegMapBlock* blk,
	bool* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::BOOL, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::BOOL);
}

void RegMapDataFrameManager::
readReg(RegMapBlock* blk,
	unsigned char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::UCHAR, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::UCHAR);
}

void RegMapDataFrameManager::
readReg(RegMapBlock* blk,
	signed char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::CHAR, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::CHAR);
}

void RegMapDataFrameManager::
readReg(RegMapBlock* blk,
	unsigned short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::USHORT, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::USHORT);
}

void RegMapDataFrameManager::
readReg(RegMapBlock* blk,
	signed short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::SHORT, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::SHORT);
}

void RegMapDataFrameManager::
readReg(RegMapBlock* blk,
	unsigned int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::UINT, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::UINT);
}

void RegMapDataFrameManager::
readReg(RegMapBlock* blk,
	signed int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::INT, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::INT);
}

void RegMapDataFrameManager::
readReg(RegMapBlock* blk,
	float* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::FLOAT, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::FLOAT);
}

void RegMapDataFrameManager::
readReg(RegMapBlock* blk,
	double* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::DOUBLE, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::DOUBLE);
}

void RegMapDataFrameManager::
readReg(RegMapBlock* blk,
	RegDate::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::DATE, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::DATE);
}

void RegMapDataFrameManager::
readReg(RegMapBlock* blk,
	Complex<float>::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::COMPLEX_FLOAT, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::COMPLEX_FLOAT);
}

//------------------------------------------------------------
// Versions which don't lock
//------------------------------------------------------------

void RegMapDataFrameManager::
readRegNoLock(RegMapBlock* blk,
	      bool* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::BOOL, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::BOOL, false);
}

void RegMapDataFrameManager::
readRegNoLock(RegMapBlock* blk,
	      unsigned char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::UCHAR, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::UCHAR, false);
}

void RegMapDataFrameManager::
readRegNoLock(RegMapBlock* blk,
	      signed char* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::CHAR, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::CHAR, false);
}

void RegMapDataFrameManager::
readRegNoLock(RegMapBlock* blk,
	      unsigned short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::USHORT, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::USHORT, false);
}

void RegMapDataFrameManager::
readRegNoLock(RegMapBlock* blk,
	      signed short* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::SHORT, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::SHORT, false);
}

void RegMapDataFrameManager::
readRegNoLock(RegMapBlock* blk,
	      unsigned int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::UINT, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::UINT, false);
}

void RegMapDataFrameManager::
readRegNoLock(RegMapBlock* blk,
	      signed int* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::INT, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::INT, false);
}

void RegMapDataFrameManager::
readRegNoLock(RegMapBlock* blk,
	      float* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::FLOAT, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::FLOAT, false);
}

void RegMapDataFrameManager::
readRegNoLock(RegMapBlock* blk,
	      double* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::DOUBLE, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::DOUBLE, false);
}

void RegMapDataFrameManager::
readRegNoLock(RegMapBlock* blk,
	      RegDate::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::DATE, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::DATE, false);
}

void RegMapDataFrameManager::
readRegNoLock(RegMapBlock* blk,
	      Complex<float>::Data* data, CoordRange* range)
{
  // Sanity check the register type
  
  checkType(blk, DataType::COMPLEX_FLOAT, range);
  
  // And pack the data 
  
  unpackData(blk, (void*) data, range, DataType::COMPLEX_FLOAT, false);
}

/**.......................................................................
 * Return true if this board is flagged.
 */
bool RegMapDataFrameManager::boardIsFlagged(RegMapBoard* brd)
{
  // The first register of each board is a scalar "status" register.
  // This has the value 0 when the board is ok, or non-zero when
  // broken.
  
  unsigned status;
  readReg(brd->blocks[0], &status);
  return status==1;
}

/**.......................................................................
 * Return true if the passed board is present in the register map
 */
bool RegMapDataFrameManager::boardIsPresent(RegMapBoard* brd)
{
  // Board will be present if this frame encompasses all registers, or
  // if the board contains any archived registers
  
  return !archivedOnly_ || brd->nArcByte_ > 0;
}

/**.......................................................................
 * Return true if the block is present in the register map
 */
bool RegMapDataFrameManager::blockIsPresent(RegMapBlock* blk)
{
  // Block is present if this frame encompasses all registers, or
  // if the register is archived
  
  return !archivedOnly_ || blk->isArchived();
}

/**.......................................................................
 * Get a unique frame id based on integral MJD half-seconds.
 */
unsigned int RegMapDataFrameManager::getId(unsigned nanoSecondInterval)
{
  RegDate date;

  readReg("frame", "utc", date.data());

  return date.timeVal().getMjdId(nanoSecondInterval);
}

/**.......................................................................
 * Get a unique frame id based on integral MJD half-seconds.
 */
void RegMapDataFrameManager::setMjd(double mjd)
{
  TimeVal timeVal;
  timeVal.setMjd(mjd);

  setMjd(timeVal);
}

/**.......................................................................
 * Get a unique frame id based on integral MJD half-seconds.
 */
void RegMapDataFrameManager::setMjd(TimeVal& timeVal)
{
  RegDate date(timeVal);

  writeReg("frame", "utc", date.data());
}

RegMapBlock* RegMapDataFrameManager::findReg(char* boardName, char* blockName)
{
  return regMap_->findRegMapBlock(boardName, blockName);
}

RegMapBoard* RegMapDataFrameManager::findRegMapBoard(std::string boardName)
{
  return find_RegMapBoard(regMap_, (char*)boardName.c_str());
}
