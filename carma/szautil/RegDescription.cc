#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/RegDescription.h"
#include "carma/szautil/RegParser.h"

#include "carma/szaarrayutils/arraymap.h"
#include "carma/szaarrayutils/monitor_stream.h"

#include <iostream>

using namespace sza::array;
using namespace sza::util;
using namespace std;

#define TEST_BLOCK(block) \
 { \
    if(block == 0) \
      std::cout << "block is NULL" << std::endl; \
    else \
      std::cout << "block is NOT NULL" << std::endl; \
 }

/**.......................................................................
 * Constructor.
 */
RegDescription::RegDescription(bool archivedOnly, ArrayMap* arrayMap) :
  archivedOnly_(archivedOnly)
{
  // Initialize internal members
  
  arrayMap_    = (arrayMap != 0 ? alias_ArrayMap(arrayMap) : arrayMapBase_.arrayMap());

  iRegMap_     = -1;
  iBoard_      = -1;
  iBlock_      = -1;
  iSlot_       = -1;
  nEl_         =  0;
  size_        =  0;
  aspect_      =  REG_PLAIN;
  integ_       =  REG_INT_PLAIN;
  iByte_       = -1;
  nBytePerEl_  =  0;

  axisRange_ = 0;
  axisRange_ = new RegAxisRange();

  resetOldestVisible();
  setFirst(true);
}

/**.......................................................................
 * Constructor.
 */
RegDescription::RegDescription(bool archivedOnly, RegMapBlock* block) :
  archivedOnly_(archivedOnly)
{
  initialize();

  axisRange_ = new RegAxisRange(block);

  resetOldestVisible();
  setFirst(true);
}

/**.......................................................................
 * Copy constructor.
 */
RegDescription::RegDescription(RegDescription& desc)
{
  initialize();

  *this = desc;
}

/**.......................................................................
 * Copy constructor.
 */
RegDescription::RegDescription(const RegDescription& desc)
{
  initialize();

  *this = (RegDescription&)desc;
}

void RegDescription::initialize() 
{
  // Initialize internal members
  
  iRegMap_     = -1;
  iBoard_      = -1;
  iBlock_      = -1;
  iSlot_       = -1;
  nEl_         =  0;
  size_        =  0;
  aspect_      =  REG_PLAIN;
  integ_       =  REG_INT_PLAIN;
  iByte_       = -1;
  nBytePerEl_  =  0;
  
  axisRange_ = 0;
}

void RegDescription::operator=(const RegDescription& desc)
{
  *this = (RegDescription&) desc;
}

void RegDescription::operator=(RegDescription& desc)
{
  // And initialize internal members
  
  arrayMap_     = alias_ArrayMap(desc.arrayMap_);
  archivedOnly_ = desc.archivedOnly_;
  outputMode_   = desc.outputMode_;
  iRegMap_      = desc.iRegMap_;
  iBoard_       = desc.iBoard_;
  iBlock_       = desc.iBlock_;
  iSlot_        = desc.iSlot_;
  nEl_          = desc.nEl_;
  size_         = desc.size_;
  aspect_       = desc.aspect_;
  integ_        = desc.integ_;
  iByte_        = desc.iByte_;
  nBytePerEl_   = desc.nBytePerEl_;
  axes_         = desc.axes_;
  range_        = desc.range_;

  if(axisRange_ == 0)
    axisRange_ = new RegAxisRange(*this, &range_);
  else
    axisRange_->setTo(*this, &range_);

  resetOldestVisible();
  setFirst(true);
}

/**.......................................................................
 * Destructor.
 */
RegDescription::~RegDescription() 
{
  if(axisRange_ != 0) {
     delete axisRange_;
     axisRange_ = 0;
  }
}

/**.......................................................................
 * Reset members which should not be reused
 */
void RegDescription::reset()
{
  range_.initialize();
}

/**.......................................................................
 * Fill in the details of this object with the requested register
 *  extend           int    If true allow the user to append, where
 *                          appropriate, one of the following components
 *                          to complex and utc register specifications:
 *                            .amp    -  The amplitude of a complex pair.
 *                            .phase  -  The phase of a complex pair.
 *                            .real   -  The real part of a complex pair.
 *                            .imag   -  The imaginary part of a complex pair.
 *                            .date   -  The Modified Julian Date of a utc pair.
 *                            .time   -  The time-of-day of a utc pair.
 *
 *                          If the user uses these notations then the
 *                          selected attribute will be recorded in
 *                          RegMapReg::aspect.
 */
void RegDescription::setTo(string regmap_name, string board_name, 
			   string block_name,
			   RegAspect aspect, 
			   RegInteg integ, 
			   Coord* inputCoord, unsigned n)
{
  // Attempt to locate the specified register map by name
  
  ArrRegMap* arregmap = arrayMap_->findArrRegMap(regmap_name);
  
  if(!arregmap || !arregmap->regmap) 
    ThrowError("Unable to find register map ");
  
  // Attempt to locate the specified register by name.
  
  RegMapBlock* blk = 
    arregmap->regmap->findRegMapBlock(board_name, block_name, true);

  if(archivedOnly_ && !blk->isArchived())
    ThrowError("Register: " << regmap_name << "." 
	       << board_name << "." << block_name
	       << " isn't archived");

  // If no elements were

  CoordRange range;
  
  //  if(inputCoord != 0 || n != 0) {

    // Initialize the selected element range
  
    unsigned index = blk->axes_->elementOffsetOf(inputCoord);
    
    Coord startCoord = blk->axes_->coordOf(index);
    range.setStartCoord(startCoord);
    
    // And set the end coordinate
    
    Coord stopCoord = blk->axes_->coordOf(index+n);
    
    range.setStopCoord(stopCoord);
    
    //  }

  range_.setContiguous(true);

  // And set the internals

  return setTo(regmap_name, board_name, block_name, aspect, integ, range);
}

/**.......................................................................
 * Method for setting internals with a range
 */
void RegDescription::setTo(string regmap_name, string board_name, 
			   string block_name,
			   RegAspect aspect, 
			   RegInteg integ, 
			   CoordRange& coordRange)
{
  ArrRegMap* arregmap = NULL;
  RegMap* regmap      = NULL;
  RegMapBlock *blk    = NULL; // The register map block that contains
			      // the register
  // Attempt to locate the specified register map by name
  
  arregmap = arrayMap_->findArrRegMap(regmap_name);
  
  if(!arregmap || !arregmap->regmap) 
    ThrowError("Unable to find register map ");
  
  // Attempt to locate the specified register by name.
  
  blk = arregmap->regmap->findRegMapBlock(board_name, block_name, true);
  
  if(archivedOnly_ && !blk->isArchived())
    ThrowError("Register: " << regmap_name << "." 
	       << board_name << "." << block_name
	       << " isn't archived");

  size_ = RegParser::getSize(blk, aspect, true);

  // Initialize the output register specification.
  
  iRegMap_    = arregmap->number;
  iBoard_     = blk->brd_->number;
  iBlock_     = blk->number_;
  iSlot_      = arregmap->slotOffsetInArrayMapOf(archivedOnly_, blk);
  iByte_      = arregmap->byteOffsetInArrayMapOf(archivedOnly_, blk);
  aspect_     = aspect;
  outputMode_ = REG_OUTPUT_RANGE;

  integ_      = integ;
  nBytePerEl_ = blk->nBytePerEl_;

  // Set the coordinate range for this register

  range_ = coordRange;

  // Fill in any axis dimensions that weren't specified

  axes_ = blk->axes_;
  axes_.fillRange(range_);

  // Store the total number of elements in this range

  nEl_  = axes_.nEl(range_);

  axisRange_->setTo(*this, &range_);
}

/*.......................................................................
 * Output a register specification to a text output stream, using the
 * format as expected by input_RegMapReg().
 *
 * Input:
 *  stream  OutputStream *  The text stream to write to.
 *  regmap        RegMap *  The register map that contains the register.
 *  mode   RegOutputMode    The format to use to write the register, from:
 *                            REG_OUTPUT_BLOCK   - board.register
 *                            REG_OUTPUT_ELEMENT - board.register[index]
 *                            REG_OUTPUT_RANGE   - board.register[index-index]
 *  reg        RegMapReg *  The specification to be recorded.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
ostream& sza::util::operator<<(std::ostream& os, RegDescription& desc)
{
  RegMapBoard* brd; // The board specified in *arreg 
  RegMapBlock* blk; // The block specified in *arreg 
  ArrRegMap* arregmap = NULL;
  RegMap* regmap      = NULL;
  
  // Get the board and block objects that correspond to the
  // specifications in *reg.
  
  if(desc.iRegMap_ < 0 || desc.iRegMap_ >= desc.arrayMap_->nregmap) 
    ThrowError("Board index (" << desc.iRegMap_ << ") out of range");
  
  arregmap = desc.arrayMap_->regmaps[desc.iRegMap_];
  regmap   = arregmap->regmap;
  
  if(desc.iBoard_ < 0 || desc.iBoard_ >= regmap->nboard_) 
    ThrowError("Board index (" << desc.iBoard_ << ") out of range");
  
  brd = regmap->boards_[desc.iBoard_];
  
  if(desc.iBlock_ < 0 || desc.iBlock_ >= brd->nblock) 
    ThrowError("Board index (" << desc.iBlock_ << ") out of range");
  
  blk = brd->blocks[desc.iBlock_];
  
  // Check that the index and nreg values make sense. Note that
  // reg->index is unsigned, so there is no need to check whether it
  // is < 0.
  
  if(!desc.axes_.rangeIsValid(desc.range_)) 
    ThrowError("Slot numbers out of range");
  
  // Check that the mode and the number of elements make sense
  
  if(desc.outputMode_ == REG_OUTPUT_ELEMENT && desc.nEl_ > 1)
    ThrowError("Too many elements " << desc.range_ 
	       << " selected for single-element output");
  
  // Output the fully qualified name of the register.
  
  os << arregmap->name << "."
     << brd->name << "."
     << blk->name_;
  
  // Write the aspect if needed.
  
  if(desc.aspect_ != REG_PLAIN) 
    os << "." << name_RegAspect(desc.aspect_);

  // Write the integration status, if needed.

  if(desc.integ_ != REG_INT_PLAIN) 
    os << "." << name_RegInteg(desc.integ_);
  
  // Write the appropriate (if any) index specification.
  
  if(desc.outputMode_ != REG_OUTPUT_BLOCK)
    os << desc.range_;
  
  return os;
}

/**.......................................................................
 * Set the output mode of a register specification
 */
void RegDescription::setOutputMode(RegOutputMode mode)
{
  outputMode_ = mode;
}

/**.......................................................................
 * Old-style: write this register onto an output stream
 */
void RegDescription::output(OutputStream* stream, RegOutputMode mode)
{
  setOutputMode(mode);
  
  ostringstream os;
  
  os << *this;
  
  if(write_OutputStream(stream, os.str().c_str())) {
    LogStream errStr;
    errStr.appendMessage(true, "Error writing to output stream");
    throw Error(errStr);
  }
}

/**.......................................................................
 * Return a vector of element ranges associated with this register
 * description
 */
vector<Range<unsigned> > RegDescription::getElementRanges(CoordRange* range)
{
  return axes_.getRanges(range==0 ? range_ : *range);
}

/**.......................................................................
 * Return a vector of slot ranges associated with this register
 * description
 */
vector<Range<unsigned> > RegDescription::getSlotRanges(CoordRange* range)
{
  vector<Range<unsigned> > ranges = getElementRanges(range);
  
  // Convert from element offsets from the beginning of the register
  // to byte offsets from the beginning of the array map
  
  for(unsigned iRange=0; iRange < ranges.size(); iRange++) 
    ranges[iRange] += iSlot_;
  
  return ranges;
}

/**.......................................................................
 * Return a vector of byte ranges associated with this register
 * description
 */
vector<Range<unsigned> > RegDescription::getByteRanges(CoordRange* coordRange)
{
  vector<Range<unsigned> > ranges = getElementRanges(coordRange);
  
  // Convert from element offsets from the beginning of the register
  // to byte offsets from the beginning of the array map
  
  for(unsigned iRange=0; iRange < ranges.size(); iRange++) {
    Range<unsigned>& range = ranges[iRange];
    unsigned start = range.start();
    unsigned stop  = range.stop();

    // The start byte will simply be the offset of this element from
    // the head of the register array

    range.setStart(iByte_ + start * nBytePerEl_);

    // Stop byte will be the number of elements selected
    // (iStop-iStart+1), times the number of bytes per element, minus
    // 1 (for the inclusive range of bytes covered by this register
    // selection)

    range.setStop(iByte_ + (stop + 1) * nBytePerEl_ - 1);
  }
  
  return ranges;
}

/**.......................................................................
 * Return the total number of bytes covered by the register in this
 * object
 */
unsigned RegDescription::nByte(CoordRange* range)
{
  return nEl(range) * nBytePerEl_;
}

/**.......................................................................
 * Return the total number of elements covered by the register in this
 * object
 */
unsigned RegDescription::nEl(CoordRange* range)
{
  return axes_.nEl(range==0 ? range_ : *range);
}

/**.......................................................................
 * Return the total number of elements covered by the register in this
 * object
 */
unsigned RegDescription::nSlot(CoordRange* range)
{
  return nEl(range);
}

/**.......................................................................
 * Return the start slot of this range in the array map
 */
int RegDescription::startSlot(CoordRange* range)
{
  Coord startCoord(range==0 ? range_.startCoord() : range->startCoord());

  return (iSlot_ < 0) ? iSlot_ : 
    iSlot_ + axes_.elementOffsetOf(startCoord);
}

/**.......................................................................
 * Return the stop slot of this range in the array map
 */
int RegDescription::stopSlot(CoordRange* range)
{
  Coord stopCoord(range==0 ? range_.stopCoord() : range->stopCoord());

  return (iSlot_ < 0) ? iSlot_ : 
    iSlot_ + axes_.elementOffsetOf(stopCoord);
}

/**.......................................................................
 * Check if a range is consistent with the axes in this register
 */
bool RegDescription::rangeIsValid(CoordRange& range)
{
  return axes_.rangeIsValid(range);
}

/**.......................................................................
 * Check if a range is consistent with the axes in this register
 */
bool RegDescription::rangeIsValid(CoordRange* range)
{
  return axes_.rangeIsValid(range);
}

/**.......................................................................
 * Re-set the coordinate for this container
 */
Coord RegDescription::currentCoord()
{
  return axisRange_->currentCoord();
}

/**.......................................................................
 * Re-set the coordinate for this container
 */
void RegDescription::setCoord(Coord& coord)
{
  CoordRange range;

  range.setStartCoord(coord);
  range.setStopCoord(coord);
  
  if(axes_.rangeIsValid(range))
    range_ = range;
  else
    ThrowError("Invalid coordinate for register: " << *this);

  // And reset the axis range

  axisRange_->setTo(*this, &range_);

  // And update the number of elements in the range

  nEl_  = axes_.nEl(range_);
}

/**.......................................................................
 * Return the regmap descriptor associated with this register
 */
ArrRegMap* RegDescription::regMap()
{
  if(iRegMap_ < 0)
    ThrowError("Register map is NULL");

  return arrayMap_->regmaps[iRegMap_];
}

/**.......................................................................
 * Return the block descriptor associated with this register
 */
RegMapBlock* RegDescription::block()
{
  if(iRegMap_ < 0 || iBoard_ < 0 || iBlock_ < 0)
    ThrowError("Block is NULL");

  return arrayMap_->regmaps[iRegMap_]->regmap->boards_[iBoard_]->blocks[iBlock_];
}

/**.......................................................................
 * Return the block descriptor associated with this register
 */
std::string RegDescription::blockName()
{
  if(iRegMap_ < 0 || iBoard_ < 0 || iBlock_ < 0)
    ThrowError("Block is NULL");

  return std::string(arrayMap_->regmaps[iRegMap_]->regmap->boards_[iBoard_]->blocks[iBlock_]->name_);
}

std::string RegDescription::aspectName()
{
  return std::string(name_RegAspect(aspect_));
}

/**.......................................................................
 * Return the board descriptor associated with this register
 */
std::string RegDescription::boardName()
{
  if(iRegMap_ < 0 || iBoard_ < 0 || iBlock_ < 0)
    ThrowError("Block is NULL");

  return std::string(arrayMap_->regmaps[iRegMap_]->regmap->boards_[iBoard_]->name);
}

/**.......................................................................
 * Return the regmap descriptor associated with this register
 */
std::string RegDescription::regMapName()
{
  if(iRegMap_ < 0 || iBoard_ < 0 || iBlock_ < 0)
    ThrowError("Block is NULL");

  return std::string(arrayMap_->regmaps[iRegMap_]->name);
}

/**.......................................................................,
 * Begin an iteration
 */
void RegDescription::begin()
{
  axisRange_->reset();
}

/**.......................................................................,
 * Return true if we are at the end of an iteration
 */
bool RegDescription::isEnd()
{
  return axisRange_->isEnd();
}

/**.......................................................................
 * Increment this object
 */
const RegDescription& RegDescription::operator++()
{
  ++(*axisRange_);
}

/**.......................................................................
 * Return the archive slot corresponding to the current element
 */
unsigned RegDescription::currentSlot()
{
  return axisRange_->currentSlot();
}

/**.......................................................................
 * Return the current element
 */
unsigned RegDescription::currentElement()
{
  return axisRange_->currentElement();
}

bool RegDescription::contains(RegDescription& desc)
{
  return (aspect_ == desc.aspect_) && 
    (iRegMap_ == desc.iRegMap_) && (iBoard_ == desc.iBoard_) && 
    (iBlock_ == desc.iBlock_) && range_.contains(desc.range_);
}

void RegDescription::setFirst(bool first)
{
  first_ = first;
}

void RegDescription::setOldestVisible(long oldest) 
{
  oldestVisible_ = oldest;
}

void RegDescription::resetOldestVisible() 
{
  oldestVisible_ = -1;
}

void RegDescription::decreaseOldestVisible(long oldest) 
{
  if(oldestVisible_ < 0 || oldest < oldestVisible_)
    oldestVisible_ = oldest;
}

void RegDescription::increaseOldestVisible(long oldest) 
{
  if(oldestVisible_ < 0 || oldest > oldestVisible_)
    oldestVisible_ = oldest;
}

unsigned RegDescription::fastestNel()
{
  return range_.nEl(range_.nAxis()-1);
}
