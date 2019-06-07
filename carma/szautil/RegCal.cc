#define __FILEPATH__ "util/RegCal.cc"

#include "carma/szautil/MonitorDataType.h"
#include "carma/szautil/RegAxisRange.h"
#include "carma/szautil/RegCal.h"
#include "carma/szautil/RegParser.h"
#include "carma/szautil/Debug.h"

#include "carma/szaarrayutils/szaconst.h"
#include <climits>

using namespace sza::util;
using namespace std;

/**.......................................................................
 * Constructor for RegCalSlot
 */
RegCal::RegCalSlot::RegCalSlot() 
{
  offset_   = 0.0;
  factor_   = 1.0;
  imagGain_ = 1.0;
  sinPhi_   = 0.0;
  cosPhi_   = 1.0;
};

/**.......................................................................
 * Empty Destructor for RegCalSlot
 */
RegCal::RegCalSlot::~RegCalSlot() {}

/**.......................................................................
 * Reset calibation factors
 */
void RegCal::RegCalSlot::reset()
{
  offset_   = 0.0;
  factor_   = 1.0;
  imagGain_ = 1.0;
  sinPhi_   = 0.0;
  cosPhi_   = 1.0;
};

/**.......................................................................
 * Create a new register calibration object from a given register map
 * and a given register calibration file.
 *
 * Input:
 *  regmap       RegMap *  The register map.
 * Output:
 *  return       RegCal *  The new register calibration object, or NULL
 *                         on error.
 */
RegCal::RegCal(ArrayMap* arrayMap, bool archivedOnly) :
  archivedOnly_(archivedOnly)
{
  arrayMap_ = (arrayMap == 0) ? arrayMapBase_.arrayMap() : arrayMap;
  calData_ = 0;

  // Before attempting any operation that might fail, initialize the
  // container at least up to the point at which it can safely be
  // passed to del_RegCal().
  
  nSlot_           = archivedOnly_ ? arrayMap_->narchive : 
    arrayMap_->nreg;  nsnapSlot_       = -1;
  nsnapByteOffset_ = -1;
  
  // Allocate and initialize the calibration array.
  
  for(unsigned i=0; i < nSlot_; i++) 
    slots_.push_back(RegCalSlot());
  
  // Determine the location of the integration count register.  During
  // calibration this register is read to determine how to turn
  // integrations into means.
  
  nsnapSlot_       = arrayMap_->slotOffsetOf(archivedOnly_, 
					     "array", "frame", "nsnap");

  nsnapByteOffset_ = arrayMap_->byteOffsetOf(archivedOnly_, 
					     "array", "frame", "nsnap");

  // Allocate the object which will manage the archived data

  calData_ = new RegCalData(arrayMap_, archivedOnly_);

  if(calData_ == 0) 
    ThrowError("Cal data couldn't be allocated");
}

/**.......................................................................
 * Copy constructor
 */
RegCal::RegCal(RegCal& regCal)
{
  arrayMap_        = regCal.arrayMap_;
  archivedOnly_    = regCal.archivedOnly_;
  slots_           = regCal.slots_;
  nsnapSlot_       = regCal.nsnapSlot_;
  nsnapByteOffset_ = regCal.nsnapByteOffset_;
}

/*.......................................................................
 * Delete a register calibration object.
 *
 * Input:
 *  regcal    RegCal *  The object to be deleted.
 * Output:
 *  return    RegCal *  Always NULL.
 */
RegCal::~RegCal() 
{
  if(calData_ != 0) {
    delete calData_;
    calData_ = 0;
  }
}

/**.......................................................................
 * Load a calibration file and record the results in regcal->slots[].
 */
void RegCal::loadCalFile(string dir, string name, bool doThrow)
{
  InputStream* stream=0;   /* The stream to attach to the cal file */
  
  // Allocate an input stream, attach it to the file and load the
  // file.
  
  if((stream = new_InputStream())==NULL ||
     open_FileInputStream(stream, (char*)dir.c_str(), (char*)name.c_str()))
    ThrowError("Error opening calibration file: " << dir << "/" << name);
  
  // Else load the calibration from the stream
  
  try {
    loadCalStream(stream, doThrow);
  } catch(Exception& err) {
    stream = del_InputStream(stream);
    ThrowError(err.what());
  }
  
  // Delete the redundant stream.
  
  stream = del_InputStream(stream);
}

/*.......................................................................
 * Read register calibration factors from an input stream, and record
 * them in regcal->slots[].
 *
 * Input:
 *  regmap      RegMap *  The register map associated with the registers
 *                        being calibrated.
 *  regcal      RegCal *  The register calibration object to be updated.
 *  stream InputStream *  The stream to read the calibration information
 *                        from.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
void RegCal::loadCalStream(InputStream* stream, bool doThrow)
{
  // A class for parsing register specifications
  
  RegParser parser(archivedOnly_); 
  
  // Check inputs.
  
  if(!stream) 
    ThrowError("NULL stream argument");
  
  if(arrayMap_->nReg(archivedOnly_) != nSlot_)
    ThrowError("Array map incompatible with cal object");
  
  // Discard any existing calibrations.
  
  reset();
  
  // Skip leading white-space.
  
  if(input_skip_white(stream, 1, 0))
    ThrowError("Error in input_skip_white()");
  
  // Read register calibration records until the end of the file or an
  // error is encountered.
  
  while(stream->nextc != EOF) {
    
    try {
      
      // Decode the next register name.

      vector<RegDescription> regs;
   
      try {
	regs = parser.inputRegs(stream, 1, arrayMap_, 
				REG_INPUT_RANGE, true, false, doThrow);
      } catch(Exception& err) {
      }
      
      switch(parser.validity()) {
	
	// A valid register specification was read and summarized in
	// 'reg'.  Read the accompanying calibrations parameters.
	
      case REG_VALID:
	try {
	  readCalGroup(stream, regs);
	} catch(...) {
	  skipCalGroup(stream);
	  ThrowError("Calibration aborted");
	}
	break;
	
	// A syntactically correct, but unknown register specification
	// was read.  In order to allow one to use the same calibration
	// file with old and new data even when registers have been
	// added or removed from the register map of that file wrt the
	// calibration file, simply skip the calibration parameters of
	// unknown registers.
	
      case REG_UNKNOWN:
	skipCalGroup(stream);
	//ThrowError("Calibration aborted");
	break;
	
	// A corrupt register specification was read.
	
      default:
	ThrowError("Calibration aborted");
	break;
      };
    } catch(...) {
    }
  };
}

/*.......................................................................
 * Read the calibration parameters for a given selection of register
 * elements.
 *
 * Input:
 *  stream InputStream *  The stream to read the calibration information
 *                        from.
 *  regmap      RegMap *  The register map associated with the registers
 *                        being calibrated.
 *  regcal      RegCal *  The register calibration object to be updated.
 *  reg   ArrRegMapReg *  The specification of the register elements
 *                        to be calibrated.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
void RegCal::readCalGroup(InputStream *stream, vector<RegDescription>& regs)
{
  ArrRegMap* arregmap;
  RegMapBoard *brd;   // The parent board of the selected register 
  RegMapBlock *blk;   // The block containing the selected register 
  
  // We can't calibrate unarchived registers.
  
  for(unsigned iReg=0; iReg < regs.size(); iReg++) {
    RegDescription& desc = regs[iReg];
    
    // Get the descriptions of the board and block that contain the
    // register specification.
    
    arregmap = arrayMap_->regmaps[desc.iRegMap()];
    brd      = arregmap->regmap->boards_[desc.iBoard()];
    blk      = brd->blocks[desc.iBlock()];
    
    
    if(desc.iSlot() < 0) {
      skipCalGroup(stream);
      ThrowError("Skipping unarchived register " 
		 << arregmap->name << "." << brd->name << "." << blk->name_);
    };
  }
  
  // Find the start of the calibration parameters.
  
  if(input_skip_space(stream, 1, 0))
    ThrowError("Error in input_skip_space()");
  
  // The next character determines whether one or an array of
  // calibration parameters are to be assigned to the specified
  // register range.
  
  if(stream->nextc == '{') {   /* An array of parameter specifiers */
    
    // Skip the opening '{'.
    
    if(input_skip_white(stream, 1, 1)) 
      ThrowError("Error in input_skip_white()");
    
    unsigned ireg;
    
    // Iterate over all regs matching this specification
    
    for(unsigned iReg=0; iReg < regs.size(); iReg++) {
      RegDescription& desc = regs[iReg];
      
      // For each register, get the vector of contiguous frame index
      // ranges to be calibrated.  (Here we really _do_ want slots)
      
      vector<Range<unsigned> > ranges = desc.getSlotRanges();
      
      for(unsigned iRange=0; iRange < ranges.size(); iRange++) {
	
	Range<unsigned>& range = ranges[iRange];
	
	// Read one calibration set per register in the range.
	
	for(ireg = range.start(); ireg <= range.stop(); ireg++){
	  
	  // Read and record the calibration factors of the current
	  // register.
	  
	  double pars[4];
	  readCalRecord(blk, stream, pars);
	  installCalPars(pars, blk, ireg, ireg);
	  
	  // Each set of calibration parameters must be separated from
	  // those of the next register slot by a comma. The last must be
	  // followed by the brace that terminates the list.
	  
	  switch(stream->nextc) {
	  case '}': // If this is the end of the set, we should be on
	    // the last range, and the last index
	    if(!(iRange == ranges.size()-1 && ireg == range.stop()))
	      ThrowError("Too few calibration parameters for register: "
			 << desc);
	    break;
	  case ',':
	    if(iRange == ranges.size()-1 && ireg == range.stop())
	      ThrowError("Too many calibration parameters for register: "
			 << desc);
	    break;
	  default:
	    if(iRange == ranges.size()-1 && ireg == range.stop()) {
	      ThrowError("Missing '}' after final calibration parameter");
	    } else {
	      ThrowError("Missing ',' after valid calibration parameter");
	    }
	    break;
	  };
	  
	  // Skip the delimiter.
	  
	  if(input_skip_space(stream, 1, 1)) 
	    ThrowError("Error in input_skip_white()");
	};
      };
    };
    
    // Read a single register calibration and record it as the
    // calibration of all of the registers in the range.
    
  } else {
    
    // Read the cal factors
    
    double pars[4];
    readCalRecord(blk, stream, pars);
    
    // Iterate over all contiguous slot ranges, installing the
    // parameters as the cal factors
    
    
    for(unsigned iReg=0; iReg < regs.size(); iReg++) {
      RegDescription& desc = regs[iReg];
      
      vector<Range<unsigned> > ranges = desc.getSlotRanges();
      
      arregmap = arrayMap_->regmaps[desc.iRegMap()];
      brd      = arregmap->regmap->boards_[desc.iBoard()];
      blk      = brd->blocks[desc.iBlock()];
      
      for(unsigned iRange=0; iRange < ranges.size(); iRange++) 
	installCalPars(pars, blk, ranges[iRange].start(), ranges[iRange].stop());
    }
  }
  
  // We should be at the end of a line.
  
  if(stream->nextc != '\n' && stream->nextc != EOF) 
    ThrowError("Unexpected characters follow the last calibration parameter");
  
  /*
   * Skip trailing white-space up to the next record.
   */
  if(input_skip_white(stream, 1, 0))
    ThrowError("Error in input_skip_white()");
}

/*.......................................................................
 * Skip to the start of the next register name in a calibration stream,
 * ignoring intervening calibration parameters. This can be used to skip
 * an errant entry.
 *
 * Input:
 *  stream     InputStream *  The stream to read from.
 * Output:
 *  return             int    0 - OK.
 *                            1 - Error.
 */
void RegCal::skipCalGroup(InputStream *stream)
{
  /*
   * Find the start of the register name that initiates the next calibration
   * specification. This is identified by searching for the next alphabetical
   * character that follows a newline (and zero or more spaces).
   */
  do {
    if(input_skip_past_eol(stream, 1) ||
       input_skip_space(stream, 1, 0))
      ThrowError("Error");
  } while(!isalpha(stream->nextc));
}

/*.......................................................................
 * Read a single set of register calibration parameters and record them
 * as the calibration of a given range of registers.
 *
 * Input:
 *  regcal      RegCal *  The register calibration container.
 *  board  RegMapBoard *  The register map of a given board.
 *  block  RegMapBlock *  The details of a given block of the specified
 *                        board.
 *  ia,ib          int    The index range of elements in the output
 *                        slot array.
 *  stream InputStream *  The stream to read the record from.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
void RegCal::readCalRecord(RegMapBlock *block, 
			   InputStream *stream,
			   double pars[4])
{
  int npar;         /* The number of parameters to be read */
  int ipar;         /* The index of the parameter that is being read */
  unsigned islot;   /* The slot index that is being updated */
  
  // Determine the number of calibration parameters needed for the the
  // calibration mode of the given block.
  
  npar = (block->flags_ & REG_COMPLEX) ? 4 : 2;
  
  // Read 'npar' double precision numbers separated by spaces or
  // horizontal tabs.
  
  for(ipar = 0; ipar < npar; ipar++) {
    if(input_double(stream, 0, pars + ipar))
      ThrowError("Calibration parameter " << ipar+1
		 << "doesn't appear to be a number");
    
    if(input_skip_space(stream, 1, 0))
      ThrowError("Error in input_skip_space()");
    
    // If the next character is a '/', read the number that follows it
    // and use it to divide the previous number.
    
    if(stream->nextc == '/') {
      double divisor;
      
      // Skip the '/' and read the divisor that follows it.
      
      if(input_skip_space(stream, 1, 1))
	ThrowError("Error in input_skip_space()");
      
      if(input_double(stream, 0, &divisor))
	ThrowError("Missing divisor after '/'");
      
      // Check for zero divisor.
      
      if(divisor == 0.0)
	ThrowError("Illegal zero divisor");
      
      pars[ipar] /= divisor;
      
      // Find the next token.
      
      if(input_skip_space(stream, 1, 0))
	ThrowError("Error in input_skip_space()");
    };
  };
  
  // Skip trailing spaces.
  
  if(input_skip_space(stream, 1, 0))
    ThrowError("Error in input_skip_space()");
}

/**.......................................................................
 * Install parameters read from a cal record as the calibation factors
 * for a range of slots
 */
void RegCal::installCalPars(double pars[4], RegMapBlock* blk, 
			    unsigned ia, unsigned ib)
{
  // Record the calibration information.
  
  if(blk->flags_ & REG_COMPLEX) {
    double offset   = pars[0];
    double factor   = pars[1];
    double imagGain = pars[2];
    double sinPhi   = sin(dtor * pars[3]);
    double cosPhi   = cos(dtor * pars[3]);
    
    for(unsigned iSlot = ia; iSlot <= ib; iSlot++) {
      RegCalSlot& slot = slots_[iSlot];
      
      slot.offset_   = offset;
      slot.factor_   = factor;
      slot.imagGain_ = imagGain;
      slot.sinPhi_   = sinPhi;
      slot.cosPhi_   = cosPhi;
    };
    
  } else {
    double offset = pars[0];
    double factor = pars[1];
    for(unsigned iSlot = ia; iSlot <= ib; iSlot++) {
      RegCalSlot& slot = slots_[iSlot];
      
      slot.offset_ = offset;
      slot.factor_ = factor;
    };
  };
}

/*.......................................................................
 * Apply the current calibration factors to an array of uncalibrated
 * registers.
 *
 * Input:
 *  regmap    RegMap *   The register map of the SZA.
 *  regcal    RegCal *   The calibration information to be applied.
 *  regset    RegSet *   The set of registers in 'raw' to be calibrated.
 *  raw   RegRawData *   The input registers to be calibrated.
 *  cal   RegCalData *   The output calibrated registers.
 * Output:
 *  return       int     0 - OK.
 *                       1 - Error.
 */
void RegCal::calibrateRegData(RegisterSet* registerSet,
			      RegRawData* raw)
{
  if(registerSet == 0)
    ThrowError("Register set argument is NULL");

  calibrateRegData(registerSet->regSet(), raw->fm);
}

/*.......................................................................
 * Apply the current calibration factors to an array of uncalibrated
 * registers.
 *
 * Input:
 *  regmap    RegMap *   The register map of the SZA.
 *  regcal    RegCal *   The calibration information to be applied.
 *  regset    RegSet *   The set of registers in 'raw' to be calibrated.
 *  raw   RegRawData *   The input registers to be calibrated.
 *  cal   RegCalData *   The output calibrated registers.
 * Output:
 *  return       int     0 - OK.
 *                       1 - Error.
 */
void RegCal::calibrateRegData(RegisterSet& regset,
			      ArrayDataFrameManager& fm)
{
  calibrateRegData(regset.regSet(), &fm);
}

/*.......................................................................
 * Apply the current calibration factors to an array of uncalibrated
 * registers.
 *
 * Input:
 *  regmap    RegMap *   The register map of the SZA.
 *  regcal    RegCal *   The calibration information to be applied.
 *  regset    RegSet *   The set of registers in 'raw' to be calibrated.
 *  raw   RegRawData *   The input registers to be calibrated.
 *  cal   RegCalData *   The output calibrated registers.
 * Output:
 *  return       int     0 - OK.
 *                       1 - Error.
 */
void RegCal::calibrateRegData(RegSet* regset,
			      ArrayDataFrameManager* fm)
{
  RegSetRange *range;    // A register selection range
  unsigned nsnap;        // The integration count

  // Check the arguments.
  
  if(!regset || !fm) 
    ThrowError("NULL " 
	       << (!regset ? "regset":"fm") << " argument");
  
  if(nSlot_ != size_RegSet(regset, archivedOnly_) || 
     !equiv_ArrayMap(arrayMap_, fm->arrayMap())) 
    ThrowError("Incompatible arguments");
  
  // Get the value of the "nsnap" register. This records the number of
  // hardware integrations that were added in software.
  
  fm->readReg("array", "frame", "nsnap", &nsnap);
  
  if(nsnap < 1)
    nsnap = 1;

  // Get the container object of the first register map of the array map,
  // the container object of the first board of the register map,
  // and the container of its first block.
  
  ArrRegMap**    arregmap = &arrayMap_->regmaps[0];   
  RegMapBoard**  brd      = &((*arregmap)->regmap->boards_[0]);
  RegMapBlock**  blk      = &((*brd)->blocks[0]);
  
  // Traverse the list of register-selection ranges to calibrate the
  // included register elements.
  
  for(range=regset_range_list(regset); range != NULL; range = range->next) {
    
    // Each range contains a start and stop byte index
    
    RegRange regRange(range->ia, range->ib, archivedOnly_, arrayMap_);
    
    // Iterate over all discrete register elements, extracting and
    // renormalizing
    
    while(!regRange.isEnd()) {
      
      RegMapBlock* blk = regRange.currentBlock();
      unsigned iSlot   = regRange.currentSlot();
      
      // Get a double representation of the raw data value
      
      calData_->slots_[iSlot] = fm->getRegVal(regRange, false);

      // Apply complex calibrations.
      
      RegCalSlot& calSlot = slots_[iSlot];

      if(blk->isComplex()) {
	Complex<float>::Data* dptr = 
	  (Complex<float>::Data*)&calData_->slots_[iSlot];

	float re = dptr->real_;
	float im = dptr->imag_;
	
	// Convert integrated register values to means if the register
	// is to be averaged
	
	if(blk->isPostAveraged()) {
	  re /= nsnap;
	  im /= nsnap;
	}

	dptr->real_ = calSlot.offset_ + re * calSlot.factor_;
	dptr->imag_ = calSlot.offset_ + calSlot.factor_ *
	  (im/calSlot.imagGain_ + re*calSlot.sinPhi_) /
	  calSlot.cosPhi_;
	
	// Else apply real-number calibrations.
	
      } else if(!blk->isUtc()) {
	
	// Convert integrated register values to means.
	
	if(blk->isPostAveraged())
	  calData_->slots_[iSlot] /= nsnap;
	
	calData_->slots_[iSlot] = calSlot.offset_ + calSlot.factor_ *
	  calData_->slots_[iSlot];
      }
      
      // Increment the range object
      
      ++regRange;
    }
  }
  
  // Since nsnap is an integrated register, it will now have been
  // reduced to unity. Return it to its original value.
  
  calData_->slots_[nsnapSlot_] = nsnap;
  
  // Record the succesful initialization of the cal object.
  
  calData_->empty_ = false;
}

/*.......................................................................
 * Reset the calibrations to 0 offset and unit gain.
 *
 * Input:
 *  regcal    RegCal *   The calibration object to be reset.
 * Output:
 *  return       int     0 - OK.
 *                       1 - Error.
 */
void RegCal::reset()
{
  // Reset each calibration entry to zero offset and unit gain.
  
  for(unsigned iSlot=0; iSlot < nSlot_; iSlot++) 
    slots_[iSlot].reset();
}

/*.......................................................................
 * Create a new array in which to store the values of calibrated
 * registers.
 *
 * Input:
 *  regmap       RegMap *  The register map.
 * Output:
 *  return   RegCalData *  The new container, or NULL on error.
 */
RegCal::RegCalData::RegCalData(ArrayMap* arrayMap, bool archivedOnly)
{
  // Check arguments.
  
  if(!arrayMap) 
    ThrowError("NULL arrayMap argument");
  
  // Before attempting any operation that might fail, initialize the
  // container at least up to the point at which it can safely be
  // passed to del_RegCalData().
  
  nSlot_ = arrayMap->nReg(archivedOnly);
  empty_ = 1;
  
  // Allocate and initialize the array of register slots.
  
  for(unsigned iSlot=0; iSlot < nSlot_; iSlot++)
    slots_.push_back(0.0);
}

/*.......................................................................
 * Delete a monitor array object.
 *
 * Input:
 *  cal      RegCalData *  The object to be deleted.
 * Output:
 *  return   RegCalData *  Always NULL.
 */
RegCal::RegCalData::~RegCalData() {}

/**.......................................................................
 * This is an argument validation function used by the get_cal_...()
 * functions.
 *
 * Input:
 *  caller        char *  The name of the calling function.
 *  cal     RegCalData *  The calibrated frame of selected registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned.
 *  n         unsigned    The number of register elements to return.
 *                        *reg must cover all elements in the index
 *                        range index..index+n-1.
 * Input/Output:
 *  data          void *  The output array.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
void RegCal::RegCalData::
checkCalArgs(string caller, RegDescription* reg, void* data, CoordRange* range)
{
  if(data==0 || reg==0) 
    ThrowError(caller << " NULL argument(s)");
  
  if(range != 0 && !reg->rangeIsValid(range))
    ThrowError("Range is invalid");
  
  if(reg->iSlot() >= 0) {
    
    // Check if the range of indices requested exceeds what is covered
    // by this register specification
    
    if(range !=0 && reg->axes().nEl(range) > reg->nEl()) {
      ThrowError(caller << ": Index range is too large");
      
      // Finally, check if the max slot number exceeds the size of the
      // cal slot array
      
    } else if(reg->stopSlot() > nSlot_) {
      ThrowError(caller << ": Invalid register specifier");
    };
  };
}

/*.......................................................................
 * Read calibrated data into a provided array of float's.
 *
 * Input:
 *  cal     RegCalData *  The calibrated frame of selected registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data         float *  An array with room for n float elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
void RegCal::RegCalData::
getCalFloat(RegDescription* reg, float *data, CoordRange* range)
{
  // Check the arguments.
  
  // This should be done only once, not for every frame
  //checkCalArgs("getCalFloat", reg, data, range);
  
  // If a NULL range was passed, substitute the range from the
  // RegDescription

  RegAxisRange iRange(*reg, range);
  
  RegMapBlock* block = reg->block();

  // If the register isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()] = 0.0;
  } else {
    
    if(block->isUtc()) {

      RegDate date;
      date = *((RegDate::Data*)&slots_[iRange.currentSlot()]);
      data[iRange.currentIterator()] = date.mjd();

    } else if(block->isComplex()) {

      Complex<float> cVal 
	= *((Complex<float>::Data*)&slots_[iRange.currentSlot()]);
      data[iRange.currentIterator()] = cVal.amp();

    } else {

      // Retrieve the requested registers.
      
      for(iRange.reset(); !iRange.isEnd(); ++iRange)
	data[iRange.currentIterator()] = slots_[iRange.currentSlot()];
    }
  };
}

void RegCal::RegCalData::
getCalFloat(RegDescription* reg, MonitorDataType* data, RegAxisRange& iRange)
{
  // Check the arguments.
  
  RegMapBlock* block = reg->block();

  // If the register isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()].val.data_.f = 0.0;
  } else {
    
    if(block->isUtc()) {

      RegDate date;
      date = *((RegDate::Data*)&slots_[iRange.currentSlot()]);
      data[iRange.currentIterator()].val.data_.f = date.mjd();

    } else if(block->isComplex()) {

      Complex<float> cVal 
	= *((Complex<float>::Data*)&slots_[iRange.currentSlot()]);
      data[iRange.currentIterator()].val.data_.f = cVal.amp();

    } else {

      // Retrieve the requested registers.
      
      for(iRange.reset(); !iRange.isEnd(); ++iRange)
	data[iRange.currentIterator()].val.data_.f = slots_[iRange.currentSlot()];
    }
  };
}

/*.......................................................................
 * Read calibrated data into a provided array of double's.
 *
 * Input:
 *  cal     RegCalData *  The calibrated frame of selected registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data        double *  An array with room for n double elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
void RegCal::RegCalData::
getCalDouble(RegDescription* reg, double *data, CoordRange* range)
{
  // Check the arguments.
  
  checkCalArgs("getCalDouble", reg, data, range);
  
  RegAxisRange iRange(*reg, range);
  
  RegMapBlock* block = reg->block();

  // If the registers isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()] = 0.0;
  } else {
    
    if(block->isUtc()) {

      RegDate date;
      date = *((RegDate::Data*)&slots_[iRange.currentSlot()]);
      data[iRange.currentIterator()] = date.mjd();

    } else if(block->isComplex()) {

      Complex<float> cVal 
	= *((Complex<float>::Data*)&slots_[iRange.currentSlot()]);
      data[iRange.currentIterator()] = cVal.amp();

    } else {

    // Retrieve the requested registers.
    
      for(iRange.reset(); !iRange.isEnd(); ++iRange)
	data[iRange.currentIterator()] = slots_[iRange.currentSlot()];
    }
  };
}

void RegCal::RegCalData::
getCalDouble(RegDescription* reg, MonitorDataType* data, RegAxisRange& iRange)
{
  // Check the arguments.
  
  RegMapBlock* block = reg->block();
  
  // If the registers isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()].val.data_.d = 0.0;
  } else {
    
    if(block->isUtc()) {

      RegDate date;
      date = *((RegDate::Data*)&slots_[iRange.currentSlot()]);
      data[iRange.currentIterator()].val.data_.d = date.mjd();

    } else if(block->isComplex()) {

      Complex<float> cVal 
	= *((Complex<float>::Data*)&slots_[iRange.currentSlot()]);
      data[iRange.currentIterator()].val.data_.d = cVal.amp();

    } else {

    // Retrieve the requested registers.
    
      for(iRange.reset(); !iRange.isEnd(); ++iRange) {
	data[iRange.currentIterator()].val.data_.d = slots_[iRange.currentSlot()];
      }
    }
  };
}

void RegCal::RegCalData::
getCalDouble(RegDescription* reg, double* data, RegAxisRange& iRange)
{
  // Check the arguments.
  
  RegMapBlock* block = reg->block();
  
  // If the registers isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()] = 0.0;
  } else {
    
    if(block->isUtc()) {

      RegDate date;
      date = *((RegDate::Data*)&slots_[iRange.currentSlot()]);
      data[iRange.currentIterator()] = date.mjd();

    } else if(block->isComplex()) {

      Complex<float> cVal 
	= *((Complex<float>::Data*)&slots_[iRange.currentSlot()]);
      data[iRange.currentIterator()] = cVal.amp();

    } else {

    // Retrieve the requested registers.
    
      for(iRange.reset(); !iRange.isEnd(); ++iRange)
	data[iRange.currentIterator()] = slots_[iRange.currentSlot()];
    }
  };
}

/*.......................................................................
 * Read calibrated data into a provided array of short's.
 *
 * Input:
 *  cal     RegCalData *  The calibrated frame of selected registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data           short *  An array with room for n short elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
void RegCal::RegCalData::
getCalShort(RegDescription* reg, short *data, CoordRange* range)
{
  // Check the arguments.
  
  checkCalArgs("getCalShort", reg, data, range);
  
  RegAxisRange iRange(*reg, range);
  
  // If the registers isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()] = 0;
  } else {
    
    /*
     * Retrieve the requested registers. If the value to be converted
     * is outside of the legal range of an int, assign the nearest limit.
     */
    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      double d = slots_[iRange.currentSlot()];
      data[iRange.currentIterator()] = (short) d;
    };
  };
}

void RegCal::RegCalData::
getCalShort(RegDescription* reg, MonitorDataType* data, RegAxisRange& iRange)
{
  // If the registers isn't archived, substitute zeroes.

  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()].val.data_.s = 0;
  } else {
    
    /*
     * Retrieve the requested registers. If the value to be converted
     * is outside of the legal range of an short, assign the nearest limit.
     */
    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      double d = slots_[iRange.currentSlot()];
      data[iRange.currentIterator()].val.data_.s = (short)d;

    };
  };
}
/*.......................................................................
 * Read calibrated data into a provided array of unsigned short's.
 *
 * Input:
 *  cal     RegCalData *  The calibrated frame of selected registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data      unsigned *  An array with room for n unsigned short elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
void RegCal::RegCalData::
getCalUshort(RegDescription* reg, unsigned short* data, CoordRange* range)
{
  // Check the arguments.
  
  checkCalArgs("getCalUshort", reg, data, range);
  
  RegAxisRange iRange(*reg, range);
  
  // If the registers isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()] = 0;
  } else {
    
    /*
     * Retrieve the requested registers. If the value to be converted
     * to unsigned short is outside of the legal range of an unsigned short
     * assign the nearest limit.
     */
    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      double d = slots_[iRange.currentSlot()];
      data[iRange.currentIterator()] = (unsigned short)d;
    };
  };
}

void RegCal::RegCalData::
getCalUshort(RegDescription* reg, MonitorDataType* data, RegAxisRange& iRange)
{
  // If the registers isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()].val.data_.ui = 0;
  } else {
    
    /*
     * Retrieve the requested registers. If the value to be converted
     * to unsigned short is outside of the legal range of an unsigned short
     * assign the nearest limit.
     */
    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      double d = slots_[iRange.currentSlot()];
      data[iRange.currentIterator()].val.data_.us = (unsigned short)d;
    };
  };
}
/*.......................................................................
 * Read calibrated data into a provided array of unsigned int's.
 *
 * Input:
 *  cal     RegCalData *  The calibrated frame of selected registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data      unsigned *  An array with room for n unsigned int elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
void RegCal::RegCalData::
getCalUint(RegDescription* reg, unsigned *data, CoordRange* range)
{
  // Check the arguments.
  
  checkCalArgs("getCalUint", reg, data, range);
  
  RegAxisRange iRange(*reg, range);
  
  // If the registers isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()] = 0;
  } else {
    
    /*
     * Retrieve the requested registers. If the value to be converted
     * to unsigned int is outside of the legal range of an unsigned int
     * assign the nearest limit.
     */
    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      double d = slots_[iRange.currentSlot()];
      data[iRange.currentIterator()] = 
	d < 0.0 ? 0 : (d > UINT_MAX ? UINT_MAX : (unsigned) d);
    };
  };
}

void RegCal::RegCalData::
getCalUint(RegDescription* reg, MonitorDataType* data, RegAxisRange& iRange)
{
  // If the registers isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()].val.data_.ui = 0;
  } else {
    
    /*
     * Retrieve the requested registers. If the value to be converted
     * to unsigned int is outside of the legal range of an unsigned int
     * assign the nearest limit.
     */
    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      double d = slots_[iRange.currentSlot()];
      data[iRange.currentIterator()].val.data_.ui = 
	d < 0.0 ? 0 : (d > UINT_MAX ? UINT_MAX : (unsigned) d);
    };
  };
}


/*.......................................................................
 * Read calibrated data into a provided array of int's.
 *
 * Input:
 *  cal     RegCalData *  The calibrated frame of selected registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data           int *  An array with room for n int elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
void RegCal::RegCalData::
getCalInt(RegDescription* reg, int *data, CoordRange* range)
{
  // Check the arguments.
  
  checkCalArgs("getCalInt", reg, data, range);
  
  RegAxisRange iRange(*reg, range);
  
  // If the registers isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()] = 0;
  } else {
    
    /*
     * Retrieve the requested registers. If the value to be converted
     * is outside of the legal range of an int, assign the nearest limit.
     */
    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      double d = slots_[iRange.currentSlot()];
      data[iRange.currentIterator()] = 
	d < INT_MIN ? INT_MIN : (d > INT_MAX ? INT_MAX : (int) d);
    };
  };
}

void RegCal::RegCalData::
getCalInt(RegDescription* reg, MonitorDataType* data, RegAxisRange& iRange)
{
  // If the registers isn't archived, substitute zeroes.

  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()].val.data_.i = 0;
  } else {
    
    /*
     * Retrieve the requested registers. If the value to be converted
     * is outside of the legal range of an int, assign the nearest limit.
     */
    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      double d = slots_[iRange.currentSlot()];
      data[iRange.currentIterator()].val.data_.i = 
	d < INT_MIN ? INT_MIN : (d > INT_MAX ? INT_MAX : (int) d);
    };
  };
}


/*.......................................................................
 * Read calibrated data into a provided array of chars.
 *
 * Input:
 *  cal     RegCalData *  The calibrated frame of selected registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data           int *  An array with room for n int elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
void RegCal::RegCalData::
getCalChar(RegDescription* reg, char *data, CoordRange* range)
{
  // Check the arguments.
  
  checkCalArgs("getCalChar", reg, data, range);
  
  RegAxisRange iRange(*reg, range);
  
  // If the registers isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()] = 0;
  } else {
    
    /*
     * Retrieve the requested registers. If the value to be converted
     * is outside of the legal range of an int, assign the nearest limit.
     */
    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      double d = slots_[iRange.currentSlot()];
      data[iRange.currentIterator()] = (char)d;
    };
  };
}

void RegCal::RegCalData::
getCalChar(RegDescription* reg, MonitorDataType* data, RegAxisRange& iRange)
{
  // If the registers isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()].val.data_.c = 0;
  } else {
    
    /*
     * Retrieve the requested registers. If the value to be converted
     * is outside of the legal range of an int, assign the nearest limit.
     */
    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      double d = slots_[iRange.currentSlot()];
      data[iRange.currentIterator()].val.data_.c = (char)d;
    };
  };
}

/*.......................................................................
 * Read calibrated data into a provided array of unsigned chars.
 *
 * Input:
 *  cal     RegCalData *  The calibrated frame of selected registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data           int *  An array with room for n int elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
void RegCal::RegCalData::
getCalUchar(RegDescription* reg, unsigned char *data, CoordRange* range)
{
  // Check the arguments.
  
  checkCalArgs("getCalUchar", reg, data, range);
  
  RegAxisRange iRange(*reg, range);
  
  // If the registers isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()] = 0;
  } else {
    
    /*
     * Retrieve the requested registers. If the value to be converted
     * is outside of the legal range of an int, assign the nearest limit.
     */
    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      double d = slots_[iRange.currentSlot()];
      data[iRange.currentIterator()] = (unsigned char)d;
    };
  };
}

void RegCal::RegCalData::
getCalUchar(RegDescription* reg, MonitorDataType* data, RegAxisRange& iRange)
{
  // If the registers isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()].val.data_.c = 0;
  } else {
    
    /*
     * Retrieve the requested registers. If the value to be converted
     * is outside of the legal range of an int, assign the nearest limit.
     */
    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      double d = slots_[iRange.currentSlot()];
      data[iRange.currentIterator()].val.data_.c = (unsigned char)d;
    };
  };
}

/*.......................................................................
 * Read calibrated data into a provided array of unsigned long's.
 *
 * Input:
 *  cal     RegCalData *  The calibrated frame of selected registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data unsigned long *  An array with room for n unsigned long elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
void RegCal::RegCalData::
getCalUlong(RegDescription* reg, unsigned long *data, CoordRange* range)
{
  // Check the arguments.
  
  checkCalArgs("getCalULong", reg, data, range);
  
  RegAxisRange iRange(*reg, range);
  
  // If the registers isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()] = 0UL;
  } else {
    
    // Retrieve the requested registers. If the value to be converted
    // to unsigned long is outside of the legal range of an unsigned
    // long assign the nearest limit.
    
    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      double d = slots_[iRange.currentSlot()];
      data[iRange.currentIterator()] = 
	d < 0.0 ? 0 : (d > ULONG_MAX ? ULONG_MAX : (unsigned long) d);
    };
  };
}

void RegCal::RegCalData::
getCalUlong(RegDescription* reg, MonitorDataType* data, RegAxisRange& iRange)
{
  // If the registers isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()].val.data_.ul = 0UL;
  } else {
    
    // Retrieve the requested registers. If the value to be converted
    // to unsigned long is outside of the legal range of an unsigned
    // long assign the nearest limit.
    
    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      double d = slots_[iRange.currentSlot()];
      data[iRange.currentIterator()].val.data_.ul = 
	d < 0.0 ? 0 : (d > ULONG_MAX ? ULONG_MAX : (unsigned long) d);
    };
  };
}

/*.......................................................................
 * Read calibrated data into a provided array of long's.
 *
 * Input:
 *  cal     RegCalData *  The calibrated frame of selected registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data          long *  An array with room for n long elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
void RegCal::RegCalData::
getCalLong(RegDescription* reg, long *data, CoordRange* range)
{
  // Check the arguments.
  
  checkCalArgs("getCalLong", reg, data, range);
  
  RegAxisRange iRange(*reg, range);
  
  // If the registers isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()] = 0L;
  } else {
    
    /*
     * Retrieve the requested registers. If the value to be converted
     * is outside of the legal range of an int, assign the nearest limit.
     */
    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      double d = slots_[iRange.currentSlot()];
      data[iRange.currentIterator()] = 
	d < LONG_MIN ? LONG_MIN : (d > LONG_MAX ? LONG_MAX : (int) d);
    };
  };
}

void RegCal::RegCalData::
getCalLong(RegDescription* reg, MonitorDataType* data, RegAxisRange& iRange)
{
  // If the registers isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()].val.data_.l = 0L;
  } else {
    
    /*
     * Retrieve the requested registers. If the value to be converted
     * is outside of the legal range of an int, assign the nearest limit.
     */
    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      double d = slots_[iRange.currentSlot()];
      data[iRange.currentIterator()].val.data_.l = 
	d < LONG_MIN ? LONG_MIN : (d > LONG_MAX ? LONG_MAX : (int) d);
    };
  };
}

/**.......................................................................
 * Unpack a string from a calibrated register frame.
 *
 * Input:
 *  cal     RegCalData *  The calibrated frame of selected registers.
 *  reg    ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  length    unsigned    The max number of characters to be placed in
 *                        data (including the '\0' terminator).
 * Input/Output:
 *  string        char *  The array of nc characters to return the string
 *                        in.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
void RegCal::RegCalData::
getCalString(RegDescription* reg, char *string, unsigned length)
{
  // Check the arguments.
  
  checkCalArgs("getCalString", reg, string);
  
  if(length < reg->nSlot())
  {
    ThrowError("String is too long for the buffer");
  }

  if(reg->nSlot() < 1) 
    ThrowError("Your zero-length string can't be terminated");
  
  // If the registers isn't archived, substitute an empty string.
  
  if(reg->iSlot() < 0) {
    string[0] = '\0';
  } else {
    
    // Get a pointer to the first element of the calibrated data array
    // that corresponds to the start of the selected range.
    
    unsigned iStartSlot = reg->startSlot();

    for(unsigned i=0; i < reg->nSlot(); i++)
      string[i] = (char)slots_[iStartSlot+i];
  };
}

/*.......................................................................
 * Read calibrated data into a provided array of float's.
 *
 * Input:
 *  cal     RegCalData *  The calibrated frame of selected registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data         float *  An array with room for n float elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
void RegCal::RegCalData::
getCalDate(RegDescription* reg, RegDate::Data* data, CoordRange* range)
{
  // Check the arguments.
  
  checkCalArgs("getCalDate", reg, data, range);
  
  // If a NULL range was passed, substitute the range from the
  // RegDescription

  RegAxisRange iRange(*reg, range);
  
  // If the register isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {

    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      data[iRange.currentIterator()].dayNo_ = 0;
      data[iRange.currentIterator()].mSec_  = 0;
    }

  } else {

    // Retrieve the requested registers.
    
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()] = 
	*((RegDate::Data*)&slots_[iRange.currentSlot()]);
  };
}

void RegCal::RegCalData::
getCalDate(RegDescription* reg, MonitorDataType* data, RegAxisRange& iRange)
{
  // If the register isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {

    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      data[iRange.currentIterator()].val.data_.date.dayNo_ = 0;
      data[iRange.currentIterator()].val.data_.date.mSec_  = 0;
    }

  } else {

    // Retrieve the requested registers.
    
    for(iRange.reset(); !iRange.isEnd(); ++iRange)
      data[iRange.currentIterator()].val.data_.date = 
	*((RegDate::Data*)&slots_[iRange.currentSlot()]);
  };
}

/*.......................................................................
 * Read calibrated data into a provided array of float's.
 *
 * Input:
 *  cal     RegCalData *  The calibrated frame of selected registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data         float *  An array with room for n float elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
void RegCal::RegCalData::
getCalComplexFloat(RegDescription* reg, Complex<float>::Data* data, 
		   CoordRange* range)
{
  // Check the arguments.
  
  checkCalArgs("getCalComplexFloat", reg, data, range);
  
  // If a NULL range was passed, substitute the range from the
  // RegDescription

  RegAxisRange iRange(*reg, range);
  
  // If the register isn't archived, substitute zeroes.
  
  if(reg->iSlot() < 0) {

    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      data[iRange.currentIterator()].real_ = 0.0;
      data[iRange.currentIterator()].imag_ = 0.0;
    }

  } else {

    // Retrieve the requested registers.
    
    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      data[iRange.currentIterator()] = 
	*((Complex<float>::Data*)&slots_[iRange.currentSlot()]);
    }
  };
}

void RegCal::RegCalData::
getCalComplexFloat(RegDescription* reg, MonitorDataType* data, 
		   RegAxisRange& iRange)
{
  // If the register isn't archived, substitute zeroes.

  if(reg->iSlot() < 0) {

    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      data[iRange.currentIterator()].val.data_.cf.real_ = 0.0;
      data[iRange.currentIterator()].val.data_.cf.imag_ = 0.0;
    }

  } else {

    // Retrieve the requested registers.
    
    for(iRange.reset(); !iRange.isEnd(); ++iRange) {
      data[iRange.currentIterator()].val.data_.cf = 
	*((Complex<float>::Data*)&slots_[iRange.currentSlot()]);
    }
  };
}

/**.......................................................................
 * Print cal factors for the requested register
 */
void RegCal::printCalFactors(std::vector<RegDescription>& regs)
{
  for(unsigned iReg=0; iReg < regs.size(); iReg++) {
    RegDescription& desc = regs[iReg];
    
    vector<Range<unsigned> > ranges = desc.getSlotRanges();
  
    for(unsigned iRange=0; iRange < ranges.size(); iRange++) 
      for(unsigned iSlot=ranges[iRange].start(); iSlot <= ranges[iRange].stop();
	  iSlot++) {
	cout << "offset[" << iSlot << "]   = " << slots_[iSlot].offset_ << endl
	     << "factor[" << iSlot << "]   = " << slots_[iSlot].factor_ << endl
	     << "imagGain[" << iSlot << "] = " << slots_[iSlot].imagGain_ << endl
	     << "imagGain[" << iSlot << "] = " << slots_[iSlot].imagGain_ << endl
	     << "sinPhi[" << iSlot << "]   = " << slots_[iSlot].sinPhi_ << endl
	     << "cosPhi[" << iSlot << "]   = " << slots_[iSlot].cosPhi_ << endl
	     << endl;
      }
  }
}

RegCal::RegCalSlot RegCal::getRegCalSlot(unsigned iSlot)
{
  if(iSlot > slots_.size()-1)
    ThrowError("Requested slot index: " << iSlot << " is out of range (0 - " << (slots_.size()-1) << ")");

  return slots_[iSlot];
}

double* RegCal::getSlotPtr(unsigned iSlot)
{
  if(iSlot > (calData_->slots_.size()-1))
    ThrowError("Requested slot index: " << iSlot << " is out of range (0 - " << (slots_.size()-1) << ")");

  return (double*)(&calData_->slots_[0] + iSlot);
}
