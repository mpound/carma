#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <limits.h>

#include "carma/szaarrayutils/regcal.h"
#include "carma/szaarrayutils/lprintf.h"
#include "carma/szaarrayutils/szaconst.h"

using namespace sza::array;

/*
 * The calibration of a scalar register is:
 *
 * reg[i] = offset + factor * reg[i].
 *
 * The calibration of a complex register is:
 *
 * real = reg[i]
 * imag = reg[i+1]
 * reg[i]   = offset + factor * real;
 * reg[i+1] = offset + factor * (imag/imag_gain + real * sin(phi)) / cos(phi);
 */

typedef struct {
  double offset;        /* The calibration offset of the register */
  double factor;        /* The calibration multiplier of the register */

/* The following are only relevant to complex registers */

  double imag_gain;     /* The gain of the imaginary channel wrt the real */
  double sinphi,cosphi; /* Sin and cos of the imaginary channel phase offset */
} RegCalSlot;

struct RegCal {
  unsigned int nslot; /* The number of register calibration slots */
  RegCalSlot *slots;  /* An array of nslot sets of calibration parameters */
  int nsnap_slot;     /* The register slot of the nsnap register */
  int nsnapByteOffset;/* The register slot of the nsnap register */
};

static int read_cal_group(InputStream *stream, ArrayMap *arraymap, 
			  RegCal *regcal, ArrRegMapReg *arreg);
static int skip_cal_group(InputStream *stream);
static int read_cal_record(RegCal *regcal, ArrRegMap* arregmap, 
			   RegMapBoard *board,
			   RegMapBlock *block, unsigned int ia, unsigned int ib,
			   InputStream *stream);

static int bad_get_cal_args(char *caller, RegCalData *cal, RegMapReg *reg,
			    unsigned index, unsigned n, void *data);

/*.......................................................................
 * Create a new register calibration object from a given register map
 * and a given register calibration file.
 *
 * Input:
 *  regmap       RegMap *  The register map.
 * Output:
 *  return       RegCal *  The new register calibration object, or NULL
 *                         on error.
 */
RegCal *new_RegCal(ArrayMap *arraymap)
{
  RegCal *regcal;   /* The object to be returned */
  unsigned int i;

  // Check arguments.

  if(!arraymap) {
    lprintf(stderr, "new_RegCal: NULL arraymap argument.\n");
    return NULL;
  };
  
  // Allocate the container of the object.

  regcal = (RegCal *) malloc(sizeof(RegCal));
  if(!regcal) {
    lprintf(stderr, "new_RegCal: Insufficient memory for container.\n");
    return NULL;
  };
  
  // Before attempting any operation that might fail, initialize the
  // container at least up to the point at which it can safely be
  // passed to del_RegCal().

  regcal->nslot = arraymap->narchive;
  regcal->slots = NULL;
  regcal->nsnap_slot      = -1;
  regcal->nsnapByteOffset = -1;
  
  // Allocate and initialize the calibration array.

  regcal->slots = (RegCalSlot *) malloc(sizeof(RegCalSlot) * regcal->nslot);
  if(!regcal->slots) {
    lprintf(stderr, "new_RegCal: Insufficient memory for cal array.\n");
    return del_RegCal(regcal);
  };
  for(i=0; i<regcal->nslot; i++) {
    RegCalSlot *slot = regcal->slots + i;
    slot->offset = 0.0;
    slot->factor = 1.0;
    slot->imag_gain = 1.0;
    slot->sinphi = 0.0;
    slot->cosphi = 1.0;
  };
  
  // Determine the location of the integration count register.  During
  // calibration this register is read to determine how to turn
  // integrations into means.

  {
    RegMapBlock *blk = find_ArrayMapBlock(arraymap, "array", "frame", "nsnap");
    if(!blk) {
      lprintf(stderr,
	      "new_RegCal: Failed to find nsnap software register.\n");
      return del_RegCal(regcal);
    };

    regcal->nsnap_slot = blk->slot_;
    regcal->nsnapByteOffset = 
      arraymap->byteOffsetInArcArrayMapOf("array", "frame", "nsnap");
  };

  return regcal;
}

/*.......................................................................
 * Delete a register calibration object.
 *
 * Input:
 *  regcal    RegCal *  The object to be deleted.
 * Output:
 *  return    RegCal *  Always NULL.
 */
RegCal *del_RegCal(RegCal *regcal)
{
  if(regcal) {
    if(regcal->slots)
      free(regcal->slots);
    free(regcal);
  };
  return NULL;
}

/*.......................................................................
 * Load a calibration file and record the results in regcal->slots[].
 *
 * Input:
 *  regmap      RegMap *  The register map associated with the registers
 *                        being calibrated.
 *  regcal      RegCal *  The register calibration object to be updated.
 *  dir           char *  The directory in which the file resides, or "".
 *  name          char *  The name of an existing text file to be opened
 *                        for read.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int load_cal_file(ArrayMap *arraymap, RegCal *regcal, char *dir, char *name)
{
  InputStream *stream;   /* The stream to attach to the cal file */
  int status = 0;        /* The return value of this function */
  
  // Allocate an input stream, attach it to the file and load the
  // file.

  if((stream = new_InputStream())==NULL ||
     open_FileInputStream(stream, dir, name) ||
     load_cal_stream(arraymap, regcal, stream))
    status = 1;
  
  // Delete the redundant stream.

  stream = del_InputStream(stream);
  return status;
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
int load_cal_stream(ArrayMap *arraymap, RegCal *regcal, InputStream *stream)
{
  ArrRegMapReg arreg;   // A decoded register specification 

  // Check inputs.

  if(!arraymap || !regcal || !stream) {
    lprintf(stderr, "load_cal_stream: NULL %s argument.\n",
	    !arraymap ? "regmap" : (!regcal ? "regcal":"stream"));
    return 1;
  };

  if(arraymap->narchive != regcal->nslot) {
    lprintf(stderr,
	    "load_cal_stream: Array map incompatible with cal object.\n");
    return 1;
  };
  
  // Discard any existing calibrations.

  reset_RegCal(regcal);
  
  // Skip leading white-space.

  if(input_skip_white(stream, 1, 0))
    return 1;
  
  // Read register calibration records until the end of the file or an
  // error is encountered.

  while(stream->nextc != EOF) {
    
    // Decode the next register name.

    switch(input_ArrRegMapReg(stream, 1, arraymap, REG_INPUT_RANGE, 0, &arreg)) {
      
      // A valid register specification was read and summarized in
      // 'reg'.  Read the accompanying calibrations parameters.

    case REG_VALID:
      if(read_cal_group(stream, arraymap, regcal, &arreg))
	return input_error(stream, 1, "Calibration aborted.\n");
      break;
      
      // A syntactically correct, but unknown register specification
      // was read.  In order to allow one to use the same calibration
      // file with old and new data even when registers have been
      // added or removed from the register map of that file wrt the
      // calibration file, simply skip the calibration parameters of
      // unknown registers.

    case REG_UNKNOWN:
      if(skip_cal_group(stream))
	return input_error(stream, 1, "Calibration aborted.\n");
      input_error(stream, 1, "Continuing calibration with next register.\n");
      break;
/*
 * A corrupt register specification was read.
 */
    case REG_INVALID:
    default:
      return input_error(stream, 1, "Calibration aborted.\n");
      break;
    };
  };
  return 0;
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
 *  reg      ArrRegMapReg *  The specification of the register elements
 *                        to be calibrated.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
static int read_cal_group(InputStream *stream, ArrayMap *arraymap, 
			  RegCal *regcal, ArrRegMapReg *arreg)
{
  ArrRegMap* arregmap;
  RegMap* regmap;     /* The parent regmap of the selected register */
  RegMapBoard *brd;   /* The parent board of the selected register */
  RegMapBlock *blk;   /* The block containing the selected register */
  unsigned int ia, ib;         /* The start and end indexes to calibrate */
  
  // Get the descriptions of the board and block that contain the
  // register specification.

  arregmap = arraymap->regmaps[arreg->regmap];
  regmap   = arregmap->regmap;
  brd      = regmap->boards_[arreg->reg.board];
  blk      = brd->blocks[arreg->reg.block];
  
  // We can't calibrate unarchived registers.

  if(arreg->reg.slot < 0) {
    input_error(stream, 1, "Skipping unarchived register %s.%s.%s[].\n",
		arregmap->name, brd->name, blk->name_);
    return skip_cal_group(stream);
  };
  
  // Get the range of frame indexes to be calibrated.

  ia = arreg->reg.slot;
  ib = ia + arreg->reg.nreg - 1;
  
  // Find the start of the calibration parameters.

  if(input_skip_space(stream, 1, 0))
    return 1;
  
  // The next character determines whether one or an array of
  // calibration parameters are to be assigned to the specified
  // register range.

  if(stream->nextc == '{') {   /* An array of parameter specifiers */
    unsigned ireg;
    
    // Skip the opening '{'.

    if(input_skip_white(stream, 1, 1))
      return 1;
    
    // Read one calibration set per register in the range.

    for(ireg = ia; ireg <= ib; ireg++) {
      
      // Read and record the calibration factors of the current
      // register.

      if(read_cal_record(regcal, arregmap, brd, blk, ireg, ireg, stream))
	return 1;
      
      // Each set of calibration parameters must be separated from
      // those of the next register slot by a comma. The last must be
      // followed by the brace that terminates the list.

      switch(stream->nextc) {
      case '}':
	if(ireg != ib)
	  return input_error(stream, 1, "Too few calibration parameters.\n");
	break;
      case ',':
	if(ireg == ib)
	  return input_error(stream, 1, "Too many calibration parameters.\n");
	break;
      default:
	if(ireg == ib)
	  return input_error(stream, 1,
			   "Missing '}' after final calibration parameter.\n");
	else
	  return input_error(stream, 1,
			   "Missing ',' after valid calibration parameter.\n");
	break;
      };
    };
/*
 * Skip the delimiter.
 */
    if(input_skip_white(stream, 1, 1))
      return 1;
/*
 * Read a single register calibration and record it as the calibration
 * of all of the registers in the range.
 */
  } else {
    if(read_cal_record(regcal, arregmap, brd, blk, ia, ib, stream))
      return 1;
  };
/*
 * We should be at the end of a line.
 */
  if(stream->nextc != '\n' && stream->nextc != EOF) {
    return input_error(stream, 1,
	     "Unexpected characters follow the last calibration parameter.\n");
  };
/*
 * Skip trailing white-space up to the next record.
 */
  if(input_skip_white(stream, 1, 0))
    return 1;
  return 0;
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
static int skip_cal_group(InputStream *stream)
{
/*
 * Find the start of the register name that initiates the next calibration
 * specification. This is identified by searching for the next alphabetical
 * character that follows a newline (and zero or more spaces).
 */
  do {
    if(input_skip_past_eol(stream, 1) ||
       input_skip_space(stream, 1, 0))
      return 1;
  } while(!isalpha(stream->nextc));
  return 0;
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
static int read_cal_record(RegCal *regcal, ArrRegMap* arregmap, 
			   RegMapBoard *board,
			   RegMapBlock *block, unsigned int ia, unsigned int ib,
			   InputStream *stream)
{
  double pars[4];   /* An array into which to read the parameters */
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
      return input_error(stream, 1,
		   "Calibration parameter %d doesn't appear to be a number.\n",
		   ipar+1);
    if(input_skip_space(stream, 1, 0))
      return 1;
    
    // If the next character is a '/', read the number that follows it
    // and use it to divide the previous number.

    if(stream->nextc == '/') {
      double divisor;
      
      // Skip the '/' and read the divisor that follows it.

      if(input_skip_space(stream, 1, 1))
	return 1;
      if(input_double(stream, 0, &divisor))
	return input_error(stream, 1, "Missing divisor after '/'.\n");
      
      // Check for zero divisor.

      if(divisor == 0.0)
	return input_error(stream, 1, "Illegal zero divisor.\n");
      pars[ipar] /= divisor;
      
      // Find the next token.

      if(input_skip_space(stream, 1, 0))
	return 1;
    };
  };
  
  // Skip trailing spaces.

  if(input_skip_space(stream, 1, 0))
    return 1;
  
  // Record the calibration information.

  if(block->flags_ & REG_COMPLEX) {
    double offset = pars[0];
    double factor = pars[1];
    double imag_gain = pars[2];
    double sinphi = sin(dtor * pars[3]);
    double cosphi = cos(dtor * pars[3]);
    for(islot=ia; islot<=ib; islot++) {
      RegCalSlot *slot = regcal->slots + islot;
      slot->offset = offset;
      slot->factor = factor;
      slot->imag_gain = imag_gain;
      slot->sinphi = sinphi;
      slot->cosphi = cosphi;
    };
  } else {
    double offset = pars[0];
    double factor = pars[1];
    for(islot=ia; islot<=ib; islot++) {
      RegCalSlot *slot = regcal->slots + islot;
      slot->offset = offset;
      slot->factor = factor;
    };
  };
  return 0;
}

#ifdef NEW_FRAME
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
int calibrate_regdata(ArrayMap *arraymap, RegCal *regcal, RegSet *regset,
		      RegRawData *raw, RegCalData *cal)
{
  RegSetRange *range;    // A register selection range
  unsigned nsnap;        // The integration count
  
  // Check the arguments.

  if(!arraymap) {
    lprintf(stderr, "calibrate_regset: NULL arraymap argument.\n");
    return 1;
  };

  if(!regcal || !regset || !raw) {
    lprintf(stderr, "calibrate_regset: NULL %s argument.\n",
	    !regcal ? "regcal" : (!regset ? "regset":"raw"));
    return 1;
  };

  if(regcal->nslot != size_RegSet(regset) || regcal->nslot != raw->nslot) {
    lprintf(stderr, "calibrate_regset: Incompatible arguments.\n");
    return 1;
  };
  
  // Get the value of the "nsnap" register. This records the number of
  // hardware integrations that were added in software.

  raw->fm->readReg("array", "frame", "nsnap", &nsnap);

  if(nsnap < 1)
    nsnap = 1;
  
  // Get the container object of the first register map of the array map,
  // the container object of the first board of the register map,
  // and the container of its first block.
  
  ArrRegMap**    arregmap = &arraymap->regmaps[0];   // An array register map
  RegMapBoard**  brd      = &((*arregmap)->regmap->boards_[0]);
  RegMapBlock**  blk      = &((*brd)->blocks[0]);
  
  // Traverse the list of register-selection ranges to calibrate the
  // included register elements.

  for(range=regset_range_list(regset); range != NULL; range = range->next) {

    // Each range contains a start and stop byte index

    RegRange regRange(true, range->ia, range->ib);

    // Iterate over all discrete register elements, extracting and
    // renormalizing

    while(!regRange.isEnd()) {

      RegMapBlock* blk = regRange->currentBlock();
      unsigned iSlot   = regRange->currentSlot();

      // Get a double representation of the raw data value

      cal->slots[iSlot] = raw->fm->getRegVal(range, false);

      // Convert integrated register values to means.

      if(blk->isSummed())
	cal->slots[iSlot] /= nsnap;

      ++regRange;
    }

    // Iterate once more, calibrating the regs we just extracted

    regRange.reset();
    while(!regRange.isEnd()) {

      RegMapBlock* blk = regRange->currentBlock();
      unsigned iSlot   = regRange->currentSlot();
    
      // Apply complex calibrations.

      if(blk->isComplex()) {
	RegCalSlot *cal_slot = regcal->slots + iSlot;
	double re = cal->slots[iSlot];
	double im = cal->slots[iSlot + 1];

	cal->slots[iSlot] = cal_slot->offset + re * cal_slot->factor;
	cal->slots[iSlot+1] = cal_slot->offset + cal_slot->factor *
	  (im/cal_slot->imag_gain + re*cal_slot->sinphi) /
	  cal_slot->cosphi;
	
	// Increment the range object -- yes, we mean to do this twice
	// (here, and once again below); since complex calibrations
	// are done in pairs, we only need to iterate over every
	// second element

	++regRange;
      
	// Else apply real-number calibrations.
	
      } else {
	RegCalSlot *cal_slot = regcal->slots + iSlot;
	cal->slots[iSlot] = cal_slot->offset + cal_slot->factor *
	  cal->slots[iSlot];
      }

      // Increment the range object

      ++regRange;
    }
  }
  
  // Since nsnap is an integrated register, it will now have been
  // reduced to unity. Return it to its original value.

  cal->slots[regcal->nsnap_slot] = nsnap;
  
  // Record the succesful initialization of the cal object.

  cal->empty = 0;

  return 0;
}
#else
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
int calibrate_regdata(ArrayMap *arraymap, RegCal *regcal, RegSet *regset,
		      RegRawData *raw, RegCalData *cal)
{
  RegSetRange *range;    // A register selection range
  unsigned nsnap;        // The integration count
  
  // Check the arguments.

  if(!arraymap) {
    lprintf(stderr, "calibrate_regset: NULL arraymap argument.\n");
    return 1;
  };

  if(!regcal || !regset || !raw) {
    lprintf(stderr, "calibrate_regset: NULL %s argument.\n",
	    !regcal ? "regcal" : (!regset ? "regset":"raw"));
    return 1;
  };

  if(regcal->nslot != size_RegSet(regset, true) || regcal->nslot != raw->nslot) {
    lprintf(stderr, "calibrate_regset: Incompatible arguments.\n");
    return 1;
  };
  
  // Get the value of the "nsnap" register. This records the number of
  // hardware integrations that were added in software.

  nsnap = raw->slots[regcal->nsnap_slot];
  if(nsnap < 1)
    nsnap = 1;
  
  // Get the container object of the first register map of the array map,
  // the container object of the first board of the register map,
  // and the container of its first block.
  
  ArrRegMap**    arregmap = &arraymap->regmaps[0];   // An array register map
  RegMapBoard**  brd      = &((*arregmap)->regmap->boards_[0]);
  RegMapBlock**  blk      = &((*brd)->blocks[0]);
  
  // Traverse the list of register-selection ranges to calibrate the
  // included register elements.

  for(range = regset_range_list(regset); range; ) {

    int start; // The index of the first element in a register block
    int end;   // The index of the last element in a register block 
    
    // Find the first block that is in the current range. Note that
    // indexes are assigned in ascending order of block number.

    // NB: we need to add arregmap->slot here because blk->slot is
    // only the index into the slots for the regmap that it belongs
    // to, not into the array map.

    while(range->ia > (end=(*arregmap)->slot+(*blk)->slot_+(*blk)->nreg_-1)) {
      if(++blk - &((*brd)->blocks[0]) >= (*brd)->nblock) {
	if(++brd - &((*arregmap)->regmap->boards_[0]) >= (*arregmap)->regmap->nboard_) {
	  if(++arregmap - &arraymap->regmaps[0] >= arraymap->nregmap)
	    return 0;
	  brd = &((*arregmap)->regmap->boards_[0]);
	}
	blk = &((*brd)->blocks[0]);
      };
    };
    
    // Get the offset of the first register element that is included
    // in the block from the beginning of the array register map.

    start = (*blk)->slot_ + (*arregmap)->slot;
    
    // Loop over the ranges that cover the new block and/or the blocks
    // that are covered by the current range.

    do {
      
      // Get the start and end indexes of the selected elements in the
      // current register block.

      int ia = range->ia > start ? range->ia : start;
      int ib = range->ib < end ? range->ib : end;
      int slot;
      
      // Convert from signed or unsigned integers to signed double.

      if((*blk)->flags_ & REG_INT) {
	for(slot=ia; slot<=ib; slot++)
	  cal->slots[slot] = ((signed *)raw->slots)[slot];
      } else if((*blk)->flags_ & REG_FLOAT) {
	for(slot=ia; slot<=ib; slot++)
	  cal->slots[slot] = *((float*)&(((unsigned *)raw->slots)[slot]));
      } else {
	for(slot=ia; slot<=ib; slot++)
	  cal->slots[slot] = ((unsigned *)raw->slots)[slot];
      };
      
      // Convert integrated register values to means.

      if((*blk)->flags_ & REG_SUM) {
	for(slot=ia; slot<=ib; slot++)
	  cal->slots[slot] /= nsnap;
      };
      
      // Apply complex calibrations.

      if((*blk)->flags_ & REG_COMPLEX) {
	for(slot=ia; slot<=ib; slot += 2) {
	  RegCalSlot *cal_slot = regcal->slots + slot;
	  double re = cal->slots[slot];
	  double im = cal->slots[slot + 1];
	  cal->slots[slot] = cal_slot->offset + re * cal_slot->factor;
	  cal->slots[slot+1] = cal_slot->offset + cal_slot->factor *
	    (im/cal_slot->imag_gain + re*cal_slot->sinphi) /
	      cal_slot->cosphi;
	};
	
	// Apply real-number calibrations.

      } else {
	for(slot=ia; slot<=ib; slot++) {
	  RegCalSlot *cal_slot = regcal->slots + slot;
	  cal->slots[slot] = cal_slot->offset + cal_slot->factor *
	    cal->slots[slot];
	};
      };
      
      // If the current range crosses the boundary to the next block,
      // advance to the next block.

      if(range->ib > end) {
	if(++blk - &((*brd)->blocks[0]) >= (*brd)->nblock) {
	  if(++brd - &((*arregmap)->regmap->boards_[0]) >= (*arregmap)->regmap->nboard_) {
	    if(++arregmap - &arraymap->regmaps[0] >= arraymap->nregmap)
	      return 0;
	    brd = &((*arregmap)->regmap->boards_[0]);
	  }
	  blk = &((*brd)->blocks[0]);
	};
	start = (*blk)->slot_;
	end   = (*blk)->slot_ + (*blk)->nreg_ - 1;
      }
      
      // If the current range is contained completely within the
      // current block, then advance to the next range.

      else {
	range = range->next;
      };
      
      // Continue for as long as there is some overlap between the
      // current range and the current block.

    } while(range && range->ia <= end);
  };
  
  // Since nsnap is an integrated register, it will now have been
  // reduced to unity. Return it to its original value.

  cal->slots[regcal->nsnap_slot] = nsnap;
  
  // Record the succesful initialization of the cal object.

  cal->empty = 0;
  return 0;
}
#endif

/*.......................................................................
 * Reset the calibrations to 0 offset and unit gain.
 *
 * Input:
 *  regcal    RegCal *   The calibration object to be reset.
 * Output:
 *  return       int     0 - OK.
 *                       1 - Error.
 */
int reset_RegCal(RegCal *regcal)
{
  unsigned int i;

  if(!regcal) {
    lprintf(stderr, "reset_RegCal: NULL regcal argument.\n");
    return 1;
  };
/*
 * Reset each calibration entry to zero offset and unit gain.
 */
  for(i=0; i<regcal->nslot; i++) {
    RegCalSlot *slot = regcal->slots + i;
    slot->offset = 0.0;
    slot->factor = 1.0;
    slot->imag_gain = 1.0;
    slot->sinphi = 0.0;
    slot->cosphi = 1.0;
  };
  return 0;
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
RegCalData *new_RegCalData(ArrayMap *arraymap)
{
  RegCalData *cal;   /* The object to be returned */
  unsigned int i;

  
  // Check arguments.

  if(!arraymap) {
    fprintf(stderr, "new_RegCalData: NULL regmap argument.\n");
    return NULL;
  };
  
  // Allocate the container of the object.

  cal = (RegCalData *) malloc(sizeof(RegCalData));
  if(!cal) {
    fprintf(stderr, "new_RegCalData: Insufficient memory for container.\n");
    return NULL;
  };
  
  // Before attempting any operation that might fail, initialize the
  // container at least up to the point at which it can safely be
  // passed to del_RegCalData().

  cal->nslot = arraymap->narchive;
  cal->slots = NULL;
  cal->empty = 1;
  
  // Allocate and initialize the array of register slots.

  cal->slots = (double *) malloc(sizeof(double) * cal->nslot);
  if(!cal->slots) {
    fprintf(stderr, "new_RegCalData: Insufficient memory for monitor array.\n");
    return del_RegCalData(cal);
  };
  for(i=0; i<cal->nslot; i++)
    cal->slots[i] = 0.0;
  return cal;
}

/*.......................................................................
 * Delete a monitor array object.
 *
 * Input:
 *  cal      RegCalData *  The object to be deleted.
 * Output:
 *  return   RegCalData *  Always NULL.
 */
RegCalData *del_RegCalData(RegCalData *cal)
{
  if(cal) {
    if(cal->slots)
      free(cal->slots);
    free(cal);
  };
  return NULL;
}

/*.......................................................................
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
static int bad_get_cal_args(char *caller, RegCalData *cal, ArrRegMapReg *arreg,
			      unsigned index, unsigned n, void *data)
{
  if(!cal || !arreg || !data) {
    lprintf(stderr, "%s: NULL argument(s).\n", caller);
    return 1;
  };

  RegMapReg* reg = &arreg->reg;

  if(reg->slot >= 0) {
    if(index + n > reg->index + reg->nreg) {
      lprintf(stderr, "%s: Index range too large.\n", caller);
      return 1;
    } else if(reg->slot + reg->nreg > cal->nslot) {
      lprintf(stderr, "%s: Invalid register specifier.\n", caller);
      return 1;
    };
  };
  return 0;
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
int get_cal_float(RegCalData *cal, ArrRegMapReg *arreg, unsigned index,
		  unsigned n, float *data)
{
  unsigned int i;
/*
 * Check the arguments.
 */
  if(bad_get_cal_args("get_cal_float", cal, arreg, index, n, data))
    return 1;

  RegMapReg* reg = &arreg->reg;
/*
 * If the registers isn't archived, substitute zeroes.
 */
  if(reg->slot < 0) {
    for(i=0; i<n; i++)
      data[i] = 0.0;
  } else {
/*
 * Get a pointer to the first element of the calibrated data array
 * that corresponds to the start of the selected range.
 */
    double *slots = cal->slots + reg->slot - reg->index + index;
/*
 * Retrieve the requested registers.
 */
    for(i=0; i<n; i++)
      data[i] = slots[i];
  };
  return 0;
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
int get_cal_double(RegCalData *cal, ArrRegMapReg *arreg, unsigned index,
		   unsigned n, double *data)
{
  unsigned int i;
/*
 * Check the arguments.
 */
  if(bad_get_cal_args("get_cal_double", cal, arreg, index, n, data))
    return 1;

  RegMapReg* reg = &arreg->reg;

/*
 * If the registers isn't archived, substitute zeroes.
 */
  if(reg->slot < 0) {
    for(i=0; i<n; i++)
      data[i] = 0.0;
  } else {
/*
 * Get a pointer to the first element of the calibrated data array
 * that corresponds to the start of the selected range.
 */
    double *slots = cal->slots + reg->slot - reg->index + index;
/*
 * Retrieve the requested registers.
 */
    for(i=0; i<n; i++)
      data[i] = slots[i];
  };
  return 0;
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
int get_cal_uint(RegCalData *cal, ArrRegMapReg *arreg, unsigned index,
		  unsigned n, unsigned *data)
{
  unsigned int i;
/*
 * Check the arguments.
 */
  if(bad_get_cal_args("get_cal_uint", cal, arreg, index, n, data))
    return 1;

  RegMapReg* reg = &arreg->reg;

/*
 * If the registers isn't archived, substitute zeroes.
 */
  if(reg->slot < 0) {
    for(i=0; i<n; i++)
      data[i] = 0;
  } else {
/*
 * Get a pointer to the first element of the calibrated data array
 * that corresponds to the start of the selected range.
 */
    double *slots = cal->slots + reg->slot - reg->index + index;
/*
 * Retrieve the requested registers. If the value to be converted
 * to unsigned int is outside of the legal range of an unsigned int
 * assign the nearest limit.
 */
    for(i=0; i<n; i++) {
      double d = slots[i];
      data[i] = d < 0.0 ? 0 : (d > UINT_MAX ? UINT_MAX : (unsigned) d);
    };
  };
  return 0;
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
int get_cal_int(RegCalData *cal, ArrRegMapReg *arreg, unsigned index,
		  unsigned n, int *data)
{
  unsigned int i;
/*
 * Check the arguments.
 */
  if(bad_get_cal_args("get_cal_int", cal, arreg, index, n, data))
    return 1;

  RegMapReg* reg = &arreg->reg;

/*
 * If the registers isn't archived, substitute zeroes.
 */
  if(reg->slot < 0) {
    for(i=0; i<n; i++)
      data[i] = 0;
  } else {
/*
 * Get a pointer to the first element of the calibrated data array
 * that corresponds to the start of the selected range.
 */
    double *slots = cal->slots + reg->slot - reg->index + index;
/*
 * Retrieve the requested registers. If the value to be converted
 * is outside of the legal range of an int, assign the nearest limit.
 */
    for(i=0; i<n; i++) {
      double d = slots[i];
      data[i] = d < INT_MIN ? INT_MIN : (d > INT_MAX ? INT_MAX : (int) d);
    };
  };
  return 0;
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
int get_cal_ulong(RegCalData *cal, ArrRegMapReg *arreg, unsigned index,
		  unsigned n, unsigned long *data)
{
  unsigned int i;
/*
 * Check the arguments.
 */
  if(bad_get_cal_args("get_cal_ulong", cal, arreg, index, n, data))
    return 1;

  RegMapReg* reg = &arreg->reg;

/*
 * If the registers isn't archived, substitute zeroes.
 */
  if(reg->slot < 0) {
    for(i=0; i<n; i++)
      data[i] = 0UL;
  } else {
/*
 * Get a pointer to the first element of the calibrated data array
 * that corresponds to the start of the selected range.
 */
    double *slots = cal->slots + reg->slot - reg->index + index;
/*
 * Retrieve the requested registers. If the value to be converted
 * to unsigned long is outside of the legal range of an unsigned long
 * assign the nearest limit.
 */
    for(i=0; i<n; i++) {
      double d = slots[i];
      data[i] = d < 0.0 ? 0 : (d > ULONG_MAX ? ULONG_MAX : (unsigned long) d);
    };
  };
  return 0;
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
int get_cal_long(RegCalData *cal, ArrRegMapReg *arreg, unsigned index,
		 unsigned n, long *data)
{
  unsigned int i;
/*
 * Check the arguments.
 */
  if(bad_get_cal_args("get_cal_long", cal, arreg, index, n, data))
    return 1;

  RegMapReg* reg = &arreg->reg;

/*
 * If the registers isn't archived, substitute zeroes.
 */
  if(reg->slot < 0) {
    for(i=0; i<n; i++)
      data[i] = 0L;
  } else {
/*
 * Get a pointer to the first element of the calibrated data array
 * that corresponds to the start of the selected range.
 */
    double *slots = cal->slots + reg->slot - reg->index + index;
/*
 * Retrieve the requested registers. If the value to be converted
 * is outside of the legal range of an long, assign the nearest limit.
 */
    for(i=0; i<n; i++) {
      double d = slots[i];
      data[i] = d < LONG_MIN ? LONG_MIN : (d > LONG_MAX ? LONG_MAX : (int) d);
    };
  };
  return 0;
}

/*.......................................................................
 * Unpack a string from a calibrated register frame.
 *
 * Input:
 *  cal     RegCalData *  The calibrated frame of selected registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  nc        unsigned    The max number of characters to be placed in
 *                        data (including the '\0' terminator).
 * Input/Output:
 *  string        char *  The array of nc characters to return the string
 *                        in.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int get_cal_string(RegCalData *cal, ArrRegMapReg *arreg, unsigned nc, char *string)
{
/*
 * Check the arguments.
 */
  if(bad_get_cal_args("get_cal_string", cal, arreg, 0, arreg->reg.nreg, 
		      string))
    return 1;
  if(nc < 1) {
    lprintf(stderr,
	    "get_cal_string: Your zero-length string can't be terminated.\n");
    return 1;
  };

  RegMapReg* reg = &arreg->reg;

/*
 * If the registers isn't archived, substitute an empty string.
 */
  if(reg->slot < 0) {
    string[0] = '\0';
  } else {
/*
 * Get a pointer to the first element of the calibrated data array
 * that corresponds to the start of the selected range.
 */
    double *slots = cal->slots + reg->slot;
/*
 * Unpack the string from the data array using a helper function from
 * regmap.c.
 */
    if(unpack_double_string(slots, reg->nreg, nc, string))
      return 1;
  };
  return 0;
}

