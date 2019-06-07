#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

#include "carma/szaarrayutils/regtemplate.h"
#include "carma/szaarrayutils/regmap.h"
#include "carma/szaarrayutils/lprintf.h"

#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/RegExpParser.h"

using namespace sza::util;
using namespace std;

/*.......................................................................
 * Create a map of available VME and software registers.
 *
 * Input:
 *
 *  regtmp  RegTemplate *  The template for creating the register map.
 *
 * Output:
 *
 *  return       RegMap *  The new register map.
 */
RegMap::RegMap(void* vtmp, bool old, bool addRegs)
{
  if(old)
    privateConstructorOld(vtmp);
  else
    privateConstructor(vtmp, addRegs);
}

void RegMap::privateConstructor(void* vtmp, bool addRegs)
{
  int i;
  RegTemplate* regtmp = (RegTemplate*)vtmp;
  using sza::util::LogStream;
  
  // A board with the following registers is inserted at the head of
  // every register map.

  static RegBlockTemp generic_regmap_board[] = {
    
    // This register is set to sza::util::FrameFlags::RECEIVED if this
    // frame has been received from a remote computer
    
    RegBlockTemp("Bit 1 will be high if data in this frame were received during the last "
		 "half-second integration.  If not, bit 0 will be set high", 
		 "received",   REG_UCHAR|REG_UNION, 0, 1),
    
    // The number of samples per integration 
    
    RegBlockTemp("The number of frames which have been integrated to produce this frame", 
		 "nsnap",      REG_UINT|REG_SUM, 0, 1),
    
    // The sequential record number 
    
    RegBlockTemp("A monotonically-increasing record number since the last re-start of the control system", 
		 "record",     REG_DEFAULT, 0, 1),
    
    // The date and time as MJD days and milli-seconds 
    
    RegBlockTemp("The MJD of this frame", 
		 "utc",        REG_UTC,     0, 1),
    
    // The Local Sidereal Time (integer milli-seconds) 
    
    RegBlockTemp("The local sidereal time corresponding to this frame (milliseconds)",
		 "lst",        REG_DEFAULT, 0, 1),
    
    // A bit-mask union of feature markers received from the control program 
    //  since the last frame was constructed. 
    
    RegBlockTemp("A bit-mask union of feature markers received from the control program "
		 "since the last frame was constructed.",
		 "features",   REG_UINT|REG_UNION, 0, 1),
    
    // The sequence mark-command sequence number associated with the
    // current value of the "features" register. 
    
    RegBlockTemp("The sequence mark-command sequence number associated with the "
		 "current value of the features register.",
		 "markSeq",    REG_UINT,           0, 1),
  };
  
  // And we will call this board the "frame" board
  
  static RegBoardTemp per_regmap_boards[] = {
    {"frame",   generic_regmap_board,   ARRAY_DIM(generic_regmap_board),  
     {0x000000U}, "Information about the data frame"}, // Soft 
  };
  
  static int nper_regmap_boards =
    sizeof(per_regmap_boards)/sizeof(per_regmap_boards[0]);
  
  // The following register blocks are inserted at the beginning of
  // every board.
  
  static RegBlockTemp per_brd_blocks[] = {
    
    // The status of the board is recorded in the following
    // register. Note that regdb_board_status_reg() and
    // snap_scanner_frame() assume that this is the first register of
    // each board.
    
    RegBlockTemp("", "status",  REG_DEFAULT, 0, 1),
  };
  
  static int nper_brd_blocks =
    sizeof(per_brd_blocks)/sizeof(per_brd_blocks[0]);
  
  // Check arguments.
  
  if(!regtmp) {
    ThrowError("NULL register-map template.");
  };
  
  if(!regtmp->boards || regtmp->nboard == 0) {
    ThrowError("Empty board array.");
  };
  
  // Before attempting any operation that might fail, initialize the
  // container at least up to the point at which it is safe to pass it
  // to del_RegMap().

  ref_count_    =  1;
  nboard_       =  regtmp->nboard;

  if(addRegs) {
    nboard_    += nper_regmap_boards;
  }

  nreg_         =  0;
  narchive_     =  0;
  nByte_        =  0;
  nArcByte_     =  0;

  // Allocate the array of board descriptions.

  boards_.resize(nboard_);

  //  COUT("Inside new RegMap with nboard = " << nboard_ << " 0");

  // Initialize the board descriptions at least up to a point at which
  // regmap can be passed to del_RegMap().
  
  if(addRegs) {
    for(i=0; i < nboard_; i++) {
      RegBoardTemp *tbrd = i < nper_regmap_boards ? 
	(per_regmap_boards + i) : (regtmp->boards + i - nper_regmap_boards);
      
      boards_[i] = new RegMapBoard(this, tbrd, i, per_brd_blocks, nper_brd_blocks);
    }
  } else {
    for(i=0; i < nboard_; i++) {
      RegBoardTemp *tbrd = (regtmp->boards + i);

      boards_[i] = new RegMapBoard(this, tbrd, i);
    }
  }

  //  COUT("Inside new RegMap with nboard = " << nboard_ << " 1");

}

/*.......................................................................
 * Create a map of available VME and software registers.
 *
 * Input:
 *
 *  regtmp  RegTemplate *  The template for creating the register map.
 *
 * Output:
 *
 *  return       RegMap *  The new register map.
 */
void RegMap::privateConstructorOld(void* vtmp)
{
  int i;
  RegTemplate* regtmp = (RegTemplate*)vtmp;
  using sza::util::LogStream;

  // A board with the following registers is inserted at the head of
  // every register map.
  
  static RegBlockTemp generic_regmap_board[] = {
    
    // This register is set to sza::util::FrameFlags::RECEIVED if this
    // frame has been received from a remote computer
    
    RegBlockTemp("Bit 1 will be high if data in this frame were received during the last "
		 "half-second integration.  If not, bit 0 will be set high", 
		 "received",   REG_DEFAULT, 0, 1),
    
    // The number of samples per integration 
    
    RegBlockTemp("The number of frames which have been integrated to produce this frame", 
		 "nsnap",      REG_UINT|REG_SUM, 0, 1),
    
    // The sequential record number 
    
    RegBlockTemp("A monotonically-increasing record number since the last re-start of the control system", 
		 "record",     REG_DEFAULT, 0, 1),
    
    // The date and time as MJD days and milli-seconds 
    
    RegBlockTemp("The MJD of this frame", 
		 "utc",        REG_UTC,     0, 1),
    
    // The Local Sidereal Time (integer milli-seconds) 
    
    RegBlockTemp("The local sidereal time corresponding to this frame (milliseconds)",
		 "lst",        REG_DEFAULT, 0, 1),
    
    // A bit-mask union of feature markers received from the control program 
    //  since the last frame was constructed. 
    
    RegBlockTemp("A bit-mask union of feature markers received from the control program "
		 "since the last frame was constructed.",
		 "features",   REG_UINT|REG_UNION, 0, 1),
    
    // The sequence mark-command sequence number associated with the
    // current value of the "features" register. 
    
    RegBlockTemp("The sequence mark-command sequence number associated with the "
		 "current value of the \"features\" register.",
		 "markSeq",    REG_UINT,           0, 1),
  };
  
  // And we will call this board the "frame" board
  
  static RegBoardTemp per_regmap_boards[] = {
    {"frame",   generic_regmap_board,   ARRAY_DIM(generic_regmap_board),  
     {0x000000U}, "Information about the data frame"}, // Soft 
  };
  
  static int nper_regmap_boards =
    sizeof(per_regmap_boards)/sizeof(per_regmap_boards[0]);
  
  
  // The following register blocks are inserted at the beginning of
  // every board.
  
  static RegBlockTemp per_brd_blocks[] = {
    
    // The status of the board is recorded in the following
    // register. Note that regdb_board_status_reg() and
    // snap_scanner_frame() assume that this is the first register of
    // each board.
    
    RegBlockTemp("", "status",  REG_DEFAULT, 0, 1),
  };
  
  static int nper_brd_blocks =
    sizeof(per_brd_blocks)/sizeof(per_brd_blocks[0]);
  
  // Check arguments.
  
  if(!regtmp) {
    LogStream errStr;
    errStr.appendMessage(true, "NULL register-map template.\n");
    throw Error(errStr);
  };
  
  if(!regtmp->boards || regtmp->nboard == 0) {
    LogStream errStr;
    errStr.appendMessage(true, "Empty board array.\n");
    throw Error(errStr);
  };
  
  // Before attempting any operation that might fail, initialize the
  // container at least up to the point at which it is safe to pass it
  // to del_RegMap().
  
  ref_count_    =  1;
  nboard_       =  regtmp->nboard + nper_regmap_boards;
  nreg_         =  0;
  narchive_     =  0;
  nByte_        =  0;
  nArcByte_     =  0;

  // Allocate the array of board descriptions.
  
  boards_.resize(nboard_);
  
  // Initialize the board descriptions at least up to a point at which
  // regmap can be passed to del_RegMap().
  
  for(i=0; i < nboard_; i++) {
    RegBoardTemp *tbrd = i < nper_regmap_boards ? 
      (per_regmap_boards + i) : (regtmp->boards + i - nper_regmap_boards);
    
    boards_[i] = new RegMapBoard(this, tbrd, i, per_brd_blocks, nper_brd_blocks);
  }
}

/**.......................................................................
 * Delete a regmap, checking the ref counter.
 */
RegMap* del_RegMap(RegMap* regmap) 
{
  if(regmap && --regmap->ref_count_ == 0) {
    delete regmap;
    return NULL;
  } else {
    return regmap;
  }
}

/*.......................................................................
 * Delete a register map created by new_RegMap().
 *
 * Input:
 *  regmap RegMap *  The register map to be deleted.
 * Output:
 *  return RegMap *  The deleted register map (ie. NULL).
 */
RegMap::~RegMap()
{
  // First delete the board array
  
  for(int iBoard=0; iBoard < nboard_; iBoard++) 
    if(boards_[iBoard] != 0)
      delete boards_[iBoard];
}

/*.......................................................................
 * Obtain an alias copy of a given register map. To discard an alias call
 * del_RegMap(regmap). This will only delete the register map once the
 * last alias has been deleted.
 *
 * Input:
 *  regmap   RegMap *  The register map that you want a copy of.
 * Output:
 *  return   RegMap *  The same as regmap.
 */
RegMap *alias_RegMap(RegMap *regmap)
{
  if(regmap)
    regmap->ref_count_++;  /* Increment the reference count of the object */
  
  return regmap;
}

/*.......................................................................
 * Lookup a named board from a given register map.
 *
 * Input:
 *  regmap       RegMap *  The register map to lookup the board from.
 *  board_name     char *  The name of the board.
 * Output:
 *  return  RegMapBoard *  The named board (see regmap.h), or NULL if not
 *                         found.
 */
RegMapBoard *find_RegMapBoard(RegMap *regmap, string board_name)
{
  if(!regmap) {
    lprintf(stderr, "find_RegMapBoard: NULL regmap.\n");
    return NULL;
  };
  
  // Lookup the named board.
  
  if(regmap->boardMap_.find(board_name) == regmap->boardMap_.end())
    return NULL;

  RegMapBoard* brdmap = regmap->boardMap_[board_name];

  return brdmap;
}

/**.......................................................................
 * Find a named register block on a given board.
 *
 * Input:
 *  board   RegMapBoard *  The board to find the register on.
 *  block_name     char *  The name of the register block.
 * Output:
 *  return  RegMapBlock *  The named register (see regmap.h), or NULL if
 *                         not found.
 */
RegMapBlock* RegMapBoard::findRegMapBlock(string blockName)
{
  // Lookup the named block
  
  if(blockMap_.find(blockName) == blockMap_.end())
    return NULL;

  return blockMap_[blockName];
}

/**.......................................................................
 * Find a named register block on a given board.
 *
 * Input:
 *  board   RegMapBoard *  The board to find the register on.
 *  block_name     char *  The name of the register block.
 * Output:
 *  return  RegMapBlock *  The named register (see regmap.h), or NULL if
 *                         not found.
 */
std::vector<RegMapBlock*> RegMapBoard::matchRegMapBlock(string regExpStr)
{
  std::vector<RegMapBlock*> matchedBlocks;
  sza::util::RegExpParser parser(regExpStr);
  
  // Search through the list of boards.  Ignore errors in the
  // input regexp string.
  
  try {
    for(unsigned iBlock=0; iBlock < blocks.size(); iBlock++) {
      std::string str(blocks[iBlock]->name_);
      if(parser.matches(str))
	matchedBlocks.push_back(blocks[iBlock]);
    }
  } catch(...) {};
  
  return matchedBlocks;
}

/**.......................................................................
 * Return a vector of board descriptors matching the input board
 * strings
 */
std::vector<RegMapBlock*> RegMapBoard::matchRegMapBlock(std::vector<std::string>& blockVec)
{
  std::vector<RegMapBlock*> matchedBlocks;
  
  // Search through the list of boards.  Ignore errors in the
  // input regexp string.
  
  try {
    for(unsigned iBlockString=0; iBlockString < blockVec.size(); iBlockString++) {
      sza::util::RegExpParser parser(blockVec[iBlockString]);

      for(unsigned iBlock=0; iBlock < blocks.size(); iBlock++) {
	std::string str(blocks[iBlock]->name_);
	if(parser.matches(str))
	  matchedBlocks.push_back(blocks[iBlock]);
      }
    }
  } catch(...) {};
  
  return matchedBlocks;
}

/*.......................................................................
 * Find a named register block on a given board.
 *
 * Input:
 *  board   RegMapBoard *  The board to find the register on.
 *  block_name     char *  The name of the register block.
 * Output:
 *  return  RegMapBlock *  The named register (see regmap.h), or NULL if
 *                         not found.
 */
RegMapBlock* find_RegMapBoard_Block(RegMapBoard *board, string block_name)
{
  return board->findRegMapBlock(block_name);
}

/*.......................................................................
 * Lookup a register by name.
 *
 * Input:
 *  regmap      RegMap *  The register map in which to find the register.
 *  board_name    char *  The name of the board.
 *  block_name    char *  The name of the register block.
 * Output:
 *  return RegMapBlock *  The named register block (see regmap.h), or
 *                        NULL if not found.
 */
RegMapBlock *find_RegMapBlock(RegMap *regmap, string board_name,
			      string block_name)
{
  RegMapBoard *board;   /* The named board */
  if(!regmap) {
    lprintf(stderr, "find_RegMapBlock: NULL regmap.\n");
    return NULL;
  };

  board = find_RegMapBoard(regmap, board_name);

  if(!board)
    return NULL;

  return find_RegMapBoard_Block(board, block_name);
}

/*.......................................................................
 * Parse a register specification from an input stream.
 *
 * Input:
 *  stream   InputStream *  The stream that contains the specification.
 *                          Trailing characters will be left unread.
 *  tell             int    If true, report errors to stderr.
 *  regmap        RegMap *  The register map in which to look up the
 *                          register, or NULL if only the syntax is to
 *                          be checked.
 *  mode    RegInputMode    The type of specification to expect, from:
 *                            REG_INPUT_BLOCK   - board.register
 *                            REG_INPUT_ELEMENT - board.register[index]
 *                            REG_INPUT_RANGE   - board.register[index-index]
 *  extend           int    If true allow the user to append, where
 *                          appropriate, one of the following components
 *                          to complex and utc register specifications:
 *                            .amp    -  The amplitude of a complex pair.
 *                            .phase  -  The phase of a complex pair.
 *                            .real   -  The real part of a complex pair.
 *                            .imag   -  The imaginary part of a complex pair.
 *                            .date   -  The Modified Julian Date of a utc pair.
 *                            .time   -  The time-of-day of a utc pair.
 *                          If the user uses these notations then the
 *                          selected attribute will be recorded in
 *                          RegMapReg::aspect.
 * Input/Output:
 *  reg        RegMapReg *  On return *reg will contain the register
 *                          specification parsed from the stream.
 * Output:
 *  return   RegValidity    REG_VALID (0) - A known register was read.
 *                          REG_UNKNOWN   - A syntactically correct
 *                                          specification of an unknown
 *                                          register was read (*reg is
 *                                          unchanged).
 *                          REG_INVALID   - The read was aborted by a
 *                                          syntax or usage error.
 */
RegValidity input_RegMapReg(InputStream *stream, int tell, RegMap *regmap,
			    RegInputMode mode, int extend, RegMapReg *reg)
{
  char brd_name[REG_NAME_LEN+1]; /* The name of the register-map boad */
  char blk_name[REG_NAME_LEN+1]; /* The name of the register boad */
  RegMapBoard *board=NULL;       /* A register board */
  RegMapBlock *block=NULL;       /* A named register block */
  long index1;                   /* The lowest block index */
  long index2;                   /* The highest block index (-1 for max) */
  RegAspect aspect;     /* The quantity to derive from complex register pairs */
  unsigned nmax=0;      /* The allowable range of indexes */
  unsigned size;        /* The number of slots per register element */
  int i;
  /*
   * Check arguments.
   */
  if(!stream) {
    lprintf(stderr, "input_RegMapReg: NULL argument.\n");
    return REG_INVALID;
  };
  /*
   * Validate the parsing mode.
   */
  switch(mode) {
  case REG_INPUT_BLOCK:
  case REG_INPUT_ELEMENT:
  case REG_INPUT_RANGE:
    break;
  default:
    lprintf(stderr, "input_RegMapReg: Unknown input mode.\n");
    return REG_INVALID;
  };
  /*
   * The next keyword is a board name. Get the associated
   * board definition.
   */
  if(input_keyword(stream, 0, 1)) {
    input_error(stream, tell, "Missing register-map board name.\n");
    return REG_INVALID;
  };
  /*
   * Copy the board name into brd_name[].
   */
  if(strlen(stream->work) > REG_NAME_LEN) {
    input_error(stream, tell, "Register board name too long.\n");
    return REG_INVALID;
  };
  strcpy(brd_name, stream->work);
  
  // The next character should be a period.
  
  if(stream->nextc != '.') {
    input_error(stream, tell,
		"Missing period after register-map board name.\n");
    return REG_INVALID;
  };
  
  // Skip the period and read the register name that follows it.
  
  if(read_InputStream(stream, 1))
    return REG_INVALID;
  if(input_keyword(stream, 0, 1)) {
    input_error(stream, tell, "Missing register name after \"%s.\"\n",
		brd_name);
    return REG_INVALID;
  };
  
  // Copy the register name into blk_name[].
  
  if(strlen(stream->work) > REG_NAME_LEN) {
    input_error(stream, tell, "Name of register block too long.\n");
    return REG_INVALID;
  };
  strcpy(blk_name, stream->work);
  
  // See if a register attribute has been specified.
  
  aspect = REG_PLAIN;
  if(stream->nextc == '.') {
    
    // Skip the period and read the attribute name that follows it.
    
    if(read_InputStream(stream, 1))
      return REG_INVALID;
    if(input_keyword(stream, 0, 1)) {
      input_error(stream, tell,
		  "Missing register attribute after '%s.%s.'\n",
		  brd_name, blk_name);
      return REG_INVALID;
    };
    
    // Attempt to identify the specified attribute.
    
    for(i=0; i < (int)REG_NASPECT; i++) {
      if(strcmp(stream->work, name_RegAspect((RegAspect)i)) == 0) {
	aspect = (RegAspect)i;
	break;
      };
    };
    if(i >= (int)REG_NASPECT) {
      input_error(stream, tell,
		  "\"%s\" is not a recognized register attribute (1).\n",
		  stream->work);
      return REG_INVALID;
    };
  };
  
  // Until otherwise specified, assume that the whole register block
  // has been selected.
  
  index1 = 0;
  index2 = -1;
  
  // See if the register name is followed by an index expression.
  
  if(stream->nextc == '[') {
    
    // Consume the '['.
    
    if(read_InputStream(stream, 1))
      return REG_INVALID;
    
    // If the next character is the terminator of the index expression
    // then the complete register block should remain
    // selected. Otherwise attempt to read the start index.
    
    if(stream->nextc != ']') {
      
      // If no number precedes the range separator then the start
      // index should remain at the default of 0.
      
      if(stream->nextc != '-') {
	
	// Read the start index.
	
	if(input_long(stream, 0, 0, &index1)) {
	  input_error(stream, tell, "Invalid register index after [.\n");
	  return REG_INVALID;
	};
	
	// If the next character is the index-expression terminator
	// then the sole index denotes a single element so set the end
	// index to the same value.
	
	if(stream->nextc == ']')
	  index2 = index1;
      };
      
      // If the next character is a range separator then consume it
      // and see what follows.
      
      if(stream->nextc == '-') {
	if(read_InputStream(stream, 1))
	  return REG_INVALID;
	
	// If the next character is the expression terminator, then
	// leave the end index at its end-of-block default. Otherwise
	// read the end index.
	
	if(stream->nextc != ']') {
	  
	  // Read the end index.
	  
	  if(!isdigit(stream->nextc) || input_long(stream, 0, 0, &index2)) {
	    input_error(stream, tell, "Invalid register index after '-'.\n");
	    return REG_INVALID;
	  };
	};
      };
    };
    
    // Make sure that the expression is terminated and consume the
    // terminator.
    
    if(stream->nextc != ']') {
      input_error(stream, tell, "Unterminated register index expression.\n");
      return REG_INVALID;
    };
    
    // Skip the ']'.
    
    if(read_InputStream(stream, 1))
      return REG_INVALID;
  };
  
  // First see if the named register exists in the current register
  // map.
  
  if(regmap) {
    board = find_RegMapBoard(regmap, brd_name);
    block = board ? find_RegMapBoard_Block(board, blk_name) : NULL;
    if(!board || !block) {
      input_error(stream, tell, "Unknown register %s.%s.\n", brd_name,
		  blk_name);
      return REG_UNKNOWN;
    };
  };
  
  // If a derived aspect was specified, check that this is allowed,
  // and determine how many elements are needed to make up the derived
  // attribute.
  
  switch(aspect) {
  case REG_PLAIN:
    size = 1;
    break;
  case REG_REAL:
  case REG_IMAG:
  case REG_AMP:
  case REG_PHASE:
    size = 2;   // Real, Imaginary 
    if(!extend || (regmap && (block->flags_ & REG_COMPLEX) == 0)) {
      (void) input_error(stream, tell,
			 "Unexpected complex attribute specified for register %s.%s.\n",
			 brd_name, blk_name);
      return REG_INVALID;
    };
    break;
  case REG_DATE:
  case REG_TIME:
    size = 2;  // Modified Julian day number, time-of-day (ms)
    if(!extend || (regmap && (block->flags_ & REG_UTC) == 0)) {
      (void) input_error(stream, tell,
			 "Unexpected UTC attribute specified for register %s.%s.\n",
			 brd_name, blk_name);
      return REG_INVALID;
    };
    break;
  default:
    size = 0;
    lprintf(stderr, "input_RegMapReg: Missing aspect (%d) in switch.\n",
	    (int) aspect);
    return REG_INVALID;
    break;
  };
  
  // Determine the max index according to the number of slots per
  // element.
  
  nmax = regmap ? block->nreg_ / size :
    (index2 >= 0 ? (index2+1):(index1 >= 0 ? (index1+1):1));
  
  // If no upper limit was provided, substitute nmax.
  
  if(index2 < 0)
    index2 = nmax-1;
  
  // Make sure that the limits are in ascending order.
  
  if(index1 > index2) {
    long ltmp = index1;
    index1 = index2;
    index2 = ltmp;
  };
  
  // Check that the indexes are in range (note that the above sort
  // means that we only have to check index2).
  
  if(regmap && index2 >= (long)nmax) {
    int is_plain = aspect == REG_PLAIN;
    input_error(stream, tell,
		"Register %s.%s%s%s[0-%d] doesn't have an element %ld.\n",
		brd_name, blk_name, is_plain ? "":".",
		is_plain ? "" : name_RegAspect(aspect),
		nmax-1, index2);
    
    // If none of the specified elements are valid then report the
    // register as unknown. Otherwise report it as invalid.
    
    return index1 >= (long)nmax ? REG_UNKNOWN : REG_INVALID;
  };
  
  // Record the results for return.
  
  if(reg) {
    reg->board = regmap ? board->number : -1;
    reg->block = regmap ? block->number_ : -1;
    reg->index = index1;
    reg->slot = (!regmap || block->slot_ < 0) ? -1 :
      (block->slot_ + index1 * size);
    reg->nreg = index2 - index1 + 1;
    reg->size = size;
    reg->aspect = aspect;
    
    // See if the specification matches the mode.
    
    switch(mode) {
    case REG_INPUT_BLOCK:
      if(regmap && reg->nreg*size != block->nreg_) {
	input_error(stream, tell,
		    "In the current context %s.%s can't take an index expression.\n",
		    brd_name, blk_name);
	return REG_INVALID;
      };
      break;
    case REG_INPUT_ELEMENT:
      if(regmap && reg->nreg != 1) {
	input_error(stream, tell,
		    "You must choose an element from %s.%s[%d].\n",
		    brd_name, blk_name, reg->nreg);
	return REG_INVALID;
      };
      break;
    case REG_INPUT_RANGE:
      break;
    };
  };
  return regmap ? REG_VALID : REG_UNKNOWN;
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
int output_RegMapReg(OutputStream *stream, RegMap *regmap, RegOutputMode mode,
		     RegMapReg *reg)
{
  RegMapBoard *brd;  /* The board specified in *reg */
  RegMapBlock *blk;  /* The block specified in *reg */
  
  // Check the arguments.
  
  if(!stream || !regmap || !reg) {
    lprintf(stderr, "output_RegMapReg: NULL argument.\n");
    return 1;
  };
  
  // Get the board and block objects that correspond to the
  // specifications in *reg.
  
  if(reg->board >= regmap->nboard_) {
    lprintf(stderr, "output_RegMapReg: Board index out of range.\n");
    return 1;
  };
  brd = regmap->boards_[reg->board];
  if(reg->block >= brd->nblock) {
    lprintf(stderr, "output_RegMapReg: Block index out of range.\n");
    return 1;
  };
  blk = brd->blocks[reg->block];
  
  // Check that the index and nreg values make sense. Note that
  // reg->index is unsigned, so there is no need to check whether it
  // is < 0.
  
  if((reg->index + reg->nreg) * reg->size > blk->nreg_) {
    lprintf(stderr, "output_RegMapReg: Slot numbers out of range.\n");
    return 1;
  };
  /*
   * Validate the output mode.
   */
  switch(mode) {
  case REG_OUTPUT_BLOCK:
  case REG_OUTPUT_ELEMENT:
  case REG_OUTPUT_RANGE:
    break;
  default:
    lprintf(stderr, "output_RegMapReg: Unknown output format.\n");
    return 1;
  };
  /*
   * Output the fully qualified name of the register.
   */
  if(write_OutputStream(stream, brd->name) ||
     write_OutputStream(stream, ".") ||
     write_OutputStream(stream, blk->name_))
    return 1;
  /*
   * Write the aspect if needed.
   */
  if(reg->aspect != REG_PLAIN &&
     (write_OutputStream(stream, ".") ||
      write_OutputStream(stream, name_RegAspect(reg->aspect))))
    return 1;
  /*
   * Write the appropriate index specification.
   */
  switch(mode) {
  case REG_OUTPUT_ELEMENT:
    if(blk->nreg_ > 1) {
      if(write_OutputStream(stream, "[") ||
	 output_ulong(stream, OUT_DECIMAL, "", 0, 0, reg->index) ||
	 write_OutputStream(stream, "]"))
	return 1;
    };
    break;
  case REG_OUTPUT_RANGE:
    if(blk->nreg_ > 1) {
      if(reg->nreg == 1) {
	if(write_OutputStream(stream, "[") ||
	   output_ulong(stream, OUT_DECIMAL, "", 0, 0, reg->index) ||
	   write_OutputStream(stream, "]"))
	  return 1;
      } else {
	if(write_OutputStream(stream, "[") ||
	   output_ulong(stream, OUT_DECIMAL, "",0,0, reg->index) ||
	   write_OutputStream(stream, "-") ||
	   output_ulong(stream, OUT_DECIMAL, "",0,0, reg->index+reg->nreg-1) ||
	   write_OutputStream(stream, "]"))
	  return 1;
      };
    };
    break;
  default:
    break;
  };
  return 0;
}

/*.......................................................................
 * Initialize a register specification object to null defaults.
 * A null specification can be detected by reg->nreg==0. Or where an
 * archivable register is required, reg->slot==-1.
 *
 * Input:
 *  reg    RegMapReg *  The specification object to be reset.
 */
void clr_RegMapReg(RegMapReg *reg) {
  if(reg) {
    reg->board = 0;
    reg->block = 0;
    reg->index = 0;
    reg->slot = -1;
    reg->nreg = 0;
    reg->size = 0;
    reg->aspect = REG_PLAIN;
  };
}

/*.......................................................................
 * Initialize a RegMapReg from the minimum essential details.
 *
 * Input:
 *  regmap    RegMap *  The register map that the specification refers
 *                      to.
 *  board   unsigned    The board of the register.
 *  block   unsigned    The register block number.
 *  index   unsigned    The index of the first element in the block.
 *                      If aspect!=REG_PLAIN, this should be a multiple
 *                      of register pairs, rather than slots, so you may
 *                      need to divide by two.
 *  nreg    unsigned    The number of elements, starting from 'index'.
 *                      Note that if aspect!=REG_PLAIN, nreg is the
 *                      number of slot pairs, so you may need to divide
 *                      by 2.
 *  aspect RegAspect    The interpretation of the register selection, from:
 *                       REG_PLAIN - A normal array of register slots. This
 *                                   is the only interpretation supported
 *                                   for non-complex registers.
 *                                   Each register consumes one slot.
 *                       REG_AMP   - The amplitudes of complex registers.
 *                                   Each register consumes two slots.
 *                       REG_PHASE - The phases of complex registers.
 *                                   Each register consumes two slots.
 *                       REG_REAL  - The real parts complex registers.
 *                                   Each register consumes two slots.
 *                       REG_IMAG  - The imaginary parts of complex registers.
 *                                   Each register consumes two slots.
 *                       REG_DATE  - The Modified Julian Date of utc registers.
 *                                   Each register consumes two slots.
 *                       REG_TIME  - The time-of-day of utc registers.
 *                                   Each register consumes two slots.
 * Input/Output:
 *  reg    RegMapReg *  On success *reg will be initialized with the
 *                      specified information. On failure, *reg will
 *                      be unchanged.
 * Output:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
int init_RegMapReg(RegMap *regmap, unsigned board, unsigned block,
		   unsigned index, unsigned nreg, RegAspect aspect,
		   RegMapReg *reg)
{
  RegMapBoard *brd;  /* The board descriptor */
  RegMapBlock *blk;  /* The block descriptor */
  unsigned size;     /* The number of slots per register */
  unsigned byteSize; /* The number of bytes per register */
  
  // Check arguments.
  
  if(!regmap || !reg) {
    lprintf(stderr, "init_RegMapReg: NULL argument(s).\n");
    return 1;
  };
  
  // Get the board container.
  
  if(board >= (unsigned)regmap->nboard_) {
    lprintf(stderr, "init_RegMapReg: Board index %d out of range 0-%d.\n",
	    board, regmap->nboard_-1);
    return 1;
  };
  
  brd = regmap->boards_[board];
  
  // Get the block container.
  
  if(block >= brd->nblock) {
    lprintf(stderr, "init_RegMapReg: Block index %d out of range 0-%d.\n",
	    block, brd->nblock-1);
    return 1;
  };
  
  blk = brd->blocks[block];
  
  // Check the aspect and determine the corresponding register size.
  
  switch(aspect) {
  case REG_PLAIN:
    size = 1;
    break;
  case REG_REAL:
  case REG_IMAG:
  case REG_AMP:
  case REG_PHASE:
    if(!(blk->flags_ & REG_COMPLEX)) {
      lprintf(stderr, "init_RegMapReg: Bad aspect for non-complex register.\n");
      return 1;
    };
    size = 2;
    break;
  case REG_DATE:
  case REG_TIME:
    if(!(blk->flags_ & REG_UTC)) {
      lprintf(stderr, "init_RegMapReg: Bad aspect for non-UTC register.\n");
      return 1;
    };
    size = 2;
    break;
  default:
    lprintf(stderr, "init_RegMapReg: Invalid aspect.\n");
    return 1;
  };
  
  // Check the index range.
  
  if(nreg < 1 || index + nreg*size - 1 >= blk->nreg_) {
    lprintf(stderr, "init_RegMapReg: Invalid index range %d-%d.\n",
	    index, index + nreg - 1);
    return 1;
  };
  
  // Initialize the output register specification.
  
  reg->board = board;
  reg->block = block;
  reg->index = index;
  reg->slot = blk->slot_ < 0 ? -1 : (blk->slot_ + reg->index * size);
  reg->nreg = nreg;
  reg->size = size;
  reg->aspect = aspect;
  
  // The byte slot location of the first requested element of this
  // register is the byte offset of this block in the archived array,
  // plus the byte offset of this element from the start of the
  // register array
  
  Coord coord(index, 0);
  reg->iArcByte_   = blk->byteOffsetInArcRegMapOf(&coord);
  reg->iByte_      = blk->byteOffsetInWholeRegMapOf(&coord);
  reg->nBytePerEl_ = blk->nBytePerEl_;
  
  return 0;
}

/*.......................................................................
 * Return the name of a register aspect.
 *
 * Input:
 *  aspect   RegAspect    The aspect to stringize.
 * Output:
 *  return        char *  The name of the aspect.
 */
char *name_RegAspect(RegAspect aspect)
{
  switch(aspect) {
  case REG_PLAIN:
    return "plain";
    break;
  case REG_REAL:
    return "real";
    break;
  case REG_IMAG:
    return "imag";
    break;
  case REG_AMP:
    return "amp";
    break;
  case REG_PHASE:
    return "phase";
    break;
  case REG_DATE:
    return "date";
    break;
  case REG_TIME:
    return "time";
    break;
  };
  lprintf(stderr, "name_RegAspect: Unknown aspect.\n");
  return "";
}

/*.......................................................................
 * Return the name of a register integ.
 *
 * Input:
 *  integ   RegInteg    The integ to stringize.
 * Output:
 *  return        char *  The name of the integ.
 */
char *name_RegInteg(RegInteg integ)
{
  switch(integ) {
  case REG_INT_PLAIN:
    return "plain";
    break;
  case REG_INT_INT:
    return "int";
    break;
  case REG_INT_DER:
    return "der";
    break;
  };

  lprintf(stderr, "name_RegInteg: Unknown aspect.\n");
  return "";
}

/*.......................................................................
 * Lookup a register description by name. Note that if facet!=REG_PLAIN
 * the index and n arguments refer to register pairs, not to individual
 * elements.
 *
 * Input:
 *  regmap     RegMap *  The register map to search.
 *  board        char *  The name of the board that the register belongs
 *                       to.
 *  facet   RegAspect *  The desired interpretation of the register.
 *                       This is only relevant to complex and UTC registers.
 *                       Pass REG_PLAIN for other registers.
 *  index    unsigned    The first register element to select. Zero
 *                       refers to the first element of the register.
 *  n        unsigned    The number of elements to select, starting from
 *                       'index'. As a special case 0 selects all of the
 *                       elements in the register, except those that
 *                       precede 'index'.
 * Input/Output:
 *  reg     RegMapReg *  On output *reg will contain the details of the
 *                       selected register.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Register not found.
 */
int find_RegMapReg(RegMap *regmap, string board_name, string block_name,
		   RegAspect facet, unsigned index, unsigned n,
		   RegMapReg *reg)
{
  RegMapBlock *blk;   /* The register map block that contains the register */
  unsigned size;      /* The number of slots per register element */
  /*
   * Check the arguments.
   */
  if(!regmap) {
    lprintf(stderr, "find_RegMapReg: NULL argument(s).\n");
    return 1;
  };
  /*
   * Attempt to locate the specified register by name.
   */
  blk = find_RegMapBlock(regmap, board_name, block_name);
  if(!blk) {
    lprintf(stderr, "Unable to find register %s.%s\n", board_name.c_str(), 
	    block_name.c_str());
    return 1;
  };
  /*
   * Get the number of slots per register element.
   */
  switch(facet) {
  case REG_PLAIN:
    size = 1;
    break;
  case REG_REAL:
  case REG_IMAG:
  case REG_AMP:
  case REG_PHASE:
  case REG_DATE:
  case REG_TIME:
    size = 2;
    break;
  default:
    lprintf(stderr, "find_RegMapReg: Invalid aspect.\n");
    return 1;
  };
  /*
   * Check the requested dimensions.
   */
  if(size * (index + n) > blk->nreg_) {
    lprintf(stderr, "Dimensions out of range in %s.%s[%u-%u]\n", 
	    board_name.c_str(),
	    block_name.c_str(), index, index + n - 1);
    return 1;
  };
  /*
   * Fill in the return container.
   */
  if(init_RegMapReg(regmap, blk->brd_->number, blk->number_, index,
		    n ? n : blk->nreg_/size - index, facet, reg))
    return 1;
  return 0;
}

/*.......................................................................
 * Return true if two register maps contain equivalent information.
 *
 * Input:
 *  regmap1   RegMap *  The first of the register maps to compare.
 *  regmap2   RegMap *  The second of the register maps to compare.
 * Output:
 *  return       int    1 - The two register maps are equivalent.
 *                      0 - The two register maps are either not equivalent
 *                          or one or both of them is NULL.
 */
int equiv_RegMap(RegMap *regmap1, RegMap *regmap2)
{
  int board;
  
  // Are we missing either of the register maps?
  
  if(!regmap1 || !regmap2)
    return 0;
  
  // Compare the general characteristics of the two maps.
  
  if(regmap1->nboard_ != regmap2->nboard_ ||
     regmap1->nreg_ != regmap2->nreg_ ||
     regmap1->narchive_ != regmap2->narchive_)
    return 0;
  
  // Compare each of the boards of the maps.
  
  for(board=0; board<regmap1->nboard_; board++) {
    RegMapBoard *brd1 = regmap1->boards_[board];
    RegMapBoard *brd2 = regmap2->boards_[board];
    int block;
    
    // Check the general characteristics of each board.
    
    if(strcmp(brd1->name, brd2->name) != 0 ||
       brd1->nblock   != brd2->nblock ||
       brd1->nreg     != brd2->nreg ||
       brd1->narchive != brd2->narchive)
      return 0;
    
    // Compare the register blocks of the board.
    
    for(block=0; block<brd1->nblock; block++) {
      
      RegMapBlock *blk1 = brd1->blocks[block];
      RegMapBlock *blk2 = brd2->blocks[block];
      
      if(strcmp(blk1->name_, blk2->name_) != 0 ||
	 blk1->flags_     != blk2->flags_ ||
	 blk1->addr_mode_ != blk2->addr_mode_ ||
	 blk1->location_  != blk2->location_ ||
	 blk1->ireg_      != blk2->ireg_ ||
	 blk1->nreg_      != blk2->nreg_ ||
	 blk1->slot_      != blk2->slot_)
	return 0;
    };
  };
  
  // The register maps are equivalent.
  
  return 1;
}

/*.......................................................................
 * Pack a printable string into a consecutive array of >= 32-bit
 * unsigned integers. Each sequence of 4 characters is treated as 4
 * digits of a base-256 integer. This is independent of the byte-order
 * of the integer and thus portable between architectures.
 *
 * Input:
 *  string     char *   The string to be packed.
 *  ndata       int     The number of output integers available. The
 *                      packed output string will be terminated with
 *                      a base-256 digit of zero when a '\0' character
 *                      is encountered in string[], or after a maximum
 *                      of ndata * 4 - 1 characters have been written.
 * Input/Output:
 *  data   unsigned *   The output array of ndata integers.
 * Output:
 *  return      int     0 - OK.
 *                      1 - Error.
 */
int pack_int_string(char *string, int ndata, unsigned *data)
{
  int i,j;
  /*
   * Check the arguments.
   */
  if(!string || !data || ndata < 1) {
    lprintf(stderr, "pack_int_string: Missing arguments.\n");
    return 1;
  };
  /*
   * Clear all elements of the integer.
   */
  for(i=0; i<ndata; i++)
    data[i] = 0U;
  /*
   * Pack 4 characters per integer.
   */
  for(i=0; i<ndata; i++) {
    /*
     * Get the next 4-character string segment.
     */
    unsigned char *str = (unsigned char *) string + i*4;
    /*
     * Pack the next 4 characters of the string[]. Stop if the
     * end of the string is encountered.
     */
    for(j=0; j<4 && str[j]; j++)
      data[i] |= (str[j] & 0xffU) << (3-j)*8U;
    /*
     * If the end of the string was encountered before filling all
     * four characters the remaining bytes will be 0 and thus
     * terminate the packed string. At this point the packing operation
     * is complete so return.
     */
    if(j<4)
      return 0;
  };
  /*
   * The string was too long, so truncate it.
   */
  data[ndata-1] &= ~0xffU;
  return 0;
}

/*.......................................................................
 * Unpack a string that was previously packed with pack_int_string().
 *
 * Input:
 *  data      unsigned *  The ndata >= 32-bit integer elements that
 *                        contain the packed string.
 *  ndata          int    The number of elements in data[].
 *  size           int    The size of the output string array.
 * Input/Output:
 *  string        char *  The output character array of size bytes.
 *                        The string will be terminated at element
 *                        size-1 if the packed string contains too
 *                        many characters to fit in this array.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int unpack_int_string(unsigned *data, int ndata, int size, char *string)
{
  int i,j;
  /*
   * Check the arguments.
   */
  if(!data || ndata < 1 || !string || size < 1) {
    lprintf(stderr, "unpack_int_string: Missing arguments.\n");
    return 1;
  };
  /*
   * Unpack 4 characters from each of the elements of data[].
   */
  for(i=0; i<ndata; i++) {
    /*
     * Get the next 4-character segment of the output string.
     */
    char *str = string + i*4;
    /*
     * Unpack the next 4 characters. Replace unprintable characters
     * with spaces.
     */
    for(j=0; j<4 && i*4+j < size; j++) {
      unsigned char c = (data[i] >> (3-j)*8U) & 0xffU;
      if(c) {
	str[j] = isprint(c) ? c : ' ';
      } else {                          /* End of string */
	str[j] = c;
	return 0;
      };
    };
  };
  /*
   * Truncate the string to fit the output array.
   */
  string[size-1] = '\0';
  return 0;
}

/*.......................................................................
 * Unpack a string that was previously packed with pack_int_string()
 * then assigned to a double precision variable. Note that the C standard
 * says that double must be able to exactly represent integers of at
 * least 10 decimal digits, so we are guaranteed that the 32-bit integer
 * values used to pack the string won't have been corrupted.
 *
 * Input:
 *  data        double *  The ndata >= 32-bit integer values converted
 *                        to ndata double's.
 *  ndata          int    The number of elements in data[].
 *  size           int    The size of the output string array.
 * Input/Output:
 *  string        char *  The output character array of size bytes.
 *                        The string will be terminated at element
 *                        size-1 if the packed string contains too
 *                        many characters to fit in this array.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int unpack_double_string(double *data, int ndata, int size, char *string)
{
  int i,j;
  /*
   * Check the arguments.
   */
  if(!data || ndata < 1 || !string || size < 1) {
    lprintf(stderr, "unpack_double_string: Missing arguments.\n");
    return 1;
  };
  /*
   * Unpack 4 characters from each of the elements of data[].
   */
  for(i=0; i<ndata; i++) {
    /*
     * Get the next 4-character segment of the output string.
     */
    char *str = string + i*4;
    /*
     * Get the next double decode.
     */
    double d = data[i];
    /*
     * Convert it to an unsigned integer of at least 32 bits and
     * make sure that it fits into this value. Be careful to avoid
     * undefined behavior just in case d wasn't packed by pack_int_string().
     */
    unsigned long u = (d < 0 || d > ULONG_MAX) ? 0 : (unsigned long)d;
    /*
     * Unpack the next 4 characters from u. Replace unprintable characters
     * with spaces.
     */
    for(j=0; j<4 && i*4+j < size; j++) {
      unsigned char c = (u >> (3-j)*8U) & 0xffU;
      if(c) {
	str[j] = isprint(c) ? c : ' ';
      } else {                          /* End of string */
	str[j] = c;
	return 0;
      };
    };
  };
  /*
   * Truncate the string to fit the output array.
   */
  string[size-1] = '\0';
  return 0;
}

//-----------------------------------------------------------------------
// RegMapBlock
//-----------------------------------------------------------------------


RegMapBlock::RegMapBlock(RegMapBoard* parent, void* vbrd, void* vblk, 
			 unsigned iBlock, unsigned nper_brd_blocks)
{
  using sza::util::LogStream;
  RegBoardTemp* tbrd = (RegBoardTemp*) vbrd;
  RegBlockTemp* tblk = (RegBlockTemp*) vblk;
  
  //  COUT("Inside RegMapBlock with name = " << tblk->name_ << " 0");

  // Convenience pointers
  
  RegMapBoard* board = parent;
  RegMap* regmap     = parent->regmap;
  
  // Initialize internal variables
  
  axes_       = 0;
  brd_        = parent;
  number_     = iBlock;
  flags_      = tblk->flags_;
  addr_mode_  = tblk->addr_mode_;
  location_   = tblk->address_ + tbrd->bases_[tblk->base_];
  ireg_       = regmap->nreg_;
  nreg_       = tblk->nEl();
  iByte_      = regmap->nByte_;
  iSlot_      = regmap->nreg_;
  nBytePerEl_ = tblk->nBytePerEl();
  comment_    = 0;

  carmaUnits_   = 0;
  carmaErrors_  = 0;
  carmaValidityBitIndex_ = tblk->carmaValidityBitIndex_;

  // Set the axes
  
  axes_       = new CoordAxes(tblk->axes_);
  
  //  COUT("Inside RegMapBlock with name = " << tblk->name_ << " 1");

  // Copy the name
  
  strcpy(name_, tblk->name_);
  
  // Copy any comments

  comment_    = new std::string(*tblk->comment_);

  //  COUT("Inside RegMapBlock with name = " << tblk->name_ << " 2");

  carmaUnits_   = new std::string(*tblk->carmaUnits_);

  carmaErrors_ = new std::vector<std::pair<std::string, std::string> >;
  for(unsigned i=0; i < tblk->carmaErrors_->size(); i++) {
    std::pair<std::string, std::string>& errPair = tblk->carmaErrors_->at(i);
    carmaErrors_->push_back(errPair);
  }

  // If the block is to be archived, allocate space for it in the
  // archive array and update the counts of archived registers.

  //  COUT("Inside RegMapBlock with name = " << tblk->name_ << " 3");  

  if(flags_ & REG_EXC) {
    slot_     = -1;
    iArcSlot_ = -1;
    iArcByte_ = -1;
  } else {
    slot_     = regmap->narchive_;
    iArcSlot_ = regmap->narchive_;
    iArcByte_ = regmap->nArcByte_;
    
    // Deprecated -- tally the number of registers
    
    board->narchive      += nreg_;
    regmap->narchive_    += nreg_;
    
    // New -- tally the number of bytes
    
    board->nArcByte_  += nByte();
    regmap->nArcByte_ += nByte();
  }
  
  // Deprecated -- keep a tally of the number of registers per
  // board and the total number of registers in the register map.
  
  board->nreg       += nreg_;
  regmap->nreg_     += nreg_;
  
  // New -- tally the number of bytes
  
  board->nByte_     += nByte();
  regmap->nByte_    += nByte();
  
  // All the register blocks of a given board must have
  // unambiguous names.
  
  if(parent->blockMap_.find(name_) != parent->blockMap_.end()) {
    ThrowError("Bad name for block " << iBlock 
	       << " of board " << parent->name);
  } else {
    parent->blockMap_[name_] = this;
  }

  //  COUT("Inside RegMapBlock with name = " << tblk->name_ << " 4");  

  // Check the addressing mode and compute the location at which
  // the register block is mapped into VxWorks address space.
  
  switch(addr_mode_) {
  case ADDR_A24D08_O:
  case ADDR_A24D08_EO:
  case ADDR_A24D16:
  case ADDR_A24D16_L:
    location_ += VME_A24D16_BASE;
    break;
  case ADDR_A24D32:
    location_ += VME_A24D32_BASE;
    break;
  case ADDR_A16D08_O:
  case ADDR_A16D08_EO:
  case ADDR_A16D16:
  case ADDR_A16D32:
  case ADDR_A16D16_L:
    location_ += VME_A16_BASE;
    break;
  case ADDR_A32D08_O:
  case ADDR_A32D08_EO:
  case ADDR_A32D16:
  case ADDR_A32D32:
  case ADDR_A32D16_L:
    location_ += VME_A32_BASE;
    break;
  case ADDR_DEFAULT:
    break;
  default:
    {
      LogStream errStr;
      errStr.initMessage(true);
      errStr << "Unknown address mode for " 
	     << parent->name << "." << name_ << endl;
      throw Error(errStr);
    }
    break;
  };
  
  //  COUT("Inside RegMapBlock with name = " << tblk->name_ << " 5");

  // Check the attributes for internal consistency
  
  checkConsistency();
};

/**.......................................................................
 * Return the number of elements in this register block
 */
RegMapBlock::RegMapBlock()
{
  axes_       = 0;
  brd_        = 0;
  number_     = 0;
  flags_      = 0;
  addr_mode_  = ADDR_DEFAULT;
  location_   = 0;
  ireg_       = 0;
  nreg_       = 0;
  iByte_      = 0;
  nBytePerEl_ = 0;
  comment_    = 0;

  carmaUnits_   = 0;
  carmaErrors_  = 0;
  carmaValidityBitIndex_ = -1;
}

/**.......................................................................
 * Return the number of elements in this register block
 */
RegMapBlock::~RegMapBlock()
{
  if(axes_ != 0) {
    delete axes_;
    axes_ = 0;
  }

  if(comment_ != 0) {
    delete comment_;
    comment_ = 0;
  }

  if(carmaUnits_ != 0) {
    delete carmaUnits_;
    carmaUnits_ = 0;
  }

  if(carmaErrors_ != 0) {
    delete carmaErrors_;
    carmaErrors_ = 0;
  }
}

bool RegMapBlock::isArchived()
{
  return !(flags_ & REG_EXC);
}

bool RegMapBlock::isBool()
{
  return (flags_ & REG_BOOL);
}

bool RegMapBlock::isChar()
{
  return (flags_ & REG_CHAR);
}

bool RegMapBlock::isUchar()
{
  return (flags_ & REG_UCHAR);
}

bool RegMapBlock::isString()
{
  return (flags_ & REG_STRING);
}

bool RegMapBlock::isUshort()
{
  return (flags_ & REG_USHORT);
}

bool RegMapBlock::isShort()
{
  return (flags_ & REG_SHORT);
}

bool RegMapBlock::isUint()
{
  return (flags_ & REG_UINT);
}

bool RegMapBlock::isInt()
{
  return (flags_ & REG_INT);
}

bool RegMapBlock::isFloat()
{
  return (flags_ & REG_FLOAT);
}

bool RegMapBlock::isDouble()
{
  return (flags_ & REG_DOUBLE);
}

bool RegMapBlock::isUtc()
{
  return (flags_ & REG_UTC);
}

// Other flags

bool RegMapBlock::isPostAveraged()
{
  return flags_ & REG_POSTAVG;
}

bool RegMapBlock::isPreAveraged()
{
  return flags_ & REG_PREAVG;
}

bool RegMapBlock::isFirst()
{
  return flags_ & REG_FIRST;
}

bool RegMapBlock::isSummed()
{
  return flags_ & REG_SUM;
}

bool RegMapBlock::isUnioned()
{
  return flags_ & REG_UNION;
}

bool RegMapBlock::isComplex()
{
  return flags_ & REG_COMPLEX;
}

/**.......................................................................
 * Check flags for self-consistency
 */
void RegMapBlock::checkConsistency()
{
  checkType(flags_);
  checkIntegration(flags_);
  checkAttributes();
}

/**.......................................................................
 * Check the type specification
 */
void RegMapBlock::checkType(unsigned flags)
{
  bool typeIsSet=false;
  bool error=false;
  
  if(flags & REG_BOOL)
    typeIsSet = true;
  
  if(flags & REG_CHAR) {
    if(typeIsSet)
      error = true;
    else
      typeIsSet = true;
  }
  
  if(flags & REG_SHORT) {
    if(typeIsSet)
      error = true;
    else
      typeIsSet = true;
  }
  
  if(flags & REG_INT) {
    if(typeIsSet)
      error = true;
    else
      typeIsSet = true;
  }
  
  if(flags & REG_FLOAT) {
    if(typeIsSet)
      error = true;
    else
      typeIsSet = true;
  }
  
  if(flags & REG_DOUBLE) {
    if(typeIsSet)
      error = true;
    else
      typeIsSet = true;
  }
  
  if((flags & REG_UTC) && typeIsSet)
    error = true;
  else
    typeIsSet = true;
  
  if(error)
    ThrowError("Register: " << *this 
	       << " has more than one type specification");
  
  if(!typeIsSet)
    ThrowError("Register: " << *this 
	       << " has no type specification");
}

/**.......................................................................
 * Check the integration specification
 */
void RegMapBlock::checkIntegration(unsigned flags)
{
  // Registers can only have one integration specification
  
  if((flags & REG_SUM) && (flags & REG_UNION))
    ThrowError("Register: " << *this << " cannot be both summed and unioned");
  
  // It doesn't make sense to union floating point types
  
  if((flags & REG_UNION) && (flags & (REG_FLOAT|REG_DOUBLE)))
    ThrowError("Floating-point register: " << *this << " cannot be unioned");
  
  // It doesn't make sense to sum boolean types
  
  if((flags & REG_SUM) && (flags & REG_BOOL))
    ThrowError("Boolean register: " << *this << " cannot be summed");
}

/**.......................................................................
 * Check if the combination of attributes is valid for this register
 */
void RegMapBlock::checkAttributes()
{
  if((flags_ & REG_COMPLEX) && !(flags_ & REG_FLOAT))
    ThrowError("Complex register: " << *this << " can only consist of floats");
  
  if(nEl() < 1)
    ThrowError("Register: " << *this << " has no elements");
}

/**.......................................................................
 * Return the number of elements in this register block
 */
unsigned RegMapBlock::nEl()
{
  return axes_->nEl();
}

/**.......................................................................
 * Return the number of bytes in this block
 */
unsigned RegMapBlock::nByte()
{
  return axes_->nEl() * nBytePerEl_;
}

/**.......................................................................
 * Return the offset, in bytes, of the requested element of this
 * register in the register map archive array
 */
int RegMapBlock::byteOffsetInArcRegMapOf(Coord* coord)
{
  // If this array is not archived, return < 0
  
  if(iArcByte_ < 0)
    return -1;
  
  // Else return the byte offset of the requested element of this
  // register in the archive array.
  
  return iArcByte_ + axes_->elementOffsetOf(coord) * nBytePerEl_;
}

/**.......................................................................
 * Return the offset, in bytes, of the requested element of this
 * register in the whole register map array
 */
int RegMapBlock::byteOffsetInWholeRegMapOf(Coord* coord)
{
  // Else return the byte offset of the requested element of this
  // register in the archive array.
  
  return iByte_ + axes_->elementOffsetOf(coord) * nBytePerEl_;
}

/**.......................................................................
 * Return the offset, in elements, of the requested element of this
 * register in the register map archive array
 */
int RegMapBlock::elementOffsetOf(sza::util::Coord* coord)
{
  // Else return the element offset of the requested element of this
  // register in the archive array.
  
  return axes_->elementOffsetOf(coord);
}

/**.......................................................................
 * An operator for printing a register block
 */
ostream& operator<<(ostream& os, RegMapBlock& block)
{
  if(block.brd_ != 0)
    os << block.brd_->name << ".";
  os << block.name_;
  
  return os;
}

//-----------------------------------------------------------------------
// RegMapBoard
//-----------------------------------------------------------------------

/**.......................................................................
 * Return the number of elements in this register block
 */
RegMapBoard::RegMapBoard()
{
  regmap        =  0;
  number        =  0;
  name[0]       =  '\0';
  nblock        =  0;
  nreg          =  0;
  narchive      =  0;
  nByte_        =  0;
  nArcByte_     =  0;
  iByte_        =  0;
  iArcByte_     = -1;
  iSlot_        =  0;
  iArcSlot_     = -1;
  comment_      =  0;
}

/**.......................................................................
 * Constructor with intialization
 */
RegMapBoard::RegMapBoard(RegMap* parent, void* vbrd,
			 unsigned iboard, 
			 void* vper_brd_blocks, unsigned nper_brd_blocks)
{
  RegBoardTemp* tbrd = (RegBoardTemp*)vbrd;
  RegBlockTemp* per_brd_blocks = (RegBlockTemp*)vper_brd_blocks;
  
  using sza::util::LogStream;
  
  regmap        =  parent;
  number        =  iboard;
  nblock        =  tbrd->nblock_ + nper_brd_blocks;
  nreg          =  0;
  narchive      =  0;
  nByte_        =  0;
  nArcByte_     =  0;
  iByte_        =  0;
  iArcByte_     = -1;
  iSlot_        =  0;
  iArcSlot_     = -1;
  comment_      =  0;

  // And copy the name

  //  COUT("Inside RegMapBoard with name = " << tbrd->name_ << " 0");

  strcpy(name, tbrd->name_);

  //  COUT("Inside RegMapBoard with name = " << tbrd->name_ << " 1");
  
  // Copy the comment

  comment_ = new std::string(tbrd->comment_);

  //  COUT("Inside RegMapBoard with name = " << tbrd->name_ << " 2");

  // The board must have at least one block of registers.
  
  if(nblock < 1) {
    ThrowError("Board " << name << " contains no registers.");
  };
  
  // Initialize the starting byte of this board in the register map
  
  iByte_     = regmap->nByte_;
  iArcByte_  = regmap->nArcByte_;
  
  // And the starting slot of this board in the register map.
  
  iSlot_     = regmap->nreg_;
  iArcSlot_  = regmap->narchive_;
  
  // All the register blocks of a given board must have
  // unambiguous names.
  
  //  COUT("Inside RegMapBoard with name = " << tbrd->name_ << " 3");

  if(parent->boardMap_.find(name) != parent->boardMap_.end()) {
    ThrowError("Bad name: " <<  name << " for board");
  } else {
    parent->boardMap_[name] = this;
  }

  //  COUT("Inside RegMapBoard with name = " << tbrd->name_ << " and nblock = " << nblock);

  // Allocate an array of register-block descriptions.
  
  
  blocks.resize(nblock);
  
  // Before attempting any operations that might fail, initialize
  // the elements of the block array at least up to the point at
  // which the array can safely be handled by del_RegMap().
  
  for(int iBlock=0; iBlock < nblock; iBlock++) {
    
    RegBlockTemp* tblk = (iBlock < nper_brd_blocks) ? (per_brd_blocks + iBlock) : 
      (tbrd->blocks_ + iBlock - nper_brd_blocks);
    
    blocks[iBlock] = 0;
    blocks[iBlock] = new RegMapBlock(this, (void*)tbrd, (void*)tblk, 
				     iBlock, nper_brd_blocks);
  }
};

/**.......................................................................
 * Return the number of elements in this register block
 */
RegMapBoard::~RegMapBoard()
{
  // Delete the block array.
  
  for(unsigned iBlock=0; iBlock < nblock; iBlock++) 
    if(blocks[iBlock] != 0) {
      delete blocks[iBlock];
      blocks[iBlock] = 0;
    }
  
  // delete any comment field which was allocated

  if(comment_ != 0) {
    delete comment_;
    comment_ = 0;
  }
}

/**.......................................................................
 * Return the offset, in bytes_ of the named board and block from
 * the start of the archived array
 */
int RegMap::byteOffsetInArcRegMapOf(RegMapBlock* blk, Coord* coord)
{
  // Throw an error if the requested register doesn't exist in the
  // register map
  
  if(blk == 0) {
    sza::util::LogStream logStr;
    logStr.appendMessage(true, "Received NULL register descriptor\n");
    throw Error(logStr);
  }
  
  // Else return the offset
  
  return blk->byteOffsetInArcRegMapOf(coord);
}

int RegMap::byteOffsetInArcRegMapOf(string board, string block, Coord* coord)
{
  try {
    RegMapBlock* blk = find_RegMapBlock(this, board, block);
    return byteOffsetInArcRegMapOf(blk, coord);
  } catch(Exception& err) {
    ThrowError("Caught exception: " << err.what() << 
	       " while looking up offset for register: " <<
	       board << "." << block);
  }
}

/**.......................................................................
 * Return the offset, in bytes_ of the named board and block from
 * the start of the whole register array
 */
int RegMap::byteOffsetInWholeRegMapOf(RegMapBlock* blk, Coord* coord)
{
  // Throw an error if the requested register doesn't exist in the
  // register map
  
  if(blk == 0) {
    sza::util::LogStream logStr;
    logStr.appendMessage(true, "Received NULL register descriptor\n");
    throw Error(logStr);
  }
  
  // Else return the offset
  
  return blk->byteOffsetInWholeRegMapOf(coord);
}

int RegMap::byteOffsetInWholeRegMapOf(string board, string block, Coord* coord)
{
  try {
  RegMapBlock* blk = find_RegMapBlock(this, board, block);
  return byteOffsetInWholeRegMapOf(blk, coord);
  } catch(Exception& err) {
    ThrowError("Caught exception: " << err.what() << 
	       " while looking up offset for register: " <<
	       board << "." << block);
  }
}

/**.......................................................................
 * Find the named block.  If doThrow is true, throw an error if the
 * register wasn't found.
 */
RegMapBlock* RegMap::findRegMapBlock(string board_name, string block_name,
				     bool doThrow)
{
  RegMapBlock* blk = find_RegMapBlock(this, board_name, block_name);
  
  if(doThrow && blk==0) {
    sza::util::LogStream errStr;
    errStr.initMessage(true);
    errStr << "Lookup of " << board_name << "." << block_name
	   << " failed." << endl;
    throw Error(errStr);
  }
  
  return blk;
}

