#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

#include "carma/szaarrayutils/arraytemplate.h"
#include "carma/szaarrayutils/lprintf.h"

#include <string>

#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/RegExpParser.h"

using namespace sza::util;
using namespace std;

/**.......................................................................
 * Create a map of available register maps
 *
 * Input:
 *  arrayTmp  ArrayTemplate * The template for creating the array map
 * Output:
 *  return    ArrayMap      * The new array map.
 */
ArrayMap::ArrayMap(void* vtmp, bool old, bool addPerBrdRegs)
{
  ArrayTemplate* arraytmp = (ArrayTemplate* )vtmp;
  using sza::util::LogStream;
  int i;
  
  // Check arguments.

  if(arraytmp==NULL) {
    LogStream errStr;
    errStr.appendMessage(true, "NULL array template.\n");
    throw Error(errStr);
  };

  if(arraytmp->templates == NULL || arraytmp->ntemplate == 0) {
    LogStream errStr;
    errStr.appendMessage(true, "Empty register map array.\n");
    throw Error(errStr);
  };
  
  // Before attempting any operation that might fail, initialize the
  // container at least up to the point at which it is safe to pass it
  // to del_ArrayMap().

  ref_count     = 1;
  nregmap       = arraytmp->ntemplate;
  nreg          = 0;
  narchive      = 0;

  nByte_        = 0;
  nArcByte_     = 0;

  //  COUT("Found ntemplate = " << arraytmp->ntemplate);

  // Resize the vector to accomoate the register map descriptions.

  regmaps.resize(nregmap);
  
  // Initialize the register map descriptions at least up to a point
  // at which arraymap can be passed to del_ArrayMap().

  for(i=0; i < nregmap; i++) {
    RegTemp* regtemp = arraytmp->templates + i;
    //    COUT("About to construct new arrRegMap");
    regmaps[i] = new ArrRegMap(this, regtemp, i, old, addPerBrdRegs);
    //    COUT("About to construct new arrRegMap... done");
  }
}

/*.......................................................................
 * Delete an array map created by new_ArrayMap().
 *
 * Input:
 *  arraymap ArrayMap *  The array map to be deleted.
 * Output:
 *  return ArrayMap *  The deleted array map (ie. NULL).
 */
ArrayMap *del_ArrayMap(ArrayMap *arraymap)
{
  int i;

  // If this object has a non-zero ref count don't delete it

  if(arraymap && --arraymap->ref_count != 0) 
    return arraymap;
  else if(arraymap && --arraymap->ref_count == 0) {
    for(i=0; i < arraymap->regmaps.size(); i++) {
      RegMap* regmap = arraymap->regmaps[i]->regmap;
      
      // If any of the register map ref counts isn't zero, don't
      // delete this object
      
      if(--regmap->ref_count_ != 0) {
	arraymap->ref_count++;

	return arraymap;
      }
    }
  }

  // Else delete the object

  delete arraymap;
  return NULL;
}

/**.......................................................................
 * Destructor for ArrayMap
 */
ArrayMap::~ArrayMap()
{
  // First free the regmap array

  for(unsigned i=0; i < nregmap; i++) {
    if(regmaps[i] != 0)
      delete regmaps[i];
  }
}

/*.......................................................................
 * Obtain an alias copy of a given array map. To discard an alias call
 * del_ArrayMap(arraymap). This will only delete the array map once the
 * last alias has been deleted.
 *
 * Input:
 *  arraymap   ArrayMap *  The register map that you want a copy of.
 * Output:
 *  return   ArrayMap *  The same as arraymap.
 */
ArrayMap *alias_ArrayMap(ArrayMap *arraymap)
{
  if(arraymap)
    arraymap->ref_count++;  // Increment the reference count of the object 

  return arraymap;
}

/*.......................................................................
 * Lookup a named register map from a given array map
 *
 * Input:
 *  arraymap       ArrayMap *  The array map to lookup the register map from.
 *  remgap_name    char     *  The name of the register map
 * Output:
 *  return  RegMap *  The named register map (see regmap.h), or NULL if not
 */
ArrRegMap* find_ArrRegMap(ArrayMap *arraymap, string regmap_name)
{
  if(!arraymap) {
    lprintf(stderr, "find_RegMap: NULL arraymap\n");
    return NULL;
  };
  
  // Lookup the named register map.

  if(arraymap->regmapMap_.find(regmap_name) == arraymap->regmapMap_.end())
    return NULL;

  ArrRegMap* regmap = arraymap->regmapMap_[regmap_name];

  return regmap;
}

/**.......................................................................
 * Lookup a named register map from a given array map
 *
 * Input:
 *  arraymap       ArrayMap *  The array map to lookup the register map from.
 *  remgap_name    char     *  The name of the register map
 * Output:
 *  return  RegMap *  The named register map (see regmap.h), or NULL if not
 */
std::vector<ArrRegMap*> ArrayMap::matchArrRegMap(string regExpStr)
{
  std::vector<ArrRegMap*> matchedRegmaps;
  sza::util::RegExpParser parser(regExpStr);

  // Search through the list of register maps.  Ignoring errors in the
  // input regexp string

  try {
    for(unsigned iRegMap=0; iRegMap < regmaps.size(); iRegMap++) {
      std::string str(regmaps[iRegMap]->name);
      if(parser.matches(str))
	matchedRegmaps.push_back(regmaps[iRegMap]);
    }
  } catch(...) {};

  return matchedRegmaps;
}

/**.......................................................................
 * Lookup a named register map from a given array map
 */
std::vector<ArrRegMap*> ArrayMap::matchArrRegMap(std::vector<std::string>& regmapVec)
{
  std::vector<ArrRegMap*> matchedRegmaps;

  // Search through the list of register maps.  Ignoring errors in the
  // input regexp string

  try {
    for(unsigned iRegmapString=0; iRegmapString < regmapVec.size(); iRegmapString++) {
      sza::util::RegExpParser parser(regmapVec[iRegmapString]);

      for(unsigned iRegMap=0; iRegMap < regmaps.size(); iRegMap++) {
	std::string str(regmaps[iRegMap]->name);
	if(parser.matches(str))
	  matchedRegmaps.push_back(regmaps[iRegMap]);
      }
    }
  } catch(...) {};

  return matchedRegmaps;
}

/*.......................................................................
 * Lookup a register by name.
 *
 * Input:
 *  arraymap     ArrayMap *  The array map in which to find the register.
 *  regmap_name  char     *  The name of the register map.
 *  board_name   char     *  The name of the board.
 *  block_name   char     *  The name of the register block.
 * Output:
 *  return RegMapBlock *  The named register block (see regmap.h), or
 *                        NULL if not found.
 */
RegMapBlock* find_ArrayMapBlock(ArrayMap *arraymap, string regmap_name, 
				string board_name, string block_name)
{
  RegMapBoard* board=NULL;   // The named board 
  ArrRegMap* regmap=NULL;       // The named register map

  if(!arraymap) {
    lprintf(stderr, "find_ArrayMapBlock: NULL arraymap.\n");
    return NULL;
  };

  regmap = find_ArrRegMap(arraymap, regmap_name);

  if(!regmap)
    return NULL;

  board = find_RegMapBoard(regmap->regmap, board_name);

  if(!board)
    return NULL;

  RegMapBlock* blk = find_RegMapBoard_Block(board, block_name);

  return blk;
}

/*.......................................................................
 * Return true if two array maps contain equivalent information.
 *
 * Input:
 *  arraymap1   ArrayMap *  The first of the register maps to compare.
 *  arraymap2   ArrayMap *  The second of the register maps to compare.
 * Output:
 *  return       int    1 - The two register maps are equivalent.
 *                      0 - The two register maps are either not equivalent
 *                          or one or both of them is NULL.
 */
int equiv_ArrayMap(ArrayMap *arraymap1, ArrayMap *arraymap2)
{
  int regmap;
  
  // Are we missing either of the register maps?

  if(!arraymap1 || !arraymap2)
    return 0;
  
  // Compare the general characteristics of the two array maps.

  if(arraymap1->nregmap  != arraymap2->nregmap ||
     arraymap1->nreg     != arraymap2->nreg ||
     arraymap1->narchive != arraymap2->narchive)
    return 0;
  
  // Compare each register map of the array maps.

  for(regmap=0; regmap<arraymap1->nregmap; regmap++) {
    ArrRegMap* regmap1 = arraymap1->regmaps[regmap];
    ArrRegMap* regmap2 = arraymap2->regmaps[regmap];

    if(equiv_RegMap(regmap1->regmap, regmap2->regmap) != 1)
      return 0;
  };
  
  // The array maps are equivalent.

  return 1;
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
int find_ArrRegMapReg(ArrayMap *arraymap, 
		     string regmap_name, string board_name, string block_name,
		     RegAspect facet, unsigned index, unsigned n,
		     ArrRegMapReg *arreg)
{
  ArrRegMap* arregmap=NULL;
  RegMap* regmap=NULL;
  RegMapBlock *blk=NULL; // The register map block that contains the
	                 // register
  unsigned size;         // The number of slots per register element 
  
  // Check the arguments.

  if(!arraymap || !arreg) {
    lprintf(stderr, "find_ArrRegMapReg: NULL argument(s).\n");
    return 1;
  };

  // Attempt to locate the specified register map by name

  arregmap = find_ArrRegMap(arraymap, regmap_name);

  if(!arregmap || !arregmap->regmap) {
    lprintf(stderr, "Unable to find register map %s\n", regmap_name.c_str());
    return 1;
  };

  regmap = arregmap->regmap;
  
  // Attempt to locate the specified register by name.

  blk = find_RegMapBlock(regmap, board_name, block_name);
  if(!blk) {
    lprintf(stderr, "Unable to find register %s.%s\n", board_name.c_str(), 
	    block_name.c_str());
    return 1;
  };
  
  // Get the number of slots per register element.

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
    lprintf(stderr, "find_ArrRegMapReg: Invalid aspect.\n");
    return 1;
  };
  
  // Check the requested dimensions.

  if(size * (index + n) > blk->nreg_) {
    lprintf(stderr, "Dimensions out of range in %s.%s[%u-%u]\n", 
	    board_name.c_str(),
	    block_name.c_str(), index, index + n - 1);
    return 1;
  };
	      
  // Fill in the return container.

  if(init_ArrRegMapReg(arraymap, arregmap->number, blk->brd_->number, 
		       blk->number_, index, n ? n : blk->nreg_/size - index, 
		       facet, arreg))
    return 1;
  return 0;
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
int init_ArrRegMapReg(ArrayMap* arraymap, unsigned iregmap, unsigned board, 
		      unsigned block, unsigned index, unsigned nreg, 
		      RegAspect aspect, ArrRegMapReg *arreg)
{
  RegMapBoard *brd;  /* The board descriptor */
  RegMapBlock *blk;  /* The block descriptor */
  unsigned size;     /* The number of slots per register */
  
  // Get the regmap container.

  if(iregmap >= (unsigned)arraymap->nregmap) {
    lprintf(stderr, 
	    "init_ArrRegMapReg: Register mapindex %d out of range 0-%d.\n",
	    iregmap, arraymap->nregmap-1);
    return 1;
  };

  ArrRegMap* arregmap = arraymap->regmaps[iregmap];
  RegMap* regmap      = arregmap->regmap;
  RegMapReg* reg      = &arreg->reg;

  // Check arguments.

  if(!regmap || !reg) {
    lprintf(stderr, "init_ArrRegMapReg: NULL argument(s).\n");
    return 1;
  };
  
  // Get the board container.

  if(board >= (unsigned)regmap->nboard_) {
    lprintf(stderr, "init_ArrRegMapReg: Board index %d out of range 0-%d.\n",
	    board, regmap->nboard_-1);
    return 1;
  };
  brd = regmap->boards_[board];
  
  // Get the block container.

  if(block >= (unsigned)brd->nblock) {
    lprintf(stderr, "init_ArrRegMapReg: Block index %d out of range 0-%d.\n",
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
      lprintf(stderr, "init_ArrRegMapReg: Bad aspect for non-complex register.\n");
      return 1;
    };
    size = 2;
    break;
  case REG_DATE:
  case REG_TIME:
    if(!(blk->flags_ & REG_UTC)) {
      lprintf(stderr, "init_ArrRegMapReg: Bad aspect for non-UTC register.\n");
      return 1;
    };
    size = 2;
    break;
  default:
    lprintf(stderr, "init_ArrRegMapReg: Invalid aspect.\n");
    return 1;
  };
  
  // Check the index range.

  if(nreg < 1 || index + nreg*size - 1 >= blk->nreg_) {
    lprintf(stderr, "init_ArrRegMapReg: Invalid index range %d-%d.\n",
	    index, index + nreg - 1);
    return 1;
  };
  
  // Initialize the output register specification.

  reg->board = board;
  reg->block = block;
  reg->index = index;
  reg->slot  = blk->slot_ < 0 ? -1 : (blk->slot_ + reg->index * size);
  reg->slot += (!arregmap || arregmap->slot < 0) ? -1 : 
    arregmap->slot;
  reg->nreg = nreg;
  reg->size = size;
  reg->aspect = aspect;

  arreg->regmap = arregmap->number;

  // The byte slot location of the first requested element of this
  // register is:
  // 
  // the byte offset of this register map in the array map, plus
  //
  // the byte offset of this block in the register map, plus
  //
  // the byte offset of this element from the start of the register
  // array

  Coord coord(index);

  reg->iArcByte_ = arraymap->byteOffsetInArcArrayMapOf(arregmap, blk, &coord);
  reg->iByte_    = arraymap->byteOffsetInWholeArrayMapOf(arregmap, blk, &coord);
  reg->nBytePerEl_ = blk->nBytePerEl_;

  return 0;
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
 *
 *                            REG_INPUT_BLOCK:
 *                               regmap.board.register
 *                            REG_INPUT_ELEMENT:
 *                               regmap.board.register[index]
 *                            REG_INPUT_RANGE:
 *                               regmap.board.register[index-index]
 *
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
 *  reg     ArrRegMapReg *  On return *reg will contain the register
 *                          specification parsed from the stream.
 * Output:
 *  return  RegValidity     REG_VALID (0) - A known register was read.
 *                          REG_UNKNOWN   - A syntactically correct
 *                                          specification of an unknown
 *                                          register was read (*reg is
 *                                          unchanged).
 *                          REG_INVALID   - The read was aborted by a
 *                                          syntax or usage error.
 */
RegValidity input_ArrRegMapReg(InputStream *stream, int tell, 
			       ArrayMap *arraymap,  RegInputMode mode, 
			       int extend, ArrRegMapReg *arreg)
{
  char regmap_name[REG_NAME_LEN+1]; // The name of the register-map 
  char brd_name[REG_NAME_LEN+1];    // The name of the register-map board 
  char blk_name[REG_NAME_LEN+1];    // The name of the register boad 
  ArrRegMap* arregmap = NULL;
  RegMap* regmap = NULL;
  RegMapBoard* board=NULL;          // A register board 
  RegMapBlock* block=NULL;          // A named register block 
  long index1;                      // The lowest block index 
  long index2;                      // The highest block index (-1 for
				    // max)
  RegAspect aspect;     // The quantity to derive from complex
			// register pairs
  unsigned nmax=0;      // The allowable range of indexes
  unsigned size;        // The number of slots per register element 
  int i;
  
  // Check arguments.

  if(!stream) {
    lprintf(stderr, "input_ArrRegMapReg: NULL argument.\n");
    return REG_INVALID;
  };
  
  // Validate the parsing mode.

  switch(mode) {
  case REG_INPUT_BLOCK:
  case REG_INPUT_ELEMENT:
  case REG_INPUT_RANGE:
    break;
  default:
    lprintf(stderr, "input_ArrRegMapReg: Unknown input mode.\n");
    return REG_INVALID;
  };
  
  // The next keyword is a register map name. Get the associated
  // register map definition.

  if(input_keyword(stream, 0, 1)) {
    input_error(stream, tell, "Missing register-map name.\n");
    return REG_INVALID;
  };
  
  // Copy the regmap name into regmap_name[].

  if(strlen(stream->work) > REG_NAME_LEN) {
    input_error(stream, tell, "Register map name too long.\n");
    return REG_INVALID;
  };
  strcpy(regmap_name, stream->work);

  // The next character should be a period.

  if(stream->nextc != '.') {
    input_error(stream, tell,
		"Missing period after register-map name.\n");
    return REG_INVALID;
  };

  // The next keyword is a board name. Skip the period and get the
  // associated board definition.

  if(read_InputStream(stream, 1))
    return REG_INVALID;
  if(input_keyword(stream, 0, 1)) {
    input_error(stream, tell, "Missing register-map board name.\n");
    return REG_INVALID;
  };
  
  // Copy the board name into brd_name[].

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
		  "\"%s\" is not a recognized register attribute (2).\n",
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
  
  // First see if the named register exists in the current array map.

  if(arraymap) {
    arregmap = find_ArrRegMap(arraymap, regmap_name);
    regmap   = arregmap ? arregmap->regmap : NULL;
    board    = regmap ? find_RegMapBoard(regmap, brd_name) : NULL;
    block    = board ? find_RegMapBoard_Block(board, blk_name) : NULL;
    if(!regmap || !board || !block) {
      input_error(stream, tell, "Unknown register %s.%s.%s.\n", regmap_name, 
		  brd_name, blk_name);
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
    size = 2;   /* Real, Imaginary */
    if(!extend || (regmap && (block->flags_ & REG_COMPLEX) == 0)) {
      (void) input_error(stream, tell,
		"Unexpected complex attribute specified for register %s.%s.%s.\n",
			 regmap_name, brd_name, blk_name);
      return REG_INVALID;
    };
    break;
  case REG_DATE:
  case REG_TIME:
    size = 2;  /* Modified Julian day number, time-of-day (ms) */
    if(!extend || (regmap && (block->flags_ & REG_UTC) == 0)) {
      (void) input_error(stream, tell,
		"Unexpected UTC attribute specified for register %s.%s.%s.\n",
			 regmap_name, brd_name, blk_name);
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
		"Register %s.%s.%s%s%s[0-%d] doesn't have an element %ld.\n",
		regmap_name, brd_name, blk_name, is_plain ? "":".",
		is_plain ? "" : name_RegAspect(aspect),
		nmax-1, index2);
    
    // If none of the specified elements are valid then report the
    // register as unknown. Otherwise report it as invalid.

    return index1 >= (long)nmax ? REG_UNKNOWN : REG_INVALID;
  };
  
  // Record the results for return.

  if(arreg) {
    RegMapReg* reg = &arreg->reg;

    arreg->regmap = arregmap ? arregmap->number : -1;
    arreg->reg.board = regmap ? board->number : -1;
    arreg->reg.block = regmap ? block->number_ : -1;
    arreg->reg.index = index1;

    arreg->reg.slot = (!regmap || block->slot_ < 0) ? -1 :
      (block->slot_ + index1 * size);

    // Add in the offset of the register map from the start of the
    // archive array

    arreg->reg.slot += (!arregmap || arregmap->slot < 0) ? -1 : 
      arregmap->slot;

    arreg->reg.nreg = index2 - index1 + 1;
    arreg->reg.size = size;
    arreg->reg.aspect = aspect;
    
    // See if the specification matches the mode.

    switch(mode) {
    case REG_INPUT_BLOCK:
      if(regmap && arreg->reg.nreg*size != block->nreg_) {
	input_error(stream, tell,
		    "In the current context %s.%s.%s can't take an index expression.\n",
		    regmap_name, brd_name, blk_name);
	return REG_INVALID;
      };
      break;
    case REG_INPUT_ELEMENT:
      if(regmap && arreg->reg.nreg != 1) {
	input_error(stream, tell,
		    "You must choose an element from %s.%s.%s[%d].\n",
		    regmap_name, brd_name, blk_name, arreg->reg.nreg);
	return REG_INVALID;
      };
      break;
    case REG_INPUT_RANGE:
      break;
    };
  };
  return arraymap ? REG_VALID : REG_UNKNOWN;
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
int output_ArrRegMapReg(OutputStream *stream, ArrayMap* arraymap, 
			RegOutputMode mode, 
			ArrRegMapReg *arreg)
{
  RegMapBoard* brd; // The board specified in *arreg 
  RegMapBlock* blk; // The block specified in *arreg 
  ArrRegMap* arregmap=NULL;
  RegMap* regmap=NULL;
  RegMapReg* reg=NULL;

  // Check the arguments.

  if(!stream || !arreg || !arraymap) {
    lprintf(stderr, "output_ArrRegMapReg: NULL argument.\n");
    return 1;
  };
  
  // Get the board and block objects that correspond to the
  // specifications in *reg.

  if(arreg->regmap >= (unsigned)arraymap->nregmap) {
    lprintf(stderr, "output_ArrRegMapReg: Board index out of range.\n");
    return 1;
  };

  arregmap = arraymap->regmaps[arreg->regmap];

  regmap = arregmap->regmap;
  reg    = &arreg->reg;

  if(reg->board >= regmap->nboard_) {
    lprintf(stderr, "output_ArrRegMapReg: Board index out of range.\n");
    return 1;
  };

  brd = regmap->boards_[reg->board];
  if(reg->block >= brd->nblock) {
    lprintf(stderr, "output_ArrRegMapReg: Block index out of range.\n");
    return 1;
  };
  blk = brd->blocks[reg->block];
  
  // Check that the index and nreg values make sense. Note that
  // reg->index is unsigned, so there is no need to check whether it
  // is < 0.

  if((reg->index + reg->nreg) * reg->size > blk->nreg_) {
    lprintf(stderr, "output_ArrRegMapReg: Slot numbers out of range.\n");
    return 1;
  };
  
  // Validate the output mode.

  switch(mode) {
  case REG_OUTPUT_BLOCK:
  case REG_OUTPUT_ELEMENT:
  case REG_OUTPUT_RANGE:
    break;
  default:
    lprintf(stderr, "output_ArrRegMapReg: Unknown output format.\n");
    return 1;
  };
  
  // Output the fully qualified name of the register.

  if(write_OutputStream(stream, arregmap->name) ||
     write_OutputStream(stream, ".") ||
     write_OutputStream(stream, brd->name) ||
     write_OutputStream(stream, ".") ||
     write_OutputStream(stream, blk->name_))
    return 1;
  
  // Write the aspect if needed.

  if(reg->aspect != REG_PLAIN &&
     (write_OutputStream(stream, ".") ||
      write_OutputStream(stream, name_RegAspect(reg->aspect))))
    return 1;
  
  // Write the appropriate index specification.

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
 *  reg    ArrRegMapReg *  The specification object to be reset.
 */
void clr_ArrRegMapReg(ArrRegMapReg *arreg) {

  if(arreg) {
    arreg->regmap = -1;
    clr_RegMapReg(&arreg->reg);
  }
}

/**.......................................................................
 * Return the offset, in bytes_ of the named board and block from
 * the start of the archived array
 */
int ArrayMap::byteOffsetInArcArrayMapOf(string regmap, string board, 
					string block, 
					Coord* coord)
 {
   ArrRegMap* aregmap = find_ArrRegMap(this, regmap);

   // Throw an error if the requested register doesn't exist in the
   // register map
   
   if(aregmap == 0) 
     ThrowError("Lookup of " << regmap << " failed");

   // Else return the offset

   int regOffset = aregmap->regmap->
     byteOffsetInArcRegMapOf(board, block, coord);

   // If the named register is not archived, the offset will be < 0

   if(regOffset < 0)
     return -1;

   return aregmap->byteOffsetInArcArrayMap() + regOffset;
 }

/**.......................................................................
 * Return the offset, in bytes_ of the named register, board and block from
 * the start of the archived array
 */
int ArrayMap::byteOffsetInArcArrayMapOf(ArrRegMap* aregmap, RegMapBlock* block, 
					Coord* coord)
{
  // Else return the offset
  
  int regOffset = aregmap->regmap->byteOffsetInArcRegMapOf(block, coord);
  
  // If the named register is not archived, the offset will be < 0
  
  if(regOffset < 0)
    return -1;

  return aregmap->byteOffsetInArcArrayMap() + regOffset;
}

/**.......................................................................
 * Return the offset, in bytes_ of the named board and block from
 * the start of the whole array map
 */
int ArrayMap::byteOffsetInWholeArrayMapOf(string regmap, string board, 
					  string block, 
					  Coord* coord)
{
   ArrRegMap* aregmap = find_ArrRegMap(this, regmap);

   // Throw an error if the requested register doesn't exist in the
   // register map

   if(aregmap == 0) 
     ThrowError("Lookup of " << regmap << " failed");

   // Else return the offset

   int regOffset = aregmap->regmap->
     byteOffsetInWholeRegMapOf(board, block, coord);

   // If the named register is not archived, the offset will be < 0

   if(regOffset < 0)
     return -1;

   return aregmap->byteOffsetInWholeArrayMap() + regOffset;
 }

/**.......................................................................
 * Return the offset, in bytes_ of the named board and block from
 * the start of the archived array
 */
int ArrayMap::byteOffsetInWholeArrayMapOf(ArrRegMap* aregmap, 
					  RegMapBlock* block, 
					  Coord* coord)
{
  // Else return the offset

   int regOffset = aregmap->regmap->byteOffsetInWholeRegMapOf(block, coord);

   // If the named register is not archived, the offset will be < 0

   if(regOffset < 0)
     return -1;

   return aregmap->byteOffsetInWholeArrayMap() + regOffset;
 }

/**.......................................................................
 * Return the offset, in slots_ of the named board and block from
 * the start of the archived array
 */
int ArrayMap::slotOffsetInArcArrayMapOf(ArrRegMap* aregmap, RegMapBlock* block, 
					Coord* coord)
 {
   if(aregmap == 0 || block == 0) 
     ThrowError("NULL arguments");

   // Else return the offset

   return aregmap->slotOffsetInArcArrayMapOf(block, coord);
 }

/**.......................................................................
 * Return the offset, in slots_ of the named board and block from
 * the start of the archived array
 */
int ArrayMap::
slotOffsetInWholeArrayMapOf(ArrRegMap* aregmap, RegMapBlock* block, 
			    Coord* coord)
 {
   if(aregmap == 0 || block == 0) 
     ThrowError("NULL arguments");

   // Else return the offset

   return aregmap->slotOffsetInWholeArrayMapOf(block, coord);
 }

/**.......................................................................
 * Return the offset, in slots_ of the named board and block from
 * the start of the archived array
 */
int ArrayMap::
slotOffsetInArcArrayMapOf(string regmapName, string boardName,
			  string blockName, Coord* coord)
 {
   ArrRegMap* aregmap=0;
   aregmap = findArrRegMap(regmapName);

   RegMapBlock* block = aregmap->regmap->findRegMapBlock(boardName, blockName);

   // And return the offset

   return aregmap->slotOffsetInArcArrayMapOf(block, coord);
 }

/**.......................................................................
 * Return the offset, in slots_ of the named board and block from
 * the start of the archived array
 */
int ArrayMap::
slotOffsetInWholeArrayMapOf(string regmapName, string boardName,
			    string blockName, Coord* coord)
 {
   ArrRegMap* aregmap=0;
   aregmap = findArrRegMap(regmapName);

   RegMapBlock* block = aregmap->regmap->findRegMapBlock(boardName, blockName);

   // And return the offset

   return aregmap->slotOffsetInWholeArrayMapOf(block, coord);
 }

/**.......................................................................
 * Find the named block.  If doThrow is true, throw an error if the
 * register wasn't found.
 */
RegMapBlock* ArrayMap::findArrayMapBlock(string regmap_name, 
					 string board_name, string block_name,
					 bool doThrow)
{
  RegMapBlock* blk = find_ArrayMapBlock(this, regmap_name, 
					board_name, block_name);

  if(doThrow && blk==0) 
    ThrowError("Lookup of " << regmap_name << "." << board_name 
	       << "." << block_name << " failed.");

  return blk;
}

/**.......................................................................
 * Find the named register map.  If doThrow is true, throw an error if
 * the register wasn't found.
 */
ArrRegMap* ArrayMap::findArrRegMap(RegMap* regmap)
{
  for(unsigned iregmap=0; iregmap < regmaps.size(); iregmap++)
    if(regmaps[iregmap]->regmap == regmap)
      return regmaps[iregmap];
  return 0;
}

/**.......................................................................
 * Find the named register map.  If doThrow is true, throw an error if
 * the register wasn't found.
 */
ArrRegMap* ArrayMap::findArrRegMap(string regmapName)
{
  for(unsigned iregmap=0; iregmap < regmaps.size(); iregmap++)
    if(strcmp(regmapName.c_str(), regmaps[iregmap]->name)==0)
      return regmaps[iregmap];
  return 0;
}

/**.......................................................................
 * Find the named block.  If doThrow is true, throw an error if the
 * register wasn't found.
 */
RegMap* ArrayMap::findRegMap(std::string regmap_name)
{
  ArrRegMap* arregmap = find_ArrRegMap(this, regmap_name);

  if(arregmap==0)
    return NULL;
  else
    return arregmap->regmap;
}

/**.......................................................................
 * Constructor with initialization
 */
ArrRegMap::ArrRegMap(ArrayMap* parent, void* vregtemp, unsigned iRegMap, bool old, bool addRegs)
{
  RegTemp* regtemp = (RegTemp*)vregtemp;
  using sza::util::LogStream;

  //  COUT("Constructing new ArrRegMap with name = " << regtemp->name << " 0");

  // Initialize the index of this register map in the array map
  
  number = iRegMap;
  
  // Initialize the index of this register map in the archive array
  
  slot      = parent->narchive;

  iSlot_    = parent->nreg;
  iArcSlot_ = parent->narchive;
  iByte_    = parent->nByte_;
  iArcByte_ = parent->nArcByte_;

  // Allocate a comment field for this regmap

  comment_  = 0;
  comment_ = new std::string(regtemp->comment_);

  // Initialize the RegMap object that this ArrRegMap container
  // manages
  
  regmap = 0;
  regmap = new RegMap(regtemp->regtemplate, old, addRegs);
  
  //  COUT("Constructing new ArrRegMap with name = " << regtemp->name << " 1");
  // Copy the name
  
  strcpy(name, regtemp->name);
  
  // Increment array map counters
  
  parent->nreg          += regmap->nreg_;
  parent->narchive      += regmap->narchive_;
  
  parent->nByte_        += regmap->nByte_;
  parent->nArcByte_     += regmap->nArcByte_;
  
  // All register maps must have unambiguous names.
  
  if(parent->regmapMap_.find(name) != parent->regmapMap_.end()) {
    ThrowError("Bad name for register map " << iRegMap);
  } else {
    parent->regmapMap_[name] = this;
  }
  //  COUT("Constructing new ArrRegMap with name = " << regtemp->name << " 2");
}

/**.......................................................................
 * Constructor for ArrRegMap()
 */
ArrRegMap::ArrRegMap() 
{
  regmap = 0;
  number = 0;
  
  // Initialize the index of this register map in the archive array
  
  slot      = -1;
  iByte_    =  0;
  iArcByte_ = -1;
  
  name[0] = '\0';
}

/**.......................................................................
 * Delete the register map if it was allocated
 */
ArrRegMap::~ArrRegMap() 
{
  if(regmap != 0) {
    delete regmap;
    regmap = 0;
  }

  if(comment_ != 0) {
    delete comment_;
    comment_ = 0;
  }
}

/**.......................................................................
 * Lookup a named register map from a given array map
 *
 * Input:
 *  arraymap       ArrayMap *  The array map to lookup the register map from.
 *  remgap_name    char     *  The name of the register map
 * Output:
 *  return  RegMap *  The named register map (see regmap.h), or NULL if not
 */
std::vector<RegMapBoard*> ArrRegMap::matchRegMapBoard(string regExpStr)
{
  std::vector<RegMapBoard*> matchedBoards;
  sza::util::RegExpParser parser(regExpStr);

  // Search through the list of boards.  Ignore errors in the
  // input regexp string.

  if(regmap != 0) {

    try {
      for(unsigned iBoard=0; iBoard < regmap->boards_.size(); iBoard++) {
	std::string str(regmap->boards_[iBoard]->name);
	if(parser.matches(str))
	  matchedBoards.push_back(regmap->boards_[iBoard]);
      }
    } catch(...) {};

  }

  return matchedBoards;
}

/**.......................................................................
 * Lookup a named register map from a given array map
 *
 * Input:
 *  arraymap       ArrayMap *  The array map to lookup the register map from.
 *  remgap_name    char     *  The name of the register map
 * Output:
 *  return  RegMap *  The named register map (see regmap.h), or NULL if not
 */
std::vector<RegMapBoard*> ArrRegMap::matchRegMapBoard(string regExpStr, CoordRange& range)
{
  std::vector<RegMapBoard*> matchedBoards;

  if(range.nAxis() == 0) {
    return matchRegMapBoard(regExpStr);
  } else {

    unsigned start = range.startIndex(0);
    unsigned stop  = range.stopIndex(0);

    for(unsigned iBoardInd=start; iBoardInd < stop; iBoardInd++) {
      std::ostringstream os;
      os << regExpStr << iBoardInd;
      sza::util::RegExpParser parser(os.str());

      // Search through the list of boards.  Ignore errors in the
      // input regexp string.

      if(regmap != 0) {

	try {
	  for(unsigned iBoard=0; iBoard < regmap->boards_.size(); iBoard++) {
	    std::string str(regmap->boards_[iBoard]->name);
	    if(parser.matches(str))
	      matchedBoards.push_back(regmap->boards_[iBoard]);
	  }
	} catch(...) {};
	
      }
    }
  }

  return matchedBoards;
}

/**.......................................................................
 * Return a vector of board descriptors matching the input board
 * strings
 */
std::vector<RegMapBoard*> ArrRegMap::matchRegMapBoard(std::vector<std::string>& boards)
{
  std::vector<RegMapBoard*> matchedBoards;

  // Search through the list of boards.  Ignore errors in the
  // input regexp string.
  
  if(regmap != 0) {
    
    try {
      for(unsigned iBoardString=0; iBoardString < boards.size(); iBoardString++) {
	sza::util::RegExpParser parser(boards[iBoardString]);

	for(unsigned iBoard=0; iBoard < regmap->boards_.size(); iBoard++) {
	  std::string str(regmap->boards_[iBoard]->name);
	  if(parser.matches(str))
	    matchedBoards.push_back(regmap->boards_[iBoard]);
	}
      }
    } catch(...) {};
  }

  return matchedBoards;
}

/**.......................................................................
 * Return the byte offset in the ArrayMap of the named block
 */
int ArrRegMap::byteOffsetInArcArrayMapOf(RegMapBlock* block, Coord* coord)
{
  return block->byteOffsetInArcRegMapOf(coord) + 
    byteOffsetInArcArrayMap();
}

/**.......................................................................
 * Return the byte offset in the ArrayMap of the named block
 */
int ArrRegMap::byteOffsetInWholeArrayMapOf(RegMapBlock* block, Coord* coord)
{
  return block->byteOffsetInWholeRegMapOf(coord) + 
    byteOffsetInWholeArrayMap();
}

/**.......................................................................
 * Return the slot offset in the ArrayMap of the named block
 */
int ArrRegMap::slotOffsetInArcArrayMapOf(RegMapBlock* block, Coord* coord)
{
  return block->slotOffsetInArcRegMapOf(coord) + 
    slotOffsetInArcArrayMap();
}

/**.......................................................................
 * Return the slot offset in the ArrayMap of the named block
 */
int ArrRegMap::slotOffsetInWholeArrayMapOf(RegMapBlock* block, Coord* coord)
{
  return block->slotOffsetInWholeRegMapOf(coord) + slotOffsetInWholeArrayMap();
}

