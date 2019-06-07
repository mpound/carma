#include "carma/szautil/RegParser.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/String.h"

#include <vector>
#include <cstring>

using namespace sza::util;
using namespace std;

/**.......................................................................
 * Constructor.
 */
RegParser::RegParser(bool archivedOnly) :
  archivedOnly_(archivedOnly) {}

/**.......................................................................
 * Destructor.
 */
RegParser::~RegParser() {}

/*.......................................................................
 * Parse a register specification from an input stream.
 *
 * Input:
 *  stream   InputStream *  The stream that contains the specification.
 *                          Trailing characters will be left unread.
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
std::vector<RegDescription> RegParser::inputRegs(InputStream* stream, 
						 bool tell,
						 ArrayMap* arrayMap,
						 RegInputMode mode, 
						 bool extend,
						 bool splitIndices,
						 bool doThrow)
{
  char regmap_name[REG_NAME_LEN+1]; // The name of the register-map 
  char board_name[REG_NAME_LEN+1];  // The name of the register-map board 
  char block_name[REG_NAME_LEN+1];  // The name of the register boad 
  ArrRegMap*   arregmap = NULL;     // A register map
  RegMapBoard* board    = NULL;     // A named register board 
  RegMapBlock* block    = NULL;     // A named register block 
  long index1;                      // The lowest block index 
  long index2;                      // The highest block index (-1 for max)
  unsigned nmax=0;                  // The allowable range of indexes
  int i;
  RegAspect aspect = REG_PLAIN;
  RegInteg integ   = REG_INT_PLAIN;
  vector<RegDescription> regs;
  CoordRange boardCoordRange;
  CoordRange coordRange;

  regs.resize(0);

  try {

    // Parse the register specification from stream

    parseFromStream(stream, regmap_name, board_name, block_name,
		    boardCoordRange, coordRange,
		    aspect, integ, mode);

    // If arrayMap == 0, then we are just parsing regs, so don't
    // attempt to match
    
    if(arrayMap==0) {
      validity_ = REG_UNKNOWN;
      return regs;
    }

    // Now iterate through the array map, looking for matches

    matchRegisters(arrayMap, regmap_name, board_name, block_name, 
		   boardCoordRange, coordRange,
		   aspect, integ,  mode,  extend, splitIndices, 
		   regs);
    
    // If nothing matched the input specifier, throw an error
    
    if(regs.size() == 0) {
      validity_ = REG_UNKNOWN;
      ThrowSimpleError("Unknown register: " 
		       << regmap_name << "." << board_name 
		       << "." << block_name << coordRange);

    };

    // Else return the vector of register descriptors
  
    validity_ = REG_VALID;  
    return regs;

  } catch(Exception& err) {

    if(doThrow) {
      throw err;
    } else {
      return regs;
    }

  }
}

std::vector<RegDescription> RegParser::inputRegsCarma(std::string regSpec,
						      bool tell,
						      ArrayMap* arrayMap,
						      RegInputMode mode, 
						      bool extend,
						      bool splitIndices,
						      bool doThrow)
{
  return inputRegsCarma(regSpec, arrayMap, mode, tell, extend, splitIndices, doThrow);
}

void RegParser::parseFromStream(InputStream* stream, char* regmap_name, char* board_name, char* block_name,
				CoordRange& boardCoordRange,
				CoordRange& coordRange,
				RegAspect& aspect,
				RegInteg& integ,
				RegInputMode mode) 
{
  // Check arguments.
  
  if(!stream) {
    validity_ = REG_INVALID;
    ThrowError("NULL argument");
  };
  
  // Validate the parsing mode.
  
  switch(mode) {
  case REG_INPUT_BLOCK:
  case REG_INPUT_ELEMENT:
  case REG_INPUT_RANGE:
    break;
  default:
    validity_ = REG_INVALID;
    ThrowError("Unknown input mode");
    break;
  };
  
  // The next keyword is a register map name. Get the associated
  // register map definition.
  
  if(input_regexp_keyword(stream, 0, 0)) {
    validity_ = REG_INVALID;
    ThrowError("Missing register-map name");
  };
  
  // Copy the regmap name into regmap_name[].
  
  if(strlen(stream->work) > REG_NAME_LEN) {
    validity_ = REG_INVALID;
    ThrowSimpleError("Register map name too long");
  };
  
  strcpy(regmap_name, stream->work);
  
  // The next character should be a period.
  
  if(stream->nextc != '.') {
    validity_ = REG_INVALID;
    ThrowSimpleError("Missing period after register-map name: work = " << stream->work);
  };
  
  // The next keyword is a board name. Skip the period and get the
  // associated board definition.
  
  if(read_InputStream(stream, 1)) {
    validity_ = REG_INVALID;
    ThrowError("Error skipping '.'");
  };
  
  if(input_board_regexp_keyword(stream, 0, 0)) {
    validity_ = REG_INVALID;
    ThrowSimpleError("Missing register-map board name");
  };
  
  // Copy the board name into board_name[].
  
  if(strlen(stream->work) > REG_NAME_LEN) {
    validity_ = REG_INVALID;
    ThrowSimpleError("Register board name too long");
  };

  strcpy(board_name, stream->work);

  boardCoordRange = readIndexRanges(stream);

  // The next character should be a period.
  
  if(stream->nextc != '.') {
    validity_ = REG_INVALID;
    ThrowSimpleError("Missing period after board name: " << board_name);
  };
  
  // Skip the period and read the register name that follows it.
  
  if(read_InputStream(stream, 1)) {
    validity_ = REG_INVALID;
    ThrowError("Error skipping '.'");
  }
  
  if(input_keyword(stream, 0, 0)) {
    validity_ = REG_INVALID;
    ThrowSimpleError("Missing register name after board name: " << board_name);
  };
  
  // Copy the register name into blk_name[].
  
  if(strlen(stream->work) > REG_NAME_LEN) {
    validity_ = REG_INVALID;
    ThrowSimpleError("Name of register block too long");
  };
  
  strcpy(block_name, stream->work);
  
  try {
    
    if(wordFollows(stream)) {

      // See if the register name is followed by a valid aspect
    
      try {

	aspect=checkAspect(stream);

	// If ths first bit was an aspect, see if the next bit is an
	// integration specifier

	if(wordFollows(stream)) {
	  integ = checkInteg(stream);
	}

	// If not, see if it was an integration status specifier
	// instead

      } catch(...) {
	aspect = REG_PLAIN;
	integ = checkInteg(stream);
      }

    }

    // See if the register name is followed by an index expression.
    
    coordRange = readIndexRanges(stream);
    
  } catch(const Exception& err) {
    validity_ = REG_INVALID;
    throw err;
  }

}

void RegParser::parseFromString(std::string  regSpec, 
				std::vector<std::string>& regmapNames,
				std::vector<std::string>& boardNames,
				std::vector<std::string>& blockNames,
				CoordRange&  coordRange,
				RegAspect&   aspect,
				RegInteg&    integ,
				RegInputMode mode,
				bool throwIfNoMatch) 
{
  // Check arguments.
  
  if(regSpec.size() == 0) {
    validity_ = REG_INVALID;
    ThrowError("Invalid register specification");
  };
  
  // Validate the parsing mode.
  
  switch(mode) {
  case REG_INPUT_BLOCK:
  case REG_INPUT_ELEMENT:
  case REG_INPUT_RANGE:
    break;
  default:
    validity_ = REG_INVALID;
    ThrowError("Unknown input mode");
    break;
  };


  std::vector<RegSpecItem> regSpecItems = parseIntoPeriodSeparatedItems(regSpec);

  if(regSpecItems.size() < 2) {
    ThrowError("Invalid register specification (too short): " << regSpec);
  }

  // Check the last two items for aspect and integ specifiers

  RegAspect tmpAspect  = REG_ASPECT_UNKNOWN;
  RegInteg  tmpInteg   = REG_INT_UNKNOWN;

  unsigned  iAspect    = 0; // The index of the item specifying an aspect
  unsigned  iInteg     = 0; // The index of the item specifying an integration type
  unsigned  iFirstAttr = regSpecItems.size(); // The index corresponding to the first attribute

  // See if either of the last two items specified an aspect or
  // integration type

  for(unsigned i=regSpecItems.size()-2; i < regSpecItems.size(); i++) {

    if(tmpAspect == REG_ASPECT_UNKNOWN) {
      try {
	tmpAspect = checkAspect(regSpecItems[i].regStr_, throwIfNoMatch);

	if(tmpAspect != REG_ASPECT_UNKNOWN) {
	  aspect = tmpAspect;
	  iAspect   = i;
	  if(iAspect < iFirstAttr) {
	    iFirstAttr = iAspect;
	  }
	}
      } catch(...) {
      }
    }

    if(tmpInteg == REG_INT_UNKNOWN) {
      try {
	tmpInteg = checkInteg(regSpecItems[i].regStr_, throwIfNoMatch);
	
	if(tmpInteg != REG_INT_UNKNOWN) {
	  integ = tmpInteg;
	  iInteg   = i;
	  if(iInteg < iFirstAttr) {
	    iFirstAttr = iInteg;
	  }
	}
      } catch(...) {
      }
    }
  }

  // Now remove any items corresponding to aspect/integration
  // specifiers

  regSpecItems.erase(regSpecItems.begin()+iFirstAttr, regSpecItems.end());

  // Now check if any index range was specified

  if(regSpecItems[regSpecItems.size()-1].hasIndexCoordRange_) {
    coordRange = regSpecItems[regSpecItems.size()-1].indexCoordRange_;
  }

  // Now get the vector of regmap names, board names, and block names

  regmapNames = getRegmapNames(regSpecItems);
  boardNames  = getBoardNames(regSpecItems);
  blockNames  = getBlockNames(regSpecItems);
}

/**.......................................................................
 * Return a vector of register-map names constructed from a vector of
 * items
 */
std::vector<std::string> RegParser::getRegmapNames(std::vector<RegSpecItem>& items)
{
  // Regmap names are always from the first item.

  return getExpandedNames(items[0]);
}

/**.......................................................................
 * Return a vector of block names constructed from a vector of
 * items
 */
std::vector<std::string> RegParser::getBlockNames(std::vector<RegSpecItem>& items)
{
  // Regmap names are always from the last item.

  return getExpandedNames(items[items.size()-1]);
}

/**.......................................................................
 * Return a vector of board names constructed from a vector of
 * items
 */
std::vector<std::string> RegParser::getBoardNames(std::vector<RegSpecItem>& items)
{
  std::vector<std::string> names;

  // Boards are constructed from the concatenation of all items
  // between the first and last

  if(items.size() < 3) {
    names.push_back("Subsystem");
    return names;
  }
  
  names = getExpandedNames(items[1]);

  for(unsigned iItem=2; iItem < items.size()-1; iItem++) {
    names = concatenateItem(names, items[iItem]);
  }

  return names;
}

/**.......................................................................
 * Concatenate expanded strings from two items
 */
std::vector<std::string> RegParser::concatenateItem(std::vector<std::string>& inNames, RegSpecItem& item)
{
  std::vector<std::string> itemNames = getExpandedNames(item);
  std::vector<std::string> outNames;

  std::ostringstream os;
  for(unsigned iIn=0; iIn < inNames.size(); iIn++) {
    string& prefix = inNames[iIn];

    for(unsigned iItem=0; iItem < itemNames.size(); iItem++) {
      string& suffix = itemNames[iItem];
      os.str("");
      os << prefix << "." << suffix;
      outNames.push_back(os.str());
    }
  }
  return outNames;
}



/**.......................................................................
 * Return a vector of register-map names constructed from a vector of
 * items
 */
std::vector<std::string> RegParser::getExpandedNames(RegSpecItem& item)
{
  // Regmap names are always from the first item.

  std::vector<std::string> names;
  std::ostringstream os;

  if(item.hasCoordRange_) {

    unsigned start = item.coordRange_.startIndex(0);
    unsigned stop  = item.coordRange_.stopIndex(0);

    for(unsigned i=start; i <= stop; i++) {
      os.str("");
      os << item.regStr_ << i;
      names.push_back(os.str());
    }

  } else {
    names.push_back(item.regStr_);
  }

  return names;
}

/**.......................................................................
 * Having parsed a register specification, iterate through the array
 * map looking for matches
 */
void RegParser::matchRegisters(ArrayMap* arrayMap, 
			       char* regmap_name, char* board_name, char* block_name, 
			       CoordRange& boardCoordRange,
			       CoordRange& coordRange,
			       RegAspect aspect,
			       RegInteg integ,
			       RegInputMode mode,
			       bool extend,
			       bool splitIndices,
			       std::vector<RegDescription>& regs)
{
  // First see if the named register exists in the current array map.
  
  std::vector<ArrRegMap*> regmaps = arrayMap->matchArrRegMap(regmap_name);
  
  // Keep track of how many blocks the input specification matches
  
  for(unsigned iRegMap=0; iRegMap < regmaps.size(); iRegMap++) {
    ArrRegMap* regmap = regmaps[iRegMap];

    std::vector<RegMapBoard*> boards = regmap->matchRegMapBoard(board_name, boardCoordRange);

    for(unsigned iBoard=0; iBoard < boards.size(); iBoard++) {
      RegMapBoard* board = boards[iBoard];

      std::vector<RegMapBlock*> blocks = board->matchRegMapBlock(block_name);
      
      for(unsigned iBlock=0; iBlock < blocks.size(); iBlock++) {
	RegMapBlock* block = blocks[iBlock];

	// Quietly continue if this block doesn't match the input
	// specifier
	
	if(!regmap || !board || !block)
	  continue;
	
	// If we are parsing archived registers only, and this
	// register is not archived, skip it.
	
	if(archivedOnly_ && !block->isArchived())
	  continue;
	
	// Copy the range for this block
	
	CoordRange range(coordRange);
	
	// Fill in index ranges left unspecified, appropriate for this
	// block.
	
	block->axes_->fillRange(range);
	
	// Get the size in slots of each element of this register
	  
	unsigned size = getSize(block, aspect, extend); 

	// See if the specification matches the mode.
	  
	checkValidityOfMode(mode, regmap, board, block, range, size);

	// Everything checks out -- install the values in a new                                              
        // descriptor, or descriptors if splitIndices is true                                                

        if(splitIndices && range.nAxis() > 1) {

          if(range.nAxis() > 2) {
	    ThrowError("Unhandled number of axes: " << range.nAxis());
	  }

          // Store the start and stop coordinate of the requested axes                                       

          Coord startCoord = range.startCoord();
          Coord stopCoord  = range.stopCoord();

          // And the start and stop indices on the slowest-changing                                          
          // axis                                                                                            

          unsigned iStart = startCoord.getIndex(0);
          unsigned iStop  = stopCoord.getIndex(0);

          // Now construct a range for each requested index of the                                           
          // slowest-changing axis, and add a descriptor for it                                              

          for(unsigned i=iStart; i <= iStop; i++) {
            startCoord.setIndex(0, i);
            stopCoord.setIndex(0, i);
            range.setStartCoord(startCoord);
            range.setStopCoord(stopCoord);

            RegDescription desc(archivedOnly_, arrayMap);

            desc.setTo(regmap->name, board->name, block->name_, aspect, integ, range);

            // Insert the new description into the vector and return                                         

            regs.push_back(desc);
          }

          // Else just add a single descriptor for the whole range                                           

        } else {

          RegDescription desc(archivedOnly_, arrayMap);
          desc.setTo(regmap->name, board->name, block->name_, aspect, integ, range);

          // Insert the new description into the vector and return                                           

          regs.push_back(desc);
        }
      } // End iteration over blocks                                                                         
    } // End iteration over boards                                                                           
  } // End iteration over regmaps      
}

/**.......................................................................
 * Having parsed a register specification, iterate through the array
 * map looking for matches
 */
void RegParser::matchRegisters(ArrayMap* arrayMap, 
			       std::vector<std::string>& regmapNames,
			       std::vector<std::string>& boardNames,
			       std::vector<std::string>& blockNames,
			       CoordRange& coordRange,
			       RegAspect aspect,
			       RegInteg integ,
			       RegInputMode mode,
			       bool extend,
			       bool splitIndices,
			       std::vector<RegDescription>& regs)
{
  // Check that the array map is not NULL
  
  if(arrayMap==0) {
    validity_ = REG_UNKNOWN;
    return;
  }
  
  // First see if the named register exists in the current array map.
  
  std::vector<ArrRegMap*> regmaps = arrayMap->matchArrRegMap(regmapNames);
  
  // Keep track of how many blocks the input specification matches
  
  for(unsigned iRegMap=0; iRegMap < regmaps.size(); iRegMap++) {
    ArrRegMap* regmap = regmaps[iRegMap];

    std::vector<RegMapBoard*> boards = regmap->matchRegMapBoard(boardNames);

    for(unsigned iBoard=0; iBoard < boards.size(); iBoard++) {
      RegMapBoard* board = boards[iBoard];

      std::vector<RegMapBlock*> blocks = board->matchRegMapBlock(blockNames);
      
      for(unsigned iBlock=0; iBlock < blocks.size(); iBlock++) {
	RegMapBlock* block = blocks[iBlock];

	// Quietly continue if this block doesn't match the input
	// specifier
	
	if(!regmap || !board || !block)
	  continue;
	
	// If we are parsing archived registers only, and this
	// register is not archived, skip it.
	
	if(archivedOnly_ && !block->isArchived())
	  continue;
	
	// Copy the range for this block
	
	CoordRange range(coordRange);
	
	// Fill in index ranges left unspecified, appropriate for this
	// block.
	
	block->axes_->fillRange(range);

	// Get the size in slots of each element of this register
	  
	unsigned size = getSize(block, aspect, extend); 

	// See if the specification matches the mode.
	  
	checkValidityOfMode(mode, regmap, board, block, range, size);
	  
	// Everything checks out -- install the values in a new                                              
        // descriptor, or descriptors if splitIndices is true                                                
	
        if(splitIndices && range.nAxis() > 1) {
	  
          if(range.nAxis() > 2) {
	    ThrowError("Unhandled number of axes: " << range.nAxis());
	  }

          // Store the start and stop coordinate of the requested axes                                       

          Coord startCoord = range.startCoord();
          Coord stopCoord  = range.stopCoord();

          // And the start and stop indices on the slowest-changing                                          
          // axis                                                                                            

          unsigned iStart = startCoord.getIndex(0);
          unsigned iStop  = stopCoord.getIndex(0);

          // Now construct a range for each requested index of the                                           
          // slowest-changing axis, and add a descriptor for it                                              

          for(unsigned i=iStart; i <= iStop; i++) {
            startCoord.setIndex(0, i);
            stopCoord.setIndex(0, i);
            range.setStartCoord(startCoord);
            range.setStopCoord(stopCoord);

            RegDescription desc(archivedOnly_, arrayMap);

            desc.setTo(regmap->name, board->name, block->name_, aspect, integ, range);

            // Insert the new description into the vector and return                                         

            regs.push_back(desc);
          }

          // Else just add a single descriptor for the whole range                                           

        } else {

          RegDescription desc(archivedOnly_, arrayMap);

          desc.setTo(regmap->name, board->name, block->name_, aspect, integ, range);

          // Insert the new description into the vector and return                                           

          regs.push_back(desc);
        }
      } // End iteration over blocks                                                                         
    } // End iteration over boards                                                                           
  } // End iteration over regmaps      
  
}

/**.......................................................................
 * Convenience method that uses external arraymap and constructs the
 * stream for an input string.
 */
std::vector<RegDescription> RegParser::inputRegsCarma(std::string regSpec,
						      ArrayMap* arrayMap,
						      RegInputMode mode, 
						      bool tell,
						      bool extend,
						      bool splitIndices,
						      bool doThrow)
{
  std::vector<std::string> regmapNames;
  std::vector<std::string> boardNames;
  std::vector<std::string> blockNames;
  ArrRegMap*   arregmap = NULL;     // A register map
  RegMapBoard* board    = NULL;     // A named register board 
  RegMapBlock* block    = NULL;     // A named register block 
  long index1;                      // The lowest block index 
  long index2;                      // The highest block index (-1 for max)
  unsigned nmax=0;                  // The allowable range of indexes
  int i;
  RegAspect aspect = REG_PLAIN;
  RegInteg integ   = REG_INT_PLAIN;
  vector<RegDescription> regs;
  CoordRange coordRange;

  regs.resize(0);

  try {

    // Parse the register specification from stream

    parseFromString(regSpec, regmapNames, boardNames, blockNames,
		    coordRange,
		    aspect, integ, mode, false);

    // If arrayMap == 0, then we are just parsing regs, so don't
    // attempt to match
    
    if(arrayMap==0) {
      validity_ = REG_UNKNOWN;
      return regs;
    }

    // Now iterate through the array map, looking for matches

    matchRegisters(arrayMap, regmapNames, boardNames, blockNames,
		   coordRange,
		   aspect, integ,  mode,  extend, splitIndices,
		   regs);

    // If nothing matched the input specifier, throw an error
    
    if(regs.size() == 0) {
      validity_ = REG_UNKNOWN;
      ThrowSimpleError("Unknown register: " << regSpec);
    };

    // Else return the vector of register descriptors
  
    validity_ = REG_VALID;  
    return regs;

  } catch(Exception& err) {

    if(doThrow) {
      throw err;
    } else {
      return regs;
    }

  }

}

/**.......................................................................
 * Parse a register specification of the form: 
 *
 * WbPipeline.IntegratorStageContainer.IntegratorStage.frameCount
 *
 * into component strings
 */
std::vector<std::string> RegParser::parseIntoPeriodSeparatedStrings(std::string regSpec)
{
  std::vector<std::string> regSpecItems;

  String str(regSpec);
  String tmpStr;

  do {
    tmpStr = str.findNextInstanceOf("", false, ".", true, true);

    if(!tmpStr.isEmpty())
      regSpecItems.push_back(tmpStr.str());
    
  } while(str.remainderContains("."));
  
  if(!str.remainder().isEmpty()) {
    regSpecItems.push_back(str.remainder().str());
  }

  return regSpecItems;
}

/**.......................................................................
 * Parse a register specification of the form: 
 *
 * WbPipeline.IntegratorStageContainer.IntegratorStage.frameCount
 *
 * into component strings
 */
std::vector<RegParser::RegSpecItem> 
RegParser::parseIntoPeriodSeparatedItems(std::string regSpec)
{
  std::vector<RegSpecItem> regSpecItems;

  String str(regSpec);
  String tmpStr;

  do {
    tmpStr = str.findNextInstanceOf("", false, ".", true, true);

    if(!tmpStr.isEmpty()) {
      regSpecItems.push_back(parseItemCarma(tmpStr.str()));
    }
    
  } while(str.remainderContains("."));
  
  if(!str.remainder().isEmpty()) {
    regSpecItems.push_back(parseItemCarma(str.remainder().str()));
  }

  return regSpecItems;
}

RegParser::RegSpecItem RegParser::parseItem(std::string str)
{
  // An item can consist of a string, followed by an optional range of
  // the form: [n], or [n-m]

  String strStr(str);
  RegSpecItem item;
  
  if(strStr.contains("[")) {
    item.regStr_ = strStr.findNextInstanceOf("", false, "[", true, false).str();
    item.hasCoordRange_ = true;
    item.coordRange_ = parseCoordRange(strStr);
  } else {
    item.regStr_        = strStr.str();
    item.hasCoordRange_ = false;
  }

  return item;
}

RegParser::RegSpecItem RegParser::parseItemCarma(std::string str)
{
  // An item can consist of a string, followed by an optional range of
  // the form: [n], or [n-m]

  String strStr(str);
  RegSpecItem item;
  
  if(strStr.contains("(")) {
    item.regStr_ = strStr.findNextInstanceOf("", false, "(", true, false).str();
    item.hasCoordRange_ = true;
    item.coordRange_ = parseCoordRangeCarma(strStr);
  } else  if(strStr.contains("[")) {
    item.regStr_ = strStr.findNextInstanceOf("", false, "[", true, false).str();
    item.hasIndexCoordRange_ = true;
    item.indexCoordRange_ = parseIndexCoordRangeCarma(strStr);
  } else {
    item.regStr_        = strStr.str();
    item.hasCoordRange_ = false;
  }

  return item;
}

CoordRange RegParser::parseCoordRange(String& str)
{
  CoordRange cr;
  String tmpStr;

  if(str.contains("-")) {

    tmpStr = str.findNextInstanceOf("[", true, "-", true, false);
    cr.setStartIndex(0, tmpStr.toInt());

    tmpStr = str.findNextInstanceOf("-", true, "]", true, true);
    cr.setStopIndex(0, tmpStr.toInt());

  } else {
    tmpStr = str.findNextInstanceOf("[", true, "]", true, true);
    cr.setIndex(tmpStr.toInt());
  }

  if(!str.remainder().isEmpty()) {
    ThrowError("String " << str << " contains extra characters after range specification");
  }
  
  return cr;
}

CoordRange RegParser::parseCoordRangeCarma(String& str)
{
  CoordRange cr;
  String tmpStr;

  if(str.contains("-")) {

    tmpStr = str.findNextInstanceOf("(", true, "-", true, false);
    cr.setStartIndex(0, tmpStr.toInt());

    tmpStr = str.findNextInstanceOf("-", true, ")", true, true);
    cr.setStopIndex(0, tmpStr.toInt());

  } else {
    tmpStr = str.findNextInstanceOf("(", true, ")", true, true);
    cr.setIndex(tmpStr.toInt());
  }

  if(!str.remainder().isEmpty()) {
    ThrowError("String " << str << " contains extra characters after range specification");
  }
  
  return cr;
}

CoordRange RegParser::parseIndexCoordRangeCarma(String& str)
{
  CoordRange cr;
  String tmpStr, rangeStr;
  unsigned iAxis=0;
  bool cont = true;

  do {

    rangeStr = str.findNextInstanceOf("[", true, "]", true, true);

    if(rangeStr.contains("-")) {
      
      tmpStr = rangeStr.findNextInstanceOf("", false, "-", true, false);

      cr.setStartIndex(iAxis, tmpStr.toInt());

      tmpStr = rangeStr.findNextInstanceOf("-", true, " ", false, false);

      cr.setStopIndex(iAxis, tmpStr.toInt());
      
    } else {
      cr.setIndex(iAxis, rangeStr.toInt());
    }

    // If the remainder is not empty, see if the string contains
    // another index range
    
    if(!str.remainder().isEmpty()) {

      if(str.remainderContains("[")) {
	++iAxis;
	cont = true;
      } else {
	ThrowError("String " << str << " contains extra characters after range specification");
      }

    } else {
      cont = false;
    }

  } while(cont);
  
  return cr;
}

/**.......................................................................
 * Convenience method that uses internal arraymap and constructs the
 * stream for an input string.
 */
vector<RegDescription> RegParser::inputRegs(std::string regStr, 
					    RegInputMode mode, 
					    bool tell,
					    bool extend,
					    bool split)
{
  InputStream* stream=0;
  stream = new_InputStream();
  
  // Check arguments.
  
  if(!stream) {
    validity_ = REG_INVALID;
    ThrowError("NULL argument");
  }
  
  // Put the rest in a try-catch clause so that we don't leave the
  // stream allocated on exit.
  
  try {
    
    // Open a new string input stream
    
    if(open_StringInputStream(stream, true, (char*) regStr.c_str())) {
      stream = del_InputStream(stream);
      validity_ = REG_INVALID;
      ThrowError("Unable to allocate stream");
    }
    
    // Get the register specification now
    
    std::vector<RegDescription> regs;

    regs = inputRegs(stream, tell, arrayMap_.arrayMap(), mode, extend, split);
    
    if(stream != 0)
      stream = del_InputStream(stream);

    return regs;

  } catch(const Exception& err) {
    
    // Free the stream allocated in this method
    
    if(stream != 0)
      stream = del_InputStream(stream);
    
    throw err;
  }
}

/**.......................................................................
 * Convenience method that uses external arraymap and constructs the
 * stream for an input string.
 */
vector<RegDescription> RegParser::inputRegs(std::string regStr, 
					    ArrayMap* arrayMap,
					    RegInputMode mode, 
					    bool tell,
					    bool extend,
					    bool split,
					    bool doThrow)
{
  InputStream* stream=0;
  stream = new_InputStream();
  
  // Check arguments.
  
  if(!stream) {
    validity_ = REG_INVALID;
    ThrowError("NULL argument");
  };
  
  // Put the rest in a try-catch clause so that we don't leave the
  // stream allocated on exit.
  
  try {
    
    // Open a new string input stream
    
    if(open_StringInputStream(stream, true, (char*) regStr.c_str())) {
      stream = del_InputStream(stream);
      validity_ = REG_INVALID;
      ThrowError("Unable to allocate stream");
    }
    
    // Get the register specification now
    
    return inputRegs(stream, tell, arrayMap, mode, extend, split, doThrow);
    
  } catch(const Exception& err) {
    
    // Free the stream allocated in this method
    
    if(stream != 0)
      stream = del_InputStream(stream);
    
    throw err;
  }
  
  // Free the stream allocated in this method
  
  if(stream != 0)
    stream = del_InputStream(stream);
}

/**.......................................................................
 * Parse a single reg
 */
RegDescription RegParser::inputReg(InputStream* stream, 
				   bool tell,
				   RegInputMode mode, 
				   bool extend,
				   ArrayMap* arrayMap)
{
  // FInd all registers matching the inputs

  std::vector<RegDescription> regs = inputRegs(stream, tell, arrayMap, mode, 
					       extend);

  // This method should be called to match one and only one register.
  // If more than one register matched, throw an error.

  if(regs.size() > 1) {
    validity_ = REG_INVALID;
    ThrowSimpleError("More than one register matches");
  }

  // It is allowed to pass a NULL arraymap to inputReg, in which case
  // syntactic checking only will be performed on the registers.  In
  // this case, however, the returned array will be empty.  To guard
  // against this, check the size of the returned array, and return an
  // empty container if it is 0.  Else we return the first (and only)
  // element of the returned array.

  if(regs.size() == 0) {
    RegDescription reg;
    return reg;
  } else
    return regs[0];
}

/**.......................................................................
 * Convenience method that uses an internal arraymap and constructs the
 * stream for an input string.
 */
RegDescription RegParser::inputReg(std::string regStr, 
				   RegInputMode mode, 
				   bool tell,
				   bool extend)
{
  std::vector<RegDescription> regs;

  regs = inputRegs(regStr, mode, tell, extend);

  if(regs.size() > 1) {
    validity_ = REG_INVALID;
    ThrowError("More than one register matches");
  }

  if(regs.size() == 0) {
    RegDescription reg;
    return reg;
  } else
    return regs[0];
}

RegDescription RegParser::inputRegCarma(std::string regStr, 
					bool tell,
					RegInputMode mode, 
					bool extend,
					ArrayMap* arraymap)
{
  std::vector<RegDescription> regs;

  regs = inputRegsCarma(regStr, arraymap, mode, tell, extend);

  if(regs.size() > 1) {
    validity_ = REG_INVALID;
    ThrowError("More than one register matches");
  }

  if(regs.size() == 0) {
    RegDescription reg;
    return reg;
  } else
    return regs[0];
}

/**.......................................................................
 * See if the next part of a register specification is a word.  If it
 * is, read it.
 */
bool RegParser::wordFollows(InputStream* stream)
{
  if(stream->nextc == '.') {

    // Skip the period and read the attribute name that follows it.
    
    if(read_InputStream(stream, 1))
      ThrowSimpleError("Missing register attribute after block name");
    
    if(input_keyword(stream, 0, 1)) 
      ThrowSimpleError("Missing register attribute after block name");

    return true;
  } else
    return false; 
}

/**.......................................................................
 * Read a register aspect
 */
RegAspect RegParser::checkAspect(InputStream* stream,  bool throwIfNoMatch)
{
  return checkAspect(stream->work, throwIfNoMatch);
}

/**.......................................................................
 * Read a register aspect
 */
RegAspect RegParser::checkAspect(std::string str, bool throwIfNoMatch)
{
  return checkAspect((char*)str.c_str(), throwIfNoMatch);
}

/**.......................................................................
 * Read a register aspect
 */
RegAspect RegParser::checkAspect(char* cstr, bool throwIfNoMatch)
{
  RegAspect aspect;

  // Check against known aspects

  unsigned iAspect;
  for(iAspect=0; iAspect < (int)REG_NASPECT; iAspect++) {
    if(iAspect != (RegAspect)REG_ASPECT_UNKNOWN) {

      if(strcmp(cstr, name_RegAspect((RegAspect)iAspect)) == 0) {
	aspect = (RegAspect)iAspect;
	break;
      };

    }
  };
  
  if(iAspect >= (int)REG_NASPECT) {
    if(throwIfNoMatch) {
      ThrowSimpleError("'" << cstr << "'" 
		       << " is not a recognized register attribute (0)");
    } else {
      return REG_ASPECT_UNKNOWN;
    }
  }
  
  return aspect;
}

/**.......................................................................
 * Read a register int
 */
RegInteg RegParser::checkInteg(InputStream* stream, bool throwIfNoMatch)
{
  return checkInteg(stream->work, throwIfNoMatch);
}

/**.......................................................................
 * Read a register int
 */
RegInteg RegParser::checkInteg(std::string str, bool throwIfNoMatch)
{
  return checkInteg((char*)str.c_str(), throwIfNoMatch);
}

/**.......................................................................
 * Read a register integration type
 */
RegInteg RegParser::checkInteg(char* cstr, bool throwIfNoMatch)
{
  RegInteg integ;

  // Check against known ints

  unsigned iInteg;
  for(iInteg=0; iInteg < (int)REG_NINTEG; iInteg++) {
    if(iInteg != (RegInteg)REG_INT_UNKNOWN) {

      if(strcmp(cstr, name_RegInteg((RegInteg)iInteg)) == 0) {
	integ = (RegInteg)iInteg;
	break;
      }

    }
  }
  
  if(iInteg >= (int)REG_NINTEG) {

    if(throwIfNoMatch) {
      ThrowSimpleError("'" << cstr << "'" 
		       << " is not a recognized register integration attribute");
    } else {
      return REG_INT_UNKNOWN;
    }
  }
  
  return integ;
}

/**.......................................................................
 * Read all index ranges following a register name
 */
CoordRange RegParser::readIndexRanges(InputStream* stream)
{
  unsigned nAxis=0;
  CoordRange coordRange;
  
  while(stream->nextc == '[') {
    Range<unsigned> range = readIndexRange(stream);
    
    coordRange.setStartIndex(nAxis, range.start());
    coordRange.setStopIndex(nAxis, range.stop());
    
    nAxis++;
  }
  return coordRange;
}  

/**.......................................................................
 * Read an index range specification
 */
Range<unsigned> RegParser::readIndexRange(InputStream* stream)
{
  LogStream errStr;
  long index1, index2;
  
  // Consume the '['.
  
  if(read_InputStream(stream, 1)) {
    errStr.appendMessage(true, "Error reading initial '['\n");
    throw Error(errStr);
  }
  
  // If the next character is the terminator of the index expression
  // then the complete register block should remain
  // selected. Otherwise attempt to read the start index.
  
  if(stream->nextc != ']') {
    
    // If no number precedes the range separator then the start
    // index should remain at the default of 0.
    
    if(stream->nextc != '-') {
      
      // Read the start index.
      
      if(input_long(stream, 0, 0, &index1)) {
	errStr.appendMessage(true, "Invalid register index after [.\n");
	throw Error(errStr);
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
      if(read_InputStream(stream, 1)) {
	errStr.appendMessage(true, "Error reading '-'\n");
	throw Error(errStr);
      }
      
      // If the next character is the expression terminator, then
      // leave the end index at its end-of-block default. Otherwise
      // read the end index.
      
      if(stream->nextc != ']') {
	
	// Read the end index.
	
	if(!isdigit(stream->nextc) || input_long(stream, 0, 0, &index2)) {
	  errStr.appendMessage(true, "Invalid register index after '-'.\n");
	  throw Error(errStr);
	};
      };
    };
  };
  
  // Make sure that the expression is terminated and consume the
  // terminator.
  
  if(stream->nextc != ']') {
    errStr.appendMessage(true, "Unterminated register index expression.\n");
    throw Error(errStr);
  };
  
  // Skip the ']'.
  
  if(read_InputStream(stream, 1)) {
    errStr.appendMessage(true, "Error reading terminal ']'\n");
    throw Error(errStr);
  }
  
  Range<unsigned> range;
  
  range.setStart((unsigned)index1);
  range.setStop((unsigned)index2);
  
  return range;
};

/**.......................................................................
 * Return the size in slots of each register element
 */
unsigned RegParser::getSize(RegMapBlock* block, RegAspect aspect, bool extend)
{
  // If a derived aspect was specified, check that this is allowed,
  // and determine how many elements are needed to make up the derived
  // attribute.
  
  unsigned size;
  
  switch(aspect) {
  case REG_PLAIN:
    size = 1;
    break;
  case REG_REAL:
  case REG_IMAG:
  case REG_AMP:
  case REG_PHASE:
    size = 2;   // Real, Imaginary 
    if(!extend || ((block->flags_ & REG_COMPLEX) == 0)) {
      ThrowSimpleError("Unexpected complex attribute specified");
    };
    break;
  case REG_DATE:
  case REG_TIME:
    size = 2;  // Modified Julian day number, time-of-day (ms) 
    if(!extend || ((block->flags_ & REG_UTC) == 0)) {
      ThrowSimpleError("Unexpected UTC attribute specified: extend = " << extend << " " << (block->flags_ & REG_UTC));
    };
    break;
  default:
    size = 0;
    ThrowError("Missing aspect " << aspect << "in switch")
      break;
  };
  return size;
}

/**.......................................................................
 * Check the mode
 */
void RegParser::
checkValidityOfMode(RegInputMode mode, ArrRegMap* aregmap, 
		    RegMapBoard* brd, RegMapBlock* blk, 
		    CoordRange& range, unsigned size)
{
  // Store the total number of register elements corresponding to this
  // range
  
  unsigned nEl;
  try {
    nEl = blk->axes_->nEl(range);
  } catch(...) {
    ThrowSimpleError("Invalid range: " << range 
		     << " for "
		     << aregmap->name << "." 
		     << brd->name  << "." 
		     << blk->name_ << *blk->axes_);
  }
  
  switch(mode) {
  case REG_INPUT_BLOCK:
    if(nEl * size != blk->nreg_) {
      ThrowSimpleError("In the current context "
		       << aregmap->name << "." 
		       << brd->name  << "." 
		       << blk->name_
		       << " can't take an index expression");
    };
    break;
  case REG_INPUT_ELEMENT:
    if(nEl != 1) {
      ThrowSimpleError("You must choose an element from " 
		       << aregmap->name << "." 
		       << brd->name  << "." 
		       << blk->name_ << *blk->axes_);
    };
    break;
  case REG_INPUT_RANGE:
    break;
  };
}
