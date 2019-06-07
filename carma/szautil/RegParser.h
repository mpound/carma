#ifndef SZA_UTIL_REGPARSER_H
#define SZA_UTIL_REGPARSER_H

/**
 * @file RegParser.h
 * 
 * Tagged: Sat Oct  2 20:30:19 UTC 2004
 * 
 * @author 
 */
#include <vector>

#include "carma/szautil/CoordRange.h"
#include "carma/szautil/RegDescription.h"
#include "carma/szautil/String.h"

#include "carma/szaarrayutils/szaregs.h"

namespace sza {
  namespace util {
    
    class RegParser {
    public:
      
      struct RegSpecItem {
	std::string regStr_;
	bool hasCoordRange_;
	CoordRange coordRange_;
	bool hasIndexCoordRange_;
	CoordRange indexCoordRange_;

	RegSpecItem() {
	  hasCoordRange_      = false;
	  hasIndexCoordRange_ = false;
	};
      };

      /**
       * Constructor.
       *
       * If archivedOnly = true, byte offset calculations, etc. will
       * be done wrt the archived array map.  If false, they will be
       * done wrt the whole array map.
       */
      RegParser(bool archivedOnly_=false);
      
      /**
       * Destructor.
       */
      virtual ~RegParser();
      
      /**
       * Parse a register block description
       */
      std::vector<RegDescription> inputRegs(std::string regStr,
					    RegInputMode mode=REG_INPUT_RANGE, 
					    bool tell=true,
					    bool extend=true,
					    bool splitIndices=false);
      
      /**
       * Convenience method that uses external arraymap and constructs
       * the stream for an input string.
       */
      std::vector<RegDescription> inputRegs(std::string regStr, 
					    ArrayMap* arrayMap,
					    RegInputMode mode, 
					    bool tell,
					    bool extend,
					    bool splitIndices=false,
					    bool doThrow=true);

      std::vector<RegDescription> inputRegsCarma(std::string regSpec,
						 ArrayMap* arrayMap,
						 RegInputMode mode, 
						 bool tell,
						 bool extend,
						 bool splitIndices=false,
						 bool doThrow=true);

      /**
       * Parse a register block description.
       *
       * If splitIndices = true, and multiple indices are specified
       * for all but the fastest-changing index, this will return
       * separate descriptors for each index specified.  ie:
       *
       *   receiver.bolometers.ac[0-3][0-99]
       *
       * would return 4 descriptors instead of one.
       *
       */
      std::vector<RegDescription> inputRegs(InputStream* stream, 
					    bool tell,
					    ArrayMap* arraymap,
					    RegInputMode mode, 
					    bool extend,
					    bool splitIndices=false,
					    bool doThrow=true);
      
      std::vector<RegDescription> inputRegsCarma(std::string regSpec,
						 bool tell,
						 ArrayMap* arraymap,
						 RegInputMode mode, 
						 bool extend,
						 bool splitIndices=false,
						 bool doThrow=true);
      
      /**
       * Parse a single register block description
       */
      RegDescription inputReg(std::string regStr,
			      RegInputMode mode=REG_INPUT_RANGE, 
			      bool tell=true,
			      bool extend=true);
      
      RegDescription inputRegCarma(std::string regStr,
				   bool tell,
				   RegInputMode mode, 
				   bool extend,
				   ArrayMap* arraymap=0);
      
      /**
       * Parse a single register block description
       */
      RegDescription inputReg(InputStream* stream, 
			      bool tell,
			      RegInputMode mode, 
			      bool extend,
			      ArrayMap* arraymap=0);
      
      /**
       * Return the validity flag for the last register read
       */
      inline RegValidity validity() {
	return validity_;
      }
      
      /**
       * Return the size in slots of each register element
       */
      static unsigned getSize(RegMapBlock* block, RegAspect aspect, bool extend);
      
    private:
      
      // An array map
      
      ArrayMapBase arrayMap_;
      
      RegValidity validity_; // The validity flag of the last read
                             // register specification
      
      bool archivedOnly_;  // If true, offsets for the archived
                           // register map should be used.
      
      // True if the next token is a '.' followed by some string

      bool wordFollows(InputStream* stream);

      /**
       * Read a register aspect
       */
      RegAspect checkAspect(InputStream* stream, bool throwIfNoMatch=true);
      static RegAspect checkAspect(std::string str, bool throwIfNoMatch=true);
      static RegAspect checkAspect(char* cstr, bool throwIfNoMatch=true);

      /**
       * Read a register integration specifier
       */
      RegInteg checkInteg(InputStream* stream, bool throwIfNoMatch=true);
      static RegInteg checkInteg(std::string str, bool throwIfNoMatch=true);
      static RegInteg checkInteg(char* cstr, bool throwIfNoMatch=true);

      /**
       * Read all index range specifications following a register name
       */
      CoordRange readIndexRanges(InputStream* stream);
      
      /**
       * Read an index range specification
       */
      Range<unsigned> readIndexRange(InputStream* stream);
      
      /**
       * Check that the register specification was compatible with the
       * input mode
       */
      void checkValidityOfMode(RegInputMode mode, ArrRegMap* aregmap,
			       RegMapBoard* brd, RegMapBlock* blk, 
			       CoordRange& range, unsigned size);
      
      void parseFromStream(InputStream* stream, char* regmap_name, char* board_name, char* block_name,
			   CoordRange& boardCoordRange,
			   CoordRange& coordRange,
			   RegAspect& aspect,
			   RegInteg& integ,
			   RegInputMode mode);

      void matchRegisters(ArrayMap* arrayMap, 
			  char* regmap_name, char* board_name, char* block_name, 
			  CoordRange& boardCoordRange,
			  CoordRange& coordRange,
			  RegAspect aspect,
			  RegInteg integ,
			  RegInputMode mode,
			  bool extend,
			  bool splitIndices,
			  std::vector<RegDescription>& regs);

    public:

      void parseFromString(std::string  regSpec, 
			   std::vector<std::string>& regmapNames,
			   std::vector<std::string>& boardNames,
			   std::vector<std::string>& blockNames,
			   CoordRange& coordRange,
			   RegAspect&   aspect,
			   RegInteg&    integ,
			   RegInputMode mode,
			   bool throwIfNoMatch=true);
      
      void matchRegisters(ArrayMap* arrayMap, 
			  std::vector<std::string>& regmapNames,
			  std::vector<std::string>& boardNames,
			  std::vector<std::string>& blockNames,
			  CoordRange& coordRange,
			  RegAspect aspect,
			  RegInteg integ,
			  RegInputMode mode,
			  bool extend,
			  bool splitIndices,
			  std::vector<RegDescription>& regs);

    public:

      static std::vector<std::string> parseIntoPeriodSeparatedStrings(std::string regSpec);

      static std::vector<RegSpecItem> parseIntoPeriodSeparatedItems(std::string regSpec);

      static CoordRange  parseCoordRange(String& str);
      static CoordRange  parseCoordRangeCarma(String& str);
      static CoordRange  parseIndexCoordRangeCarma(String& str);

      static RegSpecItem parseItem(std::string str);
      static RegSpecItem parseItemCarma(std::string str);

      static std::vector<std::string> getExpandedNames(RegSpecItem& items);
      static std::vector<std::string> concatenateItem(std::vector<std::string>& inNames, RegSpecItem& item);
      static std::vector<std::string> getBoardNames(std::vector<RegSpecItem>& items);
      static std::vector<std::string> getBlockNames(std::vector<RegSpecItem>& items);
      static std::vector<std::string> getRegmapNames(std::vector<RegSpecItem>& items);
      
    }; // End class RegParser
    
  } // End namespace util
} // End namespace sza




#endif // End #ifndef SZA_UTIL_REGPARSER_H
