#ifndef arraymap_h
#define arraymap_h

#include "carma/szaarrayutils/regmap.h"
#include "carma/szaarrayutils/hash.h"
#include "carma/szaarrayutils/netbuf.h"
#include "carma/szaarrayutils/input.h"
#include "carma/szaarrayutils/output.h"

#include <map>

#define KLUDGE

/*
 * Define type aliases for the structures that are being declared
 * below.
 */
typedef struct ArrayMap ArrayMap;
typedef struct ArrRegMap ArrRegMap;
typedef struct ArrRegMapReg ArrRegMapReg;
/*
 * A structure of the following type is used to encapsulate a single
 * register block from an array map
 */
struct ArrRegMapReg {
  unsigned regmap; // The index of the specified regmap in the array map
  RegMapReg reg;
};

int find_ArrRegMapReg(ArrayMap* arraymap, 
		     std::string regmap_name, std::string board_name, std::string block_name, 
		     RegAspect facet, unsigned index, unsigned n,
		     ArrRegMapReg *reg);

int init_ArrRegMapReg(ArrayMap* arraymap, 
		      unsigned regmap, unsigned board, unsigned block,
		      unsigned index, unsigned nreg, RegAspect aspect,
		      ArrRegMapReg *reg);

RegValidity input_ArrRegMapReg(InputStream *stream, int tell, 
			       ArrayMap* arraymap, RegInputMode mode, 
			       int extend, ArrRegMapReg *reg);

int output_ArrRegMapReg(OutputStream *stream, ArrayMap* arraymap, 
			RegOutputMode mode, ArrRegMapReg *reg);

void clr_ArrRegMapReg(ArrRegMapReg *reg);

/*
 * Collect information about a register map.
 */
struct ArrRegMap {
  char name[REG_NAME_LEN+1]; // An unambiguous name for the register map
  std::string* comment_;

  unsigned number;           // The index of this register map in the
			     // parent array map
  int slot;                  // The start index of this register map
			     // in the archive array
  unsigned iSlot_;           // The start index of this register map
			     // in the slot array
  int iArcSlot_;             // The start index of this register map
			     // in the archive slot array
  unsigned iByte_;           // The sequential byte index of this
			     // register map in the array map
  int iArcByte_;             // The sequential byte index of this
			     // register map in the archived array
			     // map, or -1 if this register map
			     // contains no archived registers in the
			     // archive byte array
  RegMap *regmap;            // Pointer to the register map

  // Constructor

  ArrRegMap(ArrayMap* parent, void* vregtemp, unsigned iRegMap, bool old=false, bool addPerBrdRegs=true);

  ArrRegMap();

  // Destructor

  ~ArrRegMap();

  // Return the byte offset in the ArrayMap of this ArrRegMap

  int byteOffsetInArcArrayMap() {
    return iArcByte_;
  };

  int byteOffsetInWholeArrayMap() {
    return iByte_;
  };

  // Convenience method for the above

  int byteOffsetInArrayMap(bool archivedOnly) {
    return archivedOnly ? byteOffsetInArcArrayMap() :
      byteOffsetInWholeArrayMap();
  }

  // Return the byte offset in the ArrayMap of the named block

  int byteOffsetInArcArrayMapOf(RegMapBlock* block, sza::util::Coord* coord=0);
  int byteOffsetInWholeArrayMapOf(RegMapBlock* block, sza::util::Coord* coord=0);

  // Convenience method

  int byteOffsetInArrayMapOf(bool archivedOnly, 
			     RegMapBlock* block, sza::util::Coord* coord=0) 
  {
    return archivedOnly ? byteOffsetInArcArrayMapOf(block, coord) :
      byteOffsetInWholeArrayMapOf(block, coord);
  }
  
  // Return the slot offset in the ArrayMap of this ArrRegMap

  int slotOffsetInArcArrayMap() {
    return iArcSlot_;
  };

  int slotOffsetInWholeArrayMap() {
    return iSlot_;
  };

  // Convenience method for the above

  int slotOffsetInArrayMap(bool archivedOnly) {
    return archivedOnly ? slotOffsetInArcArrayMap() :
      slotOffsetInWholeArrayMap();
  }

  // Return the slot offset in the ArrayMap of the named block

  int slotOffsetInArcArrayMapOf(RegMapBlock* block, sza::util::Coord* coord=0);
  int slotOffsetInWholeArrayMapOf(RegMapBlock* block, sza::util::Coord* coord=0);

  // Convenience method

  int slotOffsetInArrayMapOf(bool archivedOnly, 
			     RegMapBlock* block, sza::util::Coord* coord=0) 
  {
    return archivedOnly ? slotOffsetInArcArrayMapOf(block, coord) :
      slotOffsetInWholeArrayMapOf(block, coord);
  }
  
  // Return byte statistics for this register map

  unsigned nByte() {return regmap->nByte();};
  unsigned nArcByte() {return regmap->nArcByte();};

  // Convenience method for the above

  int nByte(bool archivedOnly) {
    return archivedOnly ? nArcByte() : nByte();
  }

  // Return a vector of boards matching the input string

  std::vector<RegMapBoard*> matchRegMapBoard(std::string regExpString);
  std::vector<RegMapBoard*> matchRegMapBoard(std::string regExpString, sza::util::CoordRange& range);
  std::vector<RegMapBoard*> matchRegMapBoard(std::vector<std::string>& boards);
};

/*
 * Collect information about all register maps.
 */
struct ArrayMap {
  unsigned ref_count;        /* Reference counter */
  std::vector<ArrRegMap*> regmaps; /* All register maps in this array */
  int nregmap;               /* The number of register maps */
  unsigned nreg;             /* The total number of registers */
  unsigned narchive;         /* The total number of archived registers */
  unsigned nByte_;           /* The total number of bytes */
  unsigned nArcByte_;        /* The total number of archived bytes */
  std::map<std::string, ArrRegMap*> regmapMap_;

  // Constructor

  ArrayMap(void* vtmp, bool old=false, bool addPerBrdRegs=true);
  ArrayMap();

  // Destructor

  ~ArrayMap();

  // Return the offset, in bytes_ of the named board and block from
  // the start of the archived array

  int byteOffsetInArcArrayMapOf(std::string regmap, std::string board, 
				std::string block, 
				sza::util::Coord* coord=0);

  int byteOffsetInWholeArrayMapOf(std::string regmap, std::string board, 
				  std::string block, 
				  sza::util::Coord* coord=0);

  // Convenience method 

  int byteOffsetOf(bool archivedOnly, std::string regmap, std::string board, 
		   std::string block, 
		   sza::util::Coord* coord=0) 
  {
    return archivedOnly ? 
      byteOffsetInArcArrayMapOf(regmap, board, block, coord) :
      byteOffsetInWholeArrayMapOf(regmap, board, block, coord);
  }

  // Byte offsets

  int byteOffsetInArcArrayMapOf(ArrRegMap* aregmap, RegMapBlock* block, 
				sza::util::Coord* coord=0);

  int byteOffsetInWholeArrayMapOf(ArrRegMap* aregmap, RegMapBlock* block, 
				  sza::util::Coord* coord=0);

  // Convenience method 

  int byteOffsetOf(bool archivedOnly, ArrRegMap* aregmap, RegMapBlock* block, 
		   sza::util::Coord* coord=0) 
  {
    return archivedOnly ? byteOffsetInArcArrayMapOf(aregmap, block, coord) :
      byteOffsetInWholeArrayMapOf(aregmap, block, coord);
  }

  // Slot offsets

  int slotOffsetInArcArrayMapOf(ArrRegMap* aregmap, RegMapBlock* block, 
				sza::util::Coord* coord=0);

  int slotOffsetInWholeArrayMapOf(ArrRegMap* aregmap, RegMapBlock* block, 
				  sza::util::Coord* coord=0);

  // Convenience method 

  int slotOffsetOf(bool archivedOnly, ArrRegMap* aregmap, RegMapBlock* block, 
		   sza::util::Coord* coord=0) {
    return archivedOnly ? slotOffsetInArcArrayMapOf(aregmap, block, coord) :
      slotOffsetInWholeArrayMapOf(aregmap, block, coord);
  }

  // Return the slot offset in the ArrayMap of the named block

  int slotOffsetInArcArrayMapOf(std::string regmap, std::string board, 
				std::string block, 
				sza::util::Coord* coord=0);
  int slotOffsetInWholeArrayMapOf(std::string regmap, std::string board, 
				  std::string block, 
				  sza::util::Coord* coord=0);

  // Convenience method

  int slotOffsetOf(bool archivedOnly, 
		   std::string regmap, std::string board, std::string block, 
		   sza::util::Coord* coord=0) 
  {
    return archivedOnly ? 
      slotOffsetInArcArrayMapOf(regmap, board, block, coord) :
      slotOffsetInWholeArrayMapOf(regmap, board, block, coord);
  }

  // Return the number of bytes in this array map

  int nByte(bool archivedOnly) {
    return archivedOnly ? nArcByte_ : nByte_;
  }

  // Return the number of registers in this array map

  int nReg(bool archivedOnly) {
    return archivedOnly ? narchive : nreg;
  }

  // Find the named block

  RegMapBlock* findArrayMapBlock(std::string regmap_name, std::string board_name, 
				 std::string block_name,
				 bool doThrow=false);
  // Find the named register map

  RegMap* findRegMap(std::string regmap_name);

  // Return the arregmap associated wtih a register map

  ArrRegMap* findArrRegMap(RegMap* regmapPtr);
  ArrRegMap* findArrRegMap(std::string regmapName);

  // Return a vector of regmaps which match the input string

  std::vector<ArrRegMap*> matchArrRegMap(std::string regExpString);
  std::vector<ArrRegMap*> matchArrRegMap(std::vector<std::string>& regmaps);
};

/* Allocate a readonly alias to a given register map. */
/*  When no longer required this should be discarded by calling */
/*  del_RegMap(). */

ArrayMap *alias_ArrayMap(ArrayMap *arrayMap);

/* Delete a register map or one of its aliases. */

ArrayMap *del_ArrayMap(ArrayMap *arrayMap);

/* Return non-zero if two register maps are equivalent. */

int equiv_ArrayMap(ArrayMap *arrayMap1, ArrayMap *arrayMap2);

ArrRegMap* find_ArrRegMap(ArrayMap *arrayMap, std::string regMapName);

RegMapBlock* find_ArrayMapBlock(ArrayMap *arraymap, std::string regmap_name, 
 				std::string board_name, std::string block_name);

#endif
