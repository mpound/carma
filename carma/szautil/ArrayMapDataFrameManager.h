#ifndef SZA_UTIL_ARRAYMAPDATAFRAMEMANAGER_H
#define SZA_UTIL_ARRAYMAPDATAFRAMEMANAGER_H

/**
 * @file ArrayMapDataFrameManager.h
 * 
 * Tagged: Wed Sep  1 03:59:25 UTC 2004
 * 
 * @author 
 */
#include <string>

#include "carma/szaarrayutils/arraymap.h"

#include "carma/szautil/DataFrameManager.h"
#include "carma/szautil/RegMapDataFrameManager.h"
#include "carma/szautil/RegRange.h"
#include "carma/szautil/Complex.h"

namespace sza {
  namespace util {
    
    class CoordRange;
    class Date;
    class RegDescription;

    class ArrayMapDataFrameManager : public DataFrameManager {
    public:
      
      /**
       * Destructor.
       */
      virtual ~ArrayMapDataFrameManager();
      
      /**
       * Overloaded base-class operator
       */
      void operator=(DataFrameManager& manager);
      
      /**
       * Class assignment operator, overloadable by inheritors.
       */
      virtual void operator=(ArrayMapDataFrameManager& manager);

      /**
       * Class increment operator.  Operator adds together parts of
       * the data frame differently depending on the register flag
       * specifications.
       */
      ArrayMapDataFrameManager& operator+=(ArrayMapDataFrameManager& fm);
      
      /**
       * Write a register map
       */
      void writeRegMap(ArrRegMap* aregmap, 
		       RegMapDataFrameManager& fm, bool lockFrame);

      void writeRegMap(std::string regmap,
		       RegMapDataFrameManager& fm, bool lockFrame);

      /**
       * Read a register map
       */
      void readRegMap(ArrRegMap* aregmap, RegMapDataFrameManager& fm);
      void readRegMap(std::string regmap, RegMapDataFrameManager& fm);

      //-------------------------------------------------------------
      // Methods to pack a named register.
      //-------------------------------------------------------------
      
      void writeReg(std::string regmap, std::string board, std::string name, 
		    unsigned char* data, CoordRange* range=0);
      
      void writeReg(std::string regmap, std::string board, std::string name, 
		    signed char* data, CoordRange* range=0);
      
      void writeReg(std::string regmap, std::string board, std::string name, 
		    bool* data, CoordRange* range=0);
      
      void writeReg(std::string regmap, std::string board, std::string name, 
		    unsigned short* data, CoordRange* range=0);
      
      void writeReg(std::string regmap, std::string board, std::string name, 
		    signed short* data, CoordRange* range=0);
      
      void writeReg(std::string regmap, std::string board, std::string name, 
		    unsigned int* data, CoordRange* range=0);
      
      void writeReg(std::string regmap, std::string board, std::string name, 
		    signed int* data, CoordRange* range=0);
      
      void writeReg(std::string regmap, std::string board, std::string name, 
		    float* data, CoordRange* range=0);
      
      void writeReg(std::string regmap, std::string board, std::string name, 
		    double* data, CoordRange* range=0);

      void writeReg(std::string regmap, std::string board, std::string name, 
		    RegDate::Data* data, CoordRange* range=0);
      
      void writeReg(std::string regmap, std::string board, std::string name, 
		    Complex<float>::Data* data, CoordRange* range=0);

      //-------------------------------------------------------------
      // Methods to pack single-valued registers.
      //-------------------------------------------------------------
      
      void writeReg(std::string regmap, std::string board, std::string name, 
		    unsigned char data, CoordRange* range=0);
      
      void writeReg(std::string regmap, std::string board, std::string name, 
		    signed char data, CoordRange* range=0);
      
      void writeReg(std::string regmap, std::string board, std::string name, 
		    bool data, CoordRange* range=0);
      
      void writeReg(std::string regmap, std::string board, std::string name, 
		    unsigned short data, CoordRange* range=0);
      
      void writeReg(std::string regmap, std::string board, std::string name, 
		    signed short data, CoordRange* range=0);
      
      void writeReg(std::string regmap, std::string board, std::string name, 
		    unsigned int data, CoordRange* range=0);
      
      void writeReg(std::string regmap, std::string board, std::string name, 
		    signed int data, CoordRange* range=0);
      
      void writeReg(std::string regmap, std::string board, std::string name, 
		    float data, CoordRange* range=0);
      
      void writeReg(std::string regmap, std::string board, std::string name, 
		    double data, CoordRange* range=0);

      void writeReg(std::string regmap, std::string board, std::string name, 
		    RegDate::Data data, CoordRange* range=0);
      
      void writeReg(std::string regmap, std::string board, std::string name, 
		    Complex<float>::Data data, CoordRange* range=0);

      //-------------------------------------------------------------
      // Methods to pack a named register.
      //-------------------------------------------------------------

      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    unsigned char* data, CoordRange* range=0, bool lockFrame=true);
      
      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    signed char* data, CoordRange* range=0, bool lockFrame=true);
      
      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    bool* data, CoordRange* range=0, bool lockFrame=true);
      
      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    unsigned short* data, CoordRange* range=0, bool lockFrame=true);
      
      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    signed short* data, CoordRange* range=0, bool lockFrame=true);
      
      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    unsigned int* data, CoordRange* range=0, bool lockFrame=true);
      
      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    signed int* data, CoordRange* range=0, bool lockFrame=true);
      
      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    float* data, CoordRange* range=0, bool lockFrame=true);
      
      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    double* data, CoordRange* range=0, bool lockFrame=true);
      
      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    RegDate::Data* data, CoordRange* range=0, bool lockFrame=true);
      
      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    Complex<float>::Data* data, CoordRange* range=0, bool lockFrame=true);

      //-------------------------------------------------------------
      // Methods to pack a single-valued register.
      //-------------------------------------------------------------

      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    unsigned char data, CoordRange* range=0);
      
      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    signed char data, CoordRange* range=0);
      
      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    bool data, CoordRange* range=0);
      
      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    unsigned short data, CoordRange* range=0);
      
      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    signed short data, CoordRange* range=0);
      
      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    unsigned int data, CoordRange* range=0);
      
      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    signed int data, CoordRange* range=0);
      
      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    float data, CoordRange* range=0);
      
      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    double data, CoordRange* range=0);
      
      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    RegDate::Data data, CoordRange* range=0);
      
      void writeReg(ArrRegMap* aregmap, RegMapBlock* blk,
		    Complex<float>::Data data, CoordRange* range=0);

      //------------------------------------------------------------
      // Methods to read a named register
      //------------------------------------------------------------
      
      void readReg(std::string regmap, std::string board, std::string name, 
		   unsigned char* data, CoordRange* range=0);
      
      void readReg(std::string regmap, std::string board, std::string name, 
		   signed char* data, CoordRange* range=0);
      
      void readReg(std::string regmap, std::string board, std::string name, 
		   bool* data, CoordRange* range=0);
      
      void readReg(std::string regmap, std::string board, std::string name, 
		   unsigned short* data, CoordRange* range=0);
      
      void readReg(std::string regmap, std::string board, std::string name, 
		   signed short* data, CoordRange* range=0);
      
      void readReg(std::string regmap, std::string board, std::string name, 
		   unsigned int* data, CoordRange* range=0);
      
      void readReg(std::string regmap, std::string board, std::string name, 
		   signed int* data, CoordRange* range=0);
      
      void readReg(std::string regmap, std::string board, std::string name, 
		   float* data, CoordRange* range=0);
      
      void readReg(std::string regmap, std::string board, std::string name, 
		   double* data, CoordRange* range=0);
      
      void readReg(std::string regmap, std::string board, std::string name, 
		   RegDate::Data* data, CoordRange* range=0);
      
      void readReg(std::string regmap, std::string board, std::string name, 
		   Complex<float>::Data* data, CoordRange* range=0);

      //------------------------------------------------------------
      // Methods to read a named register
      //------------------------------------------------------------
      
      void readReg(ArrRegMap* aregmap, RegMapBlock* blk,
		   unsigned char* data, CoordRange* range=0);
      
      void readReg(ArrRegMap* aregmap, RegMapBlock* blk,
		   signed char* data, CoordRange* range=0);
      
      void readReg(ArrRegMap* aregmap, RegMapBlock* blk,
		   bool* data, CoordRange* range=0);
      
      void readReg(ArrRegMap* aregmap, RegMapBlock* blk,
		   unsigned short* data, CoordRange* range=0);
      
      void readReg(ArrRegMap* aregmap, RegMapBlock* blk,
		   signed short* data, CoordRange* range=0);
      
      void readReg(ArrRegMap* aregmap, RegMapBlock* blk,
		   unsigned int* data, CoordRange* range=0);
      
      void readReg(ArrRegMap* aregmap, RegMapBlock* blk,
		   signed int* data, CoordRange* range=0);
      
      void readReg(ArrRegMap* aregmap, RegMapBlock* blk,
		   float* data, CoordRange* range=0);
      
      void readReg(ArrRegMap* aregmap, RegMapBlock* blk,
		   double* data, CoordRange* range=0);
      
      void readReg(ArrRegMap* aregmap, RegMapBlock* blk,
		   RegDate::Data* data, CoordRange* range=0);
      
      void readReg(ArrRegMap* aregmap, RegMapBlock* blk,
		   Complex<float>::Data* data, CoordRange* range=0);

      // Return the value of the current register in a range, cast as
      // a double

      double getRegVal(RegRange& range, bool lockFrame);

      // Return the value of the requested register index, cast as a double

      double getRegVal(RegDescription& desc);

      // Return true if this frame only contains archived registers

      inline bool archivedOnly() {return archivedOnly_;};

      // Return a pointer to the array map

      inline SzaArrayMap* arrayMap() {
	return arrayMap_;
      }

      /**
       * Get a unique frame id based on integral MJD half-seconds.
       */
      unsigned int getId(unsigned nanoSecondInterval);

      int byteOffsetInFrameOf(std::string regmap, std::string board, 
			      std::string block, Coord* coord=0);

      // Get a pointer to the start of the register described in a
      // RegDescription, in our data frame.

      void* getPtr(RegDescription& desc);

    public:
      
      SzaArrayMap* arrayMap_;       // The SZA array map 
      
      bool archivedOnly_;           // True if this frame should
				    // contain only archived registers
      /**
       * Constructor.
       */
      ArrayMapDataFrameManager(bool archivedOnly_=false);

      /**
       * Return the offset in the register map, of the data for this
       * register.
       */
      int byteOffsetInFrameOf(ArrRegMap* aregmap, RegMapBlock* blk, 
			      Coord* coord=0);
      int byteOffsetInFrameOf(ArrRegMap* aregmap);
      
      /**
       * Pack data of an arbitrary type into the underlying frame
       */
      void packData(ArrRegMap* aregmap, RegMapBlock* blk, 
		    void* data, CoordRange* range, 
		    DataType::Type type, bool lockFrame=true);
      void packData(std::string regmap, std::string board, std::string block, 
		    void* data, CoordRange* range, 
		    DataType::Type type, bool lockFrame=true);

      /**
       * Unpack data of an arbitrary type from the underlying frame
       */
      void unpackData(ArrRegMap* aregmap, RegMapBlock* blk, 
		      void* data, CoordRange* range, 
		      DataType::Type type, bool lockFrame=true);
      void unpackData(std::string regmap, std::string board, std::string block, 
		      void* data, CoordRange* range, 
		      DataType::Type type,bool lockFrame=true);
      
      /**
       * Check the type and element number of a regmap block
       */
      void checkType(std::string regmap, std::string board, std::string block, 
		     DataType::Type type,
		     CoordRange* range);

      void checkType(ArrRegMap* aregmap, RegMapBlock* blk, DataType::Type type,
		     CoordRange* range);

    public:
      /**
       * Get the descriptor for this reg map block
       */
      RegMapBlock* getReg(std::string regmap, std::string board, 
			  std::string block);

      RegMapBlock* findReg(std::string regmap, std::string board, 
			   std::string block);

      ArrRegMap* getArrReg(std::string regmap);

      /**
       * Return true if the block is present in the register map
       */
      bool regMapIsPresent(RegMap* regmap);

      /**
       * Return true if the passed board is present in the register map
       */
      bool boardIsPresent(RegMapBoard* brd);

      /**
       * Return true if this board is flagged.
       */
      bool boardIsFlagged(ArrRegMap* aregmap, RegMapBoard* brd);

      /**
       * Return true if the block is present in the register map
       */
      bool blockIsPresent(RegMapBlock* blk);

    }; // End class ArrayMapDataFrameManager
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_ARRAYMAPDATAFRAMEMANAGER_H
