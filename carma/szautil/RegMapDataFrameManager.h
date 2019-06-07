#ifndef SZA_UTIL_REGMAPDATAFRAMEMANAGER_H
#define SZA_UTIL_REGMAPDATAFRAMEMANAGER_H

/**
 * @file RegMapDataFrameManager.h
 * 
 * Tagged: Sat Aug 14 13:12:19 UTC 2004
 * 
 * @author 
 */
#include <string>

#include "carma/szaarrayutils/szaregs.h"

#include "carma/szautil/Complex.h"
#include "carma/szautil/DataFrameManager.h"


namespace sza {
  namespace util {
    
    class ArrayMapDataFrameManager;
    class Date;

    class RegMapDataFrameManager : public DataFrameManager {
    public:
      
      /**
       * Destructor.
       */
      virtual ~RegMapDataFrameManager();
      
      /**
       * Overloaded base-class operator
       */
      void operator=(DataFrameManager& manager);
      
      /**
       * Inherited class assignment operator
       */
      virtual void operator=(RegMapDataFrameManager& manager);
      
      //-------------------------------------------------------------
      // Methods to pack a named register.
      //-------------------------------------------------------------
      
      void writeReg(std::string board, std::string name, 
      		    unsigned char* data, CoordRange* range=0);
      
      void writeReg(std::string board, std::string name, 
      		    signed char* data, CoordRange* range=0);
      
      void writeReg(std::string board, std::string name, 
      		    bool* data, CoordRange* range=0);
      
      void writeReg(std::string board, std::string name, 
      		    unsigned short* data, CoordRange* range=0);
      
      void writeReg(std::string board, std::string name, 
      		    signed short* data, CoordRange* range=0);
      
      void writeReg(std::string board, std::string name, 
      		    unsigned int* data, CoordRange* range=0);
      
      void writeReg(std::string board, std::string name, 
      		    signed int* data, CoordRange* range=0);
      
      void writeReg(std::string board, std::string name, 
      		    float* data, CoordRange* range=0);
      
      void writeReg(std::string board, std::string name, 
		    double* data, CoordRange* range=0);

      void writeReg(std::string board, std::string name, 
		    RegDate::Data* data, CoordRange* range=0);
      
      void writeReg(std::string board, std::string name, 
		    Complex<float>::Data* data, CoordRange* range=0);

      //------------------------------------------------------------
      // Methods for writing single values
      //------------------------------------------------------------

      void writeReg(std::string board, std::string name, 
      		    unsigned char data, CoordRange* range=0);
      
      void writeReg(std::string board, std::string name, 
      		    signed char data, CoordRange* range=0);
      
      void writeReg(std::string board, std::string name, 
      		    bool data, CoordRange* range=0);
      
      void writeReg(std::string board, std::string name, 
      		    unsigned short data, CoordRange* range=0);
      
      void writeReg(std::string board, std::string name, 
      		    signed short data, CoordRange* range=0);
      
      void writeReg(std::string board, std::string name, 
      		    unsigned int data, CoordRange* range=0);
      
      void writeReg(std::string board, std::string name, 
      		    signed int data, CoordRange* range=0);
      
      void writeReg(std::string board, std::string name, 
      		    float data, CoordRange* range=0);
      
      void writeReg(std::string board, std::string name, 
		    double data, CoordRange* range=0);

      void writeReg(std::string board, std::string name, 
		    RegDate::Data data, CoordRange* range=0);
      
      void writeReg(std::string board, std::string name, 
		    Complex<float>::Data data, CoordRange* range=0);

      //------------------------------------------------------------
      // Versions which don't lock
      //------------------------------------------------------------
      
      void writeRegNoLock(std::string board, std::string name, 
			  unsigned char* data, CoordRange* range=0);
      
      void writeRegNoLock(std::string board, std::string name, 
			  signed char* data, CoordRange* range=0);
      
      void writeRegNoLock(std::string board, std::string name, 
			  bool* data, CoordRange* range=0);
      
      void writeRegNoLock(std::string board, std::string name, 
			  unsigned short* data, CoordRange* range=0);
      
      void writeRegNoLock(std::string board, std::string name, 
			  signed short* data, CoordRange* range=0);
      
      void writeRegNoLock(std::string board, std::string name, 
			  unsigned int* data, CoordRange* range=0);
      
      void writeRegNoLock(std::string board, std::string name, 
			  signed int* data, CoordRange* range=0);
      
      void writeRegNoLock(std::string board, std::string name, 
			  float* data, CoordRange* range=0);
      
      void writeRegNoLock(std::string board, std::string name, 
			  double* data, CoordRange* range=0);
      
      void writeRegNoLock(std::string board, std::string name, 
			  RegDate::Data* data, CoordRange* range=0);
      
      void writeRegNoLock(std::string board, std::string name, 
			  Complex<float>::Data* data, CoordRange* range=0);

      //------------------------------------------------------------
      // Versions for writing a single value
      //------------------------------------------------------------

      void writeRegNoLock(std::string board, std::string name, 
			  unsigned char data, CoordRange* range=0);
      
      void writeRegNoLock(std::string board, std::string name, 
			  signed char data, CoordRange* range=0);
      
      void writeRegNoLock(std::string board, std::string name, 
			  bool data, CoordRange* range=0);
      
      void writeRegNoLock(std::string board, std::string name, 
			  unsigned short data, CoordRange* range=0);
      
      void writeRegNoLock(std::string board, std::string name, 
			  signed short data, CoordRange* range=0);
      
      void writeRegNoLock(std::string board, std::string name, 
			  unsigned int data, CoordRange* range=0);
      
      void writeRegNoLock(std::string board, std::string name, 
			  signed int data, CoordRange* range=0);
      
      void writeRegNoLock(std::string board, std::string name, 
			  float data, CoordRange* range=0);
      
      void writeRegNoLock(std::string board, std::string name, 
			  double data, CoordRange* range=0);
      
      void writeRegNoLock(std::string board, std::string name, 
			  RegDate::Data data, CoordRange* range=0);
      
      void writeRegNoLock(std::string board, std::string name, 
			  Complex<float>::Data data, CoordRange* range=0);

      //-------------------------------------------------------------
      // Methods to pack a named register.
      //-------------------------------------------------------------
      
      void writeReg(RegMapBlock* blk,
      		    unsigned char* data, CoordRange* range=0);
      
      void writeReg(RegMapBlock* blk,
      		    signed char* data, CoordRange* range=0);
      
      void writeReg(RegMapBlock* blk,
      		    bool* data, CoordRange* range=0);
      
      void writeReg(RegMapBlock* blk,
      		    unsigned short* data, CoordRange* range=0);
      
      void writeReg(RegMapBlock* blk,
      		    signed short* data, CoordRange* range=0);
      
      void writeReg(RegMapBlock* blk,
      		    unsigned int* data, CoordRange* range=0);
      
      void writeReg(RegMapBlock* blk,
      		    signed int* data, CoordRange* range=0);
      
      void writeReg(RegMapBlock* blk,
      		    float* data, CoordRange* range=0);
      
      void writeReg(RegMapBlock* blk,
		    double* data, CoordRange* range=0);
      
      void writeReg(RegMapBlock* blk,
		    RegDate::Data* data, CoordRange* range=0);
      
      void writeReg(RegMapBlock* blk,
		    Complex<float>::Data* data, CoordRange* range=0);

      //------------------------------------------------------------
      // Versions for writing a single value
      //------------------------------------------------------------

      void writeReg(RegMapBlock* blk,
      		    unsigned char data, CoordRange* range=0);
      
      void writeReg(RegMapBlock* blk,
      		    signed char data, CoordRange* range=0);
      
      void writeReg(RegMapBlock* blk,
      		    bool data, CoordRange* range=0);
      
      void writeReg(RegMapBlock* blk,
      		    unsigned short data, CoordRange* range=0);
      
      void writeReg(RegMapBlock* blk,
      		    signed short data, CoordRange* range=0);
      
      void writeReg(RegMapBlock* blk,
      		    unsigned int data, CoordRange* range=0);
      
      void writeReg(RegMapBlock* blk,
      		    signed int data, CoordRange* range=0);
      
      void writeReg(RegMapBlock* blk,
      		    float data, CoordRange* range=0);

      void writeReg(RegMapBlock* blk,
		    double data, CoordRange* range=0);
      
      void writeReg(RegMapBlock* blk,
		    RegDate::Data data, CoordRange* range=0);
      
      void writeReg(RegMapBlock* blk,
		    Complex<float>::Data data, CoordRange* range=0);

      //------------------------------------------------------------
      // Versions which don't lock
      //------------------------------------------------------------
      
      void writeRegNoLock(RegMapBlock* blk,
			  unsigned char* data, CoordRange* range=0);
      
      void writeRegNoLock(RegMapBlock* blk,
			  signed char* data, CoordRange* range=0);
      
      void writeRegNoLock(RegMapBlock* blk,
			  bool* data, CoordRange* range=0);
      
      void writeRegNoLock(RegMapBlock* blk,
			  unsigned short* data, CoordRange* range=0);
      
      void writeRegNoLock(RegMapBlock* blk,
			  signed short* data, CoordRange* range=0);
      
      void writeRegNoLock(RegMapBlock* blk,
			  unsigned int* data, CoordRange* range=0);
      
      void writeRegNoLock(RegMapBlock* blk,
			  signed int* data, CoordRange* range=0);
      
      void writeRegNoLock(RegMapBlock* blk,
			  float* data, CoordRange* range=0);
      
      void writeRegNoLock(RegMapBlock* blk,
			  double* data, CoordRange* range=0);
      
      void writeRegNoLock(RegMapBlock* blk,
			  RegDate::Data* data, CoordRange* range=0);
      
      void writeRegNoLock(RegMapBlock* blk,
			  Complex<float>::Data* data, CoordRange* range=0);

      //------------------------------------------------------------
      // Versions for writing a single value
      //------------------------------------------------------------
      
      void writeRegNoLock(RegMapBlock* blk,
			  unsigned char data, CoordRange* range=0);
      
      void writeRegNoLock(RegMapBlock* blk,
			  signed char data, CoordRange* range=0);
      
      void writeRegNoLock(RegMapBlock* blk,
			  bool data, CoordRange* range=0);
      
      void writeRegNoLock(RegMapBlock* blk,
			  unsigned short data, CoordRange* range=0);
      
      void writeRegNoLock(RegMapBlock* blk,
			  signed short data, CoordRange* range=0);
      
      void writeRegNoLock(RegMapBlock* blk,
			  unsigned int data, CoordRange* range=0);
      
      void writeRegNoLock(RegMapBlock* blk,
			  signed int data, CoordRange* range=0);
      
      void writeRegNoLock(RegMapBlock* blk,
			  float data, CoordRange* range=0);
      
      void writeRegNoLock(RegMapBlock* blk,
			  double data, CoordRange* range=0);
      
      void writeRegNoLock(RegMapBlock* blk,
			  RegDate::Data data, CoordRange* range=0);
      
      void writeRegNoLock(RegMapBlock* blk,
			  Complex<float>::Data data, CoordRange* range=0);

      //------------------------------------------------------------
      // Methods to read a named register
      //------------------------------------------------------------
      
      void readReg(std::string board, std::string name, 
      		   unsigned char* data, CoordRange* range=0);
      
      void readReg(std::string board, std::string name, 
      		   signed char* data, CoordRange* range=0);
      
      void readReg(std::string board, std::string name, 
      		   bool* data, CoordRange* range=0);
      
      void readReg(std::string board, std::string name, 
      		   unsigned short* data, CoordRange* range=0);
      
      void readReg(std::string board, std::string name, 
      		   signed short* data, CoordRange* range=0);
      
      void readReg(std::string board, std::string name, 
      		   unsigned int* data, CoordRange* range=0);
      
      void readReg(std::string board, std::string name, 
      		   signed int* data, CoordRange* range=0);
      
      void readReg(std::string board, std::string name, 
      		   float* data, CoordRange* range=0);
      
      void readReg(std::string board, std::string name, 
		   double* data, CoordRange* range=0);
      
      void readReg(std::string board, std::string name, 
		    RegDate::Data* data, CoordRange* range=0);
      
      void readReg(std::string board, std::string name, 
		   Complex<float>::Data* data, CoordRange* range=0);

      //------------------------------------------------------------
      // Versions which don't lock
      //------------------------------------------------------------
      
      void readRegNoLock(std::string board, std::string name, 
			 unsigned char* data, CoordRange* range=0);
      
      void readRegNoLock(std::string board, std::string name, 
			 signed char* data, CoordRange* range=0);
      
      void readRegNoLock(std::string board, std::string name, 
			 bool* data, CoordRange* range=0);
      
      void readRegNoLock(std::string board, std::string name, 
			 unsigned short* data, CoordRange* range=0);
      
      void readRegNoLock(std::string board, std::string name, 
			 signed short* data, CoordRange* range=0);
      
      void readRegNoLock(std::string board, std::string name, 
			 unsigned int* data, CoordRange* range=0);
      
      void readRegNoLock(std::string board, std::string name, 
			 signed int* data, CoordRange* range=0);
      
      void readRegNoLock(std::string board, std::string name, 
			 float* data, CoordRange* range=0);
      
      void readRegNoLock(std::string board, std::string name, 
			 double* data, CoordRange* range=0);
      
      void readRegNoLock(std::string board, std::string name, 
			 RegDate::Data* data, CoordRange* range=0);
      
      void readRegNoLock(std::string board, std::string name, 
			 Complex<float>::Data* data, CoordRange* range=0);

      //------------------------------------------------------------
      // Methods to read a named register
      //------------------------------------------------------------
      
      void readReg(RegMapBlock* blk,
      		   unsigned char* data, CoordRange* range=0);
      
      void readReg(RegMapBlock* blk,
      		   signed char* data, CoordRange* range=0);
      
      void readReg(RegMapBlock* blk,
      		   bool* data, CoordRange* range=0);
      
      void readReg(RegMapBlock* blk,
      		   unsigned short* data, CoordRange* range=0);
      
      void readReg(RegMapBlock* blk,
      		   signed short* data, CoordRange* range=0);
      
      void readReg(RegMapBlock* blk,
      		   unsigned int* data, CoordRange* range=0);
      
      void readReg(RegMapBlock* blk,
      		   signed int* data, CoordRange* range=0);
      
      void readReg(RegMapBlock* blk,
      		   float* data, CoordRange* range=0);
      
      void readReg(RegMapBlock* blk,
		   double* data, CoordRange* range=0);
      
      void readReg(RegMapBlock* blk,
		   RegDate::Data* data, CoordRange* range=0);
      
      void readReg(RegMapBlock* blk,
		   Complex<float>::Data* data, CoordRange* range=0);

      //------------------------------------------------------------
      // Versions which don't lock
      //------------------------------------------------------------
      
      void readRegNoLock(RegMapBlock* blk,
			 unsigned char* data, CoordRange* range=0);
      
      void readRegNoLock(RegMapBlock* blk,
			 signed char* data, CoordRange* range=0);
      
      void readRegNoLock(RegMapBlock* blk,
			 bool* data, CoordRange* range=0);
      
      void readRegNoLock(RegMapBlock* blk,
			 unsigned short* data, CoordRange* range=0);
      
      void readRegNoLock(RegMapBlock* blk,
			 signed short* data, CoordRange* range=0);
      
      void readRegNoLock(RegMapBlock* blk,
			 unsigned int* data, CoordRange* range=0);
      
      void readRegNoLock(RegMapBlock* blk,
			 signed int* data, CoordRange* range=0);
      
      void readRegNoLock(RegMapBlock* blk,
			 float* data, CoordRange* range=0);
      
      void readRegNoLock(RegMapBlock* blk,
			 double* data, CoordRange* range=0);
      
      void readRegNoLock(RegMapBlock* blk,
			 RegDate::Data* data, CoordRange* range=0);
      
      void readRegNoLock(RegMapBlock* blk,
			 Complex<float>::Data* data, CoordRange* range=0);

      // Return true if this frame only contains archived registers
      
      inline bool archivedOnly() {return archivedOnly_;};
      
      /**
       * Get a unique frame id based on integral MJD half-seconds.
       */
      unsigned int getId(unsigned nanoSecondInterval);

      /**
       * Set the mjd of this frame
       */
      void setMjd(TimeVal& mjd);
      void setMjd(double mjd);

      RegMapBlock* findReg(char* boardName, char* blockName);

      RegMapBoard* findRegMapBoard(std::string boardName);

      /**
       * Return the offset in bytes of the data for the requested
       * register, from the beginning of the frame buffer.
       */
      int byteOffsetInFrameOf(RegMapBlock* blk, Coord* coord=0);
      int byteOffsetInFrameOf(std::string board, std::string block, 
			      Coord* coord=0);
      
      int byteOffsetInFrameOf(RegMapBlock* blk, CoordRange* range);
      int byteOffsetInFrameOf(std::string board, std::string block, 
			      CoordRange* range);
      
    protected:
      
      friend class ArrayMapDataFrameManager;
      
      SzaRegMap*   regMap_;        // An SZA register map 
      
      /**
       * A flag which will determine if this manager's frame contains
       * all registers, or only archived registers.
       */
      bool archivedOnly_; 
      
      /**
       * Return the offset in the register map, of the data for this
       * register.
       */
      int byteOffsetInRegMapOf(RegMapBlock* blk, Coord* coord=0);
      int byteOffsetInRegMapOf(std::string board, std::string block, 
			       Coord* coord=0);
      int byteOffsetInRegMapOf(RegMapBlock* blk, CoordRange* range);
      int byteOffsetInRegMapOf(std::string board, std::string block, 
			       CoordRange* range);
      
      /**
       * Pack data of an arbitrary type into the underlying frame
       */
      virtual void packData(RegMapBlock* blk, void* data, CoordRange* range, 
			    DataType::Type type, bool lock=true);

      virtual void packData(std::string board, std::string block, void* data, 
			    CoordRange* range, DataType::Type type, bool lock=true);

      virtual void packValue(RegMapBlock* blk, void* data, CoordRange* range, 
			     DataType::Type type, bool lock=true);

      virtual void packValue(std::string board, std::string block, void* data, 
			     CoordRange* range, DataType::Type type, bool lock=true);

      /**
       * Unpack data of an arbitrary type from the underlying frame
       */
      void unpackData(RegMapBlock* blk, void* data, CoordRange* range, 
		      DataType::Type type, bool lock=true);

      void unpackData(std::string board, std::string block, void* data, 
		      CoordRange* range, DataType::Type type, bool lock=true);
      
      /**
       * Private constructors.  Only inheritors should be instantiated
       */
      RegMapDataFrameManager(bool archivedOnly=false);
      
      /**
       * Check the type and element number of a regmap block
       */
      void checkType(std::string board, std::string block, DataType::Type type,
		     CoordRange* range);
      
      void checkType(RegMapBlock* blk, DataType::Type type,
		     CoordRange* range);
      
    public:
      /**
       * Get the descriptor for this reg map block
       */
      RegMapBlock* getReg(std::string board, std::string block);
      
    protected:
      /**
       * Return true if the passed board is present in the register map
       */
      bool boardIsPresent(RegMapBoard* brd);
      
      /**
       * Return true if the block is present in the register map
       */
      bool blockIsPresent(RegMapBlock* blk);
      
      /**
       * Return true if this board is flagged.
       */
      bool boardIsFlagged(RegMapBoard* brd);

    private:

      AxisRange axisRange_;

    }; // End class RegMapDataFrameManager
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_REGMAPDATAFRAMEMANAGER_H
