#ifndef SZA_UTIL_DATAFRAME_H
#define SZA_UTIL_DATAFRAME_H

/**
 * @file DataFrame.h
 * 
 * Tagged: Sat Mar 20 00:16:55 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/AxisRange.h"
#include "carma/szautil/Complex.h"
#include "carma/szautil/RegDate.h"
#include "carma/szautil/DataType.h"
#include "carma/szautil/Mutex.h"

namespace sza {
  namespace util {
    
    class AxisRange;

    /**
     * A generic interface for a data frame consisting of contiguous
     * blocks of arbitrary-sized types.  Contains pure virtual
     * methods, so it cannot be instantiated.
     */
    class DataFrame { 
    public:
      
      /**
       * Constructors.
       */
      DataFrame();
      
      /**
       * Destructor.
       */
      virtual ~DataFrame();
      
      /**
       * Resize the internal buffer. This can't be pure virtual,
       * since I call it from the constructor.
       */
      virtual void resize(unsigned int nBuffer);
      
      /**
       * Return the size of the internal buffer.
       */
      virtual unsigned int size() = 0;
      
      /**
       * Return the number of registers in the internal buffer.
       */
      unsigned int nReg();
      
      /**
       * Return the number of bytes in the internal buffer.
       */
      unsigned int nByte();
      
      /**
       * Define an operator for accessing elements of the frame buffer.
       */
      virtual unsigned char& operator[](unsigned int index);
      
      /**
       * Define an overloadable operator for assignment
       */
      virtual void operator=(DataFrame& frame);
      
      /**
       * Get a pointer to our internal data suitable for using as an
       * external network buffer
       */
      virtual unsigned char* data() = 0;
      virtual std::vector<unsigned char>& dataVec() = 0;

      /**
       * Pack an array into our internal memory
       */
      void pack(void* data, AxisRange& range, DataType::Type type,
		unsigned iStart, bool lockFrame=true);
      
      /**
       * Pack an array into our internal memory from consecutive
       * locations in the input data array.
       */
      void pack(void* data, unsigned ndata, DataType::Type type,
		unsigned iStart, bool lockFrame=true);
      
      /**
       * Pack a single value for all elements of an array
       */
      void packValue(void* data, AxisRange& range, DataType::Type type,
		     unsigned iStart, bool lockFrame=true);
      
      /**
       * Pack a single value for all elements of an array
       */
      void packValue(void* data, unsigned ndata, DataType::Type type,
		     unsigned iStart, bool lockFrame=true);
      
      /**
       * Pack an array into our internal memory
       */
      void addSum(void* data, AxisRange& range, DataType::Type type,
		  unsigned iStart, bool lockFrame=true);
      
      /**
       * Pack an array into our internal memory from consecutive
       * locations in the input data array.
       */
      void addSum(void* data, unsigned ndata, DataType::Type type,
		  unsigned iStart, bool lockFrame=true);
      
      /**
       * Pack an array into our internal memory
       */
      void addRunningAverage(void* data, AxisRange& range, DataType::Type type,
			     unsigned iStart, bool lockFrame=true);
      
      /**
       * Pack an array into our internal memory from consecutive
       * locations in the input data array.
       */
      void addRunningAverage(void* data, unsigned ndata, DataType::Type type,
			     unsigned iStart, bool lockFrame=true);
      
      void resetRunningAvgCounter();
      void incrementRunningAvgCounter();

      /**
       * Pack an array into our internal memory
       */
      void addUnion(void* data, AxisRange& range, DataType::Type type,
		    unsigned iStart, bool lockFrame=true);
      
      /**
       * Pack an array into our internal memory from consecutive
       * locations in the input data array.
       */
      void addUnion(void* data, unsigned ndata, DataType::Type type,
		    unsigned iStart, bool lockFrame=true);
      
      /**
       * Unpack an array from our internal memory
       */
      void unpack(void* data, AxisRange& range, DataType::Type type,
		  unsigned iStart, bool lockFrame=true);

      /**
       * Unpack an array from our internal memory to consecutive
       * locations in the output data array.
       */
      void unpack(void* data, unsigned ndata, DataType::Type type,
		  unsigned iStart, bool lockFrame=true);

      /**
       * Return an arbitrary pointer to our internal data, cast as the
       * requested type.  Calls the following type-specific methods
       * under the hood.
       */
      void* getPtr(unsigned int index, DataType::Type type);
      
      // Methods for returning pointers of different types to internal
      // data.  These are made virtual so that inheritors can define
      // what, if anything, happens when they are called.  For certain
      // types of frames, it may be inappropriate to recast pointers
      // to internal data, in which case some of the following should
      // throw exceptions.

      /**
       * Return a pointer to our internal data, cast as a bool
       * pointer.
       */
      virtual bool* getBoolPtr(unsigned int index=0);
      
      /**
       * Return a pointer to our internal data, cast as an unsigned
       * char pointer.
       */
      virtual unsigned char* getUcharPtr(unsigned int index=0);
      
      /**
       * Return a pointer to our internal data, cast as a char
       * pointer.
       */
      virtual char* getCharPtr(unsigned int index=0);
      
      /**
       * Return a pointer to our internal data, cast as an unsigned
       * short pointer.
       */
      virtual unsigned short* getUshortPtr(unsigned int index=0);
      
      /**
       * Return a pointer to our internal data, cast as a short
       * pointer.
       */
      virtual short* getShortPtr(unsigned int index=0);
      
      /**
       * Return a pointer to our internal data, cast as an unsigned
       * int pointer.
       */
      virtual unsigned int* getUintPtr(unsigned int index=0);
      
      /**
       * Return a pointer to our internal data, cast as an integer
       * pointer.
       */
      virtual int* getIntPtr(unsigned int index=0);
      
      /**
       * Return a pointer to our internal data, cast as an unsigned
       * long pointer.
       */
      virtual unsigned long* getUlongPtr(unsigned long index=0);
      
      /**
       * Return a pointer to our internal data, cast as an integer
       * pointer.
       */
      virtual long* getLongPtr(unsigned long index=0);
      
      /**
       * Return a pointer to our internal data, cast as a float
       * pointer.
       */
      virtual float* getFloatPtr(unsigned int index=0);
      
      /**
       * Return a pointer to our internal data, cast as a double
       * pointer.
       */
      virtual double* getDoublePtr(unsigned int index=0);

      /**
       * Return a pointer to our internal data, cast as a Date
       * pointer.
       */
      virtual RegDate::Data* getDatePtr(unsigned int index=0);

      /**
       * Return a pointer to our internal data, cast as a
       * Complex<float> pointer.
       */
      virtual Complex<float>::Data* getComplexFloatPtr(unsigned int index=0);

      /**
       * Lock the frame
       */
      void lock();

      /**
       * Unlock the frame
       */
      void unlock();

    public:

      // A counter for running averages

      unsigned nAvg_;

    private:

      /**
       * A mutex to protect the data frame.
       */
      Mutex guard_;

      /**
       * A utility member 
       */
      AxisRange axisRange_;

    }; // End class DataFrame
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_DATAFRAME_H
