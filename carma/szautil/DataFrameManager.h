#ifndef SZA_UTIL_DATAFRAMEMANAGER_H
#define SZA_UTIL_DATAFRAMEMANAGER_H

/**
 * @file DataFrameManager.h
 * 
 * Tagged: Fri Nov 14 12:37:53 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/DataFrame.h"
#include "carma/szautil/DataType.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/Mutex.h"

namespace sza {
  namespace util {
    
    /**
     * This is a base class for managing a generic dataframe of
     * registers.
     */
    class DataFrameManager {
      
    public:
      
      /**
       * Constructors
       */
      DataFrameManager();
      DataFrameManager(unsigned nBuffer);
      DataFrameManager(DataFrameManager& fm);
      DataFrameManager(DataFrame* frame);
      
      /**
       * Destructor
       */
      virtual ~DataFrameManager();
      
      /**
       * Resize the data frame
       */
      void resize(unsigned int nBuffer);

      /**
       * Pack an array of unsigned ints
       */
      void pack(unsigned int* data, unsigned int ndata, 
		int startIndex=-1);

      /**
       * Pack an array of unsigned longs
       */
      void pack(unsigned long* data, unsigned int ndata, 
		int startIndex=-1);

      /**
       * Pack an array of floats
       */
      void pack(float* data, unsigned int ndata, 
		int startIndex=-1);

      /**
       * Pack a dataframe into the requested starting location
       */
      void pack(DataFrame* frame, int startIndex);

      /**
       * Fill the next ndata registers with a constant value.
       */
      void fillBuffer(unsigned char val, unsigned int ndata);
      
      /**
       * Fill all data registers with a constant value.
       */
      void fillBuffer(unsigned char val);

      /**
       * Set a global error status for this frame.
       */
      void setErrorStatus(bool wasError);
      
      /**
       * Return a handle to the raw data frame managed by this object
       */
      DataFrame* frame();
      
      /**
       * Unpack the next ndata elements of the frame into a passed
       * byte array (incremental unpack)
       *
       * @throw Exception
       */
      void unpack(unsigned char* data, unsigned int ndata);

      /**
       * Unpack the whole frame.
       *
       * @throw Exception
       */
      void unpack(unsigned char* data);

      /**
       * Advance the internal buffer ndata elements.
       */
      void advance(unsigned ndata);
      
      /**
       * Reinitialize.  Make this virtual so that inheritors can
       * define what it means to reinitialize.
       */
      virtual void reinitialize();
      
      /**
       * Lock the frame
       */
      void lock();

      /**
       * Unlock the frame
       */
      void unlock();

      /**
       * Get a unique frame id based on integral MJD half-seconds.
       * Force inheritors to define this.
       */
      virtual unsigned int getId() {
	return 0;
      };

      /**
       * The data portion of the buffer will be offset by the size in bytes
       * of the header.
       */
      unsigned byteOffsetInFrameOfData();

      /**
       * Return the size in bytes of the frame managed by this object
       */
      unsigned sizeInBytes();

      /**
       * Return the size in bytes of the data portion only, of the
       * frame managed by this object
       */
      unsigned sizeInBytesOfData();
      
      /**
       * Define an overloadable assignment operator
       */ 
      virtual void operator=(DataFrameManager& fm);

    public:

      sza::util::DataFrame* frame_;// The data frame managed by this object

      bool dataIsInitialized_;     // False until the DataFrame buffer
				   // has been allocated
      unsigned currentIndex_;      // Current index into the
				   // DataFrame buffer
      unsigned nBuffer_;           // The number of bytes in the buffer
      unsigned nUsed_;             // The number of bytes used

    private:

      /**
       * Pack an array of arbitrary types
       */
      void pack(void* data, unsigned int ndata, DataType::Type type,
		int startIndex=-1);

      /**
       * Initialize this object
       */
      void initialize();

    }; // End class DataFrameManager
    
  }; // End namespace util
}; // End namespace sza

#endif // End #ifndef 
