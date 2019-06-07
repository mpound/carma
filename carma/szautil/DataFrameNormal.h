#ifndef SZA_UTIL_DATAFRAMENORMAL_H
#define SZA_UTIL_DATAFRAMENORMAL_H

/**
 * @file DataFrameNormal.h
 * 
 * Tagged: Sun Mar 21 15:15:45 UTC 2004
 * 
 * @author Erik Leitch
 */
#include <vector>

#include "carma/szautil/DataFrame.h"

namespace sza {
  namespace util {
    
    class DataFrameNormal : public DataFrame {
    public:
      
      /**
       * Constructors.
       */
      DataFrameNormal();

      /**
       * Constructor with buffer initialization
       */
      DataFrameNormal(unsigned int nBuffer);
      
      /**
       * Resize the internal buffer.
       */
      void resize(unsigned int nBuffer);

      /**
       * Return the size of the internal buffer.
       */
      unsigned int size();

      /**
       * Return a reference to a requested element.
       */
      inline unsigned char& operator[](unsigned index)
	{
	  return lvals_[index];
	}

      /**
       * Assignment operators, base-class and local
       */
      virtual void operator=(DataFrame& frame);
      virtual void operator=(DataFrameNormal& frame);

      /**
       * Get a pointer to our internal data suitable for using as an
       * external network buffer
       */
      virtual unsigned char* data();
      virtual std::vector<unsigned char>& dataVec();

      /**
       * Destructor.
       */
      virtual ~DataFrameNormal();
      
    private:

      friend class NetArrayDataFrameManager;

      std::vector<unsigned char> lvals_;

    }; // End class DataFrameNormal
  } // End namespace util
} // End namespace sza

#endif // End #ifndef SZA_UTIL_DATAFRAMENORMAL_H
