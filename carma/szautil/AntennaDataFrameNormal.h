#ifndef SZA_UTIL_ANTENNADATAFRAMENORMAL_H
#define SZA_UTIL_ANTENNADATAFRAMENORMAL_H

/**
 * @file AntennaDataFrameNormal.h
 * 
 * Tagged: Sun Mar 21 16:58:29 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/AntennaDataFrame.h"
#include "carma/szautil/DataFrameNormal.h"

namespace sza {
  namespace util {
    
    /**
     * This is just a normal data frame, with an additional member
     * which specifies the antenna.
     */
    class AntennaDataFrameNormal : 
      public sza::util::AntennaDataFrame,
      public sza::util::DataFrameNormal {
      
      public:
      
      /**
       * Constructor
       */
      AntennaDataFrameNormal(unsigned int nBuffer);
      
      /**
       * Assignment operators
       */
      void operator=(sza::util::DataFrameNormal& frame);
      void operator=(AntennaDataFrameNormal& frame);
      
    }; // End class AntennaDataFrameNormal
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_ANTENNADATAFRAMENORMAL_H
