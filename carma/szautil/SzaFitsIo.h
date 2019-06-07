#ifndef SZA_UTIL_SZAFITSIO_H
#define SZA_UTIL_SZAFITSIO_H

/**
 * @file SzaFitsIo.h
 * 
 * Tagged: Tue May  3 16:46:04 UTC 2005
 * 
 * @author 
 */
#include "carma/szautil/FitsIo.h"

namespace sza {
  namespace util {
    
    class SzaFitsIo : public FitsIo {
    public:
      
      /**
       * Constructor.
       */
      SzaFitsIo();
      
      /**
       * Destructor.
       */
      virtual ~SzaFitsIo();
      
    private:
    }; // End class SzaFitsIo
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_SZAFITSIO_H
