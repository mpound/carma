#ifndef SZA_UTIL_DATAARRAY_H
#define SZA_UTIL_DATAARRAY_H

/**
 * @file DataArray.h
 * 
 * Tagged: Fri Nov 14 12:39:32 UTC 2003
 * 
 * @author Erik Leitch
 */
// Required C header files from the array control code

#include "carma/szaarrayutils/regmap.h" // RegAddrMode

namespace sza {
  namespace util {
    
    /**
     * A class to encapsulate data packaging for different address
     * modes.
     */
    class DataArray {
    public:
      
      /**
       * Pack a source data array of unsigned longs into a
       * destination array of different types, using different
       * addressing modes.
       */
      static void pack(RegAddrMode addrMode, unsigned int flags,
		       unsigned int* destination, unsigned int* source, 
		       unsigned short first, unsigned short nreg);
      
      /**
       * Unpack a source data array of different types into a
       * destination array of unsigned longs, using different
       * addressing modes.
       */
      static void unpack(RegAddrMode addrMode, unsigned int flags,
			 unsigned int* destination, unsigned int* source, 
			 unsigned short first, unsigned short nreg);
      
      /**
       * Return the length, in bytes of a register un/packing operation.
       */
      static unsigned short byteLength(RegAddrMode addrMode, 
				       unsigned short nreg);
      
    private:
      
      /**
       * Prevents instantiation.  Singleton class with only static members.
       */
      DataArray();
      
      /**
       * Singleton instantiation of this class.
       */
      static DataArray dataArray_;
      
    }; // End class DataArray
    
  }; // End namespace util
}; // End namespace sza

#endif // End #ifndef SZA_UTIL_DATAARRAY_H
