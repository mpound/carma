#ifndef SZA_UTIL_YIGBITS_H
#define SZA_UTIL_YIGBITS_H

/**
 * @file YigBits.h
 * 
 * Tagged: Wed Apr 29 15:03:24 PDT 2009
 * 
 * @author SZA data acquisition
 */
namespace sza {
  namespace util {
    
    class YigBits {
    public:
      
      enum {
	UNLOCKED  = 0x1,
	LOCKED    = 0x2,
      };

    }; // End class YigBits
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_YIGBITS_H
