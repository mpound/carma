#ifndef SZA_UTIL_QUADMODFLAGS_H
#define SZA_UTIL_QUADMODFLAGS_H

/**
 * @file QuadModFlags.h
 * 
 * Tagged: Fri May 20 17:37:57 PDT 2005
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class QuadModFlags {
    public:
      
      enum {
	DISABLED     = 0x1,
	ENABLED      = 0x2,
	UNKNOWN      = 0x4,
      };

    }; // End class QuadModFlags
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_QUADMODFLAGS_H
