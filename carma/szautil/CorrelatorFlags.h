#ifndef SZA_UTIL_CORRELATORFLAGS_H
#define SZA_UTIL_CORRELATORFLAGS_H

/**
 * @file CorrelatorFlags.h
 * 
 * Tagged: Mon May 23 12:47:49 PDT 2005
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class CorrelatorFlags {
    public:
      
      enum {
	MIXED = 0x1,
	NOISE = 0x2,
	RF    = 0x4,
      };

    }; // End class CorrelatorFlags
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_CORRELATORFLAGS_H
