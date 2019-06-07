#ifndef SZA_UTIL_YIGFLAGS_H
#define SZA_UTIL_YIGFLAGS_H

/**
 * @file YigFlags.h
 * 
 * Tagged: Wed Mar 23 10:46:19 PST 2005
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class YigFlags {
    public:
      
      enum {
	UNLOCKED  = 0x1,
	SEARCHING = 0x2,
	REFINING  = 0x4,
	LOCKED    = 0x8,
	RELOCK    = 0x10
      };
    }; // End class YigFlags
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_YIGFLAGS_H
