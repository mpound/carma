#ifndef SZA_UTIL_CALTERTFLAGS_H
#define SZA_UTIL_CALTERTFLAGS_H

/**
 * @file CalTertFlags.h
 * 
 * Tagged: Wed Mar 23 11:01:47 PST 2005
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class CalTertFlags {
    public:

      enum {
	IN_POSITION    = 0x1,
	MOVING         = 0x2,
	HOMING         = 0x4,
	STOPPED        = 0x8,
	POS_SOFT_LIMIT = 0x10,
	NEG_SOFT_LIMIT = 0x20,
	HARD_LIMIT     = 0x40,
	ERROR          = 0x80 
    };
      
    }; // End class CalTertFlags
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_CALTERTFLAGS_H
