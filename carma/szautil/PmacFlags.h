#ifndef SZA_UTIL_PMACFLAGS_H
#define SZA_UTIL_PMACFLAGS_H

/**
 * @file PmacFlags.h
 * 
 * Tagged: Wed Mar 23 11:05:42 PST 2005
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class PmacFlags {
    public:

      enum {
        STOPPED             = 0x1,
	RUNNING             = 0x2,
        PROGRAM_OK          = 0x4,
        PROGRAM_ERROR       = 0x8,
        SOURCE_NOT_ACQUIRED = 0x10,
        SOURCE_ACQUIRED     = 0x20,
        STABLE              = 0x40,
	UNSTABLE            = 0x80,
      };

    }; // End class PmacFlags
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_PMACFLAGS_H
