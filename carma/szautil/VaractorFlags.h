#ifndef SZA_UTIL_VARACTORFLAGS_H
#define SZA_UTIL_VARACTORFLAGS_H

/**
 * @file VaractorFlags.h
 * 
 * Tagged: Wed Mar 23 10:56:19 PST 2005
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class VaractorFlags {
    public:
      
      enum {
	UNLOCKED  = 0x1,
	LOCKED    = 0x2,
	RF_BAD    = 0x4,
	RF_GOOD   = 0x8,

	SWEEP_OFF = 0x10,
	SWEEP_ON  = 0x20,
	GUNN_OFF  = 0x40,
	GUNN_ON   = 0x80,
      };

      enum {
	IFMON_BAD    = 0x1,
	IFMON_GOOD   = 0x2,
	DATA_INVALID = 0x4,
	DATA_VALID   = 0x8,
      };

    }; // End class VaractorFlags
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_VARACTORFLAGS_H
