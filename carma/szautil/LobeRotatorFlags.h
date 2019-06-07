#ifndef SZA_UTIL_LOBEROTATORFLAGS_H
#define SZA_UTIL_LOBEROTATORFLAGS_H

/**
 * @file LobeRotatorFlags.h
 * 
 * Tagged: Tue Jun 21 17:02:26 PDT 2005
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class LobeRotatorFlags {
    public:
      
      enum {
	WALSHING_OFF     = 0x1,
	WALSHING_ON      = 0x2,
	WALSHING_UNKNOWN = 0x4,
      };

      enum {
	PHASE_UNRCVD   = 0x1,
	PHASE_RCVD     = 0x2,
	PHASE_UNKNOWN  = 0x4,
      };

      enum {
	COL_VALID         =  0x1,
	COL_DOWNLOADING   =  0x2,
	COL_BAD_CRC       =  0x4,
	COL_TIMED_OUT     =  0x8,
	COL_UNKNOWN       = 0x10,
      };

    }; // End class LobeRotatorFlags
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_LOBEROTATORFLAGS_H
