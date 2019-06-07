#ifndef SZA_UTIL_CALTERTFLAGS_H
#define SZA_UTIL_CALTERTFLAGS_H

/**
 * @file CalTertNewFlags.h
 * 
 * Tagged: Wed Mar 23 11:01:47 PST 2005
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class CalTertNewFlags {
    public:

      enum {
	IDLE            = 0x0,
	HOMING          = 0x1,
	HOME            = 0x2,
	HOME_ERROR      = 0x4,
	MOVING          = 0x8,
	RX1CM_SELECTED  = 0x10,
	RX3MM_SELECTED  = 0x20,
	RX1MM_SELECTED  = 0x40,
	MANUAL_POSITION = 0x80,
	STUCK           = 0x100,
    };
      
    }; // End class CalTertNewFlags
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_CALTERTFLAGS_H
