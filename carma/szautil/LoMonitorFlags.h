#ifndef SZA_UTIL_LOMONITORFLAGS_H
#define SZA_UTIL_LOMONITORFLAGS_H

/**
 * @file LoMonitorFlags.h
 * 
 * Tagged: Mon Jun 20 16:46:28 PDT 2005
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class LoMonitorFlags {
    public:
      
      static const unsigned nLo_ = 5;

      enum {
	LO_STATE_UNKNOWN  = 0x0,
	LO_STATE_UNLOCKED = 0x1,
	LO_STATE_LOCKED   = 0x2,
      };

      enum {
	LO_ERR_UNKNOWN  = 0x0,
	LO_ERR_OK       = 0x1,
	LO_ERR_FREQ     = 0x2,
	LO_ERR_POWER    = 0x4,
	LO_ERR_BOTH     = 0x8,
	LO_ERR_NO_SIGNAL= 0x10,
      };
      
    }; // End class LoMonitorFlags
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_LOMONITORFLAGS_H
