#ifndef SZA_UTIL_TONESOURCEFLAGS_H
#define SZA_UTIL_TONESOURCEFLAGS_H

/**
 * @file ToneSourceFlags.h
 * 
 * Tagged: Fri May 20 14:32:15 PDT 2005
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class ToneSourceFlags {
    public:
      
      enum {
	OFF             = 0x1,
	ON              = 0x2,
	UNKNOWN         = 0x4,
      };

    }; // End class ToneSourceFlags
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_TONESOURCEFLAGS_H
