#ifndef SZA_UTIL_NOISESOURCEFLAGS_H
#define SZA_UTIL_NOISESOURCEFLAGS_H

/**
 * @file NoiseSourceFlags.h
 * 
 * Tagged: Fri May 20 14:32:15 PDT 2005
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class NoiseSourceFlags {
    public:
      
      enum {
	DISABLED = 0x1,
	ENABLED  = 0x2,
	UNKNOWN  = 0x4,
      };

    }; // End class NoiseSourceFlags
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_NOISESOURCEFLAGS_H
