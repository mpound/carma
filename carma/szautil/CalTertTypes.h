#ifndef SZA_UTIL_CALTERTTYPES_H
#define SZA_UTIL_CALTERTTYPES_H

/**
 * @file CalTertTypes.h
 * 
 * Tagged: Tue Aug 24 08:51:16 PDT 2004
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class CalTertTypes {
    public:
      
      // Enumerate valid one-wire devices
      
      enum OwDevice {
	MODULE   = 0,
	ENCODER  = 1,
	DEV_NONE = 2,
      };
      
      // Enumerate valid one-wire device commands
      
      enum OwCommand {
	READ     = 0,
	WRITE    = 1,
	CMD_NONE = 2,
      };

    }; // End class CalTertTypes
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_CALTERTTYPES_H
