#ifndef SZA_UTIL_TRACKERFLAGS_H
#define SZA_UTIL_TRACKERFLAGS_H

/**
 * @file TrackerFlags.h
 * 
 * Tagged: Wed Mar 23 11:30:53 PST 2005
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class TrackerFlags {
    public:
      
      enum {
	LACKING    = 0x1,
	TIME_ERROR = 0x2,
	UPDATING   = 0x4,
	HALT       = 0x8,
	SLEW       = 0x10,
	TRACK      = 0x20,
	TOO_LOW    = 0x40,
	TOO_HIGH   = 0x80
      };

      enum {
	OFF_SOURCE_FALSE  = 0x1,
	OFF_SOURCE_TRUE   = 0x2,
      };

    }; // End class TrackerFlags
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_TRACKERFLAGS_H
