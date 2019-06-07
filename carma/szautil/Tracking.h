#ifndef SZA_UTIL_TRACKING_H
#define SZA_UTIL_TRACKING_H

/**
 * @file Tracking.h
 * 
 * Started: Wed Dec 17 19:50:04 UTC 2003
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    /**
     * Class to enumerate the current telescope tracking type.
     */
    class Tracking {
    public:
      
      enum Type {
	TRACK_NONE  = 0x0,
	TRACK_POINT = 0x2,
	TRACK_PHASE = 0x4,
	TRACK_BOTH  = TRACK_POINT | TRACK_PHASE	
      };

    }; // End class Tracking
    
  }; // End namespace util
}; // End namespace sza

#endif
