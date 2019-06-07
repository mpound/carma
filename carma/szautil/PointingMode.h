#ifndef SZA_UTIL_POINTINGMODE_H
#define SZA_UTIL_POINTINGMODE_H

/**
 * @file PointingMode.h
 * 
 * Tagged: Thu Nov 13 16:53:47 UTC 2003
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    
    /**
     * Define a class to specify a pointing mode
     */
    class PointingMode {
      
    public:
      
      /**
       * Enumerate possible pointing modes.
       */
      enum Type {
	OPTICAL,
	RADIO,
	CURRENT
      };
      
    }; // End class PointingMode
    
  }; // End namespace util
}; // End namespace sza

#endif // End #ifndef 
