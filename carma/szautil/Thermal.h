#ifndef SZA_UTIL_THERMAL_H
#define SZA_UTIL_THERMAL_H

/**
 * @file Thermal.h
 * 
 * Tagged: Fri Oct 29 04:25:53 PDT 2004
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class Thermal {
    public:
      
      // Enumerate valid Rbox modes
      
      enum BoxMode {
	ON     = 0x0,
	OFF    = 0x1,
	MANUAL = 0x2
      };
      
      // Enumerate valid cooling device targets
      
      enum Target {
	NONE = 0x0,
	EBOX = 0x1,
	RBOX = 0x2,
	CIRC = 0x4,
	ALL  = EBOX|RBOX|CIRC	  
      };
      
    }; // End class Thermal
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_THERMAL_H
