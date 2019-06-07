#ifndef SZA_UTIL_IFLEVEL_H
#define SZA_UTIL_IFLEVEL_H

/**
 * @file IFLevel.h
 * 
 * Tagged: Wed Aug 18 06:36:58 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <cmath>

// Max power is +8 dBm according to Alberto's CAN API

#define dBmMax_ 8.0


namespace sza {
  namespace util {
    
    /**
     * A class for managing antenna IF levels
     */
    class IFLevel {
    public:

      /**
       * Return true if the passed level is valid
       */
      static bool isValidLevel(double level)
	{
	  // Max power is +8 dBm according to Alberto's CAN API

	  return !(level < 0 || level > pow(10, dBmMax_));
	}

      /**
       * Return the maximum power level
       */
      static double maxLevel()
	{
	  return pow(10, dBmMax_);
	}

    }; // End class IFLevel
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_IFLEVEL_H
