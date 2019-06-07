#ifndef DATE_H
#define DATE_H

/**
 * @file Date.h
 * 
 * Tagged: Thu Nov 13 16:53:36 UTC 2003
 * 
 * @author Erik Leitch
 */
// Include the C-style struct and method definitions this class is
// based on

#include "carma/szaarrayutils/astrom.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * Class to encapsulate a date.  This class is a wrapper around
       * a DASI-style Date struct which we need to call various
       * sza::array methods.
       */
      class Date {
	
      public:
	
	/**
	 * Constructor function just intializes the date fields by calling
	 * reset(), below.
	 */
	Date();
	
	/**
	 * Intialize the date fields to zero
	 */
	void reset();
	
	/**
	 * Convert from MJD to broken-down calendar date.
	 *
	 * @throws Exception
	 */
	void convertMjdUtcToDate(double utc);
	
	/**
	 * Return the year.
	 */
	int getYear();
	
      private:
	
	/**
	 * The DASI-style Date struct we are managing.
	 */
	sza::array::Date date_;
	
      }; // End class Date
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
