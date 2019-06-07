#ifndef SZA_UTIL_REGAXES_H
#define SZA_UTIL_REGAXES_H

/**
 * @file RegAxes.h
 * 
 * Tagged: Thu Jun 24 17:19:46 UTC 2004
 * 
 * @author 
 */
#include <vector>

#include "carma/szautil/RegCoord.h"

namespace sza {
  namespace util {
    
    /**
     * Class for managing coordinate axes.
     *
     * NB: all axis specifiers should be indexed from 0
     */
    class RegAxes { 
    public:
      
      /**
       * Constructors.
       */
      RegAxes();
      RegAxes(unsigned nel0);
      RegAxes(unsigned nel0, unsigned nel1);

      /**
       * Copy constructor
       */
      RegAxes(RegAxes* regAxes);
      
      /**
       * Method for setting arbitrary axes
       */
      void setAxis(unsigned nAxis, unsigned nEl);

      /**
       * Destructor.
       */
      virtual ~RegAxes();
      
      /**
       * Return the number of axes
       */
      unsigned int nAxis();

      /**
       * Return the number of elements
       */
      unsigned int nEl(int axis=-1);

      /**
       * Return the element offset of the specified coordinate from the
       * start of the register array
       */
      unsigned int elementOffsetOf(RegCoord& coord);

      /**
       * Return the element offset of the specified coordinate from the
       * start of the register array
       */
      unsigned int elementOffsetOf(RegCoord coord);

      /**
       * Return the element offset of the specified coordinate from the
       * start of the register array
       */
      unsigned int refElementOffsetOf(RegCoord& coord);

    private:

      std::vector<unsigned int> nel_;

      /**
       * Constructors.
       */
      void privateConstructor();

      /**
       * Return the requested index and check validity
       */
      unsigned int getIndex(RegCoord& coord, unsigned iAxis);
	
    }; // End class RegAxes
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_REGAXES_H
