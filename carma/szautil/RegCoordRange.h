#ifndef SZA_UTIL_REGCOORDRANGE_H
#define SZA_UTIL_REGCOORDRANGE_H

/**
 * @file RegCoordRange.h
 * 
 * Tagged: Sat Oct  2 00:14:18 UTC 2004
 * 
 * @author 
 */
#include "carma/szautil/CoordRange.h"
#include "carma/szautil/Range.h"
#include "carma/szautil/RegDescription.h"

#include <vector>

namespace sza {
  namespace util {
    
    /**
     * A class for iterating over slot ranges specified in a
     * CoordRange object
     */
    class RegCoordRange {
    public:
      
      /**
       * Constructor.
       */
      RegCoordRange(RegDescription& reg, CoordRange& range);
      
      /**
       * Destructor.
       */
      virtual ~RegCoordRange();
      
      /**
       * Return the current element
       */
      inline unsigned currentElement() {
	return iElCurrent_;
      }

      inline unsigned currentSlot() {
	return iElCurrent_ + iSlotOffset_;
      }

      /**
       * Prefix increment operator
       */
      const RegCoordRange& operator++();
      
      /**
       * Reset all iterators
       */
      void reset();

      /**
       * Return true if we are at the end of our range.
       */
      bool isEnd();

      /**
       * Allows cout << RegCoordRange
       */
      friend std::ostream& operator<<(std::ostream& os, RegCoordRange& range);

    private:

      int iSlotOffset_;
      unsigned iElCurrent_;
      std::vector<Range<unsigned> > ranges_;
      std::vector<Range<unsigned> >::iterator iRange_;

    }; // End class RegCoordRange
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_REGCOORDRANGE_H
