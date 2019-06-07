#ifndef SZA_UTIL_AXISRANGE_H
#define SZA_UTIL_AXISRANGE_H

/**
 * @file AxisRange.h
 * 
 * Tagged: Tue Oct  5 13:18:11 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/CoordAxes.h"
#include "carma/szautil/CoordRange.h"

#include "carma/szaarrayutils/szaregs.h"

namespace sza {
  namespace util {
    
    class AxisRange {
    public:
      
      /**
       * Constructor.
       */
      AxisRange(CoordAxes axes, CoordRange range);
      AxisRange(CoordAxes& axes, CoordRange* range=0);
      AxisRange(CoordAxes* axes, CoordRange* range=0);
      AxisRange(RegMapBlock* block, CoordRange* range=0);

      /**
       * Constructor for a simplistic axis conisting of nEl
       * consecutive elements
       */
      AxisRange(unsigned nEl);
      AxisRange();

      /**
       * Set the axis range
       */
      void setTo(CoordAxes* axes, CoordRange* range);
      void setToDc(CoordAxes* axes, CoordRange* range);
      void setTo(unsigned nEl);

      /**
       * Destructor.
       */
      virtual ~AxisRange();

      /**
       * Return the current element
       */
      inline unsigned currentElement() {
	return iElCurrent_;
      }

      /**
       * Return the current iterator
       */
      inline unsigned currentIterator() {
	return iter_;
      }

      /**
       * Prefix increment operator
       */
      const AxisRange& operator++();
      
      /**
       * Reset all iterators
       */
      void reset();

      /**
       * Return true if we are at the end of our range.
       */
      bool isEnd();

      /**
       * Allows cout << AxisRange
       */
      friend std::ostream& operator<<(std::ostream& os, AxisRange& range);
      
      /**
       * Return the current coordinate
       */
      Coord currentCoord();

      unsigned nEl();

    private:

      unsigned iter_;
      unsigned iElCurrent_;
      std::vector<Range<unsigned> > ranges_;
      std::vector<Range<unsigned> >::iterator iRange_;
      CoordAxes axes_;
      unsigned nEl_;

    }; // End class AxisRange
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_AXISRANGE_H
