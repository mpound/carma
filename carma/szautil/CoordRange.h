#ifndef SZA_UTIL_COORDRANGE_H
#define SZA_UTIL_COORDRANGE_H

/**
 * @file CoordRange.h
 * 
 * Tagged: Fri Sep 17 15:51:07 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Coord.h"

#include <iostream>

namespace sza {
  namespace util {
    
    // Class for managing a range of a coordinate axis

    class CoordRange {
    public:
      
      /**
       * Constructor.
       */
      CoordRange();
      CoordRange(unsigned index);
      CoordRange(unsigned iStart, unsigned iStop);
      CoordRange(Coord& start, Coord& stop);
      CoordRange(Coord& coord);
      CoordRange(CoordRange* coordRange);
      
      void initialize();

      /**
       * Destructor.
       */
      virtual ~CoordRange();
      
      /**
       * Set/get the index of the first element in the range
       */
      void setStartIndex(unsigned iAxis, unsigned iStart);
      unsigned startIndex(unsigned iAxis);

      // Set the start coordinate

      void setStartCoord(Coord& startCoord);
      void setStartCoord(Coord* startCoord);

      // Get the start coord

      Coord startCoord();

      void setStopIndex(unsigned iAxis, unsigned iStop);
      unsigned stopIndex(unsigned iAxis);

      // Set both the stop and start to the same index

      void setIndex(unsigned iAxis, unsigned index);

      // Same as above, but defaults to axis 0

      void setIndex(unsigned index);

      // Set the stop coordinate

      void setStopCoord(Coord& stopCoord);
      void setStopCoord(Coord* stopCoord);

      // Get the stop coord

      Coord stopCoord();

      void setCoord(Coord coord);

      /**
       * Get the number of elements in this axis
       */
      unsigned nEl(unsigned iAxis);

      /**
       * Get the number of axes specified
       */
      unsigned nAxis();

      /**
       * Return true if this coordinate range contains valid data
       */
      bool isValid();

      /**
       * Set whether or not this range is to be interpreted as a
       * contiguous range of indices from startCoord_ to stopCoord_,
       * or disjunct index ranges for the separate axes.
       *
       * For example, if this object specified an index into a 2-D
       * array, with 4 elements in the first axis, and 3 elements in
       * the second axis, the range (0, 1) - (3, 2) could be
       * inberpreted either as:
       *
       *  [0-3][1-2]      ->  [0][1-2] and [1][1-2] and [2][1-2] and [3][1-2]
       *                      (i.e., 8 elements) 
       * or
       *
       *  [0][1] - [3][2] ->  [0][1] to [3][2]
       *                      (i.e., 14 elements)
       *
       */
      void setContiguous(bool contiguous);
      bool isContiguous();

      /**
       * An operator for printing this object
       */
      friend std::ostream& operator<<(std::ostream& os, CoordRange range);

      /**
       * An operator for incrementing this object
       */
      CoordRange& operator+=(unsigned incr);

      /**
       * An operator for incrementing this object
       */
      bool operator==(CoordRange& range);

      // Return true if this range contains the passed range

      bool contains(CoordRange& range);

    private:

      Coord startCoord_;
      Coord stopCoord_;

      bool contiguous_;

    }; // End class CoordRange
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_COORDRANGE_H
