#ifndef SZA_UTIL_COORDAXES_H
#define SZA_UTIL_COORDAXES_H

/**
 * @file CoordAxes.h
 * 
 * Tagged: Thu Jun 24 17:19:46 UTC 2004
 * 
 * @author 
 */
#include <vector>

#include "carma/szautil/Coord.h"
#include "carma/szautil/CoordRange.h"
#include "carma/szautil/Range.h"

namespace sza {
  namespace util {
    
    /**
     * Class for managing coordinate axes.
     *
     * NB: All axis specifiers are indexed starting from 0
     */
    class CoordAxes { 
    public:
      
      /**
       * Constructors.
       */
      CoordAxes();
      CoordAxes(unsigned nel0);
      CoordAxes(unsigned nel0, unsigned nel1);
      CoordAxes(unsigned nel0, unsigned nel1, unsigned nel2);

      /**
       * Copy constructor
       */
      CoordAxes(CoordAxes* regAxes);
      
      /**
       * Reset this object
       */
      void reset();

      /**
       * Method for setting the number of elements on an arbitrary
       * axis
       */
      void setAxis(unsigned nAxis, unsigned nEl);

      /**
       * Destructor.
       */
      virtual ~CoordAxes();
      
      /**
       * Return the number of axes in this axis description
       */
      unsigned int nAxis();

      /**
       * Return the number of elements in an axis.
       *
       * If axis < 0, return the total number of elements in this
       * descriptor.  Else return the number of elements in the
       * requested axis.
       */
      unsigned int nEl(int axis=-1);

      /**
       * An N-dimensional axis description can be thought of as
       * indexing a contiguous one-dimensional array.
       * 
       * This method returns the offset, in elements, of the specified
       * multi-dimensional coordinate from the beginning of the
       * hypothetical 1-D array corresponding to this axis descriptor.
       */
      unsigned int elementOffsetOf(Coord& coord);
      unsigned int elementOffsetOf(Coord* coord);

      /**
       * An N-dimensional axis description can be thought of as
       * indexing a contiguous one-dimensional array.
       * 
       * This method returns the multi-dimensional coordinate
       * corresponding to the requested element offset in that
       * hypothetical 1-D array.
       */
      Coord coordOf(unsigned element);

      /**
       * CoordRange objects can specify a different range of indices
       * in each axis.  In terms of the corresponding element offset
       * in a hypothetical 1-D array corresponding to this
       * multi-dimensional axis description, these may translate into
       * a single contiguous element range, or a series of
       * non-contiguous ranges.
       *
       * This method takes a CoordRange object and returns a vector of
       * contiguous element ranges in the 1-D array.
       *
       * Neither one of these methods modifies the input.  The second
       * is provided so that a NULL range can be passed to mean "all"
       */
      std::vector<Range<unsigned> > getRanges(CoordRange range);
      std::vector<Range<unsigned> > getRanges(CoordRange* range=0);

      /**
       * Check if a coordinate range is consistent with this axis
       * specifier
       */
      bool rangeIsValid(CoordRange& range);
      bool rangeIsValid(CoordRange* range);

      /**
       * Return the total number of elements specified in a range object
       */
      unsigned nEl(CoordRange& range);
      unsigned nEl(CoordRange* range);

      /**
       * An operator for testing equality of two Axes
       */
      bool operator==(CoordAxes& axes);

      /**
       * An operator for printing this object
       */
      friend std::ostream& operator<<(std::ostream& os, CoordAxes axes);

      /**
       *  This method fills out any missing dimensions in the range object
       *  with the full range for that axis.
       */
      void fillRange(CoordRange& range);


    private:

      // The vector in which we store the number of elements per axis

      std::vector<unsigned int> nEl_;

      /**
       * Constructors.
       */
      void privateConstructor();

      /**
       * Return the requested index and check validity
       */
      void checkValidityOf(Coord& coord);

      /*
       * Find a vector element matching the stop index
       */
      std::vector<Range<unsigned> >::iterator 
	findStopIndex(std::vector<Range<unsigned> >& ranges, unsigned index);
      
      /**
       * Recursive method for computing multidimensional ranges.
       */
      void computeRanges(std::vector<Range<unsigned> >& ranges, unsigned iAxis, 
			 Coord& start, Coord& stop, unsigned offset);
	
      /**
       * Add a new range 
       */
      void addNewRange(std::vector<Range<unsigned> >& ranges, 
		       unsigned startIndex, unsigned stopIndex);

    }; // End class CoordAxes
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_COORDAXES_H
