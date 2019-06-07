#ifndef SZA_UTIL_COORD_H
#define SZA_UTIL_COORD_H

/**
 * @file Coord.h
 * 
 * Tagged: Thu Jun 24 17:38:28 UTC 2004
 * 
 * @author 
 */
#include <vector>
#include <iostream>

namespace sza {
  namespace util {
    
    /**
     * A class for specifying a coordinate in a multi-dimensional
     * space.  Supports up to three dimensions at the moment.
     */
    class Coord {
    public:
      
      /**
       * Constructors.
       */
      Coord();
      Coord(Coord* coord);
      Coord(unsigned ind1);
      Coord(unsigned ind1, unsigned ind2);
      Coord(unsigned ind1, unsigned ind2, unsigned ind3);
      
      /**
       * Destructor.
       */
      virtual ~Coord();
      
      /**
       * Set the coordinate index of the requested axis
       */
      void setIndex(unsigned nAxis, unsigned index);

      /**
       * Reserve (but don't set) an index slot for the requested axis
       */
      void reserveIndex(unsigned nAxis);

      /**
       * Reset the coordinate ntuplet
       */
      void reset(unsigned nAxis=1);

      /**
       * Return the number of axes in this coordinate nTuplet.
       */
      unsigned nAxis();

      /**
       * Return the coordinate index for axis iAxis
       */
      unsigned int getIndex(unsigned iAxis);

      /**
       * Return true if a coordinate is set for this axis
       */
      bool isSet(unsigned iAxis);

      /**
       * Check if this coordinate contains valid data
       */
      bool isValid();
      
      /**
       * Assignment operators
       */
      void operator=(Coord& coord);
      void operator=(Coord coord);

      /**
       * An operator for printing this object
       */
      friend std::ostream& operator<<(std::ostream& os, Coord& coord);

      /**
       * Add an increment to this object
       */
      Coord& operator+=(unsigned incr);

      /**
       * Add an increment to this object
       */
      bool operator==(Coord& coord);

    private:

      std::vector<unsigned int> ind_;
      std::vector<bool> initialized_;

    }; // End class Coord
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_COORD_H
