#ifndef SZA_UTIL_REGCOORD_H
#define SZA_UTIL_REGCOORD_H

/**
 * @file RegCoord.h
 * 
 * Tagged: Thu Jun 24 17:38:28 UTC 2004
 * 
 * @author 
 */
#include <vector>

namespace sza {
  namespace util {
    
    /**
     * A class for specifying a coordinate in a multi-dimensional
     * space.  Supports up to three dimensions at the moment.
     */
    class RegCoord {
    public:
      
      /**
       * Constructors.
       */
      RegCoord();
      RegCoord(RegCoord* coord);
      RegCoord(unsigned ind1);
      RegCoord(unsigned ind1, unsigned ind2);
      RegCoord(unsigned ind1, unsigned ind2, unsigned ind3);
      
      /**
       * Destructor.
       */
      virtual ~RegCoord();
      
      /**
       * Set the coordinate index of the requested axis
       */
      void setIndex(unsigned nAxis, unsigned index);

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

    private:

      std::vector<unsigned int> ind_;

    }; // End class RegCoord
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_REGCOORD_H
