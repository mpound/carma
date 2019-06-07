#ifndef SZA_UTIL_REGRANGE_H
#define SZA_UTIL_REGRANGE_H

/**
 * @file RegRange.h
 * 
 * Tagged: Sun Sep 26 14:35:48 UTC 2004
 * 
 * @author 
 */
#include "carma/szautil/ArrayMapBase.h"

#include "carma/szaarrayutils/szaregs.h"

#include <iostream>

namespace sza {
  namespace util {
    
    /**
     * A class for managing ranges of byte indices corresponding to
     * registers in a register map.
     */
    class RegRange {
    public:
      
      /**
       * Constructor.
       */
      RegRange(unsigned iByteStart, unsigned iByteStop, 
	       bool archivedOnly=false, ArrayMap* arrayMap=0);
      
      /**
       * Destructor.
       */
      virtual ~RegRange();

      /**
       * Prefix increment operator.  Increments to the next element
       * which is part of the (archived or whole) array map.
       */
      const RegRange& operator++();
      
      /**
       * Increments to the next register element, regardless of
       * whether or not it belongs to the version of the array map
       * managed by this class.
       */
      void increment();

      /**
       * Reset all iterators
       */
      void reset();

      /**
       * Return true if we are at the end of our range.
       */
      bool isEnd();

      /**
       * Allows cout << RegRange
       */
      friend std::ostream& operator<<(std::ostream& os, RegRange& range);

      /**
       * Get a pointer to the current register map
       */
      ArrRegMap* currentArrRegMap();

      /**
       * Get a pointer to the current block
       */
      RegMapBlock* currentBlock();

      /**
       * Get the current element
       */
      unsigned currentEl();

      /**
       * Get the current byte offset
       */
      unsigned currentByteOffset();

      /**
       * Get the current slot in the register map.  This is a symbolic
       * index into a consecutive array of all register elements.
       */
      int currentSlot();

    private:

      ArrayMap* arrayMap_;       // The SZA array map 
      ArrayMapBase arrayMapBase_;// The SZA array map 
      
      bool archivedOnly_;    // True if this frame should
			     // contain only archived registers

      unsigned iByteStart_;  // The starting byte into the arraymap
      unsigned iByteStop_;   // The ending byte into the arraymap
      unsigned iByteCurrent_;// The index of the current byte
      unsigned iSlotCurrent_;// The index of the current slot

      // Iterators for looping through the arraymap

      std::vector<ArrRegMap*>::iterator iregmap_;
      std::vector<RegMapBoard*>::iterator iboard_;
      std::vector<RegMapBlock*>::iterator iblock_;
      int iEl_;

      /**
       * Check the validity of the current element.
       */
      void checkValidity();	

      /**
       * Check the validity of the current register
       */
      bool currentRegisterIsValid();

    }; // End class RegRange
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_REGRANGE_H
