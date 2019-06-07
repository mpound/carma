#ifndef SZA_UTIL_REGDESCRIPTION_H
#define SZA_UTIL_REGDESCRIPTION_H

/**
 * @file RegDescription.h
 * 
 * Tagged: Mon Sep 27 21:37:46 UTC 2004
 * 
 * @author 
 */
#include "carma/szautil/ArrayMapBase.h"
#include "carma/szautil/CoordAxes.h"
#include "carma/szautil/CoordRange.h"
#include "carma/szautil/Range.h"
#include "carma/szautil/RegAxisRange.h"

#include "carma/szaarrayutils/szaregs.h"

#include <iostream>

namespace sza {
  namespace util {
    
    class RegParser;

    class RegDescription {
    public:
      
      /**
       * Constructor.
       */
      RegDescription(bool archivedOnly_=false, ArrayMap* arrayMap=0);

      /**
       * Constructor.
       */
      RegDescription(bool archivedOnly_, RegMapBlock* block);
      
      /**
       * Copy constructor
       */
      RegDescription(RegDescription& desc);
      RegDescription(const RegDescription& desc);
      
      /**
       * Assignment operator
       */
      void operator=(RegDescription& desc);
      void operator=(const RegDescription& desc);

      /**
       * Destructor.
       */
      virtual ~RegDescription();

      /**
       * Initialize class members
       */
      void initialize();

      /**
       * Reset members which should not be reused
       */
      void reset();

      /**
       * Fill in with details of the specified register, with starting
       * element specified by coord, and number of elements from the
       * start given by n.
       */
      void setTo(std::string regmap_name, 
		 std::string board_name, 
		 std::string block_name,
		 RegAspect aspect,
		 RegInteg integ,
		 Coord* coord=0, 
		 unsigned n=0);

      void setTo(std::string regmap_name, 
		 std::string board_name, 
		 std::string block_name,
		 RegAspect aspect,
		 RegInteg integ,
		 CoordRange& coordRange);

      /**
       * Set a new coordinate for this description
       */
      void setCoord(Coord& coord);

      /**
       * Set the output mode
       */
      void setOutputMode(RegOutputMode mode);

      // A method for printing this object's register specification on
      // an output stream

      friend std::ostream& operator<<(std::ostream& os, 
						 RegDescription& desc);

      /**
       * Old-style: write this register onto an output stream
       */
      void output(OutputStream* stream, RegOutputMode mode);

      /**
       * Check if a range is consistent with the axes in this register
       */
      bool rangeIsValid(CoordRange& range);
      bool rangeIsValid(CoordRange* range);

      /**
       * Return the total number of elements covered by the register in this
       * object
       */
      unsigned nEl(CoordRange* range=0);
      unsigned nByte(CoordRange* range=0);

      /**
       * Return the number of elements in the fastest-changing
       * dimension of this coordinate
       */
      unsigned fastestNel();

      /**
       * Return the range
       */
      CoordRange range() {
	return range_;
      }

      /**
       * Return the range
       */
      CoordRange* getRangePtr() {
	return &range_;
      }

      /**
       * Return the axes
       */
      CoordAxes axes() {
	return axes_;
      }

      /**
       * Return a vector of element ranges associated with this register
       * description
       */
      std::vector<Range<unsigned> > getElementRanges(CoordRange* range=0);

      /**
       * Return a vector of slot ranges in the array map associated
       * with this register description
       */
      std::vector<Range<unsigned> > getSlotRanges(CoordRange* range=0);

      /**
       * Return a vector of byte ranges in the array map associated
       * with this register description
       */
      std::vector<Range<unsigned> > getByteRanges(CoordRange* range=0);

      /**
       * Return the total number of slots covered by this register
       */
      unsigned nSlot(CoordRange* range=0);

      /**
       * Return the start/stop slot in the array map corresponding to
       * the passed range, or to the range in this descriptor if
       * range==0
       */
      int startSlot(CoordRange* range=0);
      int stopSlot(CoordRange* range=0);

      /**
       * Return the slot offset of this register in the array map
       */
      int iSlot() {
	return iSlot_;
      }

      int iRegMap() {
	return iRegMap_;
      }

      int iBoard() {
	return iBoard_;
      }

      int iBlock() {
	return iBlock_;
      }

      /**.......................................................................
       * Return the regmap descriptor associated with this register
       */
      ArrRegMap* regMap();

      RegAspect aspect() {
	return aspect_;
      }

      std::string aspectName();

      void setAspect(RegAspect aspect) {
	aspect_ = aspect;
      }
   
      RegInteg integ() {
	return integ_;
      }

      std::string integName();

      void setInteg(RegInteg integ) {
	integ_ = integ;
      }

      inline ArrayMap* arrayMap() {
	return arrayMap_;
      }

      RegMapBlock* block();

      std::string blockName();
      std::string boardName();
      std::string regMapName();

      /**
       * Begin an iteration on this registers selected indices
       */
      void begin();

      /**
       * Check if we are at the end of our iteration
       */
      bool isEnd();

      /**
       * Prefix increment operator
       */
      const RegDescription& operator++();
      
      /**
       * Return the archive slot corresponding to the current
       * element
       */
      unsigned currentSlot();

      /**
       * Return the current element
       */
      unsigned currentElement();

      /**
       * Return the coordinate corresponding to the current element
       */
      Coord currentCoord();

      // True if the selected range of elements in this descriptor
      // contains the range in the passed descriptor

      bool contains(RegDescription& desc);

      inline long oldestVisible() {
	return oldestVisible_;
      }

      inline bool isFirst() {
	return first_;
      }

      void setFirst(bool first);
      void setOldestVisible(long oldest);
      void resetOldestVisible();
      void decreaseOldestVisible(long oldest);
      void increaseOldestVisible(long oldest);

      CoordAxes  axes_;    // An axis specifier for this register

    private:

      friend class RegParser;

      // A static array map used by all objects of this type

      ArrayMapBase arrayMapBase_;
      ArrayMap* arrayMap_;

      // Per-object members

      bool archivedOnly_; // True if this class is to manage only
			  // archived registers

      RegOutputMode outputMode_; // A specification for how this
				 // register description should be
				 // written on output

      int iRegMap_;        // The index of the specified register map in the
		           // array map

      int iBoard_;         // The index of the specified board in the
		           // register-map

      int iBlock_;         // The index of the specified block on the
			   // regmap board

      int iSlot_;          // The frame index of the first element of
			   // this register.
                           //
			   // NB: This is the frame offset of the
			   // _first_ element of this register, and
			   // not the offset of the first requested
			   // element (i.e., of range_.startCoord())

      unsigned nEl_;       // The total number of elements requested

      unsigned size_;      // The number of slots per register. This
			   // is 1 unless aspect!=REG_PLAIN, in which
			   // case it becomes 2
      RegAspect aspect_;   // The quantity to derive from complex or
			   // utc register pairs

      RegInteg integ_;     // Integration status of this register

      int iByte_;          // The byte index into the array map of the
			   // first element of the requested register.
                           //
			   // NB: This is the offset of the _first_
			   // element of this register, and not the
			   // offset of the first requested element
			   // (i.e., of range_.startCoord())
      
      unsigned nBytePerEl_;// The number of bytes per element of this
			   // register
      
      CoordRange range_;   // An index-range specifier

      RegAxisRange* axisRange_; // An axis-range for iterating
      
      // This is just a utility member for use in plotting interface.

      long oldestVisible_;
      bool first_;

    }; // End class RegDescription
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_REGDESCRIPTION_H
