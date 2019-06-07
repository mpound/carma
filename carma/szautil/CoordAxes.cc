#include "carma/szautil/CoordAxes.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

using namespace sza::util;
using namespace std;

/**.......................................................................
 * Private initialization method
 */
void CoordAxes::privateConstructor() 
{
  // Default to 1-dimensions
  
  nEl_.resize(1);
}
/**.......................................................................
 * Constructor.
 */
CoordAxes::CoordAxes() 
{
  privateConstructor();
  setAxis(0, 1); // Set the default axis to have 1 element
}

/**.......................................................................
 * Constructor.
 */
CoordAxes::CoordAxes(unsigned nel0) 
{
  privateConstructor();
  setAxis(0, nel0);
}

/**.......................................................................
 * Constructor.
 */
CoordAxes::CoordAxes(unsigned nel0, unsigned nel1) 
{
  privateConstructor();
  setAxis(0, nel0);
  setAxis(1, nel1);
}

/**.......................................................................
 * Constructor.
 */
CoordAxes::CoordAxes(unsigned nel0, unsigned nel1, unsigned nel2) 
{
  privateConstructor();
  setAxis(0, nel0);
  setAxis(1, nel1);
  setAxis(2, nel2);
}

/**.......................................................................
 * Copy constructor
 */
CoordAxes::CoordAxes(CoordAxes* regAxes)
{
  *this = *regAxes;
}

/**.......................................................................
 * Reset this object
 */
void CoordAxes::reset()
{
  nEl_.resize(0);
}

/**.......................................................................
 * Method for setting arbitrary axes
 */
void CoordAxes::setAxis(unsigned iAxis, unsigned nEl)
{
  // Do nothing if the number of elements for this axis is zero

  if(nEl > 0) {

    // If the requested axis is greater than the current number of axes,
    // resize the array
  
    unsigned nAxisOld = nEl_.size();
    
    if((signed)iAxis > (signed)(nEl_.size()-1)) {
      nEl_.resize(iAxis+1);
      
      // Quietly set any axes which weren't specified to unity size
      
      for(unsigned iAxis=nAxisOld; iAxis < nEl_.size(); iAxis++)
	nEl_[iAxis] = 1;
    }
    
    // And set the number of elements for this axis
    
    nEl_[iAxis] = nEl;
  }
}

/**.......................................................................
 * Destructor.
 */
CoordAxes::~CoordAxes() {}

/**.......................................................................
 * Return the number of axes in this register
 */
unsigned int CoordAxes::nAxis()
{
  return nEl_.size();
}

/**.......................................................................
 * Return the number of elements in this register
 */
unsigned int CoordAxes::nEl(int axis)
{
  unsigned nEl;
  
  if(axis < 0) {
    nEl=nEl_[0];
    
    for(unsigned i=1; i < nEl_.size(); i++)
      nEl *= nEl_[i];
  } else {
    if(axis > nEl_.size()-1) {
      return 0;
    }
    
    nEl = nEl_[axis];
  }
  
  return nEl;
}

/**.......................................................................
 * Return the element offset of the specified coordinate from the
 * start of the register array
 */
unsigned int CoordAxes::elementOffsetOf(Coord& coord)
{
  // Check validity of this coordinate for this axis description

  checkValidityOf(coord);

  // Else compute the element offset of the requested coordinate
  
  unsigned offset = coord.getIndex(0);
  
  for(unsigned iAxis=1; iAxis < nAxis(); iAxis++)
    offset = offset * nEl_[iAxis] + 
      (coord.nAxis() > iAxis ? coord.getIndex(iAxis) : 0);
  
  return offset;
}

/**.......................................................................
 * Return the element offset of the specified coordinate from the
 * start of the register array
 */
unsigned int CoordAxes::elementOffsetOf(Coord* coord)
{
  if(coord==0)
    return 0;

  // If coord is NULL, we don't want to pass it on directly

  Coord refCoord(coord);

  return elementOffsetOf(refCoord);
}

/**.......................................................................
 * Check the validity of a coordinate for this axis descriptor
 */
void CoordAxes::checkValidityOf(Coord& coord)
{
  if(coord.nAxis() > nAxis()) {
    ThrowError("Coordinate " << coord 
	       << " has invalid number of axes (" << coord.nAxis()
	       << ") for this axis description: " << (*this));
  }

  for(unsigned iAxis=0; iAxis < coord.nAxis(); iAxis++) {
    unsigned index = coord.getIndex(iAxis);
  
    if(index < 0 || index > nEl_[iAxis]-1) {
      ThrowError("Index " << index 
		 << " is out of range for axis "
		 << iAxis);
    }
  }
}

/**.......................................................................
 * Return a vector of byte ranges associated with the
 * specified input coordinate range.
 */
std::vector<Range<unsigned> > CoordAxes::getRanges(CoordRange coordRange)
{
  // Check if the range contains valid data

  if(!rangeIsValid(coordRange)) 
    ThrowSimpleError("Range " << coordRange << " does not contain valid data");

  // The range will be considered consistent with this axis
  // description even if indices for fewer than nAxis() are specified.
  // We assume that an axis in the range for which indices have not
  // been specified should default to the whole range for that axis.
  // fillRange() will fill out any missing dimensions in the range
  // object with the full range.  Note that since coordRange is copied
  // on input to this method, the input argument is not modified on
  // return to the caller.

  fillRange(coordRange);

  // Declare the vector of 1-D ranges we will return

  std::vector<Range<unsigned> > ranges;

  // Get the start and stop coordinates in the range object

  Coord start = coordRange.startCoord();
  Coord stop  = coordRange.stopCoord();

  // If the range is to be interpreted as a contiguous range, only one
  // range should be returned, containing the element offset of the
  // start and stop coordinates

  if(coordRange.isContiguous()) {
    Range<unsigned> range(elementOffsetOf(start),
			  elementOffsetOf(stop));
    ranges.push_back(range);

    // Else the range may be a list of non-contiguous element ranges

  } else {
    computeRanges(ranges, 0, start, stop, 0);
  }

  return ranges;
}

/**.......................................................................
 * Return a vector of byte ranges associated with the
 * specified input coordinate range.
 */
std::vector<Range<unsigned> > CoordAxes::getRanges(CoordRange* coordRange)
{
  CoordRange tmpRange(coordRange);
  return getRanges(tmpRange);
}

/**.......................................................................
 * Recursive method for computing multidimensional ranges.
 */
void CoordAxes::computeRanges(std::vector<Range<unsigned> >& ranges, 
			      unsigned iAxis, 
			      Coord& start, Coord& stop, unsigned offset)
{
  unsigned iStart = start.getIndex(iAxis);
  unsigned iStop  = stop.getIndex(iAxis);
  unsigned index;

  // If this is the fastest changing (highest) dimension, add the new
  // range.

  if(iAxis==nAxis()-1) {
    addNewRange(ranges, offset + iStart, offset + iStop);
  } else {
    for(unsigned i=iStart; i <= iStop; i++)
      computeRanges(ranges, iAxis+1, start, stop, (offset + i) * nEl(iAxis+1));
  }
}

/**.......................................................................
 * Add a new range
 */
void CoordAxes::addNewRange(std::vector<Range<unsigned> >& ranges, 
			    unsigned startIndex, unsigned stopIndex)
{
  // If the vector is already populated, see if the current range can
  // be appended to the last one

  if(ranges.size() > 0) {

    // If the current start index is the next index, just expand the
    // current range

    Range<unsigned>* range = &(ranges.at(ranges.size()-1));
    if(range->stop()+1 == startIndex)
      range->setStop(stopIndex);

    // Else create a new one

    else {
      Range<unsigned> range(startIndex, stopIndex);
      ranges.push_back(range);
    }

    // Else create a new one

  } else {
    Range<unsigned> range(startIndex, stopIndex);
    ranges.push_back(range);
  }
}

/**.......................................................................
 * Find a vector element matching the stop index
 */
std::vector<Range<unsigned> >::iterator 
CoordAxes::findStopIndex(std::vector<Range<unsigned> >& ranges, unsigned index)
{
  std::vector<Range<unsigned> >::iterator range;

  for(range=ranges.begin(); range < ranges.end(); range++) {
    cout << ((*range).stop()+1) << ", " << index << endl;
    if((*range).stop()+1 == index)
      break;
  }

  return range;
}

/**.......................................................................
 * Check if a coordinate range is consistent with this axis
 * specifier
 */
bool CoordAxes::rangeIsValid(CoordRange& range)
{
  // First check if the range contains valid data

  if(!range.isValid()) {
    ReportError("Range is not valid");
    return false;
  }

  // Now check the number of axes in the range.  For now, we will
  // consider this consistent if the number of axes specified in the
  // range are less than or equal to the number of axes in this
  // object.

  if(range.nAxis() > nAxis()) {
    ReportError("Number of axes in the range exceeds that in the axis description");
    return false;
  }

  // Now check that none of the ranges exceeds our axis dimension

  for(unsigned iAxis=0; iAxis < range.nAxis(); iAxis++) {

    if(range.startIndex(iAxis) > nEl(iAxis)-1) {
      ReportError("Start index (" << range.startIndex(iAxis)
		  << ") for axis: " << iAxis 
		  << " exceeds the maximum "
		  << "(" << (nEl(iAxis)-1) << ")" 
		  << " for this axis");
      return false;
    }

    if(range.stopIndex(iAxis) > nEl(iAxis)-1) {
      ReportError("Stop index (" << range.stopIndex(iAxis)
		  << ") for axis: " << iAxis 
		  << " exceeds the maximum "
		  << "(" << (nEl(iAxis)-1) << ")" 
		  << " for this axis");
      return false;
    }
  }

  // The range is consistent with our axis specifications

  return true;
}

/**.......................................................................
 * Check if a coordinate range is consistent with this axis
 * specifier
 */
bool CoordAxes::rangeIsValid(CoordRange* range)
{
  if(range == 0)
    return true;
  else
    return rangeIsValid(*range);
}

/**.......................................................................
 * Print a range to stdout
 */
ostream& sza::util::operator<<(std::ostream& os, CoordAxes axes)
{
    for(unsigned i=0; i < axes.nAxis(); i++) {
      os << "[0";
      if(axes.nEl(i) > 1) 
	os << "-" << axes.nEl(i)-1;
      os << "]";
    }

  return os;
}

/**.......................................................................
 * Given an element offset, return the coordinate corresponding to it.
 */
Coord CoordAxes::coordOf(unsigned iEl)
{
  Coord coord;

  // To convert from element offset to coordinate in each axis, we
  // need the number of fastest-changing axis elements per element of
  // each axis.  Cache these values up front:

  std::vector<unsigned> nel;
  nel.resize(nAxis());

  unsigned nelPerElement = 1;
  for(int iAxis=nAxis()-1; iAxis >= 0; iAxis--) {
    nel[iAxis] = nelPerElement;
    nelPerElement *= nEl(iAxis);
  }

  // Now loop through, computing the index for each dimension

  for(int iAxis=0; iAxis < nAxis(); iAxis++) {
    coord.setIndex(iAxis, iEl/nel[iAxis]);
    iEl = iEl % nel[iAxis];
  }

  return coord;
}


/**.......................................................................
 *  A range will be considered consistent with this axis description
 *  even if indices for fewer than nAxis() are specified.  We assume
 *  that an axis in the range for which indices have not been
 *  specified should default to the whole range for that axis.
 * 
 *  This method fills out any missing dimensions in the range object
 *  with the full range for that axis.
 */
void CoordAxes::fillRange(CoordRange& range)
{
  for(unsigned iAxis=range.nAxis(); iAxis < nAxis(); iAxis++) {
    range.setStartIndex(iAxis, 0);
    range.setStopIndex(iAxis, nEl(iAxis)-1);
  }
}

/**.......................................................................
 * Return the total number of elements specified in a range object
 */
unsigned CoordAxes::nEl(CoordRange& coordRange)
{
  return nEl(&coordRange);
}

/**.......................................................................
 * Return the total number of elements specified in a range object
 */
unsigned CoordAxes::nEl(CoordRange* coordRange)
{
  vector<Range<unsigned> > ranges = getRanges(coordRange);

  unsigned nEl=0;
  for(unsigned i=0; i < ranges.size(); i++)
    nEl += (ranges[i].stop() - ranges[i].start() + 1);

  return nEl;
}

/**
 * An operator for testing equality of two Axes
 */
bool CoordAxes::operator==(CoordAxes& axes)
{
  if(axes.nAxis() != nAxis())
    return false;
  else {
    for(unsigned iAxis=0; iAxis < nAxis(); iAxis++)
      if(axes.nEl(iAxis) != nEl(iAxis))
	return false;
  }
}
