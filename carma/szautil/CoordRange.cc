#include "carma/szautil/CoordRange.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/Exception.h"

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructors
 */
CoordRange::CoordRange() 
{
  initialize();
}

CoordRange::CoordRange(unsigned index)
{
  initialize();

  startCoord_.setIndex(0, index);
  stopCoord_.setIndex(0, index);
}

CoordRange::CoordRange(unsigned iStart, unsigned iStop) 
{
  initialize();

  if(iStop < iStart) 
    ThrowError("iStop < iStart doesn't make any sense");
  
  startCoord_.setIndex(0, iStart);
  stopCoord_.setIndex(0, iStop);
}

CoordRange::CoordRange(Coord& coord)
{
  initialize();

  setStartCoord(coord);
  setStopCoord(coord);
}

CoordRange::CoordRange(Coord& start, Coord& stop) 
{
  initialize();

  setStartCoord(start);
  setStopCoord(stop);
}

CoordRange::CoordRange(CoordRange* coordRange) 
{
  initialize();

  if(coordRange != 0) {
    startCoord_ = coordRange->startCoord();
    stopCoord_  = coordRange->stopCoord();
  }
}

void CoordRange::initialize()
{
  contiguous_ = false;
  startCoord_.reset(0);
  stopCoord_.reset(0);
}

/**.......................................................................
 * Destructor.
 */
CoordRange::~CoordRange() {}

/**.......................................................................
 * Set/get the index of the first element in the range
 */
void CoordRange::setStartIndex(unsigned iAxis, unsigned iStart)
{
  // Set the index in the start coordinate

  startCoord_.setIndex(iAxis, iStart);

  // Make sure the stop coordinate has the same size as the start

  stopCoord_.reserveIndex(iAxis);
}

/**.......................................................................
 * Return the start index for axis iAxis
 */
unsigned CoordRange::startIndex(unsigned iAxis)
{
  return startCoord_.getIndex(iAxis);
}

/**.......................................................................
 * Set/get the index of the first element in the range
 */
void CoordRange::setStopIndex(unsigned iAxis, unsigned iStop)
{
  // Set the index in the stop coordinate

  stopCoord_.setIndex(iAxis, iStop);

  // Make sure the start coordinate has the same size as the stop

  startCoord_.reserveIndex(iAxis);
}

/**.......................................................................
 * Return the stop index for axis iAxis
 */
unsigned CoordRange::stopIndex(unsigned iAxis)
{
  return stopCoord_.getIndex(iAxis);
}

/**.......................................................................
 * Set both the start and stop index to the same value
 */
void CoordRange::setIndex(unsigned iAxis, unsigned index) 
{
  setStartIndex(iAxis, index);
  setStopIndex(iAxis, index);
}

/**.......................................................................
 * Same as above, but defaults to axis 0
 */
void CoordRange::setIndex(unsigned index) 
{
  setStartIndex(0, index);
  setStopIndex(0, index);
}

/**.......................................................................
 * Test for equality of two ranges
 */
bool CoordRange::operator==(CoordRange& range)
{
  Coord start = range.startCoord();
  Coord stop = range.stopCoord();
  return startCoord_ == start && stopCoord_ == stop;
}

/**.......................................................................
 * Print a range to stdout
 */
ostream& sza::util::operator<<(std::ostream& os, CoordRange range)
{
  unsigned start, stop;

  if(range.isContiguous()) {
    for(unsigned iAxis=0; iAxis < range.nAxis(); iAxis++) {
      start = range.startCoord_.getIndex(iAxis);
      os << "[" << start << "]";
    }
    os << "-";
    for(unsigned iAxis=0; iAxis < range.nAxis(); iAxis++) {
      stop = range.stopCoord_.getIndex(iAxis);
      os << "[" << stop << "]";
    }
  } else {
    for(unsigned iAxis=0; iAxis < range.nAxis(); iAxis++) {
      start = range.startCoord_.getIndex(iAxis);
      stop = range.stopCoord_.getIndex(iAxis);
      os << "[" << start;
      
      if(stop != start)
	os << "-" << stop;
      
      os << "]";
    } 
  }
  return os;
}
/**.......................................................................
 * Add an increment to this object
 */
CoordRange& CoordRange::operator+=(unsigned incr)
{
  startCoord_ += incr;
  stopCoord_  += incr;

  return *this;
}

/**.......................................................................
 * Get the number of axes specified
 */
unsigned CoordRange::nAxis()
{
  // Both coordinates will always be the same size, so just return
  // either one

  return startCoord_.nAxis();
}

/**.......................................................................
 * Return true if this coordinate range contains valid data
 */
bool CoordRange::isValid()
{
  if(!startCoord_.isValid() || !stopCoord_.isValid())
    return false;

  // Check also that the range is monotonically increasing in all
  // axes

  for(unsigned iAxis=0; iAxis < nAxis(); iAxis++)
    if(startCoord_.getIndex(iAxis) > stopCoord_.getIndex(iAxis))
      return false;

  return true;
}

/**.................................................................................
 * Set whether or not this range is to be interpreted as a contiguous
 * range from startCoord to stopCoord
 */
void CoordRange::setContiguous(bool contiguous)
{
  contiguous_ = contiguous;
}

/**.................................................................................
 * Return true if this range is to be interpreted as a contiguous
 * range from startCoord to stopCoord
 */
bool CoordRange::isContiguous()
{
  return contiguous_ ;
}

/**.......................................................................
 * Set the start coordinate
 */
void CoordRange::setStartCoord(Coord& startCoord)
{
  startCoord_.reset();
  for(unsigned iAxis=0; iAxis < startCoord.nAxis(); iAxis++)
    setStartIndex(iAxis, startCoord.getIndex(iAxis));
}

void CoordRange::setStartCoord(Coord* startCoord)
{
  setStartCoord(*startCoord);
}

Coord CoordRange::startCoord()
{
  return startCoord_;
}

/**.......................................................................
 * Set the stop coordinate
 */
void CoordRange::setStopCoord(Coord& stopCoord)
{
  stopCoord_.reset();
  for(unsigned iAxis=0; iAxis < stopCoord.nAxis(); iAxis++)
    setStopIndex(iAxis, stopCoord.getIndex(iAxis));
}

void CoordRange::setStopCoord(Coord* stopCoord)
{
  setStopCoord(*stopCoord);
}

Coord CoordRange::stopCoord()
{
  return stopCoord_;
}

void CoordRange::setCoord(Coord coord)
{
  setStartCoord(coord);
  setStopCoord(coord);
}

/**.......................................................................
 * Get the number of elements in this axis
 */
unsigned CoordRange::nEl(unsigned iAxis)
{
  return stopIndex(iAxis) - startIndex(iAxis) + 1;
}

bool CoordRange::contains(CoordRange& range)
{
  for(unsigned iAxis=0; iAxis < startCoord_.nAxis(); iAxis++)
    if((range.startCoord_.getIndex(iAxis) < startCoord_.getIndex(iAxis)) ||
       (range.stopCoord_.getIndex(iAxis) > stopCoord_.getIndex(iAxis)))
      return false;
  return true;
}
