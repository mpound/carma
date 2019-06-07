#include "carma/szautil/RegCoordRange.h"

using namespace sza::util;
using namespace std;

/**.......................................................................
 * Constructor.
 */
RegCoordRange::RegCoordRange(RegDescription& reg, CoordRange& range) 
{
  // Get the ranges, in elements wrt to the first element of this
  // register

  ranges_ = reg.getElementRanges(&range);

  // Else store the slot offset of this register

  iSlotOffset_ = reg.iSlot();

  // And reset to the start

  reset();
}

/**.......................................................................
 * Destructor.
 */
RegCoordRange::~RegCoordRange() {}

/**.......................................................................
 * Reset internal iterators to correspond to the starting byte of this
 * range.
 */
void RegCoordRange::reset() 
{
  iRange_     = ranges_.begin();
  iElCurrent_ = (*iRange_).start();
}

/**.......................................................................
 * Return true if we are at the end of an iteration
 */
bool RegCoordRange::isEnd()
{
  return iRange_ == ranges_.end();
}

/**.......................................................................
 * Prefix increment operator
 */
const RegCoordRange& RegCoordRange::operator++()
{
  // If we are done, just return silently

  if(!isEnd()) {

    // If we just did the last element of the current range, advance
    // to the next range

    if(iElCurrent_ == (*iRange_).stop()) {
      
      // If we are on the last range, something's wrong

      if(iRange_ == ranges_.end()) {
	ThrowError("Prematurely reached the end of the slot range");
      } else // Else increment to the next range
	iRange_++;
    } else // Else increment to the next slot
      iElCurrent_++;
  }
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
ostream& 
sza::util::operator<<(ostream& os, RegCoordRange& range)
{
  os << "Current element = " << range.iElCurrent_ << endl;

  os << "Current slot = ";

  if(range.iSlotOffset_ < 0)
    os << -1;
  else
    os << range.iSlotOffset_ + range.iElCurrent_;
  
  return os;
}
