#include "carma/szautil/RegAxisRange.h"
#include "carma/szautil/RegDescription.h"

using namespace sza::util;
using namespace std;

#define TEST_BLOCK(block) \
 { \
    if(block == 0) \
      std::cout << "block is NULL" << std::endl; \
    else \
      std::cout << "block is NOT NULL" << std::endl; \
 }

/**.......................................................................
 * Constructor.
 */
RegAxisRange::RegAxisRange(RegDescription& reg, CoordRange& range) :
  AxisRange(reg.axes(), range)
{
  // Store the slot offset of this register

  iSlotOffset_ = reg.iSlot();
}

/**.......................................................................
 * Constructor with CoordRange pointer
 */
RegAxisRange::RegAxisRange(RegDescription& reg, CoordRange* range) :
  AxisRange(reg.axes(), range==0 ? reg.getRangePtr() : range)
{
  // Store the slot offset of this register

  iSlotOffset_ = reg.iSlot();
}

/**.......................................................................
 * Constructor with no initialization
 */
RegAxisRange::RegAxisRange() : 
  AxisRange()
{
  iSlotOffset_ = -1;
}

/**.......................................................................
 * Constructor with no initialization
 */
RegAxisRange::RegAxisRange(RegMapBlock* block) : 
  AxisRange(block)
{
  TEST_BLOCK(block);
  iSlotOffset_ = -1;
  TEST_BLOCK(block);
}

/**.......................................................................
 * Set the range of this object
 */
void RegAxisRange::setTo(RegDescription& reg, CoordRange* range)
{
  iSlotOffset_ = reg.iSlot();

  CoordAxes axes = reg.axes();
  AxisRange::setTo(&axes, range);
}

/**.......................................................................
 * Destructor.
 */
RegAxisRange::~RegAxisRange() {}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
ostream& 
sza::util::operator<<(ostream& os, RegAxisRange& range)
{
  os << "Current element = " << range.currentElement() << endl;
  os << "Current slot    = " << range.currentSlot() << endl;
  return os;
}

/**.......................................................................
 * Return a singleton range associated with the current element
 */
CoordRange RegAxisRange::currentCoordRange()
{
  Coord coord = currentCoord();
  return CoordRange(coord, coord);
}

