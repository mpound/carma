#include "carma/szautil/RegAxes.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

using namespace sza::util;

/**.......................................................................
 * Private initialization method
 */
void RegAxes::privateConstructor() 
{
  // Default to two-dimensions
  
  nel_.resize(2);
}
/**.......................................................................
 * Constructor.
 */
RegAxes::RegAxes() 
{
  privateConstructor();
  setAxis(0, 1); // Set both axes to have 1 element
  setAxis(1, 1); 
}

/**.......................................................................
 * Constructor.
 */
RegAxes::RegAxes(unsigned nel0) 
{
  privateConstructor();
  setAxis(0, nel0);
  setAxis(1, 1); // Set the highest axis to have 1 element
}

/**.......................................................................
 * Constructor.
 */
RegAxes::RegAxes(unsigned nel0, unsigned nel1) 
{
  privateConstructor();
  setAxis(0, nel0);
  setAxis(1, nel1);
}

/**.......................................................................
 * Copy constructor
 */
RegAxes::RegAxes(RegAxes* regAxes)
{
  *this = *regAxes;
}

/**.......................................................................
 * Method for setting arbitrary axes
 */
void RegAxes::setAxis(unsigned iAxis, unsigned nEl)
{
  // If the requested axis is greater than the current number of axes,
  // resize the array
  
  if(iAxis > nel_.size()-1)
    nel_.resize(iAxis+1);
  
  // And set the number of elements for this axis
  
  nel_[iAxis] = nEl;
}

/**.......................................................................
 * Destructor.
 */
RegAxes::~RegAxes() {}

/**.......................................................................
 * Return the number of axes in this register
 */
unsigned int RegAxes::nAxis()
{
  return nel_.size();
}

/**.......................................................................
 * Return the number of elements in this register
 */
unsigned int RegAxes::nEl(int axis)
{
  unsigned nel;
  
  if(axis < 0) {
    nel=nel_[0];
    
    for(unsigned i=1; i < nel_.size(); i++)
      nel * nel_[i];
  } else {
    if(axis > nel_.size()) {
      LogStream errStr;
      errStr.initMessage(true);
      errStr <<  "Invalid axis: " << axis << std::endl;
      throw Error(errStr);
    }
    
    nel = nel_[axis];
  }
  
  return nel;
}

/**.......................................................................
 * Return the element offset of the specified coordinate from the
 * start of the register array
 */
unsigned int RegAxes::elementOffsetOf(RegCoord& coord)
{
  return refElementOffsetOf(coord);
}

/**.......................................................................
 * Return the element offset of the specified coordinate from the
 * start of the register array
 */
unsigned int RegAxes::elementOffsetOf(RegCoord coord)
{
  return refElementOffsetOf(coord);
}

/**.......................................................................
 * Return the element offset of the specified coordinate from the
 * start of the register array
 */
unsigned int RegAxes::refElementOffsetOf(RegCoord& coord)
{
  LogStream errStr;
  
  if(coord.nAxis() != nAxis()) {
    errStr.appendMessage(true, "Invalid coordinate for this register\n");
    throw Error(errStr);
  }
  
  // Else compute the element offset of the requested coordinate
  
  unsigned offset = getIndex(coord, 0);
  
  for(unsigned i=1; i < coord.nAxis(); i++)
    offset = offset * nel_[i] + getIndex(coord, i);
  
  return offset;
}

/**.......................................................................
 * Check the validity of the requested index.
 */
unsigned int RegAxes::getIndex(RegCoord& coord, unsigned iAxis)
{
  unsigned index = coord.getIndex(iAxis);
  
  if(index < 0 || index > nel_[iAxis]-1) {
    LogStream errStr;
    errStr.initMessage(true);
    errStr << "Index " << index 
	   << " is out of range for axis "
	   << iAxis << std::endl;
    throw Error(errStr);
  }

  return index;
}

