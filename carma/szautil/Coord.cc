#include "carma/szautil/Coord.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

#include <iostream>

using namespace sza::util;
using namespace std;

/**.......................................................................
 * Constructors.
 */
Coord::Coord() 
{
  // Default to single axis

  setIndex(0, 0);
}

Coord::Coord(unsigned ind0) 
{
  // Default to a single axis

  setIndex(0, ind0);
}

Coord::Coord(unsigned ind0, unsigned ind1)
{
  setIndex(0, ind0);
  setIndex(1, ind1);
}

Coord::Coord(unsigned ind0, unsigned ind1, unsigned ind2)
{
  setIndex(0, ind0);
  setIndex(1, ind1);
  setIndex(2, ind2);
}

Coord::Coord(Coord* coord)
{
  // If coord is not NULL, set the axes to the same length.  Else
  // default to 1-D, 0-elements

  if(coord == 0) {
    setIndex(0, 0);
  } else {
    for(unsigned iaxis=0; iaxis < coord->nAxis(); iaxis++)
      setIndex(iaxis, coord->getIndex(iaxis));
  }
}

/**.......................................................................
 * Destructor.
 */
Coord::~Coord() {}

/**.......................................................................
 * Set the coordinate index of the requested axis
 */
void Coord::setIndex(unsigned iAxis, unsigned index)
{
  unsigned prevSize = initialized_.size();

  if(ind_.size() < iAxis+1) {
    ind_.resize(iAxis+1);

    // Resize the initialization flag array, and set any elements that
    // haven't been initialized to 'false'

    initialized_.resize(iAxis+1);

    for(unsigned i=prevSize; i < iAxis+1;i++)
      initialized_[i] = false;
  }

  // Set the index and the initialization flag for this axis

  ind_[iAxis] = index;
  initialized_[iAxis] = true;
}

/**.......................................................................
 * Reserve an index slot for the requested axis.  This is a way to
 * make the coordinate object match a certain size, without actually
 * setting valid indices.
 */
void Coord::reserveIndex(unsigned iAxis)
{
  // If this object already contains a slot for this axis, do nothing.

  if(iAxis < nAxis())
    return;

  // Else, expand the object, but don't set any index for the new
  // slots

  setIndex(iAxis, 0);
  initialized_[iAxis] = false;
}

/**.......................................................................
 * Reset the coordinate ntuplet
 */
void Coord::reset(unsigned nAxis)
{
  ind_.resize(nAxis);
  initialized_.resize(nAxis);

  for(unsigned i=0; i < nAxis; i++) {
    ind_[i] = 0;
    initialized_[i] = false;
  }
}

/**.......................................................................
 * Return the number of axes in this coordinate nTuplet.
 */
unsigned Coord::nAxis()
{
  return ind_.size();
}

/**.......................................................................
 * Return the coordinate index for axis iAxis
 */
unsigned int Coord::getIndex(unsigned iAxis)
{
  LogStream errStr;

  if(ind_.size() < iAxis+1) {
    errStr.initMessage(true);
    errStr << "Invalid axis requested for axis: " << iAxis << endl;
    throw Error(errStr);
  }

  if(!initialized_[iAxis]) {
    errStr.initMessage(true);
    errStr << "No coordinate was specified for axis: " << iAxis << endl;
    throw Error(errStr);
  }
  
  // Else return the requested index

  return ind_[iAxis];
}

/**.......................................................................
 * Print a Coord object onto a stream
 */
bool Coord::operator==(Coord& coord)
{
  if(nAxis() != coord.nAxis())
    return false;

  // Iterate over axes, checking each for a match

  for(unsigned i=0; i < nAxis(); i++) {
    if(initialized_[i])
      if(coord.initialized_[i]) {

	// If indices for this axis don't match, they do not match

	if(ind_[i] != coord.ind_[i])
	  return false;

	// If both weren't initialized, they do not match

      } else {
	return false;
      }
  }
  return true;
}

/**.......................................................................
 * Print a Coord object onto a stream
 */
ostream& sza::util::operator<<(std::ostream& os, Coord& coord)
{
    for(unsigned i=0; i < coord.nAxis(); i++) 
      os << "[" << coord.getIndex(i) << "]";

  return os;
}

/**.......................................................................
 * Assignment operator
 */
void Coord::operator=(Coord& coord)
{
  if(!coord.isValid())
    ThrowError("Coord: " << coord << " does not contain valid data");

  initialized_.resize(coord.initialized_.size());
  ind_.resize(coord.ind_.size());

  for(unsigned i=0; i < ind_.size(); i++) {
    initialized_[i] = coord.initialized_[i];
    ind_[i]         = coord.ind_[i];
  }
}

/**.......................................................................
 * Assignment operator
 */
void Coord::operator=(Coord coord)
{
  if(!coord.isValid())
    ThrowError("Coord: " << coord << " does not contain valid data");

  initialized_.resize(coord.initialized_.size());
  ind_.resize(coord.ind_.size());

  for(unsigned i=0; i < ind_.size(); i++) {
    initialized_[i] = coord.initialized_[i];
    ind_[i]         = coord.ind_[i];
  }
}

/**.......................................................................
 * Add an increment to this object
 */
Coord& Coord::operator+=(unsigned incr)
{
  for(unsigned i=0; i < ind_.size(); i++) {
    if(initialized_[i])
      ind_[i] += incr;
  }

  return *this;
}

/**.......................................................................
 * Return true if a coordinate is set for this axis
 */
bool Coord::isSet(unsigned iAxis)
{
  if(iAxis >= nAxis())
    return false;
  else
    return initialized_[iAxis];
}

/**.......................................................................
 * Check if this coordinate contains valid data
 */
bool Coord::isValid()
{
  // If the object has zero dimensions, it is not valid

  //  if(nAxis()==0)
  //    return false;

  // Else the object is valid if an index has been set for each axis
  // in the object

  bool valid=true;
  for(unsigned iAxis=0; iAxis < nAxis(); iAxis++) 
    valid &= initialized_[iAxis];

  return valid;
}
