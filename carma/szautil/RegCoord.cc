#include "carma/szautil/RegCoord.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

using namespace sza::util;

/**.......................................................................
 * Constructors.
 */
RegCoord::RegCoord() 
{
  // Default to single axis

  setIndex(0, 0);
}

RegCoord::RegCoord(unsigned ind0) 
{
  // Default to single axis

  setIndex(0, ind0);
}

RegCoord::RegCoord(unsigned ind0, unsigned ind1)
{
  setIndex(0, ind0);
  setIndex(1, ind1);
}

RegCoord::RegCoord(unsigned ind0, unsigned ind1, unsigned ind2)
{
  setIndex(0, ind0);
  setIndex(1, ind1);
  setIndex(2, ind2);
}

RegCoord::RegCoord(RegCoord* coord)
{
  // If coord is NULL, default to two dimensional axes, zero length

  if(coord==0) {
    setIndex(0, 0);
    setIndex(1, 0);
  } else {

    for(unsigned iaxis=0; iaxis < coord->nAxis(); iaxis++)
      setIndex(iaxis, coord->getIndex(iaxis));
  }
}

/**.......................................................................
 * Destructor.
 */
RegCoord::~RegCoord() {}

/**.......................................................................
 * Set the coordinate index of the requested axis
 */
void RegCoord::setIndex(unsigned iAxis, unsigned index)
{
  DBPRINT(false, Debug::DEBUG3, "iAxis = " << iAxis);
  DBPRINT(false, Debug::DEBUG3, "ind_.size() = " << ind_.size());
  DBPRINT(false, Debug::DEBUG3, "cond = " <<   (iAxis+1 > ind_.size()));

  if(ind_.size() < iAxis+1)
    ind_.resize(iAxis+1);

  ind_[iAxis] = index;
}

/**.......................................................................
 * Reset the coordinate ntuplet
 */
void RegCoord::reset(unsigned nAxis)
{
  ind_.resize(nAxis);

  for(unsigned i=0; i < nAxis; i++)
    ind_[i] = 0;
}

/**.......................................................................
 * Return the number of axes in this coordinate nTuplet.
 */
unsigned RegCoord::nAxis()
{
  return ind_.size();
}

/**.......................................................................
 * Return the coordinate index for axis iAxis
 */
unsigned int RegCoord::getIndex(unsigned iAxis)
{
  LogStream errStr;

  if(ind_.size() < iAxis+1) {
    errStr.appendMessage(true, "Invalid axis requested for this coordinate\n");
    throw Error(errStr);
  }
  
  // Else return the requested index

  return ind_[iAxis];
}
