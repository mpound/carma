#include "carma/szautil/Exception.h"

#include "carma/antenna/sza/antenna/control/SlowWalsh.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::util;


/**.......................................................................
 * Constructor with initialization
 */
SlowWalsh::SlowWalsh(unsigned short iwalsh)
{
  if(!isValidIndex(iwalsh))
    throw Error("SlowWalsh::SlowWalsh: Walsh function index out of range.\n");
  
  walshFunction_ = walshFunctions_[iwalsh];
  initialized_   = true;
}

/**.......................................................................
 * Constructor with no initialization
 */
SlowWalsh::SlowWalsh() :  initialized_(false) {};

/**.......................................................................
 * Set the walsh function
 */
void SlowWalsh::changeFunction(unsigned short iwalsh)
{
  if(!isValidIndex(iwalsh))
    throw Error("SlowWalsh::changeWalsh: Received invalid index.\n");

  walshFunction_ = walshFunctions_[iwalsh];
  initialized_ = true;
}

/**.......................................................................
 * Return true if this object has been initialized
 */
bool SlowWalsh::isInitialized()
{
  return initialized_;
}

//-----------------------------------------------------------------------
// Private members
//-----------------------------------------------------------------------

/**.......................................................................
 * Return true if the passed index is in range
 */
bool SlowWalsh::isValidIndex(unsigned short iwalsh)
{
  if(iwalsh <= WALSH_PERIOD)
    return true;
  return false;
}

/**.......................................................................
 * Define a set of 16 walsh functions.  These will be used to modulate
 * the inputs for each receiver to the correlator.  When the walsh
 * state is 1, we will insert a 0-degree phase shift, when it is -1,
 * we will insert 180.
 */
const int SlowWalsh::walshFunctions_[WALSH_PERIOD][WALSH_PERIOD] = {
{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
{ 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1},
{ 1, 1,-1,-1, 1, 1,-1,-1, 1, 1,-1,-1, 1, 1,-1,-1},
{ 1,-1,-1, 1, 1,-1,-1, 1, 1,-1,-1, 1, 1,-1,-1, 1},
{ 1, 1, 1, 1,-1,-1,-1,-1, 1, 1, 1, 1,-1,-1,-1,-1},
{ 1,-1, 1,-1,-1, 1,-1, 1, 1,-1, 1,-1,-1, 1,-1, 1},
{ 1, 1,-1,-1,-1,-1, 1, 1, 1, 1,-1,-1,-1,-1, 1, 1},
{ 1,-1,-1, 1,-1, 1, 1,-1, 1,-1,-1, 1,-1, 1, 1,-1},
{ 1, 1, 1, 1, 1, 1, 1, 1,-1,-1,-1,-1,-1,-1,-1,-1},
{ 1,-1, 1,-1, 1,-1, 1,-1,-1, 1,-1, 1,-1, 1,-1, 1},
{ 1, 1,-1,-1, 1, 1,-1,-1,-1,-1, 1, 1,-1,-1, 1, 1},
{ 1,-1,-1, 1, 1,-1,-1, 1,-1, 1, 1,-1,-1, 1, 1,-1},
{ 1, 1, 1, 1,-1,-1,-1,-1,-1,-1,-1,-1, 1, 1, 1, 1},
{ 1,-1, 1,-1,-1, 1,-1, 1,-1, 1,-1, 1, 1,-1, 1,-1},
{ 1, 1,-1,-1,-1,-1, 1, 1,-1,-1, 1, 1, 1, 1,-1,-1},
{ 1,-1,-1, 1,-1, 1, 1,-1,-1, 1, 1,-1, 1,-1,-1, 1}
};

