#include "carma/szautil/Pressure.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
Pressure::Pressure() {}

Pressure::Pressure(const MilliBar& unit, double mBar)
{
  setMilliBar(mBar);
}

/**.......................................................................
 * Destructor.
 */
Pressure::~Pressure() {}

void Pressure::setMilliBar(double mBar)
{
  mBar_ = mBar;
}

double Pressure::milliBar()
{
  return mBar_;
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
ostream& 
sza::util::operator<<(ostream& os, Pressure& pressure)
{
  os << pressure.milliBar() << " mBar";
  return os;
}
