#include "carma/szautil/Delay.h"

using namespace std;
using namespace sza::util;

const double Delay::lightSpeed_    = 2.99792458e8;
const double Delay::nanoSecPerSec_ = 1e9;

/**.......................................................................
 * Constructor.
 */
Delay::Delay() 
{
  nanoSeconds_ = 0.0;
  dNanoSeconds_ = 0.0;
}

/**.......................................................................
 * Destructor.
 */
Delay::~Delay() {}

/**.......................................................................
 * Addition operator for Delay.
 */
Delay Delay::operator+(Delay& delay)
{
  Delay sum;
  sum.setDelayInNanoSeconds(nanoSeconds() + delay.nanoSeconds());
  sum.setDelayRateInNanoSeconds(nanoSecondsPerSecond() + delay.nanoSecondsPerSecond());
  return sum;
}

/**.......................................................................
 * Subtraction operator for Delay.
 */
Delay Delay::operator-(Delay& delay)
{
  Delay diff;
  diff.setDelayInNanoSeconds(nanoSeconds() - delay.nanoSeconds());
  diff.setDelayRateInNanoSeconds(nanoSecondsPerSecond() - delay.nanoSecondsPerSecond());
  return diff;
}

/**.......................................................................
 * Increment operator for Delay.
 */
Delay& Delay::operator+=(Delay& delay)
{
  nanoSeconds_  += delay.nanoSeconds();
  dNanoSeconds_ += delay.nanoSecondsPerSecond();
  return *this;
}

/**.......................................................................
 * Decrement operator for Delay.
 */
Delay& Delay::operator-=(Delay& delay)
{
  nanoSeconds_  -= delay.nanoSeconds();
  dNanoSeconds_ -= delay.nanoSecondsPerSecond();
  return *this;
}

/**.......................................................................
 * Increment operator for Delay.
 */
Delay& Delay::operator+=(Delay delay)
{
  nanoSeconds_  += delay.nanoSeconds();
  dNanoSeconds_ += delay.nanoSecondsPerSecond();
  return *this;
}

/**.......................................................................
 * Decrement operator for Delay.
 */
Delay& Delay::operator-=(Delay delay)
{
  nanoSeconds_  -= delay.nanoSeconds();
  dNanoSeconds_ -= delay.nanoSecondsPerSecond();
  return *this;
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
ostream& sza::util::operator<<(ostream& os, Delay delay)
{
  os << delay.nanoSeconds() 
     << " ns (" << delay.nanoSecondsPerSecond() << " ns/s)";
  return os;
}
