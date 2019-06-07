#include "carma/services/Delay.h"

using namespace std;
using namespace carma::services;

/**.......................................................................
 * Constructor.
 */
Delay::Delay() 
{
  nanoSeconds_ = 0.0;
}

/**.......................................................................
 * Destructor.
 */
Delay::~Delay() {}

/**.......................................................................
 * Addition operator for Delay.
 */
const Delay Delay::operator+(const Delay& delay) const
{
  Delay sum;
  sum.setDelayInNanoSeconds(nanoSeconds() + delay.nanoSeconds());
  return sum;
}

/**.......................................................................
 * Subtraction operator for Delay.
 */
const Delay Delay::operator-(const Delay& delay) const
{
  Delay diff;
  diff.setDelayInNanoSeconds(nanoSeconds() - delay.nanoSeconds());
  return diff;
}

/**.......................................................................
 * Increment operator for Delay.
 */
Delay& Delay::operator+=(const Delay& delay)
{
  nanoSeconds_ += delay.nanoSeconds();
  return *this;
}

/**.......................................................................
 * Decrement operator for Delay.
 */
Delay& Delay::operator-=(const Delay& delay)
{
  nanoSeconds_ -= delay.nanoSeconds();
  return *this;
}

/**.......................................................................
 * Increment operator for Delay.
 */
Delay& Delay::operator+=(const Delay delay)
{
  nanoSeconds_ += delay.nanoSeconds();
  return *this;
}

/**.......................................................................
 * Decrement operator for Delay.
 */
Delay& Delay::operator-=(const Delay delay)
{
  nanoSeconds_ -= delay.nanoSeconds();
  return *this;
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
ostream& carma::services::operator<<(ostream& os, Delay delay)
{
  os << delay.nanoSeconds();
  return os;
}
