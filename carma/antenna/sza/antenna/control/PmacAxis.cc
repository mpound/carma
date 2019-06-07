#include "carma/antenna/sza/antenna/control/PmacAxis.h"

using namespace std;
using namespace sza::antenna::control;

/**.......................................................................
 * Constructor method
 */
PmacAxis::PmacAxis()
{
  reset();
}

/**.......................................................................
 * Reset method
 */
void PmacAxis::reset()
{
  count_ = 0;
  rate_  = 0;
}

/**.......................................................................
 * Return the encoder count
 */
signed PmacAxis::getCount()
{
  return count_;
}

/**.......................................................................
 * Return the encoder rate
 */
signed PmacAxis::getRate()
{
  return rate_;
}

/**.......................................................................
 * Set the count
 */
void PmacAxis::setCount(signed count)
{
  count_  = count;
}

/**.......................................................................
 * Set the rate
 */
void PmacAxis::setRate(signed rate)
{
  rate_ = rate;
}


