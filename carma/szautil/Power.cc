#include "carma/szautil/Power.h"

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
Power::Power() 
{
  initialize();
}

Power::Power(const dBm& units, double dBmPow)
{
  setdBm(dBmPow);
}

/**.......................................................................
 * Destructor.
 */
Power::~Power() {}

void Power::initialize()
{
  setdBm(0.0);
}

