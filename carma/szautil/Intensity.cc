#include "carma/szautil/Intensity.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
Intensity::Intensity() {}

/**.......................................................................
 * Destructor.
 */
Intensity::~Intensity() {}

void Intensity::initialize()
{
  setJyPerSr(0.0);
}

/**.......................................................................
 * Constructor.
 */
Intensity::Intensity(const JanskyPerSr& units, double JyPerSr) 
{
  setJyPerSr(JyPerSr);
}

/**.......................................................................
 * Constructor.
 */
Intensity::Intensity(const MegaJanskyPerSr& units, double MJyPerSr) 
{
  setJyPerSr(MJyPerSr * 1e6);
}


void Intensity::setJyPerSr(double JyPerSr)
{
  JyPerSr_ = JyPerSr;
}
