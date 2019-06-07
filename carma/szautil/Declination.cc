#include "carma/szautil/Declination.h"
#include "carma/szautil/Exception.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
Declination::Declination() : DecAngle()
{
  modulo_ = true;
}

/**.......................................................................
 * Destructor.
 */
Declination::~Declination() {}

void Declination::addRadians(double radians)
{
  // Baseclass add function will fold into the range r >= 0 && r <= 2*PI

  Angle::addRadians(radians);

  // Now fold pi/2 -> 3pi/2 into appropriate dec ranges

  if(radians_ > M_PI/2 && radians_ < 3*M_PI/2) {
    radians_ = M_PI - radians_;
  }

  // And fold 3pi/2 -> 2pi into appropriate dec ranges
 
  if(radians_ >= 3*M_PI/2 && radians_ <= 2*M_PI) {
    radians_ = radians_ - 2*M_PI;
  }
}
