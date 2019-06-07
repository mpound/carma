#include "carma/szautil/SolidAngle.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
SolidAngle::SolidAngle() 
{
  initialize();
}

SolidAngle::SolidAngle(const Steradians& units, double sr)
{
  setSr(sr);
}

SolidAngle::SolidAngle(const SqArcMinutes& units, double sqarcmin)
{
  setSqArcMin(sqarcmin);
}

SolidAngle::SolidAngle(Angle& fwhm)
{
  setSr(M_PI*fwhm.radians()*fwhm.radians()/(4*log(2.0)));
}

SolidAngle::SolidAngle(Angle& fwhma, Angle& fwhmb)
{
  setSr(M_PI*fwhma.radians()*fwhmb.radians()/(4*log(2.0)));
}

/**.......................................................................
 * Destructor.
 */
SolidAngle::~SolidAngle() {}

void SolidAngle::initialize()
{
  setSr(0.0);
}

void SolidAngle::setSr(double sr)
{
  sr_ = sr;
}

void SolidAngle::setSqArcMin(double sqarcmin)
{
  setSr(sqarcmin / (Angle::arcMinPerRad_ * Angle::arcMinPerRad_));
}
