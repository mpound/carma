#include "carma/szautil/Constants.h"
#include "carma/szautil/Intensity.h"
#include "carma/szautil/Planck.h"

#include "carma/szautil/Exception.h"

#include <cmath>

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
Planck::Planck() {}

/**.......................................................................
 * Destructor.
 */
Planck::~Planck() {}

/**.......................................................................
 * Planck intensity
 */
Intensity Planck::IPlanck(Frequency nu, Temperature T)
{
  double x = xPlanck(nu, T);
  double wave = Constants::lightSpeed_.centimetersPerSec() / nu.Hz();
  double prefac = 2 * (Constants::hPlanckCgs_ * nu.Hz()) / (wave * wave);
  double jypersr = prefac / (exp(x) - 1) * 1e23;

  Intensity intensity;

  intensity.setJyPerSr(jypersr);

  return intensity;
}

/**.......................................................................
 * Return Planck dI/dT, in units of (Jy/sr) / K
 */
double Planck::JyPerSrPerKPlanck(Frequency nu, Temperature T)
{
  double x       = xPlanck(nu, T);
  double ex      = exp(x);
  double wave    = Constants::lightSpeed_.centimetersPerSec() / nu.Hz();
  double prefac  = 2 * (Constants::kBoltzCgs_) / (wave * wave);
  double jypersrperk = prefac * x * x * ex / ((ex - 1) * (ex - 1)) * 1e23;

  COUT("Inside JyPerSrPerKPlanck: nu = " << nu << " T = " << T.K() << " jypersrperk");
  return jypersrperk;
}

/**.......................................................................
 * Planck x-factor
 */
double Planck::xPlanck(Frequency nu, Temperature T)
{
  return (Constants::hPlanckCgs_ * nu.Hz()) / (Constants::kBoltzCgs_ * T.K());
}
