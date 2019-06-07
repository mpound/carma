#include "carma/szautil/SzCalculator.h"
#include "carma/szautil/Constants.h"

#include <cmath>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
SzCalculator::SzCalculator() {}

/**.......................................................................
 * Destructor.
 */
SzCalculator::~SzCalculator() {}

/**.......................................................................
 * Calculate the factor by which comptonY should be multiplied
 * to convert to CMB temperature decrement/increment
 */
Intensity SzCalculator::comptonYToIntensity(Frequency& freq)
{
  Intensity dp = dPlanck(freq, Constants::Tcmb_);
  Temperature t = comptonYToDeltaT(freq);

  Intensity conv;
  conv.setJyPerSr(dp.JyPerSr() * t.K());

  return conv;  
}

/**.......................................................................
 * Calculate the factor by which comptonY should be multiplied
 * to convert to CMB temperature decrement/increment
 */
Temperature SzCalculator::comptonYToDeltaT(Frequency& freq)
{
  double x        = planckX(freq, Constants::Tcmb_);
  double ex       = exp(x);
  double denomFac = ex-1;

  // From Peebles (24.48), p. 585
  //
  //    dN/N = x*x*ex*(ex+1)/(denomFac * denomFac) - 4*x*ex/denomFac;
  //
  // therefore dT/T = dN/N * (dN/dT)^-1 * N/T
  //
  //    N = 1/(ex - 1), therefore
  //
  //    dN/dT = ex/(ex - 1)^2 * hv/kT^2 = (1/T)*x*ex/(ex - 1)^2
  //
  // so dT/dN * N/T = (ex - 1)/(x*ex), and
  //
  //    dT/T = x*(ex+1)/denomFac - 4
  //

  double f = x*(ex+1)/(denomFac) - 4;
  Temperature YToT;

  YToT.setK(f * Constants::Tcmb_.K());

  return YToT;
}

/**.......................................................................
 * Evaluate the derivative of the Planck function wrt to T at given T
 * and freq.
 *
 * Input:
 *
 *  freq  Frequency
 *  temp  Brightness temperature
 *
 * Output:
 *
 *  The conversion, in Flux/K.
 */
Intensity SzCalculator::dPlanck(Frequency& freq, Temperature& temp)
{
  double k = Constants::kBoltzCgs_;
  double c = Constants::lightSpeed_.centimetersPerSec();
  double v = freq.Hz();
 
  double prefac = 2 * k * (v * v) / (c * c);
  double x = planckX(freq, temp);
  double expx = exp(x);

  COUT("k " << k);
  COUT("c " << c);
  COUT("v " << v);
  COUT("x " << x);
  COUT("Jy/K = " << prefac*x*x*expx/((expx-1)*(expx-1))*1e23);

  Intensity inten;
  inten.setJyPerSr(prefac*x*x*expx/((expx-1)*(expx-1))*1e23);

  return inten;
}

/**.......................................................................
 * Calculate the dimensionless x factor that enters into the Planck
 * function
 */
double SzCalculator::planckX(Frequency& freq, Temperature& temp)
{
  double k = Constants::kBoltzCgs_;
  double h = Constants::hPlanckCgs_;
  double T = temp.K();
  double v = freq.Hz();

  return (h * v) / (k * T);
}

