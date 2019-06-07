#include "carma/szautil/Sampler.h"
#include "carma/szautil/PtSrcGen.h"
#include <cstdlib>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
PtSrcGen::PtSrcGen() 
{
  dNdSIsSet_ = false;
}

/**.......................................................................
 * Destructor.
 */
PtSrcGen::~PtSrcGen() {}

/**.......................................................................
 * Set the dN/dS to use, as a power law.
 *
 * k     is N per area, in some area units
 * gamma is the power law index
 *
 * flux  is the flux of the unit in which the power law is specified
 * area  is the solid angle of the unit in which k is specified
 */
void PtSrcGen::setDnDs(double k, double gamma, 
		       Flux flux, SolidAngle area)
{
  dNdSIsSet_ = true;

  k_     = k;
  gamma_ = gamma;
  fu_    = flux;
  au_    = area;
}

/**.......................................................................
 * Set the dNdS numerically, as arrays  
 */
void PtSrcGen::
setDnDs(std::vector<double> flux, std::vector<double> num, 
	const Flux::Jansky& fluxUnit, const SolidAngle::Steradians& angleUnit)
{
}

/**.......................................................................
 * Generate a list of sources drawn from the specified
 * distribution, within the specified solid angle
 */
std::vector<Flux> PtSrcGen::generateSources(Flux fluxMin, SolidAngle area, bool doRand)
{
  // Get the number of sources we should generate.  This calculates
  // the expected number of sources above fluxMin, then generates a
  // random number from a Poisson with that mean

  unsigned nSrc = getNSrc(fluxMin, area, doRand);
  COUT("nSrc = " << nSrc);

  std::vector<Flux> fluxes(nSrc);

  // Now generate nSrc sources above fluxMin by generating random
  // numbers between 0 and 1, and calculating the flux that
  // corresponds to that value of the integral dostribution from
  // fluxMin to +inf.

  for(unsigned i=0; i < nSrc; i++) {
    double r = (double)(rand())/RAND_MAX;
    fluxes[i].setJy(pow(1.0-r,1.0/(1.0-gamma_)) * fluxMin.Jy());
  }

  return fluxes;
}

/**.......................................................................
 * Generate a list of sources drawn from the specified
 * distribution, within the specified solid angle
 */
std::vector<Flux> PtSrcGen::generateSources(Flux fluxMin, Flux fluxMax, SolidAngle area, bool doRand)
{
  // Get the number of sources we should generate.  This calculates
  // the expected number of sources above fluxMin, then generates a
  // random number from a Poisson with that mean

  unsigned nSrc = getNSrc(fluxMin, fluxMax, area, doRand);
  COUT("nSrc = " << nSrc);

  std::vector<Flux> fluxes(nSrc);

  // Now generate nSrc sources above fluxMin by generating random
  // numbers between 0 and 1, and calculating the flux that
  // corresponds to that value of the integral dostribution from
  // fluxMin to +inf.

  double rmx = fluxMax.Jy() / fluxMin.Jy();
  double prmx = pow(rmx, 1.0-gamma_);

  for(unsigned i=0; i < nSrc; i++) {
    double r = (double)(rand())/RAND_MAX;

    fluxes[i].setJy(pow(1.0-(1-prmx)*r,1.0/(1.0-gamma_)) * fluxMin.Jy());
  }

  return fluxes;
}

/**.......................................................................
 * Generate a list of source fluxes and positions within the
 * specified x/y box
 */
void PtSrcGen::generateSources(Flux fluxMin, Angle x, Angle y, 
			       std::vector<Flux>&  srcFlux, 
			       std::vector<Angle>& srcX, 
			       std::vector<Angle>& srcY, bool doRand)
{
  SolidAngle area;
  area.setSr(x.radians() * y.radians());

  srcFlux = generateSources(fluxMin, area, doRand);

  // Now that we have fluxes, generate a random location for each
  // source

  srcX.resize(srcFlux.size());
  srcY.resize(srcFlux.size());

  for(unsigned i=0; i < srcFlux.size(); i++) {

    double xr = ((double)(rand())/RAND_MAX - 0.5) * x.radians();
    double yr = ((double)(rand())/RAND_MAX - 0.5) * y.radians();

    srcX[i].setRadians(xr);
    srcY[i].setRadians(yr);
  }

}

/**.......................................................................
 * Generate a list of source fluxes and positions within the
 * specified x/y box
 */
void PtSrcGen::generateSources(Flux fluxMin, Flux fluxMax, Angle x, Angle y, 
			       std::vector<Flux>&  srcFlux, 
			       std::vector<Angle>& srcX, 
			       std::vector<Angle>& srcY, bool doRand)
{
  SolidAngle area;
  area.setSr(x.radians() * y.radians());

  srcFlux = generateSources(fluxMin, fluxMax, area, doRand);

  // Now that we have fluxes, generate a random location for each
  // source

  srcX.resize(srcFlux.size());
  srcY.resize(srcFlux.size());

  for(unsigned i=0; i < srcFlux.size(); i++) {

    double xr = ((double)(rand())/RAND_MAX - 0.5) * x.radians();
    double yr = ((double)(rand())/RAND_MAX - 0.5) * y.radians();

    srcX[i].setRadians(xr);
    srcY[i].setRadians(yr);
  }

}

/**.......................................................................
 * Generate the number of sources to generate
 */
unsigned PtSrcGen::getNSrc(Flux& fluxMin, SolidAngle& area, bool doRand)
{
  // Get the mean number of sources to generate

  COUT("Area is: " << area.sr() << " (sr), k*area = " << k_ * area.sr() <<
       " doRand = " << doRand);

  unsigned nMean = (unsigned)(k_ * (area.sr()/au_.sr()) * pow((fluxMin.Jy()/fu_.Jy()), 1.0-gamma_) / (gamma_ - 1.0));

  COUT("Area is: " << area.sr() << " (sr), k*area = " << k_ * area.sr() << " nmean = " << nMean);

  if(!doRand)
    return nMean;

  // Now generate the actual number of sources from a Poisson
  // distribution of this mean

  unsigned nSrc=0;
  if(nMean < 100) {
    
    // For small n, generate poisson samples
    
    std::vector<unsigned> n = 
      Sampler::generatePoissonSamples(nMean, 1);

    nSrc = n[0];
    
  } else {
    
    // In the limit of large N, poisson(k) --> gauss(k,sqrt(k))
    
    std::vector<double> n = 
      Sampler::generateGaussianSamples(sqrt((double)nMean), 1);

    nSrc = (unsigned)n[0] + nMean;
  }
  
  return nSrc;
}

/**.......................................................................
 * Determine the number of sources to generate, between a min and max
 * flux
 */
unsigned PtSrcGen::getNSrc(Flux& fluxMin, Flux& fluxMax, SolidAngle& area, bool doRand)
{
  // Get the mean number of sources to generate

  COUT("Area is: " << area.sr() << " (sr), k*area = " << k_ * area.sr() <<
       " doRand = " << doRand);

  unsigned nMean = (unsigned)(k_ * (area.sr()/au_.sr()) * (pow((fluxMin.Jy()/fu_.Jy()), 1.0-gamma_) - pow((fluxMax.Jy()/fu_.Jy()), 1.0-gamma_))/ (gamma_ - 1.0));

  COUT("Area is: " << area.sr() << " (sr), k*area = " << k_ * area.sr() << " nmean = " << nMean);

  if(!doRand)
    return nMean;

  // Now generate the actual number of sources from a Poisson
  // distribution of this mean

  unsigned nSrc=0;
  if(nMean < 100) {
    
    // For small n, generate poisson samples
    
    std::vector<unsigned> n = 
      Sampler::generatePoissonSamples(nMean, 1);

    nSrc = n[0];
    
  } else {
    
    // In the limit of large N, poisson(k) --> gauss(k,sqrt(k))
    
    std::vector<double> n = 
      Sampler::generateGaussianSamples(sqrt((double)nMean), 1);

    nSrc = (unsigned)n[0] + nMean;
  }
  
  return nSrc;
}
