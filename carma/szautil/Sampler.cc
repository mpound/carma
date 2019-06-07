#include "carma/szautil/Exception.h"
#include "carma/szautil/Sampler.h"

#include <cmath>
#include <cstdlib>
#include <cstdio>

using namespace std;
using namespace sza::util;

static int gcf(double *gammcf, double a, double x, double *gln);
static int gser(double *gamser, double a, double x, double *gln);
static int gammp(double a, double x, double *val);

/**.......................................................................
 * Constructor.
 */
Sampler::Sampler() 
{
  haveFn_ = false;
  isFn_   = false;
}

/**.......................................................................
 * Destructor.
 */
Sampler::~Sampler() {}

/**.......................................................................
 * Set the function we will use to generate samples
 */
void Sampler::setdYdX(SAMPLER_FN(*fn), double xmin, double xmax, double dx)
{
  unsigned nSamp = (unsigned)((xmax - xmin)/dx) + 1;

  std::vector<double> x, y;

  x.resize(nSamp);
  y.resize(nSamp);

  for(unsigned i = 0; i < nSamp; i++) {
    x[i] = xmin + i*dx;
    y[i] = fn(x[i]);
  }

  setdYdX(x, y);
}

/**.......................................................................
 * Set the function we will use to generate samples
 */
void Sampler::setdYdX(std::vector<double>& x, std::vector<double>& y)
{
  setdYdX(x.size(), &x[0], &y[0]);
}

/**.......................................................................
 * Set the function we will use to generate samples
 */
void Sampler::setdYdX(unsigned n, double* x, double* y)
{
  haveFn_ = true;
  isFn_   = true;

  nPt_ = n+1;

  // Resize the integral version

  xInt_.resize(nPt_);
  yInt_.resize(nPt_);

  // Integrate the function now

  double norm = 0.0;
  double dx = x[1]-x[0];

  for(unsigned i=0; i < n; i++) {
    norm += y[i];
  }

  xInt_[0] = x[0]-dx/2;
  for(unsigned i=1; i < n; i++) {
    xInt_[i] = x[i]+dx/2;
  }

  yInt_[0] = 0.0;
  for(unsigned i=1; i < n; i++) {
    yInt_[i] = yInt_[i-1] + y[i]/norm;
  }
}

/**.......................................................................
 * Generate samples according to the sampling function we were passed
 */
std::vector<double> Sampler::generateSamples(unsigned nSamp)
{
  if(nPt_ == 0)
    ThrowError("No sampling function has been specified");

  std::vector<double> samples(nSamp);

  if(!isFn_) {
    for(unsigned i=0; i < nSamp; i++) {
      samples[i] = binSearchForSample();
    }
  } else {
    
  }

  return samples;
}

/**.......................................................................
 * Binary search for the closest sample in the y-array.
 */
double Sampler::binSearchForSample()
{
  double y=(double)(rand())/RAND_MAX;

  double yHi, yLo, yMid;
  unsigned lo=0, hi=nPt_-1, mid;

  while(hi - lo > 1) {

    mid = (hi+lo)/2;

    yHi  = yInt_[hi];
    yLo  = yInt_[lo];
    yMid = yInt_[mid];

    if(y > yMid) {
      lo = mid;
    } else if(y < yMid) {
      hi = mid;
    } else {
      lo = hi = mid;
    }
  }

  if(lo == hi)
    return yInt_[hi];
  else
    return xInt_[lo] + (xInt_[hi] - xInt_[lo])/(yInt_[hi] - yInt_[lo]) * 
      (y-yInt_[lo]);
}

void Sampler::seed(unsigned int s)
{
  COUT("Seeding with: " << s);
  srand(s);
}

void Sampler::seedRandom()
{
  int r = rand();
  srand((unsigned int)r);
}

/**.......................................................................
 *Generate poisson samples
 */
std::vector<unsigned> Sampler::
generatePoissonSamples(double mean, unsigned ndev)
{
  int go=1,waserr=0,i;
  double y,ysmall,ymid,ylarge;
  
  std::vector<unsigned> outvals(ndev);

  // Generate the deviates here.
  // Test by doing a binary search.
  
  for(i=0;i < ndev && !waserr;i++) {
    
    double nlarge=10*mean,nsmall=1;
    double nmid=(nlarge+nsmall)/2,nrange=(nlarge-nsmall)/2;
    double nclose;
    
    y = (double)rand()/RAND_MAX;
    
    // First find values of nsmall and nlarge which bracket the value.

    do {
      waserr |= gammp(nsmall, mean, &ysmall);
      waserr |= gammp(nlarge, mean, &ylarge);
      
      ysmall = 1.0-ysmall;
      ylarge = 1.0-ylarge;
      
      if(!waserr) {
	
	// If y is above the range, increase the range until it is
	// bracketed.  Note that y can't be below the range, since
	// gammp(0,mean) = 0.0

	if(y > ylarge) {
	  nsmall = nlarge;
	  nlarge += nrange;
	} else {
	  go = 0;
	}
	
	nrange = (nlarge-nsmall)/2;
	nmid = (nlarge+nsmall)/2;
	
      }
    } while(go && !waserr);
    
    // Now we have bracketing values.  DO a binary search to narrow
    // down the range.

    while(nrange > 0.1 && !waserr) {
      
      waserr |= gammp(nsmall, mean, &ysmall);
      waserr |= gammp(nmid,   mean, &ymid);
      waserr |= gammp(nlarge, mean, &ylarge);
      
      ysmall = 1.0-ysmall;
      ymid = 1.0-ymid;
      ylarge = 1.0-ylarge;
      
      if(!waserr) {
	if(y > ymid)
	  nsmall = nmid;
	else if(y < ymid)
	  nlarge = nmid;
	else
	  nsmall = nlarge = nmid;
	
	nrange = (nlarge-nsmall);
	nmid = (nlarge+nsmall)/2;
	nclose = fabs(y-ysmall) < fabs(y-ylarge) ? nsmall : nlarge;
      }
    }
    
    // When we exit the above loop, nmid should be the value we are
    // looking for.

    if(!waserr)
      outvals[i] = (unsigned)(nclose);
  }

  if(waserr) {
    ThrowError("Error in generatePoissonSamples");
  }

  return outvals;
}


/*.......................................................................
 * Incomplete gamma function, cribbed from NR.
 */
static int gammp(double a, double x, double *val)
{
  double gln;
  int waserr=0;
  
  if (x < 0.0 || a <= 0.0) {
    fprintf(stderr,"gammp: Invalid arguments.\n");
    return 1;
  }

  if (x < (a+1.0)) {
    waserr = gser(val,a,x,&gln);
  } else {
    waserr = gcf(val,a,x,&gln);
    *val = 1.0-*val;
  }

  return waserr;
}

/*.......................................................................
 * Evaluates the incomplete gamma function by its series representation.
 * Cribbed from NR.
 */
static int gser(double *gamser, double a, double x, double *gln)
{
#define GSER_ITMAX 100
#define GSER_EPS 3.0e-7
  int n;
  double sum,del,ap;

  *gln = Sampler::lnGamma(a);

  if (x <= 0.0) {
    if (x < 0.0) {
      fprintf(stderr,"gser: x less than 0.\n");
      return 1;
    }
    *gamser=0.0;
    return 0;
  }
  else {
    ap=a;
    del=sum=1.0/a;
    for (n=1;n<=GSER_ITMAX;n++) {
      ++ap;
      del *= x/ap;
      sum += del;
      if (fabs(del) < fabs(sum)*GSER_EPS) {
        *gamser=sum*exp(-x+a*log(x)-(*gln));
        return 0;
      }
    }
    fprintf(stderr,"gser: a too large, GSER_ITMAX too small.\n");
    return 1;
  }
}


/*.......................................................................
 * Evaluate the incomplete gamma function by continued fraction
 * representation.
 *
 * Cribbed from NR.
 */
static int gcf(double *gammcf, double a, double x, double *gln)
{
#define GCF_FPMIN 1.0e-30
#define GCF_ITMAX 100
#define GCF_EPS 3.0e-7
  int i;
  double an,b,c,d,del,h;

  *gln = Sampler::lnGamma(a);

  b=x+1.0-a;
  c=1.0/GCF_FPMIN;
  d=1.0/b;
  h=d;
  for (i=1;i<=GCF_ITMAX;i++) {
    an = -i*(i-a);
    b += 2.0;
    d=an*d+b;
    if (fabs(d) < GCF_FPMIN) d=GCF_FPMIN;
    c=b+an/c;
    if (fabs(c) < GCF_FPMIN) c=GCF_FPMIN;
    d=1.0/d;
    del=d*c;
    h *= del;
    if (fabs(del-1.0) < GCF_EPS) break;
  }
  if (i > GCF_ITMAX) {
    fprintf(stderr,"gcf: a too large, GCF_ITMAX too small\n.");
    return 1;
  }
  *gammcf=exp(-x+a*log(x)-(*gln))*h;
  return 0;
}

/*.......................................................................
 * This function returns a random number, from a gaussian distribution
 * of standard deviation, num.
 */
std::vector<double> Sampler::
generateGaussianSamples(double sigma, unsigned nSamp)
{
  double rrad;
  double aval, bval, rand_num;
  
  std::vector<double> samples(nSamp);

  for(unsigned i=0; i < nSamp; i++) {

    // Acquire two uniform random numbers between -1 and 1.
    
    do {
      aval=(double)(rand())/RAND_MAX*2-1;
      bval=(double)(rand())/RAND_MAX*2-1;
      
      // The Box-Muller transformation to convert uniform to gaussian
      // deviates requires that the two deviates be converted to a
      // radius squared. The value of the radius must be less than one
      // and not equal to zero.
      
      rrad = aval*aval + bval*bval;
      
    } while(rrad >= 1 || rrad == 0.0);
    
    // Apply the Box-Muller transformation to turn the random square
    // radius found above into two Gaussian deviates.
    
    samples[i] = sigma * aval * sqrt(-2.0 * log((double) rrad)/rrad);
  }

  return samples;
}

/*.......................................................................
 * Series representation for the gamma function.
 */
double Sampler::lnGamma(double xx)
{
  double x,y,tmp,ser;

  static double cof[6]={76.18009172947146,-86.50532032941677,
                          24.01409824083091,-1.231739572450155,
                          0.1208650973866179e-2,-0.5395239384953e-5};
  int j;

  y=x=xx;
  tmp=x+5.5;
  tmp -= (x+0.5)*log(tmp);
  ser=1.000000000190015;
  for (j=0;j<=5;j++) ser += cof[j]/++y;
  return -tmp+log(2.5066282746310005*ser/x);
}

/**.......................................................................
 * Return the log of the factorial of n.  For large n, we use 
 *
 *   n! = Gamma(n+1)
 *
 * with a series approximation for the gamma function
 */
double Sampler::lnFactrl(unsigned n)
{
  static int ntop=4;
  int j;
  
  if(n > 32) {
    return lnGamma(n+1.0);
  }

  double sum = 0.0;
  for(unsigned i=0; i < n;i++) {
    sum += log((double)(n-i));
  }

  return sum;
}

/**.......................................................................
 * Return the Poisson pdf for k events given a mean of lambda:
 *
 *          e^-l * l^k
 * p(k,l) = ----------
 *               k!
 *
 * So ln(p) = -l + k*ln(l) - ln(k!)
 *
 */ 
double Sampler::poissPdf(unsigned k, double lambda)
{
  double lnp = -lambda + k * log(lambda) - lnFactrl(k);
  return exp(lnp);
}
