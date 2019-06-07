// $Id: Sampler.h,v 1.1 2010/12/13 21:06:32 eml Exp $

#ifndef SZA_UTIL_SAMPLER_H
#define SZA_UTIL_SAMPLER_H

/**
 * @file Sampler.h
 * 
 * Tagged: Fri Mar 27 15:41:06 PDT 2009
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:32 $
 * 
 * @author username: Command not found.
 */

#define SAMPLER_FN(fn) double (fn)(double x)

#include <vector>

namespace sza {
  namespace util {

    class Sampler {
    public:

      /**
       * Constructor.
       */
      Sampler();

      /**
       * Destructor.
       */
      virtual ~Sampler();

      //------------------------------------------------------------
      // Methods for specifying the sampling function
      //------------------------------------------------------------

      // Methods for specifying the sampling function, as two arrays
      // specifying dy/dx
      
      void setdYdX(unsigned n, double* x, double* y);
      void setdYdX(std::vector<double>& x, std::vector<double>& y);

      // Method for specifying the sampling dy/dx as a function

      void setdYdX(SAMPLER_FN(fn), double xMin, double xMax, double dx);

      // Method for specifying the integral of dy/dx (Y(x' > x))
      // directly

      void setYX(SAMPLER_FN(fn));

      //------------------------------------------------------------
      // Generate samples according to the specified sampling function
      //------------------------------------------------------------

      std::vector<double> generateSamples(unsigned nSamp);

      //------------------------------------------------------------
      // Generate poisson samples
      //------------------------------------------------------------

      static std::vector<unsigned> 
	generatePoissonSamples(double mean, unsigned nSamp);

      //------------------------------------------------------------
      // Generate Gaussian samples
      //------------------------------------------------------------

      static std::vector<double> 
	generateGaussianSamples(double sigma, unsigned nSamp);

      //------------------------------------------------------------
      // Seed the random number generator
      //------------------------------------------------------------

      static void seed(unsigned int s);
      static void seedRandom();

      //------------------------------------------------------------
      // Utility functions
      //------------------------------------------------------------

      // Return the natural log of the gamma function (series
      // approximation)

      static double lnGamma(double x);

      // Return the natural log of the factorial of n.  Exact for
      // small n, usese series approximation to the Gamma functions
      // for large n

      static double lnFactrl(unsigned n);

      // Return the value of the Poisson pdf, for k events, given a
      // mean of lambda

      static double poissPdf(unsigned k, double lambda);

    private:

      // Binary search for samples

      double binSearchForSample();

      // True if we have a function to integrate

      bool haveFn_;

      // True if the integrated function is specified numerically, or
      // as a functional form

      bool isFn_;

      // The number of points in our integrated function

      unsigned nPt_;

      // Integral version of the above

      std::vector<double> yInt_;      
      std::vector<double> xInt_;

    }; // End class Sampler

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_SAMPLER_H
