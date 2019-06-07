#ifndef CARMA_SERVICES_INTERPOLATOR_H
#define CARMA_SERVICES_INTERPOLATOR_H

#include "carma/services/Types.h"
#include <vector>
#include <cstddef>

extern "C" {
#include <gsl/gsl_errno.h>
#include <gsl/gsl_interp.h>
#include <gsl/gsl_spline.h>
}


namespace carma {
  namespace services {

  class Interpolator {
   public:

      /**
       * Class that wraps the Gnu Scientific Library 
       * interpolation routines. 
       *
       * @param xvalue The x values
       * @param yvalue The y = f(x) values
       * @param type The interpolation type. 
       * One of LINEAR, POLYNOMIAL, CSPLINE, CSPLINE_PERIODIC
       * AKIMA, AKIMA_PERIODIC. 
       */
      Interpolator ( 
          const ::std::vector<double> & xvalue,
          const ::std::vector<double> & yvalue,
          const interpolationType type );

      virtual ~Interpolator ( );

      /**
       * @return The value of the function at x
       * @param x The value at which to interpolate.
       * This method is mutex-protected.
       */
      double evaluate( double x );


   private:
      void freeAllocations();
      void freeGslAllocations();
      void freeDoubleAllocations();
      void initialize();
      /**
       * Set the interpolation type.
       * @param type The interpolation type.
       * One of LINEAR, POLYNOMIAL, CSPLINE, CSPLINE_PERIODIC
       * AKIMA, AKIMA_PERIODIC. 
       */
      void setInterpType( interpolationType type );


      ::gsl_interp_accel  * acc_;
      ::gsl_interp        * interp_;
      ::gsl_spline        * spline_;
      // eventually do away with these if not allowing
      // interp type to be changed after instantiation.
      ::std::vector<double> xval_;
      ::std::vector<double> yval_;
      size_t size_;
      double * x_;
      double * y_;
      double answer_;
      interpolationType     interpType_;
  }; // class Interpolator

} // namespace services
} // namespace carma


#endif //CARMA_SERVICES_INTERPOLATOR_H
    
