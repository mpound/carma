#ifndef CARMA_UTIL_COMPLEX_MANIP_H
#define CARMA_UTIL_COMPLEX_MANIP_H
#include <cmath>
#include <complex>
namespace carma {
  namespace util {
      //@todo Possibly make these templates for complex<T>.
      //We've defined visibilities as complex<float> so unclear
      //whether we really need templatized versions.
      /**
       * Compute the visibility amplitude from a complex visibility
       * @param visibility The complex visibility
       */
  
      inline float amp( std::complex<float> visibility )
      {
  	return abs( visibility );
      }
  
      /**
       * Compute the visibility phase from a complex visibility
       * @param visibility The complex visibility
       * @return The phase in degrees
       */
      inline float phase( std::complex<float> visibility ) 
      {
  	return arg(visibility) * 180.0 / M_PI;
      }
  }
}
#endif //CARMA_UTIL_COMPLEX_MANIP_H
