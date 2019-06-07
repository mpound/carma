#include "carma/services/mathFunctions.h"
#include "carma/util/IllegalArgumentException.h"
#include <cmath>

/** 
 * Jinc calculates the jinc function,  J1(x)/x,  where J1 is the
 * Bessel function of the first kind of order 1, using
 * polynomial approximations. See Abramowitz and Stegun, Handbook of
 * Mathematical Functions, sections 9.4.4 and 9.4.6, page 370.
 */
double 
carma::services::jinc(double arg)
{
    // this is cut'n'paste from MIRIAD's j1xbyx.for
//
//  for the range  0 < abs(arg) < 3, max. error is 1.3 e-8
//
    double j1xbyx = 0.0;
    double x = 0.0;
    if ( arg == 0 ) 
	throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,
		"Jinc(0) is undefined");
    if ( fabs(arg) < 3.0 ) {
        x = (arg/3.)*(arg/3.);
	// by nesting the parentheses below, we get successive powers of x.
        j1xbyx = 0.5 + x*(	- 0.56249985
     		     + x*(	+ 0.21093573
     		     + x*(	- 0.03954289
     		     + x*(	+ 0.00443319
     		     + x*(	- 0.00031761
     		     + x*(	+ 0.00001109))))));
//
//  for the range  abs(arg) > 3, max. error is 1. e-7
//
     } else {
	x = 3./fabs(arg);
	// by nesting the parentheses below, we get successive powers of x.
	double F = 0.79788456	
	                + x*( +.00000156
     			+ x*( +.01659667
     			+ x*( +.00017105
     			+ x*( -.00249511
     			+ x*( +.00113653
     			+ x*( -.00020033 ))))));
        double T = arg - 2.35619449
     			+ x*( .12499612
     			+ x*( .00005650
     			+ x*(-.00637879
     			+ x*( .00074348
     			+ x*( .00079824
     			+ x*(-.00029166 ))))));
	j1xbyx = F*cos(T)/arg/sqrt(arg);
     }
     return j1xbyx;
}

/**
 * Bracewell's jinc function = J1(pi*x)/(2*x)
 */
double 
carma::services::bracewellJinc(double x)
{
	return ( M_PI_2*jinc(M_PI*x) );
}

/** 
 * Calculate Bracewell's "Chinese hat" function:<br>
 * <tt>
 *  chat(x) = 0.5*(acos(x)-abs(x)*sqrt(1-x*x))
 *  </tt><br>
 * This is the autocorrelation of uniform disks.
 * @return chat(x) if |x| <= 1.0, otherwise return zero.
 */
double 
carma::services::chat(double x) 
{
    double retVal = 0.0;
    double t = fabs(x);
    if ( t <= 1.0 ) {
        retVal = 0.5*( acos(t) - t*sqrt( (1.0-t*t) ) );
    }
    return retVal;
}

double 
carma::services::pchat(double x) 
{
    double retVal = 0.0;
    double t = fabs(x);
    if ( t <= 1.0 ) {
    // Miriad's polynomial approximation to chat.
    // this is cut'n'paste from MIRIAD's j1xbyx.for
    // It's actually quite a poor approximation....
      retVal = 0.78639  + t * (-1.02669 
			+ t * ( 0.15909
			+ t * (-0.16691 
			+ t *   0.24491 )));
    }
    return retVal;
}
