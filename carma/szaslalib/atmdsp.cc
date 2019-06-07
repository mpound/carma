#include "slalib.h"
#include "slamac.h"
void slaAtmdsp ( double tdk, double pmb, double rh, double wl1,
                 double a1, double b1, double wl2, double *a2, double *b2 )
/*
**  - - - - - - - - - -
**   s l a A t m d s p
**  - - - - - - - - - -
**
**  Apply atmospheric-dispersion adjustments to refraction coefficients.
**
**  Given:
**     tdk   double   ambient temperature, degrees K
**     pmb   double   ambient pressure, millibars
**     rh    double   ambient relative humidity, 0-1
**     wl1   double   reference wavelength, micrometre (0.4 recommended)
**     a1    double   refraction coefficient A for wavelength wl1 (radians)
**     b1    double   refraction coefficient B for wavelength wl1 (radians)
**     wl2   double   wavelength for which adjusted A,B required
**
**  Returned:
**     *a2   double   refraction coefficient A for wavelength wl2 (radians)
**     *b2   double   refraction coefficient B for wavelength wl2 (radians)
**
**  Notes:
**
**  1  To use this routine, first call slaRefco specifying wl1 as the
**     wavelength.  This yields refraction coefficients a1,b1, correct
**     for that wavelength.  Subsequently, calls to slaAtmdsp specifying
**     different wavelengths will produce new, slightly adjusted
**     refraction coefficients which apply to the specified wavelength.
**
**  2  Most of the atmospheric dispersion happens between 0.7 micrometre
**     and the UV atmospheric cutoff, and the effect increases strongly
**     towards the UV end.  For this reason a blue reference wavelength
**     is recommended, for example 0.4 micrometres.
**
**  3  The accuracy, for this set of conditions:
**
**        height above sea level    2000 m
**                      latitude    29 deg
**                      pressure    793 mB
**                   temperature    17 degC
**                      humidity    50%
**                    lapse rate    0.0065 degC/m
**          reference wavelength    0.4 micrometre
**                star elevation    15 deg
**
**     is about 2.5 mas RMS between 0.3 and 1.0 micrometres, and stays
**     within 4 mas for the whole range longward of 0.3 micrometres
**     (compared with a total dispersion from 0.3 to 20.0 micrometres
**     of about 11 arcsec).  These errors are typical for ordinary
**     conditions and the given elevation;  in extreme conditions values
**     a few times this size may occur, while at higher elevations the
**     errors become much smaller.
**
**  4  If either wavelength exceeds 100 micrometres, the radio case
**     is assumed and the returned refraction coefficients are the
**     same as the given ones.
**
**  5  The algorithm consists of calculation of the refractivity of the
**     air at the observer for the two wavelengths, using the methods
**     of the slaRefro routine, and then scaling of the two refraction
**     coefficients according to classical refraction theory.  This
**     amounts to scaling the A coefficient in proportion to (n-1) and
**     the B coefficient almost in the same ratio (see R.M.Green,
**     "Spherical Astronomy", Cambridge University Press, 1985).
**
**  Last revision:   25 June 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double tdkok, pmbok, rhok, psat, pwo, w1, wlok, wlsq, w2, dn1, dn2, f;
 
 
/* Check for radio wavelengths. */
   if ( wl1 > 100.0 || wl2 > 100.0 ) {
 
   /* Radio: no dispersion. */
      *a2 = a1;
      *b2 = b1;
   } else {
 
   /* Optical: keep arguments within safe bounds. */
      tdkok = gmax ( tdk, 100.0 );
      tdkok = gmin ( tdkok, 500.0 );
      pmbok = gmax ( pmb, 0.0 );
      pmbok = gmin ( pmbok, 10000.0 );
      rhok  = gmax ( rh, 0.0 );
      rhok  = gmin ( rhok, 1.0 );

   /* Atmosphere parameters at the observer. */
      psat = pow ( 10.0, -8.7115 + 0.03477 * tdkok );
      pwo = rhok * psat;
      w1 = 11.2684e-6 * pwo;
 
   /* Refractivity at the observer for first wavelength. */
      wlok  = gmax ( wl1, 0.1 );
      wlsq = wlok * wlok;
      w2 = 77.5317e-6 + ( 0.43909e-6 + 0.00367e-6 / wlsq ) / wlsq;
      dn1 = ( w2 * pmbok - w1 ) / tdkok;
 
   /* Refractivity at the observer for second wavelength. */
      wlok  = gmax ( wl2, 0.1 );
      wlsq = wlok * wlok;
      w2 = 77.5317e-6 + ( 0.43909e-6 + 0.00367e-6 / wlsq ) / wlsq;
      dn2 = ( w2 * pmbok - w1 ) / tdkok;
 
   /* Scale the refraction coefficients (see Green 4.31, p93). */
      if ( dn1 != 0.0 ) {
         f = dn2 / dn1;
         *a2 = a1 * f;
         *b2 = b1 * f;
         if ( dn1 != a1 )
            *b2 = *b2 * ( 1.0 + dn1 * ( dn1 - dn2 ) /
                                      ( 2.0 * ( dn1 - a1 ) ) );
      } else {
         *a2 = a1;
         *b2 = b1;
      }
   }
}
