#include "slalib.h"
#include "slamac.h"
void slaRefco ( double hm, double tdk, double pmb, double rh,
                double wl, double phi, double tlr, double eps,
                double *refa, double *refb )
/*
**  - - - - - - - - -
**   s l a R e f c o
**  - - - - - - - - -
**
**  Determine constants A and B in atmospheric refraction model
**  dz = A tan z + B tan**3 z (for optical wavelengths).
**
**  z is the "observed" zenith distance (i.e. affected by
**  refraction) and dz is what to add to z to give the "topocentric"
**  (i.e. in vacuo) zenith distance.
**
**  The constants are such that the model agrees precisely with
**  the full integration performed by the slaRefro routine at
**  zenith distances arctan(1) and arctan(4).
**
**  Given:
**    hm    double    height of the observer above sea level (metre)
**    tdk   double    ambient temperature at the observer (deg k)
**    pmb   double    pressure at the observer (millibar)
**    rh    double    relative humidity at the observer (range 0-1)
**    wl    double    effective wavelength of the source (micrometre)
**    phi   double    latitude of the observer (radian, astronomical)
**    tlr   double    temperature lapse rate in the troposphere (degk/metre)
**    eps   double    precision required to terminate iteration (radian)
**
**  Returned:
**    *refa double    tan z coefficient (radian)
**    *refb double    tan**3 coefficient (radian)
**
**  Called:  slaRefro
**
**  Typical values for the tlr and eps arguments might be 0.0065 and
**  1e-10 respectively.
**
**  Relative to the comprehensive refraction model used by this routine,
**  the simple A tan z + B tan**3 z formula achieves 0.5 arcsec accuracy
**  for ZD < 80 deg, 0.01 arcsec accuracy for ZD < 60 deg, and
**  0.001 arcsec accuracy for ZD < 45 deg.
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double r1,r2;

/* Sample zenith distances: arctan(1) and arctan(4) */
   static double atn1 = 0.7853981633974483;
   static double atn4 = 1.325817663668033;

/* Determine refraction for the two sample zenith distances */
   slaRefro ( atn1, hm, tdk, pmb, rh, wl, phi, tlr, eps, &r1 );
   slaRefro ( atn4, hm, tdk, pmb, rh, wl, phi, tlr, eps, &r2 );

/* Solve for refraction constants */
   *refa = ( 64.0 * r1 - r2 ) / 60.0;
   *refb = ( r2 - 4.0 * r1 ) / 60.0;
}
