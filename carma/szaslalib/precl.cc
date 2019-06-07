#include "slalib.h"
#include "slamac.h"
void slaPrecl ( double ep0, double ep1, double rmatp[3][3] )
/*
**  - - - - - - - - -
**   s l a P r e c l
**  - - - - - - - - -
**
**  Form the matrix of precession between two epochs, using the
**  model of Simon et al (1994), which is suitable for long
**  periods of time.
**
**  (double precision)
**
**  Given:
**     ep0    double         beginning epoch
**     ep1    double         ending epoch
**
**  Returned:
**     rmatp  double[3][3]   precession matrix
**
**  Notes:
**
**  1)  The epochs are TDB (loosely ET) Julian epochs.
**
**  2)  The matrix is in the sense   v(ep1)  =  rmatp * v(ep0) .
**
**  3)  The absolute accuracy of the model is limited by the
**      uncertainty in the general precession, about 0.3 arcsec per
**      1000 years.  The remainder of the formulation provides a
**      precision of 1 mas over the interval from 1000AD to 3000AD,
**      0.1 arcsec from 1000BC to 5000AD and 1 arcsec from
**      4000BC to 8000AD.
**
**  Reference:
**     Simon, J.L., et al., 1994. Astron.Astrophys., 282, 663-683.
**
**  Called:  slaDeuler
**
**  Defined in slamac.h:  DAS2R
**
**  Last revision:   23 August 1994
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double t0, t, tas2r, w, zeta, z, theta;

/* Interval between basic epoch J2000.0 and beginning epoch (1000JY) */
   t0 = ( ep0 - 2000.0 ) / 1000.0;

/* Interval over which precession required (1000JY) */
   t =  ( ep1 - ep0 ) / 1000.0;

/* Euler angles */
   tas2r = t * DAS2R;
   w =       23060.9097 +
             ( 139.7459 +
             ( - 0.0038 +
             ( - 0.5918 +
             ( - 0.0037 +
                 0.0007 * t0 ) * t0 ) * t0 ) * t0 ) * t0;

   zeta =           ( w +
              ( 30.2226 +
             ( - 0.2523 +
             ( - 0.3840 +
             ( - 0.0014 +
                 0.0007 * t0 ) * t0 ) * t0 ) * t0 +
              ( 18.0183 +
             ( - 0.1326 +
               ( 0.0006 +
                 0.0005 * t0 ) * t0 ) * t0 +
             ( - 0.0583 +
             ( - 0.0001 +
                 0.0007 * t0 ) * t0 +
             ( - 0.0285 +
             ( - 0.0002 ) * t ) * t ) * t ) * t ) * t ) * tas2r;

   z =              ( w +
             ( 109.5270 +
               ( 0.2446 +
             ( - 1.3913 +
             ( - 0.0134 +
                 0.0026 * t0 ) * t0 ) * t0 ) * t0 +
              ( 18.2667 +
             ( - 1.1400 +
             ( - 0.0173 +
                 0.0044 * t0 ) * t0 ) * t0 +
             ( - 0.2821 +
             ( - 0.0093 +
                 0.0032 * t0 ) * t0 +
              ( -0.0301 +
                 0.0006 * t0
               - 0.0001 * t ) * t ) * t ) * t ) * t ) * tas2r;

   theta = ( 20042.0207 +
            ( - 85.3131 +
             ( - 0.2111 +
               ( 0.3642 +
               ( 0.0008 +
             ( - 0.0005 ) * t0 ) * t0 ) * t0 ) * t0 ) * t0 +
            ( - 42.6566 +
             ( - 0.2111 +
               ( 0.5463 +
               ( 0.0017 +
             ( - 0.0012 ) * t0 ) * t0 ) * t0 ) * t0 +
            ( - 41.8238 +
               ( 0.0359 +
               ( 0.0027 +
             ( - 0.0001 ) * t0 ) * t0 ) * t0 +
             ( - 0.0731 +
               ( 0.0019 +
                 0.0009 * t0 ) * t0 +
             ( - 0.0127 +
                 0.0011 * t0 + 0.0004 * t ) * t ) * t ) * t ) * t ) * tas2r;

/* Rotation matrix */
   slaDeuler ( "ZYZ", -zeta, theta, -z, rmatp );
}
