#include "slalib.h"
#include "slamac.h"
void slaFk425 ( double r1950, double d1950, double dr1950,
                double dd1950, double p1950, double v1950,
                double *r2000, double *d2000, double *dr2000,
                double *dd2000, double *p2000, double *v2000 )
/*
**  - - - - - - - - -
**   s l a F k 4 2 5
**  - - - - - - - - -
**
**  Convert B1950.0 FK4 star data to J2000.0 FK5.
**
**  (double precision)
**
**  This routine converts stars from the old, Bessel-Newcomb, FK4
**  system to the new, IAU 1976, FK5, Fricke system.  The precepts
**  of Smith et al (Ref 1) are followed, using the implementation
**  by Yallop et al (Ref 2) of a matrix method due to Standish.
**  Kinoshita's development of Andoyer's post-Newcomb precession is
**  used.  The numerical constants from Seidelmann et al (Ref 3) are
**  used canonically.
**
**  Given:  (all B1950.0,FK4)
**
**     r1950,d1950     double    B1950.0 RA,dec (rad)
**     dr1950,dd1950   double    B1950.0 proper motions (rad/trop.yr)
**     p1950           double    parallax (arcsec)
**     v1950           double    radial velocity (km/s, +ve = moving away)
**
**  Returned:  (all J2000.0,FK5)
**
**     *r2000,*d2000   double    J2000.0 RA,dec (rad)
**     *dr2000,*dd2000 double    J2000.0 proper motions (rad/jul.yr)
**     *p2000          double    parallax (arcsec)
**     *v2000          double    radial velocity (km/s, +ve = moving away)
**
**  Notes:
**
**  1)  The proper motions in RA are dRA/dt rather than
**      cos(Dec)*dRA/dt, and are per year rather than per century.
**
**  2)  Conversion from Besselian epoch 1950.0 to Julian epoch
**      2000.0 only is provided for.  Conversions involving other
**      epochs will require use of the appropriate precession,
**      proper motion, and E-terms routines before and/or
**      after FK425 is called.
**
**  3)  In the FK4 catalogue the proper motions of stars within
**      10 degrees of the poles do not embody the differential
**      E-term effect and should, strictly speaking, be handled
**      in a different manner from stars outside these regions.
**      However, given the general lack of homogeneity of the star
**      data available for routine astrometry, the difficulties of
**      handling positions that may have been determined from
**      astrometric fields spanning the polar and non-polar regions,
**      the likelihood that the differential E-terms effect was not
**      taken into account when allowing for proper motion in past
**      astrometry, and the undesirability of a discontinuity in
**      the algorithm, the decision has been made in this routine to
**      include the effect of differential E-terms on the proper
**      motions for all stars, whether polar or not.  At epoch 2000,
**      and measuring on the sky rather than in terms of dRA, the
**      errors resulting from this simplification are less than
**      1 milliarcsecond in position and 1 milliarcsecond per
**      century in proper motion.
**
**  References:
**
**     1  Smith, C.A. et al, 1989.  "The transformation of astrometric
**        catalog systems to the equinox J2000.0".  Astron.J. 97, 265.
**
**     2  Yallop, B.D. et al, 1989.  "Transformation of mean star places
**        from FK4 B1950.0 to FK5 J2000.0 using matrices in 6-space".
**        Astron.J. 97, 274.
**
**     3  Seidelmann, P.K. (ed), 1992.  "Explanatory Supplement to
**        the Astronomical Almanac", ISBN 0-935702-68-7.
**
**  Defined in slamac.h:  D2PI
**
**  Last revision:   20 December 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double r, d, ur, ud, px, rv, sr, cr, sd, cd, w, wd,
          x, y, z, xd, yd, zd,
          rxysq, rxyzsq, rxy, rxyz, spxy, spxyz;
   int i, j;

/* Star position and velocity vectors */
   double r0[3], rd0[3];

/* Combined position and velocity vectors */
   double v1[6], v2[6];

/* Radians per year to arcsec per century */
   static double pmf = 100.0 * 60.0 * 60.0 * 360.0 / D2PI;

/* Small number to avoid arithmetic problems */
   double tiny = 1.0e-30;

/*
** Canonical constants  (see references)
*/

/*
** km per sec to au per tropical century
** = 86400 * 36524.2198782 / 1.49597870e8
*/
   double vf = 21.095;

/* Constant vector and matrix (by rows) */
   static double a[3]  = { -1.62557e-6,  -0.31919e-6, -0.13843e-6 };
   static double ad[3] = {  1.245e-3,     -1.580e-3,   -0.659e-3  };
   static double em[6][6] =
   {
     {  0.9999256782,              /* em[0][0] */
       -0.0111820611,              /* em[0][1] */
       -0.0048579477,              /* em[0][2] */
        0.00000242395018,          /* em[0][3] */
       -0.00000002710663,          /* em[0][4] */
       -0.00000001177656 },        /* em[0][5] */

     {  0.0111820610,              /* em[1][0] */
        0.9999374784,              /* em[1][1] */
       -0.0000271765,              /* em[1][2] */
        0.00000002710663,          /* em[1][3] */
        0.00000242397878,          /* em[1][4] */
       -0.00000000006587 },        /* em[1][5] */

     {  0.0048579479,              /* em[2][0] */
       -0.0000271474,              /* em[2][1] */
        0.9999881997,              /* em[2][2] */
        0.00000001177656,          /* em[2][3] */
       -0.00000000006582,          /* em[2][4] */
        0.00000242410173 },        /* em[2][5] */

     { -0.000551,                  /* em[3][0] */
       -0.238565,                  /* em[3][1] */
        0.435739,                  /* em[3][2] */
        0.99994704,                /* em[3][3] */
       -0.01118251,                /* em[3][4] */
       -0.00485767 },              /* em[3][5] */

     {  0.238514,                  /* em[4][0] */
       -0.002667,                  /* em[4][1] */
       -0.008541,                  /* em[4][2] */
        0.01118251,                /* em[4][3] */
        0.99995883,                /* em[4][4] */
       -0.00002718 },              /* em[4][5] */

     { -0.435623,                  /* em[5][0] */
        0.012254,                  /* em[5][1] */
        0.002117,                  /* em[5][2] */
        0.00485767,                /* em[5][3] */
       -0.00002714,                /* em[5][4] */
        1.00000956 }               /* em[5][5] */
   };

/* Pick up B1950 data (units radians and arcsec/tc) */
   r = r1950;
   d = d1950;
   ur = dr1950 * pmf;
   ud = dd1950 * pmf;
   px = p1950;
   rv = v1950;

/* Spherical to Cartesian */
   sr = sin ( r );
   cr = cos ( r );
   sd = sin ( d );
   cd = cos ( d );

   r0[0] = cr * cd;
   r0[1] = sr * cd;
   r0[2] = sd;

   w = vf * rv * px;

   rd0[0] = ( -sr * cd * ur ) - ( cr * sd * ud ) + ( w * r0[0] );
   rd0[1] = ( cr * cd * ur ) - ( sr * sd * ud ) + ( w * r0[1] );
   rd0[2] = ( cd * ud ) + ( w * r0[2] );

/* Allow for e-terms and express as position+velocity 6-vector */
   w = ( r0[0] * a[0] ) + ( r0[1] * a[1] ) + ( r0[2] * a[2] );
   wd = ( r0[0] * ad[0] ) + ( r0[1] * ad[1] ) + ( r0[2] * ad[2] );

   for ( i = 0; i < 3; i++ ) {
      v1[i] = r0[i]  - a[i]  + w * r0[i];
      v1[i+3] = rd0[i] - ad[i] + wd * r0[i];
   }

/* Convert position+velocity vector to Fricke system */
   for ( i = 0; i < 6; i++ ) {
      w = 0.0;
      for ( j = 0; j < 6; j++ ) {
         w += em[i][j] * v1[j];
      }
      v2[i] = w;
   }

/* Revert to spherical coordinates */
   x = v2[0];
   y = v2[1];
   z = v2[2];
   xd = v2[3];
   yd = v2[4];
   zd = v2[5];

   rxysq = ( x * x ) + ( y * y );
   rxyzsq = ( rxysq ) + ( z * z );
   rxy = sqrt ( rxysq );
   rxyz = sqrt (  rxyzsq );

   spxy = ( x * xd ) + ( y * yd );
   spxyz = spxy + ( z * zd );

   if ( (x == 0.0) && (y == 0.0) )
      r = 0.0;
   else {
      r = atan2 ( y, x );
      if ( r < 0.0 )
         r += D2PI;
   }
   d = atan2 ( z, rxy );

   if ( rxy > tiny ) {
      ur = ( ( x * yd ) - ( y * xd ) ) / rxysq;
      ud = ( ( zd * rxysq ) - ( z * spxy ) ) / ( rxyzsq * rxy );
   }

   if ( px > tiny ) {
      rv = spxyz / ( px * rxyz * vf );
      px = px / rxyz;
   }

/* Return results */
   *r2000 = r;
   *d2000 = d;
   *dr2000 = ur / pmf;
   *dd2000 = ud / pmf;
   *v2000 = rv;
   *p2000 = px;
}
