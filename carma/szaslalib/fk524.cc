#include "slalib.h"
#include "slamac.h"
void slaFk524 ( double r2000, double d2000, double dr2000,
                double dd2000, double p2000, double v2000,
                double *r1950, double *d1950, double *dr1950,
                double *dd1950, double *p1950, double *v1950 )
/*
**  - - - - - - - - -
**   s l a F k 5 2 4
**  - - - - - - - - -
**
**  Convert J2000.0 FK5 star data to B1950.0 FK4.
**
**  (double precision)
**
**  This routine converts stars from the new, IAU 1976, FK5, Fricke
**  system, to the old, Bessel-Newcomb, FK4 system.  The precepts
**  of Smith et al (Ref 1) are followed, using the implementation
**  by Yallop et al (Ref 2) of a matrix method due to Standish.
**  Kinoshita's development of Andoyer's post-Newcomb precession is
**  used.  The numerical constants from Seidelmann et al (Ref 3) are
**  used canonically.
**
**  Given:  (all J2000.0,FK5)
**     r2000,d2000      double    J2000.0 RA,Dec (rad)
**     dr2000,dd2000    double    J2000.0 proper motions (rad/Jul.yr)
**     p2000            double    parallax (arcsec)
**     v2000            double    radial velocity (km/s, +ve = moving away)
**
**  Returned:  (all B1950.0,FK4)
**     *r1950,*d1950    double    B1950.0 RA,Dec (rad)
**     *dr1950,*dd1950  double    B1950.0 proper motions (rad/trop.yr)
**     *p1950           double    parallax (arcsec)
**     *v1950           double    radial velocity (km/s, +ve = moving away)
**
**  Notes:
**
**  1)  The proper motions in RA are dRA/dt rather than
**      cos(Dec)*dRA/dt, and are per year rather than per century.
**
**  2)  Note that conversion from Julian epoch 2000.0 to Besselian
**      epoch 1950.0 only is provided for.  Conversions involving
**      other epochs will require use of the appropriate precession,
**      proper motion, and E-terms routines before and/or after
**      FK524 is called.
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

/* Miscellaneous */
   double r, d, ur, ud, px, rv;
   double sr, cr, sd, cd, x, y, z, w;
   double v1[6], v2[6];
   double xd, yd, zd;
   double rxyz, wd, rxysq, rxy;
   int i,j;

/* Radians per year to arcsec per century */
   static double pmf = 100.0 * 60.0 * 60.0 * 360.0 / D2PI;

/* Small number to avoid arithmetic problems */
   static double tiny = 1.0e-30;

/*
** Canonical constants  (see references)
*/

/*
** km per sec to AU per tropical century
** = 86400 * 36524.2198782 / 1.49597870e8
*/
   static double vf = 21.095;

/* Constant vector and matrix (by rows) */
   static double a[6] = { -1.62557e-6,   -0.31919e-6, -0.13843e-6,
                           1.245e-3,     -1.580e-3,   -0.659e-3 };

   static double emi[6][6] =
   {
     {  0.9999256795,              /* emi[0][0] */
        0.0111814828,              /* emi[0][1] */
        0.0048590039,              /* emi[0][2] */
       -0.00000242389840,          /* emi[0][3] */
       -0.00000002710544,          /* emi[0][4] */
       -0.00000001177742 },        /* emi[0][5] */

     { -0.0111814828,              /* emi[1][0] */
        0.9999374849,              /* emi[1][1] */
       -0.0000271771,              /* emi[1][2] */
        0.00000002710544,          /* emi[1][3] */
       -0.00000242392702,          /* emi[1][4] */
        0.00000000006585 },        /* emi[1][5] */

     { -0.0048590040,              /* emi[2][0] */
       -0.0000271557,              /* emi[2][1] */
        0.9999881946,              /* emi[2][2] */
        0.00000001177742,          /* emi[2][3] */
        0.00000000006585,          /* emi[2][4] */
       -0.00000242404995 },        /* emi[2][5] */

     { -0.000551,                  /* emi[3][0] */
        0.238509,                  /* emi[3][1] */
       -0.435614,                  /* emi[3][2] */
        0.99990432,                /* emi[3][3] */
        0.01118145,                /* emi[3][4] */
        0.00485852 },              /* emi[3][5] */

     { -0.238560,                  /* emi[4][0] */
       -0.002667,                  /* emi[4][1] */
        0.012254,                  /* emi[4][2] */
       -0.01118145,                /* emi[4][3] */
        0.99991613,                /* emi[4][4] */
       -0.00002717 },              /* emi[4][5] */

     {  0.435730,                  /* emi[5][0] */
       -0.008541,                  /* emi[5][1] */
        0.002117,                  /* emi[5][2] */
       -0.00485852,                /* emi[5][3] */
       -0.00002716,                /* emi[5][4] */
        0.99996684 }               /* emi[5][5] */
   };

/* Pick up J2000 data (units radians and arcsec/JC) */
   r = r2000;
   d = d2000;
   ur = dr2000 * pmf;
   ud = dd2000 * pmf;
   px = p2000;
   rv = v2000;

/* Spherical to Cartesian */
   sr = sin ( r );
   cr = cos ( r );
   sd = sin ( d );
   cd = cos ( d );

   x = cr * cd;
   y = sr * cd;
   z = sd;

   w = vf * rv * px;

   v1[0] = x;
   v1[1] = y;
   v1[2] = z;

   v1[3] =  - ur * y - cr * sd * ud + w * x;
   v1[4] = ur * x - sr * sd * ud + w * y;
   v1[5] = cd * ud + w * z;

/* Convert position+velocity vector to BN system */
   for ( i = 0; i < 6; i++ ) {
      w = 0.0;
      for ( j = 0; j < 6; j++ ) {
         w += emi[i][j] * v1[j];
      }
      v2[i] = w;
   }

/* Position vector components and magnitude */
   x = v2[0];
   y = v2[1];
   z = v2[2];
   rxyz = sqrt ( x * x + y * y + z * z );

/* Include e-terms */
   w = x * a[0] + y * a[1] + z * a[2];
   x += a[0] * rxyz - w * x;
   y += a[1] * rxyz - w * y;
   z += a[2] * rxyz - w * z;

/* Recompute magnitude */
   rxyz = sqrt ( x * x + y * y + z * z );

/* Apply E-terms to both position and velocity */
   x = v2[0];
   y = v2[1];
   z = v2[2];
   w = x * a[0] + y * a[1] + z * a[2];
   wd = x * a[3] + y * a[4] + z * a[5];
   x += a[0] * rxyz - w * x;
   y += a[1] * rxyz - w * y;
   z += a[2] * rxyz - w * z;
   xd = v2[3] + a[3] * rxyz - wd * x;
   yd = v2[4] + a[4] * rxyz - wd * y;
   zd = v2[5] + a[5] * rxyz - wd * z;

/* Convert to spherical */
   rxysq = x * x + y * y;
   rxy = sqrt ( rxysq );

   if ( ( x == 0.0 ) && ( y == 0.0 ) ) {
      r = 0.0;
   } else {
      r = atan2 ( y, x );
      if ( r < 0.0 ) r += D2PI;
   }
   d = atan2 ( z, rxy );

   if (rxy > tiny) {
      ur = ( x * yd - y * xd ) / rxysq;
      ud = ( zd * rxysq - z * ( x * xd + y * yd ) ) /
           ( ( rxysq + z * z ) * rxy );
   }

/* Radial velocity and parallax */
   if ( px > tiny )
   {
      rv = ( x * xd + y * yd + z * zd ) / ( px * vf * rxyz );
      px = px / rxyz;
   }

/* Return results */
   *r1950 = r;
   *d1950 = d;
   *dr1950 = ur / pmf;
   *dd1950 = ud / pmf;
   *v1950 = rv;
   *p1950 = px;
}
