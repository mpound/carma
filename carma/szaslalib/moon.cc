#include "slalib.h"
#include "slamac.h"
void slaMoon ( int iy, int id, float fd, float pv[6] )
/*
**  - - - - - - - -
**   s l a M o o n
**  - - - - - - - -
**
**  Approximate geocentric position and velocity of the Moon
**  (single precision).
**
**  Given:
**     iy       int        year
**     id       int        day in year (1 = Jan 1st)
**     fd       float      fraction of day
**
**  Returned:
**     pv       float[6]   Moon position & velocity vector
**
**  Notes:
**
**  1  The date and time is TDB (loosely ET) in a Julian calendar
**     which has been aligned to the ordinary Gregorian
**     calendar for the interval 1900 March 1 to 2100 February 28.
**     The year and day can be obtained by calling slaCalyd or
**     slaClyd.
**
**  2  The Moon 6-vector is Moon centre relative to Earth centre,
**     mean equator and equinox of date.  Position part, pv[0-2],
**     is in AU;  velocity part, pv[3-5], is in AU/sec.
**
**  3  The position is accurate to better than 0.5 arcminute
**     in direction and 1000 km in distance.  The velocity
**     is accurate to better than 0.5"/hour in direction and
**     4 m/s in distance.  (RMS figures with respect to JPL DE200
**     for the interval 1960-2025 are 14 arcsec and 0.2 arcsec/hour in
**     longitude, 9 arcsec and 0.2 arcsec/hour in latitude, 350 km and
**     2 m/s in distance.)  Note that the distance accuracy is
**     comparatively poor because this routine is principally intended
**     for computing topocentric direction.
**
**  4  This routine is only a partial implementation of the original
**     Meeus algorithm (reference below), which offers 4 times the
**     accuracy in direction and 30 times the accuracy in distance
**     when fully implemented (as it is in slaDmoon).
**
**  Reference:
**     Meeus, l'Astronomie, June 1984, p348.
**
**  Defined in slamac.h:  dmod
**
**  Last revision:   19 March 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

#define D2R 0.01745329252f            /* Degrees to radians            */

#define RATCON 9.652743551e-12f       /* Rate conversion factor:       */
                                      /* D2R * D2R / (86400 * 365.25)  */

#define ERADAU 4.2635212653763e-5f    /* Earth equatorial radius in AU */
                                      /*   ( = 6378.137 / 149597870 )  */

{
   int iy4, n;
   float yi, yf, t, elp, em, emp, d, f, v, dv, emn, empn, dn, fn, coeff,
         theta, el, del, b, db, p, dp, sp, r, dr, x, y, z, xd, yd, zd,
         sel, cel, sb, cb, rcb, rbd, w, eps, sineps, coseps;

/*
**  Coefficients for fundamental arguments
**
**  Fixed term (deg), term in t (deg & whole revs + fraction per year)
**
**  Moon's mean longitude
*/
   static float elp0  = 270.434164f;
   static float elp1  = 4812.678831f;
   static float elp1i = 4680.0f;
   static float elp1f = 132.678831f;

/* Sun's mean anomaly */
   static float em0  = 358.475833f;
   static float em1  = 359.990498f;
   static float em1f = 359.990498f;

/* Moon's mean anomaly */
   static float emp0  = 296.104608f;
   static float emp1  = 4771.988491f;
   static float emp1i = 4680.0f;
   static float emp1f = 91.988491f;

/* Moon's mean elongation */
   static float d0 = 350.737486f;
   static float d1 = 4452.671142f;
   static float d1i = 4320.0f;
   static float d1f = 132.671142f;

/* Mean distance of the Moon from its ascending node */
   static float f0 = 11.250889f;
   static float f1 = 4832.020251f;
   static float f1i = 4680.0f;
   static float f1f = 152.020251f;

/*
**  Coefficients for Moon longitude, latitude, parallax series
*/
   struct term {
      float coef;      /* coefficient of L, B or P term (deg) */
      int nem;         /* multiple of M  in argument          */
      int nemp;        /*     "    "  M'  "    "              */
      int nd;          /*     "    "  D   "    "              */
      int nf;          /*     "    "  F   "    "              */
   };

/*
** Longitude                      coeff       M    M'   D    F
*/
   static struct term tl[] = { {  6.288750f,    0,   1,   0,   0 },
                               {  1.274018f,    0,  -1,   2,   0 },
                               {  0.658309f,    0,   0,   2,   0 },
                               {  0.213616f,    0,   2,   0,   0 },
                               { -0.185596f,    1,   0,   0,   0 },
                               { -0.114336f,    0,   0,   0,   2 },
                               {  0.058793f,    0,  -2,   2,   0 },
                               {  0.057212f,   -1,  -1,   2,   0 },
                               {  0.053320f,    0,   1,   2,   0 },
                               {  0.045874f,   -1,   0,   2,   0 },
                               {  0.041024f,   -1,   1,   0,   0 },
                               { -0.034718f,    0,   0,   1,   0 },
                               { -0.030465f,    1,   1,   0,   0 },
                               {  0.015326f,    0,   0,   2,  -2 },
                               { -0.012528f,    0,   1,   0,   2 },
                               { -0.010980f,    0,  -1,   0,   2 },
                               {  0.010674f,    0,  -1,   4,   0 },
                               {  0.010034f,    0,   3,   0,   0 },
                               {  0.008548f,    0,  -2,   4,   0 },
                               { -0.007910f,    1,  -1,   2,   0 },
                               { -0.006783f,    1,   0,   2,   0 },
                               {  0.005162f,    0,   1,  -1,   0 },
                               {  0.005000f,    1,   0,   1,   0 },
                               {  0.004049f,   -1,   1,   2,   0 },
                               {  0.003996f,    0,   2,   2,   0 },
                               {  0.003862f,    0,   0,   4,   0 },
                               {  0.003665f,    0,  -3,   2,   0 },
                               {  0.002695f,   -1,   2,   0,   0 },
                               {  0.002602f,    0,   1,  -2,  -2 },
                               {  0.002396f,   -1,  -2,   2,   0 },
                               { -0.002349f,    0,   1,   1,   0 },
                               {  0.002249f,   -2,   0,   2,   0 },
                               { -0.002125f,    1,   2,   0,   0 },
                               { -0.002079f,    2,   0,   0,   0 },
                               {  0.002059f,   -2,  -1,   2,   0 },
                               { -0.001773f,    0,   1,   2,  -2 },
                               { -0.001595f,    0,   0,   2,   2 },
                               {  0.001220f,   -1,  -1,   4,   0 },
                               { -0.001110f,    0,   2,   0,   2 } };
   static int NL = ( sizeof tl / sizeof ( struct term ) );

/*
** Latitude                       coeff       M    M'   D    F
*/
   static struct term tb[] = { {  5.128189f,    0,   0,   0,   1 },
                               {  0.280606f,    0,   1,   0,   1 },
                               {  0.277693f,    0,   1,   0,  -1 },
                               {  0.173238f,    0,   0,   2,  -1 },
                               {  0.055413f,    0,  -1,   2,   1 },
                               {  0.046272f,    0,  -1,   2,  -1 },
                               {  0.032573f,    0,   0,   2,   1 },
                               {  0.017198f,    0,   2,   0,   1 },
                               {  0.009267f,    0,   1,   2,  -1 },
                               {  0.008823f,    0,   2,   0,  -1 },
                               {  0.008247f,   -1,   0,   2,  -1 },
                               {  0.004323f,    0,  -2,   2,  -1 },
                               {  0.004200f,    0,   1,   2,   1 },
                               {  0.003372f,   -1,   0,  -2,   1 },
                               {  0.002472f,   -1,  -1,   2,   1 },
                               {  0.002222f,   -1,   0,   2,   1 },
                               {  0.002072f,   -1,  -1,   2,  -1 },
                               {  0.001877f,   -1,   1,   0,   1 },
                               {  0.001828f,    0,  -1,   4,  -1 },
                               { -0.001803f,    1,   0,   0,   1 },
                               { -0.001750f,    0,   0,   0,   3 },
                               {  0.001570f,   -1,   1,   0,  -1 },
                               { -0.001487f,    0,   0,   1,   1 },
                               { -0.001481f,    1,   1,   0,   1 },
                               {  0.001417f,   -1,  -1,   0,   1 },
                               {  0.001350f,   -1,   0,   0,   1 },
                               {  0.001330f,    0,   0,  -1,   1 },
                               {  0.001106f,    0,   3,   0,   1 },
                               {  0.001020f,    0,   0,   4,  -1 } };
   static int NB = ( sizeof tb / sizeof ( struct term ) );

/*
** Parallax                       coeff       M    M'   D    F
*/
   static struct term tp[] = { {  0.950724f,    0,   0,   0,   0 },
                               {  0.051818f,    0,   1,   0,   0 },
                               {  0.009531f,    0,  -1,   2,   0 },
                               {  0.007843f,    0,   0,   2,   0 },
                               {  0.002824f,    0,   2,   0,   0 } };
   static int NP = ( sizeof tp / sizeof ( struct term ) );



/* Whole years & fraction of year, and years since J1900.0 */
   yi = (float) ( iy - 1900 );
   iy4 = iy >= 4 ? iy % 4 : 3 - ( -iy - 1 ) % 4 ;
   yf = ( (float) ( 4 * ( id - 1 / ( iy4 + 1 ) )
                - iy4 - 2 ) + ( 4.0f * fd ) ) / 1461.0f;
   t  = yi + yf;

/* Moon's mean longitude */
   elp = D2R * (float) dmod ( (double) ( elp0 + elp1i * yf + elp1f * t ),
                                                                  360.0 );

/* Sun's mean anomaly */
   em = D2R * (float) dmod ( (double) ( em0 + em1f * t ), 360.0 );

/* Moon's mean anomaly */
   emp = D2R * (float) dmod ( (double) ( emp0 + emp1i * yf + emp1f * t ),
                                                                  360.0 );

/* Moon's mean elongation */
   d = D2R * (float) dmod ( (double) ( d0 + d1i * yf + d1f * t ), 360.0 );

/* Mean distance of the Moon from its ascending node */
   f = D2R * (float) dmod ( (double) ( f0 + f1i * yf + f1f * t ), 360.0 );

/* Longitude */
   v = 0.0f;
   dv = 0.0f;
   for ( n = NL -1; n >= 0; n-- ) {
      coeff = tl[n].coef;
      emn = (float) tl[n].nem;
      empn = (float) tl[n].nemp;
      dn = (float) tl[n].nd;
      fn = (float) tl[n].nf;
      theta = emn * em + empn * emp + dn * d + fn * f;
      v += coeff * ( (float) sin ( (double) theta ) );
      dv += coeff * ( (float) cos ( (double) theta ) ) *
                    ( emn * em1 + empn * emp1 + dn * d1 + fn * f1 );
   }
   el = elp + D2R * v;
   del = RATCON * ( elp1 / D2R +  dv );

/* Latitude */
   v = 0.0f;
   dv = 0.0f;
   for ( n = NB - 1; n >= 0; n-- ) {
      coeff = tb[n].coef;
      emn = (float) tb[n].nem;
      empn = (float) tb[n].nemp;
      dn = (float) tb[n].nd;
      fn = (float) tb[n].nf;
      theta = emn * em + empn * emp + dn * d + fn * f;
      v += coeff * ( (float) sin ( (double) theta ) );
      dv += coeff * ( (float) cos ( (double) theta ) ) *
                    ( emn * em1 + empn * emp1 + dn * d1 + fn * f1 );
   }
   b = D2R * v;
   db = RATCON * dv;

/* Parallax */
   v = 0.0f;
   dv = 0.0f;
   for ( n = NP - 1; n >= 0; n-- ) {
      coeff = tp[n].coef;
      emn = (float) tp[n].nem;
      empn = (float) tp[n].nemp;
      dn = (float) tp[n].nd;
      fn = (float) tp[n].nf;
      theta = emn * em + empn * emp + dn * d + fn * f;
      v += coeff * ( (float) cos ( (double) theta ) );
      dv += coeff * ( - (float) sin ( (double) theta ) ) *
                    ( emn * em1 + empn * emp1 + dn * d1 + fn * f1 );
   }
   p = D2R * v;
   dp = RATCON * dv;

/* Parallax to distance (AU, AU/sec) */
   sp = (float) sin ( (double) p );
   r = ERADAU / sp;
   dr = - r * dp * cos ( (double) p ) / sp;

/* Longitude, latitude to x, y, z (AU) */
   sel = (float) sin ( (double) el );
   cel = (float) cos ( (double) el );
   sb = (float) sin ( (double) b );
   cb = (float) cos ( (double) b );
   rcb = r * cb;
   rbd = r * db;
   w = rbd * sb - cb * dr;
   x = rcb * cel;
   y = rcb * sel;
   z = r * sb;
   xd = - y * del - w * cel;
   yd = x * del - w * sel;
   zd = rbd * cb + sb * dr;

/* Mean obliquity */
   eps = D2R * ( 23.45229f - 0.00013f * t );
   sineps = (float) sin ( (double) eps );
   coseps = (float) cos ( (double) eps );

/* To the equatorial system, mean of date */
   pv[0] = x;
   pv[1] = y * coseps - z * sineps;
   pv[2] = y * sineps + z * coseps;
   pv[3] = xd;
   pv[4] = yd * coseps - zd * sineps;
   pv[5] = yd * sineps + zd * coseps;
}
