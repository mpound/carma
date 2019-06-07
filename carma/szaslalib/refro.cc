#include "slalib.h"
#include "slamac.h"

static void atmt ( double, double, double, double, double, double,
                   double, double, double, double, double, double,
                   double*, double*, double* );
static void atms ( double, double, double, double, double,
                   double*, double* );

void slaRefro ( double zobs, double hm, double tdk, double pmb,
                double rh, double wl, double phi, double tlr,
                double eps, double *ref )
/*
**  - - - - - - - - -
**   s l a R e f r o
**  - - - - - - - - -
**
**  Atmospheric refraction for radio and optical wavelengths.
**
**  Given:
**    zobs    double  observed zenith distance of the source (radian)
**    hm      double  height of the observer above sea level (metre)
**    tdk     double  ambient temperature at the observer (deg K)
**    pmb     double  pressure at the observer (millibar)
**    rh      double  relative humidity at the observer (range 0-1)
**    wl      double  effective wavelength of the source (micrometre)
**    phi     double  latitude of the observer (radian, astronomical)
**    tlr     double  temperature lapse rate in the troposphere (degK/met
**    eps     double  precision required to terminate iteration (radian)
**
**  Returned:
**    ref     double  refraction: in vacuo ZD minus observed ZD (radian)
**
**  Notes:
**
**  1  A suggested value for the tlr argument is 0.0065D0.  The
**     refraction is significantly affected by tlr, and if studies
**     of the local atmosphere have been carried out a better tlr
**     value may be available.
**
**  2  A suggested value for the eps argument is 1e-8.  The result is
**     usually at least two orders of magnitude more computationally
**     precise than the supplied eps value.
**
**  3  The routine computes the refraction for zenith distances up
**     to and a little beyond 90 deg using the method of Hohenkerk
**     and Sinclair (NAO Technical Notes 59 and 63, subsequently adopted
**     in the Explanatory Supplement, 1992 edition - see section 3.281).
**
**  4  The C code is an adaptation of the Fortran optical refraction
**     subroutine AREF of C.Hohenkerk (HMNAO, September 1984), with
**     extensions to support the radio case.  The following modifications
**     to the original HMNAO optical refraction algorithm have been made:
**
**     .  The angle arguments have been changed to radians.
**
**     .  Any value of zobs is allowed (see note 6, below).
**
**     .  Other argument values have been limited to safe values.
**
**     .  Murray's values for the gas constants have been used
**        (Vectorial Astrometry, Adam Hilger, 1983).
**
**     .  The numerical integration phase has been rearranged for
**        extra clarity.
**
**     .  A better model for Ps(T) has been adopted (taken from
**        Gill, Atmosphere-Ocean Dynamics, Academic Press, 1982).
**
**     .  More accurate expressions for Pwo have been adopted
**        (again from Gill 1982).
**
**     .  Provision for radio wavelengths has been added using
**        expressions devised by A.T.Sinclair, RGO (private
**        communication 1989), based on the Essen & Froome
**        refractivity formula adopted in Resolution 1 of the
**        13th International Geodesy Association General Assembly
**        (Bulletin Geodesique 1963 p390).
**
**     .  Various small changes have been made to gain speed.
**
**     None of the changes significantly affects the optical results
**     with respect to the algorithm given in the 1992 Explanatory
**     Supplement.  For example, at 70 deg zenith distance the present
**     routine agrees with the ES algorithm to better than 0.05 arcsec
**     for any reasonable combination of parameters.  However, the
**     improved water-vapour expressions do make a significant difference
**     in the radio band, at 70 deg zenith distance reaching almost
**     4 arcsec for a hot, humid, low-altitude site during a period of
**     low pressure.
**
**  5  The radio refraction is chosen by specifying wl > 100 micrometres.
**     Because the algorithm takes no account of the ionosphere, the
**     accuracy deteriorates at low frequencies, below about 30 MHz.
**
**  6  Before use, the value of zobs is expressed in the range +/- pi.
**     If this ranged zobs is -ve, the result ref is computed from its
**     absolute value before being made -ve to match.  In addition, if
**     it has an absolute value greater than 93 deg, a fixed ref value
**     equal to the result for zobs = 93 deg is returned, appropriately
**     signed.
**
**  7  As in the original Hohenkerk and Sinclair algorithm, fixed values
**     of the water vapour polytrope exponent, the height of the
**     tropopause, and the height at which refraction is negligible are
**     used.
**
**  8  The radio refraction has been tested against work done by
**     Iain Coulson, JACH, (private communication 1995) for the
**     James Clerk Maxwell Telescope, Mauna Kea.  For typical conditions,
**     agreement at the 0.1 arcsec level is achieved for moderate ZD,
**     worsening to perhaps 0.5-1.0 arcsec at ZD 80 deg.  At hot and
**     humid sea-level sites the accuracy will not be as good.
**
**  9  It should be noted that the relative humidity rh is formally
**     defined in terms of "mixing ratio" rather than pressures or
**     densities as is often stated.  It is the mass of water per unit
**     mass of dry air divided by that for saturated air at the same
**     temperature and pressure (see Gill 1982).
**
**  Called:  slaDrange, atmt, atms
**
**  Defined in slamac.h:  TRUE, FALSE
**
**  Last revision:   30 January 1997
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
/* Fixed parameters */

   static double d93 = 1.623156204; /* 93 degrees in radians        */
   static double gcr = 8314.32;     /* Universal gas constant       */
   static double dmd = 28.9644;     /* Molecular weight of dry air  */
   static double dmw = 18.0152;     /* Molecular weight of water
                                                             vapour */
   static double s = 6378120.0;     /* Mean Earth radius (metre)    */
   static double delta = 18.36;     /* Exponent of temperature
                                         dependence of water vapour
                                                           pressure */
   static double ht = 11000.0;      /* Height of tropopause (metre) */
   static double hs = 80000.0;      /* Upper limit for refractive
                                                    effects (metre) */

/* Variables used when calling the internal routine atmt */
   double robs;   /* height of observer from centre of Earth (metre) */
   double tdkok;  /* temperature at the observer (deg K) */
   double alpha;  /* alpha          |        */
   double gamm2;  /* gamma minus 2  | see ES */
   double delm2;  /* delta minus 2  |        */
   double c1,c2,c3,c4,c5,c6;  /* various */

/* Variables used when calling the internal routine atms */
   double rt;     /* height of tropopause from centre of Earth (metre) */
   double tt;     /* temperature at the tropopause (deg k) */
   double dnt;    /* refractive index at the tropopause */
   double gamal;  /* constant of the atmospheric model = g*md/r */

   int is, k, n, i, j, optic;
   double zobs1, zobs2, hmok, pmbok, rhok, wlok, tol, wlsq, gb,
          a, gamma, tdc, psat, pwo, w, tempo, dn0, rdndr0, sk0,
          f0, rdndrt, zt, ft, dnts, rdndrp, zts, fts, rs,
          dns, rdndrs, zs, fs, refold, z0, zrange, fb, ff, fo,
          fe, h, r, sz, rg, dr, tg, dn, rdndr, t, f, refp, reft;

/* The refraction integrand */
#define refi(R,DN,RDNDR) ((RDNDR)/(DN+RDNDR));



/* Transform zobs into the normal range */
   zobs1 = slaDrange ( zobs );
   zobs2 = fabs ( zobs1 );
   zobs2 = gmin ( zobs2, d93 );

/* Keep other arguments within safe bounds */
   hmok = gmax ( hm, -1000.0 );
   hmok = gmin ( hmok, 10000.0 );
   tdkok = gmax ( tdk, 100.0 );
   tdkok = gmin ( tdkok, 500.0 );
   pmbok = gmax ( pmb, 0.0 );
   pmbok = gmin ( pmbok, 10000.0 );
   rhok  = gmax ( rh, 0.0 );
   rhok  = gmin ( rhok, 1.0 );
   wlok  = gmax ( wl, 0.1 );
   alpha = fabs ( tlr );
   alpha = gmax ( alpha, 0.001 );
   alpha = gmin ( alpha, 0.01 );

/* Tolerance for iteration */
   w = fabs ( eps );
   tol = gmin ( w, 0.1 ) / 2.0;

/* Decide whether optical or radio case - switch at 100 micron */
   optic = ( wlok <= 100.0 );

/* Set up model atmosphere parameters defined at the observer */
   wlsq = wlok * wlok;
   gb = 9.784 * ( 1.0 - 0.0026 * cos ( 2.0 * phi ) - 2.8e-7 * hmok );
   a = ( optic ) ?
         ( ( 287.604 + 1.6288 / wlsq + 0.0136 / ( wlsq * wlsq ) )
                 * 273.15 / 1013.25 ) * 1e-6
       :
         77.624e-6;
   gamal = gb * dmd / gcr;
   gamma = gamal / alpha;
   gamm2 = gamma - 2.0;
   delm2 = delta - 2.0;
   tdc = tdkok - 273.15;
   psat = pow ( 10.0, ( 0.7859 + 0.03477 * tdc ) /
                         ( 1.0 + 0.00412 * tdc ) ) *
                ( 1.0 + pmbok * ( 4.5e-6 + 6e-10 * tdc * tdc ) );
   pwo = ( pmbok > 0.0 ) ?
         rhok * psat / ( 1.0 - ( 1.0 - rhok ) * psat / pmbok ) :
         0.0;
   w = pwo * ( 1.0 - dmw / dmd ) * gamma / ( delta - gamma );
   c1 = a * ( pmbok + w ) / tdkok;
   c2 = ( a * w + ( optic ? 11.2684e-6 : 12.92e-6 ) * pwo ) / tdkok;
   c3 = ( gamma - 1.0 ) * alpha * c1 / tdkok;
   c4 = ( delta - 1.0 ) * alpha * c2 / tdkok;
   c5 = optic ? 0.0 : 371897e-6 * pwo / tdkok;
   c6 = c5 * delm2 * alpha / ( tdkok * tdkok );

/* Conditions at the observer */
   robs = s + hmok;
   atmt ( robs, tdkok, alpha, gamm2, delm2, c1, c2, c3, c4, c5, c6, robs,
          &tempo, &dn0, &rdndr0 );
   sk0 = dn0 * robs * sin ( zobs2 );
   f0 = refi ( robs, dn0, rdndr0 );

/* Conditions at the tropopause in the troposphere */
   rt = s + ht;
   atmt ( robs, tdkok, alpha, gamm2, delm2, c1, c2, c3, c4, c5, c6, rt,
          &tt, &dnt, &rdndrt );
   zt = asin ( sk0 / ( rt * dnt ) );
   ft = refi ( rt, dnt, rdndrt );

/* Conditions at the tropopause in the stratosphere */
   atms ( rt, tt, dnt, gamal, rt, &dnts, &rdndrp );
   zts = asin ( sk0 / ( rt * dnts ) );
   fts = refi ( rt, dnts, rdndrp );

/* At the stratosphere limit */
   rs = s + hs;
   atms ( rt, tt, dnt, gamal, rs, &dns, &rdndrs );
   zs = asin ( sk0 / ( rs * dns ) );
   fs = refi ( rs, dns, rdndrs );

/*
** Integrate the refraction integral in two parts;  first in the
** troposphere (k=1), then in the stratosphere (k=2).
*/

/* Initialize previous refraction to ensure at least two iterations */
   refold = 1e6;

/*
** Start off with 8 strips for the troposphere integration, and then
** use the final troposphere value for the stratosphere integration,
** which tends to need more strips.
*/
   is = 8;

/* Troposphere then stratosphere */
   for ( k = 1; k <= 2; k++ ) {

   /* Start z, z range, and start and end values */
      if ( k == 1 ) {
         z0 = zobs2;
         zrange = zt - z0;
         fb = f0;
         ff = ft;
      } else {
         z0 = zts;
         zrange = zs - z0;
         fb = fts;
         ff = fs;
      }

   /* Sums of odd and even values */
      fo = 0.0;
      fe = 0.0;

   /* First time through loop we have to do every point */
      n = 1;

   /* Start of iteration loop (terminates at specified precision) */
      for ( ; ; ) {

      /* Strip width */
         h = zrange / (double) is;

      /* Initialize distance from Earth centre for quadrature pass */
         r = ( k == 1 ) ? robs : rt;

      /* One pass (no need to compute evens after first time) */
         for ( i = 1; i < is; i += n ) {

         /* Sine of observed zenith distance */
            sz = sin ( z0 + h * (double) i );

         /* Find r (to nearest metre, maximum four iterations) */
            if ( sz > 1e-20 ) {
               w = sk0 / sz;
               rg = r;
               j = 0;
               do {
                  if ( k == 1 ) {
                     atmt ( robs, tdkok, alpha, gamm2, delm2,
                            c1, c2, c3, c4, c5, c6, rg,
                            &tg, &dn, &rdndr );
                  } else {
                     atms ( rt, tt, dnt, gamal, rg, &dn, &rdndr );
                  }
                  dr = ( rg * dn - w ) / ( dn + rdndr );
                  rg -= dr;
               } while ( fabs ( dr ) > 1.0 && j++ <= 4 );
               r = rg;
            }

         /* Find refractive index and integrand at r */
            if ( k == 1 ) {
               atmt ( robs, tdkok, alpha, gamm2, delm2,
                      c1, c2, c3, c4, c5, c6, r,
                      &t, &dn, &rdndr );
            } else {
               atms ( rt, tt, dnt, gamal, r, &dn, &rdndr );
            }
            f = refi ( r, dn, rdndr );

         /* Accumulate odd and (first time only) even values */
            if ( n == 1 && i % 2 == 0 ) {
               fe += f;
            } else {
               fo += f;
            }
         }

      /* Evaluate the integrand using Simpson's Rule */
         refp = h * ( fb + 4.0 * fo + 2.0 * fe + ff ) / 3.0;

      /* Has the required precision been reached? */
         if ( fabs ( refp - refold ) > tol ) {

         /* No: prepare for next iteration */
            refold = refp;   /* Save current value for convergence test */
            is += is;        /* Double the number of strips */
            fe += fo;        /* Sum of all = sum of evens next time */
            fo = 0.0;        /* Reset odds accumulator */
            n = 2;           /* Skip even values next time */

         } else {

         /* Yes: save troposphere component and terminate loop */
            if ( k == 1 ) reft = refp;
            break;
         }
      }
   }

/* Result */
   *ref = reft + refp;
   if ( zobs1 < 0.0 ) *ref = - ( *ref );
}

/*--------------------------------------------------------------------------*/

static void atmt ( double robs, double tdkok, double alpha, double gamm2,
                   double delm2, double c1, double c2, double c3,
                   double c4, double c5, double c6, double r,
                   double *t, double *dn, double *rdndr )
/*
**  - - - - -
**   a t m t
**  - - - - -
**
**  Internal routine used by slaRefro:
**
**    refractive index and derivative with respect to height for the
**    troposphere.
**
**  Given:
**    robs    double   height of observer from centre of the Earth (metre)
**    tdkok   double   temperature at the observer (deg K)
**    alpha   double   alpha          )
**    gamm2   double   gamma minus 2  ) see ES
**    delm2   double   delta minus 2  )
**    c1      double   useful term  )
**    c2      double   useful term  )
**    c3      double   useful term  ) see source of
**    c4      double   useful term  ) slaRefro main routine
**    c5      double   useful term  )
**    c6      double   useful term  )
**    r       double   current distance from the centre of the Earth (metre)
**
**  Returned:
**    *t      double   temperature at r (deg K)
**    *dn     double   refractive index at r
**    *rdndr  double   r * rate the refractive index is changing at r
**
**  This routine is derived from the ATMOSTRO routine (C.Hohenkerk,
**  HMNAO), with enhancements specified by A.T.Sinclair (RGO) to
**  handle the radio case.
**
**  Note that in the optical case c5 and c6 are zero.
*/
{
   double w, tt0, tt0gm2, tt0dm2;

   w = tdkok - alpha * ( r - robs );
   w = gmin ( w, 320.0 );
   w = gmax ( w, 200.0 );
   tt0 = w / tdkok;
   tt0gm2 = pow ( tt0, gamm2 );
   tt0dm2 = pow ( tt0, delm2 );
   *t = w;
   *dn = 1.0 + ( c1 * tt0gm2 - ( c2 - c5 / w ) * tt0dm2 ) * tt0;
   *rdndr = r * ( - c3 * tt0gm2 + ( c4 - c6 / tt0 ) * tt0dm2 );
}

/*--------------------------------------------------------------------------*/

static void atms ( double rt, double tt, double dnt, double gamal, double r,
                   double *dn, double *rdndr )
/*
**  - - - - -
**   a t m s
**  - - - - -
**
**  Internal routine used by slaRefro:
**
**   refractive index and derivative with respect to height for the
**   stratosphere.
**
**  Given:
**    rt      double   height of tropopause from centre of the Earth (metre)
**    tt      double   temperature at the tropopause (deg k)
**    dnt     double   refractive index at the tropopause
**    gamal   double   constant of the atmospheric model = g*md/r
**    r       double   current distance from the centre of the Earth (metre)
**
**  Returned:
**    *dn     double   refractive index at r
**    *rdndr  double   r * rate the refractive index is changing at r
**
**  This routine is derived from the ATMOSSTR routine (C.Hohenkerk, HMNAO).
*/
{
   double b, w;

   b = gamal / tt;
   w = ( dnt - 1.0 ) * exp ( - b * ( r - rt ) );
   *dn = 1.0 + w;
   *rdndr = - r * b * w;
}
