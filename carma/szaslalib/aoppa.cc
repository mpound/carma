#include "slalib.h"
#include "slamac.h"
void slaAoppa ( double date, double dut, double elongm, double phim,
                double hm, double xp, double yp, double tdk,
                double pmb, double rh, double wl, double tlr,
                double aoprms[14] )
/*
**  - - - - - - - - -
**   s l a A o p p a
**  - - - - - - - - -
**
**  Precompute apparent to observed place parameters required by
**  slaAopqk and slaOapqk.
**
**  Given:
**     date   d      UTC date/time (Modified Julian Date, JD-2400000.5)
**     dut    d      delta UT:  UT1-UTC (UTC seconds)
**     elongm d      mean longitude of the observer (radians, east +ve)
**     phim   d      mean geodetic latitude of the observer (radians)
**     hm     d      observer's height above sea level (metres)
**     xp     d      polar motion x-coordinate (radians)
**     yp     d      polar motion y-coordinate (radians)
**     tdk    d      local ambient temperature (DegK; std=273.155)
**     pmb    d      local atmospheric pressure (mB; std=1013.25)
**     rh     d      local relative humidity (in the range 0.0-1.0)
**     wl     d      effective wavelength (micron, e.g. 0.55)
**     tlr    d      tropospheric lapse rate (DegK/metre, e.g. 0.0065)
**
**  Returned:
**     aoprms d[14]  star-independent apparent-to-observed parameters:
**
**       (0)      geodetic latitude (radians)
**       (1,2)    sine and cosine of geodetic latitude
**       (3)      magnitude of diurnal aberration vector
**       (4)      height (hm)
**       (5)      ambient temperature (tdk)
**       (6)      pressure (pmb)
**       (7)      relative humidity (rh)
**       (8)      wavelength (wl)
**       (9)      lapse rate (tlr)
**       (10,11)  refraction constants A and B (radians)
**       (12)     longitude + eqn of equinoxes + sidereal DUT (radians)
**       (13)     local apparent sidereal time (radians)
**
**  Notes:
**
**   1)  It is advisable to take great care with units, as even
**       unlikely values of the input parameters are accepted and
**       processed in accordance with the models used.
**
**   2)  The date argument is UTC expressed as an MJD.  This is,
**       strictly speaking, improper, because of leap seconds.  However,
**       as long as the delta UT and the UTC are consistent there
**       are no difficulties, except during a leap second.  In this
**       case, the start of the 61st second of the final minute should
**       begin a new MJD day and the old pre-leap delta UT should
**       continue to be used.  As the 61st second completes, the MJD
**       should revert to the start of the day as, simultaneously,
**       the delta UTC changes by one second to its post-leap new value.
**
**   3)  The delta UT (UT1-UTC) is tabulated in IERS circulars and
**       elsewhere.  It increases by exactly one second at the end of
**       each UTC leap second, introduced in order to keep delta UT
**       within +/- 0.9 seconds.
**
**   4)  IMPORTANT -- TAKE CARE WITH THE LONGITUDE SIGN CONVENTION.
**       The longitude required by the present routine is east-positive,
**       in accordance with geographical convention (and right-handed).
**       In particular, note that the longitudes returned by the
**       slaObs routine are west-positive, following astronomical
**       usage, and must be reversed in sign before use in the present
**       routine.
**
**   5)  The polar coordinates xp,yp can be obtained from IERS
**       circulars and equivalent publications.  The maximum amplitude
**       is about 0.3 arcseconds.  If xp,yp values are unavailable,
**       use xp=yp=0.0.  See page B60 of the 1988 Astronomical Almanac
**       for a definition of the two angles.
**
**   6)  The height above sea level of the observing station, HM,
**       can be obtained from the Astronomical Almanac (Section J
**       in the 1988 edition), or via the routine slaObs.  If p,
**       the pressure in millibars, is available, an adequate
**       estimate of hm can be obtained from the expression
**
**             hm = -8149.9415 * log(p/1013.25);
**
**       (See Astrophysical Quantities, C.W.Allen, 3rd edition,
**       section 52.)  Similarly, if the pressure p is not known,
**       it can be estimated from the height of the observing
**       station, hm as follows:
**
**             p = 1013.25 * exp(-hm/8149.9415);
**
**       Note, however, that the refraction is proportional to the
**       pressure and that an accurate p value is important for
**       precise work.
**
**   7)  Repeated, computationally-expensive, calls to slaAoppa for
**       times that are very close together can be avoided by calling
**       slaAoppa just once and then using slaAoppat for the subsequent
**       times.  Fresh calls to slaAoppa will be needed only when changes
**       in the precession have grown to unacceptable levels or when
**       anything affecting the refraction has changed.
**
**  Defined in slamac.h:  D2PI, DS2R
**
**  Called:  slaGeoc, slaRefco, slaEqeqx, slaAoppat
**
**  Last revision:   25 July 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

#define C      173.14463331    /* Speed of light (AU per day) */
#define SOLSID 1.00273790935   /* Ratio between solar and sidereal time */

{
   double cphim, xt, yt, zt, xc, yc, zc, elong, phi, uau, vau;

/* Observer's location corrected for polar motion */
   cphim = cos( phim );
   xt = cos ( elongm ) * cphim;
   yt = sin ( elongm ) * cphim;
   zt = sin ( phim );
   xc = xt - xp * zt;
   yc = yt + yp * zt;
   zc = xp * xt - yp * yt + zt;

   elong = ( ( xc == 0.0 ) && ( yc == 0.0 ) ) ? 0.0 : atan2 ( yc, xc );

   phi = atan2 ( zc, sqrt ( xc * xc + yc * yc ) );
   aoprms[0] = phi;
   aoprms[1] = sin ( phi );
   aoprms[2] = cos ( phi );

/* Magnitude of the diurnal aberration vector */
   slaGeoc ( phi, hm, &uau, &vau );
   aoprms[3] = D2PI * uau * SOLSID / C;

/* Copy the refraction parameters and compute the A & B constants */
   aoprms[4] = hm;
   aoprms[5] = tdk;
   aoprms[6] = pmb;
   aoprms[7] = rh;
   aoprms[8] = wl;
   aoprms[9] = tlr;
   slaRefco ( hm, tdk, pmb, rh, wl, phi, tlr, 1e-10,
              &aoprms[10], &aoprms[11] );

/* Longitude + equation of the equinoxes + sidereal equivalent of DUT */
   aoprms[12] = elong + slaEqeqx ( date ) + dut * SOLSID * DS2R;

/* Sidereal time */
   slaAoppat ( date, aoprms );
}
