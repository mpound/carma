#include "slalib.h"
#include "slamac.h"
void slaAop ( double rap, double dap, double date, double dut,
              double elongm, double phim, double hm,
              double xp, double yp, double tdk, double pmb,
              double rh, double wl, double tlr,
              double *aob, double *zob,
              double *hob, double *dob,
              double *rob )
/*
**  - - - - - - -
**   s l a A o p
**  - - - - - - -
**
**  Apparent to observed place, for optical sources distant from
**  the solar system.
**
**  Given:
**     rap     double  geocentric apparent right ascension
**     dap     double  geocentric apparent declination
**     date    double  UTC date/time (Modified Julian Date, JD-2400000.5)
**     dut     double  delta UT:  UT1-UTC (UTC seconds)
**     elongm  double  mean longitude of the observer (radians, east +ve)
**     phim    double  mean geodetic latitude of the observer (radians)
**     hm      double  observer's height above sea level (metres)
**     xp      double  polar motion x-coordinate (radians)
**     yp      double  polar motion y-coordinate (radians)
**     tdk     double  local ambient temperature (DegK; std=273.155)
**     pmb     double  local atmospheric pressure (mB; std=1013.25)
**     rh      double  local relative humidity (in the range 0.0-1.0)
**     wl      double  effective wavelength (micron, e.g. 0.55)
**     tlr     double  tropospheric lapse rate (DegK/metre, e.g. 0.0065)
**
**  Returned:
**     aob     double  observed azimuth (radians: N=0,E=90)
**     zob     double  observed zenith distance (radians)
**     hob     double  observed Hour Angle (radians)
**     dob     double  observed Declination (radians)
**     rob     double  observed Right Ascension (radians)
**
**  Notes:
**
**   1)  This routine returns zenith distance rather than elevation
**       in order to reflect the fact that no allowance is made for
**       depression of the horizon.
**
**   2)  The accuracy of the result is limited by the corrections for
**       refraction.  Providing the meteorological parameters are
**       known accurately and there are no gross local effects, the
**       predicted apparent RA,Dec should be within about 0.1 arcsec
**       for a zenith distance of less than 70 degrees.  Even at a
**       topocentric zenith distance of 90 degrees, the accuracy in
**       elevation should be better than 1 arcmin;  useful results
**       are available for a further 3 degrees, beyond which the
**       slaRefro routine returns a fixed value of the refraction.
**       The complementary routines slaAop (or slaAopqk) and slaOap
**       (or slaOapqk) are self-consistent to better than 1 micro-
**       arcsecond all over the celestial sphere.
**
**   3)  It is advisable to take great care with units, as even
**       unlikely values of the input parameters are accepted and
**       processed in accordance with the models used.
**
**   4)  "Apparent" place means the geocentric apparent right ascension
**       and declination, which is obtained from a catalogue mean place
**       by allowing for space motion, parallax, precession, nutation,
**       annual aberration, and the Sun's gravitational lens effect.  For
**       star positions in the FK5 system (i.e. J2000), these effects can
**       be applied by means of the slaMap etc routines.  Starting from
**       other mean place systems, additional transformations will be
**       needed;  for example, FK4 (i.e. B1950) mean places would first
**       have to be converted to FK5, which can be done with the
**       slaFk425 etc routines.
**
**   5)  "Observed" Az,El means the position that would be seen by a
**       perfect theodolite located at the observer.  This is obtained
**       from the geocentric apparent RA,Dec by allowing for Earth
**       orientation and diurnal aberration, rotating from equator
**       to horizon coordinates, and then adjusting for refraction.
**       The HA,Dec is obtained by rotating back into equatorial
**       coordinates, using the geodetic latitude corrected for polar
**       motion, and is the position that would be seen by a perfect
**       equatorial located at the observer and with its polar axis
**       aligned to the Earth's axis of rotation (n.b. not to the
**       refracted pole).  Finally, the RA is obtained by subtracting
**       the HA from the local apparent ST.
**
**   6)  To predict the required setting of a real telescope, the
**       observed place produced by this routine would have to be
**       adjusted for the tilt of the azimuth or polar axis of the
**       mounting (with appropriate corrections for mount flexures),
**       for non-perpendicularity between the mounting axes, for the
**       position of the rotator axis and the pointing axis relative
**       to it, for tube flexure, for gear and encoder errors, and
**       finally for encoder zero points.  Some telescopes would, of
**       course, exhibit other properties which would need to be
**       accounted for at the appropriate point in the sequence.
**
**   7)  This routine takes time to execute, due mainly to the
**       rigorous integration used to evaluate the refraction.
**       For processing multiple stars for one location and time,
**       call slaAoppa once followed by one call per star to slaAopqk.
**       Where a range of times within a limited period of a few hours
**       is involved, and the highest precision is not required, call
**       slaAoppa once, followed by a call to slaAoppat each time the
**       time changes, followed by one call per star to slaAopqk.
**
**   8)  The date argument is UTC expressed as an MJD.  This is,
**       strictly speaking, wrong, because of leap seconds.  However,
**       as long as the delta UT and the UTC are consistent there
**       are no difficulties, except during a leap second.  In this
**       case, the start of the 61st second of the final minute should
**       begin a new MJD day and the old pre-leap delta UT should
**       continue to be used.  As the 61st second completes, the MJD
**       should revert to the start of the day as, simultaneously,
**       the delta UTC changes by one second to its post-leap new value.
**
**   9)  The delta UT (UT1-UTC) is tabulated in IERS circulars and
**       elsewhere.  It increases by exactly one second at the end of
**       each UTC leap second, introduced in order to keep delta UT
**       within +/- 0.9 seconds.
**
**  10)  IMPORTANT -- TAKE CARE WITH THE LONGITUDE SIGN CONVENTION.
**       The longitude required by the present routine is east-positive,
**       in accordance with geographical convention (and right-handed).
**       In particular, note that the longitudes returned by the
**       slaObs routine are west-positive, following astronomical
**       usage, and must be reversed in sign before use in the present
**       routine.
**
**  11)  The polar coordinates xp,yp can be obtained from IERS
**       circulars and equivalent publications.  The maximum amplitude
**       is about 0.3 arcseconds.  If xp,yp values are unavailable,
**       use xp=yp=0.0.  See page B60 of the 1988 Astronomical Almanac
**       for a definition of the two angles.
**
**  12)  The height above sea level of the observing station, hm,
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
**  13)  The azimuths etc produced by the present routine are with
**       respect to the celestial pole.  Corrections to the terrestrial
**       pole can be computed using slaPolmo.
**
**  Called:  slaAoppa, slaAopqk
**
**  Last revision:   22 February 1995
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double aoprms[14];

   slaAoppa ( date, dut, elongm, phim, hm, xp,
              yp, tdk, pmb, rh, wl, tlr, aoprms );
   slaAopqk ( rap, dap, aoprms, aob, zob, hob, dob, rob );
}
