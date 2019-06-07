#include "slalib.h"
#include "slamac.h"
void slaOap ( char *type, double ob1, double ob2, double date,
              double dut, double elongm, double phim, double hm,
              double xp, double yp, double tdk, double pmb,
              double rh, double wl, double tlr,
              double *rap, double *dap )
/*
**  - - - - - - -
**   s l a O a p
**  - - - - - - -
**
**  Observed to apparent place
**
**  Given:
**     type   c*(*)  type of coordinates - 'R', 'H' or 'A' (see below)
**     ob1    d      observed Az, HA or RA (radians; Az is N=0,E=90)
**     ob2    d      observed ZD or Dec (radians)
**     date   d      UTC date/time (modified Julian Date, JD-2400000.5)
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
**     rap    d      geocentric apparent right ascension
**     dap    d      geocentric apparent declination
**
**  Notes:
**
**  1)  Only the first character of the type argument is significant.
**      'R' or 'r' indicates that obs1 and obs2 are the observed Right
**      Ascension and Declination;  'H' or 'h' indicates that they are
**      Hour Angle (West +ve) and Declination;  anything else ('A' or
**      'a' is recommended) indicates that obs1 and obs2 are Azimuth
**      (North zero, East is 90 deg) and zenith distance.  (Zenith
**      distance is used rather than elevation in order to reflect the
**      fact that no allowance is made for depression of the horizon.)
**
**  2)  The accuracy of the result is limited by the corrections for
**      refraction.  Providing the meteorological parameters are
**      known accurately and there are no gross local effects, the
**      predicted apparent RA,Dec should be within about 0.1 arcsec
**      for a zenith distance of less than 70 degrees.  Even at a
**      topocentric zenith distance of 90 degrees, the accuracy in
**      elevation should be better than 1 arcmin;  useful results
**      are available for a further 3 degrees, beyond which the
**      slaRefro routine returns a fixed value of the refraction.
**      The complementary routines slaAop (or slaAopqk) and slaOap
**      (or slaOapqk) are self-consistent to better than 1 micro-
**      arcsecond all over the celestial sphere.
**
**  3)  It is advisable to take great care with units, as even
**      unlikely values of the input parameters are accepted and
**      processed in accordance with the models used.
**
**  4)  "Observed" Az,El means the position that would be seen by a
**      perfect theodolite located at the observer.  This is
**      related to the observed HA,Dec via the standard rotation, using
**      the geodetic latitude (corrected for polar motion), while the
**      observed HA and RA are related simply through the local
**      apparent ST.  "Observed" RA,Dec or HA,Dec thus means the
**      position that would be seen by a perfect equatorial located
**      at the observer and with its polar axis aligned to the
**      Earth's axis of rotation (n.b. not to the refracted pole).
**      By removing from the observed place the effects of
**      atmospheric refraction and diurnal aberration, the
**      geocentric apparent RA,Dec is obtained.
**
**  5)  Frequently, mean rather than apparent RA,Dec will be required,
**      in which case further transformations will be necessary.  The
**      slaAMP etc routines will convert the apparent RA,Dec produced
**      by the present routine into an "FK5" (J2000) mean place, by
**      allowing for the Sun's gravitational lens effect, annual
**      aberration, nutation and precession.  Should "FK4" (1950)
**      coordinates be needed, the routines slaFk425 etc will also
**      need to be applied.
**
**  6)  To convert to apparent RA,Dec the coordinates read from a
**      real telescope, corrections would have to be applied for
**      encoder zero points, gear and encoder errors, tube flexure,
**      the position of the rotator axis and the pointing axis
**      relative to it, non-perpendicularity between the mounting
**      axes, and finally for the tilt of the azimuth or polar axis
**      of the mounting (with appropriate corrections for mount
**      flexures).  Some telescopes would, of course, exhibit other
**      properties which would need to be accounted for at the
**      appropriate point in the sequence.
**
**  7)  The star-independent apparent-to-observed-place parameters
**      in aoprms may be computed by means of the slaAoppa routine.
**      If nothing has changed significantly except the time, the
**      slaAoppat routine may be used to perform the requisite
**      partial recomputation of aoprms.
**
**  8)  The date argument is UTC expressed as an MJD.  This is,
**      strictly speaking, wrong, because of leap seconds.  However,
**      as long as the delta UT and the UTC are consistent there
**      are no difficulties, except during a leap second.  In this
**      case, the start of the 61st second of the final minute should
**      begin a new MJD day and the old pre-leap delta UT should
**      continue to be used.  As the 61st second completes, the MJD
**      should revert to the start of the day as, simultaneously,
**      the delta UTC changes by one second to its post-leap new value.
**
**  9)  The delta UT (UT1-UTC) is tabulated in IERS circulars and
**      elsewhere.  It increases by exactly one second at the end of
**      each UTC leap second, introduced in order to keep delta UT
**      within +/- 0.9 seconds.
**
**  10) IMPORTANT -- TAKE CARE WITH THE LONGITUDE SIGN CONVENTION.
**      The longitude required by the present routine is east-positive,
**      in accordance with geographical convention (and right-handed).
**      In particular, note that the longitudes returned by the
**      slaObs routine are west-positive, following astronomical
**      usage, and must be reversed in sign before use in the present
**      routine.
**
**  11) The polar coordinates xp,yp can be obtained from IERS
**      circulars and equivalent publications.  The maximum amplitude
**      is about 0.3 arcseconds.  If xp,yp values are unavailable,
**      use xp=yp=0.0.  See page B60 of the 1988 Astronomical Almanac
**      for a definition of the two angles.
**
**  12) The height above sea level of the observing station, hm,
**      can be obtained from the Astronomical Almanac (Section J
**      in the 1988 edition), or via the routine slaObs.  If p,
**      the pressure in millibars, is available, an adequate
**      estimate of hm can be obtained from the expression
**
**            hm = -8149.9415 * log (p/1013.25);
**
**      (See Astrophysical Quantities, C.W.Allen, 3rd edition,
**      section 52.)  Similarly, if the pressure p is not known,
**      it can be estimated from the height of the observing
**      station, hm as follows:
**
**            p = 1013.25 * exp (-hm/8149.9415);
**
**      Note, however, that the refraction is proportional to the
**      pressure and that an accurate p value is important for
**      precise work.
**
**  13) The azimuths etc used by the present routine are with respect
**      to the celestial pole.  Corrections from the terrestrial pole
**      can be computed using slaPolmo.
**
**  Called:  slaAoppa, slaOapqk
**
**  Last revision:   22 February 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
  double aoprms[14];

  slaAoppa ( date, dut, elongm, phim, hm, xp, yp, tdk,
                               pmb, rh, wl, tlr, aoprms );
  slaOapqk ( type, ob1, ob2, aoprms, rap, dap );
}
