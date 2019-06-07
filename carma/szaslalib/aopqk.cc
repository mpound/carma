#include "slalib.h"
#include "slamac.h"
void slaAopqk ( double rap, double dap, double aoprms[14],
                double *aob, double *zob, double *hob,
                double *dob, double *rob )
/*
**  - - - - - - - - -
**   s l a A o p q k
**  - - - - - - - - -
**
**  Quick apparent to observed place (but see note 8, below, for
**  remarks about speed).
**
**  Given:
**     rap    double      geocentric apparent right ascension
**     dap    double      geocentric apparent declination
**     aoprms double[14]  star-independent apparent-to-observed parameters:
**
**       (0)      geodetic latitude (radians)
**       (1,2)    sine and cosine of geodetic latitude
**       (3)      magnitude of diurnal aberration vector
**       (4)      height (hm)
**       (5)      ambient temperature (t)
**       (6)      pressure (p)
**       (7)      relative humidity (rh)
**       (8)      wavelength (wl)
**       (9)      lapse rate (tlr)
**       (10,11)  refraction constants A and B (radians)
**       (12)     longitude + eqn of equinoxes + sidereal DUT (radians)
**       (13)     local apparent sidereal time (radians)
**
**  Returned:
**     *aob    double      observed azimuth (radians: N=0,E=90)
**     *zob    double      observed zenith distance (radians)
**     *hob    double      observed hour angle (radians)
**     *dob    double      observed declination (radians)
**     *rob    double      observed right ascension (radians)
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
**       observed RA,Dec predicted by this routine should be within
**       about 0.1 arcsec for a zenith distance of less than 70 degrees.
**       Even at a topocentric zenith distance of 90 degrees, the
**       accuracy in elevation should be better than 1 arcmin;  useful
**       results are available for a further 3 degrees, beyond which
**       the slaRefro routine returns a fixed value of the refraction.
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
**   7)  The star-independent apparent-to-observed-place parameters
**       in aoprms may be computed by means of the slaAoppa routine.
**       If nothing has changed significantly except the time, the
**       slaAoppat routine may be used to perform the requisite
**       partial recomputation of aoprms.
**
**   8)  At zenith distances beyond about 76 degrees, the need for
**       special care with the corrections for refraction causes a
**       marked increase in execution time.  Moreover, the effect
**       gets worse with increasing zenith distance.  Adroit
**       programming in the calling application may allow the
**       problem to be reduced.  Prepare an alternative aoprms array,
**       computed for zero air-pressure;  this will disable the
**       refraction corrections and cause rapid execution.  Using
**       this aoprms array, a preliminary call to the present routine
**       will, depending on the application, produce a rough position
**       which may be enough to establish whether the full, slow
**       calculation (using the real aoprms array) is worthwhile.
**       For example, there would be no need for the full calculation
**       if the preliminary call had already established that the
**       source was well below the elevation limits for a particular
**       telescope.
**
**   9)  The azimuths etc produced by the present routine are with
**       respect to the celestial pole.  Corrections to the terrestrial
**       pole can be computed using slaPolmo.
**
**  Called:  slaDcs2c, slaRefz, slaRefro, slaDcc2s, slaDranrm
**
**  Last revision:   22 February 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
/*
** Breakpoint for fast/slow refraction algorithm:
** ZD greater than arctan(4), (see slaRefco routine)
** or vector z less than cosine(arctan(z)) = 1/sqrt(17)
*/
   static double zbreak = 0.242535625;

   int i;

   double sphi, cphi, st, v[3], xhd, yhd, zhd, diurab, f,
          xhdt, yhdt, zhdt, xaet, yaet, zaet, azobs,
          zdt, refa, refb, zdobs, dzd, dref, ce,
          xaeo, yaeo, zaeo, hmobs, dcobs, raobs;

/* Sin, cos of latitude */
   sphi = aoprms[1];
   cphi = aoprms[2];

/* Local apparent sidereal time */
   st = aoprms[13];

/* Apparent RA,Dec to Cartesian -HA,Dec */
   slaDcs2c(rap - st, dap, v);
   xhd = v[0];
   yhd = v[1];
   zhd = v[2];

/* Diurnal aberration */
   diurab = aoprms[3];
   f = 1.0 - diurab * yhd;
   xhdt = f * xhd;
   yhdt = f * ( yhd + diurab );
   zhdt = f * zhd;

/* Cartesian -HA,Dec to Cartesian az,el (S=0,E=90) */
   xaet = sphi * xhdt - cphi * zhdt;
   yaet = yhdt;
   zaet = cphi * xhdt + sphi * zhdt;

/* Azimuth (N=0,E=90) */
   azobs = ( (xaet == 0.0) && (yaet == 0.0) ) ?
                            0.0 : atan2 ( yaet, -xaet );

/* Topocentric zenith distance */
   zdt = atan2 ( sqrt ( xaet * xaet + yaet * yaet ), zaet );

/*
** Refraction
** ----------
*/

/* Fast algorithm using two constant model */
   refa = aoprms[10];
   refb = aoprms[11];
   slaRefz ( zdt, refa, refb, &zdobs );

/* Large zenith distance? */
   if ( cos ( zdobs ) < zbreak ) {

   /* Yes: use rigorous algorithm */

   /* Initialize loop (maximum of 10 iterations) */
      i = 1;
      do {

      /* Compute refraction using current estimate of observed ZD */
         slaRefro ( zdobs, aoprms[4], aoprms[5], aoprms[6],
                    aoprms[7], aoprms[8], aoprms[0],
                    aoprms[9], 1e-8, &dref );

      /* Remaining discrepancy */
         dzd = zdobs + dref - zdt;

      /* Update the estimate */
         zdobs -= dzd;

      /* Increment the iteration counter */
         i++;

      } while ( fabs ( dzd ) > 1e-10 && i <= 10 );
   }

/* To Cartesian az/ZD */
   ce   = sin ( zdobs );
   xaeo = -cos ( azobs ) * ce;
   yaeo = sin ( azobs ) * ce;
   zaeo = cos ( zdobs );

/* Cartesian az/ZD to Cartesian -HA,Dec */
   v[0] = sphi * xaeo + cphi * zaeo;
   v[1] = yaeo;
   v[2] = -cphi * xaeo + sphi * zaeo;

/* To spherical -HA,dec */
   slaDcc2s ( v, &hmobs, &dcobs );

/* Right ascension */
   raobs = slaDranrm ( st + hmobs );

/* Return the results */
   *aob = azobs;
   *zob = zdobs;
   *hob = -hmobs;
   *dob = dcobs;
   *rob = raobs;
}
