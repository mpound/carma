#include "slalib.h"
#include "slamac.h"
void slaOapqk ( char *type, double ob1, double ob2,
                double aoprms[14], double *rap, double *dap )
/*
**  - - - - - - - - -
**   s l a O a p q k
**  - - - - - - - - -
**
**  Quick observed to apparent place.
**
**  Given:
**     type   char        type of coordinates - 'r', 'h' or 'a' (see below)
**     ob1    double      observed az, HA or RA (radians; az is n=0,e=90)
**     ob2    double      observed ZD or Dec (radians)
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
**       (10,11)  refraction constants a and b (radians)
**       (12)     longitude + eqn of equinoxes + sidereal DUT (radians)
**       (13)     local apparent sidereal time (radians)
**
**  Returned:
**     *rap    double      geocentric apparent right ascension
**     *dap    double      geocentric apparent declination
**
**  Notes:
**
**   1)  Only the first character of the type argument is significant.
**       'R' or 'r' indicates that obs1 and obs2 are the observed right
**       ascension and declination;  'H' or 'h' indicates that they are
**       hour angle (west +ve) and declination;  anything else ('A' or
**       'a' is recommended) indicates that obs1 and obs2 are azimuth
**       (north zero, east is 90 deg) and zenith distance.  (Zenith
**       distance is used rather than elevation in order to reflect the
**       fact that no allowance is made for depression of the horizon.)
**
**   2)  The accuracy of the result is limited by the corrections for
**       refraction.  Providing the meteorological parameters are
**       known accurately and there are no gross local effects, the
**       predicted apparent RA,Dec should be within about 0.1 arcsec.
**       Even at a topocentric zenith distance of 90 degrees, the
**       accuracy in elevation should be better than 1 arcmin;  useful
**       results are available for a further 3 degrees, beyond which
**       the slaRefro routine returns a fixed value of the refraction.
**       the complementary routines slaAop (or slaAopqk) and slaOap
**       (or slaOapqk) are self-consistent to better than 1 micro-
**       arcsecond all over the celestial sphere.
**
**   3)  It is advisable to take great care with units, as even
**       unlikely values of the input parameters are accepted and
**       processed in accordance with the models used.
**
**   5)  "Observed" az,el means the position that would be seen by a
**       perfect theodolite located at the observer.  This is
**       related to the observed HA,Dec via the standard rotation, using
**       the geodetic latitude (corrected for polar motion), while the
**       observed HA and RA are related simply through the local
**       apparent st.  "Observed" RA,Dec or HA,Dec thus means the
**       position that would be seen by a perfect equatorial located
**       at the observer and with its polar axis aligned to the
**       Earth's axis of rotation (n.b. not to the refracted pole).
**       by removing from the observed place the effects of
**       atmospheric refraction and diurnal aberration, the
**       geocentric apparent RA,Dec is obtained.
**
**   5)  Frequently, mean rather than apparent RA,Dec will be required,
**       in which case further transformations will be necessary.  The
**       slaAmp etc routines will convert the apparent RA,Dec produced
**       by the present routine into an "FK5" (J2000) mean place, by
**       allowing for the Sun's gravitational lens effect, annual
**       aberration, nutation and precession.  Should "FK4" (1950)
**       coordinates be needed, the routines slaFk524 etc will also
**       need to be applied.
**
**   6)  To convert to apparent RA,Dec the coordinates read from a
**       real telescope, corrections would have to be applied for
**       encoder zero points, gear and encoder errors, tube flexure,
**       the position of the rotator axis and the pointing axis
**       relative to it, non-perpendicularity between the mounting
**       axes, and finally for the tilt of the azimuth or polar axis
**       of the mounting (with appropriate corrections for mount
**       flexures).  Some telescopes would, of course, exhibit other
**       properties which would need to be accounted for at the
**       appropriate point in the sequence.
**
**   7)  The star-independent apparent-to-observed-place parameters
**       in aoprms may be computed by means of the slaAoppa routine.
**       If nothing has changed significantly except the time, the
**       slaAoppat routine may be used to perform the requisite
**       partial recomputation of aoprms.
**
**   8)  The azimuths etc used by the present routine are with respect
**       to the celestial pole.  Corrections from the terrestrial pole
**       can be computed using slaPolmo.
**
**  Called:  slaDcs2c, slaDcc2s, slaRefro, slaDranrm
**
**  Last revision:   22 February 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{

/* Breakpoint for fast/slow refraction algorithm:
   ZD greater than arctan(4), (see slaRefco routine)
   or vector z less than cosine(arctan(z)) = 1/sqrt(17) */
   static double zbreak = 0.242535625;

   char c;
   double c1, c2, sphi, cphi, st, ce, xaeo, yaeo, zaeo, v[3],
          xmhdo, ymhdo, zmhdo, az, sz, zdo, tz, dref, zdt,
          xaet, yaet, zaet, xmhda, ymhda, zmhda, diurab, f, hma;

/* Coordinate type */
   c = *type;

/* Coordinates */
   c1 = ob1;
   c2 = ob2;

/* Sin, cos of latitude */
   sphi = aoprms[1];
   cphi = aoprms[2];

/* Local apparent sidereal time */
   st = aoprms[13];

/* Standardize coordinate type */
   if ( c == 'r' || c == 'R' ) {
      c = 'R';
   } else if ( c == 'h' || c  == 'H' ) {
      c = 'H';
   } else {
      c = 'A';
   }

/* If az,ZD convert to Cartesian (s=0,e=90) */
   if ( c == 'A' ) {
      ce = sin ( c2 );
      xaeo = - cos ( c1 ) * ce;
      yaeo = sin ( c1 ) * ce;
      zaeo = cos ( c2 );
   } else {

   /* If RA,Dec convert to HA,Dec */
      if ( c == 'R' ) {
         c1 = st - c1;
      }

   /* To Cartesian -HA,Dec */
      slaDcs2c ( -c1, c2, v );
      xmhdo = v[0];
      ymhdo = v[1];
      zmhdo = v[2];

   /* To Cartesian az,el (s=0,e=90) */
      xaeo = sphi * xmhdo - cphi * zmhdo;
      yaeo = ymhdo;
      zaeo = cphi * xmhdo + sphi * zmhdo;
   }

/* Azimuth (s=0,e=90) */
   az = atan2 ( yaeo, xaeo );

/* Sine of observed ZD, and observed ZD */
   sz = sqrt ( xaeo * xaeo + yaeo * yaeo );
   zdo = atan2 ( sz, zaeo );

/*
   Refraction
   ----------

   Large zenith distance? */
   if ( zaeo >= zbreak ) {

   /* Fast algorithm using two constant model */
      tz = sz / zaeo;
      dref = ( aoprms[10] + aoprms[11] * tz * tz ) * tz;
   } else {

   /* Rigorous algorithm for large ZD */
      slaRefro ( zdo, aoprms[4], aoprms[5], aoprms[6], aoprms[7],
                  aoprms[8], aoprms[0], aoprms[9], 1e-8, &dref );
   }
   zdt = zdo + dref;

/* To Cartesian az,ZD */
   ce = sin ( zdt );
   xaet = cos ( az ) * ce;
   yaet = sin ( az ) * ce;
   zaet = cos ( zdt );

/* Cartesian az,ZD to Cartesian -HA,Dec */
   xmhda = sphi * xaet + cphi * zaet;
   ymhda = yaet;
   zmhda = - cphi * xaet + sphi * zaet;

/* Diurnal aberration */
   diurab = -aoprms[3];
   f = 1.0 - diurab * ymhda;
   v[0] = f * xmhda;
   v[1] = f * ( ymhda + diurab );
   v[2] = f * zmhda;

/* To spherical -HA,Dec */
   slaDcc2s ( v, &hma, dap );

/* Right ascension */
   *rap = slaDranrm ( st + hma );
}
