#include "slalib.h"
#include "slamac.h"
void slaPolmo ( double elongm, double phim, double xp, double yp,
                double *elong, double *phi, double *daz )
/*
**  - - - - - - - - -
**   s l a P o l m o
**  - - - - - - - - -
**
**  Polar motion:  correct site longitude and latitude for polar
**  motion and calculate azimuth difference between celestial and
**  terrestrial poles.
**
**  Given:
**     elongm   double    mean longitude of the observer (radians, east +ve)
**     phim     double    mean geodetic latitude of the observer (radians)
**     xp       double    polar motion x-coordinate (radians)
**     yp       double    polar motion y-coordinate (radians)
**
**  Returned:
**     elong    double*   true longitude of the observer (radians, east +ve)
**     phi      double*   true geodetic latitude of the observer (radians)
**     daz      double*   azimuth correction (terrestrial-celestial, radians)
**
**  Notes:
**
**   1)  "Mean" longitude and latitude are the (fixed) values for the
**       site's location with respect to the IERS terrestrial reference
**       frame;  the latitude is geodetic.  TAKE CARE WITH THE LONGITUDE
**       SIGN CONVENTION.  The longitudes used by the present routine
**       are east-positive, in accordance with geographical convention
**       (and right-handed).  In particular, note that the longitudes
**       Returned by the slaObs routine are west-positive, following
**       astronomical usage, and must be reversed in sign before use in
**       the present routine.
**
**   2)  xp and yp are the (changing) coordinates of the Celestial
**       Ephemeris Pole with respect to the IERS Reference Pole.
**       xp is positive along the meridian at longitude 0 degrees,
**       and yp is positive along the meridian at longitude
**       270 degrees (i.e. 90 degrees west).  Values for xp,yp can
**       be obtained from IERS circulars and equivalent publications;
**       the maximum amplitude observed so far is about 0.3 arcseconds.
**
**   3)  "True" longitude and latitude are the (moving) values for
**       the site's location with respect to the celestial ephemeris
**       pole and the meridian which corresponds to the Greenwich
**       apparent sidereal time.  The true longitude and latitude
**       link the terrestrial coordinates with the standard celestial
**       models (for precession, nutation, sidereal time etc).
**
**   4)  The azimuths produced by slaAop and slaAopqk are with
**       respect to due north as defined by the Celestial Ephemeris
**       Pole, and can therefore be called "celestial azimuths".
**       However, a telescope fixed to the Earth measures azimuth
**       essentially with respect to due north as defined by the
**       IERS Reference Pole, and can therefore be called "terrestrial
**       azimuth".  Uncorrected, this would manifest itself as a
**       changing "azimuth zero-point error".  The value daz is the
**       correction to be added to a celestial azimuth to produce
**       a terrestrial azimuth.
**
**   5)  The present routine is rigorous.  For most practical
**       purposes, the following simplified formulae provide an
**       adequate approximation:
**
**       elong = elongm+xp*cos(elongm)-yp*sin(elongm);
**       phi   = phim+(xp*sin(elongm)+yp*cos(elongm))*tan(phim);
**       daz   = -sqrt(xp*xp+yp*yp)*cos(elongm-atan2(xp,yp))/cos(phim);
**
**       An alternative formulation for daz is:
**
**       x = cos(elongm)*cos(phim);
**       y = sin(elongm)*cos(phim);
**       daz = atan2(-x*yp-y*xp,x*x+y*y);
**
**   Reference:  Seidelmann, P.K. (ed), 1992.  "Explanatory Supplement
**               to the Astronomical Almanac", ISBN 0-935702-68-7,
**               sections 3.27, 4.25, 4.52.
**
**  Last revision:   22 February 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double sel, cel, sph, cph, xm, ym, zm, xnm, ynm, znm,
          sxp, cxp, syp, cyp, zw, xt, yt, zt, xnt, ynt;



/* Site mean longitude and mean geodetic latitude as a Cartesian vector. */
   sel = sin ( elongm );
   cel = cos ( elongm );
   sph = sin ( phim );
   cph = cos ( phim );

   xm = cel * cph;
   ym = sel * cph;
   zm = sph;

/* Rotate site vector by polar motion, Y-component then X-component. */
   sxp = sin ( xp );
   cxp = cos ( xp );
   syp = sin ( yp );
   cyp = cos ( yp );

   zw = ( - ym * syp + zm * cyp );

   xt = xm * cxp - zw * sxp;
   yt = ym * cyp + zm * syp;
   zt = xm * sxp + zw * cxp;

/* Rotate also the geocentric direction of the terrestrial pole (0,0,1). */
   xnm = - sxp * cyp;
   ynm = syp;
   znm = cxp * cyp;

   cph = sqrt ( xt * xt + yt * yt );
   if ( cph == 0.0 ) xt = 1.0;
   sel = yt / cph;
   cel = xt / cph;

/* Return true longitude and true geodetic latitude of site. */
   *elong = atan2 ( yt, xt );
   *phi = atan2 ( zt, cph );

/* Return current azimuth of terrestrial pole seen from site position. */
   xnt = ( xnm * cel + ynm * sel ) * zt - znm * cph;
   ynt = - xnm * sel + ynm * cel;
   *daz = atan2 ( - ynt, - xnt );

   return;
}
