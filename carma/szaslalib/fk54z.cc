#include "slalib.h"
#include "slamac.h"
void slaFk54z ( double r2000, double d2000, double bepoch,
                double *r1950, double *d1950,
                double *dr1950, double *dd1950 )
/*
**  - - - - - - - - -
**   s l a F k 5 4 z
**  - - - - - - - - -
**
**  Convert a J2000.0 FK5 star position to B1950.0 FK4 assuming
**  zero proper motion and parallax.
**
**  (double precision)
**
**  This routine converts star positions from the new, IAU 1976,
**  FK5, Fricke system to the old, Bessel-Newcomb, FK4 system.
**
**  Given:
**     r2000,d2000     double     J2000.0 FK5 RA,Dec (rad)
**     bepoch          double     Besselian epoch (e.g. 1950)
**
**  Returned:
**     *r1950,*d1950    double    B1950.0 FK4 RA,Dec (rad) at epoch BEPOCH
**     *dr1950,*dd1950  double    B1950.0 FK4 proper motions (rad/trop.yr)
**
**  Notes:
**
**  1)  The proper motion in RA is dRA/dt rather than cos(Dec)*dRA/dt.
**
**  2)  Conversion from Julian epoch 2000.0 to Besselian epoch 1950.0
**      only is provided for.  Conversions involving other epochs will
**      require use of the appropriate precession routines before and
**      after this routine is called.
**
**  3)  Unlike in the slaFK524 routine, the FK5 proper motions, the
**      parallax and the radial velocity are presumed zero.
**
**  4)  It is the intention that FK5 should be a close approximation
**      to an inertial frame, so that distant objects have zero proper
**      motion;  such objects have (in general) non-zero proper motion
**      in FK4, and this routine returns those fictitious proper
**      motions.
**
**  5)  The position returned by this routine is in the B1950
**      reference frame but at Besselian epoch bepoch.  For
**      comparison with catalogues the bepoch argument will
**      frequently be 1950.0.
**
**  Called:  slaFk524, slaPm
**
**  Last revision:   30 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   static double zero = 0.0;

   double r, d, px, rv;

/* FK5 equinox J2000 (any epoch) to FK4 equinox B1950 epoch B1950 */
   slaFk524 ( r2000, d2000, zero, zero, zero, zero,
               &r, &d, dr1950, dd1950, &px, &rv );

/* Fictitious proper motion to epoch bepoch */
   slaPm ( r, d, *dr1950, *dd1950, zero, zero, 1950.0, bepoch,
            r1950, d1950 );
}
