#include "slalib.h"
#include "slamac.h"
double slaDpav ( double v1 [ 3 ], double v2 [ 3 ] )
/*
**  - - - - - - - -
**   s l a D p a v
**  - - - - - - - -
**
**  Position angle of one celestial direction with respect to another.
**
**  (double precision)
**
**  Given:
**     v1    double[3]    direction cosines of one point
**     v2    double[3]    direction cosines of the other point
**
**  (The coordinate frames correspond to RA,Dec, Long,Lat etc.)
**
**  The result is the bearing (position angle), in radians, of point
**  v2 with respect to point v1.  It is in the range +/- pi.  The
**  sense is such that if v2 is a small distance east of v1, the
**  bearing is about +pi/2.  Zero is returned if the two points
**  are coincident.
**
**  The vectors v1 and v2 need not be unit vectors.
**
**  The routine slaDbear performs an equivalent function except
**  that the points are specified in the form of spherical
**  coordinates.
**
**  Last revision:   9 December 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double x1, y1, z1, w, r, xu1, yu1, zu1, dx, dy, dz, sq, cq;


/* Unit vector to point 1. */
   x1 = v1 [ 0 ];
   y1 = v1 [ 1 ];
   z1 = v1 [ 2 ];
   w = sqrt ( x1 * x1 + y1 * y1 + z1 * z1 );
   if ( w != 0.0 ) {
      x1 /= w;
      y1 /= w;
      z1 /= w;
   }

/* Unit vector "north" from point 1. */
   r = sqrt ( x1 * x1 + y1 * y1 );
   r = r == 0.0 ? 1e-5 : r;
   w = z1 / r;
   xu1 = - x1 * w;
   yu1 = - y1 * w;
   zu1 = r;

/* Vector from point 1 to point 2. */
   dx = v2 [ 0 ] - x1;
   dy = v2 [ 1 ] - y1;
   dz = v2 [ 2 ] - z1;

/* Position angle. */
   sq = dx * yu1 * z1 + dy * zu1 * x1 + dz * xu1 * y1
      - dz * yu1 * x1 - dy * xu1 * z1 - dx * zu1 * y1;
   cq = dx * xu1 + dy * yu1 + dz * zu1;
   if ( sq == 0.0 && cq == 0.0 ) cq = 1.0;
   return atan2 ( sq, cq );
}
