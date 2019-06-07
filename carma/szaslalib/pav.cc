#include "slalib.h"
#include "slamac.h"
double slaPav ( float v1 [ 3 ], float v2 [ 3 ] )
/*
**  - - - - - - -
**   s l a P a v
**  - - - - - - -
**
**  Position angle of one celestial direction with respect to another.
**
**  (single precision)
**
**  Given:
**     v1    float[3]    direction cosines of one point
**     v2    float[3]    direction cosines of the other point
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
**  The routine slaBear performs an equivalent function except
**  that the points are specified in the form of spherical
**  coordinates.
**
**  Called:  slaDpav
**
**  Last revision:   9 December 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int i;
   double d1 [ 3 ], d2 [ 3 ];


/* Call the double precision version. */
   for ( i = 0; i < 3; i++ ) {
      d1 [ i ] = (double) v1 [ i ];
      d2 [ i ] = (double) v2 [ i ];
   }
   return (float) slaDpav ( d1, d2 );
}
