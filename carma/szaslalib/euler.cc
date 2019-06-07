#include "slalib.h"
#include "slamac.h"
void slaEuler ( char *order, float phi, float theta, float psi,
                float rmat[3][3] )
/*
**  - - - - - - - - -
**   s l a E u l e r
**  - - - - - - - - -
**
**  Form a rotation matrix from the Euler angles - three successive
**  rotations about specified Cartesian axes.
**
**  (single precision)
**
**  Given:
**    *order  char         specifies about which axes the rotations occur
**    phi     float        1st rotation (radians)
**    theta   float        2nd rotation (   "   )
**    psi     float        3rd rotation (   "   )
**
**  Returned:
**    rmat   float[3][3]   rotation matrix
**
**  A rotation is positive when the reference frame rotates
**  anticlockwise as seen looking towards the origin from the
**  positive region of the specified axis.
**
**  The characters of order define which axes the three successive
**  rotations are about.  A typical value is 'ZXZ', indicating that
**  rmat is to become the direction cosine matrix corresponding to
**  rotations of the reference frame through phi radians about the
**  old z-axis, followed by theta radians about the resulting x-axis,
**  then psi radians about the resulting z-axis.
**
**  The axis names can be any of the following, in any order or
**  combination:  x, y, z, uppercase or lowercase, 1, 2, 3.  Normal
**  axis labelling/numbering conventions apply;  the xyz (=123)
**  triad is right-handed.  Thus, the 'ZXZ' example given above
**  could be written 'ZXZ' or '313' (or even 'zxz' or '3xz').  Order
**  is terminated by length or by the first unrecognized character.
**
**  Fewer than three rotations are acceptable, in which case the later
**  angle arguments are ignored.  Zero rotations produces a unit rmat.
**
**  Called:  slaDeuler
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int j, i;
   double w[3][3];

/* Compute matrix in double precision */
   slaDeuler ( order, (double) phi, (double) theta, (double) psi, w );

/* Copy the result */
   for ( j = 0; j < 3; j++ ) {
      for ( i = 0; i < 3; i++ ) {
         rmat[i][j] = (float) w[i][j];
      }
   }
}
