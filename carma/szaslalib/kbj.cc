#include "slalib.h"
#include "slamac.h"
void slaKbj ( int jb, double e, char *k, int *j )
/*
**  - - - - - - -
**   s l a K b j
**  - - - - - - -
**
**  Select epoch prefix 'B' or 'J'.
**
**  Given:
**     jb     int         slaDbjin prefix status:  0=none, 1='B', 2='J'
**     e      double      epoch - Besselian or Julian
**
**  Returned:
**     *k     char        'B' or 'J'
**     *j     int         status:  0=OK
**
**  If jb=0, B is assumed for e < 1984.0, otherwise J.
**
**  Last revision:   23 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{

/* Preset status */
   *j = 0;

/* If prefix given expressly, use it */
   if ( jb == 1 ) {
      *k = 'B';
   } else if ( jb == 2 ) {
      *k = 'J';

/* If no prefix, examine the epoch */
   } else if ( jb == 0 ) {

   /* If epoch is pre-1984.0, assume Besselian;  otherwise Julian */
      if ( e < 1984.0 ) {
         *k = 'B';
      } else {
         *k = 'J';
      }

/* If illegal prefix, return error status */
   } else {
      *k = ' ';
      *j = 1;
   }
}
