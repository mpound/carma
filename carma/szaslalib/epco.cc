#include "slalib.h"
#include "slamac.h"
#include <ctype.h>
double slaEpco ( char k0, char k, double e )
/*
**  - - - - - - - -
**   s l a E p c o
**  - - - - - - - -
**
**  Convert an epoch into the appropriate form - 'B' or 'J'.
**
**  Given:
**     k0    char        form of result:  'B'=Besselian, 'J'=Julian
**     k     char        form of given epoch:  'B' or 'J'
**     e     double      epoch
**
**  Called:  slaEpb, slaEpj2d, slaEpj, slaEpb2d
**
**  Notes:
**
**     1) The result is always either equal to or very close to
**        the given epoch e.  The routine is required only in
**        applications where punctilious treatment of heterogeneous
**        mixtures of star positions is necessary.
**
**     2) k0 and k are not validated, and only their first characters
**        are used, interpreted as follows:
**
**        o  If k0 and k are the same the result is e.
**        o  If k0 is 'B' or 'b' and k isn't, the conversion is J to B.
**        o  In all other cases, the conversion is B to J.
**
**  Last revision:   18 November 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double result;
   int c;

   c = toupper ( (int) k0 );
   if ( c == toupper ( (int) k ) ) {
      result = e;
   } else {
      if ( c == (int) 'B' ) {
         result = slaEpb ( slaEpj2d ( e ) );
      } else {
         result = slaEpj ( slaEpb2d ( e ) );
      }
   }
   return ( result );
}
