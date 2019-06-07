#include "slalib.h"
#include "slamac.h"
#include <string.h>
void slaDbjin ( char *string, int *nstrt,
                double *dreslt, int *jf1, int *jf2 )
/*
**  - - - - - - - - -
**   s l a D b j i n
**  - - - - - - - - -
**
**  Convert free-format input into double precision floating point,
**  using slaDfltin but with special syntax extensions.
**
**  The purpose of the syntax extensions is to help cope with mixed
**  FK4 and FK5 data.  In addition to the syntax accepted by slaDfltin,
**  the following two extensions are recognized by dbjin:
**
**     1)  A valid non-null field preceded by the character 'B'
**         (or 'b') is accepted.
**
**     2)  A valid non-null field preceded by the character 'J'
**         (or 'j') is accepted.
**
**  The calling program is notified of the incidence of either of these
**  extensions through an supplementary status argument.  The rest of
**  the arguments are as for slaDfltin.
**
**  Given:
**     *string    char      string containing field to be decoded
**     *nstrt     int       where to start decode (1st = 1)
**
**
**  Returned:
**     *nstrt     int       incremented
**     *dreslt    double    result
**     *jf1       int       dfltin status: -1 = -OK
**                                          0 = +OK
**                                         +1 = null field
**                                         +2 = error
**     *jf2       int       syntax flag:  0 = normal slaDfltin syntax
**                                       +1 = 'B' or 'b'
**                                       +2 = 'J' or 'j'
**
**  Called:  slaDfltin
**
**  For details of the basic syntax, see slaDfltin.
**
**  Last revision:   22 December 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int j2a, lenstr, na, j1a, nb, j1b;
   char c;

/* Preset syntax flag */
   j2a = 0;

/* Length of string */
   lenstr = strlen ( string );

/* Position of current character */
   na = *nstrt;

/* Attempt normal decode */
   slaDfltin ( string, &na, dreslt, &j1a );

/* Proceed only if pointer still within string */
   if ( ( na > 0 ) && ( na <= lenstr ) ) {

   /* See if slaDfltin reported a null field */
      if ( j1a == 1 ) {

      /* It did: examine character it stuck on */
         c = string[na-1];
         if ( c == 'B' || c == 'b' ) {

         /* 'B' or 'b' - provisionally note */
            j2a = 1;

         } else if ( c == 'J' || c == 'j' ) {

         /* 'J' or 'j' - provisionally note */
            j2a = 2;
         }

      /* Following B or J, attempt to decode a number */
         if ( j2a == 1 || j2a == 2 ) {
            nb = na + 1;
            slaDfltin ( string, &nb, dreslt, &j1b );

         /* If successful, copy pointer and status */
            if ( j1b <= 0 ) {
               na = nb;
               j1a = j1b;

         /* If not, forget about the B or J */
            } else {
               j2a = 0;
            }
         }
      }
   }

/* Return argument values and exit */
   *nstrt = na;
   *jf1 = j1a;
   *jf2 = j2a;
}
