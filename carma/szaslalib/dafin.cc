#include "slalib.h"
#include "slamac.h"
void slaDafin ( char *string, int *iptr, double *a, int *j )
/*
**  - - - - - - - - -
**   s l a D a f i n
**  - - - - - - - - -
**
**  Sexagesimal character string to angle.
**
**  (double precision)
**
**  Given:
**     string  char*   string containing deg, arcmin, arcsec fields
**     iptr    int     where to start decode at (1st = 1)
**
**  Returned:
**     iptr    int     advanced past the decoded angle
**     a       double  angle in radians
**     j       int     status:  0 = OK
**                             +1 = default, A unchanged
**                             -1 = bad degrees      )
**                             -2 = bad arcminutes   )  (note 3)
**                             -3 = bad arcseconds   )
**
**  Example:
**
**    argument    before                           after
**
**    string      '-57 17 44.806  12 34 56.7'      unchanged
**    iptr        1                                16 (points to 12...)
**
**    a           ?                                -1.00000
**    j           ?                                0
**
**    A further call to slaDafin, without adjustment of iptr, will
**    decode the second angle, 12deg 34min 56.7sec.
**
**  Notes:
**
**     1)  The first three "fields" in string are degrees, arcminutes,
**         arcseconds, separated by spaces or commas.  The degrees field
**         may be signed, but not the others.  The decoding is carried
**         out by the dfltin routine and is free-format.
**
**     2)  Successive fields may be absent, defaulting to zero.  For
**         zero status, the only combinations allowed are degrees alone,
**         degrees and arcminutes, and all three fields present.  If all
**         three fields are omitted, a status of +1 is returned and a is
**         unchanged.  In all other cases a is changed.
**
**     3)  Range checking:
**           The degrees field is not range checked.  However, it is
**           expected to be integral unless the other two fields are absent.
**           The arcminutes field is expected to be 0-59, and integral if
**           the arcseconds field is present.  If the arcseconds field
**           is absent, the arcminutes is expected to be 0-59.9999...
**           The arcseconds field is expected to be 0-59.9999...
**
**     4)  Decoding continues even when a check has failed.  Under these
**         circumstances the field takes the supplied value, defaulting
**         to zero, and the result a is computed and returned.
**
**     5)  Further fields after the three expected ones are not treated
**         as an error.  The pointer iptr is left in the correct state
**         for further decoding with the present routine or with slaDfltin
**         etc.  See the example, above.
**
**     6)  If string contains hours, minutes, seconds instead of degrees
**         etc, or if the required units are turns (or days) instead of
**         radians, the result a should be multiplied as follows:
**           for        to obtain    multiply
**           string     a in         a by
**           d ' "      radians      1       =  1.0
**           d ' "      turns        1/2pi   =  0.1591549430918953358
**           h m s      radians      15      =  15.0
**           h m s      days         15/2pi  =  2.3873241463784300365
**
**  Called:  slaDfltin
**
**  Defined in slamac.h:  DAS2R
**
**  Last revision:   1 August 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
/* Local variables */
   int jd, jf, jm, js;
   double arcsec, arcmin, deg;

/* Preset the status to OK */
   jf = 0;

/* Defaults */
   deg = 0.0;
   arcmin = 0.0;
   arcsec = 0.0;

/* Decode degrees, arcminutes, arcseconds */
   slaDfltin ( string, iptr, &deg, &jd );
   if ( jd > 1 ) {
      jf = -1;
   } else {
      slaDfltin ( string, iptr, &arcmin, &jm );
      if ( jm < 0 || jm > 1 ) {
         jf = -2;
      } else {
         slaDfltin ( string, iptr, &arcsec, &js );
         if ( js < 0 || js > 1 ) {
            jf = -3;

      /* See if the combination of fields is credible */
         } else if ( jd > 0 ) {

         /* No degrees:  arcmin, arcsec ought also to be absent */
            if ( jm == 0 ) {

            /* Suspect arcmin */
               jf = -2;
            } else if ( js == 0 ) {

            /* Suspect arcsec */
               jf = -3;
            } else {

            /* All three fields absent */
               jf = 1;
            }

      /* Degrees present:  if arcsec present so ought arcmin to be */
         } else if ( jm != 0 && js == 0 ) {
            jf = -3;

      /* Tests for range and integrality */

      /* Degrees */
         } else if ( jm == 0 && dint ( deg ) != deg ) {
            jf = -1;

      /* Arcminutes */
         } else if ( ( js == 0 && dint ( arcmin ) != arcmin )
                     || arcmin >= 60.0 ) {
            jf = -2;

      /* Arcseconds */
         } else if ( arcsec >= 60.0 ) {
            jf = -3;
         }
      }
   }

/* Unless all three fields absent, compute angle value */
   if ( jf <= 0 ) {
      *a = ( ( fabs ( deg ) * 60.0 + arcmin ) * 60.0 + arcsec ) * DAS2R;
      if (jd < 0) {
           *a = -(*a);
      }
   }

/* Return the status */
   *j = jf;
}
