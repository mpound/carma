#include "slalib.h"
#include "slamac.h"
void slaAfin ( char *string, int *iptr, float *a, int *j )
/*
**  - - - - - - - -
**   s l a A f i n
**  - - - - - - - -
**
**  Sexagesimal character string to angle.
**
**  (single precision)
**
**  Given:
**     string  c*(*)   string containing deg, arcmin, arcsec fields
**     iptr    int     where to start decode (1st = 1)
**
**  Returned:
**     iptr    int     advanced past the decoded angle
**     a       float   angle in radians
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
**    a           ?                                -1.00000f
**    j           ?                                0
**
**    A further call to slaAfin, without adjustment of iptr, will
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
**           d ' "      radians      1       =  1.0f
**           d ' "      turns        1/2pi   =  0.1591549430918953358f
**           h m s      radians      15      =  15.0f
**           h m s      days         15/2pi  =  2.3873241463784300365f
**
**  Called:  slaDfltin
**
**  Last revision:   16 November 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double ad;

/* Call the double precision version */
   slaDafin ( string, iptr, &ad, j );
   if ( *j <= 0 ) *a = (float) ad;
}
