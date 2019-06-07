#include "slalib.h"
#include "slamac.h"
void slaAoppat ( double date, double aoprms[14] )
/*
**  - - - - - - - - - -
**   s l a A o p p a t
**  - - - - - - - - - -
**
**  Recompute the sidereal time in the apparent to observed place
**  star-independent parameter block.
**
**  Given:
**     date   double      UTC date/time (Modified Julian Date, JD-2400000.5)
**                        (see slaAoppa source for comments on leap seconds)
**     aoprms double[14]  star-independent apparent-to-observed parameters
**
**       (0-11)   not required
**       (12)     longitude + eqn of equinoxes + sidereal dut
**       (13)     not required
**
**  Returned:
**     aoprms double[14]  star-independent apparent-to-observed parameters:
**
**       (0-12)   not changed
**       (13)     local apparent sidereal time (radians)
**
**  For more information, see slaAoppa.
**
**  Called:  slaGmst
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   aoprms[13] = slaGmst ( date ) + aoprms[12];
}
