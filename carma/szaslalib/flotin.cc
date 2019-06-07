#include "slalib.h"
#include "slamac.h"
void slaFlotin ( char *string, int *nstrt, float *reslt, int *jflag )
/*
**  - - - - - - - - - -
**   s l a F l o t i n
**  - - - - - - - - - -
**
**  Convert free-format input into single precision floating point.
**
**  Given:
**     *string      char      string containing field to be decoded
**     *nstrt       int       where to start decode (1st = 1)
**
**  Returned:
**     *nstrt       int        advanced to next field
**     *reslt       float      result
**     *jflag       int        -1 = -OK, 0 = +OK, 1 = null field, 2 = error
**
**  Called:  slaDfltin
**
**  Notes:
**
**     1     A tab character is interpreted as a space, and lower
**           case d,e are interpreted as upper case.
**
**     2     The basic format is #^.^@#^ where # means + or -,
**           ^ means a decimal subfield and @ means d or e.
**
**     3     Spaces:
**             Leading spaces are ignored.
**             Embedded spaces are allowed only after # and d or e,
**             and after . where the first ^ is absent.
**             Trailing spaces are ignored;  the first signifies
**             end of decoding and subsequent ones are skipped.
**
**     4     Field separators:
**             Any character other than +,-,0-9,.,d,e or space may be
**             used to end a field.  Comma is recognized by slaFlotin
**             as a special case; it is skipped, leaving the
**             pointer on the next character.  See 12, below.
**
**     5     Both signs are optional.  The default is +.
**
**     6     The mantissa defaults to 1.
**
**     7     The exponent defaults to e0.
**
**     8     The decimal subfields may be of any length.
**
**     9     The decimal point is optional for whole numbers.
**
**     10    A null field is one that does not begin with
**           +,-,0-9,.,d or e, or consists entirely of spaces.
**           If the field is null, jflag is set to 1 and reslt
**           is left untouched.
**
**     11    nstrt = 1 for the first character in the string.
**
**     12    On return from slaFlotin, nstrt is set ready for the next
**           decode - following trailing blanks and (if used) the
**           comma separator.  If a separator other than comma is
**           being used, nstrt must be incremented before the next
**           call to slaFlotin.
**
**     13    Errors (jflag=2) occur when:
**             a)  A +, -, d or e is left unsatisfied.
**             b)  The decimal point is present without at least
**                 one decimal subfield.
**             c)  An exponent more than 100 has been presented.
**
**     14    When an error has been detected, nstrt is left
**           pointing to the character following the last
**           one used before the error came to light.  This
**           may be after the point at which a more sophisticated
**           program could have detected the error.  For example,
**           slaFlotin does not detect that '1e999' is unacceptable
**           until the whole field has been read.
**
**     15    Certain highly unlikely combinations of mantissa &
**           exponent can cause arithmetic faults during the
**           decode, in some cases despite the fact that they
**           together could be construed as a valid number.
**
**     16    Decoding is left to right, one pass.
**
**     17    End of field may occur in either of two ways:
**             a)  As dictated by the string length.
**             b)  Detected during the decode.
**                 (b overrides a.)
**
**     18    See also slaDfltin and slaIntin.
**
**  Last revision:   23 November 1995
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double dreslt;

/* Call the double precision version */
   slaDfltin ( string, nstrt, &dreslt, jflag );
   if ( *jflag <= 0 ) *reslt = (float) dreslt;
}
