#include "slalib.h"
#include "slamac.h"
#include <string.h>

static int idchf ( int, char*, int*, int*, double* );          /**/

/* Definitions shared between slaDfltin and idchf */           /**/
#define NUMBER 0                                               /**/
#define SPACE  1                                               /**/
#define EXPSYM 2                                               /**/
#define PERIOD 3                                               /**/
#define PLUS   4                                               /**/
#define MINUS  5                                               /**/
#define COMMA  6                                               /**/
#define OTHER  7                                               /**/
#define END    8                                               /**/

void slaDfltin ( char *string, int *nstrt, double *dreslt, int *jflag )
/*
**  - - - - - - - - - -
**   s l a D f l t i n
**  - - - - - - - - - -
**
**  Convert free-format input into double precision floating point.
**
**  Given:
**     *string     char       string containing field to be decoded
**     *nstrt      int        where to start decode (1st = 1)
**
**  Returned:
**     *nstrt      int        advanced to next field
**     *dreslt     double     result
**     *jflag      int        -1 = -OK, 0 = +OK, 1 = null field, 2 = error
**
**  Called:  idchf
**
**  Notes:
**
**     1     A tab character is interpreted as a space, and lower
**           case d,e are interpreted as upper case.
**
**     2     The basic format is #^.^@#^ where # means + or -,
**           ^ means a decimal subfield and @ means D or E.
**
**     3     Spaces:
**             Leading spaces are ignored.
**             Embedded spaces are allowed only after # and D or E,
**             and after . where the first ^ is absent.
**             Trailing spaces are ignored;  the first signifies
**             end of decoding and subsequent ones are skipped.
**
**     4     Field separators:
**             Any character other than +,-,0-9,.,D,E or space may be
**             used to end a field.  Comma is recognized by slaDfltin
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
**           +,-,0-9,.,D or E, or consists entirely of spaces.
**           If the field is null, jflag is set to 1 and dreslt
**           is left untouched.
**
**     11    nstrt = 1 for the first character in the string.
**
**     12    On return from slaDfltin, nstrt is set ready for the next
**           decode - following trailing blanks and (if used) the
**           comma separator.  If a separator other than comma is
**           being used, nstrt must be incremented before the next
**           call to slaDfltin.
**
**     13    Errors (jflag=2) occur when:
**             a)  A +, -, D or E is left unsatisfied.
**             b)  The decimal point is present without at least
**                 one decimal subfield.
**             c)  An exponent more than 100 has been presented.
**
**     14    When an error has been detected, nstrt is left
**           pointing to the character following the last
**           one used before the error came to light.  This
**           may be after the point at which a more sophisticated
**           program could have detected the error.  For example,
**           slaDfltin does not detect that '1e999' is unacceptable
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
**     18    See also slaFlotin and slaIntin.
**
**  Last revision:   26 July 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int l_string, nptr, ndigit;
   double digit;

/* Current state of the decode and the values it can take */

   int state;

#define seek_sign                       100
#define neg_mant                        200
#define seek_1st_leading_digit          300
#define accept_leading_digit            400
#define seek_digit_when_none_before_pt  500
#define seek_trailing_digit             600
#define accept_trailing_digit           700
#define accept_uns_exp_no_mant          800
#define seek_sign_exp                   900
#define neg_exp                        1000
#define seek_1st_exp_digit             1100
#define accept_exp_digit               1200
#define end_of_field                   1300
#define build_result                   1310
#define seeking_end_of_field           1620
#define next_field_OK                  1720
#define next_field_default             9100
#define null_field                     9110
#define next_field_error               9200
#define error                          9210
#define done                           9900


   int msign, nexp, ndp, isignx, j;
   double dmant;


/* Find string length */
   l_string = strlen ( string );

/* Current character index */
   nptr = *nstrt - 1;

/* Set defaults: mantissa & sign, exponent & sign, decimal place count */
   dmant = 0.0;
   msign = 1;
   nexp = 0;
   isignx = 1;
   ndp = 0;

/* Initialize state to "looking for sign" */
   state = seek_sign;

/* Loop until decode is complete */
   while ( state != done ) {
      switch ( state ) {

      case seek_sign :

      /* Look for sign */
         switch ( idchf ( l_string, string, &nptr, &ndigit, &digit ) ) {
         case NUMBER :
            state = accept_leading_digit;
            break;
         case SPACE :
            state = seek_sign;
            break;
         case EXPSYM :
            state = accept_uns_exp_no_mant;
            break;
         case PERIOD :
            state = seek_digit_when_none_before_pt;
            break;
         case PLUS :
            state = seek_1st_leading_digit;
            break;
         case MINUS :
            state = neg_mant;
            break;
         case OTHER :
            state = next_field_default;
            break;
         case COMMA :
         case END :
            state = null_field;
            break;
         default :
            state = error;
         }
         break;

      case neg_mant :

      /* Negative mantissa */
         msign = -1;

      case seek_1st_leading_digit :

      /* Look for first leading decimal */
         switch ( idchf ( l_string, string, &nptr, &ndigit, &digit ) ) {
         case NUMBER :
            state = accept_leading_digit;
            break;
         case SPACE :
            state = seek_1st_leading_digit;
            break;
         case EXPSYM :
            state = accept_uns_exp_no_mant;
            break;
         case PERIOD :
            state = seek_digit_when_none_before_pt;
            break;
         case PLUS :
         case MINUS :
         case COMMA :
         case OTHER :
            state = next_field_error;
            break;
         case END :
         default :
            state = error;
         }
         break;

      case accept_leading_digit :

      /* Accept leading decimals */
         dmant = dmant * 1e1 + digit;
         switch ( idchf ( l_string, string, &nptr, &ndigit, &digit ) ) {
         case NUMBER :
            state = accept_leading_digit;
            break;
         case SPACE :
            state = build_result;
            break;
         case EXPSYM :
            state = seek_sign_exp;
            break;
         case PERIOD :
            state = seek_trailing_digit;
            break;
         case PLUS :
         case MINUS :
         case COMMA :
         case OTHER :
            state = end_of_field;
            break;
         case END :
            state = build_result;
            break;
         default :
            state = error;
         }
         break;

      case seek_digit_when_none_before_pt :

      /* Look for decimal when none preceded the point */
         switch ( idchf ( l_string, string, &nptr, &ndigit, &digit ) ) {
         case NUMBER :
            state = accept_trailing_digit;
            break;
         case SPACE :
            state = seek_digit_when_none_before_pt;
            break;
         case EXPSYM :
         case PERIOD :
         case PLUS :
         case MINUS :
         case COMMA :
         case OTHER :
            state = next_field_error;
            break;
         case END :
         default :
            state = error;
         }
         break;

      case seek_trailing_digit :

      /* Look for trailing decimals */
         switch ( idchf ( l_string, string, &nptr, &ndigit, &digit ) ) {
         case NUMBER :
            state = accept_trailing_digit;
            break;
         case EXPSYM :
            state = seek_sign_exp;
            break;
         case PERIOD :
         case PLUS :
         case MINUS :
         case COMMA :
         case OTHER :
            state = end_of_field;
            break;
         case SPACE :
         case END :
            state = build_result;
            break;
         default :
            state = error;
         }
         break;

      case accept_trailing_digit :

      /* Accept trailing decimals */
         ndp++;
         dmant = dmant * 1e1 + digit;
         state = seek_trailing_digit;
         break;

      case accept_uns_exp_no_mant :

      /* Exponent symbol first in field: default mantissa to 1 */
         dmant = 1.0;

      case seek_sign_exp :

      /* Look for sign of exponent */
         switch ( idchf ( l_string, string, &nptr, &ndigit, &digit ) ) {
         case NUMBER :
            state = accept_exp_digit;
            break;
         case SPACE :
            state = seek_sign_exp;
            break;
         case PLUS :
            state = seek_1st_exp_digit;
            break;
         case MINUS :
            state = neg_exp;
            break;
         case EXPSYM :
         case PERIOD :
         case COMMA :
         case OTHER :
            state = next_field_error;
            break;
         case END :
         default :
            state = error;
         }
         break;

      case neg_exp :

      /* Exponent negative */
         isignx = -1;

      case seek_1st_exp_digit :

      /* Look for first digit of exponent */
         switch ( idchf ( l_string, string, &nptr, &ndigit, &digit ) ) {
         case NUMBER :
            state = accept_exp_digit;
            break;
         case SPACE :
            state = seek_1st_exp_digit;
            break;
         case EXPSYM :
         case PERIOD :
         case PLUS :
         case MINUS :
         case COMMA :
         case OTHER :
            state = next_field_error;
            break;
         case END :
         default :
            state = error;
         }
         break;

      case accept_exp_digit :

      /* Use exponent digit */
         nexp = nexp * 10 + ndigit;
         if ( nexp > 100 ) {
            state = next_field_error;
         } else {

         /* Look for subsequent digits of exponent */
            switch ( idchf ( l_string, string, &nptr, &ndigit, &digit ) ) {
            case NUMBER :
               state = accept_exp_digit;
               break;
            case SPACE :
               state = build_result;
               break;
            case EXPSYM :
            case PERIOD :
            case PLUS :
            case MINUS :
            case COMMA :
            case OTHER :
               state = end_of_field;
               break;
            case END :
               state = build_result;
               break;
            default :
               state = error;
            }
         }
         break;

      case end_of_field :

      /* Off the end of the field: move pointer back */
         nptr--;

      case build_result :

      /* Combine exponent and decimal place count */
         nexp = nexp * isignx - ndp;

      /* Sign of exponent? */
         if ( nexp >= 0 ) {

         /* Positive exponent: scale up */
            while ( nexp >= 10 ) {
               dmant *= 1e10;
               nexp -= 10;
            }
            while ( nexp >= 1 ) {
               dmant *= 1e1;
               nexp--;
            }
         } else {

         /* Negative exponent: scale down */
            while ( nexp <= -10 ) {
               dmant /= 1e10;
               nexp += 10;
            }
            while ( nexp <= -1 ) {
               dmant /= 1e1;
               nexp++;
            }
         }

      /* Get result & status */
         if ( msign == 1 ) {
             *dreslt = dmant;
             j = 0;
         } else {
             *dreslt = -dmant;
             j = -1;
         }

      case seeking_end_of_field :

      /* Skip to end of field */
         switch ( idchf ( l_string, string, &nptr, &ndigit, &digit ) ) {
         case SPACE :
            state = seeking_end_of_field;
            break;
         case NUMBER :
         case EXPSYM :
         case PERIOD :
         case PLUS :
         case MINUS :
         case OTHER :
            state = next_field_OK;
            break;
         case COMMA :
         case END :
            state = done;
            break;
         default :
            state = error;
         }
         break;

      case next_field_OK :

      /* Next field terminates successful decode */
         nptr--;
         state = done;
         break;

      case next_field_default :

      /* Next field terminates null decode */
         nptr--;

      case null_field :

      /* Null decode */
         j = 1;
         state = done;
         break;

      case next_field_error :

      /* Next field detected prematurely */
         nptr--;

      case error :

      /* Decode has failed: set bad status */
         j = 2;
         state = done;
         break;

      default :
         state = error;
      }
   }

/* Finished: return updated pointer and the status */
   *nstrt = nptr + 1;
   *jflag = j;
}

static int idchf ( int l_string, char *string,
                   int *nptr, int *ndigit, double *digit )
/*
**  - - - - -
**   i d c h f
**  - - - - -
**
**  Internal routine used by slaDfltin:
**
**  identify next character in string.
**
**  Given:
**     l_string    int         length of string
**     string      char*       string
**     nptr        int*        character to be identified (1st = 0)
**
**  Returned:
**     nptr        int*        incremented unless end of field
**     ndigit      int*        0-9 if character was a numeral
**     digit       double*     (double) ndigit
**
**  Returned (function value):
**     idchf       int         vector for identified character:
**
**                                value   meaning
**
**                                NUMBER  0-9
**                                SPACE   space or tab
**                                EXPSYM  D, d, E or e
**                                PERIOD  .
**                                PLUS    +
**                                MINUS   -
**                                COMMA   ,
**                                OTHER   else
**                                END     outside field
**
**  Last revision:   24 June 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int ivec, ictab;
   char c;

/* Character/vector tables */

#define NCREC (20)
   static char kctab[NCREC] = { '0','1','2','3','4','5',
                                '6','7','8','9',
                                ' ','\t',
                                'D','d','E','e',
                                '.',
                                '+',
                                '-',
                                ',' };

   static int kvtab[NCREC] = { NUMBER, NUMBER, NUMBER, NUMBER, NUMBER,
                               NUMBER, NUMBER, NUMBER, NUMBER, NUMBER,
                               SPACE, SPACE,
                               EXPSYM, EXPSYM, EXPSYM, EXPSYM,
                               PERIOD,
                               PLUS,
                               MINUS,
                               COMMA };


/* Initialize returned value */
   ivec = OTHER;

/* Pointer outside field? */
   if ( *nptr < 0 || *nptr >= l_string ) {

   /* Yes: prepare returned value */
      ivec = END;

   } else {

   /* Not end of field: identify character */
      c = string [ *nptr ];
      for ( ictab = 0; ictab < NCREC; ictab++ ) {
         if ( kctab [ ictab ] == c ) {

         /* Recognized */
            ivec = kvtab [ ictab ];

         /* Allow for numerals */
            *ndigit = ictab;
            *digit = (double) *ndigit;

         /* Quit the loop */
            break;
         }
      }

   /* Increment pointer */
      ( *nptr )++;
   }

/* Return the value identifying the character */
   return ivec;
}
