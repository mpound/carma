#include "slalib.h"
#include "slamac.h"
#include <string.h>
#include <limits.h>

static int idchi ( int, char*, int*, double* );                    /**/

/* Definitions shared between slaIntin and idchi */                /**/
#define NUMBER 0                                                   /**/
#define SPACE  1                                                   /**/
#define PLUS   2                                                   /**/
#define MINUS  3                                                   /**/
#define COMMA  4                                                   /**/
#define OTHER  5                                                   /**/
#define END    6                                                   /**/

void slaIntin ( char *string, int *nstrt, long *ireslt, int *jflag )
/*
**  - - - - - - - - -
**   s l a I n t i n
**  - - - - - - - - -
**
**  Convert free-format input into a long integer.
**
**  Given:
**     string    char*    string containing number to be decoded
**     nstrt     int*     where to start decode (1st = 1)
**     ireslt    long*    current value of result
**
**  Returned:
**     nstrt     int*     advanced to next number
**     ireslt    long*    result
**     jflag     int*     status: -1 = -OK, 0 = +OK, 1 = null, 2 = error
**
**  Called:  idchi
**
**  Notes:
**
**     1     The reason slaIntin has separate OK status values for +
**           and - is to enable minus zero to be detected.   This is
**           of crucial importance when decoding mixed-radix numbers.
**           For example, an angle expressed as deg, arcmin, arcsec
**           may have a leading minus sign but a zero degrees field.
**
**     2     A TAB is interpreted as a space.
**
**     3     The basic format is the sequence of fields #^, where
**           # is a sign character + or -, and ^ means a string of
**           decimal digits.
**
**     4     Spaces:
**
**             .  Leading spaces are ignored.
**
**             .  Spaces between the sign and the number are allowed.
**
**             .  Trailing spaces are ignored;  the first signifies
**                end of decoding and subsequent ones are skipped.
**
**     5     Delimiters:
**
**             .  Any character other than +,-,0-9 or space may be
**                used to signal the end of the number and terminate
**                decoding.
**
**             .  Comma is recognized by slaIntin as a special case;  it
**                is skipped, leaving the pointer on the next character.
**                See 9, below.
**
**     6     The sign is optional.  The default is +.
**
**     7     A "null result" occurs when the string of characters being
**           decoded does not begin with +,- or 0-9, or consists
**           entirely of spaces.  When this condition is detected, jflag
**           is set to 1 and ireslt is left untouched.
**
**     8     nstrt = 1 for the first character in the string.
**
**     9     On return from slaIntin, nstrt is set ready for the next
**           decode - following trailing blanks and any comma.  If a
**           delimiter other than comma is being used, nstrt must be
**           incremented before the next call to slaIntin, otherwise
**           all subsequent calls will return a null result.
**
**     10    Errors (jflag=2) occur when:
**
**             .  there is a + or - but no number;  or
**
**             .  the number is larger than LONG_MAX.
**
**     11    When an error has been detected, nstrt is left
**           pointing to the character following the last
**           one used before the error came to light.
**
**     12    See also slaFlotin and slaDfltin.
**
**  Last revision:   26 July 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int l_string, nptr;
   double digit;

/* Current state of the decode and the values it can take */

   int state;

#define seek_sign                       100
#define neg                             200
#define seek_1st_digit                  300
#define accept_digit                    400
#define seek_digit                      410
#define end_of_field                   1600
#define build_result                   1610
#define seeking_end_of_field           1630
#define next_field_OK                  1720
#define next_field_default             9100
#define null_field                     9110
#define next_field_error               9200
#define error                          9210
#define done                           9900


   int j;
   double dres;


/* Find string length */
   l_string = strlen ( string );

/* Current character index (1st = 0) */
   nptr = *nstrt - 1;

/* Set defaults: result & sign */
   dres = 0.0;
   j = 0;

/* Initialize state to "looking for sign" */
   state = seek_sign;

/* Loop until decode is complete */
   while ( state != done ) {
      switch ( state ) {

      case seek_sign :

      /* Look for sign */
         switch ( idchi ( l_string, string, &nptr, &digit ) ) {
         case NUMBER :
            state = accept_digit;
            break;
         case SPACE :
            state = seek_sign;
            break;
         case PLUS :
            state = seek_1st_digit;
            break;
         case MINUS :
            state = neg;
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

      case neg :

      /* Negative result */
         j = -1;

      case seek_1st_digit :

      /* Look for first leading decimal */
         switch ( idchi ( l_string, string, &nptr, &digit ) ) {
         case NUMBER :
            state = accept_digit;
            break;
         case SPACE :
            state = seek_1st_digit;
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

      case accept_digit :

      /* Accept decimals */
         dres = dres * 1e1 + digit;
         state = ( fabs ( dres ) <= LONG_MAX) ?
                       seek_digit : next_field_error;
         break;

      case seek_digit :

      /* Look for next decimal */
         switch ( idchi ( l_string, string, &nptr, &digit ) ) {
         case NUMBER :
            state = accept_digit;
            break;
         case SPACE :
            state = build_result;
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

      case end_of_field :

      /* Off the end of the field: move pointer back */
         nptr--;

      case build_result :

      /* Make the result */
         if ( j ) dres = - dres;
         *ireslt = (long) ( dnint ( dres ) );

      case seeking_end_of_field :

      /* Skip to end of field */
         switch ( idchi ( l_string, string, &nptr, &digit ) ) {
         case SPACE :
            state = seeking_end_of_field;
            break;
         case NUMBER :
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

static int idchi ( int l_string, char *string, int *nptr, double *digit )
/*
**  - - - - -
**   i d c h i
**  - - - - -
**
**  Internal routine used by slaIntin:
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
**     digit       double*     0.0 - 9.0 if character was a numeral
**
**  Returned (function value):
**                 int         vector for identified character:
**
**                                value   meaning
**
**                                NUMBER  0-9
**                                SPACE   space or tab
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

#define NCREC (15)
   static char kctab[NCREC] = { '0','1','2','3','4','5',
                                '6','7','8','9',
                                ' ','\t',
                                '+',
                                '-',
                                ',' };

   static int kvtab[NCREC] = { NUMBER, NUMBER, NUMBER, NUMBER, NUMBER,
                               NUMBER, NUMBER, NUMBER, NUMBER, NUMBER,
                               SPACE, SPACE,
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
            *digit = (double) ictab;

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
