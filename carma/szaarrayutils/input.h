#ifndef input_h
#define input_h

#include <stdarg.h>
#include <stdio.h>  /* (FILE *) */
#include "carma/szaarrayutils/freelist.h"

/* Declare the tag for a generic input stream. */

typedef struct InputStream InputStream;

/* Declare the tag for a generic input source. */

typedef struct InputSource InputSource;

/* List iterator-specific method function declarations */

/*
 * Read a single character from an input source and place it in
 * InputSource::nextc.
 *
 * Input:
 *  source InputSource *  The input source to read from.
 * Output:
 *  return         int    0 - OK.
 *                        1 - EOF reached.
 */
#define INPUT_READ_FN(fn) int (fn)(InputSource *source)

/*
 * Delete a source context (as recorded in InputSource::data).
 *
 * Input:
 *  data    void *  The context data to be deleted.
 * Output:
 *  return  void *  The deleted context data (always NULL).
 */
#define INPUT_DEL_FN(fn) void *(fn)(void *data)

/*
 * Perform the equivalent of clearerr() on a source.
 *
 * Input:
 *  source   InputSource *  The input source to be reset.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Couldn't clear error status.
 */
#define INPUT_CLR_FN(fn) int (fn)(InputSource *source)

/*
 * Report the source and position of the stream to stderr in a form
 * that is suitable to be used as the prefix of an error message.
 *
 * Input:
 *  source   InputSource *  The input source to describe.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
#define INPUT_ERR_FN(fn) int (fn)(InputSource *source)

/* Set the maximum length of any input token */

enum {INPUT_WORKLEN=10240};

/* Set the length of the error-message buffer */

enum {INPUT_ERR_LEN=81};

/* Define the distinguishing members of a specific input-stream */

struct InputSource {
  INPUT_READ_FN(*read_fn);   /* Function to read one character into nextc */
  INPUT_CLR_FN(*clr_fn);     /* Function to reset the input stream */
  INPUT_ERR_FN(*err_fn);     /* Function that generates an error prefix */
  INPUT_DEL_FN(*del_fn);     /* Iterator destructor */
  int nextc;                 /* The next character in the input source */
  int was_escape;            /* True if last character was an unescaped '\' */
  int escaped;               /* The escaped character */
  void *data;                /* Source-specific data */
  InputSource *next;         /* The input source that created this one */
};

/* Declare a generic input stream container */

struct InputStream {
  InputSource *source;       /* The top of a stack of input sources */
  FreeList *source_mem;      /* A free-list of InputSource objects */
  int nextc;                 /* The next character in the input stream */
  int in_string;             /* True while reading chars from within a string */
  char work[INPUT_WORKLEN];  /* Work buffer for decoding lexical components */
};

/*
 * Construct an initially closed input-stream, ready to be connected
 * to an input source with open_InputStream().
 */
InputStream *new_InputStream(void);

/*
 * Declare a generic stream destructor.
 * Note that this will call close_InputStream() if the stream is
 * connected to an input source.
 */
InputStream *del_InputStream(InputStream *stream);

/*
 * Connect an input stream to a specific input source. This function
 * should only be called from type-specific stream open_*()
 * functions (see below). Note that if another input source is currently
 * connected, it will be pushed down a stack of input sources, and reading
 * will resume from it when the end of new input source has been reached.
 */
int open_InputStream(InputStream *stream, void *data,
		     INPUT_READ_FN(*read_fn), INPUT_CLR_FN(*clr_fn),
		     INPUT_ERR_FN(*err_fn), INPUT_DEL_FN(*del_fn));

/*
 * Close all input sources of an input stream. Future reads will return
 * an error until the stream is successfully re-opened to another input source.
 */
void close_InputStream(InputStream *stream);

/*
 * Declare functions that connect generic input streams to
 * specific types of input sources.
 */
int open_FileInputStream(InputStream *stream, char *dir, char *name);
int open_StringInputStream(InputStream *stream, int copy,char *string);
int open_StdioInputStream(InputStream *stream, int do_close, FILE *fp);

/*
 * The following function resets a stream by clearing its error
 * flags if possible, and reading up to the first character of
 * the next line. On failure it returns non-zero.
 */
int reset_InputStream(InputStream *stream);

/*
 * The following function reads the next character from the input stream.
 * In the process it removes escaped newline characters and comments.
 * The tell argument should be 1 if parse errors are to be reported to
 * stderr.
 */
int read_InputStream(InputStream *stream, int tell);

/*
 * Canned lexical input functions.
 */
int input_skip_to_eol(InputStream *stream, int tell);
int input_skip_past_eol(InputStream *stream, int tell);
int input_skip_space(InputStream *stream, int tell, int advance);
int input_skip_white(InputStream *stream, int tell, int advance);

/*
 * Read an ASCII keyword from an input stream. Keywords must commence
 * with an alphabetical character continue with alphanumeric
 * and underscore characters. The output keyword will be left in
 * stream->work[], where it will have been converted to lower case
 * if the fold argument is true.
 */
int input_keyword(InputStream *stream, int tell, int fold);
int inputEnumKeyword(InputStream *stream, int tell, int fold);
int input_regexp_keyword(InputStream *stream, int tell, int fold);
int input_board_regexp_keyword(InputStream *stream, int tell, int fold);

/*
 * Read a string that is enclosed in double quotes. All C-style
 * escape sequences (eg. \n) are recognized. The string will be
 * left in stream->work[].
 */
int input_quoted_string(InputStream *stream, int tell);

/*
 * The following macro defines the prototype of functions like
 * isspace(), to be used by input_literal() to identify characters
 * to include in the string.
 */
#define IS_LITERAL_FN(fn) int (fn)(int c)

/*
 * Read an unquoted string. The chararacters listed in opn[] and cls[]
 * are treated as open and close parentheses. The string will be
 * terminated when the provided is_literal() function returns 0
 * outside of sub-strings and unmatched parentheses. The only
 * exception to this is if the terminating character is a newline
 * character and this character was preceded (ignoring intervening
 * spaces) by one of the characters in nl_escapes[]. The string will
 * be left in stream->work[]. The terminating character will be left
 * in the stream.
 */
int input_literal(InputStream *stream, int tell, char *opn, char *cls,
		  IS_LITERAL_FN(*is_literal), char *nl_escapes);

/*
 * Read an ASCII word from an input stream. A word is defined as a string
 * of printable characters excluding white-space. If fold is true, fold
 * upper case characters to lower case. The string will be left in
 * stream->work[].
 */
int input_word(InputStream *stream, int tell, int fold);

/*
 * Read a signed integer. If anybase is true, interpret a number
 * that starts with a zero as octal, one that starts with 0x as
 * hexadecimal and one that starts with 0b as binary. Otherwise only
 * accept base 10 numbers.
 */
int input_long(InputStream *stream, int tell, int anybase, long *lval);

/*
 * Read an unsigned integer. If anybase is true, interpret a number
 * that starts with a zero as octal, one that starts with 0x as
 * hexadecimal and one that starts with 0b as binary. Otherwise only
 * accept base 10 numbers.
 */
int input_ulong(InputStream *stream, int tell, int anybase, unsigned long *ulval);

/*
 * Read a double precision number.
 */
int input_double(InputStream *stream, int tell, double *dval);

typedef struct {
  enum {
    NUM_DOUBLE,   /* Number was written as an int with no exponent */
    NUM_INT       /* Number was written as floating point and/or with exponent*/
  } type;
  int sign;       /* Sign of number as -1 or 1 */
  union {
    double dval;  /* Use if type==NUM_DOUBLE */
    int ival;     /* Use if type==NUM_INT */
  } value;
} Number;

int input_number(InputStream *stream, int tell, int sign_ok, Number *number);

/*
 * Read one or more sexagesimal components separated by ':' characters.
 * The first number can have any integral value and can be signed. The
 * last number can have a fractional part. The intervening parts must
 * be integers between 0 and 59. The resulting number has the dimensions
 * of the first number. Thus 23:30 can also be entered as 23.5.
 */
int input_sexagesimal(InputStream *stream, int tell, double *result);

/*
 * Read a date like 23-JAN-1997.
 */
int input_date(InputStream *stream, int tell, int *year, int *month, int *day);

/*
 * Read a time from an input stream. The following are valid time
 * specifications along with their interpretations:
 *
 *  23:34:04.5  ->   23:34:04.5
 *  23:34:4.5   ->   23:34:04.5
 *  23:34:10    ->   23:34:10
 *  23:34       ->   23:34:00
 *  23          ->   23:00:00
 */
int input_time(InputStream *stream, int tell, int *hour, int *min, double *sec);

/*
 * Read a date and optional time. The following are valid specifications
 * and their interpretations.
 *
 *  12-DEC-1998                 ->  12-DEC-1998 00:00:00.0
 *  12-DEC-1998:13              ->  12-DEC-1998 13:00:00.0
 *  12-DEC-1998:13:34           ->  12-DEC-1998 13:34:00.0
 *  12-DEC-1998:13:34:23        ->  12-DEC-1998 13:34:23.0
 *  12-DEC-1998:13:34:23.5      ->  12-DEC-1998 13:34:23.5
 *
 * The following are also valid if nospace!=0.
 *
 *  12-DEC-1998 13              ->  12-DEC-1998 13:00:00.0
 *  12-DEC-1998 13:34           ->  12-DEC-1998 13:34:00.0
 *  12-DEC-1998 13:34:23        ->  12-DEC-1998 13:34:23.0
 *  12-DEC-1998 13:34:23.5      ->  12-DEC-1998 13:34:23.5
 */
int input_date_and_time(InputStream *stream, int tell, int nospace, int *year,
			int *month, int *day, int *hour, int *min, double *sec);

/*
 * Read a time interval written like the following example:
 *
 *  23d:14h:16m:2.32s   (ie. 23 days, 14 hours, 16 minutes, 2.32 seconds)
 *
 * Any of the components can be omitted, but each trailing colon is taken
 * as evidence that another component follows. The result is returned in
 * seconds, via the *interval argument.
 */
int input_interval(InputStream *stream, int tell, double *interval);

/*-----------------------------------------------------------------------
 * Error management conventions and facilities.
 *
 * When the above input_whatever() functions detect an error they
 * optionally report the source of the error to stderr and then return
 * non-zero to indicate that an error occured. The input_error()
 * function is designed with this in mind.  It is a printf-style
 * function takes an argument that says whether to report the error
 * message or to discard it, and that returns 1 for use as a non-zero
 * error code. One statement can then be used both to conditionally
 * report the error and to return from the calling function with an
 * error code. For example:
 *
 * int input_ulong(InputStream *stream, int tell, unsigned long *ul)
 * {
 *   if(!isdigit(stream->nextc))
 *    return input_error(stream, tell, "Not an unsigned integer");
 *   ...
 *  }
 *
 * Where possible input_error() reports the location of the errant input.
 * For example, if the input stream is connected to a file then it reports
 * the file name and line number at which the error was encountered.
 *
 * Note that if an input_xxx() function itself calls an input_yyy()
 * function then it should set the tell argument of input_yyy() to 0
 * if it intends to report the error itself.
 */

/*
 * When this header is compiled with gcc, the following macro
 * is expanded at the end of the prototype of input_error(). The
 * result informs gcc that the format and trailing arguments should
 * be checked as though the function were printf().
 */
#ifdef __GNUC__
#define CHECK_FORMAT __attribute__ ((format (printf, 3, 4)))
#else
#define CHECK_FORMAT
#endif

/*
 * An fprintf-like function for optionally reporting error messages.
 * If 'tell' is non-zero the message is reported to stderr. Otherwise
 * the message is discarded. The input_error() function always returns
 * 1, for use by calling functions as an error-return code (see above).
 */
int input_error(InputStream *stream, int tell, const char *fmt, ...) CHECK_FORMAT;
#undef CHECK_FORMAT
void input_verror(InputStream *stream, const char *fmt, va_list args);

IS_LITERAL_FN(isHostName);

#endif
