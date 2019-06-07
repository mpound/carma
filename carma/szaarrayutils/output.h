#ifndef output_h
#define output_h

#include <stdarg.h>
#include <stdio.h>

/* Declare the tag for a generic output stream. */

typedef struct OutputStream OutputStream;

/* List iterator-specific method function declarations */

/*
 * Write a given number of characters to an output stream.
 *
 * Input:
 *  stream OutputStream *  The output stream to write to.
 *  text     const char *  A string to be written to the stream.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
#define OUTPUT_WRITE_FN(fn) int (fn)(OutputStream *stream, const char *text)

/*
 * Delete a stream context (as recorded in OutputStream::data).
 *
 * Input:
 *  data    void *  The context data to be deleted.
 * Output:
 *  return  void *  The deleted context data (always NULL).
 */
#define OUTPUT_DEL_FN(fn) void *(fn)(void *data)

/* Set the maximum length of any output token */

enum {OUTPUT_WORKLEN=1024};

/* Declare a generic output stream container */

struct OutputStream {
  OUTPUT_WRITE_FN(*write_fn); /* Function to write a string to the stream */
  OUTPUT_DEL_FN(*del_fn);     /* Iterator destructor */
  char work[OUTPUT_WORKLEN];  /* Work buffer for encoding lexical components */
  void *data;                 /* Type-specific data */
  bool interactive;
};

/*
 * Construct an initially closed output stream, ready to be connected to
 * an output sink with open_OutputStream().
 */
OutputStream *new_OutputStream(void);

/*
 * Connect an output stream to a specific output sink. This function
 * should only be called from type-specific stream open_*()
 * functions (see below).
 */
int open_OutputStream(OutputStream *stream, void *data, 
		     OUTPUT_WRITE_FN(*write_fn), OUTPUT_DEL_FN(*del_fn));

/*
 * Close an output stream. Future writes will return with an error
 * until the stream is successfully re-opened to another output sink.
 */
void close_OutputStream(OutputStream *stream);

/*
 * Declare constructors for provided output stream iterators.
 */
int open_FileOutputStream(OutputStream *stream, char *dir, char *name);
int open_StringOutputStream(OutputStream *stream, int truncate,
			    char *buffer, size_t size);
int open_StdioOutputStream(OutputStream *stream, int do_close, FILE *fp);
int open_LprintfOutputStream(OutputStream *stream, FILE *fp);
int open_StdoutStream(OutputStream *stream);
int open_StderrStream(OutputStream *stream);

/*
 * Declare a generic stream destructor.
 */
OutputStream *del_OutputStream(OutputStream *stream);

/*
 * Write a '\0' terminated string to an output stream.
 */
int write_OutputStream(OutputStream *stream, const char *s);

/*
 * Write an array of n characters to an output stream.
 */
int nwrite_OutputStream(OutputStream *stream, const char *s, size_t n);

/*
 * Reopen a string output stream to a new line to be composed.
 */
int clr_StringOutputStream(OutputStream *stream);

/*
 * Write a quoted string to the specified stream. This includes
 * prepending and postpending " characters and the conversion of
 * unprintable characters and embedded " characters to their equivalent
 * C escape sequences (eg. tab -> \t).
 */
int output_quoted_string(OutputStream *stream, char *string);

typedef enum {
  ET_BELL=1,      /* \a   - Bell */
  ET_BS=2,        /* \b   - Backspace */
  ET_FF=4,        /* \f   - Form feed */
  ET_NL=8,        /* \n   - New line */
  ET_CR=16,       /* \r   - Carriage return */
  ET_HT=32,       /* \t   - Horizontal Tab */
  ET_VT=64,       /* \v   - Vertical Tab */
  ET_ESC=128,     /* \\   - Backslash */
  ET_QUOTE=256,   /* \'   - Single quotes */
  ET_SPEECH=512,  /* \"   - Double quotes */
  ET_OTHER=1024,  /* \0nn - Other unprintable characters */
/*
 * All of the above.
 */
  ET_ALL = (ET_BELL | ET_BS | ET_FF | ET_NL | ET_CR | ET_HT | ET_VT | ET_ESC |
	    ET_QUOTE | ET_SPEECH | ET_OTHER),
/*
 * None of the above.
 */
  ET_NONE=0
} EscapeType;

/*
 * Output an unquoted string, optionally with selected unprintable
 * and white-space characters displayed as escape sequences. The
 * filter argument should be a bitwise union of EscapeType enumerators,
 * with each bit specifying a character code that should be displayed
 * as a C-style escape sequence. The flags[] argument should be an array
 * of printf-style flags, of which only the '-' flag is used.
 * If the output string doesn't take min_width characters then it will
 * be padded with spaces. If the '-' flag has been specified trailing
 * spaces will be added, otherwise leading spaces will be used. If the
 * string takes more than max_width characters before padding then it
 * will be truncated unless max_width is zero.
 */
int output_string(OutputStream *stream, unsigned filter, char *flags,
		  unsigned min_width, int max_width, int max_char,
		  char *string);

/*
 * The following functions are OutputStream equivalents to fprintf()
 * and vfprintf().
 */
int output_printf(OutputStream *stream, const char *fmt, ...);
int output_vprintf(OutputStream *stream, const char *fmt, va_list ap);

/*
 * Write a long int in a given base.
 */
typedef enum {
  OUT_BINARY = 2,
  OUT_OCTAL = 8,
  OUT_DECIMAL = 10,
  OUT_HEX = 16
} OutputBase;

int output_long(OutputStream *stream, OutputBase base, char *flags, int width,
		int precision, long lval);
int output_ulong(OutputStream *stream, OutputBase base, char *flags, int width,
		int precision, unsigned long ulval);
int output_double(OutputStream *stream, char *flags, int width, int precision,
		  char type, double dval);

int output_sexagesimal(OutputStream *stream, char *flags, int width,
		       int ninteger, int precision, double number);
int output_date(OutputStream *stream, char *flags, int width,
		int day, int month, int year);
int output_spaces(OutputStream *stream, int n);
int output_zeros(OutputStream *stream, int n);
int output_interval(OutputStream *stream, char *flags, int width,
		    int precision, double interval);

#endif
