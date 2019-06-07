#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <limits.h>
#include <float.h>

#include "carma/szaarrayutils/output.h"
#include "carma/szaarrayutils/lprintf.h"
#include "carma/szaarrayutils/print.h"
#include "carma/szaarrayutils/pathname.h"

/*
 * Declare a file output stream type.
 */
typedef struct {
  FILE *fp;         /* File pointer to the opened file */
  char *name;       /* The name with which the file was opened */
} FileOutput;

/*
 * Declare a string output stream type.
 */
typedef struct {
  char *string;    /* The string output buffer */
  int truncate;    /* Truncate the string silently on overflow? */
  size_t max_len;  /* One character less that the dimensioned size of string */
  size_t length;   /* The current length of the string (not including '\0') */
} StringOutput;

/*
 * Declare a stdio-stream output stream.
 */
typedef struct {
  int do_close;    /* True if del_StdioOutput should close fp */
  FILE *fp;        /* The FILE pointer of the stream */
} StdioOutput;

static OUTPUT_WRITE_FN(write_FileOutput);
static OUTPUT_DEL_FN(del_FileOutput);
static OUTPUT_WRITE_FN(write_StringOutput);
static OUTPUT_DEL_FN(del_StringOutput);
static OUTPUT_WRITE_FN(write_StdioOutput);
static OUTPUT_DEL_FN(del_StdioOutput);
static OUTPUT_WRITE_FN(write_StdoutOutput);
static OUTPUT_WRITE_FN(write_StderrOutput);

static PRT_OUT_FN(write_printf_output);

#define PI (3.14159265358979323846)
#define RADIANS_TO_ARCSEC (3600.0 * 180.0 / PI)
#define RADIANS_TO_DEGREES (180.0 / PI)

static int format_integer(OutputStream *stream, OutputBase base,
			  char *flags, int width, int precision,
			  int negative, unsigned long ulval);

/*.......................................................................
 * Generic output stream constructor. Note that the resturned stream will
 * act as though closed until open_OutputStream() is called to connect
 * it to an output sink.
 *
 * Output:
 *  return        OutputStream * The new stream context.
 */
OutputStream *new_OutputStream(void)
{
  OutputStream *stream;  /* The new stream context */
/*
 * Allocate the container.
 */
  stream = (OutputStream *) malloc(sizeof(OutputStream));
  if(!stream) {
    lprintf(stderr, "new_OutputStream: Insufficient memory.\n");
    return NULL;
  };
/*
 * Initialize the container.
 */
  stream->write_fn = 0;
  stream->del_fn = 0;
  stream->data = NULL;
  stream->interactive = false;

  return stream;
}

/*.......................................................................
 * Delete an output stream previously returned by new_OutputStream().
 *
 * Input:
 *  stream  OutputStream *  The stream to be deleted.
 * Output:
 *  return  OutputStream *  The deleted stream context (Always NULL).
 */
OutputStream *del_OutputStream(OutputStream *stream)
{
  if(stream) {
    if(stream->del_fn)
      stream->data = stream->del_fn(stream->data);
    free(stream);
  };
  return NULL;
}

/*.......................................................................
 * Connect an existing output stream to a new output sink. If the
 * output stream is currently connected to such a sink, then
 * close_OutputStream() will be called first.
 *
 * Input:
 *  stream      OutputStream *  The stream to be (re-)opened.
 *  data                void *  The stream-type instance context data.
 *  write_fn OUTPUT_WRITE_FN(*) The write method function for the stream.
 *  del_fn     OUTPUT_DEL_FN(*) The destructor method function to be
 *                              called upon to delete 'data'.
 * Output:
 *  return             int     0 - OK.
 *                             1 - Error. Note that the stream will be
 *                                 left closed and 'data' should be deleted
 *                                 deleted by the caller.
 */
int open_OutputStream(OutputStream *stream, void *data,
		     OUTPUT_WRITE_FN(*write_fn), OUTPUT_DEL_FN(*del_fn))
{
/*
 * Check arguments.
 */
  if(!stream) {
    lprintf(stderr, "open_OutputStream: NULL stream.\n");
    return 1;
  };
  if(!write_fn) {
    lprintf(stderr, "open_OutputStream: Missing method function(s).\n");
    return 1;
  };
/*
 * Close any existing stream connection.
 */
  close_OutputStream(stream);
/*
 * Re-initialize the stream.
 */
  stream->write_fn = write_fn;
  stream->del_fn = del_fn;
  stream->data = data;
  return 0;
}

/*.......................................................................
 * Close an output stream but don't delete it. This allows the stream to
 * be re-opened as another instance of the same or a different type of
 * stream without having to re-allocate the generic stream context.
 * Once a stream has been closed, calls to write_OutputStream() return
 * 1 until it is succesfully re-opened.
 *
 * Output:
 *  stream   OutputStream *   The stream to close.
 */
void close_OutputStream(OutputStream *stream)
{
  if(stream) {
    if(stream->del_fn)
      stream->data = stream->del_fn(stream->data);
    stream->write_fn = 0;
    stream->del_fn = 0;
  };
}

/*.......................................................................
 * Write a '\0' terminated string to an output stream.
 *
 * Input:
 *  stream  OutputStream *  The output stream to write to.
 *  s         const char *  A '\0' terminated string of characters to
 *                          be written to the stream.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
int write_OutputStream(OutputStream *stream, const char *s)
{
  if(!stream || !stream->write_fn) {
    lprintf(stderr, "write_OutputStream: The stream is closed.\n");
    return 1;
  };
  return stream->write_fn(stream, s);
}

/*.......................................................................
 * Write an array of nc characters to an output stream.
 *
 * Input:
 *  stream  OutputStream *  The output stream to write to.
 *  text      const char *  The array of nc characters to be written.
 *  nc            size_t    The number of characters to be written.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
int nwrite_OutputStream(OutputStream *stream, const char *text, size_t nc)
{
  char buffer[100];   /* A buffer in which to copy '\0' terminated string */
                      /*  segments */
  size_t ndone;       /* The number of bytes written so far */
  size_t nnew;        /* The latest number of bytes to be written */
/*
 * Check arguments.
 */
  if(!stream || !stream->write_fn) {
    lprintf(stderr, "nwrite_OutputStream: The stream is closed.\n");
    return 1;
  };
/*
 * Stream write functions require '\0' terminated strings. Dispatch
 * the string in segments of up to sizeof(buffer)-1 characters plus
 * a terminating '\0' in buffer[].
 */
  for(ndone=0; ndone<nc; ndone+=nnew) {
/*
 * How many of the remaining characters will fit into buffer[]?
 */
    nnew = (nc-ndone) < sizeof(buffer) ? (nc-ndone) : (sizeof(buffer)-1);
/*
 * Copy nnew characters into buffer[] and add a '\0' terminator.
 */
    memcpy(buffer, text + ndone, nnew);
    buffer[nnew] = '\0';
/*
 * Dispatch the new string segment to the stream write function.
 */
    if(stream->write_fn(stream, buffer))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Connect an output stream to an existing text file. The file name
 * is constructed from dir[] and name[] as described in pathname.h.
 *
 * Input:
 *  stream  OutputStream *  The stream to connect the file to.
 *  dir             char *  The directory in which the file resides, or "".
 *  name            char *  The name of a new text file to be opened
 *                          for write.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
int open_FileOutputStream(OutputStream *stream, char *dir, char *name)
{
  FileOutput *fdata; /* The stream context to be returned */
/*
 * Sanity check the arguments.
 */
  if(!stream) {
    lprintf(stderr, "open_FileOutputStream: NULL stream.\n");
    return 1;
  };    
  if(!dir || !name) {
    lprintf(stderr, "open_FileOutputStream: File name not specified.\n");
    return 1;
  };
/*
 * Allocate the file-stream context.
 */
  fdata = (FileOutput *) malloc(sizeof(FileOutput));
  if(!fdata) {
    lprintf(stderr, "open_FileOutputStream: Insufficient memory.\n");
    return 1;
  };
/*
 * Before attempting any operation that might fail, initialize the
 * container at least up to the point at which it can be passed to
 * del_FileOutput().
 */
  fdata->fp = NULL;
  fdata->name = NULL;
/*
 * Construct the file name.
 */
  fdata->name = new_pathname(dir, name);
  if(!fdata->name) {
    fdata = (FileOutput* )del_FileOutput(fdata);
    return 1;
  };
/*
 * Attempt to open the named file.
 */
  fdata->fp = fopen(fdata->name, "w");
  if(!fdata->fp) {
    lprintf(stderr, "open_FileOutputStream: Unable to open file: '%s'.\n",
	    fdata->name);
    fdata = (FileOutput* )del_FileOutput(fdata);
    return 1;
  };
/*
 * Register the file with the output stream.
 */
  if(open_OutputStream(stream, fdata, write_FileOutput, del_FileOutput)) {
    fdata = (FileOutput* )del_FileOutput(fdata);
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Write a given number of characters to a file output stream.
 *
 * Input:
 *  stream OutputStream *  The output stream to write to.
 *  text           char *  The string to be written.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static OUTPUT_WRITE_FN(write_FileOutput)
{
  FileOutput *fdata = (FileOutput *) stream->data;
  if(fputs(text, fdata->fp) == EOF) {
    lprintf(stderr, "write_FileOutput: Error writing to file \"%s\".\n",
	    fdata->name);
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Delete the context of a file stream.
 *
 * Input:
 *  data    void *  The file context to be deleted.
 * Output:
 *  return  void *  The deleted stream context (Always NULL).
 */
static OUTPUT_DEL_FN(del_FileOutput)
{
  FileOutput *fdata = (FileOutput *) data;
  if(fdata) {
    if(fdata->fp && fclose(fdata->fp)) {
      lprintf(stderr, "Error closing file: %s.\n",
	      fdata->name ? fdata->name : "(unknown)");
    };
    if(fdata->name)
      free(fdata->name);
    free(fdata);
  };
  return NULL;
}

/*.......................................................................
 * Create and connect an output stream to an existing output character
 * array buffer.
 *
 * Input:
 *  stream OutputStream *  The stream to connect the buffer to.
 *  truncate        int    Normally, overflowing the supplied buffer
 *                         is treated as an error. To instead have the
 *                         output silently truncated, set this argument
 *                         to non-zero.
 *  buffer         char *  The buffer to associate with the stream.
 *                         Once associated with a stream this buffer
 *                         should be treated as readonly for all other
 *                         uses until either close_OutputStream() or
 *                         del_OutputStream() is called. Also note that
 *                         'buffer' must exist for at least as long as
 *                         the stream.
 *  size         size_t    The dimensioned size of buffer[]. The string
 *                         composed in this buffer will always be
 *                         terminated with a '\0' character, so this
 *                         sets an upper limit on the longest string
 *                         of size-1 characters.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
int open_StringOutputStream(OutputStream *stream, int truncate,
			    char *buffer, size_t size)
{
  StringOutput *sdata;   /* The context to be registered with the stream */
/*
 * Check arguments.
 */
  if(!stream || !buffer) {
    lprintf(stderr, "open_StringOutputStream: NULL %s.\n",
	    !stream ? "stream":"buffer");
    return 1;
  };
  if(size < 1) {
    lprintf(stderr, "open_StringOutputStream: Zero length buffer.\n");
    return 1;
  };
/*
 * Allocate the file-stream context.
 */
  sdata = (StringOutput *) malloc(sizeof(StringOutput));
  if(!sdata) {
    lprintf(stderr, "open_StringOutputStream: Insufficient memory.\n");
    return 1;
  };
/*
 * Before attempting any operation that might fail, initialize the
 * container at least up to the point at which it can be passed to
 * del_StringOutput().
 */
  sdata->string = buffer;
  sdata->string[0] = '\0';
  sdata->truncate = truncate;
  sdata->length = 0;
  sdata->max_len = size - 1;
/*
 * Register the string to the specified stream.
 */
  if(open_OutputStream(stream, sdata, write_StringOutput, del_StringOutput)) {
    sdata = (StringOutput* )del_StringOutput(sdata);
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Write a given number of characters to a string output stream.
 *
 * Input:
 *  stream OutputStream *  The output stream to write to.
 *  text           char *  A string to be written to the stream.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static OUTPUT_WRITE_FN(write_StringOutput)
{
  StringOutput *sdata = (StringOutput *) stream->data;
  size_t nc = strlen(text);
/*
 * Would appending the new string segment overflow the buffer?
 */
  if(sdata->length + nc > sdata->max_len) {
    if(sdata->truncate) {
      nc = sdata->max_len - sdata->length;
    } else {
      lprintf(stderr, "write_StringOutput: Output string buffer too small.\n");
      return 1;
    };
  };
  memcpy(sdata->string + sdata->length, text, nc);
  sdata->length += nc;
  sdata->string[sdata->length] = '\0';
  return 0;
}

/*.......................................................................
 * Delete the context of a string outpt stream.
 *
 * Input:
 *  data    void *  The context to be deleted.
 * Output:
 *  return  void *  The deleted context (Always NULL).
 */
static OUTPUT_DEL_FN(del_StringOutput)
{
  StringOutput *sdata = (StringOutput *) data;
  if(sdata) {
    free(sdata);
  };
  return NULL;
}

/*.......................................................................
 * Reopen a string output stream so that its buffer can be reused.
 *
 * Input:
 *  stream    OutputStream *  A string output stream.
 * Output:
 *  return             int    0 - OK.
 *                            1 - Error - stream is not a string
 *                                output stream.
 */
int clr_StringOutputStream(OutputStream *stream)
{
  StringOutput *sdata;   /* The string-specific context of the stream */
/*
 * Make sure that the stream is a string stream.
 */
  if(!stream || stream->write_fn != write_StringOutput) {
    lprintf(stderr, "clr_StringOutputStream: Not a string output stream.\n");
    return 1;
  };
/*
 * Rewind the output buffer.
 */
  sdata = (StringOutput* )stream->data;
  sdata->string[0] = '\0';
  sdata->length = 0;
  return 0;
}

/*.......................................................................
 * Connect an output stream to a (FILE *) pointer opened for read.
 *
 * Input:
 *  stream  OutputStream *  The output stream to connect the file to.
 *  do_close         int    If a subsequent call to close_OutputStream()
 *                          or del_OutputStream() should close fp then
 *                          specify non-zero.
 *  fp              FILE *  The pointer to a text stream that has been
 *                          opened for write.
 * Output:
 *  return  OutputStream *  The new output stream, or NULL on error.
 */
int open_StdioOutputStream(OutputStream *stream, int do_close, FILE *fp)
{
  StdioOutput *sdata;   /* The context to be registered with the stream */
/*
 * Check arguments.
 */
  if(!stream || !fp) {
    lprintf(stderr, "open_StdioOutputStream: NULL %s.\n",
	    !stream ? "stream":"file-pointer");
    return 1;
  };
/*
 * Allocate the stdio-stream context.
 */
  sdata = (StdioOutput *) malloc(sizeof(StdioOutput));
  if(!sdata) {
    lprintf(stderr, "open_StdioOutputStream: Insufficient memory.\n");
    return 1;
  };
/*
 * Before attempting any operation that might fail, initialize the
 * container at least up to the point at which it can be passed to
 * del_StdioOutput().
 */
  sdata->do_close = do_close;
  sdata->fp = fp;
/*
 * Register the stdio-pointer to the specified stream.
 */
  if(open_OutputStream(stream, sdata, write_StdioOutput, del_StdioOutput)) {
    sdata = (StdioOutput* )del_StdioOutput(sdata);
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Write a given number of characters to a stdio-stream output stream.
 *
 * Input:
 *  stream OutputStream *  The output stream to write to.
 *  text           char *  An array of nc characters to be written to the
 *                         stream.
 *  nc              int    The number of characters to copy from text[].
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static OUTPUT_WRITE_FN(write_StdioOutput)
{
  StdioOutput *sdata = (StdioOutput *) stream->data;
  if(fputs(text, sdata->fp) == EOF) {
    lprintf(stderr, "write_StdioStream: Error writing to stdio-stream.\n");
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Delete the context of a stdio output stream.
 *
 * Input:
 *  data    void *  The context to be deleted.
 * Output:
 *  return  void *  The deleted context (Always NULL).
 */
static OUTPUT_DEL_FN(del_StdioOutput)
{
  StdioOutput *sdata = (StdioOutput *) data;
  if(sdata) {
    if(sdata->do_close && sdata->fp && fclose(sdata->fp))
      lprintf(stderr, "del_StdioOutput: Error closing stdio stream.\n");
    free(sdata);
  };
  return NULL;
}

/*.......................................................................
 * Connect an output stream to lprintf(stdout).
 *
 * Input:
 *  stream  OutputStream *  The output stream to connect to lprintf().
 * Output:
 *  return  OutputStream *  The new output stream, or NULL on error.
 */
int open_StdoutStream(OutputStream *stream)
{
/*
 * Check arguments.
 */
  if(!stream) {
    lprintf(stderr, "open_StdoutStream: Invalid inputs.\n");
    return 1;
  };
/*
 * Register the lprintf-pointer to the specified stream.
 */
  if(open_OutputStream(stream, NULL, write_StdoutOutput, 0))
    return 1;
  return 0;
}

/*.......................................................................
 * Write a given number of characters to lprintf(stdout).
 *
 * Input:
 *  stream OutputStream *  The output stream to write to.
 *  text           char *  An array of nc characters to be written to the
 *                         stream.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static OUTPUT_WRITE_FN(write_StdoutOutput)
{
  return lprintf(stdout, "%s", text) < 0;
}

/*.......................................................................
 * Connect an output stream to lprintf(stderr).
 *
 * Input:
 *  stream  OutputStream *  The output stream to connect to lprintf().
 * Output:
 *  return  OutputStream *  The new output stream, or NULL on error.
 */
int open_StderrStream(OutputStream *stream)
{
/*
 * Check arguments.
 */
  if(!stream) {
    lprintf(stderr, "open_StderrStream: Invalid inputs.\n");
    return 1;
  };
/*
 * Register the lprintf-pointer to the specified stream.
 */
  if(open_OutputStream(stream, NULL, write_StderrOutput, 0))
    return 1;
  return 0;
}

/*.......................................................................
 * Write a given number of characters to lprintf(stderr).
 *
 * Input:
 *  stream OutputStream *  The output stream to write to.
 *  text           char *  An array of nc characters to be written to the
 *                         stream.
 *  nc              int    The number of characters to copy from text[].
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static OUTPUT_WRITE_FN(write_StderrOutput)
{
  return lprintf(stderr, "%s", text) < 0;
}

/*.......................................................................
 * Write a string to an output stream. The string will be enclosed in
 * " characters and un-printable characters and embedded " characters
 * will be converted to equivalent C escape sequences.
 *
 * Input:
 *  stream  OutputStream *  The stream to write to.
 *  string          char *  The string to be written.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
int output_quoted_string(OutputStream *stream, char *string)
{
  char buffer[10];  /* Escape-sequence compilation buffer */
/*
 * Introduce the string with a quote.
 */
  if(write_OutputStream(stream, "\""))
    return 1;
/*
 * Write one character at a time, being careful to escape special
 * characters.
 */
  for( ; *string; string++) {
    switch(*string) {
    case '\a':
      strcpy(buffer, "\\a");
      break;
    case '\b':
      strcpy(buffer, "\\b");
      break;
    case '\f':
      strcpy(buffer, "\\f");
      break;
    case '\n':
      strcpy(buffer, "\\n");
      break;
    case '\r':
      strcpy(buffer, "\\r");
      break;
    case '\t':
      strcpy(buffer, "\\t");
      break;
    case '\v':
      strcpy(buffer, "\\v");
      break;
    case '\\':
      strcpy(buffer, "\\\\");
      break;
    case '\?':
      strcpy(buffer, "\\\?");
      break;
    case '\'':
      strcpy(buffer, "\\\'");
      break;
    case '\"':
      strcpy(buffer, "\\\"");
      break;
    default:
      if(isgraph((int) *string) || *string == ' ') {
	buffer[0] = *string;
	buffer[1] = '\0';
      } else { /* Display unknown characters in octal */
	sprintf(buffer, "\\0%o", (unsigned int) *string);
      };
      break;
    };
/*
 * Add the new character sequence to the output stream.
 */
    if(write_OutputStream(stream, buffer))
      return 1;
  };
/*
 * Terminate the string with a closing quote.
 */
  if(write_OutputStream(stream, "\""))
    return 1;
  return 0;
}

/*.......................................................................
 * Write an unquoted string to an output stream. Quotation marks and
 * un-printable characters will optionally be translated into
 * equivalent C escape sequences.
 *
 * Input:
 *  stream  OutputStream *  The stream to write to.
 *  filter      unsigned    The set of special characters to translate
 *                          into C-style escape sequences, expressed
 *                          as a bitwise union of EscapeType
 *                          enumerators.
 *  flags           char *  An array of printf-style flags, or which
 *                          only the '-' (left justification) flag
 *                          is used.
 *  min_width   unsigned    The minimum number of characters in the
 *                          output string. If fewer characters are
 *                          available in string[] then spaces will
 *                          be appended (or prepended if left
 *                          justification has been selected).
 *  max_width        int    If this is >= 0 then it specifies
 *                          the maximum number of output characters.
 *  max_chars        int    If this is >= 0 then it specifies the
 *                          maximum number of characters to take
 *                          from string[].
 *  string          char *  The string to be written.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
int output_string(OutputStream *stream, unsigned filter, char *flags,
		  unsigned min_width, int max_width, int max_chars,
		  char *string)
{
  char buff[10];     /* A character expansion buffer */
  char *wptr;        /* The next unused character in stream->work[] */
  int width=0;       /* The number of characters rendered */
  int chars=0;       /* The number of characters taken from string[] */
  int left_adjust=0; /* True if left justification has been requested */
/*
 * We will compose the string in the stream work buffer and use
 * wptr as the pointer to the next unused character therein.
 */
  wptr = stream->work;
/*
 * Limit the number of output characters to the size of the work
 * buffer, being careful to leave room for a terminal '\0' character.
 */
  if(max_width < 0 || max_width >= OUTPUT_WORKLEN)
    max_width = OUTPUT_WORKLEN;
/*
 * If no limit has been set on the number of characters to extract
 * from the input string, substitute the length of the output work
 * buffer.
 */
  if(max_chars < 0)
    max_chars = OUTPUT_WORKLEN;
/*
 * Start off with an empty string in the work buffer.
 */
  *wptr = '\0';
/*
 * Write one character at a time, being careful to escape special
 * characters.
 */
  for( ; *string && chars < max_chars; string++, chars++) {
    char *str;       /* The string to append for the next character */
    size_t length;   /* The length of str[] */
/*
 * Expand selected control characters into escape sequences.
 */
    switch(*string) {
    case '\a':
      str = (char* )(filter & ET_BELL ? "\\a" : "\a");
      break;
    case '\b':
      str = (char* )(filter & ET_BS ? "\\b" : "\b");
      break;
    case '\f':
      str = (char* )(filter & ET_FF ? "\\f" : "\f");
      break;
    case '\n':
      str = (char* )(filter & ET_NL ? "\\n" : "\n");
      break;
    case '\r':
      str = (char* )(filter & ET_CR ? "\\r" : "\r");
      break;
    case '\t':
      str = (char* )(filter & ET_HT ? "\\t" : "\t");
      break;
    case '\v':
      str = (char* )(filter & ET_VT ? "\\v" : "\v");
      break;
    case '\\':
      str = (char* )(filter & ET_ESC ? "\\\\" : "\\");
      break;
    case '\'':
      str = (char* )(filter & ET_QUOTE ? "\\\'" : "\'");
      break;
    case '\"':
      str = (char* )(filter & ET_SPEECH ? "\\\"" : "\"");
      break;
    default:
/*
 * If the character is printable, or if escaping of unprintable
 * has not been enabled, create a dummy string containing the new
 * character. Otherwise render an octal escape sequence to represent
 * the character.
 */
      if(isgraph((int) *string) || *string == ' ' || !(filter & ET_OTHER)) {
	buff[0] = *string;
	buff[1] = '\0';
      } else {
	sprintf(buff, "\\0%o", (unsigned int) *string);
      };
      str = buff;
      break;
    };
/*
 * Determine the length of the expanded version of the latest character.
 */
    length = strlen(str);
/*
 * If the addition of the new character would overflow the maximum field
 * width, quit writing characters.
 */
    if((int)(width + length) > (int)max_width)
      break;
/*
 * Add the characters to the work buffer.
 */
    strcpy(wptr, str);
    wptr += length;
    width += length;
  };
/*
 * Decompose the flags string.
 */
  for(wptr=flags; *wptr; wptr++) {
    switch(*wptr) {
    case '-':
      left_adjust = 1;
      break;
    };
  };
/*
 * Pad the output with leading spaces?
 */
  if(left_adjust && width < (int)min_width &&
     output_spaces(stream, min_width - width))
    return 1;
/*
 * Output the translated string.
 */
  if(write_OutputStream(stream, stream->work))
    return 1;
/*
 * Pad the output with trailing spaces?
 */
  if(!left_adjust && width < (int)min_width &&
     output_spaces(stream, min_width - width))
    return 1;
  return 0;
}

/*.......................................................................
 * Write a signed long int to an output stream, in one of a variety of
 * number bases.
 *
 * Input:
 *  stream  OutputStream *   The stream to write to.
 *  base      OutputBase     The base to write in from:
 *                             OUT_BINARY    -   Base 2.
 *                             OUT_OCTAL     -   Base 8.
 *                             OUT_DECIMAL   -   Base 10.
 *                             OUT_HEX       -   Base 16.
 *  flags           char *   Printf style flags. This is a string formed
 *                           from the concatenation of one or more of:
 *                             '-'  -  Left justification within the field.
 *                             '+'  -  Always print a sign character.
 *                             ' '  -  If the number is positive and the
 *                                     '+' flag is absent, prefix a space.
 *                             '#'  -  Prefix the number as follows:
 *                                       base==OUT_BINARY  => "0b"
 *                                       base==OUT_OCTAL   => "0"
 *                                       base==OUT_DECIMAL => ""
 *                                       base==OUT_HEX     => "0x"
 *                             '0'  -  Pad to 'width' with leading spaces
 *                                     unless the '-' flag is present.
 *  width            int     The minimum field width. If the converted
 *                           number takes fewer characters it will be
 *                           padded with leading spaces, or leading 0s if
 *                           the '0' flag is present, or trailing spaces
 *                           if the '-' flag is present.
 *  precision        int     The mimimum number of numeric digits. The
 *                           number will be padded with leading zeros
 *                           if necessary to reach this width.
 *  lval            long     The value to be written.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Error.
 */
int output_long(OutputStream *stream, OutputBase base, char *flags,
		int width, int precision, long lval)
{
  unsigned long ulval;   /* The positive version of lval */
  int negative;          /* True if lval is negative */
/*
 * Check arguments.
 */
  if(!stream || !flags) {
    lprintf(stderr, "output_long: NULL argument(s).\n");
    return 1;
  };
  switch(base) {
  case OUT_BINARY:
  case OUT_OCTAL:
  case OUT_DECIMAL:
  case OUT_HEX:
    break;
  default:
    lprintf(stderr, "output_long: Unknown radix.\n");
    return 1;
  };
/*
 * Convert lval to sign magnitude.
 */
  negative = lval < 0;
  ulval = negative ? -lval : lval;
/*
 * Delegate work to a function that is shared with output_ulong().
*/
  return format_integer(stream, base, flags, width, precision, negative, ulval);
}

/*.......................................................................
 * Write an unsigned long int to an output stream, in one of a variety of
 * number bases.
 *
 * Input:
 *  stream  OutputStream *   The stream to write to.
 *  base      OutputBase     The base to write in from:
 *                             OUT_BINARY    -   Base 2.
 *                             OUT_OCTAL     -   Base 8.
 *                             OUT_DECIMAL   -   Base 10.
 *                             OUT_HEX       -   Base 16.
 *  flags           char *   Printf style flags. This is a string formed
 *                           from the concatenation of one or more of:
 *                             '-'  -  Left justification within the field.
 *                             '#'  -  Prefix the number as follows:
 *                                       base==OUT_BINARY  => "0b"
 *                                       base==OUT_OCTAL   => "0"
 *                                       base==OUT_DECIMAL => ""
 *                                       base==OUT_HEX     => "0x"
 *                             '0'  -  Pad to 'width' with leading spaces
 *                                     unless the '-' flag is present.
 *  width            int     The minimum field width. If the converted
 *                           number takes fewer characters it will be
 *                           padded with leading spaces, or leading 0s if
 *                           the '0' flag is present, or trailing spaces
 *                           if the '-' flag is present.
 *  precision        int     The mimimum number of numeric digits. The
 *                           number will be padded with leading zeros
 *                           if necessary to reach this width.
 *  lval            long     The value to be written.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Error.
 */
int output_ulong(OutputStream *stream, OutputBase base, char *flags,
		int width, int precision, unsigned long ulval)
{
/*
 * Check arguments.
 */
  if(!stream || !flags) {
    lprintf(stderr, "output_ulong: NULL argument(s).\n");
    return 1;
  };
  switch(base) {
  case OUT_BINARY:
  case OUT_OCTAL:
  case OUT_DECIMAL:
  case OUT_HEX:
    break;
  default:
    lprintf(stderr, "output_ulong: Unknown radix.\n");
    return 1;
  };
/*
 * Delegate work to a function that is shared with output_long().
 */
  return format_integer(stream, base, flags, width, precision, 0, ulval);
}

/*.......................................................................
 * Internal function used to write an integer to an output stream.
 *
 * Input:
 *  stream  OutputStream *   The stream to write to.
 *  base      OutputBase     The base to write in from:
 *                             OUT_BINARY    -   Base 2.
 *                             OUT_OCTAL     -   Base 8.
 *                             OUT_DECIMAL   -   Base 10.
 *                             OUT_HEX       -   Base 16.
 *  flags           char *   Printf style flags. This is a string formed
 *                           from the concatenation of one or more of:
 *                             '-'  -  Left justification within the field.
 *                             '+'  -  Always print a sign character.
 *                             ' '  -  If the number is positive and the
 *                                     '+' flag is absent, prefix a space.
 *                             '#'  -  Prefix the number as follows:
 *                                       base==OUT_BINARY  => "0b"
 *                                       base==OUT_OCTAL   => "0"
 *                                       base==OUT_DECIMAL => ""
 *                                       base==OUT_HEX     => "0x"
 *                             '0'  -  Pad to 'width' with leading spaces
 *                                     unless the '-' flag is present.
 *  width            int     The minimum field width. If the converted
 *                           number takes fewer characters it will be
 *                           padded with leading spaces, or leading 0s if
 *                           the '0' flag is present, or trailing spaces
 *                           if the '-' flag is present.
 *  precision        int     The mimimum number of numeric digits. The
 *                           number will be padded with leading zeros
 *                           if necessary to reach this width.
 *  negative         int     If true, ulval contains the positive mantissa
 *                           of a negative number.
 *  ulval           long     The positive version of the value to be
 *                           written.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Error.
 */
static int format_integer(OutputStream *stream, OutputBase base,
			  char *flags, int width, int precision,
			  int negative, unsigned long ulval)
{
  char prefix[4];         /* A composition buffer for sign and base prefixes */
  char *cptr;             /* A pointer into prefix[] or stream->work[] */
  int ndigits;            /* The number of numeric digits before padding */
  int nprefix;            /* The number of prefix characters */
  int nzeros;             /* The number of zero padding characters needed */
  int left_adjust = 0;    /* Left adjustment requested */
  int show_positive = 0;  /* True to prefix positive numbers with + */
  int show_space = 0;     /* True to show space in place of + sign */
  int show_prefix = 0;    /* True to prepend base-enumeration prefix */
  int show_zeros = 0;     /* True to pad with zeros to field width */
/*
 * Impose limits on the two widths.
 */
  if(width < 1)
    width = 1;
  if(precision < 1)
    precision = 1;
/*
 * Decompose the flags string.
 */
  for(cptr=flags; *cptr; cptr++) {
    switch(*cptr) {
    case '-':
      left_adjust = 1;
      break;
    case '+':
      show_positive = 1;
      break;
    case ' ':
      show_space = 1;
      break;
    case '#':
      show_prefix = 1;
      break;
    case '0':
      show_zeros = 1;
      break;
    };
  };
/*
 * Compose the prefix.
 */
  cptr = &prefix[0];
/*
 * Insert the sign character if needed.
 */
  if(negative)
    *cptr++ = '-';
  else if(show_positive)
    *cptr++ = '+';
  else if(show_space)
    *cptr++ = ' ';
/*
 * Show a base prefix if requested.
 */
  if(show_prefix) {
    switch(base) {
    case OUT_BINARY:
      *cptr++ = '0';
      *cptr++ = 'b';
      break;
    case OUT_OCTAL:
      *cptr++ = '0';
      break;
    case OUT_DECIMAL:
      break;
    case OUT_HEX:
      *cptr++ = '0';
      *cptr++ = 'x';
      break;
    }
  };
/*
 * Terminate the prefix string.
 */
  *cptr = '\0';
/*
 * Determine the length of the prefix.
 */
  nprefix = cptr - &prefix[0];
/*
 * Print the unpadded number.
 */
  switch(base) {
  case OUT_BINARY:
    {
      int shift;       /* The bit shift required to test a given bit */
      int started = 0; /* True after first non-zero bit */
/*
 * Output bits from most significant to least significant.
 */
      cptr = stream->work;
      for(shift=sizeof(unsigned long) * CHAR_BIT - 1; shift >= 0; shift--) {
	int isset = ulval & 1U<<shift;
	if(isset || started) {
	  started = 1;
	  *cptr++ = isset ? '1' : '0';
	};
      };
/*
 * Make sure that at least one digit is displayed.
 */
      if(!started)
	*cptr++ = 0;
    };
/*
 * Terminate the number string.
 */
    *cptr = '\0';
    break;
  case OUT_OCTAL:
    sprintf(stream->work, "%lo", ulval);
    break;
  case OUT_DECIMAL:
    sprintf(stream->work, "%ld", ulval);
    break;
  case OUT_HEX:
    sprintf(stream->work, "%lx", ulval);
    break;
  };
/*
 * Get the length of the converted number.
 */
  ndigits = strlen(stream->work);
/*
 * Output any space padding that is needed before the prefix.
 */
  if(!show_zeros && !left_adjust) {
    int nspaces = width - nprefix - (ndigits > precision ? ndigits : precision);
    if(nspaces > 0 && output_spaces(stream, nspaces))
      return 1;
  };
/*
 * Output the prefix.
 */
  if(write_OutputStream(stream, prefix))
    return 1;
/*
 * Pad the number up to its precision (or width if(show_zeros)) with zeros.
 */
  if(show_zeros && width - nprefix > precision)
    nzeros = width - nprefix - ndigits;
  else
    nzeros = precision - ndigits;
  if(nzeros <= 0)
    nzeros = 0;
  else if(output_zeros(stream, nzeros))
    return 1;
/*
 * Output the converted digits.
 */
  if(write_OutputStream(stream, stream->work))
    return 1;
/*
 * If left adjustment was requested, pad up to the field width with spaces.
 */
  if(left_adjust) {
    int nspaces = width - nprefix - nzeros - ndigits;
    if(nspaces > 0 && output_spaces(stream, nspaces))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Write a double precision number to an output stream.
 *
 * Input:
 *  stream  OutputStream *   The stream to write to.
 *  flags           char *   Printf style flags. This is a string formed
 *                           from the concatenation of one or more of:
 *                             '-'  -  Left justification within the field.
 *                             '+'  -  Always print a sign character.
 *                             ' '  -  If the number is positive and the
 *                                     '+' flag is absent, prefix a space.
 *                             '#'  -  Always display a decimal point
 *                                     even if no digit follows it, and
 *                                     don't strip off trailing zeros.
 *                             '0'  -  Pad to 'width' with leading spaces
 *                                     unless the '-' flag is present.
 *  width            int     The minimum field width. If the converted
 *                           number takes fewer characters it will be
 *                           padded with leading spaces, or leading 0s if
 *                           the '0' flag is present, or trailing spaces
 *                           if the '-' flag is present.
 *  precision        int     The precision of the displayed number,
 *                           interpretted as the mimimum number of
 *                           significant figures for fmt=='g', or the
 *                           number of decimal places for fmt=='f' and
 *                           fmt=='e'. Trailing zeros up to the specified
 *                           precision will be stripped unless the '#'
 *                           flag is present.
 *  type             int     One of the following format specifiers:
 *                             'f' or 'F' - Fixed point.
 *                             'g' or 'G' - Fixed number of significant figures.
 *                             'e' or 'E' - Exponential notation fixed point.
 *  dval          double     The value to be written.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Error.
 */
int output_double(OutputStream *stream, char *flags, int width, int precision,
		  char type, double dval)
{
  char fmt[12];  /* The printf() format string. This must be long enough to */
                 /*  hold '%' followed by one of each of '-','+',' ','#','0' */
                 /*  followed by "*.*", followed by one of 'f','e' or 'g' */
  char *flagptr;     /* A pointer into flags[] */
  char *fmtptr;      /* A pointer into fmt[] */
  int seen_plus = 0; /* True after a '+' has been encountered in flags[] */
  int seen_left = 0; /* True after a '-' has been encountered in flags[] */
  int seen_space = 0;/* True after a ' ' has been encountered in flags[] */
  int seen_hash = 0; /* True after a '#' has been encountered in flags[] */
  int seen_zero = 0; /* True after a '0' has been encountered in flags[] */
/*
 * Check arguments.
 */
  if(!stream || !flags) {
    lprintf(stderr, "output_double: NULL argument(s).\n");
    return 1;
  };
/*
 * Enforce positivity in the character counts.
 */
  if(width < 0)
    width = 0;
  if(precision < 0)
    precision = 0;
/*
 * Start composing the format string.
 */
  fmtptr = fmt;
  *fmtptr++ = '%';
/*
 * Copy valid printf flags into printf_flags[], while rejecting
 * invalid flags and duplicate flags.
 */
  for(flagptr=flags; *flagptr; flagptr++) {
    switch(*flagptr) {
    case '-':
      if(!seen_left) {
	seen_left = 1;
	*fmtptr++ = '-';
      };
      break;
    case '+':
      if(!seen_plus) {
	seen_plus = 1;
	*fmtptr++ = '+';
      };
      break;
    case ' ':
      if(!seen_space) {
	seen_space = 1;
	*fmtptr++ = ' ';
      };
      break;
    case '#':
      if(!seen_hash) {
	seen_hash = 1;
	*fmtptr++ = '#';
      };
      break;
    case '0':
      if(!seen_zero) {
	seen_zero = 1;
	*fmtptr++ = '0';
      };
      break;
    };
  };
/*
 * Accomodate the width and precision.
 */
  *fmtptr++ = '*';
  *fmtptr++ = '.';
  *fmtptr++ = '*';
/*
 * Copy the format specifier character.
 */
  switch(type) {
  case 'f': case 'F':
  case 'e': case 'E':
  case 'g': case 'G':
    *fmtptr++ = type;
    break;
  default:
    lprintf(stderr, "output_double: Innappropriate format specifier '%c'.\n",
	    type);
    return 1;
  };
/*
 * Terminate the format string.
 */
  *fmtptr = '\0';
/*
 * Write the double into to work buffer.
 */
  sprintf(stream->work, fmt, width, precision, dval);
/*
 * Display the formatted number on the output stream.
 */
  return write_OutputStream(stream, stream->work);
}

/*.......................................................................
 * Write a sexagesimal number to an output stream as +/-ddd:mm:ss.sss
 * where ddd is the integral part of the input number, mm is the largest
 * multiple of 1/60 that can be subtracted from the fractional part, and
 * ss.sss is what remains in units of 1/3600. Thus to display an angle
 * in degrees, arcminutes, and arcseconds specify the angle in degrees.
 * To display a time in hours, minutes and seconds, specify the time in 
 * hours.
 *
 * Input:
 *  stream    OutputStream *  The stream to write to.
 *  flags             char *  This is should be a string consisting of a
 *                            concatenation of zero or more of the
 *                            following printf() flags:
 *                             '-'  -  Left justify the number within
 *                                     the specified field width.
 *                             '+'  -  Always prepend a sign character to
 *                                     the ddd component, even if positive.
 *                             ' '  -  If the number is positive and the
 *                                     '+' flag is absent, prefix a space.
 *                             '0'  -  Pad up to the specified field width
 *                                     with leading zeroes unless the '-'
 *                                     flag is present.
 *                             '#'  -  Trim trailing zeroes from the
 *                                     fractional part of the seconds field.
 *                                     If this removes all the digits of
 *                                     the fractional part, also omit the
 *                                     decimal point.
 *  width              int    The minimum field width. If the converted
 *                            number takes fewer characters it will be
 *                            padded with leading spaces by default, by
 *                            leading 0s if the '0' flag is present, or by
 *                            trailing spaces if the '-' flag is present.
 *  ninteger           int    The mimimum number of digits to display in
 *                            the initial ddd component. Leading zeros
 *                            will be added if necessary.
 *  precision          int    The number of figures after the decimal point
 *                            in the final 1/3600 (eg. seconds) field.
 *  number          double    The number to be represented.
 * Output:
 *  return             int    0 - OK.
 *                            1 - Error.
 */
int output_sexagesimal(OutputStream *stream, char *flags, int width,
		       int ninteger, int precision, double number)
{
  char secs[DBL_DIG+6];    /* String with room for a maximum precision */
                           /*  floating point number + a bit */
  double deg_int,deg_frc;  /* Integral and fractional degrees */
  double min_int,min_frc;  /* Integral and fractional arcminutes */
  char *prefix;            /* The sign prefix */
  char *cptr;              /* A pointer intp flags[] */
  int negative = 0;        /* True if the number is negative */
  int nprefix;             /* The number of prefix characters */
  int ndigits;             /* The number of digits in the first component */
  int nzeros;              /* The number of zero padding characters needed */
  int left_adjust = 0;     /* Left adjustment requested */
  int show_positive = 0;   /* True to prefix positive numbers with + */
  int show_space = 0;      /* True to show space in place of + sign */
  int show_zeros = 0;      /* True to pad with zeros to field width */
  int trim_fraction = 0;   /* True to trim trailing spaces from the */
                           /*  fractional part of the seconds field. */
  int i;
/*
 * Impose limits on the length constraints.
 */
  if(width < 1)
    width = 1;
  if(precision < 0)
    precision = 0;
  if(ninteger < 1)
    ninteger = 1;
  if(precision > DBL_DIG+1)
    precision = DBL_DIG+1;
/*
 * Check arguments.
 */
  if(!stream || !flags) {
    lprintf(stderr, "output_sexagesimal: NULL argument(s).\n");
    return 1;
  };
/*
 * Decompose the flags string.
 */
  for(cptr=flags; *cptr; cptr++) {
    switch(*cptr) {
    case '-':
      left_adjust = 1;
      break;
    case '+':
      show_positive = 1;
      break;
    case ' ':
      show_space = 1;
      break;
    case '0':
      show_zeros = 1;
      break;
    case '#':
      trim_fraction = 1;
      break;
    };
  };
/*
 * NB. To make it easy to refer to the three components, we will
 * arbitrarily denote them as degrees, minutes and seconds.
 *
 * Convert the number to sign, magnitude format.
 */
  if(number < 0) {
    number = -number;
    negative = 1;
  };
/*
 * On architectures that can represent -0, the above test fails to
 * detect that -0 is negative, so force 0 always to be +0.
 */
  if(number == 0.0)
    number = 0.0;
/*
 * Split decimal degrees into integral and fractional parts.
 */
  deg_frc = modf(number, &deg_int);
/*
 * Split the fractional degrees into fractional and integral minutes.
 */
  min_frc = modf(deg_frc * 60.0, &min_int);
/*
 * Write the seconds part in advance so that we can see whether in its
 * rounded form it is displayed as 60.
 */
  do {
    if(precision > 0)
      sprintf(secs, "%#0*.*f", precision+3, precision, min_frc * 60.0);
    else
      sprintf(secs, "%02.0f", min_frc * 60.0);
/*
 * If the rounded number is 60 then carry to the next highest minute.
 */
    if(secs[0] == '6' && secs[1] == '0') {
      min_frc = 0.0;
      if(++min_int >= 60.0) {
	min_int = 0.0;
	deg_int++;
      };
      secs[0] = '\0';
    };
  } while(secs[0]=='\0');
/*
 * Remove trailing zeroes from the fractional part?
 */
  if(trim_fraction && precision > 0) {
    for(i=strlen(secs); i > 0 && secs[i-1] == '0'; i--)
      ;
    if(secs[i-1] == '.')
      i--;
    secs[i] = '\0';
  };
/*
 * Compose the unpadded magnitude of the number.
 */
  sprintf(stream->work, "%d%n:%.2d:%s", (int)deg_int, &ndigits,
	  (int)min_int, secs);
/*
 * Remove all but the length of the first component from the
 * desired width.
 */
  width -= strlen(stream->work) - ndigits;
  if(width < 0)
    width = 0;
/*
 * Compose the prefix.
 */
  if(negative)
    prefix = "-";
  else if(show_positive)
    prefix = "+";
  else if(show_space)
    prefix = " ";
  else
    prefix = "";
/*
 * Determine the length of the prefix.
 */
  nprefix = strlen(prefix);
/*
 * Output any space padding that is needed before the prefix.
 */
  if(!show_zeros && !left_adjust) {
    int nspaces = width - nprefix - (ndigits > ninteger ? ndigits : ninteger);
    if(nspaces > 0 && output_spaces(stream, nspaces))
      return 1;
  };
/*
 * Output the prefix.
 */
  if(write_OutputStream(stream, prefix))
    return 1;
/*
 * Pad the first component of the magnitude up to its requested minimum
 * width.
 */
  if(show_zeros && width - nprefix > ninteger)
    nzeros = width - nprefix - ndigits;
  else
    nzeros = ninteger - ndigits;
  if(nzeros <= 0)
    nzeros = 0;
  else if(output_zeros(stream, nzeros))
    return 1;
/*
 * Output the magnitude components.
 */
  if(write_OutputStream(stream, stream->work))
    return 1;
/*
 * If left adjustment was requested, pad up to the field width with spaces.
 */
  if(left_adjust) {
    int nspaces = width - nprefix - nzeros - ndigits;
    if(nspaces > 0 && output_spaces(stream, nspaces))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Write a date to an output stream in the form dd/mm/yyyy.
 *
 * Input:
 *  stream  OutputStream *  The stream to write to.
 *  flags           char *  Printf style flags. This is a string formed
 *                          from the concatenation of one or more of:
 *                            '-'  -  Left justification within the field.
 *  width            int    The minimum field width. If the converted
 *                          number takes fewer characters it will be
 *                          padded with leading spaces, or trailing spaces
 *                          if the '-' flag is present.
 *  day              int    The day of the month (1-31). This will evoke
 *                          an error if out of range for the given month.
 *  month            int    The month of the year (1-12).
 *  year             int    The year, including its century (>=0).
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
int output_date(OutputStream *stream, char *flags, int width,
		int day, int month, int year)
{
  int left_adjust = 0;   /* Left adjustment requested? */
  int isleap;            /* True if the specified year is a leap year */
  char *cptr;            /* A pointer intp flags[] */
/*
 * Record the number of days per month, first in normal years, then in
 * leap years.
 */
  static char daytab[2][12] = {
    {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
    {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
  };
/*
 * List the conventional 3-letter abbreviations for the names of months.
 */
  static char *month_names[12] = {
    "JAN",  "FEB",  "MAR",  "APR", 
    "MAY",  "JUN",  "JUL",  "AUG",
    "SEP",  "OCT",  "NOV",  "DEC"
  };
/*
 * Check the arguments.
 */
  if(!stream) {
    lprintf(stderr, "output_date: NULL argument.\n");
    return 1;
  };
  if(year < 0) {
    lprintf(stderr, "output_date: Negative years are not supported.\n");
    return 1;
  };
  if(month < 1 || month > 12) {
    lprintf(stderr, "output_date: Month out of range 1-12.\n");
    return 1;
  };
/*
 * Is the specified year a leap year?
 */
  isleap = (year%4 == 0 && year%100 != 0) || year%400 == 0;
/*
 * Check that the date makes sense.
 */
  if(day < 1 || day > daytab[isleap][month-1]) {
    lprintf(stderr, "Day of month out of legal range 1-%d.\n",
	    daytab[isleap][month-1]);
    return 1;
  };
/*
 * Render the date to the work string of the stream.
 */
  sprintf(stream->work, "%02d-%s-%04d", day, month_names[month-1], year);
/*
 * Determine the number of spaces to pad the output with to reach the
 * requested field width.
 */
  width -= strlen(stream->work);
/*
 * Decompose the flags string.
 */
  for(cptr=flags; *cptr; cptr++) {
    switch(*cptr) {
    case '-':
      left_adjust = 1;
      break;
    };
  };
/*
 * If right-adjustment was requested pad up to the field width with
 * spaces.
 */
  if(!left_adjust && width > 0) {
    if(width > 0 && output_spaces(stream, width))
      return 1;
  };
/*
 * Output the date string.
 */
  if(write_OutputStream(stream, stream->work))
    return 1;
/*
 * If left adjustment was requested, pad up to the field width with spaces.
 */
  if(left_adjust && width > 0) {
    if(width > 0 && output_spaces(stream, width))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Write one or more spaces to an output stream.
 *
 * Input:
 *  stream  OutputStream *  The stream to write to.
 *  n                int    The number of spaces to be written.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
int output_spaces(OutputStream *stream, int n)
{
  static char spaces[] = "                                           ";
  int i;
/*
 * Write up to sizeof(spaces[]-1) spaces at a time.
 */
  for(i=0; i<n; i += sizeof(spaces)-1) {
    char *start = (n-i >= (int)sizeof(spaces)-1) ? spaces :
      (spaces + sizeof(spaces)-(n-i)-1);
    if(write_OutputStream(stream, start))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Write one or more zeros to an output stream.
 *
 * Input:
 *  stream  OutputStream *  The stream to write to.
 *  n                int    The number of zeros to be written.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
int output_zeros(OutputStream *stream, int n)
{
  static char zeros[] = "0000000000000000000000000000000000000000000";
  int i;
/*
 * Write up to sizeof(zeros[]-1) zeros at a time.
 */
  for(i=0; i<n; i += sizeof(zeros)-1) {
    char *start = (n-i >= (int)sizeof(zeros)-1) ? zeros :
      (zeros + sizeof(zeros)-(n-i)-1);
    if(write_OutputStream(stream, start))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Create an OutputStream equivalent of fprintf().
 *
 * Input:
 *  stream  OutputStream *  The stream to write to.
 *  fmt             char *  The printf format.
 *  ...                     The arguments needed by the printf format.
 * Output:
 *  return           int    The number of characters output, or -1
 *                          on error.
 */
int output_printf(OutputStream *stream, const char *fmt, ...)
{
  va_list ap;    /* The variable argument list */
  int nsent;     /* The number of characters written */
/*
 * Check the explicit arguments.
 */
  if(!stream || !fmt) {
    lprintf(stderr, "output_printf: NULL argument(s).\n");
    return -1;
  };
/*
 * Initialize the variable argument list and have it processed.
 */
  va_start(ap, fmt);
  nsent = print_format(fmt, ap, write_printf_output, (PrintContext) stream);
  va_end(ap);
  return nsent;
}

/*.......................................................................
 * Create an OutputStream equivalent of vfprintf().
 *
 * Input:
 *  stream  OutputStream *  The stream to write to.
 *  fmt             char *  The printf format.
 *  ap           va_list    The argument-list needed by the printf format.
 *                          The caller must apply va_start() to ap before
 *                          calling this function, and must apply va_end()
 *                          after this function returns.
 * Output:
 *  return           int    The number of characters output, or -1
 *                          on error.
 */
int output_vprintf(OutputStream *stream, const char *fmt, va_list ap)
{
/*
 * Check the explicit arguments.
 */
  if(!stream || !fmt) {
    lprintf(stderr, "output_vprintf: NULL argument(s).\n");
    return -1;
  };
  return print_format(fmt, ap, write_printf_output, (PrintContext) stream);
}

/*.......................................................................
 * This is the output function passed to print_format() by
 * output_printf() and output_vprintf(). It sends each converted string
 * segment to the specified output stream.
 */
static PRT_OUT_FN(write_printf_output)
{
  OutputStream *stream = (OutputStream *) context;
  if(nwrite_OutputStream(stream, buffer, nc))
    return 1;
  return 0;
}

/*.......................................................................
 * Output a time interval in the same format as is read by
 * input_interval(). The format looks like the following example:
 *
 *  -23d:12h:32m:45.378s
 *
 * Leading and trailing components are omitted if zero.
 *
 * Input:
 *  stream    OutputStream *  The stream to write to.
 *  flags             char *  This is should be a string consisting of a
 *                            concatenation of zero or more of the
 *                            following printf()-style flags:
 *                             '-'  -  Left justify the number within
 *                                     the specified field width.
 *                             '+'  -  Always prepend a sign character to
 *                                     the first component, even if positive.
 *                             ' '  -  If the number is positive and the
 *                                     '+' flag is absent, prefix a space.
 *                             '0'  -  Pad up to the specified field width
 *                                     with leading zeroes unless the '-'
 *                                     flag is present.
 *                             '#'  -  Trim trailing zeroes from the
 *                                     fractional part of the final field.
 *                                     If this removes all the digits of
 *                                     the fractional part, also omit the
 *                                     decimal point.
 *                             'd'  -  Show the days field, even if zero.
 *                             'h'  -  Show the hours field, even if zero.
 *                             'm'  -  Show the minutes field, even if zero.
 *                             's'  -  Show the seconds field, even if zero.
 *  width              int    The minimum field width. If the converted
 *                            number takes fewer characters it will be
 *                            padded with leading spaces by default, by
 *                            leading 0s if the '0' flag is present, or by
 *                            trailing spaces if the '-' flag is present.
 *  precision          int    The number of figures after the decimal point
 *                            in the final field.
 *  interval        double    The number of seconds in the interval.
 * Output:
 *  return             int    0 - OK.
 *                            1 - Error.
 */
int output_interval(OutputStream *stream, char *flags, int width,
		    int precision, double interval)
{
  char sec_str[DBL_DIG+6]; /* A work string with room for a maximum */
                           /*  precision floating point number + "a bit" */
  int sign;                /* The unit sign of the interval */
  char *prefix;            /* The sign prefix */
  char *cptr;              /* A pointer intp flags[] */
  double remainder;        /* The remainder of removing successive components */
  double secs;             /* The floating point seconds component */
  int days;                /* The integer days component */
  int hours;               /* The integer hours component */
  int mins;                /* The integer minutes component */
  int nprefix;             /* The number of sign-prefix characters */
  int left_adjust = 0;     /* Left adjustment requested */
  int show_positive = 0;   /* True to prefix positive numbers with + */
  int show_space = 0;      /* True to show space in place of + sign */
  int show_zeros = 0;      /* True to pad with zeros to field width */
  int trim_fraction = 0;   /* True to trim trailing spaces from the */
                           /*  fractional part of the seconds field. */
  int nc = 0;              /* The number of characters written so far */
  int first = 1;           /* True until the first component has been written */
  int npad;                /* The number of padding characters needed to */
                           /*  make up the requested minimum field width. */
  int i;
/*
 * Create bit-mask values for the individual components fields.
 */
  enum {
    SHOW_DAYS = 1,        /* Set this bit to force displaying the days field */
    SHOW_HOURS = 2,       /* Set this bit to force displaying the hours field */
    SHOW_MINS = 4,        /* Set this bit to force displaying the mins field */
    SHOW_SECS = 8         /* Set this bit to force displaying the secs field */
  };
  unsigned fields=0;      /* The bit-mask of fields to be shown. */
/*
 * Check arguments.
 */
  if(!stream || !flags) {
    lprintf(stderr, "output_interval: NULL argument(s).\n");
    return 1;
  };
/*
 * Impose limits on the length constraints.
 */
  if(width < 1)
    width = 1;
  if(precision < 0)
    precision = 0;
  if(precision > DBL_DIG+1)
    precision = DBL_DIG+1;
/*
 * Decompose the flags string.
 */
  for(cptr=flags; *cptr; cptr++) {
    switch(*cptr) {
    case '-':
      left_adjust = 1;
      break;
    case '+':
      show_positive = 1;
      break;
    case ' ':
      show_space = 1;
      break;
    case '0':
      show_zeros = 1;
      break;
    case '#':
      trim_fraction = 1;
      break;
    case 'd':
      fields |= SHOW_DAYS;
      break;
    case 'h':
      fields |= SHOW_HOURS;
      break;
    case 'm':
      fields |= SHOW_MINS;
      break;
    case 's':
      fields |= SHOW_SECS;
      break;
    };
  };
/*
 * Get the sign and absolute value the interval.
 */
  sign = interval < 0.0 ? -1 : 1;
  remainder = fabs(interval);
/*
 * Determine the integral number of days.
 */
  days = (int)floor(remainder / 86400.0);
  remainder -= days * 86400.0;
/*
 * Determine the integral number of hours.
 */
  hours = (int)floor(remainder / 3600.0);
  remainder -= hours * 3600.0;
/*
 * Determine the integral number of minutes.
 */
  mins = (int)floor(remainder / 60.0);
  remainder -= mins * 60.0;
/*
 * Determine the floating-point number of seconds.
 */
  secs = remainder;
/*
 * Write the seconds part in advance so that we can see whether in its
 * rounded form it is displayed as 60.
 */
  do {
    if(precision > 0)
      sprintf(sec_str, "%#0*.*f", precision+3, precision, secs);
    else
      sprintf(sec_str, "%02.0f", secs);
/*
 * If the rounded number is 60 then carry to the next highest minute.
 */
    if(sec_str[0] == '6' && sec_str[1] == '0') {
      secs = 0.0;
      if(++mins >= 60.0) {
	mins = 0;
	if(++hours >= 24.0) {
	  hours = 0;
	  days++;
	};
      };
      sec_str[0] = '\0';
    };
  } while(sec_str[0]=='\0');
/*
 * Remove trailing zeroes from the fractional part?
 */
  if(trim_fraction && precision > 0) {
    for(i=strlen(sec_str); i > 0 && sec_str[i-1] == '0'; i--)
      ;
    if(sec_str[i-1] == '.')
      i--;
    sec_str[i] = '\0';
  };
/*
 * Which fields are non-zero?
 */
  if(days)
    fields |= SHOW_DAYS;
  if(hours)
    fields |= SHOW_HOURS;
  if(mins)
    fields |= SHOW_MINS;
  if(secs != 0.0)
    fields |= SHOW_SECS;
/*
 * If no fields are marked for display, arrange for the seconds field
 * to be displayed.
 */
  if(fields == 0)
    fields = SHOW_SECS;
/*
 * Display the day field?
 */
  if(fields & SHOW_DAYS) {
    int nnew = 0;
    sprintf(stream->work + nc, "%dd%n", days, &nnew);
    first = 0;
    nc += nnew;
  };
  if(fields & SHOW_HOURS) {
    int nnew = 0;
    sprintf(stream->work + nc, "%s%dh%n", first ? "":":", hours, &nnew);
    first = 0;
    nc += nnew;
  };
  if(fields & SHOW_MINS) {
    int nnew = 0;
    sprintf(stream->work + nc, "%s%dm%n", first ? "":":", mins, &nnew);
    first = 0;
    nc += nnew;
  };
  if(fields & SHOW_SECS) {
    int nnew = 0;
    sprintf(stream->work + nc, "%s%ss", first ? "":":", sec_str);
    first = 0;
    nc += nnew;
  };
/*
 * Determine the sign prefix, if any, plus its length.
 */
  prefix = (char*)(sign < 0 ? "-" : (show_positive ? "+" : (show_space ? " ":"")));
  nprefix = strlen(prefix);
/*
 * How many padding characters are needed to bring the field width
 * up to 'width' characters?
 */
  npad = width - nprefix + nc;
  if(npad < 0)
    npad = 0;
/*
 * Is any space-padding needed?
 */
  if(npad && !show_zeros && !left_adjust &&
     output_spaces(stream, npad))
    return 1;
/*
 * Now display the prefix, if any.
 */
  if(nprefix > 0 && write_OutputStream(stream, prefix))
    return 1;
/*
 * Is any zero padding needed between the sign prefix and the
 * number?
 */
  if(npad && show_zeros && !left_adjust &&
     output_spaces(stream, npad))
    return 1;
/*
 * Now output the number.
 */
  if(write_OutputStream(stream, stream->work))
    return 1;
/*
 * If left adjustment was requested, pad up to the field width with spaces.
 */
  if(npad && left_adjust && output_spaces(stream, npad))
    return 1;
  return 0;
}
