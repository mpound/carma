#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <errno.h>

#include "carma/szaarrayutils/input.h"
#include "carma/szaarrayutils/lprintf.h"
#include "carma/szaarrayutils/pathname.h"

/* #define DEBUG */
/*
 * Declare a file input stream type.
 */
typedef struct {
  FILE *fp;        /* The file pointer of the opened file */
  char *name;      /* The name with which the file was opened */
  int neol;        /* The number of newline characters seen so far */
} FileInput;

/*
 * Declare a string input stream type.
 */
typedef struct {
  int free_string; /* True if 'string' should be freed by del_StringInput */
  char *string;    /* The string I/O buffer */
  int nread;       /* The number of characters read so far */
} StringInput;

/*
 * Declare a stdio-stream input stream type.
 */
typedef struct {
  int do_close;    /* True if del_StdioInput should close fp */
  FILE *fp;        /* The stdio input stream to read from */
} StdioInput;

static INPUT_READ_FN(read_FileInput);
static INPUT_DEL_FN(del_FileInput);
static INPUT_CLR_FN(clr_FileInput);
static INPUT_ERR_FN(err_FileInput);
static INPUT_READ_FN(read_StringInput);
static INPUT_DEL_FN(del_StringInput);
static INPUT_CLR_FN(clr_StringInput);
static INPUT_ERR_FN(err_StringInput);
static INPUT_READ_FN(read_StdioInput);
static INPUT_DEL_FN(del_StdioInput);
static INPUT_CLR_FN(clr_StdioInput);
static INPUT_ERR_FN(err_StdioInput);
static int is_digit(int c, int base);

static InputSource *close_InputSource(InputStream *stream);

/*.......................................................................
 * Generic input stream constructor. Note that the resturned stream will
 * act as though closed until open_InputStream() is called to connect
 * it to a source of input.
 *
 * Output:
 *  return      InputStream * The new stream context.
 */
InputStream *new_InputStream(void)
{
  InputStream *stream;  /* The new stream context */
/*
 * Allocate the container.
 */
  stream = (InputStream *) malloc(sizeof(InputStream));
  if(!stream) {
    lprintf(stderr, "new_InputStream: Insufficient memory.\n");
    return NULL;
  };
/*
 * Initialize the container.
 */
  stream->source = NULL;
  stream->source_mem = NULL;
  stream->nextc = ' ';
  stream->in_string = 0;
/*
 * Allocate a free-list for allocating InputSource objects from.
 */
  stream->source_mem = new_FreeList("new_InputStream", sizeof(InputSource), 5);
  if(!stream->source_mem)
    return del_InputStream(stream);
  return stream;
}

/*.......................................................................
 * Delete an input stream previously returned by new_InputStream().
 *
 * Input:
 *  stream  InputStream *  The stream to be deleted.
 * Output:
 *  return  InputStream *  The deleted stream context (Always NULL).
 */
InputStream *del_InputStream(InputStream *stream)
{
  if(stream) {
    close_InputStream(stream);
    stream->source_mem = del_FreeList("del_InputStream", stream->source_mem, 1);
    free(stream);
  };
  return NULL;
}

/*.......................................................................
 * Connect an existing input stream to a new source of input. If the
 * input stream is currently connected to such a source, it will be 
 * pushed onto a stack of input sources, to be restored later when
 * close_InputStream() is called.
 *
 * Input:
 *  stream       InputStream *   The stream to be (re-)opened.
 *  data                void *   The stream-type instance context data.
 *  read_fn    INPUT_READ_FN(*)  The read method function for the stream.
 *  clr_fn      INPUT_CLR_FN(*)  The reset method function for the stream.
 *  err_fn      INPUT_ERR_FN(*)  The error-prefixing function of the stream.
 *  del_fn      INPUT_DEL_FN(*)  The destructor method function to be
 *                               called upon to delete 'data'.
 * Output:
 *  return               int     0 - OK.
 *                               1 - Error. Note that the stream will be
 *                                   left closed and 'data' should be deleted
 *                                   deleted by the caller.
 */
int open_InputStream(InputStream *stream, void *data,
		     INPUT_READ_FN(*read_fn), INPUT_CLR_FN(*clr_fn),
		     INPUT_ERR_FN(*err_fn), INPUT_DEL_FN(*del_fn))
{
  InputSource *source;   /* The container of the new input-source specifics */
/*
 * Check arguments.
 */
  if(!stream || !data) {
    lprintf(stderr, "open_InputStream: NULL %s.\n",
	    !stream ? "stream":"data");
    return 1;
  };
  if(!read_fn || !clr_fn || !err_fn || !del_fn) {
    lprintf(stderr, "open_InputStream: Missing method function(s).\n");
    return 1;
  };
/*
 * Allocate the new input-source container.
 */
  source = (InputSource* )new_FreeListNode("open_InputStream", stream->source_mem);
  if(!source)
    return 1;
/*
 * Initialize the input-source container.
 */
  source->read_fn = read_fn;
  source->clr_fn = clr_fn;
  source->err_fn = err_fn;
  source->del_fn = del_fn;
  source->nextc = ' ';
  source->was_escape = 0;
  source->escaped = ' ';
  source->data = data;
  source->next = NULL;
/*
 * Push the input source onto the stack of sources.
 */
  source->next = stream->source;
  stream->source = source;
/*
 * If this stream is the only one on the stack re-initialize the
 * stream.
 */
  if(!source->next) {
    stream->nextc = ' ';
    stream->in_string = 0;
  };
/*
 * Get the first character.
 */
  if(read_InputStream(stream, 1))
    return 1;
  return 0;
}

/*.......................................................................
 * Close an input stream but don't delete it. This allows the stream to
 * be re-opened as another instance of the same or a different type of
 * stream without having to re-allocate the generic stream context.
 * Once a stream has been closed, calls to read_InputStream() return
 * 1 until it is succesfully re-opened.
 *
 * Input:
 *  stream   InputStream *   The stream to close.
 */
void close_InputStream(InputStream *stream)
{
  if(stream) {
    while(stream->source)
      close_InputSource(stream);
  };
}

/*.......................................................................
 * Remove and close the input source at the top of the stack.
 *
 * Input:
 *  stream  InputStream *   The host input stream.
 * Output:
 *  return  InputSource *   The deleted input source (NULL).
 */
static InputSource *close_InputSource(InputStream *stream)
{
  InputSource *source = stream->source;
  if(source) {
/*
 * Remove the input source from the stack.
 */
    stream->source = source->next;
    source->next = NULL;
/*
 * Delete its contents.
 */
    if(source->del_fn)
      source->data = source->del_fn(source->data);
    source->read_fn = 0;
    source->clr_fn = 0;
    source->err_fn = 0;
    source->del_fn = 0;
/*
 * Return the container to the free-list.
 */
    source = (InputSource* )del_FreeListNode("close_InputSource", 
					     stream->source_mem, source);
/*
 * Prepare to read from the newly reconnected enclosing input-source.
 */
    if(stream->source)
      stream->nextc = stream->source->nextc;
  };
  return NULL;
}

/*.......................................................................
 * Reset an input stream previously returned by new_InputStream().
 *
 * Input:
 *  stream  InputStream *  The stream to be reset.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Couldn't reset status.
 */
int reset_InputStream(InputStream *stream)
{
  InputSource *source = stream ? stream->source : NULL;
  if(source) {
    lprintf(stderr, "reset_InputStream: Stream closed.\n");
    return 1;
  };
/*
 * Clear the error status and find the start of the next line.
 */
  return source->clr_fn(source) || input_skip_past_eol(stream, 1);
}

/*.......................................................................
 * Connect an input stream to an existing text file. The file name
 * is constructed from dir[] and name[] as described in pathname.h.
 *
 * Input:
 *  stream  InputStream *  The input stream to connect the file to.
 *  dir            char *  The directory in which the file resides, or "".
 *  name           char *  The name of an existing text file to be opened
 *                         for read.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
int open_FileInputStream(InputStream *stream, char *dir, char *name)
{
  FileInput *fdata; /* The file-specific context to be registered */
/*
 * Sanity check the arguments.
 */
  if(!stream) {
    lprintf(stderr, "open_FileInputStream: NULL stream.\n");
    return 1;
  };    
  if(!dir || !name) {
    lprintf(stderr, "open_FileInputStream: File name not specified.\n");
    return 1;
  };
/*
 * Allocate the file-stream context.
 */
  fdata = (FileInput *) malloc(sizeof(FileInput));
  if(!fdata) {
    lprintf(stderr, "open_FileInputStream: Insufficient memory.\n");
    return 1;
  };
/*
 * Before attempting any operation that might fail, initialize the
 * container at least up to the point at which it can be passed to
 * del_FileInput().
 */
  fdata->fp = NULL;
  fdata->name = NULL;
  fdata->neol = 0;
/*
 * Compose the file name.
 */
  fdata->name = new_pathname(dir, name);
  if(!fdata->name) {
    fdata = (FileInput* )del_FileInput(fdata);
    return 1;
  };
/*
 * Attempt to open the named file.
 */
  fdata->fp = fopen(fdata->name, "r");
  if(!fdata->fp) {
    lprintf(stderr, "Unable to open file: '%s'.\n", fdata->name);
    fdata = (FileInput* )del_FileInput(fdata);
    return 1;
  };
/*
 * Register the file with the input stream.
 */
  if(open_InputStream(stream, fdata, read_FileInput, clr_FileInput,
		      err_FileInput, del_FileInput)) {
    fdata = (FileInput* )del_FileInput(fdata);
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Read one character from a file input source.
 *
 * Input:
 *  source  InputSource *  The file source.
 * Output:
 *  return          int    0 - OK.
 *                         1 - EOF reached.
 */
static INPUT_READ_FN(read_FileInput)
{
  FileInput *fdata = (FileInput *) source->data;
  source->nextc = getc(fdata->fp);
/*
 * Count newline characters.
 */
  if(source->nextc == '\n')
    fdata->neol++;
  return 0;
}

/*.......................................................................
 * Delete the context of a file source.
 *
 * Input:
 *  data    void *  A FileInput pointer cast to (void *).
 * Output:
 *  return  void *  The deleted context (Always NULL).
 */
static INPUT_DEL_FN(del_FileInput)
{
  FileInput *fdata = (FileInput *) data;
  if(fdata) {
    if(fdata->fp) {
      if(fclose(fdata->fp)) {
	lprintf(stderr, "Error closing file: %s.\n",
		fdata->name ? fdata->name : "(unknown)");
      };
    };
    if(fdata->name)
      free(fdata->name);
    free(fdata);
  };
  return NULL;
}

/*.......................................................................
 * Reset the error status of a file source.
 *
 * Input:
 *  source  InputSource *  The file source.
 */
static INPUT_CLR_FN(clr_FileInput)
{
  FileInput *fdata = (FileInput *) source->data;
  clearerr(fdata->fp);
  return 0;
}

/*.......................................................................
 * Output an error-message prefix for a file source to stderr.
 *
 * Input:
 *  source  InputSource *  The file source.
 */
static INPUT_ERR_FN(err_FileInput)
{
  FileInput *fdata = (FileInput *) source->data;
  int line_number = fdata->neol + (source->nextc != '\n');
  return lprintf(stderr, "%s %d: ", fdata->name, line_number) < 0;
}

/*.......................................................................
 * Connect an input stream to an existing string.
 *
 * Input:
 *  stream       InputStream *  The stream to connect the string to.
 *  copy                 int    Whether to make a dynamically allocated
 *                              copy of the string. You should only
 *                              specify false (0) if you are absolutely
 *                              certain that either close_InputStream()
 *                              or del_InputStream() will be called before
 *                              the specified string is destroyed or
 *                              modified.
 *  string              char *  The string to initialize the iterator with.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
int open_StringInputStream(InputStream *stream, int copy, char *string)
{
  StringInput *sdata;     /* The string-specific context to be registered */
/*
 * Sanity check the arguments.
 */
  if(!stream) {
    lprintf(stderr, "open_StringInputStream: NULL stream.\n");
    return 1;
  };    
  if(!string) {
    lprintf(stderr, "open_StringInputStream: NULL string.\n");
    return 1;
  };
/*
 * Allocate the stream context.
 */
  sdata = (StringInput *) malloc(sizeof(StringInput));
  if(!sdata) {
    lprintf(stderr, "open_StringInputStream: Insufficient memory.\n");
    return 1;
  };
/*
 * Before attempting any operation that might fail, initialize the
 * container at least up to the point at which it can be passed to
 * del_StringInput().
 */
  sdata->free_string = copy;
  sdata->string = NULL;
  sdata->nread = 0;
/*
 * Allocate a copy of the input string if requested.
 */
  if(copy) {
    sdata->string = (char *) malloc(strlen(string) + 1);
    if(!sdata->string) {
      lprintf(stderr, "open_StringInputStream: Insufficient memory.\n");
      sdata = (StringInput* )del_StringInput(sdata);
      return 1;
    };
    strcpy(sdata->string, string);
  } else {
    sdata->string = string;
  };
/*
 * Register the string to the specified stream.
 */
  if(open_InputStream(stream, sdata, read_StringInput, clr_StringInput,
		      err_StringInput, del_StringInput)) {
    sdata = (StringInput* )del_StringInput(sdata);
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Read a single character from an input string source and place it in
 * source->nextc.
 *
 * Input:
 *  source  InputSource *  A string input source.
 * Output:
 *  return          int    0 - OK.
 *                         1 - EOF reached.
 */
static INPUT_READ_FN(read_StringInput)
{
  StringInput *sdata = (StringInput *) source->data;
/*
 * Get the next character from the string.
 */
  source->nextc = sdata->string[sdata->nread++];
/*
 * Convert end-of-string into the EOF character.
 */
  if(source->nextc == '\0')
    source->nextc = EOF;
  return 0;
}

/*.......................................................................
 * Delete the context of a string stream.
 *
 * Input:
 *  data    void *  The string context to be deleted.
 * Output:
 *  return  void *  The deleted context (Always NULL).
 */
static INPUT_DEL_FN(del_StringInput)
{
  StringInput *sdata = (StringInput *) data;
  if(sdata) {
    if(sdata->free_string && sdata->string)
      free(sdata->string);
    free(sdata);
  };
  return NULL;
}

/*.......................................................................
 * Reset a string input source.
 *
 * Input:
 *  source  InputSource *  The string source.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Couldn't clear error status.
 */
static INPUT_CLR_FN(clr_StringInput)
{
  StringInput *sdata = (StringInput *) source->data;
/*
 * Can't read past end of string.
 */
  if(sdata->nread > (int)strlen(sdata->string))
    return 1;
  return 0;
}

/*.......................................................................
 * Output an error-message prefix for a string source to stderr.
 *
 * Input:
 *  source  InputSource *  The file source.
 */
static INPUT_ERR_FN(err_StringInput)
{
  return 0;
}

/*.......................................................................
 * Connect an input stream to a (FILE *) pointer opened for read.
 *
 * Input:
 *  stream  InputStream *  The input stream to connect the file to.
 *  do_close        int    If a subsequent call to close_InputStream()
 *                         or del_InputStream() should close fp then
 *                         specify non-zero.
 *  fp             FILE *  The pointer to an open text stdio-stream,
 *                         opened for read. You must not read or position
 *                         this file while it is connected to an input
 *                         stream.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
int open_StdioInputStream(InputStream *stream, int do_close, FILE *fp)
{
  StdioInput *sdata;   /* The stream context to be registered */
/*
 * Check arguments.
 */
  if(!stream || !fp) {
    lprintf(stderr, "open_StdioInputStream: NULL %s.\n",
	    !stream ? "stream":"file pointer");
    return 1;
  };
/*
 * Allocate the stdio-stream context.
 */
  sdata = (StdioInput *) malloc(sizeof(StdioInput));
  if(!sdata) {
    lprintf(stderr, "open_StdioInputStream: Insufficient memory.\n");
    return 1;
  };
/*
 * Before attempting any operation that might fail, initialize the
 * container at least up to the point at which it can be passed to
 * del_StdioInput().
 */
  sdata->do_close = do_close;
  sdata->fp = fp;
/*
 * Register the stdio-pointer to the specified stream.
 */
  if(open_InputStream(stream, sdata, read_StdioInput, clr_StdioInput,
		      err_StdioInput, del_StdioInput)) {
    sdata = (StdioInput* )del_StdioInput(sdata);
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Read one character from a stdio-stream input source.
 *
 * Input:
 *  source  InputSource *  The stdio-stream source.
 * Output:
 *  return          int    0 - OK.
 *                         1 - EOF reached.
 */
static INPUT_READ_FN(read_StdioInput)
{
  StdioInput *sdata = (StdioInput *) source->data;
/*
 * Get the next character from the stream.
 */
  source->nextc = getc(sdata->fp);
  return 0;
}

/*.......................................................................
 * Delete an StdioInput stream context.
 *
 * Input:
 *  data    void *  The stream context to be deleted.
 * Output:
 *  return  void *  The deleted stream context (Always NULL).
 */
static INPUT_DEL_FN(del_StdioInput)
{
  StdioInput *sdata = (StdioInput *) data;
  if(sdata) {
    if(sdata->do_close && sdata->fp && fclose(sdata->fp))
      lprintf(stderr, "del_StdioInput: Error closing stdio stream.\n");
    free(sdata);
  };
  return NULL;
}

/*.......................................................................
 * Reset a stdio-stream source.
 *
 * Input:
 *  source  InputSource *  The source context.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Couldn't clear error status.
 */
static INPUT_CLR_FN(clr_StdioInput)
{
  StdioInput *sdata = (StdioInput *) source->data;
  clearerr(sdata->fp);
  return 0;
}

/*.......................................................................
 * Output an error-message prefix for a stdio input-stream to stderr.
 *
 * Input:
 *  source  InputSource *  The stdio input source.
 */
static INPUT_ERR_FN(err_StdioInput)
{
  return 0;
}

/*.......................................................................
 * Read the next character of an input stream and record it in
 * stream->nextc.
 *
 * Input:
 *  stream  InputStream *  The stream to read from.
 *  tell            int    Non-zero to report errors to stderr.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
int read_InputStream(InputStream *stream, int tell)
{
  InputSource *source; /* The input source that is at the top of the stack */
  int was_escape;      /* Preserved copy of previous source->was_escape */
/*
 * Get the current input source.
 */
  source = stream ? stream->source : NULL;
/*
 * Check that we have an open stream.
 */
  if(!source) {
    if(stream)
      stream->nextc = EOF;
    return input_error(stream, tell, "End of input.\n");
  };
/*
 * Preserve the previous value of stream->was_escape before resetting
 * it to zero.
 */
  was_escape = source->was_escape;
  source->was_escape = 0;
/*
 * If the last returned character was an escape character then the next
 * character has already been read.
 */
  if(was_escape) {
#ifdef DEBUG
  lprintf(stdout,"Last returned character was escaped\n");
#endif

    source->nextc = source->escaped;
/*
 * If we have reached the end of the current input source, close it
 * and continue the read from its parent (if it has one).
 */
  } else if(source->nextc == EOF) {
#ifdef DEBUG
  lprintf(stdout,"Reached end of input source.\n");
#endif

    close_InputSource(stream);
    source = stream->source;
    if(!source) {
      stream->nextc = EOF;
      return 0;
    };
    stream->nextc = source->nextc;
/*
 * Read the next character from the current input source.
 */
  } else if(source->read_fn(source)) {
#ifdef DEBUG
  lprintf(stdout,"Read next character.\n");
#endif

    return 1;
  };
/*
 * Check for special characters.
 */
#ifdef DEBUG
  lprintf(stdout,"[%c]",stream->nextc);
#endif
  switch(source->nextc) {
  case '\\':         /* Check for escaped newlines and escaped quotes */
/*
 * Is this the start of a new escape sequence?
 */
    if(!was_escape) {
/*
 * Look-ahead at the character being escaped.
 */
      if(source->read_fn(source))
	return 1;
/*
 * If the escaped character is a newline character then skip both the
 * escape and the newline and return the next character after the newline.
 */
      if(source->nextc == '\n') {
	return read_InputStream(stream, tell);
      } else {
/*
 * Record the escaped character so that it can be returned on the next
 * call to this function, and record the escape character for return
 * this time.
 */
	source->was_escape = 1;
	source->escaped = source->nextc;
	source->nextc = '\\';
      };
    };
    break;
  case '\"':         /* Keep a record of whether we are in a string */
    if(!was_escape)
      stream->in_string = !stream->in_string;
    break;
  case '#':          /* Discard comments */
    if(!was_escape && !stream->in_string) {
      while(source->nextc != '\n' && source->nextc != EOF) {
	if(source->read_fn(source))
	  return 1;
      };
    };
    break;
  case '\n':          /* Valid characters not recognized by isprint() */
  case ' ':
  case '\t':
  case EOF:
    break;
  default:           /* Check for valid characters */
    if(!isprint(source->nextc))
      return input_error(stream, tell, "Unprintable charactern.\n");
    break;
  };
/*
 * Copy the newly read character into the stream. At the end of
 * the input source add a newline. This ensures that the
 * last line of the source is properly terminated and because it
 * has the side effect of deferring the closure of the stream until
 * the next call to this function, it allows errors pertaining to
 * the last character of the source to be prefixed by input_error()
 * using the error-prefix function of the current source.
 */
  stream->nextc = source->nextc==EOF ? '\n' : source->nextc;
  return 0;
}

/*.......................................................................
 * Skip to the end of the current line. On return the next character
 * will be '\n' or EOF.
 *
 * Input:
 *  stream   InputStream *  The stream to read from.
 *  tell             int    Non-zero to report errors to stderr.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
int input_skip_to_eol(InputStream *stream, int tell)
{
  do {
    if(stream->nextc == '\n' || stream->nextc == EOF)
      return 0;
  } while(read_InputStream(stream, tell) == 0);
  return 1;
}

/*.......................................................................
 * Skip to the start of the next line.
 *
 * Input:
 *  stream   InputStream *   The stream to read from.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Error.
 */
int input_skip_past_eol(InputStream *stream, int tell)
{
  do {
    if(stream->nextc == '\n' || stream->nextc == EOF)
      return read_InputStream(stream, tell);
  } while(read_InputStream(stream, tell) == 0);
  return 1;
}

/*.......................................................................
 * Read an ASCII keyword from an input stream.
 *
 * Keywords must start with an alphabetical character and only consist
 * of alphanumeric and underscore characters.
 *
 * The output keyword will have been folded in to lower case and
 * left in stream->work[].
 *
 * Input:
 *  stream  InputStream * The input stream to read the keyword from.
 *  tell            int   Non-zero to report errors to stderr.
 *  fold            int   If true fold uppercase characters to
 *                        lower-case.
 * Output:
 *  return          int   0 - OK.
 *                        1 - Error.
 */
int input_keyword(InputStream *stream, int tell, int fold)
{
  int i;
/*
 * Check the first character of the keyword. This must either be
 * alphabetical or an underscore.
 */
  if(!isalpha(stream->nextc) && stream->nextc != '_')
    return input_error(stream, tell, "Not a keyword.\n");
/*
 * Copy up to INPUT_WORKLEN-1 alphanumeric and underscore characters from
 * the input stream.
 */
  for(i=0; i<INPUT_WORKLEN-1; i++) {
#ifdef DEBUG
    lprintf(stdout,"%c",stream->nextc);
#endif
    if(isalnum(stream->nextc) || stream->nextc == '_') {
      stream->work[i] = (fold && isupper(stream->nextc)) ?
	tolower(stream->nextc) : stream->nextc;
    } else {
      break;     /* Past end of keyword */
    };
    if(read_InputStream(stream, tell))
      return 1;
  };
/*
 * Terminate the string.
 */
  stream->work[i] = '\0';
/*
 * Buffer overflow?
 */
  if(i >= INPUT_WORKLEN)
    return input_error(stream, tell, "Keyword too long.\n");
  return 0;
}

/*.......................................................................
 * Read an ASCII keyword from an input stream.
 *
 * Keywords must start with an alphabetical character and only consist
 * of alphanumeric and underscore characters.
 *
 * The output keyword will have been folded in to lower case and
 * left in stream->work[].
 *
 * Input:
 *  stream  InputStream * The input stream to read the keyword from.
 *  tell            int   Non-zero to report errors to stderr.
 *  fold            int   If true fold uppercase characters to
 *                        lower-case.
 * Output:
 *  return          int   0 - OK.
 *                        1 - Error.
 */
int inputEnumKeyword(InputStream *stream, int tell, int fold)
{
  int i;
/*
 * Check the first character of the keyword. This must either be
 * alphabetical or an underscore.
 */
  if(!isalnum(stream->nextc) && stream->nextc != '_')
    return input_error(stream, tell, "Not a keyword.\n");
/*
 * Copy up to INPUT_WORKLEN-1 alphanumeric and underscore characters from
 * the input stream.
 */
  for(i=0; i<INPUT_WORKLEN-1; i++) {
#ifdef DEBUG
    lprintf(stdout,"%c",stream->nextc);
#endif
    if(isalnum(stream->nextc) || stream->nextc == '_') {
      stream->work[i] = (fold && isupper(stream->nextc)) ?
	tolower(stream->nextc) : stream->nextc;
    } else {
      break;     /* Past end of keyword */
    };
    if(read_InputStream(stream, tell))
      return 1;
  };
/*
 * Terminate the string.
 */
  stream->work[i] = '\0';
/*
 * Buffer overflow?
 */
  if(i >= INPUT_WORKLEN)
    return input_error(stream, tell, "Keyword too long.\n");
  return 0;
}

/*.......................................................................
 * Read an ASCII regexp keyword from an input stream.
 *
 * Regexp keywords can contain any character except a period.
 *
 * The output keyword will have been folded in to lower case and
 * left in stream->work[].
 *
 * Input:
 *  stream  InputStream * The input stream to read the keyword from.
 *  tell            int   Non-zero to report errors to stderr.
 *  fold            int   If true fold uppercase characters to
 *                        lower-case.
 * Output:
 *  return          int   0 - OK.
 *                        1 - Error.
 */
int input_board_regexp_keyword(InputStream *stream, int tell, int fold)
{
  int i;
/*
 * Copy up to INPUT_WORKLEN-1 alphanumeric and underscore characters from
 * the input stream.
 */
  for(i=0; i<INPUT_WORKLEN-1; i++) {
    if(!isspace(stream->nextc) && stream->nextc != '.' && stream->nextc != '[') {
      stream->work[i] = (fold && isupper(stream->nextc)) ?
	tolower(stream->nextc) : stream->nextc;
    } else {
      break;     /* Past end of keyword */
    };
    if(read_InputStream(stream, tell))
      return 1;
  };
/*
 * Terminate the string.
 */
  stream->work[i] = '\0';
/*
 * Buffer overflow?
 */
  if(i >= INPUT_WORKLEN)
    return input_error(stream, tell, "Keyword too long.\n");
  return 0;
}

/*.......................................................................
 * Read an ASCII regexp keyword from an input stream.
 *
 * Regexp keywords can contain any character except a period.
 *
 * The output keyword will have been folded in to lower case and
 * left in stream->work[].
 *
 * Input:
 *  stream  InputStream * The input stream to read the keyword from.
 *  tell            int   Non-zero to report errors to stderr.
 *  fold            int   If true fold uppercase characters to
 *                        lower-case.
 * Output:
 *  return          int   0 - OK.
 *                        1 - Error.
 */
int input_regexp_keyword(InputStream *stream, int tell, int fold)
{
  int i;
/*
 * Copy up to INPUT_WORKLEN-1 alphanumeric and underscore characters from
 * the input stream.
 */
  for(i=0; i<INPUT_WORKLEN-1; i++) {
#ifdef DEBUG
    lprintf(stdout,"%c",stream->nextc);
#endif
    if(!isspace(stream->nextc) && stream->nextc != '.') {
      stream->work[i] = (fold && isupper(stream->nextc)) ?
	tolower(stream->nextc) : stream->nextc;
    } else {
      break;     /* Past end of keyword */
    };
    if(read_InputStream(stream, tell))
      return 1;
  };
/*
 * Terminate the string.
 */
  stream->work[i] = '\0';
/*
 * Buffer overflow?
 */
  if(i >= INPUT_WORKLEN)
    return input_error(stream, tell, "Keyword too long.\n");
  return 0;
}

/*.......................................................................
 * Read an ASCII word from an input stream. A word is defined as a string
 * of printable characters excluding white-space.
 *
 * The output keyword will be left in stream->work[].
 *
 * Input:
 *  stream  InputStream * The input stream to read the word from.
 *  tell            int   Non-zero to report errors to stderr.
 *  fold            int   If true fold uppercase characters to
 *                        lower-case.
 * Output:
 *  return          int   0 - OK.
 *                        1 - Error.
 */
int input_word(InputStream *stream, int tell, int fold)
{
  int i;
/*
 * Copy up to INPUT_WORKLEN-1 printable characters from
 * the input stream.
 */
  for(i=0; i<INPUT_WORKLEN-1; i++) {
    if(isprint(stream->nextc) && !isspace(stream->nextc)) {
      stream->work[i] = (fold && isupper(stream->nextc)) ?
	tolower(stream->nextc) : stream->nextc;
    } else {
      break;     /* Past end of keyword */
    };
    if(read_InputStream(stream, tell))
      return 1;
  };
/*
 * Terminate the string.
 */
  stream->work[i] = '\0';
/*
 * Buffer overflow?
 */
  if(i >= INPUT_WORKLEN)
    return input_error(stream, tell, "Word too long.\n");
/*
 * Zero-length word?
 */
  if(i==0)
    return input_error(stream, tell, "Zero-length word.\n");
  return 0;
}

/*.......................................................................
 * Skip white-space starting from or just after the character in
 * stream->nextc. Unlike input_skip_space() this treats newline as a space.
 *
 * Input:
 *  stream InputStream * The stream to read from.
 *  tell           int   Non-zero to report errors to stderr.
 *  advance        int   If true, start skipping white-space from beyond
 *                       stream->nextc. Otherwise only skip white-space
 *                       if stream->nextc is a white-space character.
 * Output:
 *  return         int   0 - OK.
 *                       1 - Error.
 */
int input_skip_white(InputStream *stream, int tell, int advance)
{
  int ierr = 0;
/*
 * Stop at EOF.
 */
  if(stream->nextc==EOF)
    return 0;
/*
 * Is stream->nextc significant?
 */
  if(!advance && !isspace(stream->nextc))
    return 0;
/*
 * Skip white-space following stream->nextc in the stream.
 */
  while((ierr=read_InputStream(stream, tell))==0 && isspace(stream->nextc))
    ;
  return ierr;
}

/*.......................................................................
 * Skip spaces and horizontal tabs starting from or just after the
 * character in stream->nextc. If you want to skip newlines as well
 * use input_skip_white().
 *
 * Input:
 *  stream InputStream * The stream to read from.
 *  tell           int   Non-zero to report errors to stderr.
 *  advance        int   If true, start skipping spaces from beyond
 *                       stream->nextc. Otherwise only skip spaces
 *                       if stream->nextc is a space character.
 * Output:
 *  return         int   0 - OK.
 *                       1 - Error.
 */
int input_skip_space(InputStream *stream, int tell, int advance)
{
  int ierr = 0;
/*
 * Stop at EOF.
 */
  if(stream->nextc==EOF)
    return 0;
/*
 * Is stream->nextc significant?
 */
  if(!advance && !(stream->nextc==' ' || stream->nextc=='\t'))
    return 0;
/*
 * Skip spaces following stream->nextc in the stream.
 */
  while((ierr=read_InputStream(stream, tell))==0 &&
	(stream->nextc==' ' || stream->nextc=='\t'))
    ;
  return ierr;
}

/*.......................................................................
 * Read a quoted string from an input stream.
 *
 * The initial and trailing "'s will be omitted from the output string.
 * Escaped characters will be converted to their un-escaped ASCII forms.
 *
 * The resulting string will be left in stream->work[].
 *
 * Input:
 *  stream InputStream *  The stream to read the string from.
 *  tell           int    Non-zero to report errors to stderr.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int input_quoted_string(InputStream *stream, int tell)
{
  int wpos = 0;          /* Index of next unused character in 'work[]' */
/*
 * Check for the initial double-quote.
 */
  if(stream->nextc != '\"')
    return input_error(stream, tell, "Missing initial \"");
  if(read_InputStream(stream, tell))
    return 1;
/*
 * Copy up to INPUT_WORKLEN processed characters up to the next un-escaped
 * double-quote into work[].
 */
  while(stream->nextc != '\"' && stream->nextc != EOF) {
    int c;  /* The next output character */
/*
 * Check for escaped characters.
 */
    if(stream->nextc == '\\') {
      if(read_InputStream(stream, tell))
	return 1;
/*
 * Handle octal representations of characters.
 */
      if(stream->nextc == '0') {
	int ndigit;
	char digits[3];
/*
 * Read up to two octal digits into digits[].
 */
	if(read_InputStream(stream, tell))
	  return 1;
	for(ndigit = 0; ndigit < 3 && is_digit(stream->nextc,8); ndigit++) {
	  digits[ndigit] = stream->nextc;
	  if(read_InputStream(stream, tell))
	    return 1;
	};
/*
 * Read the octal number from digits[].
 */
	digits[ndigit] = '\0';
	c = ndigit > 0 ? (unsigned char) strtol(digits, NULL, 8) : '\0';
/*
 * Handle hexadecimal representations of characters.
 */
      } else if(stream->nextc == 'x' || stream->nextc == 'X') {
	int ndigit;
	char digits[3];
/*
 * Read up to two trailing hexadecimal digits into digits[].
 */
	if(read_InputStream(stream, tell))
	  return 1;
	for(ndigit = 0; ndigit < 3 && is_digit(stream->nextc, 16); ndigit++) {
	  digits[ndigit] = stream->nextc;
	  if(read_InputStream(stream, tell))
	    return 1;
	};
/*
 * Read the octal number from digits[].
 */
	digits[ndigit] = '\0';
	c = ndigit > 0 ? (unsigned char) strtol(digits, NULL, 8) : '\0';
/*
 * Interpret 2-character ANSI-C escapes.
 */
      } else {
	switch(stream->nextc) {
	case 'a':         /* Alert (bell) character */
	  c = '\a';
	  break;
	case 'b':         /* Backspace */
	  c = '\b';
	  break;
	case 'f':         /* Formfeed */
	  c = '\f';
	  break;
	case 'n':         /* Newline */
	  c = '\n';
	  break;
	case 'r':         /* Carriage return */
	  c = '\r';
	  break;
	case 't':         /* Horizontal tab */
	  c = '\t';
	  break;
	case 'v':         /* Vertical tab */
	  c = '\v';
	  break;
	case '\\':        /* Backslash */
	  c = '\\';
	  break;
	case '\?':        /* Question mark */
	  c = '\?';
	  break;
	case '\'':        /* Single quote */
	  c = '\'';
	  break;
	case '\"':        /* Double quote */
	  c = '\"';
	  break;
	default:          /* Unknown escape - just discard the backslash */
	  c = stream->nextc;
	  break;
	};
	if(read_InputStream(stream, tell))
	  return 1;
      };
    } else {
      c = stream->nextc;
      if(read_InputStream(stream, tell))
	return 1;
    };
/*
 * Record the new character in the return array if there is room for
 * both it and a final terminating '\0'.
 */
    if(wpos < INPUT_WORKLEN-1) {
      stream->work[wpos++] = c;
    } else {
      return input_error(stream, tell, "String too long.\n");
    };
  };
/*
 * Terminate the string.
 */
  stream->work[wpos] = '\0';
/*
 * End of input?
 */
  if(stream->nextc == EOF)
    return input_error(stream, tell, "Unterminated string.\n");
/*
 * Skip the trailing '"'.
 */
  return read_InputStream(stream, tell);
}

/*.......................................................................
 * Read an unquoted string from an input stream.
 *
 * Characters will be read until all open parentheses have been matched,
 * all quoted sub-strings have been read to completion and a character
 * is encountered that results in is_literal() returning 0.
 *
 * The function recognizes sub-strings enclosed in double quotes and
 * doesn't match parentheses within these strings. Escape sequences are
 * left unexpanded.
 *
 * The resulting string will be left in stream->work[], minus leading and
 * trailing white-space.
 *
 * Input:
 *  stream       InputStream *  The stream to read the string from.
 *  tell                 int    Non-zero to report errors to stderr.
 *  opn                 char *  The list of open parentheses characters.
 *  cls                 char *  The list of close parentheses characters
 *                              that match the open parentheses in opn[].
 *                              Eg. if opn[]="({[", then cls[]=")}]".
 *  is_literal IS_LITERAL_FN(*) A function such as isdigit() that returns
 *                              non-zero if a given character, found outside
 *                              of parentheses and quoted strings, should be
 *                              included within the literal string. On
 *                              encountering one that shouldn't be included,
 *                              the string is terminated and the function
 *                              returns.
 *  nl_escapes          char *  In cases where is_literal() returns 0 for
 *                              newlines, but one doesn't want to terminate
 *                              input there when the newline character is
 *                              escaped, specify a non-empty array of
 *                              characters which supress the terminating
 *                              effect of a following newline character.
 *                              Spaces and tabs between these characters
 *                              and the newline character are ignored.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int input_literal(InputStream *stream, int tell, char *opn, char *cls,
		  IS_LITERAL_FN(*is_literal), char *nl_escapes)
{
  int wpos = 0;           /* Index of next unused character in 'work[]' */
  int in_string=0;        /* True if we are in a string */
  int was_escape = 0;     /* True if the previous character was an escape */
                          /*  character. */
  int escape_newline = 0; /* True after seeing one of the characters that */
                          /*  suppresses the terminating effect of a newline. */
/*
 * Declare a string array to be used as a stack for keeping track
 * of unmatched parentheses.
 */
  enum {MAX_UNMATCHED=20};       /* The size of unmatched[] */
  char unmatched[MAX_UNMATCHED]; /* The parenthesis stack */
  int nunmatched = 0;            /* The number of unmatched parenthesis in */
                                 /*  unmatched[] */
/*
 * Check arguments.
 */
  if(!stream || !opn || !cls || !is_literal || !nl_escapes)
    return input_error(stream, 1, "input_literal: NULL argument(s).\n");
/*
 * Make sure that every open parenthesis in opn[] is matched by a
 * close parenthesis in cls[].
 */
  if(strlen(opn) != strlen(cls)) {
    return input_error(stream, 1, "input_literal: The opn[] and cls[] arguments have incompatible lengths.\n");
  };
/*
 * Copy up to INPUT_WORKLEN processed characters into the work buffer.
 * Stopping when a terminator character is seen outside of sub-strings
 * and parentheses.
 */
  while(wpos < INPUT_WORKLEN && stream->nextc != EOF &&
	(nunmatched > 0 || in_string || is_literal(stream->nextc) ||
	 (stream->nextc=='\n' && escape_newline))) {
/*
 * Get the new character.
 */
    int c = stream->nextc;
/*
 * Ignore open and close parentheses and newline escape characters
 * within strings.
 */
    if(!in_string) {
/*
 * Does the character escape a following newline character?
 * If the character isn't one of the specified escape characters
 * turn off any preceding escape when the next non-space character
 * is seen.
 */
      if(strchr(nl_escapes, c))
	escape_newline = 1;
      else if(c!=' ' && c!='\t')
	escape_newline = 0;
/*
 * If the character is one of the open parenthesis characters,
 * append it to the stack of open parentheses in unmatched[].
 */
      if(strchr(opn, c)) {
	if(nunmatched >= MAX_UNMATCHED) {
	  return input_error(stream, tell,
			     "Too many unmatched (%s) characters.\n", opn);
	};
	unmatched[nunmatched++] = c;
/*
 * Is the character one of the close parenthesis characters?
 */
      } else if(strchr(cls, c)) {
/*
 * Locate the position of the parenthesis character in opn[] and cls[].
 */
	int posn = strchr(cls, c) - cls;
/*
 * Check that the close parenthesis matches the last unmatched open
 * parenthesis.
 */
	if(nunmatched==0 || unmatched[nunmatched-1] != opn[posn])
	  return input_error(stream, tell, "Mismatched '%c' parenthesis.\n", c);
/*
 * There is now one less unmatched open parenthesis.
 */
	nunmatched--;
      };
    };
/*
 * Check for escape characters.
 */
    if(c == '\\') {
      was_escape = !was_escape;
/*
 * Start or end of sub-string?
 */
    } else if(c == '"') {
      if(!was_escape)
	in_string = !in_string;
      was_escape = 0;
    };
/*
 * Record the new character in the return array if there is room for
 * both it and a final terminating '\0'.
 */
    if(wpos < INPUT_WORKLEN-1) {
      if(wpos || !isspace(c))   /* Strip leading white-space */
	stream->work[wpos++] = c;
    } else {
      return input_error(stream, tell, "String too long.\n");
    };
/*
 * Read the next character.
 */
    if(read_InputStream(stream, tell))
      return 1;
  };
/*
 * Terminate the string at the last non-white-space character.
 */
  while(--wpos >= 0 && isspace((int) stream->work[wpos]))
    ;
  stream->work[wpos+1] = '\0';
/*
 * Is the string empty?
 */
  if(wpos < 0)
    return input_error(stream, tell, "Zero length string.\n");
  return 0;
}

/*.......................................................................
 * Read a long integer from an input stream.
 *
 * Input:
 *  stream InputStream *  The stream to read the integer from.
 *  tell           int    Non-zero to report errors to stderr.
 *  anybase        int    If true, interpret an initial zero (after the
 *                        optional sign) as the start of a C-style base
 *                        specification prefix. The following prefixes
 *                        are supported:
 *                         0x      -  Hexadecimal.
 *                         0[0-7]  -  Octal [0-7]....
 *                         0b      -  Binary.
 *                        else decimal.
 * Input/Output:
 *  lval          long *  On output *lval will contain the
 *                        long integer read from the input stream.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int input_long(InputStream *stream, int tell, int anybase, long *lval)
{
  unsigned long ulval;  /* The return value of input_ulong() */
  int sign = 1;         /* +1 if the number is positive, -1 if negative */
/*
 * Get the optional sign character.
 */
  if(stream->nextc == '+' || stream->nextc == '-') {
    sign = stream->nextc=='+' ? 1 : -1;
    if(read_InputStream(stream, tell))
      return 1;
  };
/*
 * Having removed the sign prefix, get input_ulong() to read the unsigned
 * part.
 */
  if(input_ulong(stream, 0, anybase, &ulval))
    return input_error(stream, tell, "Missing integer.\n");
/*
 * Give the number its sign.
 */
  *lval = sign * ulval;
  return 0;
}

/*.......................................................................
 * Read an unsigned long integer from an input stream.
 *
 * Input:
 *  stream   InputStream *  The stream to read the integer from.
 *  tell             int    Non-zero to report errors to stderr.
 *  anybase          int    If true, interpret an initial zero as the
 *                          start of a C-style base specification prefix.
 * Input/Output:
 *  ulval  unsigned long *  On output *ulval will contain the unsigned
 *                          long integer read from the input stream.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
int input_ulong(InputStream *stream, int tell, int anybase, unsigned long *ulval)
{
  int wpos = 0;  /* Index of next unused character in 'stream->work[]' */
  int base;      /* The base in which the number is written */
/*
 * Identify the base by looking at the first number.
 * Numbers starting with 0 are to be interpretted as
 * octal unless the 0 is immediately followed by an X or a B.
 * 0x switches to hexadecimal and 0b switches to binary.
 */
  if(anybase && stream->nextc == '0') {
    if(read_InputStream(stream, tell))
      return 1;
    switch(stream->nextc) {
    case 'x': case 'X':
      base = 16;
      if(read_InputStream(stream, tell))
	return 1;
      break;
    case 'b': case 'B':
      base = 2;
      if(read_InputStream(stream, tell))
	return 1;
      break;
    case 0: case 1: case 2: case 3: case 4: case 5: case 6: case 7:
      base = 8;
      break;
    case 8: case 9:  /* Not octal if 0 followed by non-octal digit */
      base = 10;
      break;
    default:
      *ulval = 0;
      return 0;
    };
  } else {
    base = 10;
  };
/*
 * Make sure that we have a number of some variety.
 */
  if(!is_digit(stream->nextc, base))
    return input_error(stream, tell, "Missing unsigned integer.\n");
/*
 * Copy up to INPUT_WORKLEN-1 alphanumeric and underscore characters from
 * the input stream.
 */
  while(is_digit(stream->nextc, base) && wpos < INPUT_WORKLEN) {
    stream->work[wpos++] = stream->nextc;
    if(read_InputStream(stream, tell))
      return 1;
  };
/*
 * Check for buffer overflow.
 */
  if(wpos >= INPUT_WORKLEN)
    return input_error(stream, tell, "Integer too long.\n");
/*
 * Terminate the string.
 */
  stream->work[wpos] = '\0';
/*
 * Read the number out of the work buffer.
 */
  *ulval = strtoul(stream->work, NULL, base);
  return 0;
}

/*.......................................................................
 * Read a number from an ASCII input stream.
 *
 * Input:
 *  stream InputStream *  The stream to read the number from.
 *  tell           int    Non-zero to report errors to stderr.
 * Input/Output:
 *  dval        double *  The resulting number will be assigned to
 *                        *dval.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int input_double(InputStream *stream, int tell, double *dval)
{
  Number number;  /* The container for the return value of input_number(). */
/*
 * Read the number.
 */
  if(input_number(stream, 0, 1, &number))
    return input_error(stream, tell, "Missing number.\n");
#ifdef _GPP
  *dval = number.sign * (number.type==Number::NUM_INT ? 
			 number.value.ival :
			 number.value.dval);
#else
  *dval = number.sign * (number.type==NUM_INT ? 
			 number.value.ival :
			 number.value.dval);
#endif

  return 0;
}

/*.......................................................................
 * Read a number from an ASCII input stream. Note that this function
 * does not skip leading white-space.
 *
 * Input:
 *  stream InputStream *  The stream to read the number from.
 *  tell           int    Non-zero to report errors to stderr.
 *  sign_ok        int    None zero if the number can have a sign.
 * Input/Output:
 *  number      Number *  The resulting number in a form appropriate to
 *                        way that it was written.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int input_number(InputStream *stream, int tell, int sign_ok, Number *number)
{
  int have_mantissa = 0; /* Used to record existence of mantissa digits */
  int wpos = 0;          /* Index of next unused character in 'work[]' */
  int is_int = 1;        /* True if number is an integer with no exponent */
/*
 * Interpret and skip sign characters
 */
  number->sign = stream->nextc=='-' ? -1 : 1;
  if((stream->nextc == '+' || stream->nextc == '-')) {
    if(!sign_ok)
      return input_error(stream, tell, "Unexpected sign.\n");
    if(wpos < INPUT_WORKLEN) {
      stream->work[wpos++] = stream->nextc;
      if(read_InputStream(stream, tell))
	return 1;
    };
  };
/*
 * Get digits from before the decimal point.
 * and record them in the work buffer.
 */
  wpos = 0;
  if(isdigit(stream->nextc) && wpos < INPUT_WORKLEN) {
    have_mantissa = 1;
    do {
      stream->work[wpos++] = stream->nextc;
      if(read_InputStream(stream, tell))
	return 1;
    } while(isdigit(stream->nextc) && wpos < INPUT_WORKLEN);
  };
/*
 * See if there is a decimal point.
 */
  if(stream->nextc == '.' && wpos < INPUT_WORKLEN) {
    is_int = 0;
    stream->work[wpos++] = stream->nextc;
    if(read_InputStream(stream, tell))
      return 1;
/*
 * See if there are any digits following the decimal point.
 */
    if(isdigit(stream->nextc) && wpos < INPUT_WORKLEN) {
      have_mantissa = 1;
      do {
	stream->work[wpos++] = stream->nextc;
	if(read_InputStream(stream, tell))
	  return 1;
      } while(isdigit(stream->nextc) && wpos < INPUT_WORKLEN);
    };
  };
/*
 * Do we have a valid mantissa?
 */
  if(wpos < INPUT_WORKLEN && !have_mantissa)
    return input_error(stream, tell, "Missing number.\n");
/*
 * Check for an exponent.
 */
  if((stream->nextc == 'e' || stream->nextc == 'E') && wpos < INPUT_WORKLEN) {
    is_int = 0;
    stream->work[wpos++] = stream->nextc;
    if(read_InputStream(stream, tell))
      return 1;
/*
 * Get optional sign.
 */
    if((stream->nextc == '+' || stream->nextc == '-') && wpos < INPUT_WORKLEN) {
      stream->work[wpos++] = stream->nextc;
      if(read_InputStream(stream, tell))
	return 1;
    };
/*
 * Get the integer part of the exponent.
 */
    if(wpos < INPUT_WORKLEN) {
      if(!isdigit(stream->nextc))
	return input_error(stream, tell, "Malformed exponent.\n");
      do {
	stream->work[wpos++] = stream->nextc;
	if(read_InputStream(stream, tell))
	  return 1;
      } while(isdigit(stream->nextc) && wpos < INPUT_WORKLEN);
    };
  };
/*
 * Check for buffer overflow.
 */
  if(wpos >= INPUT_WORKLEN)
    return input_error(stream, tell, "Number too long.\n");
/*
 * Terminate the string copy of the number.
 */
  stream->work[wpos] = '\0';
/*
 * Read the number from the string.
 */
  if(is_int) {
#ifdef _GPP
    number->type = Number::NUM_INT;
#else
    number->type = NUM_INT;
#endif
    number->value.ival = atoi(stream->work);
  } else {
#ifdef _GPP
    number->type = Number::NUM_DOUBLE;
#else
    number->type = NUM_DOUBLE;
#endif
    number->value.dval = atof(stream->work);
  };
  return 0;
}

/*.......................................................................
 * Read a date of form 23JAN1997 or 23/01/1997 from an input stream.
 * It is assumed that the entered year includes its century.
 *
 * Input:
 *  stream   InputStream *  The input stream.
 *  tell             int    Non-zero to report errors to stderr.
 * Input/Output:
 *  int             year *  The year will be assigned to *year.
 *  int            month *  The month number within the year (1,12) will
 *                          be assigned to *month.
 *  int              day *  The day number of the month (1,31) will be
 *                          assigned to *day.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
int input_date(InputStream *stream, int tell, int *year, int *month, int *day)
{
  char *usage = "Invalid date - use DD-MMM-YYYY";
  unsigned long dy;         /* The day of the month */
  unsigned long mn;         /* The month of the year */
  unsigned long yr;         /* The year */
  int isleap;               /* True if the parsed year is a leap year */
  enum {MONTH_LEN=3};       /* The length of a month-name abbreviation */
  char mname[MONTH_LEN+1];  /* A 3-letter month-name abbreviation + '\0' */
  enum {NUM_MONTH=12};      /* The number of months in a year */
  int i;
/*
 * Record the number of days per month, first in normal years, then in
 * leap years.
 */
  static char daytab[2][NUM_MONTH] = {
    {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
    {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
  };
/*
 * List the conventional 3-letter abbreviations for the names of months.
 */
  static const char *months[NUM_MONTH] = {
    "JAN",  "FEB",  "MAR",  "APR", 
    "MAY",  "JUN",  "JUL",  "AUG",
    "SEP",  "OCT",  "NOV",  "DEC"
  };
/*
 * Check arguments.
 */
  if(!stream || !day || !month || !year)
    return input_error(stream, 1, "input_date: NULL argument(s).\n");
/*
 * Read the day of the month.
 */
  if(input_ulong(stream, 0, 0, &dy))
    return input_error(stream, tell, usage);
/*
 * The next character should be a '-' separator.
 */
  if(stream->nextc != '-')
    return input_error(stream, tell, usage);
/*
 * Skip the separator.
 */
  if(read_InputStream(stream, 0))
    return input_error(stream, tell, usage);
/*
 * Read the 3 characters of a month name abbreviation and
 * convert them to upper case.
 */
  for(i=0; i<MONTH_LEN; i++) {
/*
 * Check the new character.
 */
    if(!isalpha(stream->nextc))
      return input_error(stream, tell, usage);
/*
 * Convert the new character to lower case and add it to the
 * accumulated month name.
 */
    mname[i] = islower(stream->nextc) ? toupper(stream->nextc):stream->nextc;
/*
 * Advance to the next character.
 */
    if(read_InputStream(stream, 0))
      return input_error(stream, tell, usage);
  };
/*
 * Terminate the month name.
 */
  mname[MONTH_LEN] = '\0';
/*
 * Lookup the month name.
 */
  for(mn=1; mn<=NUM_MONTH && strcmp(mname, months[mn-1]) != 0; mn++)
    ;
/*
 * Month not recognized?
 */
  if(mn > NUM_MONTH)
    return input_error(stream, tell, "Unknown month [%s]", mname);
/*
 * The next character should be a '-' separator.
 */
  if(stream->nextc != '-')
    return input_error(stream, tell, usage);
/*
 * Skip the separator.
 */
  if(read_InputStream(stream, 0))
    return input_error(stream, tell, usage);
/*
 * Read the year.
 */
  if(input_ulong(stream, 0, 0, &yr))
    return input_error(stream, tell, usage);
/*
 * Is this a leap year?
 */
  isleap = (yr%4 == 0 && yr%100 != 0) || yr%400 == 0;
/*
 * Check that the date makes sense.
 */
  if(dy < 1 || dy > (unsigned long)(daytab[isleap][mn-1])) {
    return input_error(stream, tell, "Nonexistent date (%02lu-%.3s-%04lu)",
		dy, months[mn-1], yr);
  };
/*
 * Assign the values for return.
 */
  *day = dy;
  *month = mn;
  *year = yr;
  return 0;
}

/*.......................................................................
 * Read a sexagesimal-format number from an input stream. The expected
 * syntax is an optional sign, followed by zero or more integers separated
 * by colons, followed by a floating point number with optional fractional
 * part: [+-](int:)*int.int.
 *
 * Thus the following 5 numbers are equivalent:
 *
 *   -0:23:30:36 -23:30:36 -23:30:36.0 -23:30.6 -23.51
 *
 * In each case the returned number would be -23.51.
 *
 * Input:
 *  stream   InputStream *  The input stream.
 *  tell             int    Non-zero to report errors to stderr.
 * Input/Output:
 *  result        double *  The number that was read.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
int input_sexagesimal(InputStream *stream, int tell, double *result)
{
  Number n;             /* A number read by input_number() */
  double number;        /* The number accumulated so far */
  double divisor = 1.0; /* The scale factor to divide the next component by */
  int negative = 0;     /* True if the number is negative */
/*
 * Read the first number. Only this component is ever allowed to have
 * an explicit sign.
 */
  if(input_number(stream, tell, 1, &n))
    return 1;
#ifdef _GPP
  number = n.type==Number::NUM_DOUBLE ? n.value.dval : n.value.ival;
#else
  number = n.type==NUM_DOUBLE ? n.value.dval : n.value.ival;
#endif
  negative = n.sign == -1;
/*
 * Read components until a non-integral number or a component that
 * is not followed by a component separator is encountered.
 */
#ifdef _GPP
  while(n.type==Number::NUM_INT && stream->nextc == ':') {
#else
  while(n.type==NUM_INT && stream->nextc == ':') {
#endif
    double d;
/*
 * Skip the separator and read the next component.
 */
    if(read_InputStream(stream, 0) || input_number(stream, 0, 0, &n))
      return input_error(stream, tell, "Not a sexagesimal component.\n");
/*
 * Get a double precision version of the number and domain check it.
 */
#ifdef _GPP
    d = n.type==Number::NUM_DOUBLE ? n.value.dval : n.value.ival;
#else
    d = n.type==NUM_DOUBLE ? n.value.dval : n.value.ival;
#endif
    if(d >= 60)
      return input_error(stream, tell, "Illegal sexagesimal component >= 60.\n");
/*
 * Add the new component with the appropriate scale factor.
 */
    divisor *= 60.0;
    number += d / divisor;
  };
/*
 * Return the result.
 */
  *result = negative ? -number : number;
  return 0;
}

/*.......................................................................
 * Parse a sexagesimal time (eg. 23:34:00) from an input stream.
 * The recognized variations, and their meanings, are:
 *
 *  23:34:00.0  ->   23:34:00
 *  23:34:00    ->   23:34:00
 *  23:34       ->   23:34:00
 *  23          ->   23:00:00
 *
 * Input:
 *  stream   InputStream *  The stream to read from.
 *  tell             int    Non-zero to report errors to stderr.
 * Input/Output:
 *  int             hour *  The number of hours (0..23).
 *  int              min *  The number of minutes into hour 'hour' (0..59).
 *  double           sec *  The number of seconds into minute 'min' (0..<60).
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
int input_time(InputStream *stream, int tell, int *hour, int *min, double *sec)
{
  char *usage = "Invalid time - use hh:mm:ss.s";
  unsigned long v;    /* An hour or minute value */
  double s;           /* A seconds value */
/*
 * Check the arguments.
 */
  if(!stream || !hour || !min || !sec)
    return input_error(stream, 1, "input_time: NULL argument(s).\n");
/*
 * Read the hour component.
 */
  if(input_ulong(stream, 0, 0, &v) || v > 23)
    return input_error(stream, tell, usage);
  *hour = v;
/*
 * If there is a minutes component then there should be a : separator.
 */
  if(stream->nextc == ':') {
/*
 * Skip the separator and read the minutes component.
 */
    if(read_InputStream(stream, 0) || input_ulong(stream, 0, 0, &v) || v > 59)
      return input_error(stream, tell, usage);
    *min = v;
  } else {
    *min = 0;
  };
/*
 * If there is a seconds component then there should be a : separator.
 */
  if(stream->nextc == ':') {
/*
 * Skip the separator and read the seconds component
 */
    if(read_InputStream(stream, 0) || input_double(stream, 0, &s) ||
       s < 0.0 || s >= 60.0)
      return input_error(stream, tell, usage);
    *sec = s;
  } else {
    *sec = 0.0;
  };
  return 0;
}

/*.......................................................................
 * Parse a date and time from an input stream.
 *
 * The following are valid specifications:
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
 *
 * Input:
 *  stream   InputStream *  The input stream.
 *  tell             int    Non-zero to report errors to stderr.
 *  nospace          int    By default, the year field can be separated
 *                          from the hour field by either a colon or a space.
 *                          To disallow a space as a separator, set nospace
 *                          to non-zero.
 * Input/Output:
 *  int             year *  The year will be assigned to *year.
 *  int            month *  The month number within the year (1,12) will
 *                          be assigned to *month.
 *  int              day *  The day number of the month (1,31) will be
 *                          assigned to *day.
 *  int             hour *  The number of hours into day 'day' (0..23).
 *  int              min *  The number of minutes into hour 'hour' (0..59).
 *  double           sec *  The number of seconds into minute 'min' (0..<60).
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
int input_date_and_time(InputStream *stream, int tell, int nospace, int *year,
			int *month, int *day, int *hour, int *min, double *sec)
{
  int read_time;   /* True if there is a time-specification to be read */
/*
 * Check the arguments.
 */
  if(!stream || !year || !month || !day || !hour || !min || !sec)
    return input_error(stream, 1, "input_date_and_time: NULL argument(s).\n");
/*
 * Read the date components.
 */
  if(input_date(stream, tell, year, month, day))
    return 1;
/*
 * See if there is a time specification.
 */
  if(stream->nextc == ':' || (!nospace && stream->nextc==' ')) {
    if(read_InputStream(stream, 0))
      return input_error(stream, tell, "Missing time specification.\n");
    read_time = isdigit(stream->nextc);
  } else {
    read_time = 0;
  };
/*
 * Read a time specification?
 */
  if(read_time) {
    if(input_time(stream, tell, hour, min, sec))
      return 1;
  } else {
    *hour = *min = 0;
    *sec = 0.0;
  };
  return 0;
}

/*.......................................................................
 * Return true if the given character can be used to represent one
 * digit of one of three number bases.
 *
 * Input:
 *  c      int  The character to test.
 *  base   int  The number base to test for, from 2,8,10,16.
 * Output:
 *  return int  1 -  The character is a digit of 'base'.
 *              0 -  The character is not a digit of 'base'.
 */
static int is_digit(int c, int base)
{
  switch(c) {
  case '0':
  case '1':
    return base > 1;
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
    return base > 7;
  case '8':
  case '9':
    return base > 9;
  case 'a': case 'A':
  case 'b': case 'B':
  case 'c': case 'C':
  case 'd': case 'D':
  case 'e': case 'E':
  case 'f': case 'F':
    return base > 15;
  };
  return 0;
}

/*.......................................................................
 * An fprintf-like function for optionally reporting error messages.
 * If 'tell' is non-zero the message is reported to stderr. Otherwise
 * the message is discarded. The input_error() function always returns
 * 1, for use by calling functions as an error-return code (see above).
 *
 * Input:
 *  stream InputStream *  The stream that generated the error.
 *  tell           int    If non-zero report the message to stderr.
 *                        Otherwise discard it.
 *  fmt           char *  A standard printf() format.
 *  ...                   Arguments corresponding to format specifiers
 *                        in fmt[].
 * Output:
 *  return         int    1 - The normal error code of input_xxx() functions.
 */
int input_error(InputStream *stream, int tell, const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  if(stream) {
    if(tell)
      input_verror(stream, fmt, ap);
  } else {
    lprintf(stderr, "input_error: NULL stream (");
    vlprintf(stderr, fmt, ap);
    lprintf(stderr, ")\n");
  };
  va_end(ap);
  return 1;
}

/*.......................................................................
 * Report an input-stream error to stderr.
 *
 * Input:
 *  stream InputStream *  The input stream that contained the error.
 *  fmt           char *  A standard printf() format.
 *  args       va_list    Variable argument list arguments corresponding
 *                        to format specifiers in fmt[]. This must have
 *                        been initialized via a call to va_start().
 */
void input_verror(InputStream *stream, const char *fmt, va_list args)
{
/*
 * Prefix the message with an indication of the location of the error.
 */
  if(stream->source)
    stream->source->err_fn(stream->source);
/*
 * Append the error message.
 */
  vlprintf(stderr, fmt, args);
  return;
}

/*.......................................................................
 * Read a time interval written like the following example:
 *
 *  23d:14h:16m:2.32s   (ie. 23 days, 14 hours, 16 minutes, 2.32 seconds)
 *
 * Any of the components can be omitted, but each trailing colon is taken
 * as evidence that another component follows. Only the first of the
 * chosen components can have a sign, and only the last component can
 * have a fractional part. For each component, it is an error for
 * the value of that component to exceed one unit of the previous component.
 * Thus 23d:300m is ok, but 23d:4h:300m isn't. The result is
 * returned in seconds, via the *interval argument.
 *
 * Input:
 *  stream   InputStream *  The input stream.
 *  tell             int    Non-zero to report errors to stderr.
 * Input/Output:
 *  interval      double *  The interval will be assigned to *interval
 *                          in seconds.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
int input_interval(InputStream *stream, int tell, double *interval)
{
  int sign=1;      /* The sign of the interval */
  double days=0.0; /* The value of the day component of the interval */
  double hours=0.0;/* The value of the hour component of the interval */
  double mins=0.0; /* The value of the minutes component of the interval */
  double secs=0.0; /* The value of the seconds component of the interval */
/*
 * Enumerate the units of the components in the order that they
 * are expected.
 */
  enum {INT_NONE, INT_DAYS, INT_HOURS, INT_MINS, INT_SECS};
  int type = INT_NONE;  /* The type of the latest component */
/*
 * Check the arguments.
 */
  if(!stream || !interval)
    return input_error(stream, tell, "input_interval: NULL argument(s).\n");
/*
 * Read at least one time component, each component suffixed with a letter
 * that specifies its units, and separated from the next component by a colon.
 */
  do {
    Number number;         /* The new time component */
    int last_type = type;  /* The type of the last component parsed */
/*
 * Unless this is the first component, we have already verified that the
 * next character is a ':'. Skip this.
 */
    if(type!=INT_NONE && read_InputStream(stream, tell))
      return 1;
/*
 * Read the next time component.
 */
    if(input_number(stream, 0, type==INT_NONE, &number))
      return input_error(stream, tell, "Missing time-interval component.\n");
/*
 * Get the suffix letter that specifies the units.
 */
    switch(stream->nextc) {
    case 'd':   /* Days */
      type = INT_DAYS;
#ifdef _GPP
      days = number.type==Number::NUM_INT ? number.value.ival : number.value.dval;
#else
      days = number.type==NUM_INT ? number.value.ival : number.value.dval;
#endif
      break;
    case 'h':   /* Hours */
      type = INT_HOURS;
#ifdef _GPP
      hours = number.type==Number::NUM_INT ? number.value.ival : number.value.dval;
#else
      hours = number.type==NUM_INT ? number.value.ival : number.value.dval;
#endif
      if(last_type == INT_DAYS && hours >= 24.0) {
	return input_error(stream, tell,
			   "Out of range hour field in time interval.\n");
      };
      break;
    case 'm':   /* Minutes */
      type = INT_MINS;
#ifdef _GPP
      mins = number.type==Number::NUM_INT ? number.value.ival : number.value.dval;
#else
      mins = number.type==NUM_INT ? number.value.ival : number.value.dval;
#endif
      if((last_type == INT_DAYS && mins >= 1440.0) ||
	 (last_type == INT_HOURS && mins >= 60.0)) {
	return input_error(stream, tell,
			   "Out of range hour field in time interval.\n");
      };
      break;
    case 's':   /* Seconds */
      type = INT_SECS;
#ifdef _GPP
      secs = number.type==Number::NUM_INT ? number.value.ival : number.value.dval;
#else
      secs = number.type==NUM_INT ? number.value.ival : number.value.dval;
#endif
      if((last_type == INT_DAYS && secs >= 86400.0) ||
	 (last_type == INT_HOURS && secs >= 3600.0) ||
	 (last_type == INT_MINS && secs >= 60.0)) {
	return input_error(stream, tell,
			   "Out of range hour field in time interval.\n");
      };
      break;
    default:
      if(stream->nextc=='\n') {
	return input_error(stream, tell, "Missing suffix in time-interval.\n");
      } else {
	return input_error(stream, tell,
		"Unknown suffix '%c' follows a time-interval component.\n",
		 stream->nextc);
      };
      break;
    };
/*
 * Skip the suffix.
 */
    if(read_InputStream(stream, tell))
      return 1;
/*
 * The first component specifies the sign.
 */
    if(last_type==INT_NONE)
      sign = number.sign;
/*
 * Only the last component is allowed to be non-integer, so complain
 * if there is another component and the latest component wasn't
 * an integer.
 */
#ifdef _GPP
    if(stream->nextc == ':' && number.type==Number::NUM_DOUBLE) {
#else
    if(stream->nextc == ':' && number.type==NUM_DOUBLE) {
#endif
	return input_error(stream, tell, "Unexpected fraction part in time-interval component.");
    };
/*
 * Check that the components are being specified in the correct order.
 */
    if(last_type >= type) {
      return input_error(stream, tell,
			 "Out of order time-interval component.\n");
    };
  } while(stream->nextc == ':');
/*
 * Compute the time interval in seconds.
 */
  *interval = sign * (secs + 60 * (mins + 60 * (hours + 24 * days)));
  return 0;
}

IS_LITERAL_FN(isHostName)
{
  return isalnum(c) || c== '.';
}
