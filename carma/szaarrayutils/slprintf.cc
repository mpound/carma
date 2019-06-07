#include <stdarg.h>
#include <string.h>

#ifdef VXW
#include <vxWorks.h>
#include <fioLib.h>
#endif

#include "carma/szaarrayutils/print.h"
#include "carma/szaarrayutils/slprintf.h"

/*
 * A container of the following type is allocated from the stack
 * on entry to vslprintf(), initialized with details about the
 * caller's output buffer, and passed to fioFormatV() for use by
 * sl_output_fn().
 */

typedef struct {
  char *s;        /* The user's output buffer */ 
  int nmax;       /* The length of the user's output buffer */
  int length;     /* The current length of the string in s[] */
} PrintData;

static PRT_OUT_FN(sl_output_fn);

/*.......................................................................
 * This function is equivalent to sprintf(), except that the length of
 * the output string is provided by the caller so that the output can
 * be truncated instead of over-running the buffer and corrupting the
 * calling program.
 *
 * Input:
 *  s       char *   The string to print to.
 *  n     size_t     The length of the string.
 *  fmt     char *   The printf-style format string.
 *  ...              The variable argument list that goes with the
 *                   specified format.
 * Output:
 *  return   int     The number of characters written, or -1 on error.
 */
int slprintf(char *s, size_t n, const char *fmt, ...)
{
  int nc;      /* The number of characters succesfully output (-1 on error) */
  va_list ap;  /* The variable argument list */
  va_start(ap, fmt);
  nc = vslprintf(s, n, fmt, ap);
  va_end(ap);
  return nc;
}

/*.......................................................................
 * This function is equivalent to vsprintf(), except that the length of
 * the output string is provided by the caller so that the output can
 * be truncated instead of over-running the buffer and corrupting the
 * calling program.
 *
 * Input:
 *  s       char *   The string to print to.
 *  n     size_t     The length of the string.
 *  fmt     char *   The printf-style format string.
 *  ap   va_list     The variable argument list that goes with the
 *                   specified format.
 * Output:
 *  return   int     The number of characters written, or -1 on error.
 */
int vslprintf(char *s, size_t n, const char *fmt, va_list ap)
{
  PrintData data;  /* A container for details about the output buffer */
  int nc;          /* The number of characters written, or -1 on error */
/*
 * Record details about the output buffer in a container. This can
 * then be passed to the fioFormatV() output function.
 */
  data.s = s;
  data.nmax = n;
  data.length = 0;
/*
 * Format output into data.s[].
 */
  nc = print_format(fmt, ap, sl_output_fn, (PrintContext) &data);
/*
 * Terminate the accumulated string in data.s[].
 * Note that sl_output_fn() guarantees that data.length is < data.nmax.
 */
  data.s[data.length] = '\0';
/*
 * Return a count of the number of characters in s[], excluding the
 * terminating '\0'. Note that this may be less than the number
 * expected if the output had to be truncated.
 */
  return nc;
}

/*.......................................................................
 * This is the function that print_format() calls to render output into
 * the caller's output buffer.
 */
static PRT_OUT_FN(sl_output_fn)
{
  PrintData *data = (PrintData *) context;
/*
 * Work out how many of the nc characters in buffer[] can be appended
 * to data->s[]. Leave room for a '\0' terminator at the end of
 * data->s[].
 */
  int nnew = nc;
  if(data->length + nnew + 1 >= data->nmax)
    nnew = data->nmax - data->length - 1;
/*
 * Append nnew characters from buffer[] to data.s[].
 */
  if(nnew > 0) {
    memcpy(data->s + data->length, buffer, nnew);
    data->length += nnew;
  };
  return 0;
}
