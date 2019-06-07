#ifndef print_h
#define print_h

#ifdef VXW
#include <fioLib.h>
#endif

/*
 * This module provides a facility that parses printf-style formats and
 * sequentially passes each converted argument and intervening string
 * segments to a given caller-provided output function. The caller
 * can also provide anonymous data to be passed to their output function
 * via a (void *context) argument. Note that under vxWorks fioFormatV
 * is substituted for print_format() so don't compile print.c under vxWorks.
 */

/*
 * Under vxWorks fioFormatV() is substituted for print_format().
 * fioFormatV() uses an int argument to pass application-specific
 * context data, whereas print_format() uses (void *). The following
 * typedef is an attempt to keep the two interfaces relatively compatible.
 */
#ifdef VXW
typedef int PrintContext;
#else
typedef void *PrintContext;
#endif

/*
 * The caller-provided output function should be prototyped and declared
 * with the following macro. The arguments of the function are:
 *
 * Input:
 *  buffer           char *   The rendered output of the latest converted
 *                            argument or intervening string segment. This
 *                            buffer is NOT '\0' terminated.
 *  nc                int     The number of characters to extract from nc.
 *  context  PrintContext     The context argument passed to print_format().
 * Output:
 *  return            int     0 - OK.
 *                            1 - An error occured. This tells print_format()
 *                                to abort and return -1.
 */
#define PRT_OUT_FN(fn) int (fn)(char *buffer, int nc, PrintContext context)

/*.......................................................................
 * This is the function that is responsible for parsing a format
 * string and sending the print output to a caller provided output
 * function. Its arguments are:
 *
 * Input:
 *  fmt              char *  The printf() format to use to convert the
 *                           arguments in 'ap'.
 *  ap            va_list    The list of arguments to associate with fmt[].
 *                           It is the responsibility of the caller to
 *                           call va_start() before calling print_format(),
 *                           and va_end() after print_format() returns.
 *  out_fn     PRT_OUT_FN(*) The function to pass converted output to.
 *  context  PrintContext    A pointer to any data that the caller wishes
 *                           to have access to within out_fn().
 * Output:
 *  return            int    The number of bytes passed to out_fn(), or
 *                           -1 on error.
 */
#ifdef VXW
#define print_format fioFormatV
#else
int print_format(const char *fmt, va_list ap,
		 PRT_OUT_FN(*out_fn), PrintContext context);
#endif

#endif
