#ifndef lprintf_h
#define lprintf_h

#include "carma/szautil/Logger.h"

#include <stdio.h>
#include <stdarg.h>

/*
 * lprintf() is a drop-in replacement to fprintf() and printf()
 * designed for redirecting stdout and/or stderr messages to alternate
 * logging channels such as message queues, TCP/IP streams, widgets
 * etc.. Note that lprintf(stream,...) will behave identically to
 * fprintf(stream,...) until divert_lprintf(stream,...) is called to
 * register a logging function to that stream.
 *
 * A limitation of this facility is that when logging to a function is
 * enabled, individual messages are truncated at LOG_MSGLEN bytes
 * (actually LOG_MSGLEN+1 when account is take of the trailing '\0').
 *
 * Note that the combination of lprintf() and print_format() [and
 * presumably its vxWorks equivalent, fioFormatV()] use about 750
 * bytes of stack space.
 *
 * Threads issues:
 *  In pthreads programs be sure to define the _POSIX_C_SOURCE macro
 *  to be at least 199506L during compilation of all files that include
 *  lprintf.h (including lprintf.c).
 *
 *  In VxWorks programs define VXW when compiling any file that includes
 *  lprintf.h, and call vw_install_lprintf() before creating the first task
 *  [apart from the caller of vw_install_lprintf()] that will use lprintf().
 *  If subsequently you need to deinstall the module that contains lprintf(),
 *  make sure that all tasks that use lprintf() have been deleted (with the
 *  possible exception of the task that calls vw_remove_lprintf()), then
 *  call vw_remove_lprintf() to remove the task creation and deletion
 *  hooks that were installed by vw_install_lprintf().
 *
 *  Provided that the above guidlines are followed, divert_lprintf()
 *  and lprintf() will operate on a per thread basis.  In particular,
 *  when logging functions are registered to a thread, the associated
 *  streams will be line buffered on a per thread basis to prevent
 *  interleaving of partial lines from different threads.
 */

/*
 * VxWorks installation and removal functions for lprintf().
 * See the above discussion of "Threads issues".
 */
#ifdef VXW
int vw_install_lprintf(void);
int vw_remove_lprintf(void);
#endif

/*
 * The following is the length of the longest log message
 * (excluding '\0'). Longer messages will be truncated before
 * being dispatched to the function registered to the associated
 * stream.
 */
#define LOG_MSGLEN 127

/*
 * When this header is compiled with gcc, the following macro
 * is expanded at the end of the prototype of lprintf. The
 * result informs gcc that the format and trailing arguments should
 * be checked as though the function were printf().
 */
#undef CHECK_FORMAT
#ifdef __GNUC__
#define CHECK_FORMAT __attribute__ ((format (printf, 2, 3)))
#else
#define CHECK_FORMAT
#endif

/*
 * The following functions are drop-in logging replacements for the
 * similarly named stdio functions. When logging is disabled, they
 * behave identically to their stdio counterparts.
 */
int lprintf(FILE *stream, const char *fmt, ...) CHECK_FORMAT;
#undef CHECK_FORMAT

int vlprintf(FILE *stream, const char *fmt, va_list ap);
int lputc(int c, FILE *stream);
int lputs(const char *s, FILE *stream);
int lflush(FILE *stream);

namespace sza {
  namespace array {
    /*
     * The dispatch function is passed an enumerated equivalent of
     * stdout or stderr, depending upon which stream the caller
     * presented to lprintf(). This is useful if a single
     * dispatch function is registered to both stdout and stderr.
     */
    enum LogStream {
      LOG_STDOUT,      /* Log to stdout */
      LOG_STDERR       /* Log to stderr */
    };

  };
};

/*
 * Application-specific dispatch functions and their prototypes
 * should be declared using the following macro. The arguments
 * of a dispatch function are used as follows:
 *
 * Input:
 *  message      char * The latest message composed by lprintf().
 *                      This will be at most LOG_MSGLEN+1 bytes in
 *                      length and will '\0' terminated, regardless
 *                      of whether the message had to be truncated.
 *                      Note that the trailing newline is omitted.
 *  id      LogStream   The identity of the stream that the caller
 *                      of lprintf() cited. This is only useful if
 *                      the same dispatch function has been assigned
 *                      to stdout and stderr.
 *  context      void * The value of the 'context' argument of
 *                      divert_lprintf() when the dispatch function
 *                      was registered. Note that in a threaded
 *                      environment, modifications to this context
 *                      will need to be semaphore protected.
 * Output:
 *  return        int   0 - The message was logged without error.
 *                      1 - An error occured. This tells lprintf()
 *                          to return -1 instead of a character count.
 */
#define LOG_DISPATCHER(fn) int (fn)(char *message, sza::array::LogStream id, void *context, unsigned seq, bool isEnd)

/*
 * The following function registers a function to be called to process
 * the lprintf() line-buffered output of stdout and stderr. To revert
 * the specified stream to fprintf(), send log_fn=0. In order to allow
 * nesting of calls to divert_lprintf(), if the old_fn and old_context
 * arguments are both non-NULL, then any previous dispatch function
 * and its context will be assigned to them. The caller can then
 * reinstall them if needed.
 */
int divert_lprintf(FILE *stream, LOG_DISPATCHER(*log_fn), void *context,
		   LOG_DISPATCHER(**old_fn), void **old_context);

#endif
