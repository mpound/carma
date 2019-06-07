#ifndef slprintf_h
#define slprintf_h

#include <stddef.h>
#include <stdarg.h>

/*-----------------------------------------------------------------------
 * This modules provides the slprintf and vslprintf functions. These
 * are designed to act like the currently non-standard snprintf()
 * and vsnprintf(). The latter functions will be part of the
 * next C standard, but are not currently available in some OS's
 * (including Solaris).
 *-----------------------------------------------------------------------*/

/*
 * When this header is compiled with gcc, the following macro
 * is expanded at the end of the prototype of lprintf. The
 * result informs gcc that the format and trailing arguments should
 * be checked as though the function were printf().
 */
#undef CHECK_FORMAT
#ifdef __GNUC__
#define CHECK_FORMAT __attribute__ ((format (printf, 3, 4)))
#else
#define CHECK_FORMAT
#endif

int slprintf(char *s, size_t n, const char *format, ...) CHECK_FORMAT;
int vslprintf(char *s, size_t n, const char *format, va_list ap);

#endif
