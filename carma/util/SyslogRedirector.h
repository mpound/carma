#ifndef CARMA_UTIL_SYSLOGREDIRECTOR_H
#define CARMA_UTIL_SYSLOGREDIRECTOR_H

#include <string>

namespace carma {
namespace util {

/*
 * Force all glibc error messages to go to stderr
 *
 * By default, glibc prints error messages using the internal function
 * __libc_message(). By default, this will send the error message and associated
 * backtrace to the controlling TTY. If that fails, the error message (but not
 * the associated backtrace) to syslog.
 *
 * However, glibc has a nice feature where you can force all output to go to
 * stderr rather than the controlling TTY. The syslog fallback will be called if
 * stderr fails for some reason.
 *
 * In order to capture both the message and associated backtrace from glibc
 * error messages, it is useful to force the output to stderr.
 */
void forceGlibcErrorsToStderr();

/*
 * Redirect any file descriptor to syslog
 *
 * This is most commonly used to redirect stdout or stderr to syslog and
 * therefore capture the output that would otherwise be lost.
 *
 * This is especially useful when there is a desire to capture glibc error
 * messages, such as the ones that result from heap corruption and abort
 * the program.
 */
void redirectFdToSyslog(int fd, const std::string &fdname);

} // namespace carma::util
} // namespace carma

#endif /* CARMA_UTIL_SYSLOGREDIRECTOR_H */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
