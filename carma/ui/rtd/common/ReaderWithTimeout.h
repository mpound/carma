#ifndef CARMA_UI_RTD_READERWITHTIMEOUT_H
#define CARMA_UI_RTD_READERWITHTIMEOUT_H

/*
 * @file
 *
 * Reads from stdin with a timeout
 *
 * @author Steve Scott
 * $id: $
 *
 * $CarmaCopyright$
 */

#include <sys/time.h>

#include <cstdio>
#include <string>

namespace carma {
namespace ui {
namespace rtd {

/**
 * Read a series of bytes with a timeout and return a raw byte string
 * with the contents of the read.
 *
 * All byte strings begin with a 4-byte integer (in network byte order) which
 * contains the number of bytes in the packet. The data follows immediately.
 *
 * Works fine with sockets.
 */
class ReaderWithTimeout {
public:
    /// Constructor
    ReaderWithTimeout();
    /// Constructor
    ReaderWithTimeout(const struct ::timeval tv);
    /// Constructor
    ReaderWithTimeout(const int fd, const struct ::timeval tv);

    /**
     * Read input from file and put into input string.
     * If an error is encountered, an error message is put in the string.
     * @param input string in which to place input from stdin
     * @param timeoutSeconds timeout for receiving the input (seconds)
     * @return true for success, false for failure.
     */
    bool getBytes(std::string &bytes, struct ::timeval tv);
    bool getBytes(std::string &bytes);

    /**
     * Get error code of last IO
     * @return error code (see below)
     */
    int getErrorCode() const;

    /// Error codes
    enum {
        SUCCESS,                // successful read
        BROKEN_CNX,             // broken connection
        TIMEOUT,                // timeout
        IOCTL_ERROR,            // error with ioctl(FIONREAD)
        POLL_ERROR,             // error with poll()
        READ_ERROR,             // error with read()
    };

private:
    bool getNBytes(int nBytes, struct ::timeval *tv, std::string &bytes);

    const int inputFile; // file descriptor
    struct ::timeval defaultTimeout;
    int errorCode;
};

} // namespace carma::ui::rtd
} // namespace carma::ui
} // namespace carma

#endif  //  CARMA_UI_RTD_READERWITHTIMEOUT_H
