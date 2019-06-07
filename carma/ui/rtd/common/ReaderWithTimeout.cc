/*
 * Implementation of a reader from stdin with timeout.
 *
 * @author Steve Scott
 * $id: $
 *
 * $CarmaCopyright$
 */

#include <unistd.h>
#include <netinet/in.h>
#include <sys/poll.h>
#include <sys/ioctl.h>

#include <iostream>
#include <cstring>
#include <cerrno>

#include <carma/util/Trace.h>

#include <carma/ui/rtd/common/ReaderWithTimeout.h>

using namespace ::std;
using namespace carma::util;
using namespace carma::ui::rtd;

ReaderWithTimeout::ReaderWithTimeout(const struct timeval tv)
    : inputFile(STDIN_FILENO)
    , defaultTimeout(tv)
    , errorCode(SUCCESS)
{
    // empty
}


ReaderWithTimeout::ReaderWithTimeout()
    : inputFile(STDIN_FILENO)
    , errorCode(SUCCESS)
{
    // default timeout 5 seconds
    this->defaultTimeout.tv_sec = 5;
    this->defaultTimeout.tv_usec = 0;
}

ReaderWithTimeout::ReaderWithTimeout(const int fd, const struct timeval tv)
    : inputFile(fd)
    , defaultTimeout(tv)
    , errorCode(SUCCESS)
{
    // empty
}

int ReaderWithTimeout::getErrorCode() const
{
    return errorCode;
}

/**
 * Read an exact number of bytes with a timeout.
 *
 * @param nBytes the number of bytes to read
 * @param tv the struct timeval describing the timeout (may be NULL for infinite)
 * @param bytes the string to contain an error message or the data bytes
 * @return true for success, false for failure
 */
bool ReaderWithTimeout::getNBytes(int nBytes, struct timeval *tv, std::string &bytes)
{
    // zero out the return string
    bytes.clear();

    // setup for the select() system call
    fd_set rfds;
    FD_ZERO(&rfds);
    FD_SET(inputFile, &rfds);

    while (true) {
        // success, we read all of the bytes requested
        if (nBytes <= 0) {
            errorCode = SUCCESS;
            return true;
        }

        // timeout
        if (tv && tv->tv_sec <= 0 && tv->tv_usec <= 0) {
            errorCode = TIMEOUT;
            bytes = "overall timeout";
            return false;
        }

        int ret = select(1, &rfds, NULL, NULL, tv);

        // select error
        if (ret < 0) {
            errorCode = POLL_ERROR;
            bytes = "select: " + std::string(strerror(errno));
            return false;
        }

        // select timeout (0 fds became ready)
        if (ret == 0) {
            errorCode = TIMEOUT;
            bytes = "select: timeout";
            return false;
        }

        // get the number of bytes available
        int available;
        if (ioctl(inputFile, FIONREAD, &available) == -1) {
            errorCode = IOCTL_ERROR;
            bytes = "ioctl(FIONREAD): " + std::string(strerror(errno));
            return false;
        }

        // no bytes are available, but select() returned successfully
        // this probably means the connection is broken (closed on the other end)
        if (available == 0) {
            errorCode = BROKEN_CNX;
            bytes = "connection broken";
            return false;
        }

        // limit the read to the number of bytes requested
        if (nBytes < available)
            available = nBytes;

        for (int i = 0; i < available; i++) {
            unsigned char buf;
            ret = read(inputFile, &buf, sizeof(buf));
            if (ret == 0) {
                errorCode = BROKEN_CNX;
                bytes = "read: " + std::string(strerror(errno));
                return false;
            } else if (ret == 1) {
                bytes += buf;
                nBytes -= 1;
            } else {
                errorCode = READ_ERROR;
                bytes = "read: " + std::string(strerror(errno));
                return false;
            }
        }
    }

    // should never get here
    errorCode = SUCCESS;
    return true;
}

bool ReaderWithTimeout::getBytes(std::string &bytes, struct timeval tv)
{
    // read the first 4 bytes (size in network byte order)
    if (this->getNBytes(4, &tv, bytes) == false)
        return false;

    // calculate the number of bytes in the packet
    union {
        uint32_t len;
        char bytes[4];
    } un;

    for (int i = 0; i < 4; i++) {
        un.bytes[i] = bytes.at(i);
    }

    const uint32_t len = ntohl(un.len);
    return this->getNBytes(len, &tv, bytes);
}

bool ReaderWithTimeout::getBytes(std::string &bytes)
{
    return this->getBytes(bytes, this->defaultTimeout);
}
