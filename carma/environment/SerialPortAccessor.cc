/*
 * ONE-LINE DESCRIPTION
 *
 * Copyright (c) 2011 Ira W. Snyder <iws@ovro.caltech.edu>
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <iostream>
#include <cstdio>
#include <sstream>
#include <errno.h>
#include <poll.h>

#include <carma/util/programLogging.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/ErrorException.h>
using namespace carma::util;

#include <carma/environment/SerialPortAccessor.h>

#define ARRAY_SIZE(x) sizeof(x) / sizeof(*x)

namespace carma {
namespace environment {

/*----------------------------------------------------------------------------*/
/* Constructor, Destructor, Public Interface                                  */
/*----------------------------------------------------------------------------*/

SerialPortAccessor::SerialPortAccessor(const std::string &device, const int mode)
    : device_(device)
    , mode_(mode)
    , fd_(-1)
{
    // intentionally left empty
}

SerialPortAccessor::~SerialPortAccessor()
{
    // automatically close on destruct
    this->close();
}

void SerialPortAccessor::open()
{
    struct termios tio;
    int fd = -1;
    int ret;

    // the device is already open, ignore
    if (this->fd_ != -1)
        return;

    // we do everything in a try-except block so that it is
    // easy to exit at any point and close the file descriptor
    try {

        // open the device with the correct mode
        fd = ::open(device_.c_str(), mode_);
        if (fd == -1) {
            std::ostringstream oss;
            oss << "open failed: " << strerror(errno);
            throw CARMA_ERROR(oss.str());
        }

        // check that the device is a tty
        ret = isatty(fd);
        if (ret <= 0) {
            std::ostringstream oss;
            oss << "isatty failed: " << strerror(errno);
            throw CARMA_ERROR(oss.str());
        }

        // flush the input and output buffers
        ret = tcflush(fd, TCIOFLUSH);
        if (ret < 0) {
            std::ostringstream oss;
            oss << "tcflush failed: " << strerror(errno);
            throw CARMA_ERROR(oss.str());
        }

        // read the termios currently set in the device
        ret = tcgetattr(fd, &tio);
        if (ret < 0) {
            std::ostringstream oss;
            oss << "tcgetattr failed: " << strerror(errno);
            throw CARMA_ERROR(oss.str());
        }

        // call the user-provided setup function
        this->setup(&tio);

        // set the new termios
        ret = tcsetattr(fd, TCSAFLUSH, &tio);
        if (ret < 0) {
            std::ostringstream oss;
            oss << "tcsetattr failed: " << strerror(errno);
            throw CARMA_ERROR(oss.str());
        }

        // flush the input and output buffers
        ret = tcflush(fd, TCIOFLUSH);
        if (ret < 0) {
            std::ostringstream oss;
            oss << "tcflush failed: " << strerror(errno);
            throw CARMA_ERROR(oss.str());
        }

    } catch (...) {
        // error: make sure the file descriptor is closed
        if (fd != -1)
            ::close(fd);

        // log the error
        std::ostringstream oss;
        oss << "error opening serial port: " << getStringForCaught();
        programLogErrorIfPossible(oss.str());
    }

    // everything completed, save the file descriptor
    this->fd_ = fd;
}

void SerialPortAccessor::close()
{
    // not open, just return
    if (this->fd_ == -1)
        return;

    // close the file descriptor
    ::close(this->fd_);
    this->fd_ = -1;
}

ssize_t SerialPortAccessor::recv(char *buf, size_t len, int timeout)
{
    // the whole thing is in a try/catch block to make it
    // easy to perform cleanup actions
    try {
        struct pollfd fds[1];
        int fd = this->fd_;
        int ret;

        fds[0].fd = fd;
        fds[0].events = POLLIN;

        // block until the file descriptor is readable
        ret = poll(fds, ARRAY_SIZE(fds), timeout);

        // handle timeout
        if (ret == 0) {
            throw CARMA_ERROR("poll timeout");
        }

        // handle error
        if (ret == -1) {
            std::ostringstream oss;
            oss << "poll failed: " << strerror(errno);
            throw CARMA_ERROR(oss.str());
        }

        // it worked, perform the read
        const ssize_t bytes = ::read(fd, buf, len);

        // handle error
        if (bytes < 0) {
            std::ostringstream oss;
            oss << "read failed: " << strerror(errno);
            throw CARMA_ERROR(oss.str());
        }

        // everything worked, return the number of bytes read
        // from the serial port
        return bytes;

    } catch (...) {
        std::ostringstream oss;
        oss << "error in recv: " << getStringForCaught();
        programLogErrorIfPossible(oss.str());

        // just return no bytes
        return 0;
    }
}

ssize_t SerialPortAccessor::send(const char *buf, size_t len, int timeout)
{
    // the whole thing is in a try/catch block to make
    // error handling nice and easy
    try {
        struct pollfd fds[1];
        int fd = this->fd_;
        int ret;

        fds[0].fd = fd;
        fds[0].events = POLLOUT;

        // block until the file descriptor is writeable
        ret = poll(fds, ARRAY_SIZE(fds), timeout);

        // handle timeout
        if (ret == 0) {
            throw CARMA_ERROR("poll timeout");
        }

        // handle error
        if (ret == -1) {
            std::ostringstream oss;
            oss << "poll failed: " << strerror(errno);
            throw CARMA_ERROR(oss.str());
        }

        // it worked, perform the write
        const ssize_t bytes = ::write(fd, buf, len);

        // handle error
        if (bytes < 0) {
            std::ostringstream oss;
            oss << "write failed: " << strerror(errno);
            throw CARMA_ERROR(oss.str());
        }

        // everything worked, return the number of bytes written
        // to the serial port
        return bytes;

    } catch (...) {
        std::ostringstream oss;
        oss << "error in send: " << getStringForCaught();
        programLogErrorIfPossible(oss.str());

        // just return no bytes
        return 0;
    }
}

ssize_t SerialPortAccessor::recv_n(char *buf, size_t len, int timeout)
{
    size_t offset = 0;
    size_t avail = len;

    // run until there are no bytes left
    while (avail > 0) {
        const ssize_t bytes = this->recv(buf + offset, avail, timeout);
        offset += bytes;
        avail -= bytes;
    }

    return len;
}

ssize_t SerialPortAccessor::send_n(const char *buf, size_t len, int timeout)
{
    size_t offset = 0;
    size_t avail = len;

    // run until there are no bytes left
    while (avail > 0) {
        const ssize_t bytes = this->send(buf + offset, avail, timeout);
        offset += bytes;
        avail -= bytes;
    }

    return len;
}

/*----------------------------------------------------------------------------*/
/* Protected Overrideable Routines                                            */
/*----------------------------------------------------------------------------*/

void SerialPortAccessor::setup(struct termios *tio)
{
    // clear termios
    memset(tio, 0, sizeof(*tio));

    // 115200 8N1, no RTS/CTS, ignore modem control lines
    tio->c_cflag = B115200 | CS8 | CLOCAL | CREAD | HUPCL;
    tio->c_iflag = IGNPAR;
    tio->c_oflag = IGNPAR;

    /* set input mode (non-canonical, no echo, ...) */
    tio->c_lflag = NOFLSH;

    /* inter-character timer unused */
    tio->c_cc[VTIME] = 0;

    /* blocking read until 1 char has been rx'd */
    tio->c_cc[VMIN] = 1;
}

} // namespace environment
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 et tw=112: */
