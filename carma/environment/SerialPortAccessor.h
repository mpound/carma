/*
 * Simple Serial Port Accessor
 *
 * Copyright (c) 2011 Ira W. Snyder <iws@ovro.caltech.edu>
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#ifndef SERIAL_PORT_ACCESSOR_H
#define SERIAL_PORT_ACCESSOR_H

#include <string>
#include <fcntl.h>
#include <termios.h>

namespace carma {
namespace environment {

class SerialPortAccessor
{
    public:
        SerialPortAccessor(const std::string &device, const int mode = O_RDWR | O_NOCTTY | O_NONBLOCK);
        virtual ~SerialPortAccessor();

        /*
         * open and close the device
         *
         * These are "smart": they will ignore subsequent attempts to
         * open or close the device once it has reached the correct state
         */
        void open();
        void close();

        /* recv or send some bytes: timeout is for poll() */
        ssize_t recv(char *buf, size_t len, int timeout = 5000);
        ssize_t send(const char *buf, size_t len, int timeout = 5000);

        /* recv or send exactly len bytes: will block until all bytes are sent */
        ssize_t recv_n(char *buf, size_t len, int timeout = 5000);
        ssize_t send_n(const char *buf, size_t len, int timeout = 5000);

    protected:

        /*
         * setup the serial device
         *
         * This is automatically called during SerialPortAccessor::open(). It
         * should be overridden with the settings you wish to use.
         */
        virtual void setup(struct termios *tio);

    private:
        const std::string device_;
        const int mode_;
        int fd_;
};

} // namespace environment
} // namespace carma


#endif /* SERIAL_PORT_ACCESSOR_H */

/* vim: set ts=4 sts=4 sw=4 et tw=92: */
