#ifndef CARMA_UTIL_SCOPED_SINGLE_CHAR_IO_MODE_H
#define CARMA_UTIL_SCOPED_SINGLE_CHAR_IO_MODE_H

#include <termio.h>

namespace carma {
namespace util {


class ScopedSingleCharIoMode {
    public:
        explicit ScopedSingleCharIoMode( );

        /* virtual */ ~ScopedSingleCharIoMode( );

    private:
        // No copying
        ScopedSingleCharIoMode( const ScopedSingleCharIoMode & );
        ScopedSingleCharIoMode & operator=( const ScopedSingleCharIoMode & );

        struct ::termio oldTermio_;
};


} // namespace carma::util
} // namespace carma

#endif
