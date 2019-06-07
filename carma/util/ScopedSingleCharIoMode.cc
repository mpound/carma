#include "carma/util/ScopedSingleCharIoMode.h"

#include <termio.h>


namespace carma {
namespace util {


ScopedSingleCharIoMode::ScopedSingleCharIoMode( ) {
    // Get present state to start with
    ioctl( 0, TCGETA, &oldTermio_ );

    struct termio newTermio = oldTermio_;

    // Turn off the echo and canonical mode control bits in the struct.
    // ECHO and ICANON are single bit constants.
    newTermio.c_lflag &= ~(ECHO + ICANON); // No echos, non-canonical mode
    newTermio.c_cc[VMIN] = 1; // Read single chars at a time
    ioctl( 0, TCSETA, &newTermio );  // Set up terminal
}


ScopedSingleCharIoMode::~ScopedSingleCharIoMode( ) {
    ioctl( 0, TCSETA, &oldTermio_ );  // Restore the terminal
}


} // namespace carma::util
} // namespace carma
