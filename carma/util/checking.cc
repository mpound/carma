#include "carma/util/checking.h"

#if CARMA_CHECKED_BUILD

#include <iostream>

namespace carma {
namespace util {

void
CheckingMessage( const char *   stringPrefix,
                 const char *   string,
                 const char *   stringSuffix,
                 const char *   fileName,
                 const long     lineNo ) {
    std::cerr   << "["
                << stringPrefix
                << string
                << stringSuffix
                << ", file: "
                << fileName
                << ", line: "
                << lineNo
                << "]\n"
                << std::flush;
}

}  // namespace checking
}  // namespace carma

#endif
