#ifndef CARMA_ANTENNA_OVRO_LOGGINGUTILS_H
#define CARMA_ANTENNA_OVRO_LOGGINGUTILS_H

#include <iosfwd>

namespace carma {
namespace antenna {

namespace common {

    class RxTypeInfo;

    void logInfoWithRxNdc( const carma::antenna::common::RxTypeInfo & info,
                           const ::std::string & msg );

} // namespace common
} // namespace antenna
} // namespace carma
#endif
