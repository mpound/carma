#ifndef CARMA_ANTENNA_COMMON_IMAGEFWD_H
#define CARMA_ANTENNA_COMMON_IMAGEFWD_H

namespace cimg_library {
    template< typename T > class CImg;
}

namespace carma {
namespace antenna {
namespace common {

    typedef ::cimg_library::CImg< float > Image;

}}} // namespace carma::antenna::common
#endif
