#ifndef CARMA_ANTENNA_COMMON_IMAGE_H
#define CARMA_ANTENNA_COMMON_IMAGE_H

#include <CImg.h>

#include "carma/antenna/common/Image_fwd.h"

namespace carma {
namespace antenna {
namespace common {

    /** 
     * Typedef for raw framegrabber image.  
     */
    typedef cimg_library::CImg< unsigned char > RawImage;

    struct CImgStats {

        CImgStats( const cimg_library::CImg< double > & stats);

        double min;
        double max;
        double mean;
        double variance;
        double xmin;
        double ymin;
        double zmin;
        double cmin;
        double xmax;
        double ymax;
        double zmax;
        double cmax;
    };

}}}
#endif
