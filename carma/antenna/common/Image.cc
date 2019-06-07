#include "carma/antenna/common/Image.h"
#include "carma/util/ErrorException.h"

using namespace carma::antenna::common;

using namespace cimg_library;


CImgStats::CImgStats( const CImg< double > & stats )
{
    if ( stats.size() != 12 ) 
        throw CARMA_ERROR( "CImg.size() != 12" );

    min = stats[0];
    max = stats[1];
    mean = stats[2];
    variance = stats[3];
    xmin = stats[4];
    ymin = stats[5];
    zmin = stats[6];
    cmin = stats[7];
    xmax = stats[8];
    ymax = stats[9];
    zmax = stats[10];
    cmax = stats[11];
}
