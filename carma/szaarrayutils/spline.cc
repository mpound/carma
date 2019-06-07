#include <stdlib.h>
#include <math.h>
#include <string.h> /* memcpy */

#ifdef VXW
#include "carma/szaarrayutils/vxWorks.h"
#endif

#include "carma/szaarrayutils/lprintf.h"
#include "carma/szaarrayutils/spline.h"

/*
 * Compile the float version of the spline module.
 */
#undef SP_OBJ
#undef SP_TYPE
#define SP_OBJ FltSpline
#define SP_TYPE float

#include "carma/szaarrayutils/spline_template.h"

/*
 * Compile the double version of the spline module.
 */
#undef SP_OBJ
#undef SP_TYPE
#define SP_OBJ DblSpline
#define SP_TYPE double

#include "carma/szaarrayutils/spline_template.h"

