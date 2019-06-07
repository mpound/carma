#include "carma/antenna/sza/antenna/control/PointingCorrections.h"

using namespace std;
using namespace sza::antenna::control;

/**.......................................................................
 * Constructor.
 */
PointingCorrections::PointingCorrections()
{
  az      = 0.0; // The apparent azimuth of the source 
  el      = 0.0; // The apparent elevation of the source 
  pa      = 0.0; // The apparent parallactic angle of the source 
  lat     = 0.0; // The corrected latitude of the source 
  sin_az  = 0.0; // sin(az) 
  cos_az  = 0.0; // cos(az) 
  sin_el  = 0.0; // sin(el) 
  cos_el  = 0.0; // cos(el) 
  sin_lat = 0.0; // sin(lat) 
  cos_lat = 0.0; // cos(lat) 
}
