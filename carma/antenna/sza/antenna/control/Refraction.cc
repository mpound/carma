#include <cmath>
#include <iostream>

#include "carma/szautil/Debug.h"

#include "carma/antenna/sza/antenna/control/Refraction.h"

using namespace std;
using namespace sza::util;
using namespace sza::antenna::control;

/**.......................................................................
 * Constructor just calls initialization method
 */
Refraction::Refraction()
{
  reset();
}

/**.......................................................................
 * Reset our data members to something sensible
 */
void Refraction::reset()
{
  a_      = 0.0;
  b_      = 0.0;
  usable_ = false;
}

/*.......................................................................
 * Compute the offset to add to the elevation to correct for atmospheric
 * refraction.
 *
 * Input/Output:
 *  f    PointingCorrections *  The pointing to be corrected.
 *
 * Output:
 *  inc       double *  The applied offset (radians) will be
 *                      assigned to *inc.
 */
double Refraction::apply(PointingCorrections* f)
{
  // Get local copies of cached terms.

  double cos_el  = f->cos_el;
  double sin_el  = f->sin_el;
  double sin2_el = sin_el  * sin_el;
  double sin3_el = sin2_el * sin_el;
  double sin4_el = sin2_el * sin2_el;
  double cos2_el = cos_el  * cos_el;
  double cos3_el = cos2_el * cos_el;
  double inc;

  // We can only correct sources above the horizon.

  if(f->el > 0.0) {

    // Use the equation suggested in the starlink user notes for
    // slalib (sun67), suitably modified for use with elevation
    // instead of zenith angle, and expanded to use just sin() and
    // cos() instead of tan() and sin().

    DBPRINT(true, Debug::DEBUG9, "Applying refraction: a = " 
	    << a_ << " b = " << b_);

    inc = (a_ * cos_el * sin3_el + b_ * sin_el * cos3_el) /
      (sin4_el + (a_ * sin2_el + 3.0 * b_ * cos2_el));

    f->el += inc;
    f->sin_el = sin(f->el);
    f->cos_el = cos(f->el);

  } else {
    inc = 0.0;
  };
  
  return inc;
}

/**.......................................................................
 * Record the refraction parameters in micro-arcseconds.
 */
void Refraction::pack(signed* s_elements)
{
  s_elements[0] = static_cast<signed>(a_ * rtomas * 1000.0);
  s_elements[1] = static_cast<signed>(b_ * rtomas * 1000.0);
}
