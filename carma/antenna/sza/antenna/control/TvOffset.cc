#include <cmath>

#include "carma/szautil/Debug.h"
#include "carma/antenna/sza/antenna/control/TvOffset.h"
#include "carma/antenna/sza/antenna/control/Pointing.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::util;

/**.......................................................................
 * Constructor just calls reset
 */
TvOffset::TvOffset()
{
  reset();
}

/**.......................................................................
 * Reset the offsets
 */
void TvOffset::reset()
{
  zero_angle_ = 0.0;
  up_         = 0.0;
  right_      = 0.0;
  pending_    = false;
  seq_        = 0;
}

/**.......................................................................
 * Set the offset
 */
void TvOffset::set(OffsetMsg msg)
{
  
  // Add the offsets to any that haven't yet been translated into az
  // and el offsets.

  up_    += msg.body.tv.up;
  right_ += msg.body.tv.right;
  
  // And set the pending flag and sequence number of the pending
  // transaction

  DBPRINT(true, Debug::DEBUG3, "TV offsets are now: " << up_ << " " << right_);
  //  pending_ = true;
  //  seq_ = seq;
}

/**.......................................................................
 * Set the zero-point angle.
 */
void TvOffset::setAngle(double zero_angle)
{
  zero_angle_ = zero_angle;
}

/**.......................................................................
 * Add in the Tv offsets to the pointing corrections
 */
void TvOffset::apply(PointingCorrections* f, double* daz, double* del) 
{
  double dk = 0.0;
  *daz = 0.0;
  *del = 0.0;
  
  // Is there a new correction to add?

  if(up_ != 0.0 || right_ != 0.0) {
    
    // Convert from the requested cartesian offsets of the image on
    // the tv to the equivalent polar offsets to be applied to the
    // telescope. Make the origin of the polar angle the local
    // vertical of the telescope.
    //
    // Note that the optical telescope inverts the image, and of
    // course, that the sky appears to move in the opposite direction
    // to the telescope. These two effects cancel each other.

    double angle = atan2(right_, up_) + dk - zero_angle_;
    double cos_angle = cos(angle);
    double sin_angle = sin(angle);
    double size = sqrt(right_ * right_ + up_ * up_);
    double cos_size = cos(size);
    double sin_size = sin(size);
    
    // It is conceivably possible that although tv->right and tv->up
    // have both been checked to be non-zero, if they are at the limit
    // of the machine precision, sin_size might be zero. This would
    // produce a divide by zero error below, so trap it.

    if(sin_size != 0.0) {
      
      // Now convert the offsets into az and el offsets.

      double offset_el = asin(cos_size * f->sin_el +
			      sin_size * f->cos_el * cos_angle);

      *del = offset_el - f->el;

      double tmp = f->cos_el * cos_size/sin_size - f->sin_el * cos_angle;

      *daz = (sin_angle==0.0 && tmp==0.0) ? 0.0 : atan2(sin_angle, tmp);
    };
    
    // Start accumulating new TV offsets from the user.

    up_ = 0.0;
    right_ = 0.0;
  };
}

/**.......................................................................
 * Add in the Tv offsets to the pointing corrections
 */
void TvOffset::apply(Pointing* p, double* daz, double* del) 
{
  double dk = 0.0;
  *daz = 0.0;
  *del = 0.0;
  
  // Is there a new correction to add?

  if(up_ != 0.0 || right_ != 0.0) {
    
    // Convert from the requested cartesian offsets of the image on
    // the tv to the equivalent polar offsets to be applied to the
    // telescope. Make the origin of the polar angle the local
    // vertical of the telescope.
    //
    // Note that the optical telescope inverts the image, and of
    // course, that the sky appears to move in the opposite direction
    // to the telescope. These two effects cancel each other.

    double angle = atan2(right_, up_) + dk - zero_angle_;
    double cos_angle = cos(angle);
    double sin_angle = sin(angle);
    double size = sqrt(right_ * right_ + up_ * up_);
    double cos_size = cos(size);
    double sin_size = sin(size);
    
    // It is conceivably possible that although tv->right and tv->up
    // have both been checked to be non-zero, if they are at the limit
    // of the machine precision, sin_size might be zero. This would
    // produce a divide by zero error below, so trap it.

    Position* mount = p->Position(Pointing::MOUNT_ANGLES);

    double sin_el = sin(mount->el_);
    double cos_el = cos(mount->el_);

    if(sin_size != 0.0) {
      
      // Now convert the offsets into az and el offsets.

      double offset_el = asin(cos_size * sin_el +
			      sin_size * cos_el * cos_angle);

      *del = offset_el - mount->el_;

      double tmp = cos_el * cos_size/sin_size - sin_el * cos_angle;

      *daz = (sin_angle==0.0 && tmp==0.0) ? 0.0 : atan2(sin_angle, tmp);
    };
    
    // Start accumulating new TV offsets from the user.

    up_ = 0.0;
    right_ = 0.0;
  };
}
