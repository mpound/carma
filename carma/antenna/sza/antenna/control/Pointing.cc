#include <cstring>
#include <cmath>

#include "carma/antenna/sza/antenna/control/Pointing.h"

#include "carma/szaarrayutils/szaconst.h"

using namespace std;
using namespace sza::util;
using namespace sza::antenna::control;

/**.......................................................................
 * Constructor just calls reset()
 */
Pointing::Pointing()
{
  reset();
}

/**.......................................................................
 * Initialize data members to something sensible
 */
void Pointing::reset()
{
  name_[0]    = '\0';
  mjd_        = 0;
  sec_        = 0;
  ra_         = 0.0;   // Zero the pointing flow parameters. (these are
		    // unknown for slews)
  dec_        = 0.0;
  dist_       = 0.0;
  refraction_ = 0.0;

  geocentric_.reset();
  topocentric_.reset();
  
  // pmac_new_position() will fill in the position fields with the
  // reported location of the telescope.

  mountAngles_.reset();
  
  // We want the drives to stop when they reach the target position.

  mountRates_.reset();
  
  // Tell pmac_new_position() not to move any of the axes.

  axes_ = Axis::NONE;

  isFixed_ = false;
  isHalt_  = false;
  currentPositionIsSet_ = false;
  mode_ = sza::util::PmacMode::DEFAULT;
}

/**.......................................................................
 * Install the name of the source
 */
void Pointing::setName(char* name)
{
  strncpy(name_, name, SRC_LEN);
  name_[SRC_LEN-1] = '\0';
}

/**.......................................................................
 * Install the name of the source
 */
unsigned char* Pointing::getName()
{
  return (unsigned char*)name_;
}

/**.......................................................................
 * Record the current time as days and seconds of UTC
 */
void Pointing::setTime(double utc)
{
  mjd_ = static_cast<int>(floor(utc));
  sec_ = static_cast<int>(ceil((utc - mjd_) * daysec));
}

/**.......................................................................
 * Record the current time as days and seconds of UTC
 */
void Pointing::setTime(int mjd, int sec)
{
  mjd_ = mjd;
  sec_ = sec;
}

/**.......................................................................
 * Install the angles to which the axes will be driven.
 */
void Pointing::setAngles(double az, double el, double pa)
{
  mountAngles_.az_ = az;
  mountAngles_.el_ = el;
  mountAngles_.pa_ = pa;
}

/**.......................................................................
 * Install the rates with which the axes will be driven.
 */
void Pointing::setRates(double az, double el, double pa)
{
  mountRates_.az_ = az;
  mountRates_.el_ = el;
  mountRates_.pa_ = pa;
}

/**.......................................................................
 * Install the set of axes to drive.
 */
void Pointing::setAxes(Axis::Type axes)
{
  axes_ = axes;
}

/**.......................................................................
 * Return the set of axes to drive.
 */
Axis::Type Pointing::getAxes()
{
  return axes_;
}

/**.......................................................................
 * Set the RA of the source.
 */
void Pointing::setRa(double ra)
{
  ra_ = ra;
}

/**.......................................................................
 * Set the DEC of the source.
 */
void Pointing::setDec(double dec)
{
  dec_ = dec;
}

/**.......................................................................
 * Set the distance to the source.
 */
void Pointing::setDist(double dist)
{
  dist_ = dist;
}

/**.......................................................................
 * Set the refraction correction.
 */
void Pointing::setRefraction(double refraction)
{ 
  refraction_ = refraction;
}

/**
 * A public method to convert from mount angle to encoder counts
 */
void Pointing::convertMountToEncoder(Encoder* encoder, 
				     PmacAxis* axis,
				     int current,
				     bool ignoreWrapLogic)
{
  switch (encoder->getAxis()) {
  case Axis::AZ:
    encoder->convertMountToEncoder(mountAngles_.az_, mountRates_.az_, axis, 
				   current, ignoreWrapLogic);
    break;
  case Axis::EL:
    encoder->convertMountToEncoder(mountAngles_.el_, mountRates_.el_, axis,
				   current, ignoreWrapLogic);
    break;
  case Axis::PA:
    encoder->convertMountToEncoder(mountAngles_.pa_, mountRates_.pa_, axis,
				   current, ignoreWrapLogic);
    break;
  default:
    ErrorDef(err, "Pointing::convertMountToEncoder: Unrecognized axis.\n");
    break;
  }
}

/**.......................................................................
 * Return true if the axis mask includes the requested axis
 */
bool Pointing::includesAxis(Axis::Type axis)
{
  return axes_ & axis;
}

/**.......................................................................
 * Return a pointer to the requested Position object
 */
sza::antenna::control::Position* Pointing::Position(PositionType type)
{
  switch (type) {
  case Pointing::MOUNT_ANGLES:
    return &mountAngles_;
    break;
  case Pointing::MOUNT_RATES:
    return &mountRates_;
    break;
  case Pointing::TOPOCENTRIC:
    return &topocentric_;
    break;
  case Pointing::GEOCENTRIC:
    return &geocentric_;
    break;
  default:
    throw Error("Pointing::Position: Unrecognized position type.\n");
    break;
  }
}

/*.......................................................................
 * Compute the geocentric azimuth and elevation of the source.
 *
 * Input:
 *  lst      double   The local apparent sidereal time.
 *
 * Input/Output:
 *  f   PointingCorrections * On input the sin_lat and cos_lat members must have
 *                    been initialized. On output the az,el,sin_az,cos_az,
 *                    sin_el,cos_el terms will have been initialized.
 */
void Pointing::computeGeocentricPosition(double lst, PointingCorrections *f)
{
  // Compute the hour angle of the source.

  double ha = lst - ra_;
  double sin_ha = sin(ha);
  double cos_ha = cos(ha);

  // Precompute trig terms.

  double cos_dec = cos(dec_);
  double sin_dec = sin(dec_);

  // Evaluate pertinent spherical trig equations.

  double cos_el_cos_az = sin_dec * f->cos_lat - cos_dec * f->sin_lat * cos_ha;
  double cos_el_sin_az = -cos_dec * sin_ha;
  double cos2_el=cos_el_cos_az * cos_el_cos_az + cos_el_sin_az * cos_el_sin_az;
  double cos_el_sin_pa = sin_ha * f->cos_lat;
  double cos_el_cos_pa = f->sin_lat * cos_dec - sin_dec * f->cos_lat * cos_ha;

  // Get the geocentric elevation and its trig forms from the above equations.

  f->cos_el = sqrt(cos2_el);
  f->sin_el = sin_dec * f->sin_lat + cos_dec * f->cos_lat * cos_ha;
  f->el = asin(f->sin_el);

  // Compute the geocentric azimuth.

  f->az = (cos_el_cos_az==0.0 && cos_el_sin_az==0.0) ?
    0.0 : wrap2pi(atan2(cos_el_sin_az, cos_el_cos_az));
  f->sin_az = sin(f->az);
  f->cos_az = cos(f->az);

  // Compute the parallactic angle.

  f->pa = (cos_el_cos_pa==0.0 && cos_el_sin_pa==0.0) ?
    0.0 : wrapPi(atan2(cos_el_sin_pa, cos_el_cos_pa));
  
  // Install these in our geocentric container.

  geocentric_.set(Axis::AZ, f->az);
  geocentric_.set(Axis::EL, f->el);
  geocentric_.set(Axis::PA, f->pa);
}

/**.......................................................................
 * Pack the UTC for archival into the register database
 */
void Pointing::packUtc(unsigned* u_elements) 
{
  u_elements[0] = mjd_;
  u_elements[1] = sec_ * 1000;
}

/**.......................................................................
 * Return the current UTC.
 */
double Pointing::getUtc()
{
  return static_cast<double>(mjd_) + static_cast<double>(sec_) / daysec;
}

/**.......................................................................
 * Return the current UTC.
 */
RegDate Pointing::getDate()
{
  RegDate date(mjd_, sec_ * 1000);
  return date;
}

/**.......................................................................
 * Return the applied refraction correction.
 */
double Pointing::getRefraction()
{
  return refraction_;
}

/**.......................................................................
 * Pack the source name for archival in the register database.
 */
void Pointing::packSourceName(unsigned* u_elements, int nel)
{
  if(pack_int_string(name_, nel, u_elements))
    throw Error("Pointing::packSourceName: Error in pack_int_string().\n");
}

/**.......................................................................
 * Pack the equatorial geocentric position.
 */
void Pointing::packEquatGeoc(signed* s_elements)
{
  s_elements[0] = static_cast<signed>(ra_   * rtomas);
  s_elements[1] = static_cast<signed>(dec_  * rtomas);
  s_elements[2] = static_cast<signed>(dist_ * 1.0e6);  // Micro-AU
}

/**.......................................................................
 * Pack geocentric horizon coordinates.
 */
void Pointing::packHorizGeoc(signed* s_elements)
{
  geocentric_.pack(s_elements);
}

/**.......................................................................
 * Pack topocentric horizon coordinates.
 */
void Pointing::packHorizTopo(signed* s_elements)
{
  topocentric_.pack(s_elements);
}

/**.......................................................................
 * Pack horizon mount coordinates.
 */
void Pointing::packHorizMount(signed* s_elements)
{
  mountAngles_.pack(s_elements);
}

/**.......................................................................
 * Set up pointing for a new halt command
 */
void Pointing::setupForHalt(SzaShare* share)
{
  // Reset all pointing parameters

  reset();

  // Record a blank source name

  setName("(none)");

  // Give the slew the timestamp of the next 1-second boundary. This
  // is when it will be submitted to the pmac.

  double utc = share->getUtc();

  setTime(utc);

  isFixed_ = true;
  isHalt_  = true;
}
/**.......................................................................
 * Set up pointing in preparation for a pmac reboot
 */
void Pointing::setupForReboot(SzaShare* share)
{
  // Reset all pointing parameters

  reset();
  
  // Record a blank source name.

  setName("(none)");
  
  // Give the slew the timestamp of the next 1-second boundary. This
  // is when it will be submitted to the pmac.
  
  double  utc = share->getUtc();

  setTime(utc);

  isFixed_ = true;

  mode_ = sza::util::PmacMode::REBOOT;
}

/**.......................................................................
 * Set up pointing for a new slew command
 */
void Pointing::setupForSlew(SzaShare* share_, TrackerMsg* msg)
{
  
  // Zero the pointing flow parameters. These are unknown for slews.

  reset();
  
  // Record the source name.

  setName(msg->body.slew.source);
  
  // Give the slew the timestamp of the next 1-second boundary. This
  // is when it will be submitted to the pmac.

  double utc = share_->getUtc();
  setTime(utc);
  
  // Record the new positions.

  setAngles(msg->body.slew.az, msg->body.slew.el, 
	    msg->body.slew.pa);
  
  // We want the drives to stop when they reach the target position.

  setRates(0.0, 0.0, 0.0);
  
  // Which axes are to slewed?
  
  setAxes(msg->body.slew.axes);

  isFixed_ = true;
}

/**.......................................................................
 * Set up pointing for a track command
 */
void Pointing::setupForTrack()
{
  isFixed_ = false;
}

/**.......................................................................
 * Round an angle into the range -pi..pi.
 *
 * Input:
 *  angle    double   The angle to be rounded (radians).
 *
 * Output:
 *  return   double   An angle in the range:  -pi <= angle < pi.
 */
double Pointing::wrapPi(double angle)
{
  double a = wrap2pi(angle);
  return a < pi ? a : (a-twopi);
}

/**.......................................................................
 * Round an angle into the range 0-2.pi. Note that fmod() is not used
 * because it was found that the version of fmod() that comes with the
 * 68060 and other Motorola CPU's is implemented as a while loop which
 * takes seconds (!) to finish if the divisor is much smaller than the
 * numerator.
 *
 * Input:
 *  angle    double   The angle to be rounded (radians).
 * Output:
 *  return   double   An angle in the range:  0 <= angle < 2.pi.
 */
double Pointing::wrap2pi(double angle)
{
  return (angle >= 0.0) ? (angle - twopi * floor(angle/twopi)) :
			  (angle + twopi * ceil(-angle/twopi));
}

/**.......................................................................
 * Return true if this is a fixed source
 */
bool Pointing::isFixed()
{
  return isFixed_;
}

/**.......................................................................
 * Return true if this is a halt
 */
bool Pointing::isHalt()
{
  return isHalt_;
}

/**.......................................................................
 * Store the current position
 */
void Pointing::setCurrentPosition(AxisPositions& axes)
{
  // If the current position hasn't already been set, store the
  // current position

  if(!currentPositionIsSet_) {
    currentPosition_ = axes;
    currentPositionIsSet_ = true;
  }
}

