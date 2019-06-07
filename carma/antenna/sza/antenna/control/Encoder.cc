#include <iostream>
#include <cmath>
#include <climits>

#include "carma/antenna/sza/antenna/control/Encoder.h"
#include "carma/antenna/sza/antenna/control/PmacAxis.h"

#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

#include "carma/szaarrayutils/szaconst.h"

using namespace sza::antenna::control;
using namespace sza::util;
using namespace std;

/**.......................................................................
 * Constructor sets the axis this object represents
 */
Encoder::Encoder(Axis::Type axis)
{
  axis_ = axis;
  reset();
}

/**.......................................................................
 * Reset internal members of this object
 */
void Encoder::reset()
{
  zero_            = 0.0;
  countsPerRadian_ = 0.0;
  countsPerTurn_   = 0;
  min_             = 0;
  max_             = 0;
  mountMin_        = 0;
  mountMax_        = 0;
  slewRate_        = 100;  // Full speed = 100% 
}

/**.......................................................................
 * Set the encoder count at the zero point of this axis
 */
void Encoder::setZero(double zero)
{
  zero_ = zero;
}

/**.......................................................................
 * Set the slew rate for this axis
 */
void Encoder::setSlewRate(long rate)
{
  std::cout << "setSlewRate: got a slew rate: " << std::endl;
  slewRate_ = rate;
}

/**.......................................................................
 * Convert from encoder counts to radians on the sky
 */
double Encoder::convertCountsToSky(int count)
{
  return count / countsPerRadian_ - zero_;
}

/**....................................................................... 
 * Set the counts per radian for this encoder.
 */
void Encoder::setCountsPerRadian(double countsPerRadian)
{
  countsPerRadian_ = countsPerRadian;
}

/**.......................................................................
 * Set the counts per turn for this encoder.
 */
void Encoder::setCountsPerTurn(int countsPerTurn)
{
  countsPerTurn_ = countsPerTurn;
}

/**.......................................................................
 * Update the mount limits.
 */
void Encoder::updateMountLimits()
{
  double emin, emax;

  emin = convertCountsToSky(min_);
  emax = convertCountsToSky(max_);

  // Record the new azimuth limits, noting that the min encoder value
  // will correspond to the maximum mount angle if the two coordinate
  // systems increase in opposite directions.

  if(emin < emax) {
    mountMin_ = emin;
    mountMax_ = emax;
  } else {
    mountMin_ = emax;
    mountMax_ = emin;
  };
}

/**.......................................................................
 * Install new encoder (count) limits
 */
void Encoder::setLimits(long min, long max)
{
  // Enforce min < max.

  if(min <= max) {
    min_ = min;
    max_ = max;
  } else {
    min_ = max;
    max_ = min;
  };
}

/**
 * Return the axis type of this encoder
 */
Axis::Type Encoder::getAxis()
{
  return axis_;
}

double Encoder::getMountMin()
{
  return mountMin_;
}

double Encoder::getMountMax()
{
  return mountMax_;
}

signed Encoder::getSlewRate()
{
  return slewRate_;
}

/**.......................................................................
 * Convert from Mount coordinates (radians, radians/sec) to encoder
 * counts and rates.
 */
void Encoder::convertMountToEncoder(double angle, double rate, 
				    PmacAxis* axis,
				    int current,
				    bool ignoreWrapLogic)
{
  signed enc_rate, enc_count;
  LogStream errStr;

  if(Debug::debugging(Debug::DEBUG4)) {
    cout << "mountToEncoder angle:   " << angle << endl;
    cout << "mountToEncoder rate:    " << rate << endl;
  }

  // Don't proceed if the encoder calibration is not present.  This
  // will cause an arithmetic exception below if it is zero.

  if(countsPerTurn_ == 0) {
    errStr.appendMessage(true, "Encoder counts per turn is 0.");
    throw Error(errStr);
  }

  // Convert the specified rate to encoder units * 1000.

  rate *= floor(countsPerRadian_ * 1000.0 + 0.5);
  /*
   * Limit the rate to the range of a signed long. Note that I use
   * -LONG_MAX in place of LONG_MIN because the VxWorks definition of
   * LONG_MIN is currently broken. It is defined as (-2147483648),
   * which translates to a unary minus operator followed by a number
   * that is > LONG_MAX, so gcc complains that it has to represent it
   * as an unsigned long, and that definately isn't what I want!
   */
  if(rate < -LONG_MAX)
    enc_rate = -LONG_MAX;
  else if(rate > LONG_MAX)
    enc_rate = LONG_MAX;
  else
    enc_rate = (long) rate;

  // Add in user-specified tracking offsets to the target position and
  // convert to encoder units.

#if 1
  double azMinDeg = ((double)(min_)/countsPerRadian_ - zero_) * 180/M_PI;
  double azMaxDeg = ((double)(max_)/countsPerRadian_ - zero_) * 180/M_PI;
  COUT("current = " << current << " zero = " << zero_/M_PI*180 << " degrees, min = " << azMinDeg << " max = " << azMaxDeg);
#endif

  enc_count = static_cast<long>
    (floor((zero_ + angle) * countsPerRadian_ + 0.5));

#if 1
  COUT("angle = " << angle << " rate = " << rate << " enc_count = " << enc_count);
#endif

  // Is the encoder limited to less than a turn?

  if(max_ - min_ < countsPerTurn_) {

    // Adjust the preliminary encoder count to within 1 turn forward
    // of the minimum available encoder position.

    int delta = enc_count - min_;
    if(delta < 0)
      enc_count += countsPerTurn_ * (-delta / countsPerTurn_ + 1);
    else if(delta > countsPerTurn_)
      enc_count -= countsPerTurn_ * (delta / countsPerTurn_);

    // If the resulting encoder position is not in the available
    // range, substitute the count of the nearest encoder limit.

    if(enc_count > max_) {
      if(enc_count - max_ < min_ - (enc_count - countsPerTurn_))
	enc_count = max_;
      else
	enc_count = min_;
    };
  } else {
    /**
     * If the encoder limits cover a turn or more, then the requested
     * position can always be reached. In fact, because of the overlap
     * regions of the drive, a given angle may correspond to more than one
     * encoder position. We want to choose the one that is closest to the
     * current position of the drive. Start by determining the distance
     * between our preliminary encoder position and the current
     * encoder position on this axis.
     */
    int delta = enc_count - current;

    // Adjust the preliminary encoder count to within 1 turn forward
    // of the current encoder position.

    if(delta < 0)
      enc_count += countsPerTurn_ * (-delta / countsPerTurn_ + 1);
    else if(delta > countsPerTurn_)
      enc_count -= countsPerTurn_ * (delta / countsPerTurn_);

    // If the equivalent encoder count behind the current encoder
    // position is closer to the current encoder position, use it
    // instead.

    if(enc_count - current > countsPerTurn_/2)
      enc_count -= countsPerTurn_;

    // If the encoder count exceeds one of the limits, adjust it by a turn.

    if(enc_count < min_)
      enc_count += countsPerTurn_;
    else if(enc_count > max_)
      enc_count -= countsPerTurn_;
  };

  // If an explicit wrap mode was requested, assert it now, if it
  // doesn't cause us to exceed any limits

  COUT("WrapMode = " << wrapMode_ << " ignore = " << ignoreWrapLogic);

  if(!ignoreWrapLogic) {
    switch (wrapMode_) {
    case WrapMode::ADD:
      if(enc_count + countsPerTurn_ < max_) {
	COUT("Commanding " <<  (enc_count + countsPerTurn_) << " instead of " << enc_count << " wrap mode add");
	enc_count += countsPerTurn_;
      } else {
	COUT("Can't command " <<  (enc_count + countsPerTurn_) << " because it would exceed max = " << max_ << " commanding " << enc_count);
      }
      break;
    case WrapMode::SUBTRACT:
      if(enc_count - countsPerTurn_ > min_) {
	COUT("Commanding " <<  (enc_count - countsPerTurn_) << " instead of " << enc_count << " wrap mode subtract");
	enc_count -= countsPerTurn_;
      } else {
	COUT("Can't command " <<  (enc_count - countsPerTurn_) << " because it would be below min = " << min_ << " commanding " << enc_count);
      }
      break;
    default:
      break;
    }
  }

  // Install the newly computed values in the axis object

  axis->setRate(enc_rate);
  axis->setCount(enc_count);
}

/**.......................................................................
 * Pack encoder limits for archival in the register database
 */
void Encoder::packLimits(signed* s_elements)
{
  s_elements[0] = static_cast<signed>(mountMin_ * rtomas);
  s_elements[1] = static_cast<signed>(mountMax_ * rtomas);
}

/**.......................................................................
 * Pack encoder zero points for archival in the register database.
 */
void Encoder::packZero(signed* s_elements)
{
  *s_elements = static_cast<signed>(zero_ * rtomas);
}

/**.......................................................................
 * Pack this encoder multiplier for archival in the register database.
 */
void Encoder::packCountsPerTurn(signed* s_elements)
{
  *s_elements = static_cast<signed>(countsPerTurn_);
}

/**.......................................................................
 * Set the wrap mode
 */
void Encoder::setWrapMode(WrapMode::Mode wrapMode)
{
  wrapMode_ = (axis_ == Axis::AZ) ? wrapMode : WrapMode::NONE;
}

/**.......................................................................
 * Get the wrap mode
 */
WrapMode::Mode Encoder::getWrapMode()
{
  return wrapMode_;
}


/**.......................................................................
 * Get the wrap mode
 */
string Encoder::getWrapModeString()
{
  if(wrapMode_ == WrapMode::NONE) return "NONE";
  if(wrapMode_ == WrapMode::ADD) return "ADD";
  if(wrapMode_ == WrapMode::SUBTRACT) return "SUBTRACT";
  return "UNKNOWN";
}




