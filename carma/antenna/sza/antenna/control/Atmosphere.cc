#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"

#include "carma/antenna/sza/antenna/control/Atmosphere.h"

using namespace sza::antenna::control;
using namespace sza::util;

/**.......................................................................
 * Constructor
 */
Atmosphere::Atmosphere()
{
  // Just initialize the current model to be the radio model.
  // Internal refraction members get initialized by their own
  // constructors.

  currentRefraction_ = &radio_;
}

/**.......................................................................
 * Reset the data members managed by this object
 */
void Atmosphere::reset()
{
  // Reset the refraction internal members

  radio_.reset();
  optical_.reset();

  // Initialize the current model to be the radio

  currentRefraction_ = &radio_;
}

/**.......................................................................
 * Return a pointer to the requested refraction container
 */
sza::antenna::control::Refraction* 
Atmosphere::Refraction(PointingMode::Type mode)
{
  switch (mode) {
  case PointingMode::OPTICAL:

    DBPRINT(true, Debug::DEBUG9, "Getting optical refraction (a=" 
	    << optical_.getA()
	    << " , b= " << optical_.getB());

    DBPRINT(true, Debug::DEBUG9, "Current refraction (a=" 
	    << currentRefraction_->getA()
	    << " , b= " << currentRefraction_->getB());

    return &optical_;
    break;
  case PointingMode::RADIO:
    DBPRINT(true, Debug::DEBUG9, "Getting radio refraction (a=" 
	    << radio_.getA()
	    << " , b= " << radio_.getB());

    DBPRINT(true, Debug::DEBUG9, "Current refraction (a=" 
	    << currentRefraction_->getA()
	    << " , b= " << currentRefraction_->getB());

    return &radio_;
    break;
  case PointingMode::CURRENT:
    return currentRefraction_;
    break;
  default:
    throw Error("Atmosphere::Refraction: Unrecognized refraction mode.\n");
    break;
  }
}

/**.......................................................................
 * Select which refraction model is the one to be used
 */
void Atmosphere::setCurrentRefraction(PointingMode::Type mode)
{
  switch (mode) {
  case PointingMode::OPTICAL:
    currentRefraction_ = &optical_;
    break;
  case PointingMode::RADIO:
    currentRefraction_ = &radio_;
    break;
  default:
    ErrorDef(err,"Atmosphere::setCurrentRefraction: "
	     "Invalid refraction mode.\n");
    break;
  }
}

/**.......................................................................
 * Return a pointer to the requested refraction container
 */
bool Atmosphere::isCurrent(sza::antenna::control::Refraction* r)
{
  return (r == currentRefraction_);
}

/**.......................................................................
 * Return a pointer to the current refraction
 */
sza::antenna::control::Refraction* Atmosphere::currentRefraction()
{
  return currentRefraction_;
}

/**.......................................................................
 * Apply the current refraction correction to the pointing corrections
 */
double Atmosphere::applyRefraction(PointingCorrections* f)
{
  return currentRefraction_->apply(f);
}
