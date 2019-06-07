#include <cmath>

#include "carma/szautil/Exception.h"
#include "carma/szautil/Debug.h"

#include "carma/antenna/sza/antenna/control/Model.h"

#include "carma/szaarrayutils/szaconst.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::util;

/**.......................................................................
 * Constructor simply intializes each of the encoders
 */
Model::Model() : 
  az_(Axis::AZ), el_(Axis::EL), pa_(Axis::PA) 
{
  reset();
}

/**.......................................................................
 * Destructor.
 */
Model::~Model() {}

/**.......................................................................
 * Reset internal data members.
 */
void Model::reset()
{
  // Reset the encoder calibrations

  az_.reset();
  el_.reset();

  // Reset the tilts

  azt_.reset();
  elt_.reset();

  // Reset the collimation

  opticalCollimation_.reset();
  radioCollimation_.reset();
  currentCollimation_ = &radioCollimation_;

  // Reset the flexure

  opticalFlexure_.reset();
  radioFlexure_.reset();
  currentFlexure_ = &radioFlexure_;
}

/**.......................................................................
 * Return a pointer to the requested collimation container
 */
sza::antenna::control::Collimation* 
Model::Collimation(PointingMode::Type mode)
{
  switch (mode) {
  case PointingMode::OPTICAL:
    return &opticalCollimation_;
    break;
  case PointingMode::RADIO:
    return &radioCollimation_;
    break;
  case PointingMode::CURRENT:
    return currentCollimation_;
    break;
  default:
    throw Error("Model::Collimation: Unrecognized collimation mode.\n");
    break;
  }
}

/**.......................................................................
 * Return a pointer to the current collimation model
 */
sza::antenna::control::Collimation* Model::currentCollimation()
{
  return currentCollimation_;
}

/**.......................................................................
 * Return a pointer to the requested flexure container
 */
sza::antenna::control::Flexure* 
Model::Flexure(PointingMode::Type mode)
{
  switch (mode) {
  case PointingMode::OPTICAL:
    return &opticalFlexure_;
    break;
  case PointingMode::RADIO:
    return &radioFlexure_;
    break;
  default:
    throw Error("Model::Flexure: Unrecognized flexure mode.\n");
    break;
  }
}

/**.......................................................................
 * Return a pointer to the current flexure model
 */
sza::antenna::control::Flexure* Model::currentFlexure()
{
  return currentFlexure_;
}

/**.......................................................................
 * Return a pointer to the requested encoder container
 */
sza::antenna::control::Encoder* 
Model::Encoder(Axis::Type axis)
{
  switch (axis) {
  case Axis::AZ:
    return &az_;
    break;
  case Axis::EL:
    return &el_;
    break;
  case Axis::PA:
    return &pa_;
    break;
  default:
    throw Error("Model::Encoder: Unrecognized encoder axis.\n");
    break;
  }
}

/**.......................................................................
 * Compute and store the new mount limits as angles on the sky
 */
void Model::updateMountLimits()
{
  az_.updateMountLimits();
  el_.updateMountLimits();
}

/**.......................................................................
 * Return true if the passed collimation container is the current one.
 */
bool Model::isCurrent(sza::antenna::control::Collimation* collim)
{
  return collim == currentCollimation_;
}

/**.......................................................................
 * Return true if the passed flexure container is the current one.
 */
bool Model::isCurrent(sza::antenna::control::Flexure* flexure)
{
  return flexure == currentFlexure_;
}

/**.......................................................................
 * Set which pointing mode (optical or radio) is the current
 * collimation mode).
 */
void Model::setCurrentCollimation(PointingMode::Type mode)
{
  switch (mode) {
  case PointingMode::OPTICAL:
    currentCollimation_ = &opticalCollimation_;
    break;
  case PointingMode::RADIO:
    currentCollimation_ = &radioCollimation_;
    break;
  default:
    ErrorDef(err, "Model::setCurrentCollimation: Invalid collimation mode.\n");
    break;
  }
}

/**.......................................................................
 * Set which pointing mode (optical or radio) is the current
 * flexure mode).
 */
void Model::setCurrentFlexure(PointingMode::Type mode)
{
  switch (mode) {
  case PointingMode::OPTICAL:
    currentFlexure_ = &opticalFlexure_;
    break;
  case PointingMode::RADIO:
    currentFlexure_ = &radioFlexure_;
    break;
  default:
    ErrorDef(err, "Model::setCurrentFlexure: Invalid flexure mode.\n");
    break;
  }
}

/**.......................................................................
 * Adjust the elevation to account for telescope flexure.
 *
 * Input/Output:
 *  f    PointingCorrections *  The elevation pointing to be corrected.
 */
void Model::applyFlexure(PointingCorrections* f)
{
  currentFlexure_->apply(f);
}

/**.......................................................................
 * Correct the collimation of the telescope.
 *
 * Input/Output:
 *  f       PointingCorrections *  The az/el pointing to be corrected.
 */
void Model::applyCollimation(PointingCorrections* f)
{
  currentCollimation_->apply(f);
}

/**.......................................................................
 * Pack the zero points for encoders managed by this object
 */
void Model::packEncoderZeros(signed* s_elements) 
{
  az_.packZero(s_elements);
  el_.packZero(s_elements+1);
}

/**.......................................................................
 * Pack the multipliers for encoders managed by this object.
 */
void Model::packEncoderMultipliers(signed* s_elements)
{
  az_.packCountsPerTurn(s_elements);
  el_.packCountsPerTurn(s_elements+1);
}

/**.......................................................................
 * Pack the tilts managed by this object.
 */
void Model::packTilts(signed* s_elements)
{
  azt_.packHaTilt(s_elements);
  azt_.packLatTilt(s_elements+1);
  elt_.packTilt(s_elements+2);
}

/**.......................................................................
 * Pack the flexure term managed by this object.
 */
void Model::packFlexure(signed* s_elements)
{
  DBPRINT(true, Debug::DEBUG3, "Packing flexure");
  currentFlexure_->pack(s_elements);
}

/**.......................................................................
 * Pack which collimation mode is the current one.
 */
void Model::packCollimationMode(unsigned* u_elements)
{
  u_elements[0] = (currentCollimation_ == &radioCollimation_);
}

/**.......................................................................
 * Pack the current collimation correction.
 */
void Model::packCollimation(signed* s_elements)
{
  currentCollimation_->pack(s_elements);
}

/**.......................................................................
 * Return a pointer to the requested collimation container
 */
sza::antenna::control::AxisTilt* 
Model::AxisTilt(Axis::Type axis)
{
  switch (axis) {
  case Axis::AZ:
    return &azt_;
    break;
  case Axis::EL:
    return &elt_;
    break;
  default:
    throw Error("Model::AxisTilt: Unrecognized axis.\n");
    break;
  }
}


