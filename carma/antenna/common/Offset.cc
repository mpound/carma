// $Id: Offset.cc,v 1.3 2007/06/28 05:03:42 abeard Exp $

#include "carma/antenna/common/Offset.h"

using namespace std;
using namespace carma::services;
using namespace carma::antenna::common;

/**.......................................................................
 * Constructor.
 */
Offset::Offset() :
  offset_(Angle(0.0, "radians")) {}

/**.......................................................................
 * Destructor.
 */
Offset::~Offset() {}

void Offset::set(const carma::services::Angle& offset) {
  offset_ = offset;
}

Angle* Offset::getOffset() {
  return &offset_;
}

