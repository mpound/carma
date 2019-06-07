#ifndef CARMA_ANTENNA_COMMON_DRIVECONTROLUTILS_H
#define CARMA_ANTENNA_COMMON_DRIVECONTROLUTILS_H

#include "carma/corba/corba.h"

#include "carma/antenna/common/DriveControl.h"

#include <string>



namespace carma {
namespace antenna {
namespace common {

::std::string
raDecEpochToString( const DriveControl::RaDecEpoch & epoch );

::std::string
raDecTripletToString( const DriveControl::RaDecTriplet & triplet );

}}} // namespace carma::antenna::common
#endif
