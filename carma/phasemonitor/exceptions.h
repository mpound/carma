#ifndef CARMA_PHASEMONITOR_EXCEPTIONS_H
#define CARMA_PHASEMONITOR_EXCEPTIONS_H

#include "carma/util/ErrorException.h"
#include <iostream>
#include <sstream>

namespace carma {
namespace phasemonitor {

MAKE_DERIVED_ERROR_EXCEPTION(PhaseMonitorDeviceException);
MAKE_DERIVED_ERROR_EXCEPTION(PhaseMonitorDeviceReplayException);
MAKE_DERIVED_ERROR_EXCEPTION(PhaseMonitorWorkerException);
MAKE_DERIVED_ERROR_EXCEPTION(PhaseMonitorSamplesException);
MAKE_DERIVED_ERROR_EXCEPTION(BadVoltageException);
MAKE_DERIVED_ERROR_EXCEPTION(IdenticalVoltageException);
MAKE_DERIVED_ERROR_EXCEPTION(SerialCommException);
MAKE_DERIVED_ERROR_EXCEPTION(InvalidResponseException);

}} // namespace carma::phasemonitor
#endif
