#include <stdlib.h>
#include <iostream>
#include <string>

#include "carma/antenna/sza/antenna/corba/Calibrator.h"

using namespace sza::antenna::corba;
/*
 * Set the Calibrator to the requested position
 */
void Calibrator::setPos(carma::antenna::common::CalibratorControl::Position pos)
{
  std::cout << "Calibrator::setCalibrator() stub" << std::endl;
};

