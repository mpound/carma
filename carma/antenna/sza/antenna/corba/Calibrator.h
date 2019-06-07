#ifndef CALIBRATOR_H
#define CALIBRATOR_H

/**
 * @file Calibrator.h
 *
 * Tagged: Fri Nov 14 12:37:51 UTC 2003
 *
 * @author Erik Leitch
 */
#include "carma/antenna/common/CalibratorControl.h"

/**
 * Define a class for Calibrator control
 */
namespace sza {
  namespace antenna {
    namespace corba {

      class Calibrator { 

	public:

	void setPos(carma::antenna::common::CalibratorControl::Position pos);
      };

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif

