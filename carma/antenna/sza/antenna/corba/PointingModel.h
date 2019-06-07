#ifndef POINTINGMODEL_H
#define POINTINGMODEL_H

/**
 * @file PointingModel.h
 *
 * Tagged: Fri Nov 14 12:38:04 UTC 2003
 *
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/corba/Corba.h"

#ifdef SZA_PM
#include "carma/antenna/common/PointingModelControl.h"
#endif

#include "carma/antenna/common/DriveControl.h"

/**
 * Define a PointingModel class
 */
namespace sza {
  namespace antenna {
    namespace corba {


#ifdef SZA_PM

      class PointingModel {

	public:

	void scheduleNextSequenceNo(unsigned long seq);

	void setAzTilts(double tilt1, double tilt2);

	void setElTilts(double tilt1, double tilt2);

	void setTilts(double azTilt1, double azTilt2,
		      double elTilt1, double elTilt2);

	void setAzElMisalignment(double misalignment);

	void setFlexure(carma::antenna::common::
			PointingModelControl::Model model,
			double flexure);

	void setCollimation(carma::antenna::common::
			    PointingModelControl::Model model,
			    double magnitude, double direction);

	void setEncoderZero(double az, double el);

	void setEncoderCal(double az, double el);

	void setAzResiduals(double res1, double res2);

	void setElResiduals(double res1, double res2);

	void setFourierResidual(double residual);

	}; // End class PointingModel

#else
	class PointingModel {

	public:

	}; // End class PointingModel
#endif


    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif
