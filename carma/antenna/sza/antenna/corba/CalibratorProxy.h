#ifndef CALIBRATORPROXY_H
#define CALIBRATORPROXY_H

/**
 * @file Calibrator.h
 *
 * Tagged: Fri Nov 14 12:37:51 UTC 2003
 *
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/corba/Proxy.h"
#include "carma/antenna/sza/control/szaCalibratorControl.h"
#include "carma/szautil/CalPos.h"

/**
 * Define a class for Calibrator control
 */
namespace sza {
  namespace antenna {
    namespace corba {

      class CalibratorProxy : public Proxy {

	public:

	// Constructor with a pointer to the parent AntennaMaster.

	CalibratorProxy(sza::antenna::control::AntennaMaster* parent);

	// Destructor.

	~CalibratorProxy();

	// IDL interface

	void setPos(carma::antenna::common::CalibratorControl::Position pos,
		    CORBA::ULong seq);

	void homeTertiary();

	void positionTertiaryAngle(double angleDegrees);

	void positionTertiaryRx(carma::antenna::common::RxControl::Type rx);

	sza::util::CalPos::Pos carmaCalPosToSzaCalPos(carma::antenna::common::CalibratorControl::Position pos);

	sza::util::Rx::Id carmaRxToSzaRxId(carma::antenna::common::RxControl::Type type);

      };

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif

