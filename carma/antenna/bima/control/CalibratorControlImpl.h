/**
 * @file
 * CalibratorControlImpl CORBA control implementation.
 *
 * @author Colby Gutierrez-Kraybill
 * Version: $Revision: 1.11 $
 * $Date: 2012/02/21 21:06:58 $
 * $Id: CalibratorControlImpl.h,v 1.11 2012/02/21 21:06:58 abeard Exp $
 */
#ifndef CARMA_ANTENNA_BIMA_CALIBRATORCONTROLIMPL_H
#define CARMA_ANTENNA_BIMA_CALIBRATORCONTROLIMPL_H

// Carma includes
#include "carma/corba/corba.h"
#include "carma/util/ExceptionUtils.h"

#include "carma/antenna/bima/RxClient.h"
#include "carma/antenna/bima/Configuration.h"
#include "carma/antenna/bima/control/IDLutils.h"
#include "carma/antenna/common/CalibratorControl.h"

// Carma Tools includes
#include <log4cpp/Category.hh>

namespace carma
{
  namespace antenna
  {
    namespace bima
    {

      /**
       * CalibrationControlImpl Corba control class.
       * This class implements the IDL defined interface for antenna calibration
       * and dispatches commands to appropriate CAN devices via delegation.
       */
      class CalibratorControlImpl :
	public RxClient
      {
      public:

        /**
         * Constructor
         * @param optics Pointer to underlying bima optics CAN device.
         * @param poa Pointer to encompassing poa.
         */
        CalibratorControlImpl( Configuration &config );

        /**
         * Destructor
         */
        ~CalibratorControlImpl();

        void setPos(
            carma::antenna::common::CalibratorControl::Position position);

        void setPos(
            carma::antenna::common::CalibratorControl::Position position,
            ::CORBA::ULong seqNo );

    private:

        log4cpp::Category &log_;
	Configuration &_config;

      }; // End class carma::antenna::bima::CalibratorControlImpl
    } // namespace bima
  } // namespace antenna
} // End namespace carma

#endif // CARMA_ANTENNA_BIMA_CALIBRATORCONTROLIMPL_H

