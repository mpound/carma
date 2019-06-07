/**
 * @file
 * OpticalTelImpl Corba interface implementation.
 *
 * @author Colby Gutierrez-Kraybill
 * $Revision: 1.13 $
 * $Date: 2012/02/29 16:22:53 $
 * $Id: OpticalTelControlImpl.h,v 1.13 2012/02/29 16:22:53 abeard Exp $
 */
#ifndef CARMA_ANTENNA_BIMA_OPTICALTELCONTROLIMPL_H
#define CARMA_ANTENNA_BIMA_OPTICALTELCONTROLIMPL_H

#include "carma/util/UserException.h"
#include "carma/antenna/common/OpticalTelCommon.h"
#include "carma/antenna/bima/OpticalFlap.h"

// Carma Tools includes
#include <log4cpp/Category.hh>

namespace carma
{
 namespace antenna
 {
  namespace bima
  {

    /**
     * OpticalTelControl implementation class.
     */
    class OpticalTelControlImpl :
        public carma::antenna::common::OpticalTelCommon
    {
    public:

        /**
         * Constructor.
         * @param optMon Reference to carma::monitor::AntennaCommon::OpticalTel instance.
         * @param fg Reference to frame grabber instance.
         */
        OpticalTelControlImpl( carma::monitor::AntennaCommon::OpticalTel &optMon,
                               carma::antenna::common::FrameGrabber &fg,
                               Configuration &config,
                               float azFieldOfViewInArcminutes,
                               float elFieldOfViewInArcminutes,
                               float rotationInDegrees,
                               bool simulate );

        /**
         * Destructor.
         */
        virtual ~OpticalTelControlImpl();

    private:

        // turn and setFocus are antenna specific, and are defined here...
        // all other control functions are defined in
        // carma::antenna::common::OpticalTelCommon
        void turn( carma::antenna::common::SwitchState state );

        // the following are protected members from
        // carma::antenna::common::OpticalTelCommon

        // log4cpp::Category &log_;
        // carma::monitor::AntennaCommon::OpticalTel &mon_;
        OpticalFlap *_flap;
        Configuration &_config;

   }; // End class OpticalTelControlImpl
  } // namespace bima
 } // namespace antenna
} // namespace carma

#endif

