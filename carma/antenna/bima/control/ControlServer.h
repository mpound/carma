/** @file
 * carma::antenna::bima::ControlServer class declaration.
 * ConrtolServer architecture follows OVRO pattern, but
 * with caveats, such as Telemetry differences vs CanBus...
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.10 $
 * $Id: ControlServer.h,v 1.10 2012/08/01 14:13:06 friedel Exp $
 */
#ifndef CARMA_ANTENNA_BIMA_CONTROLSERVER_H
#define CARMA_ANTENNA_BIMA_CONTROLSERVER_H

#include "carma/antenna/bima/control/CalibratorControlImpl.h"
#include "carma/antenna/bima/control/CryoControlImpl.h"
#include "carma/antenna/bima/control/DriveControlImpl.h"
#include "carma/antenna/bima/control/FocusControlImpl.h"
#include "carma/antenna/bima/control/FrontEndControlImpl.h"
#include "carma/antenna/bima/control/LOControlImpl.h"
#include "carma/antenna/bima/control/OpticsControlImpl.h"
#include "carma/antenna/bima/control/PolarizationControlImpl.h"
#include "carma/antenna/bima/control/RxControlImpl.h"
#include "carma/antenna/bima/control/RxSelectorImpl.h"

#include <memory>

namespace carma
{

  namespace corba {
    class Server;
  }

  namespace antenna
  {
    namespace bima
    {

    /**
     * ControlServer class encapsulates most control details.
     * This class is mainly responsible for encapsulating internal CORBA
     * control details by incarnating all related control DOs which
     * in turn send requests to the Telemetry control system.
     * The idea behind the class is to confine most details
     * to a single class so that they can be more modularly changed in the 
     * future if need be. So sayeth the Andy.
     */
      class ControlServer
      {
       public:
        
        /**
         * Constructor.
         * The constructor is responsible for creating all CORBA DOs and 
         * publishing them.  Note this object must be created on the stack.
         */
        ControlServer ( Configuration &config,
                        carma::corba::Server & server );

        /**
         * Destructor
         */
        virtual ~ControlServer(); 

        /**
         * Run control server.
         * This method blocks on the orb!
         */
        void runServer();

       private:
        
        // For now, publish all three receiver types onto the nameserver.
        // Later I can add them to the AntennaControl object if I'm not
        // able to convince everybody to get rid of it.
        Configuration &_config;

        carma::antenna::bima::CalibratorControlImpl cal_;
        carma::antenna::bima::CryoControlImpl cryo_;
        carma::antenna::bima::DriveControlImpl drive_;
        carma::antenna::bima::LOControlImpl lo_;
        carma::antenna::bima::FocusControlImpl focus_;
        carma::antenna::bima::FrontEndControlImpl fe_;
        carma::antenna::bima::OpticsControlImpl op_;
        carma::antenna::bima::PolarizationControlImpl po_;
        std::auto_ptr< carma::antenna::bima::RxControlImpl > rxcontrolAp_;
        std::auto_ptr< carma::antenna::bima::RxSelectorImpl > rxselectorAp_;

        carma::corba::Server & server_;

	static void *MonitorThread(void *data);

      }; // class ControlServer
    } // namespace bima
  } // namespace antenna
} // namespace carma

#endif // CARMA_ANTENNA_BIMA_CONTROLSERVER_H
