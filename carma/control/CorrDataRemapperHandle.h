// $Id: CorrDataRemapperHandle.h,v 1.1 2011/02/25 01:23:38 eml Exp $

#ifndef CARMA_CONTROL_CORRDATAREMAPPERHANDLE_H
#define CARMA_CONTROL_CORRDATAREMAPPERHANDLE_H

/**
 * @file CorrDataRemapperHandle.h
 * 
 * Tagged: Thu Feb 24 15:07:14 PST 2011
 * 
 * @version: $Revision: 1.1 $, $Date: 2011/02/25 01:23:38 $
 * 
 * @author username: Command not found.
 */
#include "carma/control/RemoteObjHandleT.h"

#include "carma/correlator/transport/CorrDataRemapperControl.h"

#include "carma/monitor/ControlSubsystem.h"

#include <string>
#include <vector>

namespace carma {

  namespace monitor {
    class MonitorSystem;
  }
  
  namespace control {

    typedef RemoteObjHandleT<carma::correlator::CorrDataRemapperControl> CorrDataRemapperRemoteObjHandle;

    class CorrDataRemapperHandle : public CorrDataRemapperRemoteObjHandle {
    public:

      CorrDataRemapperHandle( 
        carma::monitor::MonitorSystem& monitorSystem,
        carma::monitor::ControlSubsystemBase::Reachable& reachable);

      /**
       * Destructor.
       */
      virtual ~CorrDataRemapperHandle();

      void clearAstroBandInputMap(unsigned astroBandNo);

      void updateAstroBandInputMap(unsigned astroBandNo, std::vector<carma::correlator::CorrDataRemapperControl::AstroBandInput>& abVec);

    }; // End class CorrDataRemapperHandle

  } // End namespace control
} // End namespace carma



#endif // End #ifndef CARMA_CONTROL_CORRDATAREMAPPERHANDLE_H
