/**@file
 * 
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.26 $
 * $Date: 2013/02/13 15:48:17 $
 * $Id: MonitorUpdater.h,v 1.26 2013/02/13 15:48:17 friedel Exp $
 */

#ifndef CARMA_ANTENNA_BIMA_MONITORUPDATER_H
#define CARMA_ANTENNA_BIMA_MONITORUPDATER_H

#define MAXGAP 0.0001736111
// two frames in MJD
#define MAXFASTGAP 0.00001574074
#define MAXSLOWGAP 0.00003148148

// C++ Standard library includes
#include <iostream>
#include <ostream>

// C includes
#include <time.h>
#include <stdlib.h>
#include <unistd.h>

// CARMA includes
#include "carma/util/Program.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/Trace.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/antenna/bima/SharedMemory.h"
#include "carma/util/IPQreader.h"
#include "carma/services/Units.h"
#include "carma/services/Global.h"
#include "carma/util/ErrorException.h"
#include "carma/monitor/AntennaCommon.h"
#include "carma/monitor/BimaSubsystem.h"
#include "carma/antenna/bima/Configuration.h"
#include "carma/antenna/bima/LO.h"
#include "carma/antenna/bima/IF.h"
#include "carma/antenna/bima/Rx.h"
#include "carma/antenna/bima/DewarRegulation.h"
#include "carma/antenna/bima/Drives.h"
#include "carma/antenna/bima/CalWheel.h"
#include "carma/antenna/bima/Secondary.h"
#include "carma/antenna/bima/Polarizer.h"


/** @class carma::antenna::bima::MonitorUpdater
 * Server stub for MonitorUpdater
 * Documentation for the 
 *
 */

#define FLATTENCASEENUM( A, B, C ) \
   case A: \
     C = B; \
   break;

/**
 * Contains all carma related code.
 */
namespace carma
{
  namespace antenna
  {
    namespace bima
    {
    
    
    /**
     */
      class MonitorUpdater
      {
      public:
	
	/** 
	 * Constructor.
	 */
	MonitorUpdater( Configuration &config, 
                    log4cpp::Category &logger,
                    double autoWriteDelayInS );

	/**
	 * Destructor
	 */
	virtual ~MonitorUpdater();
	

	void run( void );
	static void *startThread( void *arg );
	void monitorThread( void );

        void deadManOk()
         { _mutex.Lock(); _ok = true; _mutex.Unlock();};
        bool deadManCheck()
         { _mutex.Lock(); bool status = _ok; _ok = false; _mutex.Unlock(); return status; };

      private:
	
	// Copy and assignment not permitted for this class.
	MonitorUpdater( const MonitorUpdater & );
	MonitorUpdater &operator=( const MonitorUpdater & );

    void setAntCommonLoMonitorPoints( 
        carma::monitor::AntennaCommon::LO & antComLO,
        carma::antenna::bima::Rx & rx,
        carma::antenna::bima::LO & lo,
        carma::monitor::AntennaCommon::YigStateMonitorPointEnum::YIGSTATE & yigState, 
        carma::monitor::AntennaCommon::LoStateMonitorPointEnum::LOSTATE & loState,
        const carma::monitor::VaractorModule & varactor,
        const int strobe );

    void setAntCommonRxMonitorPoints( 
        carma::monitor::AntennaCommon::Receivers & antComRx,
        carma::antenna::bima::Rx & rx,
        carma::monitor::AntennaCommon::RxStateMonitorPointEnum::RXSTATE & rxGoodmpe,
        const carma::monitor::AntennaCommon::YigStateMonitorPointEnum::YIGSTATE & yigState, 
        const carma::monitor::AntennaCommon::LoStateMonitorPointEnum::LOSTATE & loState );
	
        carma::monitor::BimaSubsystem *_monPnts;
        carma::monitor::BimaSubsystem *_readMonPnts;
        Rx *_rx;
        DewarRegulation *_dewarReg;
        CalWheel *_calwheel;
        Secondary *_secondary;
        Polarizer *_polarizer;
        LO *_lo;
        IF *_if;
        Drives *_drives;
        int _apcCount;
        int _epcCount;

        Configuration &_config;
	log4cpp::Category &_logger;
        bool _emulate;
        bool _ok;
        carma::util::PthreadMutex _mutex;
      };  

    } // Namespace bima
  } // Namespace antenna
} // Namespace carma 


#endif // CARMA_ANTENNA_BIMA_MONITORUPDATER_H
