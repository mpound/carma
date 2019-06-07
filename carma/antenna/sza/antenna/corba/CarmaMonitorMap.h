// $Id: CarmaMonitorMap.h,v 1.2 2010/12/13 20:52:26 eml Exp $

#ifndef SZA_ANTENNA_CORBA_CARMAMONITORMAP_H
#define SZA_ANTENNA_CORBA_CARMAMONITORMAP_H

/**
 * @file CarmaMonitorMap.h
 * 
 * Tagged: Fri Aug 14 13:40:31 PDT 2009
 * 
 * @version: $Revision: 1.2 $, $Date: 2010/12/13 20:52:26 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/Directives.h"

#if DIR_USE_ANT_CORBA
#include "carma/antenna/sza/antenna/corba/Corba.h"
#include "carma/monitor/MonitorPoint.h"
#include "carma/monitor/SzaSubsystem.h"
#endif

namespace sza {
  namespace antenna {
    namespace corba {

      class CarmaMonitorMap {
      public:

	/**
	 * Constructor.
	 */
	CarmaMonitorMap();

	/**
	 * Destructor.
	 */
	virtual ~CarmaMonitorMap();

#if DIR_USE_ANT_CORBA
	static carma::monitor::MonitorPoint* 
	  getMonitorPoint(carma::monitor::SzaSubsystem* antennaMonitor, 
			  std::string board, std::string block);
#endif

      private:
      }; // End class CarmaMonitorMap

    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_CARMAMONITORMAP_H
