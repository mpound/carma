/**@file
 * 
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.7 $
 * $Date: 2006/06/27 18:39:16 $
 * $Id: bimaControlHost.h,v 1.7 2006/06/27 18:39:16 abeard Exp $
 */

#ifndef CARMA_ANTENNA_BIMA_CONTROL_H
#define CARMA_ANTENNA_BIMA_CONTROL_H

// C++ Standard library includes
#include <map>
#include <string>
#include <vector>
#include <iostream>
#include <ostream>

// Unix
#include <unistd.h>
#include <pthread.h>
#include <errno.h>

// CARMA includes
#include "carma/util/Program.h"
#include "carma/services/Global.h"
#include "carma/util/Logger.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Time.h"

#include "carma/antenna/bima/Configuration.h"
#include "carma/antenna/bima/AntennaNameResolver.h"
#include "carma/antenna/bima/SharedMemory.h"
#include "carma/antenna/bima/control/ControlServer.h"

#ifndef BIMA_DEBUG
#define BIMA_DEBUG 0
#endif

/** @class carma::antenna::bima::bimaControlHost
 * Server stub for bimaControlHost
 *
 */

/**
 * Contains all carma related code.
 */
namespace carma {
  namespace antenna {
    namespace bima {
    
    /**
     * Contains all bimaControlHost related code.
     */
    class bimaControlHost
      {
      public:
	
	/** 
	 * Constructor.
	 * Creates a bimaControlHost with a feed into the bima shared mem
         * as given...
	 * @param bimaShm Reference to bima shared memory handle.
	 * @param logger Reference to logger.
	 */
	bimaControlHost( SharedMemory &bimaShm, log4cpp::Category &logger );

	/**
	 * Destructor
	 */
	virtual ~bimaControlHost();
	
	// Copy and assignment not permitted for this class.
	bimaControlHost( const bimaControlHost & );
	bimaControlHost &operator=( const bimaControlHost & );
	
	carma::util::Time time_;
	log4cpp::Category &logger_;

	SharedMemory &bimaShm_;

      };  

    } // Namespace bima
  } // Namespace antenna
} // Namespace carma 


#endif // CARMA_ANTENNA_BIMA_CONTROL_H
